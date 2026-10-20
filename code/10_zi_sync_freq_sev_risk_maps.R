# ==============================================================================
# 10_zi_Sync_Sev_Risk_maps.R
# Shiny App — Frequency, Severity & Risk Maps  (with synchronized maps)
#
# MODELS:
# - Frequency : model_zi_nb_vol_full   (ZINB, log_vol + weather offset)
# - Severity  : modello_severita_logistico (logistic GLMM, P(severe|accident))
# - Risk      : E[accidents] × P(severe) = expected severe accidents
#
# DIFFERENCE FROM 09:
# - The SEVERITY and RISK tabs show two synchronized side-by-side maps (leafsync):
#     LEFT  → Pedestrian NOT Involved (Pedestrian_involved = 0)
#     RIGHT → Pedestrian Involved     (Pedestrian_involved = 1)
# - The "Pedestrian involved" radio button in the sidebar is removed
#   because both versions are always shown simultaneously.
# - The FREQUENCY tab stays unchanged (pedestrian is not in the frequency model).
#
# TRAFFIC CONSISTENCY:
# A single slider (volume percentile) controls both models:
#   0–33%  → Scarso (Light)  | 34–66% → Normale (Normal) | 67–100% → Intenso (Heavy)
# ==============================================================================

library(shiny)
library(leaflet)
library(leafsync)
library(dplyr)
library(tidyr)
library(sf)
library(glmmTMB)
library(lme4)

rm(list = ls())

# FIX (error "add.bizdays.default ... argument 'n' is missing"):
# the "bizdays" package exports its own offset() function, with a
# different signature from stats::offset() used in the model formula
# (offset(log(giorni_esposizione))). If "bizdays" is attached in the
# session, it masks stats::offset and every predict() breaks with this
# error. The correct version is forced here, before any predict() call.
offset <- stats::offset

# ==============================================================================
# 1. LOADING MODELS
# ==============================================================================

load("zi_frequency_model_objects_vol_v2.RData")  # FIX: era "...vol.RData" (senza _v2)
# model_zi_nb_vol_full, model_exposure_offset, scenario_vol, traffico,
# esposizione_annua, mappa_quartieri

load("severity_model_objects.RData")
# modello_severita_logistico, data_severity2
# FIX: rimossa la riga "modello_severita_logistico <- modello_severita2_glmm".
# "modello_severita2_glmm" non esiste in severity_model_objects.RData: il file
# 08 already saves the object under the correct name modello_severita_logistico.

# ==============================================================================
# 2. OGGETTI BASE E CALCOLO MASSIMI GLOBALI (LEGENDE FISSE)
# ==============================================================================

mappa_base <- mappa_quartieri %>%
  dplyr::select(DEN_Z_URB, geometry) %>%
  group_by(DEN_Z_URB) %>%
  summarise(.groups = "drop") %>%
  st_transform(crs = 4326)

quartieri  <- sort(unique(mappa_base$DEN_Z_URB))
all_months <- c("Jan","Feb","Mar","Apr","May","Jun",
                "Jul","Aug","Sep","Oct","Nov","Dec")

pct_to_traffic <- function(pct) {
  dplyr::case_when(pct <= 33 ~ "Scarso", pct <= 66 ~ "Normale", TRUE ~ "Intenso")
}

get_log_vol <- function(daytype, period, pct) {
  scenario_vol %>%
    filter(DayType == daytype, DayPeriod == period) %>%
    group_by(DEN_Z_URB, DayType, DayPeriod) %>%
    summarise(
      log_vol = log(quantile(volume_veicoli, probs = pct / 100, na.rm = TRUE)),
      .groups = "drop"
    )
}

# --- RICERCA DEI MASSIMI GLOBALI PER LEGENDE FISSE ---
max_vols <- scenario_vol %>%
  group_by(DEN_Z_URB, DayType, DayPeriod) %>%
  summarise(
    log_vol = log(max(volume_veicoli, na.rm = TRUE)),
    .groups = "drop"
  )

all_combinations <- expand.grid(
  DEN_Z_URB = quartieri,
  Month = all_months,
  DayType = c("weekday", "weekend"),
  Weather_choice = c("Clear", "NotClear"),  # FIX: livello "UI", vedi sotto
  DayPeriod = c("Morning", "Afternoon", "Evening", "Night"),
  Pedestrian_involved = c(0, 1),
  stringsAsFactors = FALSE
) %>%
  left_join(max_vols, by = c("DEN_Z_URB", "DayType", "DayPeriod")) %>%
  # giorni_esposizione = 1: prediction for a single time unit
  mutate(giorni_esposizione = 1) %>%
  filter(!is.na(log_vol))

all_combinations <- all_combinations %>%
  mutate(
    Traffic = "Intenso",
    Weekend = ifelse(DayType == "weekend", 1, 0),
    # rain_day (severity model, file 08): is NOT a 0/1 flag, it is a string
    # "Rain"/"Dry" — effectively duplicates Weather_simple under another name
    # (stessa fonte/logica: FENOMENI contiene "piogg").
    rain_day = ifelse(Weather_choice == "NotClear", "Rain", "Dry"),
    # FIX: Weather_simple for the frequency model (file 07) was
    # fit with levels {"Dry","Rain"}, not {"Clear","NotClear"}.
    # Clear == Dry, NotClear == Rain: stesso concetto, etichetta diversa.
    Weather_simple = ifelse(Weather_choice == "Clear", "Dry", "Rain")
  )

all_combinations$mu <- predict(model_zi_nb_vol_full, newdata = all_combinations,
                               type = "response", allow.new.levels = TRUE)

all_combinations$sev <- predict(modello_severita_logistico, newdata = all_combinations,
                                type = "response", allow.new.levels = TRUE)

all_combinations$risk <- all_combinations$mu * all_combinations$sev

MAX_FREQ <- ceiling(max(all_combinations$mu,   na.rm = TRUE))
MAX_SEV  <- 1
MAX_RISK <- ceiling(max(all_combinations$risk, na.rm = TRUE))

# ==============================================================================
# 3. UI — STILE MAPPING URBAN TRAFFIC RISK
# ==============================================================================

YELLOW <- "#FFD93D"
BLACK  <- "#0D0D0D"
RED    <- "#E83A2F"
WHITE  <- "#FFFFFF"
GREY   <- "#F2F2F2"

custom_css <- tags$style(HTML(sprintf("
  @import url('https://fonts.googleapis.com/css2?family=Barlow+Condensed:wght@400;600;700;900&family=Barlow:wght@300;400;500&display=swap');

  * { box-sizing: border-box; margin: 0; padding: 0; }

  body {
    background: %s;
    font-family: 'Barlow', sans-serif;
    color: %s;
  }

  /* ---- HEADER ---- */
  .app-header {
    background: %s;
    padding: 18px 32px 14px 32px;
    border-bottom: 4px solid %s;
    display: flex;
    align-items: baseline;
    gap: 18px;
  }
  .app-title {
    font-family: 'Barlow Condensed', sans-serif;
    font-weight: 900;
    font-size: 28px;
    letter-spacing: 1px;
    color: %s;
    text-transform: uppercase;
  }
  .app-subtitle {
    font-family: 'Barlow Condensed', sans-serif;
    font-weight: 600;
    font-size: 13px;
    letter-spacing: 2px;
    color: %s;
    text-transform: uppercase;
    opacity: 0.6;
  }

  /* ---- SIDEBAR ---- */
  .sidebar-panel {
    background: %s !important;
    border-right: none !important;
    padding: 24px 20px !important;
    min-height: calc(100vh - 64px);
  }

  .section-label {
    font-family: 'Barlow Condensed', sans-serif;
    font-weight: 700;
    font-size: 11px;
    letter-spacing: 3px;
    color: %s;
    text-transform: uppercase;
    margin: 20px 0 10px 0;
    padding-bottom: 5px;
    border-bottom: 1.5px solid %s;
    opacity: 0.55;
  }
  .section-label:first-of-type { margin-top: 4px; }

  /* selects */
  .form-group label {
    font-family: 'Barlow Condensed', sans-serif;
    font-weight: 600;
    font-size: 12px;
    letter-spacing: 1.5px;
    text-transform: uppercase;
    color: %s;
    opacity: 0.7;
    margin-bottom: 4px;
  }
  .form-control {
    background: rgba(255,255,255,0.08) !important;
    border: 1px solid rgba(255,255,255,0.18) !important;
    border-radius: 3px !important;
    color: %s !important;
    font-family: 'Barlow', sans-serif !important;
    font-size: 13px !important;
    padding: 7px 10px !important;
    height: 36px !important;
  }
  .form-control:focus {
    border-color: %s !important;
    box-shadow: 0 0 0 2px rgba(255,217,61,0.25) !important;
    outline: none !important;
  }
  select option { background: #1a1a1a; color: %s; }

  /* slider */
  .irs--shiny .irs-bar { background: %s !important; border-color: %s !important; }
  .irs--shiny .irs-handle { background: %s !important; border-color: %s !important; }
  .irs--shiny .irs-from, .irs--shiny .irs-to,
  .irs--shiny .irs-single { background: %s !important; }
  .irs--shiny .irs-line { background: rgba(255,255,255,0.15) !important; }
  .irs--shiny .irs-min, .irs--shiny .irs-max {
    color: rgba(255,255,255,0.35) !important;
    font-size: 10px;
  }

  /* traffic badge */
  .traffic-badge {
    font-family: 'Barlow Condensed', sans-serif;
    font-weight: 700;
    font-size: 13px;
    letter-spacing: 1.5px;
    text-transform: uppercase;
    padding: 7px 14px;
    border-radius: 3px;
    margin-top: 8px;
    display: inline-block;
  }

  /* tabs */
  .main-panel { background: %s; }
  .nav-tabs { background: %s !important; border-bottom: 2px solid %s !important; }
  .nav-tabs > li > a {
    font-family: 'Barlow Condensed', sans-serif !important;
    font-weight: 700 !important;
    font-size: 13px !important;
    letter-spacing: 2px !important;
    text-transform: uppercase !important;
    color: rgba(0,0,0,0.45) !important;
    border: none !important;
    border-radius: 0 !important;
    padding: 12px 22px !important;
    background: transparent !important;
  }
  .nav-tabs > li.active > a,
  .nav-tabs > li.active > a:hover {
    color: %s !important;
    border-bottom: 3px solid %s !important;
    background: transparent !important;
  }

  /* map wrapper */
  .map-wrap { position: relative; }
  .map-desc {
    position: absolute;
    bottom: 28px;
    left: 14px;
    background: %s;
    border-left: 4px solid %s;
    padding: 10px 14px;
    z-index: 999;
    max-width: 320px;
    pointer-events: none;
  }
  .map-desc-title {
    font-family: 'Barlow Condensed', sans-serif;
    font-weight: 900;
    font-size: 15px;
    letter-spacing: 1px;
    text-transform: uppercase;
    color: %s;
    margin-bottom: 3px;
  }
  .map-desc-sub {
    font-size: 11px;
    color: rgba(0,0,0,0.55);
    line-height: 1.45;
  }
  .map-desc-sub .MathJax, .map-desc-sub .MathJax_Display {
    font-size: 95%% !important;
  }

  /* ---- MODEL FORMULA BAR (sync tabs) ---- */
  .model-formula-bar {
    background: %s;
    border-left: 4px solid %s;
    padding: 8px 16px;
    font-size: 12px;
    color: rgba(13,13,13,0.7);
    line-height: 1.45;
  }
  .model-formula-bar .MathJax, .model-formula-bar .MathJax_Display {
    font-size: 95%% !important;
  }

  /* ---- SYNC MAP LABELS ---- */
  .sync-label-bar {
    display: flex;
    width: 100%%;
  }
  .sync-label-left, .sync-label-right {
    flex: 1 1 50%%;
    width: 50%%;
    box-sizing: border-box;
    font-family: 'Barlow Condensed', sans-serif;
    font-weight: 700;
    font-size: 13px;
    letter-spacing: 2px;
    text-transform: uppercase;
    text-align: center;
    padding: 7px 14px;
    border-bottom: 3px solid;
  }
  .sync-label-left  { background: %s; color: %s; border-color: %s; }
  .sync-label-right { background: %s; color: %s; border-color: %s; }
",
   BLACK, BLACK,           # body bg, color
   YELLOW, BLACK,          # header bg, border
   BLACK,                  # title color
   BLACK,                  # subtitle color
   BLACK,                  # sidebar bg
   WHITE, WHITE,           # section label color, border
   WHITE, WHITE,           # label color, select color
   YELLOW,                 # select focus border
   WHITE,                  # option color
   YELLOW, YELLOW,         # slider bar
   YELLOW, YELLOW,         # slider handle
   BLACK,                  # slider bubble bg
   GREY,                   # main panel bg
   GREY, BLACK,            # tabs bg, tabs border
   BLACK, BLACK,           # active tab color, border
   WHITE, RED,             # map desc bg, border
   BLACK,                  # map desc title
   WHITE, BLACK,           # model formula bar bg, border
   BLACK, WHITE, BLACK,    # sync label LEFT: bg, color, border
   YELLOW, BLACK, YELLOW   # sync label RIGHT: bg, color, border
)))

# UI helper: label bar above the side-by-side maps, with an optional LaTeX caption
sync_label_bar <- function(formula = NULL) {
  tagList(
    if (!is.null(formula)) div(class = "model-formula-bar", formula),
    div(class = "sync-label-bar",
      div(class = "sync-label-left",  "PEDESTRIAN NOT INVOLVED"),
      div(class = "sync-label-right", "PEDESTRIAN INVOLVED")
    )
  )
}

map_height <- "calc(100vh - 148px)"  # height reduced by ~36px for the label bar

ui <- withMathJax(fluidPage(
  custom_css,

  # Header
  div(class = "app-header",
    span(class = "app-title", "Mapping Urban Traffic Risk"),
    span(class = "app-subtitle", "Rome Road Network — A Data Driven Approach")
  ),

  div(style = "display:flex; height:calc(100vh - 64px);",

    # --- SIDEBAR ---
    div(class = "sidebar-panel", style = "width:260px; flex-shrink:0; overflow-y:auto;",

      div(class = "section-label", "Scenario"),

      selectInput("month", "Month",
        choices = c("Generic" = "Generic", setNames(all_months, all_months)),
        selected = "Generic"),

      selectInput("period", "Time Period",
        choices = c("Morning","Afternoon","Evening","Night"), selected = "Morning"),

      selectInput("daytype", "Day Type",
        choices = c("Weekday" = "weekday", "Weekend" = "weekend"), selected = "weekday"),

      selectInput("weather", "Weather",
        choices = c("Clear" = "Clear", "Adverse" = "NotClear"), selected = "Clear"),

      div(class = "section-label", "Traffic"),

      sliderInput("vol_pct", "Volume Level",
        min = 0, max = 100, value = 50, step = 1, post = "%"),

      uiOutput("traffic_badge"),

      # Info note: pedestrian is now always shown as a double map
      div(style = "margin-top:20px; padding: 10px 12px;
                   background: rgba(255,255,255,0.07);
                   border-left: 3px solid #FFD93D;
                   font-size: 11px; color: rgba(255,255,255,0.6);
                   font-family: 'Barlow', sans-serif; line-height:1.5;",
        "Severity & Risk show side-by-side maps:",
        tags$br(),
        tags$span(style="background:#0D0D0D; color:#FFFFFF; font-weight:700; padding:1px 6px; border-radius:2px;", "LEFT"),
        " = No pedestrian",
        tags$br(),
        tags$span(style="background:#FFD93D; color:#0D0D0D; font-weight:700; padding:1px 6px; border-radius:2px;", "RIGHT"),
        " = Pedestrian involved"
      )
    ),

    # --- MAIN ---
    div(class = "main-panel", style = "flex:1; overflow:hidden;",
      tabsetPanel(id = "tabs",

        # ---- TAB FREQUENCY (invariata) ----
        tabPanel("FREQUENCY",
          div(class = "map-wrap",
            leafletOutput("map_freq", height = "calc(100vh - 112px)"),
            div(class = "map-desc",
              div(class = "map-desc-title", "Expected Accidents"),
              div(class = "map-desc-sub",
                "\\(\\mathbb{E}[\\text{accidents}_i] = \\mu_i\\), \\(\\log(\\mu_i) = \\mathbf{x}_i^\\top\\boldsymbol{\\beta} + \\log(\\text{vol}_i)\\) — Zero-Inflated Negative Binomial (ZINB) model, sensor-volume offset & weather-exposure correction.")
            )
          )
        ),

        # ---- TAB SEVERITY (doppia mappa sincronizzata) ----
        tabPanel("SEVERITY",
          sync_label_bar(
            "\\(P(\\text{severe}_i \\mid \\text{accident}_i) = \\text{logit}^{-1}\\!\\left(\\mathbf{x}_i^\\top\\boldsymbol{\\beta} + b_{j(i)}\\right)\\), \\(b_j \\sim \\mathcal{N}(0,\\sigma_b^2)\\) — GLMM logistic model."
          ),
          uiOutput("sync_sev")
        ),

        # ---- TAB RISK (doppia mappa sincronizzata) ----
        tabPanel("RISK",
          sync_label_bar(
            "\\(\\text{Risk}_i = \\mathbb{E}[\\text{accidents}_i] \\times P(\\text{severe}_i \\mid \\text{accident}_i) = \\mu_i \\cdot p_i\\) — expected severe accidents per neighbourhood."
          ),
          uiOutput("sync_risk")
        )
      )
    )
  )
))

# ==============================================================================
# 4. SERVER
# ==============================================================================

server <- function(input, output, session) {

  # Traffic badge colorato
  output$traffic_badge <- renderUI({
    cfg <- list(
      Scarso  = list(label = "Light",  bg = "#2979FF", col = "#fff"),
      Normale = list(label = "Normal", bg = YELLOW,    col = BLACK),
      Intenso = list(label = "Heavy",  bg = RED,       col = "#fff")
    )
    cat <- pct_to_traffic(input$vol_pct)
    c   <- cfg[[cat]]
    div(class = "traffic-badge",
        style = sprintf("background:%s; color:%s;", c$bg, c$col),
        paste0("→ ", c$label))
  })

  # --- FREQUENCY ---
  freq_pred <- reactive({
    mesi    <- if (input$month == "Generic") all_months else input$month
    vol_sel <- get_log_vol(input$daytype, input$period, input$vol_pct)

    # FIX: model_zi_nb_vol_full (file 07) uses Weather_simple in {"Dry","Rain"},
    # while the UI works in {"Clear","NotClear"} (the same thing, different
    # label). It is translated here before building the prediction grid.
    weather_freq <- ifelse(input$weather == "Clear", "Dry", "Rain")

    grid_pred <- expand.grid(
      DEN_Z_URB = quartieri, Month = mesi,
      DayType = input$daytype, Weather_simple = weather_freq,
      DayPeriod = input$period, stringsAsFactors = FALSE
    ) %>%
      left_join(vol_sel, by = c("DEN_Z_URB", "DayType", "DayPeriod")) %>%
      # giorni_esposizione = 1: expected accidents in a single time unit
      mutate(giorni_esposizione = 1)

    grid_pred <- grid_pred %>%
      mutate(log_vol = ifelse(is.na(log_vol), median(log_vol, na.rm = TRUE), log_vol))

    grid_pred$Month     <- factor(grid_pred$Month,     levels = all_months)
    grid_pred$DayPeriod <- factor(grid_pred$DayPeriod, levels = c("Morning","Afternoon","Evening","Night"))

    grid_pred %>%
      mutate(mu = predict(model_zi_nb_vol_full, newdata = .,
                          type = "response", allow.new.levels = TRUE)) %>%
      group_by(DEN_Z_URB) %>%
      summarise(value = mean(mu, na.rm = TRUE), .groups = "drop") %>%
      mutate(value = ifelse(is.nan(value), NA_real_, value))
  })

  # --- SEVERITY for a given value of Pedestrian_involved ---
  sev_pred_ped <- function(ped_val) {
    reactive({
      mesi <- if (input$month == "Generic") all_months else input$month

      expand.grid(
        DEN_Z_URB           = quartieri,
        Month               = mesi,
        DayPeriod           = input$period,
        Weekend             = as.integer(input$daytype == "weekend"),
        rain_day            = ifelse(input$weather == "NotClear", "Rain", "Dry"),
        Pedestrian_involved = ped_val,
        Traffic             = pct_to_traffic(input$vol_pct),
        stringsAsFactors    = FALSE
      ) %>%
        mutate(sev = predict(modello_severita_logistico, newdata = .,
                             type = "response", allow.new.levels = TRUE)) %>%
        group_by(DEN_Z_URB) %>%
        summarise(value = mean(sev, na.rm = TRUE), .groups = "drop")
    })
  }

  sev_pred_no  <- sev_pred_ped(0)   # Pedestrian NOT involved
  sev_pred_yes <- sev_pred_ped(1)   # Pedestrian Involved

  # --- HELPER: costruisce un singolo leaflet (senza legenda, dominio fisso) ---
  make_leaf <- function(data, domain, title, fmt = "%.3f",
                        legend = FALSE, legend_pos = "bottomright") {
    md  <- mappa_base %>% left_join(data, by = "DEN_Z_URB")
    pal <- colorNumeric(c("#FFFDE7","#FFD93D","#E8612F","#8B0000"),
                        domain = c(0, domain), na.color = "#CCCCCC")
    m <- leaflet(md, options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor   = ~pal(value),
        fillOpacity = 0.82,
        color       = BLACK,
        weight      = 0.8,
        highlightOptions = highlightOptions(
          weight = 2, color = BLACK, fillOpacity = 0.95, bringToFront = TRUE),
        popup = ~paste0(
          "<div style='font-family:Barlow Condensed,sans-serif;'>",
          "<div style='font-weight:900;font-size:15px;letter-spacing:1px;",
          "text-transform:uppercase;border-bottom:2px solid ", YELLOW,
          ";padding-bottom:4px;margin-bottom:6px;'>",
          DEN_Z_URB, "</div>",
          "<div style='font-size:12px;'>",
          "<b>", title, ":</b> ", sprintf(fmt, value), "</div></div>"
        )
      )
    if (legend) {
      m <- m %>% addLegend(
        pal = pal, values = c(0, domain),
        title = title, position = legend_pos,
        labFormat = labelFormat(digits = 3)
      )
    }
    m
  }

  # --- FREQUENZA (singola mappa, invariata) ---
  output$map_freq <- renderLeaflet({
    make_leaf(freq_pred(), MAX_FREQ, "Expected Accidents", legend = TRUE)
  })

  # --- SEVERITY: sync LEFT (no ped) + RIGHT (ped) ---
  # leafsync::sync() restituisce un htmlwidget normale, non un widget Shiny nativo:
  # va wrappato in renderUI → uiOutput.
  output$sync_sev <- renderUI({
    m_no  <- make_leaf(sev_pred_no(),  MAX_SEV, "P(Severe) — No Ped",  "%.3f", legend = TRUE)
    m_yes <- make_leaf(sev_pred_yes(), MAX_SEV, "P(Severe) — Ped",     "%.3f", legend = TRUE)
    sync(m_no, m_yes, ncol = 2, sync = "all")
  })

  # --- RISK: sync LEFT (no ped) + RIGHT (ped) ---
  output$sync_risk <- renderUI({
    freq <- freq_pred()

    risk_no <- freq %>%
      left_join(sev_pred_no(), by = "DEN_Z_URB", suffix = c("_f","_s")) %>%
      mutate(value = value_f * value_s) %>%
      dplyr::select(DEN_Z_URB, value)

    risk_yes <- freq %>%
      left_join(sev_pred_yes(), by = "DEN_Z_URB", suffix = c("_f","_s")) %>%
      mutate(value = value_f * value_s) %>%
      dplyr::select(DEN_Z_URB, value)

    m_no  <- make_leaf(risk_no,  MAX_RISK, "Risk — No Ped", legend = TRUE)
    m_yes <- make_leaf(risk_yes, MAX_RISK, "Risk — Ped",    legend = TRUE)
    sync(m_no, m_yes, ncol = 2, sync = "all")
  })
}

shinyApp(ui, server)

