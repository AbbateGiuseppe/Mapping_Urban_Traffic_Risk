# ============================================================
#  SHINY APP - Expected accidents per road (GLMM)
#  3 panels (one neighborhood each), roads colored yellow->red
#  based on the prediction, month slider (manual/automatic) + filters
#  for time of day (DayPeriod) and weather.
#
#  METRIC: expected accidents per DAY of the weather condition
#           rate = sum(expected) / sum(g_cond)
#           (exposure = condition-days only, WITHOUT road_len)
#
#  INPUT in the app folder (unchanged names):
#    - pred_grid.rds
#    - roads.rds
#    - mappa_quartieri.rds
# ============================================================

library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# ------------------------------------------------------------
# Load data
# ------------------------------------------------------------
pred_grid <- readRDS("pred_grid.rds")
roads     <- readRDS("roads.rds")
conf_q    <- readRDS("mappa_quartieri.rds")
COL_NOME_Q <- "DEN_Z_URB"

# neighborhood boundaries in lat/lon
conf_geo <- conf_q %>%
  st_transform(4326) %>%
  mutate(.qnome = as.character(.data[[COL_NOME_Q]]))

# road geometries in lat/lon
roads_geo <- roads %>%
  mutate(road_id = as.integer(road_id)) %>%
  select(road_id) %>%
  st_transform(4326)

# normalize pred_grid; exposure = condition-days ONLY (g_cond)
pg <- pred_grid %>%
  mutate(
    road_id   = as.integer(as.character(road_id)),
    DEN_Z_URB = as.character(DEN_Z_URB),
    Month     = as.character(Month),
    DayPeriod = as.character(DayPeriod),
    DayType   = as.character(DayType),
    Meteo     = as.character(Meteo),
    espo      = g_cond                # exposure = days of the condition
  )

# month order
mese_ord  <- c("Jan","Feb","Mar","Apr","May","Jun",
               "Jul","Aug","Sep","Oct","Nov","Dec")
mese_nomi <- c(Jan="January",Feb="February",Mar="March",Apr="April",
               May="May",Jun="June",Jul="July",Aug="August",
               Sep="September",Oct="October",Nov="November",Dec="December")
mesi      <- mese_ord[mese_ord %in% unique(pg$Month)]
quartieri <- sort(unique(pg$DEN_Z_URB))
periodi   <- c("All", sort(unique(pg$DayPeriod)))
daytype_lv<- c("All", sort(unique(pg$DayType)))
meteo_lv  <- c("All", sort(unique(pg$Meteo)))

# ------------------------------------------------------------
# METRIC: expected accidents per condition-day
#   rate = sum(expected) / sum(g_cond)
# Continuous scale, square root, domain capped at the 98th percentile.
# ------------------------------------------------------------
tassi_all <- pg %>%
  group_by(DEN_Z_URB, Month, road_id) %>%
  summarise(rate = 365 * sum(expected) / sum(espo), .groups = "drop") %>%   # accidents/year
  filter(is.finite(rate))

# cap at the 95th percentile (used to be 0.98: put 0.98 back here for the original scale)
cap_hi   <- as.numeric(quantile(tassi_all$rate, 0.95, na.rm = TRUE))
rng_rate <- c(0, cap_hi)
pal <- colorNumeric(
  palette = c("#ffffcc","#ffeda0","#fed976","#feb24c","#fd8d3c","#f03b20","#bd0026"),
  domain  = c(0, sqrt(cap_hi)), na.color = "#555"
)
pal_rate <- function(x) pal(sqrt(pmin(x, cap_hi)))

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body{background:#ffffff;color:#1a1a1a;font-family:'Segoe UI',sans-serif}
    .well,.form-group{color:#1a1a1a}
    h3,h4{font-weight:600;color:#1a1a1a}
    .leaflet-container{background:#ffffff}
  "))),
  titlePanel("Expected accidents per road — GLMM"),
  fluidRow(
    column(6,
      sliderInput("mese", "Month", min = 1, max = length(mesi), value = 1, step = 1,
                  animate = animationOptions(interval = 900, loop = TRUE),
                  ticks = FALSE),
      textOutput("mese_lbl")
    )
  ),
  fluidRow(
    column(4, selectInput("dp", "Time of day", choices = periodi, selected = "All")),
    column(4, selectInput("dt", "Day type", choices = daytype_lv, selected = "All")),
    column(4, selectInput("mt", "Weather", choices = meteo_lv, selected = "All"))
  ),
  fluidRow(
    lapply(seq_along(quartieri), function(i) {
      column(4,
        h4(quartieri[i]),
        leafletOutput(paste0("map", i), height = 520)
      )
    })
  ),
  fluidRow(
    column(12,
      div(style="margin-top:14px;font-size:13px",
        tags$b("Expected accidents per year"),
        div(style="display:flex;align-items:center;gap:10px;margin-top:6px",
          span(sprintf("%.2f", rng_rate[1]), style="font-variant-numeric:tabular-nums"),
          div(style=paste0("height:14px;width:320px;border:1px solid #ccc;",
              "background:linear-gradient(90deg,#ffffcc,#ffeda0,#fed976,#feb24c,#fd8d3c,#f03b20,#bd0026)")),
          span(sprintf("%.2f+", rng_rate[2]), style="font-variant-numeric:tabular-nums")
        )
      )
    )
  ),
  helpText("Continuous shared color scale (square-root transform): yellow = low, red = high.")
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {

  output$mese_lbl <- renderText({
    m <- mesi[input$mese]; paste("Selected month:", mese_nomi[[m]])
  })

  dati_mese <- reactive({
    m <- mesi[input$mese]
    d <- pg %>% filter(Month == m)
    if (input$dp != "All") d <- d %>% filter(DayPeriod == input$dp)
    if (input$dt != "All") d <- d %>% filter(DayType == input$dt)
    if (input$mt != "All") d <- d %>% filter(Meteo == input$mt)
    d %>%
      group_by(DEN_Z_URB, road_id) %>%
      summarise(rate = 365 * sum(expected) / sum(espo), .groups = "drop") %>%   # accidents/year
      filter(is.finite(rate))
  })

  # base maps
  for (i in seq_along(quartieri)) {
    local({
      ii <- i
      output[[paste0("map", ii)]] <- renderLeaflet({
        q  <- quartieri[ii]
        rid <- pg %>% filter(DEN_Z_URB == q) %>% pull(road_id) %>% unique()
        g  <- roads_geo %>% filter(road_id %in% rid)
        poly <- conf_geo %>% filter(.qnome == q)
        m <- leaflet(g) %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addPolylines(weight = 4, color = "#cccccc", group = "base")
        if (nrow(poly) > 0) {
          m <- m %>%
            addPolygons(data = poly, fill = FALSE,
                        color = "#222222", weight = 2.5, opacity = 1,
                        group = "confine") %>%
            fitBounds(lng1 = as.numeric(sf::st_bbox(poly)["xmin"]),
                      lat1 = as.numeric(sf::st_bbox(poly)["ymin"]),
                      lng2 = as.numeric(sf::st_bbox(poly)["xmax"]),
                      lat2 = as.numeric(sf::st_bbox(poly)["ymax"]))
        }
        m
      })
    })
  }

  # update the road colors
  observe({
    d <- dati_mese()
    for (i in seq_along(quartieri)) {
      local({
        ii <- i; q <- quartieri[ii]
        dq <- d %>% filter(DEN_Z_URB == q)
        g  <- roads_geo %>%
          filter(road_id %in% dq$road_id) %>%
          left_join(dq, by = "road_id")
        leafletProxy(paste0("map", ii), data = g) %>%
          clearGroup("pred") %>%
          addPolylines(weight = 2, opacity = 1,
                       color = ~pal_rate(rate), group = "pred",
                       label = ~sprintf("road %s — %.2f accidents/year", road_id, rate))
      })
    }
  })
}

shinyApp(ui, server)
