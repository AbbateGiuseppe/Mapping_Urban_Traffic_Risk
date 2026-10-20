# ============================================================
#  app_glmm.R — Predictive Statistics (GLMM)
#  Scenario-based predictions, fixed colour scale
#  File: acc.rds, roads.rds, mappa_quartieri.rds, pred_grid.rds
# ============================================================

library(shiny)
library(leaflet)
library(leafgl)
library(sf)
library(dplyr)

select  <- dplyr::select
filter  <- dplyr::filter
mutate  <- dplyr::mutate

YELLOW <- "#FFD93D"; BLACK <- "#0D0D0D"; RED <- "#E83A2F"
WHITE  <- "#FFFFFF"; GREY  <- "#1a1a1a"; DARK <- "#141414"

# ------------------------------------------------------------
# DATA
# ------------------------------------------------------------
acc             <- readRDS("acc.rds")
roads           <- readRDS("roads.rds")
mappa_quartieri <- readRDS("mappa_quartieri.rds")
pred_grid       <- readRDS("pred_grid.rds")

punti <- acc %>%
  sf::st_drop_geometry() %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  mutate(
    Year      = as.integer(Year),
    Month     = factor(Month, levels = month.abb),
    DayPeriod = as.character(DayPeriod),
    Meteo     = as.character(rain_day),
    Quartiere = as.character(DEN_Z_URB)
  ) %>%
  dplyr::select(longitude, latitude, Year, Month, DayPeriod, Meteo, Quartiere)

quartieri_target <- c("Esquilino", "Torre Angela", "Centro Storico")

# ------------------------------------------------------------
# GEOMETRIES
# ------------------------------------------------------------
roads_simp <- roads %>%
  mutate(road_id = as.integer(road_id)) %>%
  sf::st_simplify(dTolerance = 0.0001, preserveTopology = TRUE)

quartieri_map <- mappa_quartieri %>%
  group_by(DEN_Z_URB) %>%
  summarise(.groups = "drop") %>%
  sf::st_simplify(dTolerance = 0.0001, preserveTopology = TRUE)

quartieri_lines <- sf::st_cast(quartieri_map, "MULTILINESTRING")

quartieri_target_lines <- quartieri_lines %>%
  filter(DEN_Z_URB %in% quartieri_target)

.roads_in_q <- function(q) {
  bb   <- sf::st_bbox(quartieri_map %>% filter(DEN_Z_URB == q))
  poly <- sf::st_as_sfc(bb)
  sf::st_filter(roads_simp, poly, .predicate = sf::st_intersects)
}
roads_esq <- .roads_in_q("Esquilino")
roads_tor <- .roads_in_q("Torre Angela")
roads_cen <- .roads_in_q("Centro Storico")

border_esq <- quartieri_target_lines %>% filter(DEN_Z_URB == "Esquilino")
border_tor <- quartieri_target_lines %>% filter(DEN_Z_URB == "Torre Angela")
border_cen <- quartieri_target_lines %>% filter(DEN_Z_URB == "Centro Storico")

.bbox <- function(q) {
  bb <- sf::st_bbox(quartieri_map %>% filter(DEN_Z_URB == q))
  list(lng=mean(c(bb["xmin"],bb["xmax"])), lat=mean(c(bb["ymin"],bb["ymax"])))
}
bb_esq <- .bbox("Esquilino")
bb_tor <- .bbox("Torre Angela")
bb_cen <- .bbox("Centro Storico")

# ------------------------------------------------------------
# FIXED COLOR SCALE — computed across all possible scenarios
# ------------------------------------------------------------
ids_target <- unique(c(roads_esq$road_id, roads_tor$road_id, roads_cen$road_id))

all_expected <- pred_grid %>%
  mutate(road_id = as.integer(as.character(road_id))) %>%
  filter(road_id %in% ids_target) %>%
  pull(expected)

GLOBAL_RANGE_LINEAR <- range(all_expected, na.rm = TRUE)
GLOBAL_RANGE_LOG    <- range(log1p(all_expected), na.rm = TRUE)

PAL_LINEAR <- colorNumeric(c("#FFFDE7","#FFD93D","#E8612F","#8B0000"),
                           domain=GLOBAL_RANGE_LINEAR, na.color="#cccccc")
PAL_LOG    <- colorNumeric(c("#FFFDE7","#FFD93D","#E8612F","#8B0000"),
                           domain=GLOBAL_RANGE_LOG, na.color="#cccccc")

# ------------------------------------------------------------
# CSS
# ------------------------------------------------------------
custom_css <- tags$style(HTML(sprintf("
  @import url('https://fonts.googleapis.com/css2?family=Barlow+Condensed:wght@400;600;700;900&family=Barlow:wght@300;400;500&display=swap');
  * { box-sizing:border-box; margin:0; padding:0; }
  body { background:%s; font-family:'Barlow',sans-serif; color:%s; }
  .app-header { background:%s; padding:18px 32px 14px; border-bottom:4px solid %s; display:flex; align-items:baseline; gap:18px; }
  .app-title { font-family:'Barlow Condensed',sans-serif; font-weight:900; font-size:28px; letter-spacing:1px; color:%s; text-transform:uppercase; }
  .app-subtitle { font-family:'Barlow Condensed',sans-serif; font-weight:600; font-size:13px; letter-spacing:2px; color:%s; text-transform:uppercase; opacity:0.6; }
  .sidebar-panel, .well { background:%s !important; border:none !important; border-right:1px solid rgba(255,255,255,0.08) !important; border-radius:0 !important; padding:24px 20px !important; min-height:calc(100vh - 80px); }
  .section-label { font-family:'Barlow Condensed',sans-serif; font-weight:700; font-size:11px; letter-spacing:3px; color:%s; text-transform:uppercase; margin:20px 0 10px; padding-bottom:5px; border-bottom:1.5px solid rgba(255,255,255,0.15); opacity:0.55; }
  .form-group label { font-family:'Barlow Condensed',sans-serif; font-weight:600; font-size:12px; letter-spacing:1.5px; text-transform:uppercase; color:rgba(255,255,255,0.6); margin-bottom:4px; }
  .form-control { background:rgba(255,255,255,0.08) !important; border:1px solid rgba(255,255,255,0.18) !important; border-radius:3px !important; color:%s !important; font-family:'Barlow',sans-serif !important; font-size:13px !important; padding:7px 10px !important; height:36px !important; }
  .form-control:focus { border-color:%s !important; box-shadow:0 0 0 2px rgba(255,217,61,0.25) !important; }
  select option { background:#1a1a1a; color:%s; }
  .irs--shiny .irs-bar { background:%s !important; border-color:%s !important; }
  .irs--shiny .irs-handle { background:%s !important; border-color:%s !important; }
  .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background:%s !important; color:%s !important; }
  .irs--shiny .irs-line { background:rgba(255,255,255,0.15) !important; }
  .irs--shiny .irs-min, .irs--shiny .irs-max { color:rgba(255,255,255,0.35) !important; font-size:10px; background:transparent !important; }
  .map-desc { position:absolute; top:16px; right:16px; z-index:1000; background:rgba(20,20,20,0.92); border-left:3px solid %s; padding:10px 14px; max-width:260px; pointer-events:none; }
  .map-desc-title { font-family:'Barlow Condensed',sans-serif; font-weight:900; font-size:14px; letter-spacing:1px; text-transform:uppercase; color:%s; }
  .map-desc-sub { font-size:10px; color:rgba(255,255,255,0.55); margin-top:3px; line-height:1.4; }
  .row-label { background:%s; font-family:'Barlow Condensed',sans-serif; font-weight:700; font-size:11px; letter-spacing:3px; text-transform:uppercase; padding:6px 16px; }
  .summary-box { background:%s; padding:12px 16px; margin-top:12px; border-left:3px solid %s; }
",
  DARK,WHITE, YELLOW,BLACK,BLACK,BLACK,
  GREY,WHITE,WHITE,YELLOW,WHITE,
  YELLOW,YELLOW,YELLOW,YELLOW,BLACK,BLACK,
  RED,WHITE, DARK,DARK,YELLOW
)))

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------
.base_map_q <- function(roads_q, border_q, center, zoom=14) {
  leaflet(options=leafletOptions(zoomControl=FALSE,preferCanvas=TRUE)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng=center$lng, lat=center$lat, zoom=zoom) %>%
    addPolylines(data=roads_q,  color="#aaaaaa", weight=0.6, opacity=0.5) %>%
    addPolylines(data=border_q, color=BLACK,     weight=2,   opacity=0.9)
}

.update_obs_points <- function(map_id, d_obs) {
  leafletProxy(map_id) %>% clearGroup("obs")
  if(nrow(d_obs)==0) return()
  d_sf <- d_obs %>% sf::st_as_sf(coords=c("longitude","latitude"), crs=4326)
  leafletProxy(map_id) %>%
    addGlPoints(data=d_sf, group="obs", fillColor=RED, fillOpacity=0.8, radius=4)
}

.draw_pred_fixed <- function(map_id, roads_q, d, scale) {
  roads_pred <- roads_q %>%
    left_join(d, by="road_id") %>%
    filter(!is.na(expected))
  if(nrow(roads_pred)==0) return()

  pal          <- if(scale=="log") PAL_LOG else PAL_LINEAR
  global_range <- if(scale=="log") GLOBAL_RANGE_LOG else GLOBAL_RANGE_LINEAR
  vals         <- if(scale=="log") log1p(roads_pred$expected) else roads_pred$expected

  leafletProxy(map_id) %>%
    clearGroup("pred") %>%
    clearControls() %>%
    addPolylines(
      data=roads_pred, color=pal(vals),
      weight=2.5, opacity=0.9, group="pred",
      popup=~paste0(
        "<div style='font-family:Barlow Condensed,sans-serif;background:#141414;",
        "color:#fff;padding:8px;border-left:3px solid #FFD93D;'>",
        "<b>Road ",road_id," — ",highway,"</b><br/>",
        "<span style='color:#FFD93D;'>E[acc]:</span> ",round(expected,4),"</div>"
      )
    ) %>%
    addLegend(pal=pal, values=global_range,
              title=if(scale=="log")"log(1+E[acc])" else "E[accidents]",
              position="bottomright",
              labFormat=labelFormat(digits=3))
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  custom_css,
  div(class="app-header",
    span(class="app-title","GLMM Predictive Dashboard"),
    span(class="app-subtitle","Rome Road Accidents — NB2 GLMM Predictions")
  ),
  div(style="display:flex;height:calc(100vh - 80px);",

    div(style="width:220px;flex-shrink:0;",
      div(class="sidebar-panel",
        div(class="section-label","Scenario"),
        selectInput("pred_month","Month",
                    choices=sort(unique(as.character(pred_grid$Month))),selected="Jan"),
        selectInput("pred_period","Day Period",
                    choices=sort(unique(as.character(pred_grid$DayPeriod))),selected="Morning"),
        selectInput("pred_daytype","Day Type",
                    choices=sort(unique(as.character(pred_grid$DayType))),selected="weekday"),
        selectInput("pred_meteo","Weather",choices=c("Dry","Rain"),selected="Dry"),
        tags$hr(style="border-color:rgba(255,255,255,0.1);margin:16px 0;"),
        div(class="section-label","Color Scale"),
        radioButtons("pred_scale",NULL,choices=c("Linear"="linear","Log"="log"),
                     selected="log",inline=TRUE),
        tags$p(style="color:rgba(255,255,255,0.35);font-size:10px;margin-top:6px;line-height:1.5;",
               "Scale is fixed across all scenarios for fair visual comparison."),
        tags$hr(style="border-color:rgba(255,255,255,0.1);margin:16px 0;"),
        uiOutput("pred_summary")
      )
    ),

    div(style="flex:1;display:flex;flex-direction:column;overflow:hidden;",

      div(class="row-label",
          style=sprintf("color:%s;border-bottom:3px solid %s;",YELLOW,YELLOW),
          "▲  PREDICTED — E[accidents] per road segment (fixed colour scale)"),

      div(style="flex:1;display:flex;border-bottom:3px solid #FFD93D;",
        div(style="flex:1;position:relative;border-right:1px solid rgba(255,255,255,0.1);",
          div(class="map-desc",div(class="map-desc-title","Esquilino"),div(class="map-desc-sub","Predicted")),
          leafletOutput("mp_esq",height="100%",width="100%")),
        div(style="flex:1;position:relative;border-right:1px solid rgba(255,255,255,0.1);",
          div(class="map-desc",div(class="map-desc-title","Torre Angela"),div(class="map-desc-sub","Predicted")),
          leafletOutput("mp_tor",height="100%",width="100%")),
        div(style="flex:1;position:relative;",
          div(class="map-desc",div(class="map-desc-title","Centro Storico"),div(class="map-desc-sub","Predicted")),
          leafletOutput("mp_cen",height="100%",width="100%"))
      ),

      div(class="row-label",
          style="color:rgba(255,255,255,0.6);border-bottom:1px solid rgba(255,255,255,0.1);",
          "▼  OBSERVED — recorded accident locations"),

      div(style="flex:1;display:flex;",
        div(style="flex:1;position:relative;border-right:1px solid rgba(255,255,255,0.1);",
          div(class="map-desc",div(class="map-desc-title","Esquilino"),div(class="map-desc-sub","Observed")),
          leafletOutput("mo_esq",height="100%",width="100%")),
        div(style="flex:1;position:relative;border-right:1px solid rgba(255,255,255,0.1);",
          div(class="map-desc",div(class="map-desc-title","Torre Angela"),div(class="map-desc-sub","Observed")),
          leafletOutput("mo_tor",height="100%",width="100%")),
        div(style="flex:1;position:relative;",
          div(class="map-desc",div(class="map-desc-title","Centro Storico"),div(class="map-desc-sub","Observed")),
          leafletOutput("mo_cen",height="100%",width="100%"))
      )
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {

  pred_scenario <- reactive({
    pred_grid %>%
      filter(
        as.character(Month)     == input$pred_month,
        as.character(DayPeriod) == input$pred_period,
        as.character(DayType)   == input$pred_daytype,
        as.character(Meteo)     == input$pred_meteo
      ) %>%
      group_by(road_id) %>%
      summarise(expected=mean(expected,na.rm=TRUE), .groups="drop") %>%
      mutate(road_id=as.integer(as.character(road_id)))
  })

  obs_q <- function(q) reactive({
    punti %>% filter(
      Quartiere==q,
      as.character(Month)     == input$pred_month,
      as.character(DayPeriod) == input$pred_period,
      as.character(Meteo)     == input$pred_meteo
    )
  })
  obs_esq <- obs_q("Esquilino")
  obs_tor <- obs_q("Torre Angela")
  obs_cen <- obs_q("Centro Storico")

  output$pred_summary <- renderUI({
    d <- pred_scenario()
    if(nrow(d)==0) return(NULL)
    tags$div(class="summary-box",
      tags$div(style=sprintf("color:%s;font-family:'Barlow Condensed',sans-serif;font-size:11px;letter-spacing:2px;text-transform:uppercase;margin-bottom:8px;opacity:0.55;",WHITE),"Expected accidents"),
      fluidRow(
        column(6,
          tags$div(style=sprintf("color:%s;font-family:'Barlow Condensed',sans-serif;font-weight:900;font-size:22px;",YELLOW),round(mean(d$expected,na.rm=TRUE),3)),
          tags$div(style="color:rgba(255,255,255,0.4);font-size:11px;","mean / road")),
        column(6,
          tags$div(style=sprintf("color:%s;font-family:'Barlow Condensed',sans-serif;font-weight:900;font-size:22px;",RED),round(max(d$expected,na.rm=TRUE),3)),
          tags$div(style="color:rgba(255,255,255,0.4);font-size:11px;","max / road"))
      ),
      tags$div(style="margin-top:10px;color:rgba(255,255,255,0.35);font-size:10px;",
        tags$b(style=sprintf("color:%s;",YELLOW),"Roads: "), nrow(d))
    )
  })

  output$mp_esq <- renderLeaflet({ .base_map_q(roads_esq,border_esq,bb_esq) })
  output$mp_tor <- renderLeaflet({ .base_map_q(roads_tor,border_tor,bb_tor) })
  output$mp_cen <- renderLeaflet({ .base_map_q(roads_cen,border_cen,bb_cen) })
  output$mo_esq <- renderLeaflet({ .base_map_q(roads_esq,border_esq,bb_esq) })
  output$mo_tor <- renderLeaflet({ .base_map_q(roads_tor,border_tor,bb_tor) })
  output$mo_cen <- renderLeaflet({ .base_map_q(roads_cen,border_cen,bb_cen) })

  observe({
    d <- pred_scenario(); scale <- input$pred_scale
    .draw_pred_fixed("mp_esq", roads_esq, d, scale)
    .draw_pred_fixed("mp_tor", roads_tor, d, scale)
    .draw_pred_fixed("mp_cen", roads_cen, d, scale)
  })

  observe({
    .update_obs_points("mo_esq", obs_esq())
    .update_obs_points("mo_tor", obs_tor())
    .update_obs_points("mo_cen", obs_cen())
  })
}

shinyApp(ui, server)
