# ============================================================
#  app.R — Road Accident Dashboard Roma
#  Startup veloce: precomputed.RData carica FDA + geometrie
# ============================================================

library(shiny)
library(leaflet)
library(leafgl)
library(sf)
library(dplyr)
library(fda)
library(DT)

select  <- dplyr::select
filter  <- dplyr::filter
mutate  <- dplyr::mutate
arrange <- dplyr::arrange
rename  <- dplyr::rename
count   <- dplyr::count

YELLOW <- "#FFD93D"; BLACK <- "#0D0D0D"; RED <- "#E83A2F"
WHITE  <- "#FFFFFF"; GREY  <- "#1a1a1a"; DARK <- "#141414"

# ------------------------------------------------------------
# DATI
# ------------------------------------------------------------

acc             <- readRDS("acc.rds")
roads           <- readRDS("roads.rds")
mappa_quartieri <- readRDS("mappa_quartieri.rds")
pred_grid       <- readRDS("pred_grid.rds")

library(dplyr); library(sf)

quartieri_target <- c("Esquilino", "Torre Angela", "Centro Storico")

roads_simp <- roads %>%
  sf::st_simplify(dTolerance = 0.0001, preserveTopology = TRUE)

quartieri_map <- mappa_quartieri %>%
  group_by(DEN_Z_URB) %>%
  summarise(.groups = "drop") %>%
  sf::st_simplify(dTolerance = 0.0001, preserveTopology = TRUE)

.roads_in_q <- function(q) {
  bb   <- sf::st_bbox(quartieri_map %>% filter(DEN_Z_URB == q))
  poly <- sf::st_as_sfc(bb)
  sf::st_filter(roads_simp, poly, .predicate = sf::st_intersects)
}
roads_esq <- .roads_in_q("Esquilino")
roads_tor <- .roads_in_q("Torre Angela")
roads_cen <- .roads_in_q("Centro Storico")

cat("roads_esq:", nrow(roads_esq), "\n")
cat("roads_tor:", nrow(roads_tor), "\n")
cat("roads_cen:", nrow(roads_cen), "\n")

ids_target <- unique(as.integer(c(
  roads_esq$road_id, roads_tor$road_id, roads_cen$road_id
)))
cat("ids_target:", length(ids_target), "\n")

road_totals <- pred_grid %>%
  mutate(road_id = as.integer(as.character(road_id))) %>%
  filter(road_id %in% ids_target) %>%
  group_by(road_id) %>%
  summarise(pred_tot = sum(expected, na.rm = TRUE), .groups = "drop")

cat("road_totals:", nrow(road_totals), "\n")
head(road_totals)

class(roads_esq$road_id)
head(as.integer(roads_esq$road_id))
head(as.integer(as.character(roads_esq$road_id)))

test_join <- roads_esq %>%
  dplyr::mutate(road_id = as.integer(as.character(road_id))) %>%
  dplyr::left_join(road_totals, by = "road_id")

cat("match trovati:", sum(!is.na(test_join$pred_tot)), "su", nrow(test_join), "\n")






acc       <- readRDS("acc.rds")
pred_grid <- readRDS("pred_grid.rds")
load("precomputed.RData")          # fd_*, pca_*, roads_*, border_*, bb_*, quartieri_*
load("modelli_incidenti_strade.RData")

punti <- acc %>%
  sf::st_drop_geometry() %>%
  filter(!is.na(longitude), !is.na(latitude)) %>%
  mutate(
    Year      = as.integer(Year),
    Month     = factor(Month, levels = month.abb),
    DayPeriod = as.character(DayPeriod),
    Meteo     = as.character(rain_day),
    Quartiere = as.character(DEN_Z_URB),
    Hour_int  = as.integer(substr(as.character(Hour), 1, 2))
  ) %>%
  dplyr::select(longitude, latitude, Year, Month, DayPeriod,
                Meteo, Quartiere, Hour_int, Day)

anni             <- sort(unique(punti$Year))
mesi             <- month.abb
quartieri        <- sort(unique(punti$Quartiere))
periodi          <- sort(unique(punti$DayPeriod))
meteo_lev        <- sort(unique(punti$Meteo))
giorni_ord       <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
quartieri_target <- c("Esquilino", "Torre Angela", "Centro Storico")

# roads_simp for the global map (page 1)
roads_simp <- roads_with_zone %>%
  sf::st_simplify(dTolerance = 0.0001, preserveTopology = TRUE)

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
  .nav-tabs { background:%s !important; border-bottom:3px solid %s !important; padding:0 24px; }
  .nav-tabs > li > a { font-family:'Barlow Condensed',sans-serif !important; font-weight:700 !important; font-size:13px !important; letter-spacing:2px !important; text-transform:uppercase !important; color:rgba(255,255,255,0.4) !important; border:none !important; border-bottom:3px solid transparent !important; border-radius:0 !important; padding:14px 22px 11px !important; margin-bottom:-3px !important; background:transparent !important; }
  .nav-tabs > li.active > a, .nav-tabs > li > a:hover { color:%s !important; border-bottom:3px solid %s !important; background:transparent !important; }
  .tab-content { background:%s !important; }
  .sidebar-panel, .well { background:%s !important; border:none !important; border-right:1px solid rgba(255,255,255,0.08) !important; border-radius:0 !important; padding:24px 20px !important; min-height:calc(100vh - 120px); }
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
  .counter-badge { background:%s; color:%s; font-family:'Barlow Condensed',sans-serif; font-weight:700; font-size:14px; letter-spacing:1px; text-transform:uppercase; padding:10px 14px; border-left:3px solid %s; margin-top:16px; }
  .btn-reset { background:transparent !important; border:1px solid rgba(255,255,255,0.25) !important; color:rgba(255,255,255,0.6) !important; font-family:'Barlow Condensed',sans-serif !important; font-weight:600 !important; font-size:12px !important; letter-spacing:2px !important; text-transform:uppercase !important; width:100%% !important; margin-top:16px !important; padding:8px !important; border-radius:2px !important; }
  .btn-reset:hover { border-color:%s !important; color:%s !important; }
  .map-desc { position:absolute; top:16px; right:16px; z-index:1000; background:rgba(20,20,20,0.92); border-left:3px solid %s; padding:10px 14px; max-width:260px; pointer-events:none; }
  .map-desc-title { font-family:'Barlow Condensed',sans-serif; font-weight:900; font-size:14px; letter-spacing:1px; text-transform:uppercase; color:%s; }
  .map-desc-sub { font-size:10px; color:rgba(255,255,255,0.55); margin-top:3px; line-height:1.4; }
  .desc-page { background:%s; padding:28px 32px; color:%s; }
  .desc-page h3 { font-family:'Barlow Condensed',sans-serif; font-weight:900; font-size:22px; letter-spacing:1px; text-transform:uppercase; color:%s; border-bottom:2px solid %s; padding-bottom:8px; }
  .desc-page h4 { font-family:'Barlow Condensed',sans-serif; font-weight:700; font-size:16px; letter-spacing:2px; text-transform:uppercase; color:rgba(255,255,255,0.55); margin:24px 0 12px; }
  .desc-page p.subtitle { color:rgba(255,255,255,0.45); font-size:12px; }
  .desc-page hr { border-color:rgba(255,255,255,0.1); margin:20px 0; }
  .cpv-badge { display:inline-block; font-family:'Barlow Condensed',sans-serif; font-weight:700; font-size:11px; letter-spacing:2px; text-transform:uppercase; padding:3px 10px; border-radius:2px; margin:4px 4px 0 0; }
  .shiny-plot-output { background-color:%s !important; }
  .dataTables_wrapper { color:%s !important; }
  table.dataTable thead th { font-family:'Barlow Condensed',sans-serif !important; font-weight:700 !important; letter-spacing:2px !important; text-transform:uppercase !important; font-size:12px !important; color:%s !important; border-bottom:2px solid %s !important; background:%s !important; }
  table.dataTable tbody tr { background:%s !important; color:%s !important; }
  table.dataTable tbody tr:hover { background:rgba(255,217,61,0.08) !important; }
  .dataTables_filter input, .dataTables_length select { background:rgba(255,255,255,0.08) !important; border:1px solid rgba(255,255,255,0.18) !important; color:%s !important; border-radius:3px !important; }
  .dataTables_info, .dataTables_paginate { color:rgba(255,255,255,0.4) !important; }
  .paginate_button.current { background:%s !important; color:%s !important; border:none !important; }
  .leaflet-control-layers { background:rgba(20,20,20,0.92) !important; border:1px solid rgba(255,255,255,0.15) !important; color:%s !important; font-family:'Barlow Condensed',sans-serif !important; font-size:12px !important; }
  .leaflet-control-layers label { color:%s !important; }
  .row-label { background:%s; color:%s; font-family:'Barlow Condensed',sans-serif; font-weight:700; font-size:11px; letter-spacing:3px; text-transform:uppercase; padding:6px 16px; }
",
                                      DARK,WHITE, YELLOW,BLACK,BLACK,BLACK,
                                      DARK,YELLOW,YELLOW,YELLOW,DARK,
                                      GREY,WHITE,WHITE,YELLOW,WHITE,
                                      YELLOW,YELLOW,YELLOW,YELLOW,BLACK,BLACK,
                                      DARK,WHITE,YELLOW, YELLOW,YELLOW,
                                      RED,WHITE,
                                      DARK,WHITE,WHITE,YELLOW,
                                      DARK,
                                      WHITE,WHITE,YELLOW,GREY,GREY,WHITE,WHITE,
                                      YELLOW,BLACK, WHITE,WHITE,
                                      DARK,YELLOW
)))

# ------------------------------------------------------------
# HELPERS
# ------------------------------------------------------------
.plot_raw <- function(fd_obj, x_labels=NULL) {
  par(bg=DARK,col.axis=WHITE,col.lab=WHITE,col.main=WHITE,fg="grey40")
  plot.fd(fd_obj,main="",xaxt=if(!is.null(x_labels))"n" else "s")
  if(!is.null(x_labels)) axis(1,at=seq_along(x_labels),labels=x_labels,cex.axis=0.8,col.axis=WHITE)
}
.plot_mean_sd <- function(fd_obj, x_labels=NULL) {
  par(bg=DARK,col.axis=WHITE,col.lab=WHITE,col.main=WHITE,fg="grey40")
  media <- mean.fd(fd_obj); std <- sd.fd(fd_obj)
  plot.fd(fd_obj,col="grey30",lty=1,main="",xaxt=if(!is.null(x_labels))"n" else "s")
  if(!is.null(x_labels)) axis(1,at=seq_along(x_labels),labels=x_labels,cex.axis=0.8,col.axis=WHITE)
  lines(media,lwd=3,col=YELLOW)
  lines(media+std,lwd=2,lty=2,col=RED)
  lines(media-std,lwd=2,lty=2,col=RED)
  legend("topright",legend=c("Mean","Mean ± SD"),col=c(YELLOW,RED),lty=c(1,2),lwd=2,cex=0.8,text.col=WHITE,bg=DARK,box.col="grey30")
}
.plot_scores <- function(pca_obj, labels) {
  par(bg=DARK,col.axis=WHITE,col.lab=WHITE,fg="grey40")
  plot(pca_obj$scores[,1],pca_obj$scores[,2],type="n",xlab="FPC1",ylab="FPC2",main="")
  text(pca_obj$scores[,1],pca_obj$scores[,2],labels=labels,cex=0.65,col=YELLOW)
}
.cpv_ui <- function(pca_obj) {
  cpv <- round(cumsum(pca_obj$values[1:3])/sum(pca_obj$values[pca_obj$values>0])*100,1)
  tags$div(style="margin:8px 0;",
           tags$span(class="cpv-badge",style=sprintf("background:%s;color:%s;",YELLOW,BLACK),paste0("FPC1 · ",cpv[1],"%")),
           tags$span(class="cpv-badge",style=sprintf("background:%s;color:%s;",RED,WHITE),paste0("FPC2 · ",cpv[2],"%")),
           tags$span(class="cpv-badge",style="background:rgba(255,255,255,0.15);color:#fff;",paste0("FPC3 · ",cpv[3],"%"))
  )
}
.dt_dark <- function(df) {
  DT::datatable(df,rownames=FALSE,class="table table-sm",
                options=list(pageLength=15,dom="ftp",order=list(list(ncol(df)-1L,"desc"))))
}
.slider_play_btn <- function(id, min_val, max_val, step_val=1, interval_ms=800) {
  sliderInput(id,NULL,min=min_val,max=max_val,value=min_val,step=step_val,
              animate=animationOptions(interval=interval_ms,loop=TRUE,
                                       playButton=tags$button("▶  Play",style=sprintf("background:%s;color:%s;border:none;font-family:'Barlow Condensed',sans-serif;font-weight:700;font-size:12px;letter-spacing:2px;padding:6px 16px;border-radius:2px;cursor:pointer;",YELLOW,BLACK)),
                                       pauseButton=tags$button("⏸  Pause",style=sprintf("background:rgba(255,255,255,0.15);color:%s;border:none;font-family:'Barlow Condensed',sans-serif;font-weight:700;font-size:12px;letter-spacing:2px;padding:6px 16px;border-radius:2px;cursor:pointer;",WHITE))
              )
  )
}
.base_map_q <- function(roads_q, border_q, center, zoom=14) {
  leaflet(options=leafletOptions(zoomControl=FALSE,preferCanvas=TRUE)) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng=center$lng, lat=center$lat, zoom=zoom) %>%
    addPolylines(data=roads_q,  color="#aaaaaa", weight=0.6, opacity=0.5) %>%
    addPolylines(data=border_q, color=BLACK,     weight=2,   opacity=0.9)
}
.update_pred <- function(map_id, roads_q, d, scale) {
  roads_pred <- roads_q %>%
    mutate(road_id=as.integer(as.character(road_id))) %>%
    left_join(d, by="road_id") %>%
    filter(!is.na(expected))
  if(nrow(roads_pred)==0) return()
  vals <- if(scale=="log") log1p(roads_pred$expected) else roads_pred$expected
  pal  <- colorNumeric(c("#FFFDE7","#FFD93D","#E8612F","#8B0000"),
                       domain=range(vals,na.rm=TRUE), na.color="#cccccc")
  leafletProxy(map_id) %>%
    clearGroup("pred") %>%
    removeControl(paste0(map_id,"_leg")) %>%
    addPolylines(data=roads_pred,color=~pal(vals),weight=2.5,opacity=0.9,group="pred",
                 popup=~paste0("<div style='font-family:Barlow Condensed,sans-serif;background:#141414;color:#fff;padding:8px;border-left:3px solid #FFD93D;'><b>",highway,"</b><br/><span style='color:#FFD93D;'>E[acc]:</span> ",round(expected,4),"</div>")
    ) %>%
    addLegend(pal=pal,values=vals,title=if(scale=="log")"log(1+E)" else "E[acc]",
              position="bottomright",layerId=paste0(map_id,"_leg"),labFormat=labelFormat(digits=3))
}
.update_obs <- function(map_id, d_obs) {
  leafletProxy(map_id) %>% clearGroup("obs")
  if(nrow(d_obs)==0) return()
  d_sf <- d_obs %>% sf::st_as_sf(coords=c("longitude","latitude"),crs=4326)
  leafletProxy(map_id) %>%
    addGlPoints(data=d_sf,group="obs",fillColor=RED,fillOpacity=0.8,radius=4)
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  custom_css,
  div(class="app-header",
      span(class="app-title","Road Accident Dashboard"),
      span(class="app-subtitle","Rome — 2010–2022")
  ),
  tabsetPanel(id="main_tabs",
              
              # ======================================================
              # PAGINA 1 — ACCIDENT MAP
              # ======================================================
              tabPanel(title="ACCIDENT MAP",
                       div(style="display:flex;height:calc(100vh - 120px);",
                           div(style="width:240px;flex-shrink:0;",
                               div(class="sidebar-panel",
                                   div(class="section-label","Filters"),
                                   sliderInput("anno","Year",min=min(anni),max=max(anni),value=c(min(anni),max(anni)),step=1,sep=""),
                                   selectInput("mese","Month",choices=c("All"="All",setNames(mesi,mesi)),selected="All"),
                                   selectInput("quartiere","Neighbourhood",choices=c("All"="All",setNames(quartieri,quartieri)),selected="All"),
                                   selectInput("periodo","Day Period",choices=c("All"="All",setNames(periodi,periodi)),selected="All"),
                                   selectInput("meteo","Weather",choices=c("All"="All",setNames(meteo_lev,meteo_lev)),selected="All"),
                                   actionButton("reset","↺  Reset Filters",class="btn-reset"),
                                   uiOutput("contatore")
                               )
                           ),
                           div(style="flex:1;position:relative;",
                               div(class="map-desc",div(class="map-desc-title","Accident Locations"),div(class="map-desc-sub","GPS coordinates of recorded accidents.")),
                               leafletOutput("mappa",height="100%",width="100%")
                           )
                       )
              ),
              
              # ======================================================
              # PAGINA 2 — DESCRIPTIVE STATISTICS
              # ======================================================
              tabPanel(title="DESCRIPTIVE STATISTICS",
                       div(class="desc-page",
                           h3("Functional Accident Profiles by Neighbourhood"),
                           tags$p(class="subtitle","Each curve = one neighbourhood. Fourier basis (periodic domain)."),
                           tags$hr(),
                           h4("Raw Functional Profiles"),
                           fluidRow(
                             column(4,plotOutput("fda_mese_raw",height="260px"),tags$p(class="subtitle",style="text-align:center;margin-top:4px;","Monthly")),
                             column(4,plotOutput("fda_ora_raw", height="260px"),tags$p(class="subtitle",style="text-align:center;margin-top:4px;","Hourly")),
                             column(4,plotOutput("fda_day_raw", height="260px"),tags$p(class="subtitle",style="text-align:center;margin-top:4px;","Weekly"))
                           ),
                           tags$hr(),
                           h4("Mean Function ± Standard Deviation"),
                           fluidRow(
                             column(4,plotOutput("fda_mese_sd",height="260px")),
                             column(4,plotOutput("fda_ora_sd", height="260px")),
                             column(4,plotOutput("fda_day_sd", height="260px"))
                           ),
                           tags$hr(),
                           h4("Functional PCA — Modes of Variation"),
                           tabsetPanel(
                             tabPanel("Monthly",br(),plotOutput("fpca_mese",height="300px"),uiOutput("cpv_mese")),
                             tabPanel("Hourly", br(),plotOutput("fpca_ora", height="300px"),uiOutput("cpv_ora")),
                             tabPanel("Weekly", br(),plotOutput("fpca_day", height="300px"),uiOutput("cpv_day"))
                           ),
                           tags$hr(),
                           h4("FPCA Score Scatter — Neighbourhood Positioning"),
                           fluidRow(
                             column(4,plotOutput("scores_mese",height="280px"),tags$p(class="subtitle",style="text-align:center;margin-top:4px;","Monthly scores")),
                             column(4,plotOutput("scores_ora", height="280px"),tags$p(class="subtitle",style="text-align:center;margin-top:4px;","Hourly scores")),
                             column(4,plotOutput("scores_day", height="280px"),tags$p(class="subtitle",style="text-align:center;margin-top:4px;","Weekly scores"))
                           ),
                           tags$hr(),
                           h3("Peak Accident Periods by Neighbourhood",style="margin-top:32px;"),
                           tags$p(class="subtitle","Ranked by total accidents. Use slider or Play to animate."),
                           tabsetPanel(
                             tabPanel("By Month",br(),
                                      fluidRow(column(9,.slider_play_btn("slider_month",1,12,1,900)),column(3,uiOutput("label_month"))),br(),
                                      DT::dataTableOutput("tbl_month")),
                             tabPanel("By Hour",br(),
                                      fluidRow(column(9,.slider_play_btn("slider_hour",0,23,1,600)),column(3,uiOutput("label_hour"))),br(),
                                      DT::dataTableOutput("tbl_hour")),
                             tabPanel("By Day of Week",br(),
                                      fluidRow(column(9,.slider_play_btn("slider_day",1,7,1,1000)),column(3,uiOutput("label_day"))),br(),
                                      DT::dataTableOutput("tbl_day"))
                           )
                       )
              ),
              
              # ======================================================
              # PAGINA 3 — PREDICTIVE STATISTICS
              # ======================================================
              tabPanel(title="PREDICTIVE STATISTICS",
                       div(style="display:flex;height:calc(100vh - 120px);",
                           
                           div(style="width:220px;flex-shrink:0;",
                               div(class="sidebar-panel",
                                   div(class="section-label","Scenario"),
                                   selectInput("pred_month","Month",choices=sort(unique(as.character(pred_grid$Month))),selected="Jan"),
                                   selectInput("pred_period","Day Period",choices=sort(unique(as.character(pred_grid$DayPeriod))),selected="Morning"),
                                   selectInput("pred_daytype","Day Type",choices=sort(unique(as.character(pred_grid$DayType))),selected="weekday"),
                                   selectInput("pred_meteo","Weather",choices=c("Dry","Rain"),selected="Dry"),
                                   tags$hr(style="border-color:rgba(255,255,255,0.1);margin:16px 0;"),
                                   div(class="section-label","Color Scale"),
                                   radioButtons("pred_scale",NULL,choices=c("Linear"="linear","Log"="log"),selected="log",inline=TRUE),
                                   tags$hr(style="border-color:rgba(255,255,255,0.1);margin:16px 0;"),
                                   uiOutput("pred_summary")
                               )
                           ),
                           
                           div(style="flex:1;display:flex;flex-direction:column;overflow:hidden;",
                               
                               # label predicted
                               div(class="row-label",style=sprintf("border-bottom:3px solid %s;color:%s;",YELLOW,YELLOW),
                                   "▲  PREDICTED — E[accidents] per road segment"),
                               
                               # riga mappe predicted
                               div(style="flex:1;display:flex;border-bottom:3px solid #FFD93D;",
                                   div(style="flex:1;position:relative;border-right:1px solid rgba(255,255,255,0.1);",
                                       div(class="map-desc",div(class="map-desc-title","Esquilino"),div(class="map-desc-sub","Predicted")),
                                       leafletOutput("mp_esq",height="100%",width="100%")
                                   ),
                                   div(style="flex:1;position:relative;border-right:1px solid rgba(255,255,255,0.1);",
                                       div(class="map-desc",div(class="map-desc-title","Torre Angela"),div(class="map-desc-sub","Predicted")),
                                       leafletOutput("mp_tor",height="100%",width="100%")
                                   ),
                                   div(style="flex:1;position:relative;",
                                       div(class="map-desc",div(class="map-desc-title","Centro Storico"),div(class="map-desc-sub","Predicted")),
                                       leafletOutput("mp_cen",height="100%",width="100%")
                                   )
                               ),
                               
                               # label observed
                               div(class="row-label",style="color:rgba(255,255,255,0.6);border-bottom:1px solid rgba(255,255,255,0.1);",
                                   "▼  OBSERVED — recorded accident locations"),
                               
                               # riga mappe observed
                               div(style="flex:1;display:flex;",
                                   div(style="flex:1;position:relative;border-right:1px solid rgba(255,255,255,0.1);",
                                       div(class="map-desc",div(class="map-desc-title","Esquilino"),div(class="map-desc-sub","Observed")),
                                       leafletOutput("mo_esq",height="100%",width="100%")
                                   ),
                                   div(style="flex:1;position:relative;border-right:1px solid rgba(255,255,255,0.1);",
                                       div(class="map-desc",div(class="map-desc-title","Torre Angela"),div(class="map-desc-sub","Observed")),
                                       leafletOutput("mo_tor",height="100%",width="100%")
                                   ),
                                   div(style="flex:1;position:relative;",
                                       div(class="map-desc",div(class="map-desc-title","Centro Storico"),div(class="map-desc-sub","Observed")),
                                       leafletOutput("mo_cen",height="100%",width="100%")
                                   )
                               )
                           )
                       )
              )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  # ============================================================
  # PAGINA 1
  # ============================================================
  observeEvent(input$reset, {
    updateSliderInput(session,"anno",value=c(min(anni),max(anni)))
    updateSelectInput(session,"mese",      selected="All")
    updateSelectInput(session,"quartiere", selected="All")
    updateSelectInput(session,"periodo",   selected="All")
    updateSelectInput(session,"meteo",     selected="All")
  })
  
  dati_filtrati <- reactive({
    d <- punti %>% filter(Year>=input$anno[1], Year<=input$anno[2])
    if(input$mese      !="All") d <- d %>% filter(as.character(Month)==input$mese)
    if(input$quartiere !="All") d <- d %>% filter(Quartiere==input$quartiere)
    if(input$periodo   !="All") d <- d %>% filter(DayPeriod==input$periodo)
    if(input$meteo     !="All") d <- d %>% filter(Meteo==input$meteo)
    d
  })
  
  output$contatore <- renderUI({
    div(class="counter-badge", format(nrow(dati_filtrati()),big.mark="."), " accidents")
  })
  
  output$mappa <- renderLeaflet({
    leaflet(options=leafletOptions(zoomControl=TRUE,preferCanvas=TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng=12.496,lat=41.902,zoom=12) %>%
      addPolylines(data=roads_simp,    color="#888888",weight=0.8,opacity=0.5,group="Roads") %>%
      addPolylines(data=quartieri_lines,color=BLACK,   weight=2,  opacity=0.85,group="Neighbourhoods") %>%
      addLayersControl(overlayGroups=c("Roads","Neighbourhoods"),
                       options=layersControlOptions(collapsed=FALSE),position="bottomleft")
  })
  
  observe({
    d <- dati_filtrati()
    d_sf <- d %>% sf::st_as_sf(coords=c("longitude","latitude"),crs=4326)
    leafletProxy("mappa") %>%
      clearGroup("accidents") %>%
      addGlPoints(data=d_sf,group="accidents",fillColor=RED,fillOpacity=0.7,radius=3)
  })
  
  # ============================================================
  # PAGINA 2
  # ============================================================
  output$fda_mese_raw <- renderPlot({ .plot_raw(fd_mese,month.abb) }, bg=DARK)
  output$fda_ora_raw  <- renderPlot({ par(bg=DARK,col.axis=WHITE,col.lab=WHITE,fg="grey40"); plot.fd(fd_ora,main="") }, bg=DARK)
  output$fda_day_raw  <- renderPlot({ .plot_raw(fd_day,giorni_ord) }, bg=DARK)
  output$fda_mese_sd  <- renderPlot({ .plot_mean_sd(fd_mese,month.abb) }, bg=DARK)
  output$fda_ora_sd   <- renderPlot({ .plot_mean_sd(fd_ora) },            bg=DARK)
  output$fda_day_sd   <- renderPlot({ .plot_mean_sd(fd_day,giorni_ord) }, bg=DARK)
  output$fpca_mese <- renderPlot({ par(mfrow=c(1,3),bg=DARK,col.axis=WHITE,col.lab=WHITE,fg="grey40"); plot.pca.fd(pca_m,nx=100,harm=c(1,2,3)); par(mfrow=c(1,1)) }, bg=DARK)
  output$fpca_ora  <- renderPlot({ par(mfrow=c(1,3),bg=DARK,col.axis=WHITE,col.lab=WHITE,fg="grey40"); plot.pca.fd(pca_o,nx=100,harm=c(1,2,3)); par(mfrow=c(1,1)) }, bg=DARK)
  output$fpca_day  <- renderPlot({ par(mfrow=c(1,3),bg=DARK,col.axis=WHITE,col.lab=WHITE,fg="grey40"); plot.pca.fd(pca_d,nx=100,harm=c(1,2,3)); par(mfrow=c(1,1)) }, bg=DARK)
  output$cpv_mese <- renderUI({ .cpv_ui(pca_m) })
  output$cpv_ora  <- renderUI({ .cpv_ui(pca_o) })
  output$cpv_day  <- renderUI({ .cpv_ui(pca_d) })
  output$scores_mese <- renderPlot({ .plot_scores(pca_m,quartieri) }, bg=DARK)
  output$scores_ora  <- renderPlot({ .plot_scores(pca_o,quartieri) }, bg=DARK)
  output$scores_day  <- renderPlot({ .plot_scores(pca_d,quartieri) }, bg=DARK)
  
  output$label_month <- renderUI({ tags$div(style=sprintf("color:%s;font-family:'Barlow Condensed',sans-serif;font-weight:900;font-size:28px;padding-top:2px;",YELLOW), month.abb[input$slider_month]) })
  output$label_hour  <- renderUI({ tags$div(style=sprintf("color:%s;font-family:'Barlow Condensed',sans-serif;font-weight:900;font-size:28px;padding-top:2px;",YELLOW), sprintf("%02d:00",input$slider_hour)) })
  output$label_day   <- renderUI({ tags$div(style=sprintf("color:%s;font-family:'Barlow Condensed',sans-serif;font-weight:900;font-size:28px;padding-top:2px;",YELLOW), giorni_ord[input$slider_day]) })
  
  output$tbl_month <- DT::renderDataTable({
    sel <- month.abb[input$slider_month]
    punti %>% filter(as.character(Month)==sel) %>% count(Quartiere,name="Accidents") %>%
      left_join(punti %>% count(Quartiere,name="Total"),by="Quartiere") %>%
      arrange(desc(Accidents)) %>% dplyr::rename(Neighbourhood=Quartiere) %>% .dt_dark()
  })
  output$tbl_hour <- DT::renderDataTable({
    punti %>% filter(!is.na(Hour_int),Hour_int==input$slider_hour) %>% count(Quartiere,name="Accidents") %>%
      left_join(punti %>% count(Quartiere,name="Total"),by="Quartiere") %>%
      arrange(desc(Accidents)) %>% dplyr::rename(Neighbourhood=Quartiere) %>% .dt_dark()
  })
  output$tbl_day <- DT::renderDataTable({
    sel <- giorni_ord[input$slider_day]
    punti %>% filter(!is.na(Day),Day==sel) %>% count(Quartiere,name="Accidents") %>%
      left_join(punti %>% count(Quartiere,name="Total"),by="Quartiere") %>%
      arrange(desc(Accidents)) %>% dplyr::rename(Neighbourhood=Quartiere) %>% .dt_dark()
  })
  
  # ============================================================
  # PAGINA 3
  # ============================================================
  pred_scenario <- reactive({
    pred_grid %>%
      filter(
        as.character(Month)     == input$pred_month,
        as.character(DayPeriod) == input$pred_period,
        as.character(DayType)   == input$pred_daytype,
        as.character(Meteo)     == input$pred_meteo
      ) %>%
      group_by(road_id) %>%
      summarise(expected=mean(expected,na.rm=TRUE),.groups="drop") %>%
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
    tags$div(
      tags$div(style=sprintf("color:%s;font-family:'Barlow Condensed',sans-serif;font-size:11px;letter-spacing:2px;opacity:0.55;text-transform:uppercase;margin-bottom:8px;",WHITE),"Expected accidents"),
      fluidRow(
        column(6,
               tags$div(style=sprintf("color:%s;font-family:'Barlow Condensed',sans-serif;font-weight:900;font-size:22px;",YELLOW),round(mean(d$expected,na.rm=TRUE),3)),
               tags$div(style="color:rgba(255,255,255,0.4);font-size:11px;","mean / road")
        ),
        column(6,
               tags$div(style=sprintf("color:%s;font-family:'Barlow Condensed',sans-serif;font-weight:900;font-size:22px;",RED),round(max(d$expected,na.rm=TRUE),3)),
               tags$div(style="color:rgba(255,255,255,0.4);font-size:11px;","max / road")
        )
      )
    )
  })
  
  # mappe base predicted (renderizzate una volta)
  output$mp_esq <- renderLeaflet({ .base_map_q(roads_esq, border_esq, bb_esq) })
  output$mp_tor <- renderLeaflet({ .base_map_q(roads_tor, border_tor, bb_tor) })
  output$mp_cen <- renderLeaflet({ .base_map_q(roads_cen, border_cen, bb_cen) })
  
  # base observed maps (rendered once)
  output$mo_esq <- renderLeaflet({ .base_map_q(roads_esq, border_esq, bb_esq) })
  output$mo_tor <- renderLeaflet({ .base_map_q(roads_tor, border_tor, bb_tor) })
  output$mo_cen <- renderLeaflet({ .base_map_q(roads_cen, border_cen, bb_cen) })
  
  # update the colored predicted roads
  observe({
    d     <- pred_scenario()
    scale <- input$pred_scale
    .update_pred("mp_esq", roads_esq, d, scale)
    .update_pred("mp_tor", roads_tor, d, scale)
    .update_pred("mp_cen", roads_cen, d, scale)
  })
  
  # update the observed points
  observe({
    .update_obs("mo_esq", obs_esq())
    .update_obs("mo_tor", obs_tor())
    .update_obs("mo_cen", obs_cen())
  })
}

shinyApp(ui, server)

