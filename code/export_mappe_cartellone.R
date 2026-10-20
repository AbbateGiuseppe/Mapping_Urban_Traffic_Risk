# ============================================================
#  EXPORT POSTER MAPS — June, All filters
#  3 views x 3 neighborhoods + 3 separate LEGENDS (one per type).
#  SHARED SCALES per type: same colors = same value across neighborhoods.
#    (A) GLMM  : expected rate/day            (shared 95th-pct cap)
#    (B) POINTS: raw accidents                (no continuous scale)
#    (C) DENS  : smoothed raw count           (shared 95th-pct cap)
#
#  Requires: pred_grid, roads, acc, mappa_quartieri
# ============================================================

library(sf); library(dplyr); library(ggplot2)
library(ggspatial); library(prettymapr); library(spdep)

MESE_SEL <- "Jun"; OUT_DIR <- "poster_maps"
DPI <- 300; W_IN <- 8; H_IN <- 8; K_NN <- 8
dir.create(OUT_DIR, showWarnings = FALSE)
PAL_COLS <- c("#FFFDE7","#FFD93D","#feb24c","#E8612F","#bd0026","#8B0000")
quartieri_target <- c("Centro Storico", "Esquilino", "Torre Angela")

# ------------------------------------------------------------
# 1. Roads and boundaries (ALL in 4326: no manual coord_sf)
# ------------------------------------------------------------
roads_sf  <- st_as_sf(roads) %>% mutate(road_id = as.integer(road_id))
roads_geo <- roads_sf %>% select(road_id) %>% st_transform(4326)
roads_m   <- roads_sf %>% select(road_id) %>% st_transform(32633)
conf_geo  <- st_as_sf(mappa_quartieri) %>% st_transform(4326) %>%
  mutate(.qnome = as.character(DEN_Z_URB))
road_quart <- pred_grid %>%
  mutate(road_id = as.integer(as.character(road_id)),
         DEN_Z_URB = as.character(DEN_Z_URB)) %>%
  distinct(road_id, DEN_Z_URB)

# ------------------------------------------------------------
# 2. GLMM June rate + shared cap (across all neighborhoods)
# ------------------------------------------------------------
pg <- pred_grid %>%
  mutate(road_id = as.integer(as.character(road_id)),
         DEN_Z_URB = as.character(DEN_Z_URB),
         Month = as.character(Month), espo = g_cond)
tasso_mese <- pg %>%
  filter(Month == MESE_SEL) %>%
  group_by(DEN_Z_URB, road_id) %>%
  summarise(rate = sum(expected)/sum(espo), .groups = "drop") %>%
  filter(is.finite(rate))
GLMM_CAP <- quantile(tasso_mese$rate, 0.95, na.rm = TRUE) %>% as.numeric()

# ------------------------------------------------------------
# 3. June accidents + smoothed raw density per neighborhood
# ------------------------------------------------------------
acc_geo <- st_as_sf(
  acc %>%
    filter(DEN_Z_URB %in% quartieri_target, as.character(Month) == MESE_SEL) %>%
    mutate(lon = as.numeric(longitude), lat = as.numeric(latitude)) %>%
    filter(!is.na(lon), !is.na(lat)),
  coords = c("lon","lat"), crs = 4326, remove = FALSE)
acc_m <- acc_geo %>% st_transform(32633)

calc_dens <- function(q) {
  rid <- road_quart %>% filter(DEN_Z_URB == q) %>% pull(road_id)
  rq  <- roads_m %>% filter(road_id %in% rid)
  if (nrow(rq) < (K_NN + 1)) return(NULL)
  acc_q <- acc_m %>% filter(DEN_Z_URB == q)
  if (nrow(acc_q) > 0) {
    nn  <- st_nearest_feature(acc_q, rq)
    cnt <- tibble(road_id = rq$road_id[nn]) %>% count(road_id, name = "n_acc")
    n_acc <- tibble(road_id = rq$road_id) %>%
      left_join(cnt, by = "road_id") %>%
      mutate(n_acc = tidyr::replace_na(n_acc, 0L)) %>% pull(n_acc)
  } else n_acc <- rep(0L, nrow(rq))
  coords <- st_coordinates(st_centroid(st_geometry(rq)))
  nb <- knn2nb(knearneigh(coords, k = min(K_NN, nrow(coords)-1)))
  lw <- nb2listw(include.self(nb), style = "W")
  # DOUBLE smoothing: apply lag.listw twice (k=8 -> smoother)
  s1 <- lag.listw(lw, as.numeric(n_acc), zero.policy = TRUE)
  rq$cnt_smooth <- lag.listw(lw, s1, zero.policy = TRUE)
  rq %>% st_transform(4326) %>% select(road_id, cnt_smooth)
}
dens_all <- lapply(quartieri_target, function(q){d<-calc_dens(q); if(!is.null(d)) d$.q<-q; d})
dens_all <- dens_all[!sapply(dens_all, is.null)]
names(dens_all) <- sapply(dens_all, function(d) d$.q[1])
DENS_CAP <- quantile(do.call(rbind, dens_all)$cnt_smooth, 0.95, na.rm = TRUE) %>% as.numeric()

# SHARED scales (fixed limits -> identical for all neighborhoods)
sc_glmm <- scale_color_gradientn(colours = PAL_COLS, limits = c(0, sqrt(GLMM_CAP)),
             name = "acc./day", labels = function(x) sprintf("%.4f", x^2))
sc_dens <- scale_color_gradientn(colours = PAL_COLS, limits = c(0, sqrt(DENS_CAP)),
             name = "accidents\n(smoothed\ncount)", labels = function(x) sprintf("%.1f", x^2))

# ------------------------------------------------------------
# 4. helper: save a map (robust basemap, NO manual coord_sf)
# ------------------------------------------------------------
salva <- function(p, nome) {
  f <- file.path(OUT_DIR, nome)
  ggsave(f, p, width = W_IN, height = H_IN, dpi = DPI, bg = "white")
  cat("saved:", f, "\n")
}
base_map <- function(poly) {
  ggplot() +
    annotation_map_tile(type = "cartolight", zoomin = -1, progress = "none") +
    layer_spatial(poly, fill = NA, color = "black", linewidth = 0.6)
}

# ------------------------------------------------------------
# 5. generate the 9 maps (legends suppressed: legend.position="none")
# ------------------------------------------------------------
for (q in quartieri_target) {
  rid  <- road_quart %>% filter(DEN_Z_URB == q) %>% pull(road_id)
  poly <- conf_geo %>% filter(.qnome == q)
  qfile <- gsub(" ","_",q)

  # (A) GLMM
  gA <- roads_geo %>% filter(road_id %in% rid) %>%
    left_join(tasso_mese %>% filter(DEN_Z_URB==q), by="road_id")
  pA <- base_map(poly) +
    layer_spatial(gA, aes(color = sqrt(pmin(rate, GLMM_CAP))), linewidth = 0.7) +
    sc_glmm + labs(title=q, subtitle="GLMM - expected rate (June)") +
    theme_void(base_size=14) +
    theme(plot.title=element_text(face="bold"), legend.position="none")
  salva(pA, paste0("A_GLMM_", qfile, "_June.png"))

  # (B) POINTS
  pts <- acc_geo %>% filter(DEN_Z_URB==q)
  gB  <- roads_geo %>% filter(road_id %in% rid)
  pB <- base_map(poly) +
    layer_spatial(gB, color="black", linewidth=0.4) +
    layer_spatial(pts, color="#E83A2F", alpha=0.55, size=1.6) +
    labs(title=q, subtitle="Observed accidents (June)") +
    theme_void(base_size=14) + theme(plot.title=element_text(face="bold"))
  salva(pB, paste0("B_POINTS_", qfile, "_June.png"))

  # (C) DENS
  gC <- dens_all[[q]]
  if (!is.null(gC)) {
    pC <- base_map(poly) +
      layer_spatial(gC, aes(color = cnt_smooth), linewidth = 0.9) +
      sc_dens + labs(title=q, subtitle="Observed accident density (June, smoothed)") +
      theme_void(base_size=14) +
      theme(plot.title=element_text(face="bold"), legend.position="none")
    salva(pC, paste0("C_DENS_", qfile, "_June.png"))
  }
}

# ------------------------------------------------------------
# 6. separate LEGENDS (one per type) — extracted as standalone images
# ------------------------------------------------------------
library(grid); library(gtable)
estrai_legenda <- function(scala, nome) {
  d <- data.frame(x=1, y=1, v=c(0, sqrt(if(grepl("GLMM",nome)) GLMM_CAP else DENS_CAP)))
  g <- ggplot(d, aes(x,y,color=v)) + geom_point() + scala +
    theme_void(base_size=16) +
    theme(legend.position="right", legend.key.height=unit(2,"cm"))
  gt <- ggplotGrob(g)
  leg <- gt$grobs[[which(sapply(gt$grobs, function(z) z$name)=="guide-box")]]
  f <- file.path(OUT_DIR, nome)
  ggsave(f, leg, width=2.5, height=5, dpi=DPI, bg="white")
  cat("legend:", f, "\n")
}
estrai_legenda(sc_glmm, "LEGEND_GLMM.png")
estrai_legenda(sc_dens, "LEGEND_DENS.png")
# POINTS legend: simple, single color
{
  d <- data.frame(x=1,y=1,Accident="Accident")
  g <- ggplot(d, aes(x,y,color=Accident)) + geom_point(size=4) +
    scale_color_manual(values=c(Accident="#E83A2F"), name=NULL) +
    theme_void(base_size=16) + theme(legend.position="right")
  gt <- ggplotGrob(g)
  leg <- gt$grobs[[which(sapply(gt$grobs, function(z) z$name)=="guide-box")]]
  ggsave(file.path(OUT_DIR,"LEGEND_POINTS.png"), leg, width=2.5, height=2, dpi=DPI, bg="white")
  cat("legend: LEGEND_POINTS.png\n")
}

cat("\nDone: 9 maps + 3 legends in '", OUT_DIR, "/'\n", sep="")

# ============================================================
#  NOTES
#  - SHARED scales: GLMM_CAP and DENS_CAP computed across all neighborhoods
#    (95th pct). limits=c(0,sqrt(cap)) -> identical for the 3 neighborhoods.
#  - The maps are exported WITHOUT a legend; the 3 legends are separate files
#    (LEGEND_GLMM/_DENS/_POINTS.png) to be placed once on the poster.
#  - FIX for blank map: no manual coord_sf; layer_spatial() handles
#    the limits and alignment with the basemap. zoomin=-1 avoids
#    'bounding box too small'. Requires internet for the tiles.
# ============================================================
