load("Base_Esplorazione_offset.RData")


# ============================================================
#  PRELIMINARY DESCRIPTIVE SPATIAL ANALYSIS
#  Precedes the GLMM: documents WHERE and HOW accidents are concentrated
#  in space, motivating the modeling choices (spatial random effects).
#
#  Unit: accident point (KDE) and road/road_id (Gi*, Moran, LISA).
#  The 3 neighborhoods are too few for spatial statistics at that level.
#  Neighborhood between roads: k nearest neighbors (k=8) on the centroids
#  -> robust: every road always has neighbors, no isolated units.
# ============================================================

library(sf)
library(dplyr)
library(spdep)        # vicinati, Moran, Getis-Ord, LISA
library(ggplot2)
# library(spatstat)   # alternative KDE (ppp); here stat_density_2d from ggplot is used instead

quartieri_target <- c("Esquilino", "Torre Angela", "Centro Storico")
anni_target      <- 2017:2022

# ------------------------------------------------------------
# 0. Prepare accidents (points) and roads (lines) in the 3 neighborhoods
#    Use a PROJECTED CRS (meters) for correct distances/densities.
#    Rome -> UTM 33N (EPSG:32633).
# ------------------------------------------------------------
acc_pt <- acc %>%
  filter(DEN_Z_URB %in% quartieri_target,
         as.integer(Year) %in% anni_target) %>%
  st_as_sf(coords = c("longitude","latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(32633)

roads_sf <- st_transform(st_as_sf(roads), 32633)
roads_sf$road_id  <- seq_len(nrow(roads_sf))
roads_sf$road_len <- as.numeric(st_length(roads_sf))

# accident count per road (road closest to each accident)
nn <- st_nearest_feature(acc_pt, roads_sf)
cnt <- tibble(road_id = roads_sf$road_id[nn]) %>%
  count(road_id, name = "n_acc")

roads_sf <- roads_sf %>%
  left_join(cnt, by = "road_id") %>%
  mutate(
    n_acc   = tidyr::replace_na(n_acc, 0L),
    dens    = n_acc / (road_len/1000)   # accidents per km (normalizes for length)
  )

# ------------------------------------------------------------
# 1. KDE - kernel density of the accident points
#    Heatmap of "where most accidents occur", per neighborhood.
# ------------------------------------------------------------
# ------------------------------------------------------------
# 1. KDE - kernel density (RASTER version without isoband)
# ------------------------------------------------------------
acc_xy <- acc_pt %>%
  mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2]) %>%
  st_drop_geometry()

p_kde_raster <- ggplot(acc_xy, aes(X, Y)) +
  # contour = FALSE is the trick to avoid triggering McAfee
  stat_density_2d(aes(fill = after_stat(density)), geom = "raster", contour = FALSE) +
  scale_fill_viridis_c(option = "inferno") +
  facet_wrap(~ DEN_Z_URB, scales = "free") +
  labs(title = "Kernel density of accidents (Raster KDE)",
       fill = "Density") +
  theme_minimal()
print(p_kde_raster)
# ------------------------------------------------------------
# 2. Neighborhood between roads: k nearest neighbors (k = 8) on the centroids
# ------------------------------------------------------------
ctr   <- st_centroid(st_geometry(roads_sf))
coords <- st_coordinates(ctr)
knn8  <- knearneigh(coords, k = 8)
nb    <- knn2nb(knn8)
lw    <- nb2listw(nb, style = "W")   # row-standardized weights


# Compute the bounding box (X and Y limits) of just the roads in the 3 neighborhoods
bbox_target <- st_bbox(roads_sf)

# Extract the individual limits to use them in coord_sf
xlim_zoom <- c(bbox_target["xmin"], bbox_target["xmax"])
ylim_zoom <- c(bbox_target["ymin"], bbox_target["ymax"])

# ------------------------------------------------------------
# 3. GLOBAL Moran's I on accident rate per road
#    Is there overall spatial autocorrelation?
# ------------------------------------------------------------
cat("\n== Global Moran's I (accident density/km) ==\n")
moran_g <- moran.test(roads_sf$dens, lw, zero.policy = TRUE)
print(moran_g)
# permutation test (more robust, does not assume normality):
print(moran.mc(roads_sf$dens, lw, nsim = 999, zero.policy = TRUE))

# ------------------------------------------------------------
# 4. LISA - Local Moran: locates the clusters
#    Classes: High-High / Low-Low (cluster), High-Low / Low-High (outlier)
# ------------------------------------------------------------
locm <- localmoran(roads_sf$dens, lw, zero.policy = TRUE)
roads_sf$Ii      <- locm[,"Ii"]
roads_sf$p_lisa  <- locm[,"Pr(z != E(Ii))"]

z      <- scale(roads_sf$dens)[,1]
lag_z  <- lag.listw(lw, z, zero.policy = TRUE)
roads_sf$lisa_cluster <- dplyr::case_when(
  roads_sf$p_lisa > 0.05            ~ "Not sign.",
  z > 0 & lag_z > 0                 ~ "High-High",
  z < 0 & lag_z < 0                 ~ "Low-Low",
  z > 0 & lag_z < 0                 ~ "High-Low",
  TRUE                              ~ "Low-High"
)
cat("\n== LISA: count per class ==\n")
print(table(roads_sf$lisa_cluster))

p_lisa <- ggplot(roads_sf) +
  geom_sf(aes(color = lisa_cluster), linewidth = .8) +
  scale_color_manual(values = c("High-High"="#c0392b","Low-Low"="#2980b9",
                                "High-Low"="#e67e22","Low-High"="#9b59b6",
                                "Not sign."="grey85")) +
  # --- FORCE THE ZOOM ONTO THE TARGET NEIGHBORHOODS ---
  coord_sf(xlim = xlim_zoom, ylim = ylim_zoom, expand = TRUE) + 
  labs(title = "LISA - local clusters of accidents per road",
       color = "cluster") +
  theme_minimal()
print(p_lisa)
# ------------------------------------------------------------
# 5. Getis-Ord Gi* - hotspot / coldspot
#    Includes the road itself in the neighborhood (include.self) -> Gi*.
# ------------------------------------------------------------
lw_self <- nb2listw(include.self(nb), style = "W")
gi <- localG(roads_sf$dens, lw_self, zero.policy = TRUE)
roads_sf$Gi_z <- as.numeric(gi)
roads_sf$hotspot <- dplyr::case_when(
  roads_sf$Gi_z >=  1.96 ~ "Hotspot (95%)",
  roads_sf$Gi_z <= -1.96 ~ "Coldspot (95%)",
  TRUE                   ~ "Not sign."
)
cat("\n== Getis-Ord Gi*: count per class ==\n")
print(table(roads_sf$hotspot))

p_gi <- ggplot(roads_sf) +
  geom_sf(aes(color = Gi_z), linewidth = .8) +
  scale_color_gradient2(low = "#2980b9", mid = "grey90", high = "#c0392b",
                        midpoint = 0) +
  # --- FORCE THE ZOOM ONTO THE TARGET NEIGHBORHOODS ---
  coord_sf(xlim = xlim_zoom, ylim = ylim_zoom, expand = TRUE) + 
  labs(title = "Getis-Ord Gi* - hotspot (red) / coldspot (blue)",
       color = "Gi* (z)") +
  theme_minimal()
print(p_gi)

# ------------------------------------------------------------
# DENSITY ON THE ROADS - only the 3 target neighborhoods
# ------------------------------------------------------------
roads_sf <- st_transform(st_as_sf(roads), 32633)
roads_sf$road_id  <- seq_len(nrow(roads_sf))
roads_sf$road_len <- as.numeric(st_length(roads_sf))

# --- Keep only the roads in the 3 neighborhoods ---
# CASE A: 'roads' already has a neighborhood column (e.g. DEN_Z_URB)
if ("DEN_Z_URB" %in% names(roads_sf)) {
  roads_sf <- roads_sf %>% filter(DEN_Z_URB %in% quartieri_target)
} else {
  # CASE B: no neighborhood column -> clip using the neighborhood polygons.
  # If you have an sf of neighborhoods (e.g. 'quartieri_sf' with a name field), use it:
  if (exists("quartieri_sf")) {
    poly <- quartieri_sf %>%
      st_transform(32633) %>%
      filter(DEN_Z_URB %in% quartieri_target) %>%
      st_union()
    roads_sf <- roads_sf[st_intersects(roads_sf, poly, sparse = FALSE)[,1], ]
  } else {
    # CASE C: fallback without polygons -> use the (buffered) envelope of the accidents
    poly <- acc_pt %>% st_union() %>% st_convex_hull() %>% st_buffer(300)
    roads_sf <- roads_sf[st_intersects(roads_sf, poly, sparse = FALSE)[,1], ]
  }
}

# recompute road_len on the filtered roads (length already present, ok)
# accident count: road closest to each accident
nn  <- st_nearest_feature(acc_pt, roads_sf)
cnt <- tibble(road_id = roads_sf$road_id[nn]) %>%
  count(road_id, name = "n_acc")

roads_sf <- roads_sf %>%
  left_join(cnt, by = "road_id") %>%
  mutate(
    n_acc = tidyr::replace_na(n_acc, 0L),
    dens  = n_acc / (road_len / 1000)        # accidents per km
  )

# --- Density summary on the roads ---
cat("\n== Accident density/km on the roads (3 neighborhoods) ==\n")
print(summary(roads_sf$dens))

# Top 10 densest roads
roads_sf %>%
  st_drop_geometry() %>%
  arrange(desc(dens)) %>%
  select(road_id, n_acc, road_len, dens) %>%
  head(10) %>%
  print()

# Map of the density on the roads
p_dens <- ggplot(roads_sf) +
  geom_sf(aes(color = dens), linewidth = .8) +
  scale_color_viridis_c(option = "inferno", trans = "sqrt") +
  # --- FORCE THE ZOOM ONTO THE TARGET NEIGHBORHOODS ---
  coord_sf(xlim = xlim_zoom, ylim = ylim_zoom, expand = TRUE) + 
  labs(title = "Accident density on the roads (acc./km) - 3 neighborhoods",
       color = "acc./km") +
  theme_minimal()
print(p_dens)


# Example density map split per individual neighborhood (no empty intermediate gaps)
p_dens_facet <- ggplot(roads_sf) +
  geom_sf(aes(color = dens), linewidth = .8) +
  scale_color_viridis_c(option = "inferno", trans = "sqrt") +
  # facet_wrap lets each panel zoom into its own local geometry
  facet_wrap(~ DEN_Z_URB, scales = "free") + 
  labs(title = "Accident density per road in the target neighborhoods",
       color = "acc./km") +
  theme_minimal()
print(p_dens_facet)
# ------------------------------------------------------------
#  HOW TO TELL THIS IN THE METHODOLOGY SECTION
#  - KDE: the hot spots show non-uniform spatial concentration
#    -> justifies a model that accounts for location.
#  - Global Moran's I: if significant (and of non-negligible magnitude),
#    confirms spatial dependence -> motivates (1|road_id)/(1|DEN_Z_URB).
#  - LISA: locates the High-High clusters (critical areas) and the outliers.
#  - Gi*: identifies the statistically significant hotspots, useful for
#    operational guidance (where to intervene) alongside the GLMM's map
#    of expected accidents.
#  NB large n: as with the GLMM, assess the MAGNITUDE in addition to the p-value.
# ------------------------------------------------------------
