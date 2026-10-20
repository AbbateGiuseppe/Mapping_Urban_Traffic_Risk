#'######################################'#
#'########### PREPROCESSING ############'#
#'######################################'#

graphics.off()
rm(list = ls())
options(warn = -1, timeout = 800)


## LIBRARIES AND UTILITIES -----------------------------------------------------
if(!require(pacman)) install.packages("pacman")
pacman::p_load("rstudioapi")
source("utils/packages.R")

# Loading Packages and Auxiliary Functions
file.sources <- list.files("utils/", pattern = "*.R", full.names = TRUE)
invisible(sapply(file.sources, source, .GlobalEnv))

# Colors and Map Type
col_bdy <- rgb(128, 128, 128, max = 255)
col_nodes <- rgb(0, 0, 0, max = 255)
col_edges <- rgb(60, 60, 60, max = 255)
col_accidents <- rgb(190, 0, 0, max = 255)
map_type <- "CartoDB.Positron"



## SPATIAL SUPPORT -------------------------------------------------------------
# Municipality of Interest: Bergamo, Bologna, Busto Arsizio, Cremona, Milan, Monza, Rome
municipality <- "Rome"

# Administrative Boundary
adm_bdy <- getAdmBdy(municipality = municipality)

# Plot
mapview(adm_bdy, layer.name = "boundary", legend = FALSE,
        col.region = col_bdy, alpha.regions = 0.3, lwd = 0.3)

# Bounds of Interest (min/max lon/lat)
bounds <- NULL
# bounds <- c(xmin, xmax, ymin, ymax)

# Examples for Bergamo:
# bounds <- c(9.68702, 9.70143, 45.69725, 45.71305) # Rondo delle Valli
# bounds <- c(9.65257, 9.67864, 45.68385, 45.69773) # Bergamo Centre (Citta Bassa)
# bounds <- c(9.65510, 9.68479, 45.69944, 45.72274) # Bergamo Centre (Citta Alta)
# bounds <- c(9.65660, 9.68704, 45.67213, 45.68228) # Motorway Junction

# Spatial Domain
roads <- getDomain(municipality = municipality, bounds = bounds)

# Plot
mapview(roads) +
  mapview(adm_bdy, layer.name = "boundary", legend = FALSE,
          col.region = col_bdy, alpha.regions = 0.3, lwd = 0.3)

# Mesh for Estimation
mesh <- getMesh(municipality = municipality, bounds = bounds)

mesh_df <- data.frame(lon = mesh$nodes[,1], lat = mesh$nodes[,2])
mesh_sf_df <- st_as_sf(mesh_df, coords = c("lon", "lat"), crs = 4326)

mesh_linnet <- as.linnet(mesh)

mesh_sfnetwork <- as_sfnetwork(mesh_linnet, directed = FALSE, edges_as_lines = TRUE)
st_crs(mesh_sfnetwork) <- 4326

mesh_sf <- st_as_sf(mesh_sfnetwork, "edges")

# Plot
mapview(mesh_sf, map.type = map_type, layer.name = "edges", legend = FALSE,
        color = col_edges, alpha.regions = 1, lwd = 1.5) +
  mapview(mesh_sf_df, layer.name = "nodes", legend = FALSE,
          col.region = col_nodes, alpha.regions = 1, cex = 1) +
  mapview(adm_bdy, layer.name = "boundary", legend = FALSE,
          col.region = col_bdy, alpha.regions = 0.3, lwd = 0.3)




## DATA ------------------------------------------------------------------------
# Import the Dataset
data_df <- readDataset(municipality = municipality)

data_sf_df <- st_as_sf(data_df, coords = c("Longitude", "Latitude"), crs = 4326)

# Plot (first 100 events and first 5 variables only)
mapview(mesh_sf, map.type = map_type, layer.name = "edges", legend = FALSE,
        color = col_edges, alpha.regions = 1, lwd = 1.5) +
  mapview(mesh_sf_df, layer.name = "nodes", legend = FALSE,
          col.region = col_nodes, alpha.regions = 1, cex = 1) +
  mapview(adm_bdy, layer.name = "boundary", legend = FALSE,
          col.region = col_bdy, alpha.regions = 0.3, lwd = 0.3) +
  mapview(data_sf_df[1:100,1:5], layer.name = "accidents", legend = FALSE,
          col.region = col_accidents, alpha.regions = 1, cex = 3)




## SESSION TIMESTAMP -------------------------------------------------------
# Can be useful for:
# - naming output files
# - keeping track of when a given run was executed

# Create the Current Date / Time
current_datetime <- Sys.time()
formatted_datetime <- format(current_datetime, "[%Y-%m-%d]-[%H-%M-%S]")

## ===========================================================================
##  ACCIDENT PLOT 2017-2021 (2020 EXCLUDED) -- with snap to roads
##  To be appended at the end of 01_preprocessing.R (after all the variables
##  have already been created: adm_bdy, mesh_sf, mesh_sf_df, data_df, etc.)
## ===========================================================================

## 1. FILTER BY YEAR ----------------------------------------------------

# Automatically detect the year column
# Priority order: Anno > year > Year > extracted from Date/Data/datetime columns
cat("Columns available in the dataset:\n")
print(names(data_df))

year_col <- NULL

if ("Anno" %in% names(data_df)) {
  year_col <- "Anno"
} else if ("year" %in% names(data_df)) {
  year_col <- "year"
} else if ("Year" %in% names(data_df)) {
  year_col <- "Year"
} else {
  # Look for date/datetime columns and extract the year from them
  date_candidates <- names(data_df)[sapply(data_df, function(x) {
    inherits(x, c("Date", "POSIXct", "POSIXlt"))
  })]
  if (length(date_candidates) > 0) {
    date_col <- date_candidates[1]
    cat(sprintf("Year extracted from column '%s'\n", date_col))
    data_df$Year_extracted <- as.integer(format(as.POSIXct(data_df[[date_col]]), "%Y"))
    year_col <- "Year_extracted"
  } else {
    # Last resort: look for columns whose name contains "dat" or "time"
    fuzzy <- grep("dat|time|ora|Data|Dat|Time", names(data_df), value = TRUE, ignore.case = TRUE)
    if (length(fuzzy) > 0) {
      date_col <- fuzzy[1]
      cat(sprintf("Attempting to extract the year from '%s'\n", date_col))
      data_df$Year_extracted <- as.integer(substr(as.character(data_df[[date_col]]), 1, 4))
      year_col <- "Year_extracted"
    } else {
      stop(paste(
        "Could not find a year column in the dataset.",
        "Add the filtering manually:",
        "  data_df$my_year <- <your_logic>",
        "  year_col <- 'my_year'",
        sep = "\n"
      ))
    }
  }
}

cat(sprintf("Using year column: '%s'\n", year_col))

# Filter: 2017, 2018, 2019, 2021 (2020 excluded)
years_to_keep <- c(2017, 2018, 2019, 2021)
data_filtered <- data_df[data_df[[year_col]] %in% years_to_keep, ]

cat(sprintf("Accidents in the selected period: %d\n", nrow(data_filtered)))
cat("Distribution by year:\n")
print(table(data_filtered[[year_col]]))


## 2. CONVERT TO SF -------------------------------------------------------

data_sf_filtered <- st_as_sf(
  data_filtered,
  coords = c("Longitude", "Latitude"),
  crs    = 4326
)


## 3. SNAP TO ROADS ----------------------------------------------------------
# Each accident point is projected onto the nearest road segment,
# so that no point falls outside the road network.

cat("Computing snap to roads...\n")

# Make sure mesh_sf and the points share the same CRS
mesh_sf_snap <- st_transform(mesh_sf, 4326)

# Index of the nearest segment for each accident
nearest_edge_idx <- st_nearest_feature(data_sf_filtered, mesh_sf_snap)

# Line (LINESTRING) connecting each accident to its nearest point on the road
# pairwise = TRUE: i-th point vs i-th selected segment
nearest_lines <- st_nearest_points(
  data_sf_filtered,
  mesh_sf_snap[nearest_edge_idx, ],
  pairwise = TRUE
)

# The second point of each linestring is the point on the road
snapped_list <- lapply(seq_along(nearest_lines), function(i) {
  pts <- st_cast(st_sfc(nearest_lines[[i]], crs = 4326), "POINT")
  pts[2]   # point projected onto the road
})
snapped_sfc <- do.call(c, snapped_list)
st_crs(snapped_sfc) <- 4326

# Replace the original geometry with the "snapped" one
data_snapped <- data_sf_filtered
st_geometry(data_snapped) <- snapped_sfc

cat("Snap complete.\n")

## 4. PACKAGE AND TILE SETUP (Leaflet / CartoDB.Positron style) -------------

if (!requireNamespace("ggplot2",   quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("maptiles",  quietly = TRUE)) install.packages("maptiles")
if (!requireNamespace("tidyterra", quietly = TRUE)) install.packages("tidyterra")
library(ggplot2)
library(maptiles)
library(tidyterra)

# Web Mercator (EPSG:3857): the same CRS used internally by Leaflet
wm_crs <- 3857

adm_bdy_wm      <- st_transform(adm_bdy,      wm_crs)
mesh_sf_wm      <- st_transform(mesh_sf,       wm_crs)
data_snapped_wm <- st_transform(data_snapped,  wm_crs)

# Download CartoDB.Positron tiles (same provider as the map_type used in mapview)
# zoom 11 works well for all of Rome; increase to 12 for tighter bounds
cat("Downloading CartoDB.Positron tiles...\n")
tiles <- get_tiles(
  x        = adm_bdy_wm,
  provider = "CartoDB.Positron",
  zoom     = 11,
  crop     = TRUE
)
cat("Tiles downloaded.\n")

# Base theme shared between the two plots
theme_leaflet <- theme_void(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15, hjust = 0.5,
                                   margin = margin(b = 4)),
    plot.subtitle   = element_text(size = 11, hjust = 0.5, color = "grey30",
                                   margin = margin(b = 6)),
    plot.caption    = element_text(size = 8, color = "grey55", hjust = 0.5,
                                   margin = margin(t = 8)),
    plot.margin     = margin(12, 12, 12, 12),
    plot.background = element_rect(fill = "white", color = NA)
  )



## 4a. ROADS-ONLY PLOT -------------------------------------------------------

p_roads <- ggplot() +
  # CartoDB.Positron basemap tiles
  geom_spatraster_rgb(data = tiles) +
  # Semi-transparent grey fill for the municipality of Rome
  geom_sf(
    data      = adm_bdy_wm,
    fill      = rgb(160, 160, 160, max = 255),
    color     = rgb(60, 60, 60, max = 255),
    linewidth = 0.8,
    alpha     = 0.35
  ) +
  # Thicker road mesh drawn over the tiles
  geom_sf(
    data      = mesh_sf_wm,
    color     = rgb(40, 40, 40, max = 255),
    linewidth = 0.65,
    alpha     = 0.9
  ) +
  theme_leaflet +
  labs(
    title   = sprintf("Road Network — %s", municipality),
    caption = sprintf(
      "%d segments  |  © CartoDB · © OpenStreetMap contributors",
      nrow(mesh_sf_wm)
    )
  )

roads_filename <- sprintf(
  "roads_%s_%s.png",
  tolower(municipality),
  format(Sys.time(), "%Y%m%d_%H%M%S")
)

ggsave(
  filename = roads_filename,
  plot     = p_roads,
  width    = 14,
  height   = 12,
  dpi      = 300,
  units    = "in",
  bg       = "white"
)

cat(sprintf("Roads plot saved as: %s\n", roads_filename))


## 4b. ACCIDENTS PLOT ---------------------------------------------------------

p <- ggplot() +
  # CartoDB.Positron basemap tiles
  geom_spatraster_rgb(data = tiles) +
  # Semi-transparent grey fill for the municipality of Rome
  geom_sf(
    data      = adm_bdy_wm,
    fill      = rgb(160, 160, 160, max = 255),
    color     = rgb(60, 60, 60, max = 255),
    linewidth = 0.8,
    alpha     = 0.35
  ) +
  # Road network
  geom_sf(
    data      = mesh_sf_wm,
    color     = rgb(50, 50, 50, max = 255),
    linewidth = 0.55,
    alpha     = 0.85
  ) +
  # Accidents snapped to the roads
  geom_sf(
    data  = data_snapped_wm,
    color = rgb(190, 0, 0, max = 255),
    size  = 0.8,
    alpha = 0.6,
    shape = 16
  ) +
  theme_leaflet +
  labs(
    title    = sprintf("Road Accidents — %s", municipality),
    subtitle = sprintf(
      "Years 2017–2021 (2020 excluded)  |  n = %d accidents",
      nrow(data_snapped_wm)
    ),
    caption  = "Points projected on the nearest road  |  © CartoDB · © OpenStreetMap contributors"
  )


## 5. SAVE PNG --------------------------------------------------------

output_filename <- sprintf(
  "accidents_%s_2017-2021_no2020_%s.png",
  tolower(municipality),
  format(Sys.time(), "%Y%m%d_%H%M%S")
)

ggsave(
  filename = output_filename,
  plot     = p,
  width    = 14,
  height   = 12,
  dpi      = 300,
  units    = "in",
  bg       = "white"
)

cat(sprintf("Accidents plot saved as: %s\n", output_filename))
