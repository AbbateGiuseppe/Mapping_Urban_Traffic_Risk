# ============================================================
#  DESCRIPTIVE ANALYSIS - points/zones with the most accidents per neighborhood
#  depending on the active FLAG:
#     - "daynight"  : Day  vs Night
#     - "weather"   : Clear   vs Rain
#     - "weekend"   : Weekday vs Weekend
#
#  For each neighborhood: accident points colored by flag + the top hotspots
#  (grid cells with the most accidents) highlighted.
#
#  Requires in memory: acc  (from Base_Esplorazione_offset.Rdata)
#  with at least: longitude, latitude, DEN_Z_URB, and the columns for the flags.
# ============================================================

library(sf)
library(dplyr)
library(ggplot2)

# ------------------------------------------------------------
# 0. CONFIGURATION  <-- adjust HERE if the column names differ
# ------------------------------------------------------------
COL_LON   <- "longitude"
COL_LAT   <- "latitude"
COL_QUART <- "DEN_Z_URB"
COL_DP    <- "DayPeriod"   # Morning/Afternoon/Evening/Night
COL_METEO <- "Meteo"       # Dry/Rain
COL_DATE  <- "data"        # date column (for weekend); if absent see NOTE below

quartieri_target <- c("Esquilino", "Torre Angela", "Centro Storico")
CELL_M    <- 100           # cell side (m) for aggregating the "points with most accidents"
TOP_N     <- 5             # how many hotspots to highlight per neighborhood

# WHICH FLAG TO ACTIVATE: "daynight" | "weather" | "weekend"
FLAG <- "daynight"

# ------------------------------------------------------------
# 1. Prepare the accident points in the 3 neighborhoods (UTM 33N, meters)
# ------------------------------------------------------------
acc_pt <- acc %>%
  filter(.data[[COL_QUART]] %in% quartieri_target) %>%
  st_as_sf(coords = c(COL_LON, COL_LAT), crs = 4326, remove = FALSE) %>%
  st_transform(32633)

# ------------------------------------------------------------
# 2. Build the three flags from the existing columns
# ------------------------------------------------------------
acc_pt <- acc_pt %>%
  mutate(
    # DAY/NIGHT: Night = Evening+Night, Day = Morning+Afternoon
    f_daynight = ifelse(.data[[COL_DP]] %in% c("Evening", "Night"),
                        "Night", "Day"),
    # WEATHER: Rain vs Clear (everything that is not Rain -> Clear)
    f_weather  = ifelse(.data[[COL_METEO]] == "Rain", "Rain", "Clear")
  )

# WEEKEND/WEEKDAY: derived from the date, if the column exists
if (COL_DATE %in% names(acc_pt)) {
  wd <- as.POSIXlt(as.Date(acc_pt[[COL_DATE]]))$wday   # 0=Sun ... 6=Sat
  acc_pt$f_weekend <- ifelse(wd %in% c(0, 6), "Weekend", "Weekday")
} else {
  acc_pt$f_weekend <- NA_character_
  message("NOTE: date column '", COL_DATE,
          "' not found: the weekend flag is not available. ",
          "Set COL_DATE to the correct name.")
}

# choose the active flag
flag_col <- switch(FLAG,
  daynight = "f_daynight",
  weather  = "f_weather",
  weekend  = "f_weekend",
  stop("FLAG must be 'daynight', 'weather' or 'weekend'")
)
flag_title <- switch(FLAG,
  daynight = "Day / Night",
  weather  = "Clear / Rain",
  weekend  = "Weekday / Weekend"
)
flag_cols <- switch(FLAG,
  daynight = c(Day = "#f1a340", Night = "#542788"),
  weather  = c(Clear  = "#fdae61", Rain  = "#2c7bb6"),
  weekend  = c(Weekday= "#1b9e77", Weekend = "#d95f02")
)

acc_pt$flag <- acc_pt[[flag_col]]
acc_pt <- acc_pt %>% filter(!is.na(flag))

# ------------------------------------------------------------
# 3. "Points with the most accidents": aggregate on a grid (CELL_M) and count
#    -> for each neighborhood find the cells (zones) with the highest count
# ------------------------------------------------------------
xy <- st_coordinates(acc_pt)
acc_pt$cx <- floor(xy[,1] / CELL_M)
acc_pt$cy <- floor(xy[,2] / CELL_M)

hot <- acc_pt %>%
  st_drop_geometry() %>%
  group_by(.data[[COL_QUART]], cx, cy) %>%
  summarise(n = n(), .groups = "drop") %>%
  rename(quart = 1) %>%
  group_by(quart) %>%
  slice_max(n, n = TOP_N, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    # cell center in meters -> convert back to lat/lon for the plot
    X = (cx + 0.5) * CELL_M,
    Y = (cy + 0.5) * CELL_M
  )
# hotspot cell centers in lat/lon
hot_sf <- st_as_sf(hot, coords = c("X","Y"), crs = 32633) %>%
  st_transform(4326)
hot$lon <- st_coordinates(hot_sf)[,1]
hot$lat <- st_coordinates(hot_sf)[,2]

cat("\n== Top", TOP_N, "zones by accident count (per neighborhood) ==\n")
print(hot %>% select(quart, n, lon, lat))

# points in lat/lon for the plot
acc_geo <- acc_pt %>% st_transform(4326)
acc_geo$lon <- st_coordinates(acc_geo)[,1]
acc_geo$lat <- st_coordinates(acc_geo)[,2]
acc_geo$quart <- acc_geo[[COL_QUART]]

# ------------------------------------------------------------
# 4. PLOT: one descriptive map per neighborhood
#    points colored by flag + hotspots (black circles sized by count)
# ------------------------------------------------------------
plots <- lapply(quartieri_target, function(q) {
  d  <- acc_geo %>% filter(quart == q)
  h  <- hot     %>% filter(quart == q)
  if (nrow(d) == 0) return(NULL)

  ggplot() +
    geom_point(data = d, aes(lon, lat, color = flag),
               size = 0.9, alpha = 0.55) +
    # hotspots: empty black circles, radius ~ number of accidents
    geom_point(data = h, aes(lon, lat, size = n),
               shape = 21, fill = NA, color = "black", stroke = 1.1) +
    scale_color_manual(values = flag_cols, name = flag_title) +
    scale_size_continuous(range = c(4, 12), name = "accidents\n(top zone)") +
    coord_quickmap() +
    labs(title = q,
         subtitle = paste("Accidents by", flag_title,
                          "+ top", TOP_N, "zones"),
         x = NULL, y = NULL) +
    theme_minimal() +
    theme(axis.text = element_text(size = 7))
})

for (p in plots) if (!is.null(p)) print(p)

# ------------------------------------------------------------
# 5. Supporting descriptive chart: accident count by flag
#    and by neighborhood (bars), useful to quantify the imbalance
# ------------------------------------------------------------
conteggi <- acc_geo %>%
  st_drop_geometry() %>%
  count(quart, flag, name = "n")

cat("\n== Accident count by neighborhood x flag ==\n")
print(conteggi)

p_bar <- ggplot(conteggi, aes(quart, n, fill = flag)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = flag_cols, name = flag_title) +
  labs(title = paste("Accidents by neighborhood -", flag_title),
       x = NULL, y = "n. accidents") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
print(p_bar)

# ============================================================
#  USAGE:
#  - change FLAG above ("daynight" / "weather" / "weekend")
#    and rerun to get the maps with the other split.
#  - CELL_M controls how "coarse" the hotspot zones are
#    (100 m = block; increase for wider zones).
#  - TOP_N = how many hot zones to highlight per neighborhood.
# ============================================================
