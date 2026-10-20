# ==============================================================================
# 07_zi_glmm_v2_rain_quota.R
# Zero-Inflated Negative Binomial GLMM — Final Model (v2)
#
# Inherits the structure of 07_zi_glmm_final.R, incorporating the logic of:
#   - 12_Traffic_Rain_Offset.R : rain defined from a SINGLE source (FENOMENI
#     contains "piogg"), joined by date on the numerator (accidents) and
#     the denominator (exposure days), instead of Clear/NotClear
#     derived from an already-aggregated column.
#   - 13__GLMM_roads.R         : exposure computed per Year x Month
#     (share of Rain/Dry days), not pooled over the whole 2017-2021
#     period; model family comparison + DHARMa diagnostics.
#
# The main model (sec. 6) keeps log_vol as a free covariate,
# as in the original file 7. Sec. 10 at the bottom ALSO implements the
# modeling choice from file 13: traffic folded into the offset with its
# elasticity fixed at 1, instead of freely estimated.
# ==============================================================================

graphics.off()
rm(list = ls())

if (!require(pacman)) install.packages("pacman")
pacman::p_load(rstudioapi, sf, dplyr, ggplot2, tidyr, glmmTMB, stringi, stringr,
               lubridate, readxl, readr, DHARMa)

source("utils/packages.R")

load("Base_Esplorazione_offset.RData")


# ==============================================================================
# 1. WEATHER — RAIN definition from a single source (logic from file 12)
#    "Rain" = the FENOMENI column contains the word "piogg"
#    (captures "pioggia", "pioggia temporale", "pioggia nebbia", etc.)
#    Computed here FIRST because it is needed both to flag the
#    accidents (numerator) and the exposure days (denominator).
# ==============================================================================

meteo <- read_excel(
  "data/Rome/meteo_roma_completo.csv.xlsx",
  sheet = "meteo_roma_completo"
)

mesi_map <- c(
  "Gennaio" = "Jan", "Febbraio" = "Feb", "Marzo" = "Mar", "Aprile" = "Apr",
  "Maggio" = "May", "Giugno" = "Jun", "Luglio" = "Jul", "Agosto" = "Aug",
  "Settembre" = "Sep", "Ottobre" = "Oct", "Novembre" = "Nov", "Dicembre" = "Dec"
)

meteo <- meteo %>%
  mutate(
    date     = as_date(DATA),
    Month    = mesi_map[Mese],
    Year     = year(date),
    wday_num = as.POSIXlt(DATA)$wday,
    DayType  = ifelse(wday_num %in% c(0, 6), "weekend", "weekday"),
    # logic from file 12: "rain" = the word "piogg" in FENOMENI
    Weather_simple = if_else(
      str_detect(str_to_lower(coalesce(FENOMENI, "")), "piogg"),
      "Rain", "Dry"
    )
  )

cat("Total weather days:", nrow(meteo),
    "| rainy:", sum(meteo$Weather_simple == "Rain"),
    "| dry:", sum(meteo$Weather_simple == "Dry"),
    "| NA dates:", sum(is.na(meteo$date)), "\n")

anni_meteo <- sort(unique(meteo$Year))

mesi_ordine <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# ==============================================================================
# 2. DATA PREPARATION — accidents flagged Rain/Dry from the SAME source (file 12)
#    instead of recoding an already-aggregated Weather_simple column.
#    NB: adapt the name of the accident date column (here assumed to be
#    "DateHour"), as indicated by the ADAPT comment in file 12.
# ==============================================================================

acc <- acc %>%
  mutate(DEN_Z_URB = stri_trans_general(DEN_Z_URB, "Latin-ASCII"))

mappa_quartieri <- mappa_quartieri %>%
  mutate(DEN_Z_URB = stri_trans_general(DEN_Z_URB, "Latin-ASCII"))

acc <- acc %>%
  mutate(
    Traffic = ifelse(Traffic == "Unknown", "Normale", Traffic),
    date    = as_date(DateHour)          # <-- ADAPT: real name of the date column
  ) %>%
  select(-Weather_simple) %>%             # removes the previous Clear/NotClear version
  left_join(meteo %>% select(date, Weather_simple), by = "date") %>%
  filter(!is.na(Weather_simple)) %>%      # excludes accidents without weather data (as in file 12)
  filter(Year >= 2017 & Year <= 2021 & Year != 2020)

cat("Accidents flagged - rain:", sum(acc$Weather_simple == "Rain"),
    "| dry:", sum(acc$Weather_simple == "Dry"), "\n")

# ==============================================================================
# 3. WEATHER EXPOSURE — Year x Month granularity (logic from file 12+13)
#
#    NB compared to giorni_wide in file 12: that table is Year x
#    month_num WITHOUT DayType, because the model in file 13 does not
#    stratify by DayType. Model 7, on the other hand, counts Y separately
#    for weekday/weekend, so the exposure must have the same
#    granularity as the response: without DayType, weekday and weekend in the
#    same month would end up with the same monthly denominator,
#    even though the available weekdays (~22) and weekend days
#    (~8-9) differ - a systematic bias on the offset.
#    The style/names of giorni_wide (file 12) are therefore kept, adding
#    DayType as an extra grouping dimension.
# ==============================================================================

giorni_wide <- meteo %>%
  filter(!is.na(date)) %>%
  count(Year, Month, DayType, Weather_simple, name = "n_giorni_cond") %>%
  pivot_wider(names_from = Weather_simple, values_from = n_giorni_cond,
              values_fill = 0) %>%
  rename(giorni_rain = Rain, giorni_dry = Dry)

print(head(giorni_wide))

# long format for the join with scenario_vol (one row per condition)
esposizione_annua <- giorni_wide %>%
  pivot_longer(cols = c(giorni_rain, giorni_dry),
               names_to = "Weather_simple", values_to = "giorni_esposizione") %>%
  mutate(Weather_simple = recode(Weather_simple,
                                  giorni_rain = "Rain", giorni_dry = "Dry"))
esposizione_annua <- esposizione_annua %>%
  mutate(giorni_esposizione = na_if(giorni_esposizione, 0))

# pooled fallback (across all years) for missing Year-specific cells
# (same imputation logic that the original file 7 uses for
# missing traffic)
giorni_wide_pool <- meteo %>%
  filter(!is.na(date)) %>%
  count(Month, DayType, Weather_simple, name = "n_giorni_cond") %>%
  pivot_wider(names_from = Weather_simple, values_from = n_giorni_cond,
              values_fill = 0) %>%
  rename(giorni_rain = Rain, giorni_dry = Dry)

esposizione_pool <- giorni_wide_pool %>%
  pivot_longer(cols = c(giorni_rain, giorni_dry),
               names_to = "Weather_simple", values_to = "giorni_esposizione_pool") %>%
  mutate(Weather_simple = recode(Weather_simple,
                                  giorni_rain = "Rain", giorni_dry = "Dry"))

# ==============================================================================
# 4. TRAFFIC TABLE — volume_veicoli per DEN_Z_URB x Month x DayType x DayPeriod
#    (unchanged from file 7: traffic remains a free covariate,
#    it is not folded into the offset as in file 13)
# ==============================================================================

traffico_raw <- read_delim(
  "data/Rome/tabella_modello_arricchita.csv",
  delim     = ";",
  col_types = cols(.default = col_character())
)

traffico <- traffico_raw %>%
  mutate(
    Year            = as.numeric(Year),
    workdays_light  = as.numeric(workdays_light),
    workdays_heavy  = as.numeric(workdays_heavy),
    saturdays_light = as.numeric(saturdays_light),
    saturdays_heavy = as.numeric(saturdays_heavy),
    sundays_light   = as.numeric(sundays_light),
    sundays_heavy   = as.numeric(sundays_heavy)
  ) %>%
  mutate(
    weekday_vol = replace_na(workdays_light,  0) + replace_na(workdays_heavy,  0),
    weekend_vol = replace_na(saturdays_light, 0) + replace_na(saturdays_heavy, 0) +
                  replace_na(sundays_light,   0) + replace_na(sundays_heavy,   0)
  ) %>%
  select(DEN_Z_URB, Year, Month, DayPeriod, id_segment, weekday_vol, weekend_vol) %>%
  pivot_longer(
    cols      = c(weekday_vol, weekend_vol),
    names_to  = "DayType",
    values_to = "vol"
  ) %>%
  mutate(DayType = ifelse(DayType == "weekday_vol", "weekday", "weekend")) %>%
  group_by(DEN_Z_URB, Year, Month, DayType, DayPeriod) %>%
  summarise(
    volume_veicoli = mean(vol, na.rm = TRUE),
    n_segmenti     = n(),
    .groups        = "drop"
  )

anni_comuni <- intersect(sort(unique(traffico$Year)), anni_meteo)
cat("Years used in the model:", anni_comuni, "\n")

# ==============================================================================
# 5. SCENARIO CONSTRUCTION — now with Year in the grid (logic from file 12+13)
#    Y and giorni_esposizione are computed per Year x Month x DayType x
#    Weather_simple x DayPeriod, NO LONGER pooled over the whole period:
#    this way every row genuinely reflects the weather of THAT year/month.
# ==============================================================================

scenario_vol <- acc %>%
  filter(Year %in% anni_comuni) %>%
  group_by(DEN_Z_URB, Year, Month, DayType, Weather_simple, DayPeriod) %>%
  summarise(Y = n(), .groups = "drop") %>%
  complete(DEN_Z_URB, Year, Month, DayType, Weather_simple, DayPeriod,
           fill = list(Y = 0))

scenario_vol$Month     <- factor(scenario_vol$Month, levels = mesi_ordine)
scenario_vol$Month     <- relevel(scenario_vol$Month, ref = "Jan")
scenario_vol$DayPeriod <- as.factor(scenario_vol$DayPeriod)
scenario_vol$DayPeriod <- relevel(scenario_vol$DayPeriod, ref = "Morning")

# Join exposure: Year-specific first, then pooled fallback if missing
scenario_vol <- scenario_vol %>%
  left_join(esposizione_annua, by = c("Year", "Month", "DayType", "Weather_simple")) %>%
  left_join(esposizione_pool, by = c("Month", "DayType", "Weather_simple")) %>%
  mutate(
    giorni_esposizione = ifelse(is.na(giorni_esposizione),
                                 giorni_esposizione_pool, giorni_esposizione)
  ) %>%
  select(-giorni_esposizione_pool)

stopifnot(!any(is.na(scenario_vol$giorni_esposizione)))


traffico <- traffico %>%
  filter(Year %in% anni_comuni) %>%
  group_by(DEN_Z_URB, Month, DayType, DayPeriod,Year) 

scenario_vol <- scenario_vol %>%
  left_join(traffico, by = c("DEN_Z_URB", "Month", "DayType", "DayPeriod","Year"))

# Impute missing volume via the categorical Traffic variable (unchanged)
traffic_cat <- acc %>%
  filter(Year %in% anni_comuni) %>%
  group_by(DEN_Z_URB, Month, DayType, DayPeriod, Year, Traffic) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(DEN_Z_URB, Month, DayType, DayPeriod,Year) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  select(DEN_Z_URB, Month, DayType, DayPeriod, Year,Traffic_cat = Traffic)

scenario_vol <- scenario_vol %>%
  left_join(traffic_cat, by = c("DEN_Z_URB", "Month", "DayType", "DayPeriod", "Year"))

calibrazione <- scenario_vol %>%
  filter(!is.na(volume_veicoli)) %>%
  group_by(Traffic_cat, DayType, DayPeriod) %>%
  summarise(vol_imputed = median(volume_veicoli, na.rm = TRUE), .groups = "drop")

scenario_vol <- scenario_vol %>%
  left_join(calibrazione, by = c("Traffic_cat", "DayType", "DayPeriod")) %>%
  mutate(
    volume_veicoli = ifelse(is.na(volume_veicoli), vol_imputed, volume_veicoli)
  ) %>%
  select(-vol_imputed) %>%
  filter(!is.na(volume_veicoli)) %>%
  mutate(log_vol = log(volume_veicoli))

cat(sprintf("Rows in the final dataset: %d\n", nrow(scenario_vol)))
cat(sprintf("Proportion of zeros: %.1f%%\n", 100 * mean(scenario_vol$Y == 0)))

# ==============================================================================
# 6. FINAL MODEL: ZINB with log_vol + weather offset (granular Year x Month)
#    Year is NOT in the formula as a fixed predictor (as in 13__GLMM_roads.R,
#    where Year is only used to correctly build the grid/exposure,
#    not as a model term).
# ==============================================================================

model_zi_nb_vol_full <- glmmTMB(
  Y ~ Month + DayType + Weather_simple + log_vol + DayPeriod +
    offset(log(giorni_esposizione)) +
    (1 | DEN_Z_URB),
  ziformula = ~ DayType + Weather_simple + log_vol + DayPeriod,
  family     = nbinom2,
  data       = scenario_vol
)

summary(model_zi_nb_vol_full)

cat("\n--- Estimated traffic elasticity (log_vol coeff.) ---\n")
print(fixef(model_zi_nb_vol_full)$cond["log_vol"])

cat("\n--- Rain effect (Weather_simpleRain coeff.) ---\n")
print(fixef(model_zi_nb_vol_full)$cond["Weather_simpleRain"])

# ==============================================================================
# 7. FAMILY COMPARISON + DHARMa DIAGNOSTICS (logic from file 13, sec. 6-7)
# ==============================================================================

m_pois <- glmmTMB(
  Y ~ Month + DayType + Weather_simple + log_vol + DayPeriod +
    stats::offset(log(giorni_esposizione)) + (1 | DEN_Z_URB),
  family = poisson, data = scenario_vol
)

m_nb <- glmmTMB(
  Y ~ Month + DayType + Weather_simple + log_vol + DayPeriod +
    offset(log(giorni_esposizione)) + (1 | DEN_Z_URB),
  family = nbinom2, data = scenario_vol
)

m_zip <- glmmTMB(
  Y ~ Month + DayType + Weather_simple + log_vol + DayPeriod +
    offset(log(giorni_esposizione)) + (1 | DEN_Z_URB),
  ziformula = ~1, family = poisson, data = scenario_vol
)

cat("\n--- Family comparison (AIC/BIC) ---\n")
print(AIC(m_pois, m_nb, m_zip, model_zi_nb_vol_full))
print(BIC(m_pois, m_nb, m_zip, model_zi_nb_vol_full))

cat("\n--- DHARMa diagnostics on the final model ---\n")
sim <- simulateResiduals(model_zi_nb_vol_full)
plot(sim)
testDispersion(sim)
testZeroInflation(sim)

# ==============================================================================
# 9. PREDICTIONS AND MAP (unchanged: exposure for the single night remains
#    set to 1, regardless of year - Year is not in the model)
# ==============================================================================

vol_mediano <- scenario_vol %>%
  group_by(DEN_Z_URB, DayType, DayPeriod) %>%
  summarise(log_vol_med = median(log_vol, na.rm = TRUE), .groups = "drop")

scenario_pred_vol <- expand.grid(
  DEN_Z_URB      = unique(scenario_vol$DEN_Z_URB),
  Month          = unique(scenario_vol$Month),
  DayType        = "weekend",
  Weather_simple = "Rain",
  DayPeriod      = "Night"
) %>%
  left_join(
    vol_mediano %>% filter(DayType == "weekend", DayPeriod == "Night"),
    by = c("DEN_Z_URB", "DayType", "DayPeriod")
  ) %>%
  mutate(
    log_vol            = log_vol_med,
    giorni_esposizione = 1   # expected accidents in a single night
  )

scenario_pred_vol$mu_hat <- predict(
  model_zi_nb_vol_full,
  newdata = scenario_pred_vol,
  type    = "response"
)

pred_quartieri_vol <- scenario_pred_vol %>%
  group_by(DEN_Z_URB) %>%
  summarise(mu_hat = mean(mu_hat), .groups = "drop") %>%
  arrange(desc(mu_hat))

cat("\n--- Top 20 neighborhoods by expected accident rate ---\n")
print(head(pred_quartieri_vol, 20))

mappa_pred_vol <- mappa_quartieri %>%
  left_join(pred_quartieri_vol, by = "DEN_Z_URB")

ggplot(mappa_pred_vol) +
  geom_sf(aes(fill = mu_hat), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c(na.value = "grey90") +
  labs(
    title    = "Predicted accident intensity (ZINB, log_vol + weather Rain/Dry offset)",
    subtitle = "weekend night, Rain, average traffic volume",
    fill     = "Expected accidents\n(single night)"
  ) +
  theme_minimal()

# ==============================================================================
# 10. ALTERNATIVE MODEL — modeling choice from file 13
#
#     In 13__GLMM_roads.R traffic is NOT a covariate: it is part
#     of the exposure, in the same way as road_len and the
#     weather-day share:
#         exposure = road_len * traffic_val * share
#         ... + offset(log(exposure))
#     This is equivalent to FIXING the traffic elasticity at 1 (a
#     doubling of traffic doubles the expected accidents "by
#     definition"), instead of freely estimating it as log_vol does in the
#     main model (where the elasticity is the estimated coefficient,
#     typically different from 1).
#
#     At the neighborhood level there is no equivalent of road_len, so
#     the "file-13-style" exposure here is:
#         exposure = giorni_esposizione * volume_veicoli * area_km2
#     where area_km2 replaces road_len as a measure of the "size"
#     of the spatial unit (a larger neighborhood has more opportunities
#     for an accident, for the same traffic and weather).
#     log_vol is dropped from the fixed part (and from the ziformula, for consistency:
#     traffic is no longer a separate predictor, it is in the offset).
# ==============================================================================

# Neighborhood area (substitute for road_len). NB: the pre-computed
# Shape_Area field is not trusted without checking the CRS - if
# mappa_quartieri is in geographic coordinates, Shape_Area would be in
# degrees^2 and would have no physical meaning. It is recomputed with st_area() after
# a transformation to a metric CRS (32633 = UTM 33N, suitable for Rome).
cat("CRS of mappa_quartieri:", st_crs(mappa_quartieri)$input, "\n")

crs_metrico <- 32633  # UTM 33N, suitable for Rome

mappa_quartieri <- st_transform(mappa_quartieri, crs_metrico)
roads           <- st_transform(roads, crs_metrico)

# 2. (recommended) Fix any invalid geometries
mappa_quartieri <- st_make_valid(mappa_quartieri)
roads <- st_make_valid(roads)

# 3. Intersection: clip the roads to the neighborhood boundaries
roads_quartieri <- st_intersection(roads, mappa_quartieri)

# 4. Length of each resulting segment
roads_quartieri <- roads_quartieri %>%
  mutate(length_m = st_length(geometry))

# 5. Sum per neighborhood
km_per_quartiere <- roads_quartieri %>%
  st_drop_geometry() %>%
  group_by(DEN_Z_URB) %>%       # <- put the neighborhood ID column name here
  summarise(km_strade = sum(as.numeric(length_m)) / 1000) %>%
  arrange(desc(km_strade))


scenario_vol <- scenario_vol %>%
  left_join(km_per_quartiere, by = "DEN_Z_URB")



scenario_vol <- scenario_vol %>%
  mutate(esposizione_fissa = giorni_esposizione * km_strade)

stopifnot(all(scenario_vol$esposizione_fissa > 0))

model_exposure_offset <- glmmTMB(
  Y ~ Month + DayType + Weather_simple + DayPeriod + log_vol +
    offset(log(esposizione_fissa)) +
    (1 | DEN_Z_URB),
  ziformula = ~ DayType + Weather_simple + DayPeriod,
  family     = nbinom2,
  data       = scenario_vol
)

summary(model_exposure_offset)

cat("\n--- Rain effect ---\n")
print(fixef(model_exposure_offset)$cond["Weather_simpleRain"])

cat("\n--- Comparison: weather-only offset vs weather*km_strade offset ---\n")
print(AIC(model_zi_nb_vol_full, model_exposure_offset))
print(BIC(model_zi_nb_vol_full, model_exposure_offset))


sim_offset <- simulateResiduals(model_exposure_offset)
testOutliers(sim_offset,type="bootstrap")
plotResiduals(sim_offset)
testDispersion(sim_offset)
testZeroInflation(sim_offset)

# ==============================================================================
# 11. SAVING
# ==============================================================================

save(
  model_zi_nb_vol_full,
  model_exposure_offset,
  #m_pois, m_nb, m_zip,
  scenario_vol,
  traffico,
  esposizione_annua,
  mappa_quartieri,
  file = "zi_frequency_model_objects_vol_v2.RData"
)
