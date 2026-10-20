load("Base_Esplorazione_offset.RData")
# ============================================================
#  GLMM expected accidents per road  -  WITH DayPeriod + DayType + RAIN
#  Grid: road_id x Year x Month x DayPeriod x DayType x Meteo{Rain,Dry}
#  Fixed covariates: Month + DayPeriod + DayType + Meteo + log_traffico
#  Offset: log(road_len * giorni_condizione_meteo)   [road_len in KM]
#  Random effects: road_id, DEN_Z_URB
#
#  - TRAFFIC: covariate (log), NOT offset
#  - OFFSET: road_len(km) * ABSOLUTE days of the weather condition
# ============================================================

library(sf)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)
library(data.table)
library(glmmTMB)
library(readxl)
library(shiny)

quartieri_target <- c("Esquilino", "Torre Angela", "Centro Storico")

COL_PIOGGIA_ACC <- "rain_day"
VAL_PIOGGIA     <- "Rain"

# ------------------------------------------------------------
# 0. WEATHER: Rain/Dry days per year-month (ABSOLUTE count -> offset)
# ------------------------------------------------------------
meteo <- read_excel("meteo_roma_completo.csv.xlsx") %>%
  mutate(
    date     = as_date(DATA),
    rain_day = if_else(str_detect(str_to_lower(coalesce(FENOMENI, "")), "piogg"),
                       "Rain", "Dry"),
    Year     = year(date)
  )
cat("Weather - date range:", as.character(range(meteo$date)), "\n")
anni_meteo <- sort(unique(meteo$Year))

giorni_cond <- meteo %>%
  mutate(Month = month.abb[month(date)]) %>%
  count(Year, Month, rain_day, name = "g_cond")

anni_target <- intersect(2017:2021, anni_meteo)
cat("Years used (accidents & weather):", paste(anni_target, collapse = ", "), "\n")

# ------------------------------------------------------------
# 1. Accidents (acc, already flagged with rain) + spatial join to road
# ------------------------------------------------------------
acc2 <- acc %>%
  filter(DEN_Z_URB %in% quartieri_target,
         as.integer(Year) %in% anni_target,
         !is.na(.data[[COL_PIOGGIA_ACC]])) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  mutate(Meteo = if_else(.data[[COL_PIOGGIA_ACC]] == VAL_PIOGGIA, "Rain", "Dry"))

roads <- st_transform(st_as_sf(roads), st_crs(acc2))
roads$road_id  <- seq_len(nrow(roads))
roads$road_len <- as.numeric(st_length(roads)) / 1000   # length in KM

nearest <- st_nearest_feature(acc2, roads)
acc2$road_id  <- roads$road_id[nearest]
acc2$highway  <- roads$highway[nearest]
acc2$road_len <- roads$road_len[nearest]

# ------------------------------------------------------------
# 2. Counts: road x Year x Month x DayPeriod x DayType x Meteo
# ------------------------------------------------------------
counts <- acc2 %>%
  st_drop_geometry() %>%
  mutate(Year = as.integer(Year)) %>%
  count(road_id, highway, DEN_Z_URB, road_len,
        Year, Month, DayPeriod, DayType, Meteo, name = "n_accidents")

# ------------------------------------------------------------
# 3. Full grid with true zeros
# ------------------------------------------------------------
road_info <- counts %>% distinct(road_id, highway, DEN_Z_URB, road_len)
months_all  <- sort(unique(counts$Month))
periods_all <- sort(unique(counts$DayPeriod))
daytype_all <- sort(unique(counts$DayType))
meteo_all   <- c("Rain", "Dry")

df <- road_info %>%
  crossing(Year = anni_target, Month = months_all,
           DayPeriod = periods_all, DayType = daytype_all, Meteo = meteo_all) %>%
  left_join(counts,
            by = c("road_id","highway","DEN_Z_URB","road_len",
                   "Year","Month","DayPeriod","DayType","Meteo")) %>%
  mutate(n_accidents = replace_na(n_accidents, 0L))

# ------------------------------------------------------------
# 4. OFFSET = road_len * giorni_condizione 
# ------------------------------------------------------------
traffico <- fread("tabella_modello_arricchita.csv")
traffico_agg <- traffico %>%
  mutate(Year = as.integer(Year)) %>%
  group_by(DEN_Z_URB, Year, Month, DayPeriod) %>%
  summarise(traffico_val = mean(valore, na.rm = TRUE), .groups = "drop")

df <- df %>%
  mutate(rain_day = Meteo) %>%
  left_join(giorni_cond, by = c("Year","Month","rain_day")) %>%
  left_join(traffico_agg, by = c("DEN_Z_URB","Year","Month","DayPeriod")) %>%
  mutate(g_cond = replace_na(g_cond, 0L))

cat("Cells without traffic data:", sum(is.na(df$traffico_val)),
    "| without weather-days:", sum(df$g_cond == 0), "\n")

df <- df %>%
  filter(!is.na(traffico_val), traffico_val > 0, g_cond > 0) %>%
  mutate(
    esposizione  = road_len * g_cond,
    log_traffico = log(traffico_val)
  )

df2 <- df %>%
  filter(!is.na(traffico_val), traffico_val > 0, g_cond > 0) %>%
  mutate(
    esposizione  = g_cond,
    log_traffico = log(traffico_val)
  )

df <- df %>%
  mutate(
    Month     = factor(Month),
    DayPeriod = factor(DayPeriod),
    DayType   = factor(DayType),
    Meteo     = factor(Meteo, levels = c("Dry","Rain")),
    road_id   = factor(road_id),
    DEN_Z_URB = factor(DEN_Z_URB),
    Year      = factor(Year)
  )

df2 <- df2 %>%
  mutate(
    Month     = factor(Month),
    DayPeriod = factor(DayPeriod),
    DayType   = factor(DayType),
    Meteo     = factor(Meteo, levels = c("Dry","Rain")),
    road_id   = factor(road_id),
    DEN_Z_URB = factor(DEN_Z_URB),
    Year      = factor(Year)
  )

cat("Rows:", nrow(df), "| roads:", nlevels(df$road_id),
    "| zero share:", round(mean(df$n_accidents == 0), 3), "\n")

# ------------------------------------------------------------
# 5. Negative Binomial GLMM
#    Covariates: Month + DayPeriod + DayType + Meteo + log_traffico
# ------------------------------------------------------------
m_glmm_dp <- glmmTMB(
  n_accidents ~ Month + DayPeriod + DayType + Meteo + log_traffico +
    (1 | road_id) + (1 | DEN_Z_URB) +
    offset(log(esposizione)),
  family = nbinom2,
  data   = df
)
summary(m_glmm_dp)
# MeteoRain: exp(coef) = relative risk of rain vs dry per day.
# log_traffico: accident elasticity with respect to traffic.

# ------------------------------------------------------------
# 6. Family comparison
# ------------------------------------------------------------
m_pois <- glmmTMB(
  n_accidents ~ Month + DayPeriod + DayType + Meteo + log_traffico +
    (1 | road_id) + (1 | DEN_Z_URB) + offset(log(esposizione)),
  family = poisson, data = df
)
m_zip <- glmmTMB(
  n_accidents ~ Month + DayPeriod + DayType + Meteo + log_traffico +
    (1 | road_id) + (1 | DEN_Z_URB) + offset(log(esposizione)),
  ziformula = ~1, family = poisson, data = df
)
print(AIC(m_pois, m_zip, m_glmm_dp))
print(BIC(m_pois, m_zip, m_glmm_dp))




# ------------------------------------------------------------
# 5. Negative Binomial GLMM WITHOUT ROAD LEN
#    Covariates: Month + DayPeriod + DayType + Meteo + log_traffico
# ------------------------------------------------------------
m_glmm_dp_no_road_len <- glmmTMB(
  n_accidents ~ Month + DayPeriod + DayType + Meteo + log_traffico +
    (1 | road_id) + (1 | DEN_Z_URB) +
    offset(log(esposizione)),
  family = nbinom2,
  data   = df2
)
summary(m_glmm_dp_no_road_len)
# MeteoRain: exp(coef) = relative risk of rain vs dry per day.
# log_traffico: accident elasticity with respect to traffic.

# ------------------------------------------------------------
# 6. Family comparison WITHOUT ROAD LEN
# ------------------------------------------------------------
m_pois_no_road_len <- glmmTMB(
  n_accidents ~ Month + DayPeriod + DayType + Meteo + log_traffico +
    (1 | road_id) + (1 | DEN_Z_URB) + offset(log(esposizione)),
  family = poisson, data = df2
)
m_zip_no_road_len <- glmmTMB(
  n_accidents ~ Month + DayPeriod + DayType + Meteo + log_traffico +
    (1 | road_id) + (1 | DEN_Z_URB) + offset(log(esposizione)),
  ziformula = ~1, family = poisson, data = df2
)
print(AIC(m_pois_no_road_len, m_zip_no_road_len, m_glmm_dp_no_road_len ))
print(BIC(m_pois_no_road_len, m_zip_no_road_len, m_glmm_dp_no_road_len ))

# ------------------------------------------------------------
# 5. Negative Binomial GLMM WITHOUT NEIGHBORHOOD
#    Covariates: Month + DayPeriod + DayType + Meteo + log_traffico
# ------------------------------------------------------------
m_glmm_dp_no_road_len_no_Z_URB <- glmmTMB(
  n_accidents ~ Month + DayPeriod + DayType + Meteo + log_traffico +
    (1 | road_id) +
    offset(log(esposizione)),
  family = nbinom2,
  data   = df2
)
summary(m_glmm_dp_no_road_len_no_Z_URB)
# MeteoRain: exp(coef) = relative risk of rain vs dry per day.
# log_traffico: accident elasticity with respect to traffic.

# ------------------------------------------------------------
# 6. Family comparison WITHOUT NEIGHBORHOOD
# ------------------------------------------------------------
m_pois_no_road_len_no_Z_URB <- glmmTMB(
  n_accidents ~ Month + DayPeriod + DayType + Meteo + log_traffico +
    (1 | road_id) + offset(log(esposizione)),
  family = poisson, data = df2
)
m_zip_no_road_len_no_Z_URB <- glmmTMB(
  n_accidents ~ Month + DayPeriod + DayType + Meteo + log_traffico +
    (1 | road_id) + offset(log(esposizione)),
  ziformula = ~1, family = poisson, data = df2
)
print(AIC(m_pois_no_road_len_no_Z_URB, m_zip_no_road_len_no_Z_URB, m_glmm_dp_no_road_len_no_Z_URB ))
print(BIC(m_pois_no_road_len_no_Z_URB, m_zip_no_road_len_no_Z_URB, m_glmm_dp_no_road_len_no_Z_URB ))

anova(m_zip_no_road_len_no_Z_URB, m_glmm_dp_no_road_len_no_Z_URB)



# ------------------------------------------------------------
# 7. DHARMa diagnostics
# ------------------------------------------------------------
### NO ROAD LEN

library(DHARMa)
sim <- simulateResiduals(m_glmm_dp_no_road_len)
plot(sim)
testDispersion(sim)
testZeroInflation(sim)

### ROAD LEN

library(DHARMa)
sim <- simulateResiduals(m_glmm_dp)
plot(sim)
testDispersion(sim)
testZeroInflation(sim)


# ------------------------------------------------------------
# 8. Prediction: road x month x time-of-day x daytype x weather
# ------------------------------------------------------------
traffico_ref <- traffico_agg %>%
  group_by(DEN_Z_URB, Month, DayPeriod) %>%
  summarise(traffico_val = mean(traffico_val, na.rm = TRUE), .groups = "drop")
giorni_ref <- giorni_cond %>%
  rename(Meteo = rain_day) %>%
  group_by(Month, Meteo) %>%
  summarise(g_cond = mean(g_cond), .groups = "drop")

pred_grid <- road_info %>%
  mutate(across(c(road_id, highway, DEN_Z_URB), as.factor)) %>%
  crossing(Month = levels(df$Month), DayPeriod = levels(df$DayPeriod),
           DayType = levels(df$DayType), Meteo = levels(df$Meteo)) %>%
  mutate(
    Month     = factor(Month,     levels = levels(df$Month)),
    DayPeriod = factor(DayPeriod, levels = levels(df$DayPeriod)),
    DayType   = factor(DayType,   levels = levels(df$DayType)),
    Meteo     = factor(Meteo,     levels = levels(df$Meteo))
  ) %>%
  left_join(traffico_ref, by = c("DEN_Z_URB","Month","DayPeriod")) %>%
  left_join(giorni_ref %>% mutate(Meteo = factor(Meteo, levels = levels(df$Meteo))),
            by = c("Month","Meteo")) %>%
  filter(!is.na(traffico_val), traffico_val > 0, !is.na(g_cond), g_cond > 0) %>%
  mutate(esposizione  = g_cond,
         log_traffico = log(traffico_val))

pred_grid$expected <- predict(m_glmm_dp_no_road_len_no_Z_URB, newdata = pred_grid, type = "response")
head(pred_grid)

# ------------------------------------------------------------
# 9. Saves for the Shiny app
# ------------------------------------------------------------
saveRDS(acc, "acc.rds")   # in the same folder as pred_grid.rds, roads.rds, etc.
saveRDS(df2, "df2.rds")
saveRDS(pred_grid, "pred_grid.rds")
saveRDS(roads,     "roads.rds")
saveRDS(mappa_quartieri, "mappa_quartieri.rds")
save(m_glmm_dp_no_road_len,m_glmm_dp, m_pois,m_pois_no_road_len, m_zip,m_zip_no_road_len,m_glmm_dp_no_road_len_no_Z_URB, m_pois_no_road_len_no_Z_URB, m_zip_no_road_len_no_Z_URB , file = "modelli_incidenti_strade.RData")
shiny::runApp("app_glmm.R")
