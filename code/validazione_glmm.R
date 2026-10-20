load("modelli_incidenti_strade.RData")
roads <- readRDS("roads.rds")
df2 <- readRDS("df2.rds")
# ============================================================
#  VALIDATION of the road GLMM (m_glmm_dp_no_road_len_no_Z_URB)
#  1. DHARMa  - residuals, dispersion, zero-inflation, spatial autocorr.
#  2. pseudo-R^2 (marginal and conditional)
#  3. Cross-validation with SPATIAL BLOCKS (grid cells) WITHIN the neighborhood
#
#  Requires in memory: m_glmm_dp_no_road_len_no_Z_URB, df, roads  (from the model script)
# ============================================================

str(dataset_quartieri_sf)
names(dataset_quartieri_sf)
head(as.data.frame(dataset_quartieri_sf), 3)
unique(dataset_quartieri_sf$Year)   # or the name of the year column

library(glmmTMB)
library(DHARMa)
library(sf)
library(dplyr)

# ------------------------------------------------------------
# 0. ROAD CENTROIDS (needed for spatial DHARMa and the CV grid)
#    roads has road_id and geometry; project to meters (UTM 33N) for the cells.
# ------------------------------------------------------------
roads_m <- st_transform(roads, 32633)
centro <- roads_m %>%
  mutate(road_id = as.integer(road_id)) %>%
  { bind_cols(road_id = .$road_id,
              as.data.frame(st_coordinates(st_centroid(st_geometry(.))))) } %>%
  rename(X = X, Y = Y)
# align the centroids to the order of the road_ids present in df
df <- df2 %>% mutate(road_id_int = as.integer(as.character(road_id)))

# ============================================================
# 1. DHARMa
# ============================================================
set.seed(1)
sim <- simulateResiduals(m_glmm_dp_no_road_len_no_Z_URB, n = 1000)

cat("\n==== 1. DHARMa ====\n")
plot(sim_dharma)
print(testUniformity(sim_dharma))
print(testDispersion(sim_dharma))
print(testZeroInflation(sim_dharma))
plotResiduals(sim_dharma, form = df$Meteo)
plotResiduals(sim_dharma, form = df$DayPeriod)
testOutliers(sim_dharma, type = "bootstrap")

# spatial autocorrelation of residuals aggregated by road
sim_road <- recalculateResiduals(sim, group = df$road_id)
rid_order <- as.integer(as.character(unique(df$road_id)))   # order of the groups
cm <- centro[match(rid_order, centro$road_id), ]
cat("\n-- Moran's I on residuals (per road) --\n")
print(testSpatialAutocorrelation(sim_road_dharma, x = cm$X, y = cm$Y))

# ============================================================
# 2. PSEUDO-R^2
# ============================================================
# robust alternative
install.packages("performance")
library(performance)# if you don't have it
performance::r2(m_r2)

# ============================================================
# 3. SPATIAL BLOCK CV (cells) WITHIN the neighborhood
#    - overlay a grid of cells (CELL_M meters) on the road centroids
#    - each road -> one cell (unique id neighborhood+cell)
#    - leave one cell out at a time, refit on the rest and predict the cell
#    Prediction WITHIN the neighborhood but in contiguous blocks -> no leakage
#    between immediately adjacent roads.
# ============================================================
library(parallel)

# resume the block-CV code (CELL_M, df_cv, blocchi, etc.)
# but also save the per-row predictions, not just RMSE per block

cl <- makeCluster(4L)
clusterExport(cl, "df_cv_min")
clusterEvalQ(cl, { library(glmmTMB); library(dplyr); NULL })
clusterSetRNGStream(cl, 1)

cv_list <- parLapply(cl, blocchi, function(b) {
  tryCatch({
    train <- df_cv_min[df_cv_min$block != b, , drop = FALSE]
    test  <- df_cv_min[df_cv_min$block == b, , drop = FALSE]
    if (nrow(test) == 0 || nrow(train) == 0) return(NULL)
    
    m_cv <- glmmTMB(
      n_accidents ~ Month + DayPeriod + DayType + Meteo + log_traffico +
        (1 | road_id) + offset(log(esposizione)),
      family = nbinom2, data = train,
      control = glmmTMBControl(parallel = 1)
    )
    pred <- predict(m_cv, newdata = test, type = "response",
                    allow.new.levels = TRUE)
    
    # NOTE: save road_id, obs and pred for every row of the test set
    data.frame(
      block   = b,
      road_id = test$road_id,
      obs     = test$n_accidents,
      pred    = pred
    )
  }, error = function(e) NULL)
})
stopCluster(cl)

# combine all out-of-sample predictions into a single dataframe
cv_full <- do.call(rbind, cv_list)
cat("Total out-of-sample CV rows:", nrow(cv_full), "\n")

# ---- CELL LEVEL (pointwise) ----
RMSE_cv_cell <- sqrt(mean((cv_full$obs - cv_full$pred)^2))
baseline_cell <- mean(cv_full$obs)
RMSE_cv_cell_baseline <- sqrt(mean((cv_full$obs - baseline_cell)^2))

cat("==== CV — cell level ====\n")
cat("CV RMSE:      ", round(RMSE_cv_cell, 4), "\n")
cat("Baseline RMSE:", round(RMSE_cv_cell_baseline, 4), "\n")
cat("Improvement:", round((1 - RMSE_cv_cell/RMSE_cv_cell_baseline)*100, 1), "%\n\n")

# ---- ROAD LEVEL (aggregated) ----
cv_road <- cv_full %>%
  group_by(road_id) %>%
  summarise(obs_tot = sum(obs), pred_tot = sum(pred), .groups = "drop")

RMSE_cv_road <- sqrt(mean((cv_road$obs_tot - cv_road$pred_tot)^2))
baseline_road <- mean(cv_road$obs_tot)
RMSE_cv_road_baseline <- sqrt(mean((cv_road$obs_tot - baseline_road)^2))

cat("==== CV — road level ====\n")
cat("CV RMSE:      ", round(RMSE_cv_road, 4), "\n")
cat("Baseline RMSE:", round(RMSE_cv_road_baseline, 4), "\n")
cat("Improvement:", round((1 - RMSE_cv_road/RMSE_cv_road_baseline)*100, 1), "%\n")
cat("Per-road correlation:", round(cor(cv_road$obs_tot, cv_road$pred_tot), 3), "\n")

# ============================================================
#  NOTES
#  - CELL_M controls the block size: smaller cells = more blocks,
#    more local test but slower (refits the model for every block).
#  - The CV refits the GLMM once per block: with many blocks this can
#    be slow. To speed it up, increase CELL_M (fewer, larger blocks).
#  - Prediction within the neighborhood: the neighborhood random effect is
#    known (the neighborhood is in the training set), only the individual
#    roads in the block are new.
# ============================================================

saveRDS(sim,      "sim_dharma.rds")
saveRDS(sim_road, "sim_road_dharma.rds")


