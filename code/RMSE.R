library(glmmTMB)
library(dplyr)

# load the model and training data
load("modelli_incidenti_strade.RData")   # contains m_glmm_dp_no_road_len_no_Z_URB
df2 <- readRDS("df2.rds")

m <- m_glmm_dp_no_road_len_no_Z_URB

# predictions on the training data
pred <- predict(m, type = "response")
obs  <- df2$n_accidents

# RMSE
RMSE <- sqrt(mean((obs - pred)^2))
MAE  <- mean(abs(obs - pred))

cat("RMSE (in-sample):", round(RMSE, 4), "\n")
cat("MAE  (in-sample):", round(MAE, 4), "\n")

# baseline: always predict the mean
baseline_pred <- mean(obs)
RMSE_baseline <- sqrt(mean((obs - baseline_pred)^2))
cat("RMSE baseline (constant mean):", round(RMSE_baseline, 4), "\n")
cat("Improvement over baseline:",
    round((1 - RMSE/RMSE_baseline) * 100, 1), "%\n")

# observed-predicted correlation
cat("Corr(obs, pred):", round(cor(obs, pred), 3), "\n")

rmse_per_road <- df2 %>%
  mutate(pred = pred) %>%
  group_by(road_id) %>%
  summarise(
    obs_tot  = sum(n_accidents),
    pred_tot = sum(pred),
    .groups = "drop"
  )

RMSE_road <- sqrt(mean((rmse_per_road$obs_tot - rmse_per_road$pred_tot)^2))
cat("RMSE aggregated per road:", round(RMSE_road, 4), "\n")
cat("Corr per road (totals):", round(cor(rmse_per_road$obs_tot, rmse_per_road$pred_tot), 3), "\n")


# per-road baseline: always predict the observed per-road mean
baseline_road <- mean(rmse_per_road$obs_tot)

RMSE_road_baseline <- sqrt(mean((rmse_per_road$obs_tot - baseline_road)^2))

cat("RMSE GLMM (per road):    ", round(RMSE_road, 4), "\n")
cat("RMSE baseline (per road):", round(RMSE_road_baseline, 4), "\n")
cat("Improvement over baseline (per road):",
    round((1 - RMSE_road / RMSE_road_baseline) * 100, 1), "%\n")
