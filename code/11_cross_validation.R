# ==============================================================================
# 09_cross_validation.R
# Cross-validation of the two models:
#   ① ZINB GLMM   (accident frequency)  ← 07_zi_glmm_final.R
#   ② Logistic GLMM (accident severity) ← 08_glmm_severity.R
#
# Strategy:
#   • 5-Fold CV (observation level) — main metric
#   • Leave-One-Neighborhood-Out (LONO) — to assess spatial generalization
#
# Outputs produced:
#   cv_zinb_obs_vs_pred.png   — scatter observed vs predicted (ZINB)
#   cv_zinb_residui.png       — residuals per fold (ZINB)
#   cv_zinb_rootogram.png     — rootogram (ZINB)
#   cv_zinb_lono.png          — LONO RMSE per neighborhood (ZINB)
#   cv_severity_roc.png       — ROC curve (severity)
#   cv_severity_calibration.png — calibration plot (severity)
#   cv_severity_density.png   — distribution of P̂ per class (severity)
# ==============================================================================

# ── 0. Configuration ──────────────────────────────────────────────────────────
graphics.off()
rm(list = ls())

set.seed(42)
K_FOLDS   <- 5      # number of folds
RUN_LONO  <- FALSE   # set to FALSE to skip (faster)

if (!require(pacman)) install.packages("pacman")
pacman::p_load(
  glmmTMB, lme4, dplyr, tidyr, ggplot2,
  pROC, caret, stringi, readxl, readr, sf, lubridate
)


# ==============================================================================
# ══════════════════════════════════════════════════════════════════════════════
#  PART 1  ·  ZINB GLMM  (accident frequency)
# ══════════════════════════════════════════════════════════════════════════════
# ==============================================================================

cat("\n\n══════════════════════════════════════════════\n")
cat("   PART 1: Cross-Validation — ZINB GLMM\n")
cat("══════════════════════════════════════════════\n\n")

# ── 1.1  Load the objects produced by 07_zi_glmm_final.R ────────────────────
load("zi_frequency_model_objects_vol.RData")
# Available objects:
#   model_zi_nb_vol_full   — full model
#   scenario_vol           — aggregated dataset (row = covariate combination)
#   traffico_medio, esposizione, mappa_quartieri

n_zinb <- nrow(scenario_vol)
cat(sprintf("ZINB dataset: %d rows  |  %.1f%% zeros\n\n",
            n_zinb, 100 * mean(scenario_vol$Y == 0)))

# ── 1.2  Assign the folds (random shuffle) ──────────────────────────────────────
fold_id_zinb <- sample(rep(seq_len(K_FOLDS), length.out = n_zinb))

zinb_cv_list <- vector("list", K_FOLDS)

for (f in seq_len(K_FOLDS)) {
  cat(sprintf("  [ZINB] Fold %d / %d  ...", f, K_FOLDS))
  
  train_z <- scenario_vol[fold_id_zinb != f, ]
  test_z  <- scenario_vol[fold_id_zinb == f, ]
  
  # Re-fit the model on the training set
  fit_z <- tryCatch(
    glmmTMB(
      Y ~ Month + DayType + Weather_simple + log_vol + DayPeriod +
        stats::offset(log(giorni_esposizione)) +
        (1 | DEN_Z_URB),
      ziformula = ~ DayType + Weather_simple + log_vol + DayPeriod,
      family    = nbinom2,
      data      = train_z
    ),
    error = function(e) {
      cat(sprintf("  ✗ ERROR: %s\n", conditionMessage(e)))
      NULL
    }
  )
  
  if (!is.null(fit_z)) {
    pred_z <- predict(fit_z, newdata = test_z, type = "response",
                      allow.new.levels = TRUE)
    zinb_cv_list[[f]] <- data.frame(
      fold      = f,
      observed  = test_z$Y,
      predicted = pmax(pred_z, 0)   # safeguard against negative numerical values
    )
    cat(" ✓\n")
  }
}

zinb_cv <- do.call(rbind, zinb_cv_list)

# ── 1.3  Performance metrics — ZINB ───────────────────────────────────────
zinb_metrics <- zinb_cv %>%
  summarise(
    RMSE        = sqrt(mean((observed - predicted)^2)),
    MAE         = mean(abs(observed - predicted)),
    Pearson_r   = cor(observed, predicted),
    Spearman_r  = cor(observed, predicted, method = "spearman"),
    # Proportion of zeros correctly classified (threshold = 0.5)
    Zero_Acc    = mean((observed == 0) == (predicted < 0.5)),
    # Approximate log-score using Poisson (indicative metric)
    LogScore_approx = mean(dpois(observed, lambda = pmax(predicted, 1e-6), log = TRUE))
  )

# Metrics per fold (to check stability)
zinb_by_fold <- zinb_cv %>%
  group_by(fold) %>%
  summarise(
    RMSE = sqrt(mean((observed - predicted)^2)),
    MAE  = mean(abs(observed - predicted)),
    r    = cor(observed, predicted),
    .groups = "drop"
  )

cat("\n--- ZINB Metrics — 5-Fold CV ---\n")
print(as.data.frame(zinb_metrics))
cat("\n--- Metrics per Fold ---\n")
print(as.data.frame(zinb_by_fold))

# ── 1.4  Leave-One-Neighborhood-Out (LONO) — ZINB ────────────────────────────
if (RUN_LONO) {
  quartieri_zinb <- unique(scenario_vol$DEN_Z_URB)
  Q <- length(quartieri_zinb)
  lono_zinb_list <- vector("list", Q)
  
  cat(sprintf("\n=== ZINB: LONO across %d neighborhoods ===\n", Q))
  
  for (i in seq_along(quartieri_zinb)) {
    q_i     <- quartieri_zinb[i]
    train_l <- scenario_vol[scenario_vol$DEN_Z_URB != q_i, ]
    test_l  <- scenario_vol[scenario_vol$DEN_Z_URB == q_i, ]
    
    fit_l <- tryCatch(
      glmmTMB(
        Y ~ Month + DayType + Weather_simple + log_vol + DayPeriod +
          stats::offset(log(giorni_esposizione)) +
          (1 | DEN_Z_URB),
        ziformula = ~ DayType + Weather_simple + log_vol + DayPeriod,
        family    = nbinom2,
        data      = train_l
      ),
      error = function(e) NULL
    )
    
    if (!is.null(fit_l)) {
      pred_l <- predict(fit_l, newdata = test_l, type = "response",
                        allow.new.levels = TRUE)
      lono_zinb_list[[i]] <- data.frame(
        quartiere = q_i,
        RMSE_q    = sqrt(mean((test_l$Y - pred_l)^2)),
        MAE_q     = mean(abs(test_l$Y - pred_l)),
        r_q       = suppressWarnings(cor(test_l$Y, pred_l))
      )
    }
    
    if (i %% 10 == 0)
      cat(sprintf("  %d / %d neighborhoods completed\n", i, Q))
  }
  
  lono_zinb <- do.call(rbind, lono_zinb_list)
  
  cat(sprintf("\n  LONO mean RMSE: %.4f  |  mean MAE: %.4f\n",
              mean(lono_zinb$RMSE_q, na.rm = TRUE),
              mean(lono_zinb$MAE_q,  na.rm = TRUE)))
  cat("\n  Top 10 hardest-to-predict neighborhoods (LONO RMSE):\n")
  print(lono_zinb %>% arrange(desc(RMSE_q)) %>% head(10))
}

# ── 1.5  Plots — ZINB ───────────────────────────────────────────────────────

# (a) Observed vs Predicted
p_zinb_1 <- ggplot(zinb_cv, aes(x = predicted, y = observed)) +
  geom_jitter(alpha = 0.25, size = 0.7, color = "#2166AC") +
  geom_abline(slope = 1, intercept = 0, color = "#D6604D",
              linewidth = 0.9, linetype = "dashed") +
  geom_smooth(method = "loess", se = TRUE, color = "#4DAC26", linewidth = 0.8) +
  labs(
    title    = "ZINB GLMM — Observed vs Predicted (5-Fold CV)",
    subtitle = sprintf("Pearson r = %.3f  |  RMSE = %.4f",
                       zinb_metrics$Pearson_r, zinb_metrics$RMSE),
    x        = "Predicted value (μ̂)",
    y        = "Observed value (Y)"
  ) +
  theme_minimal(base_size = 12)

ggsave("cv_zinb_obs_vs_pred.png", p_zinb_1, width = 7, height = 5.5, dpi = 150)

# (b) Residuals vs Predicted per fold
zinb_cv <- zinb_cv %>%
  mutate(residuo = observed - predicted)

p_zinb_2 <- ggplot(zinb_cv, aes(x = predicted, y = residuo, color = factor(fold))) +
  geom_point(alpha = 0.25, size = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_smooth(aes(group = 1), method = "loess", se = FALSE,
              color = "black", linewidth = 0.8) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title  = "ZINB GLMM — Residuals per Fold",
    x      = "Predicted value",
    y      = "Residual (Y − ŷ)",
    color  = "Fold"
  ) +
  theme_minimal(base_size = 12)

ggsave("cv_zinb_residui.png", p_zinb_2, width = 7, height = 5.5, dpi = 150)

# (c) Rootogram (hanging rootogram for count models)
max_y <- min(max(zinb_cv$observed), 20)  # truncated for readability
rootogram_df <- zinb_cv %>%
  mutate(
    observed_bin = pmin(observed, max_y),
    pred_bin     = pmin(round(predicted), max_y)
  ) %>%
  group_by(count = observed_bin) %>%
  summarise(
    obs_freq  = n(),
    pred_freq = sum(predicted),  # sum of the expected means
    .groups   = "drop"
  ) %>%
  mutate(
    sqrt_obs  = sqrt(obs_freq),
    sqrt_pred = sqrt(pred_freq)
  )

p_zinb_3 <- ggplot(rootogram_df, aes(x = count)) +
  geom_col(aes(y = sqrt_obs), fill = "#4393C3", alpha = 0.6, width = 0.8) +
  geom_line(aes(y = sqrt_pred), color = "#D6604D", linewidth = 1) +
  geom_point(aes(y = sqrt_pred), color = "#D6604D", size = 2) +
  labs(
    title    = "ZINB GLMM — Rootogram (√frequency)",
    subtitle = "Bars = √observed  |  Red line = √predicted",
    x        = "Count Y",
    y        = "√Frequency"
  ) +
  theme_minimal(base_size = 12)

ggsave("cv_zinb_rootogram.png", p_zinb_3, width = 8, height = 5, dpi = 150)

# (d) LONO: RMSE per neighborhood
if (RUN_LONO && exists("lono_zinb")) {
  p_zinb_4 <- ggplot(lono_zinb,
                     aes(x = reorder(quartiere, RMSE_q), y = RMSE_q,
                         fill = RMSE_q)) +
    geom_col() +
    scale_fill_viridis_c(direction = -1) +
    coord_flip() +
    labs(
      title    = "ZINB GLMM — LONO: RMSE per Neighborhood",
      subtitle = sprintf("Mean RMSE = %.4f", mean(lono_zinb$RMSE_q, na.rm = TRUE)),
      x        = NULL,
      y        = "RMSE (leave-one-out)",
      fill     = "RMSE"
    ) +
    theme_minimal(base_size = 9) +
    theme(legend.position = "none")
  
  ggsave("cv_zinb_lono.png", p_zinb_4, width = 8, height = 10, dpi = 150)
}

# ==============================================================================
# ══════════════════════════════════════════════════════════════════════════════
#  PART 2  ·  Logistic GLMM  (accident severity)
# ══════════════════════════════════════════════════════════════════════════════
# ==============================================================================

cat("\n\n══════════════════════════════════════════════\n")
cat("   PART 2: Cross-Validation — Logistic GLMM\n")
cat("══════════════════════════════════════════════\n\n")

# ── 2.1  Load the objects produced by 08_glmm_severity.R ───────────────────
load("severity_model_objects.RData")
# Available objects:
#   modello_severita2_glmm  — full model
#   data_severity2          — individual-level dataset (row = accident)

# Consistent recoding of the factors (as in the original script)
mesi_ord <- c("Jan","Feb","Mar","Apr","May","Jun",
              "Jul","Aug","Sep","Oct","Nov","Dec")

data_severity2 <- data_severity2 %>%
  filter(!is.na(severity), !is.na(DEN_Z_URB)) %>%
  mutate(
    # Add as.character() to prevent the factors from turning into numbers
    Traffic    = ifelse(Traffic == "Unknown", "Normale", as.character(Traffic)),
    DayPeriod  = relevel(as.factor(DayPeriod), ref = "Morning"),
    Traffic    = relevel(as.factor(Traffic),   ref = "Normale"),
    Month      = relevel(factor(Month, levels = mesi_ord), ref = "Jan")
  )
n_sev <- nrow(data_severity2)
cat(sprintf("Severity dataset: %d rows  |  %.1f%% severe\n\n",
            n_sev, 100 * mean(data_severity2$severity)))

# ── 2.2  Folds stratified by severity ───────────────────────────────────────
# Stratification ensures that every fold has the same proportion
# of events (severity=1), essential for imbalanced classes.
folds_sev <- caret::createFolds(
  y    = data_severity2$severity,
  k    = K_FOLDS,
  list = TRUE,
  returnTrain = FALSE
)

sev_cv_list <- vector("list", K_FOLDS)

for (f in seq_len(K_FOLDS)) {
  cat(sprintf("  [Severity] Fold %d / %d  ...", f, K_FOLDS))
  
  idx_test <- folds_sev[[f]]
  train_s  <- data_severity2[-idx_test, ]
  test_s   <- data_severity2[ idx_test, ]
  
  fit_s <- tryCatch(
    lme4::glmer(
      severity ~ DayPeriod + Weekend + Weather_adverse +
                 Pedestrian_involved + Traffic + Month +
                 (1 | DEN_Z_URB),
      data    = train_s,
      family  = binomial(link = "logit"),
      control = glmerControl(
        optimizer = "bobyqa",
        optCtrl   = list(maxfun = 2e5)
      )
    ),
    error = function(e) {
      cat(sprintf("  ✗ ERROR: %s\n", conditionMessage(e)))
      NULL
    }
  )
  
  if (!is.null(fit_s)) {
    pred_p <- predict(fit_s, newdata = test_s, type = "response",
                      allow.new.levels = TRUE)
    sev_cv_list[[f]] <- data.frame(
      fold        = f,
      observed    = test_s$severity,
      predicted_p = pred_p
    )
    cat(" ✓\n")
  }
}

sev_cv <- do.call(rbind, sev_cv_list)

# ── 2.3  Performance metrics — Severity ──────────────────────────────────────

# ROC curve aggregated over all folds
roc_obj <- pROC::roc(sev_cv$observed, sev_cv$predicted_p,
                     quiet = TRUE, direction = "<")
auc_val <- pROC::auc(roc_obj)

# Optimal threshold (maximizes Youden J = Sens + Spec − 1)
best_coords <- pROC::coords(
  roc_obj, "best",
  ret            = c("threshold", "sensitivity", "specificity", "ppv", "npv"),
  best.method    = "youden",
  transpose      = FALSE
)
thresh_opt <- best_coords$threshold[1]

pred_class <- as.integer(sev_cv$predicted_p >= thresh_opt)

# Confusion matrix
cm <- table(Predicted = pred_class, Observed = sev_cv$observed)
cat("\n--- Confusion Matrix (optimal Youden threshold) ---\n")
print(cm)

# Log-loss (cross-entropy)
eps     <- 1e-9
logloss <- -mean(
  sev_cv$observed * log(sev_cv$predicted_p + eps) +
  (1 - sev_cv$observed) * log(1 - sev_cv$predicted_p + eps)
)

# Overall metrics
sev_metrics <- data.frame(
  AUC         = as.numeric(auc_val),
  Brier_Score = mean((sev_cv$observed - sev_cv$predicted_p)^2),
  Log_Loss    = logloss,
  Accuracy    = mean(pred_class == sev_cv$observed),
  Sensitivity = best_coords$sensitivity[1],
  Specificity = best_coords$specificity[1],
  PPV         = best_coords$ppv[1],
  NPV         = best_coords$npv[1],
  Threshold   = thresh_opt
)

# Metrics per fold
sev_by_fold <- sev_cv %>%
  group_by(fold) %>%
  summarise(
    AUC         = as.numeric(pROC::auc(pROC::roc(observed, predicted_p, quiet = TRUE))),
    Brier_Score = mean((observed - predicted_p)^2),
    .groups     = "drop"
  )

cat("\n--- Severity Metrics — 5-Fold CV (stratified) ---\n")
print(as.data.frame(sev_metrics))
cat("\n--- Metrics per Fold ---\n")
print(as.data.frame(sev_by_fold))

# ── 2.4  LONO — Logistic GLMM ────────────────────────────────────────────────
if (RUN_LONO) {
  quartieri_sev <- unique(data_severity2$DEN_Z_URB)
  Q2 <- length(quartieri_sev)
  lono_sev_list <- vector("list", Q2)
  
  cat(sprintf("\n=== Severity: LONO across %d neighborhoods ===\n", Q2))
  
  for (i in seq_along(quartieri_sev)) {
    q_i      <- quartieri_sev[i]
    train_ls <- data_severity2[data_severity2$DEN_Z_URB != q_i, ]
    test_ls  <- data_severity2[data_severity2$DEN_Z_URB == q_i, ]
    
    # Skip if the test set has only one severity level
    if (length(unique(test_ls$severity)) < 2) next
    
    fit_ls <- tryCatch(
      lme4::glmer(
        severity ~ DayPeriod + Weekend + Weather_adverse +
          Pedestrian_involved + Traffic + Month +
          (1 | DEN_Z_URB),
        data    = train_ls,
        family  = binomial(link = "logit"),
        control = glmerControl(optimizer = "bobyqa",
                               optCtrl   = list(maxfun = 2e5))
      ),
      error = function(e) NULL
    )
    
    if (!is.null(fit_ls)) {
      pred_ls <- predict(fit_ls, newdata = test_ls, type = "response",
                         allow.new.levels = TRUE)
      lono_sev_list[[i]] <- data.frame(
        quartiere   = q_i,
        AUC_q       = tryCatch(
          as.numeric(pROC::auc(pROC::roc(test_ls$severity, pred_ls, quiet = TRUE))),
          error = function(e) NA
        ),
        Brier_q     = mean((test_ls$severity - pred_ls)^2)
      )
    }
    
    if (i %% 10 == 0)
      cat(sprintf("  %d / %d neighborhoods completed\n", i, Q2))
  }
  
  lono_sev <- do.call(rbind, Filter(Negate(is.null), lono_sev_list))
  
  cat(sprintf("\n  LONO mean AUC:   %.4f\n", mean(lono_sev$AUC_q,   na.rm = TRUE)))
  cat(sprintf("  LONO mean Brier: %.4f\n",   mean(lono_sev$Brier_q, na.rm = TRUE)))
}


# ── 2.5  Plots — Severity ───────────────────────────────────────────────────

# (a) ROC curve with AUC and optimal threshold
png("cv_severity_roc.png", width = 700, height = 620, res = 130)
plot(
  roc_obj,
  col       = "#D6604D",
  lwd       = 2.5,
  main      = sprintf("Logistic GLMM — ROC Curve  (5-Fold CV)\nAUC = %.3f", auc_val),
  cex.main  = 1.1
)
abline(a = 0, b = 1, lty = 2, col = "grey60")
# Optimal threshold point
points(
  x   = 1 - sev_metrics$Specificity,
  y   = sev_metrics$Sensitivity,
  pch = 19, cex = 1.8, col = "#4393C3"
)
text(
  x      = 1 - sev_metrics$Specificity + 0.05,
  y      = sev_metrics$Sensitivity - 0.04,
  labels = sprintf("Threshold = %.3f\nSens=%.2f Spec=%.2f",
                   thresh_opt,
                   sev_metrics$Sensitivity,
                   sev_metrics$Specificity),
  cex    = 0.8, col = "#4393C3"
)
dev.off()

# (b) Calibration plot
N_BINS   <- 10
sev_cv   <- sev_cv %>%
  mutate(
    bin = cut(predicted_p,
              breaks         = seq(0, 1, length.out = N_BINS + 1),
              include.lowest = TRUE,
              labels         = FALSE)
  )

calib_df <- sev_cv %>%
  group_by(bin) %>%
  summarise(
    mean_pred = mean(predicted_p),
    mean_obs  = mean(observed),
    n         = n(),
    .groups   = "drop"
  )

# Hosmer-Lemeshow statistic (approximation)
hl_stat <- sum(
  (calib_df$n * (calib_df$mean_obs - calib_df$mean_pred)^2) /
  (calib_df$n * calib_df$mean_pred * (1 - calib_df$mean_pred) + 1e-9)
)

p_sev_2 <- ggplot(calib_df, aes(x = mean_pred, y = mean_obs)) +
  geom_abline(slope = 1, intercept = 0,
              linetype = "dashed", color = "grey40") +
  geom_line(color = "#D6604D", linewidth = 0.8) +
  geom_point(aes(size = n), color = "#D6604D") +
  geom_text(aes(label = scales::comma(n)),
            vjust = -1, size = 2.8, color = "grey30") +
  scale_size_continuous(range = c(3, 9)) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title    = "Logistic GLMM — Calibration Plot (5-Fold CV)",
    subtitle = sprintf("Hosmer-Lemeshow χ² ≈ %.2f  (10 bin)", hl_stat),
    x        = "Mean predicted probability",
    y        = "Mean observed frequency",
    size     = "N observations"
  ) +
  theme_minimal(base_size = 12)

ggsave("cv_severity_calibration.png", p_sev_2, width = 6.5, height = 6.5, dpi = 150)

# (c) Distribution of P̂ per observed class
p_sev_3 <- ggplot(sev_cv, aes(x = predicted_p, fill = factor(observed))) +
  geom_density(alpha = 0.55, bw = 0.03) +
  geom_vline(xintercept = thresh_opt,
             linetype = "dashed", color = "grey20", linewidth = 0.8) +
  annotate("text",
           x     = thresh_opt + 0.02,
           y     = Inf,
           label = sprintf("threshold = %.3f", thresh_opt),
           vjust = 1.5, hjust = 0, size = 3.5, color = "grey20") +
  scale_fill_manual(
    values = c("#4393C3", "#D6604D"),
    labels = c("Not Severe (0)", "Severe (1)")
  ) +
  labs(
    title    = "Logistic GLMM — Distribution of P̂ per observed class",
    subtitle = sprintf("AUC = %.3f  |  Brier = %.4f",
                       sev_metrics$AUC, sev_metrics$Brier_Score),
    x        = "Predicted probability of severity",
    y        = "Density",
    fill     = NULL
  ) +
  theme_minimal(base_size = 12)

ggsave("cv_severity_density.png", p_sev_3, width = 7.5, height = 5, dpi = 150)

# (d) AUC stability per fold
p_sev_4 <- ggplot(sev_by_fold, aes(x = factor(fold), y = AUC)) +
  geom_col(fill = "#4393C3", width = 0.6) +
  geom_hline(yintercept = as.numeric(auc_val),
             linetype = "dashed", color = "#D6604D", linewidth = 0.9) +
  geom_text(aes(label = round(AUC, 3)),
            vjust = -0.4, size = 3.5) +
  ylim(0, 1) +
  labs(
    title    = "Logistic GLMM — AUC per Fold",
    subtitle = sprintf("Aggregate AUC = %.3f", as.numeric(auc_val)),
    x        = "Fold",
    y        = "AUC"
  ) +
  theme_minimal(base_size = 12)

ggsave("cv_severity_auc_by_fold.png", p_sev_4, width = 6, height = 4.5, dpi = 150)



save(
  zinb_cv, zinb_metrics, zinb_by_fold,
  sev_cv,  sev_metrics,  sev_by_fold,
  roc_obj, auc_val,
  file = "cv_results.RData"
)


