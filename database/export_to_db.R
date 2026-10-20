#!/usr/bin/env Rscript
# ============================================================
#  EXPORT PIPELINE: Rome Traffic Risk project -> aggregated CSV exports
#  for the "Mapping_Urban_Traffic_Risk" showcase database.
#
#  IMPORTANT: this script extracts ONLY aggregated / model-derived
#  results (predictions, model coefficients, functional-PCA summaries,
#  reference geometry). It deliberately does NOT export individual
#  accident records (raw microdata stays out of the public repo).
#
#  Run from the "Progetto base" folder (same folder as the .rds/.RData
#  files it reads). Writes CSVs into ./db_exports/.
# ============================================================

suppressMessages({
  library(sf)
  library(dplyr)
  library(glmmTMB)
})

OUT <- "db_exports"
dir.create(OUT, showWarnings = FALSE)

write_out <- function(df, name) {
  f <- file.path(OUT, paste0(name, ".csv"))
  write.csv(df, f, row.names = FALSE)
  cat("wrote:", f, " (", nrow(df), "rows )\n")
}

# ------------------------------------------------------------
# helper: extract a glmmTMB model's fixed-effect coefficient table
# using only CACHED values (fixef + sdr$cov.fixed), so it works even
# when the installed glmmTMB/TMB version differs from the one the
# model was originally fit with (which breaks summary()/VarCorr()
# because those trigger a fresh TMB recompilation).
# ------------------------------------------------------------
extract_coefs <- function(m, model_name) {
  fe <- tryCatch(fixef(m)$cond, error = function(e) NULL)
  if (is.null(fe)) return(NULL)
  terms <- names(fe)
  n <- length(fe)
  se <- rep(NA_real_, n)
  if (!is.null(m$sdr) && !is.null(m$sdr$cov.fixed)) {
    cv <- m$sdr$cov.fixed
    if (nrow(cv) >= n) {
      se <- sqrt(diag(cv))[seq_len(n)]
    }
  }
  z <- as.numeric(fe) / se
  p <- 2 * pnorm(-abs(z))
  data.frame(
    model_name = model_name,
    term       = terms,
    estimate   = as.numeric(fe),
    std_error  = se,
    z_value    = z,
    p_value    = p,
    stringsAsFactors = FALSE
  )
}

extract_model_meta <- function(m, model_name, description) {
  aic_val  <- tryCatch(AIC(m), error = function(e) NA_real_)
  bic_val  <- tryCatch(BIC(m), error = function(e) NA_real_)
  nobs_val <- tryCatch(nobs(m), error = function(e) NA_integer_)
  fam      <- tryCatch(family(m)$family, error = function(e) NA_character_)
  form     <- tryCatch(deparse(formula(m), width.cutoff = 500L), error = function(e) NA_character_)
  form     <- paste(form, collapse = " ")
  data.frame(
    model_name  = model_name,
    family      = fam,
    formula     = form,
    aic         = aic_val,
    bic         = bic_val,
    n_obs       = nobs_val,
    description = description,
    stringsAsFactors = FALSE
  )
}

model_meta_rows <- list()
coef_rows       <- list()

# ============================================================
# 1. GLMM frequency models (model-comparison set)
# ============================================================
if (file.exists("modelli_incidenti_strade.RData")) {
  e1 <- new.env()
  load("modelli_incidenti_strade.RData", envir = e1)

  descr <- c(
    m_pois                        = "Poisson GLMM, full predictor set (baseline count model)",
    m_pois_no_road_len            = "Poisson GLMM, road length dropped from predictors",
    m_pois_no_road_len_no_Z_URB   = "Poisson GLMM, road length and neighborhood random intercept dropped",
    m_zip                         = "Zero-inflated Poisson GLMM, full predictor set",
    m_zip_no_road_len             = "Zero-inflated Poisson GLMM, road length dropped",
    m_zip_no_road_len_no_Z_URB    = "Zero-inflated Poisson GLMM, road length and neighborhood random intercept dropped",
    m_glmm_dp                     = "Negative-binomial (nbinom2) GLMM, full predictor set",
    m_glmm_dp_no_road_len         = "Negative-binomial (nbinom2) GLMM, road length dropped",
    m_glmm_dp_no_road_len_no_Z_URB = "Negative-binomial (nbinom2) GLMM, road length and neighborhood random intercept dropped -- FINAL model used for prediction / Shiny app"
  )

  for (nm in ls(e1)) {
    m <- get(nm, envir = e1)
    if (!inherits(m, "glmmTMB")) next
    d <- if (!is.na(descr[nm])) descr[[nm]] else ""
    model_meta_rows[[nm]] <- extract_model_meta(m, nm, d)
    cf <- extract_coefs(m, nm)
    if (!is.null(cf)) coef_rows[[nm]] <- cf
  }
  rm(e1); gc()
}

# ============================================================
# 2. Spatial GLMM (exponential spatial covariance on road centroids)
# ============================================================
if (file.exists("modello_spatial.RData")) {
  e2 <- new.env()
  load("modello_spatial.RData", envir = e2)
  if (exists("m_spatial", envir = e2)) {
    m <- get("m_spatial", envir = e2)
    model_meta_rows[["m_spatial"]] <- extract_model_meta(
      m, "m_spatial",
      "Negative-binomial GLMM with an exponential spatial covariance term exp(pos + 0 | group) on road centroids, to check whether unexplained spatial autocorrelation remains after fixed effects"
    )
    cf <- extract_coefs(m, "m_spatial")
    if (!is.null(cf)) coef_rows[["m_spatial"]] <- cf
  }
  rm(e2); gc()
}

if (length(model_meta_rows) > 0) {
  write_out(bind_rows(model_meta_rows), "model_registry")
}
if (length(coef_rows) > 0) {
  write_out(bind_rows(coef_rows), "model_coefficients")
}

# ============================================================
# 3. GLMM prediction grid (road x month x day-period x day-type x weather)
#    -- this is the core, queryable "results" table.
# ============================================================
if (file.exists("pred_grid.rds")) {
  pg <- readRDS("pred_grid.rds")
  pg_out <- pg %>%
    transmute(
      road_id          = as.character(road_id),
      neighborhood     = DEN_Z_URB,
      highway_type     = as.character(highway),
      road_len         = road_len,
      month            = Month,
      day_period       = DayPeriod,
      day_type         = DayType,
      weather          = Meteo,
      traffic_volume   = traffico_val,
      exposure_days    = g_cond,
      log_traffic      = log_traffico,
      expected_accidents_per_day = expected
    )
  write_out(pg_out, "glmm_predictions")
}

# ============================================================
# 4. Reference tables: roads and neighborhoods (geometry simplified
#    to centroid lat/lon -- no need for full polygons/linestrings in
#    a SQLite showcase db).
# ============================================================
if (file.exists("precomputed.RData")) {
  e3 <- new.env()
  load("precomputed.RData", envir = e3)

  if (exists("roads_with_zone", envir = e3)) {
    rwz <- get("roads_with_zone", envir = e3)
    rwz <- st_as_sf(rwz)
    cen <- suppressWarnings(st_centroid(st_geometry(rwz)))
    cen_ll <- st_coordinates(st_transform(cen, 4326))
    rwz_out <- rwz %>%
      st_drop_geometry() %>%
      transmute(
        road_id        = as.character(road_id),
        highway_type   = as.character(highway),
        road_len       = road_len,
        neighborhood   = DEN_Z_URB
      ) %>%
      mutate(centroid_lon = cen_ll[, "X"], centroid_lat = cen_ll[, "Y"])
    # keep only roads inside the 3 study neighborhoods, to match the
    # scope of the study (full Rome network is ~49k segments and is
    # just OSM reference data, not part of the analysis)
    study_hoods <- c("Centro Storico", "Esquilino", "Torre Angela")
    rwz_study <- rwz_out %>% filter(neighborhood %in% study_hoods)
    write_out(rwz_study, "roads")
  }

}

if (file.exists("mappa_quartieri.rds")) {
  mq <- st_as_sf(readRDS("mappa_quartieri.rds"))
  cen <- suppressWarnings(st_centroid(st_geometry(mq)))
  cen_ll <- st_coordinates(st_transform(st_transform(mq, 4326) %>% st_geometry() %>% st_centroid(), 4326))
  study_hoods <- c("Centro Storico", "Esquilino", "Torre Angela")
  mq_out <- mq %>%
    st_drop_geometry() %>%
    transmute(
      neighborhood  = DEN_Z_URB,
      area_m2       = Shape__Are,
      perimeter_m   = Shape__Len
    )
  mq_out$centroid_lon <- cen_ll[, "X"]
  mq_out$centroid_lat <- cen_ll[, "Y"]
  mq_out$is_study_area <- mq_out$neighborhood %in% study_hoods
  write_out(mq_out, "neighborhoods")
}

# ============================================================
# 5. Functional Data Analysis: basis coefficients + functional PCA
#    (temporal accident-rate profiles by hour / day-of-week / month,
#    per neighborhood -- aggregated curves, not individual accidents)
# ============================================================
if (file.exists("precomputed.RData")) {
  e4 <- new.env()
  load("precomputed.RData", envir = e4)

  fd_to_df <- function(fdobj, profile_type, hood_names) {
    co <- fdobj$coefs   # basis_coef x neighborhood matrix
    df <- as.data.frame(as.table(co))
    names(df) <- c("basis_index", "neighborhood_idx", "coefficient")
    df$neighborhood <- hood_names[as.integer(df$neighborhood_idx)]
    df$profile_type <- profile_type
    df[, c("profile_type", "neighborhood", "basis_index", "coefficient")]
  }

  pca_to_scores_df <- function(pcafd, profile_type, hood_names) {
    sc <- pcafd$scores
    colnames(sc) <- paste0("pc", seq_len(ncol(sc)))
    df <- as.data.frame(sc)
    df$neighborhood <- hood_names
    df$profile_type <- profile_type
    df
  }

  pca_to_varprop_df <- function(pcafd, profile_type) {
    data.frame(
      profile_type = profile_type,
      component    = seq_along(pcafd$varprop),
      variance_proportion = pcafd$varprop
    )
  }

  # neighborhood names/order: fd objects are indexed by the same
  # neighborhood ordering used when they were built (all 155 Rome
  # neighborhoods, matching quartieri_map order)
  hood_names <- NULL
  if (exists("quartieri_map", envir = e4)) {
    hood_names <- get("quartieri_map", envir = e4)$DEN_Z_URB
  }

  if (!is.null(hood_names)) {
    fd_all <- list()
    if (exists("fd_ora", envir = e4))  fd_all[["hour"]]  <- fd_to_df(get("fd_ora",  envir = e4), "hour",  hood_names)
    if (exists("fd_day", envir = e4))  fd_all[["day"]]   <- fd_to_df(get("fd_day",  envir = e4), "day",   hood_names)
    if (exists("fd_mese", envir = e4)) fd_all[["month"]] <- fd_to_df(get("fd_mese", envir = e4), "month", hood_names)
    if (length(fd_all) > 0) write_out(bind_rows(fd_all), "fda_basis_coefficients")

    pca_scores_all <- list()
    pca_var_all <- list()
    if (exists("pca_o", envir = e4)) {
      pca_scores_all[["hour"]]  <- pca_to_scores_df(get("pca_o", envir = e4), "hour", hood_names)
      pca_var_all[["hour"]]     <- pca_to_varprop_df(get("pca_o", envir = e4), "hour")
    }
    if (exists("pca_d", envir = e4)) {
      pca_scores_all[["day"]]   <- pca_to_scores_df(get("pca_d", envir = e4), "day", hood_names)
      pca_var_all[["day"]]      <- pca_to_varprop_df(get("pca_d", envir = e4), "day")
    }
    if (exists("pca_m", envir = e4)) {
      pca_scores_all[["month"]] <- pca_to_scores_df(get("pca_m", envir = e4), "month", hood_names)
      pca_var_all[["month"]]    <- pca_to_varprop_df(get("pca_m", envir = e4), "month")
    }
    if (length(pca_scores_all) > 0) write_out(bind_rows(pca_scores_all), "fpca_scores")
    if (length(pca_var_all) > 0) write_out(bind_rows(pca_var_all), "fpca_variance_explained")
  }
  rm(e4); gc()
}

# ============================================================
# 6. OPTIONAL extras -- only exported if these files exist (they are
#    produced by scripts that may not have been (re)run yet):
#      - cv_results.RData          (11_cross_validation.R)
#      - FDA_Clustering_Results.RData / cluster_comparison.csv (05.1 ...)
#      - pred_corretta_kriging.rds (15.regression_kriging_residui.R)
#    If you want these tables populated, re-run those scripts in R
#    (in this same folder) and then re-run this export script.
# ============================================================
if (file.exists("cv_results.RData")) {
  e5 <- new.env()
  load("cv_results.RData", envir = e5)
  if (exists("zinb_metrics", envir = e5)) write_out(as.data.frame(get("zinb_metrics", envir = e5)), "cv_zinb_metrics")
  if (exists("zinb_by_fold", envir = e5)) write_out(as.data.frame(get("zinb_by_fold", envir = e5)), "cv_zinb_by_fold")
  if (exists("sev_metrics", envir = e5))  write_out(as.data.frame(get("sev_metrics", envir = e5)), "cv_severity_metrics")
  if (exists("sev_by_fold", envir = e5))  write_out(as.data.frame(get("sev_by_fold", envir = e5)), "cv_severity_by_fold")
  cat("NOTE: cv_results.RData found and exported.\n")
} else {
  cat("NOTE: cv_results.RData not found on disk -- cross-validation tables left empty.\n")
  cat("      Re-run 11_cross_validation.R, then re-run this export script, to populate them.\n")
}

if (file.exists("cluster_comparison.csv")) {
  file.copy("cluster_comparison.csv", file.path(OUT, "fda_cluster_comparison.csv"), overwrite = TRUE)
  cat("NOTE: cluster_comparison.csv found and copied.\n")
} else {
  cat("NOTE: cluster_comparison.csv not found -- FDA k-means cluster-assignment table left empty.\n")
  cat("      Re-run \"05.1 fda_kmeans_quartieri.R\", then re-run this export script, to populate it.\n")
}

if (file.exists("pred_corretta_kriging.rds")) {
  pk <- readRDS("pred_corretta_kriging.rds")
  write_out(as.data.frame(pk), "kriging_corrected_predictions")
  cat("NOTE: pred_corretta_kriging.rds found and exported.\n")
} else {
  cat("NOTE: pred_corretta_kriging.rds not found -- regression-kriging table left empty.\n")
  cat("      Re-run \"15.regression_kriging_residui.R\", then re-run this export script, to populate it.\n")
}

cat("\n==== DONE. CSV exports written to:", normalizePath(OUT), "====\n")
