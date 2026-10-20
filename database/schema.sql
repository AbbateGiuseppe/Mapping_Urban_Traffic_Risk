-- ============================================================
-- Mapping Urban Traffic Risk -- showcase database schema
--
-- Contains ONLY aggregated / model-derived results (predictions,
-- fitted model coefficients, functional-PCA summaries, reference
-- geometry). No individual accident record is stored here.
-- ============================================================

PRAGMA foreign_keys = ON;

-- Reference: the 3 neighborhoods this study focuses on, plus all 155
-- official Rome urban zones (for context / mapping).
CREATE TABLE neighborhoods (
    neighborhood    TEXT PRIMARY KEY,
    area_m2         REAL,
    perimeter_m     REAL,
    centroid_lon    REAL,
    centroid_lat    REAL,
    is_study_area   INTEGER NOT NULL DEFAULT 0  -- 1 = one of the 3 focus neighborhoods
);

-- Reference: road segments inside the 3 study neighborhoods
-- (OSM-derived network used by the GLMM). A handful of roads sit
-- right on a neighborhood boundary and so appear once per
-- neighborhood they were assigned to -- hence the composite key.
CREATE TABLE roads (
    road_id         TEXT NOT NULL,
    highway_type    TEXT,
    road_len        REAL,
    neighborhood    TEXT NOT NULL REFERENCES neighborhoods(neighborhood),
    centroid_lon    REAL,
    centroid_lat    REAL,
    PRIMARY KEY (road_id, neighborhood)
);
CREATE INDEX idx_roads_road_id ON roads(road_id);

-- Model comparison table: every GLMM variant that was fit, with its
-- formula, family, AIC/BIC and n_obs -- the actual model-selection
-- record from the project.
CREATE TABLE model_registry (
    model_name      TEXT PRIMARY KEY,
    family          TEXT,
    formula         TEXT,
    aic             REAL,
    bic             REAL,
    n_obs           INTEGER,
    description     TEXT
);

-- Fixed-effect coefficient table for every model in model_registry.
CREATE TABLE model_coefficients (
    model_name      TEXT NOT NULL REFERENCES model_registry(model_name),
    term            TEXT NOT NULL,
    estimate        REAL,
    std_error       REAL,
    z_value         REAL,
    p_value         REAL,
    PRIMARY KEY (model_name, term)
);

-- Core results table: the GLMM's predicted expected accident rate for
-- every (road, month, day-period, day-type, weather) combination.
-- This is the main table to query ("which road/time/condition has the
-- highest predicted risk").
CREATE TABLE glmm_predictions (
    road_id                      TEXT NOT NULL,
    neighborhood                 TEXT NOT NULL REFERENCES neighborhoods(neighborhood),
    highway_type                 TEXT,
    road_len                     REAL,
    month                        TEXT NOT NULL,   -- 3-letter English month abbreviation
    day_period                   TEXT NOT NULL,   -- Morning / Afternoon / Evening / Night
    day_type                     TEXT NOT NULL,   -- weekday / weekend
    weather                      TEXT NOT NULL,   -- Dry / Rain
    traffic_volume               REAL,
    exposure_days                REAL,
    log_traffic                  REAL,
    expected_accidents_per_day   REAL NOT NULL
);
CREATE INDEX idx_pred_road   ON glmm_predictions(road_id);
CREATE INDEX idx_pred_hood   ON glmm_predictions(neighborhood);
CREATE INDEX idx_pred_month  ON glmm_predictions(month);

-- Functional Data Analysis: B-spline basis coefficients of the
-- smoothed accident-rate curve (by hour of day / day of week / month
-- of year) for each of Rome's 155 neighborhoods.
CREATE TABLE fda_basis_coefficients (
    profile_type    TEXT NOT NULL,   -- 'hour' | 'day' | 'month'
    neighborhood    TEXT NOT NULL REFERENCES neighborhoods(neighborhood),
    basis_index     TEXT NOT NULL,
    coefficient     REAL NOT NULL,
    PRIMARY KEY (profile_type, neighborhood, basis_index)
);

-- Functional PCA: per-neighborhood scores on the first principal
-- components of each temporal profile (used as the input for the
-- k-means clustering step).
CREATE TABLE fpca_scores (
    profile_type    TEXT NOT NULL,
    neighborhood    TEXT NOT NULL REFERENCES neighborhoods(neighborhood),
    pc1             REAL,
    pc2             REAL,
    pc3             REAL,
    PRIMARY KEY (profile_type, neighborhood)
);

CREATE TABLE fpca_variance_explained (
    profile_type            TEXT NOT NULL,
    component               INTEGER NOT NULL,
    variance_proportion     REAL NOT NULL,
    PRIMARY KEY (profile_type, component)
);

-- ------------------------------------------------------------
-- The tables below are part of the project's methodology but their
-- source artifacts were not available at export time (see README /
-- docs/techniques.md). They are created empty and ready to receive
-- data once the corresponding analysis script is re-run and exported.
-- ------------------------------------------------------------

-- Spatially-blocked / leave-one-neighborhood-out cross-validation
-- results for the frequency (ZINB) and severity (logistic) models.
CREATE TABLE cv_zinb_metrics (
    metric          TEXT PRIMARY KEY,
    value           REAL
);

CREATE TABLE cv_zinb_by_fold (
    fold            INTEGER PRIMARY KEY,
    rmse            REAL,
    pearson_r       REAL,
    n_obs           INTEGER
);

CREATE TABLE cv_severity_metrics (
    metric          TEXT PRIMARY KEY,
    value           REAL
);

CREATE TABLE cv_severity_by_fold (
    fold            INTEGER PRIMARY KEY,
    auc             REAL,
    n_obs           INTEGER
);

-- FDA + k-means cluster assignment per neighborhood (hour/day/month
-- profiles), from "05.1 fda_kmeans_quartieri.R".
CREATE TABLE fda_cluster_assignments (
    neighborhood    TEXT PRIMARY KEY REFERENCES neighborhoods(neighborhood),
    cluster_hour    INTEGER,
    cluster_month   INTEGER
);

-- Regression-kriging-corrected predictions per road
-- (from "15.regression_kriging_residui.R").
CREATE TABLE kriging_corrected_predictions (
    road_id             TEXT PRIMARY KEY,
    mu_mean             REAL,   -- original GLMM-predicted mean
    mu_corretto         REAL    -- kriging-corrected mean
);

-- Logistic GLMM coefficients for accident SEVERITY (from
-- "08_glmm_severity.r"); separate from model_coefficients because the
-- severity model object wasn't available to export yet.
CREATE TABLE severity_model_coefficients (
    model_name      TEXT NOT NULL,
    term            TEXT NOT NULL,
    estimate        REAL,
    std_error       REAL,
    z_value         REAL,
    p_value         REAL,
    PRIMARY KEY (model_name, term)
);

-- Moran's I / Getis-Ord Gi* spatial-autocorrelation test results
-- (from "12.1 analisi_spaziale_preliminare.R").
CREATE TABLE spatial_autocorrelation_tests (
    test_name       TEXT PRIMARY KEY,   -- e.g. "Moran's I - GLMM residuals"
    statistic       REAL,
    p_value         REAL,
    notes           TEXT
);

-- Curated methodology reference: every technique used in the project,
-- independent of whether its numeric output is currently populated
-- above. This is what makes the "which techniques were used" question
-- directly queryable.
CREATE TABLE techniques (
    technique_name      TEXT PRIMARY KEY,
    category            TEXT NOT NULL,   -- preprocessing / modeling / validation / spatial / clustering / visualization
    description         TEXT NOT NULL,
    script_reference    TEXT,
    results_table       TEXT             -- which table(s) above hold its numeric output, if any
);
