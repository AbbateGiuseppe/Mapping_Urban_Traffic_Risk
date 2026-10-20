#!/usr/bin/env python3
"""
Build mapping_urban_risk.db (SQLite) from the CSV exports produced by
export_to_db.R, plus a hand-curated `techniques` table documenting the
project's methodology.

Usage:
    python3 build_database.py [--exports-dir db_exports] [--out mapping_urban_risk.db]

Only aggregated / model-derived data goes into this database -- no
individual accident record is included.
"""
import argparse
import csv
import os
import sqlite3
import sys

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

# table_name -> (csv_filename, [(csv_column, db_column), ...] or None to use csv header as-is)
TABLE_SOURCES = {
    "neighborhoods": "neighborhoods.csv",
    "roads": "roads.csv",
    "model_registry": "model_registry.csv",
    "model_coefficients": "model_coefficients.csv",
    "glmm_predictions": "glmm_predictions.csv",
    "fda_basis_coefficients": "fda_basis_coefficients.csv",
    "fpca_scores": "fpca_scores.csv",
    "fpca_variance_explained": "fpca_variance_explained.csv",
    # optional / may not exist yet:
    "cv_zinb_metrics": "cv_zinb_metrics.csv",
    "cv_zinb_by_fold": "cv_zinb_by_fold.csv",
    "cv_severity_metrics": "cv_severity_metrics.csv",
    "cv_severity_by_fold": "cv_severity_by_fold.csv",
    "kriging_corrected_predictions": "kriging_corrected_predictions.csv",
}

BOOL_COLUMNS = {"is_study_area"}


def load_csv(path):
    with open(path, newline="", encoding="utf-8") as f:
        reader = csv.DictReader(f)
        rows = list(reader)
    return rows


def coerce(value, col):
    if value is None or value == "" or value == "NA":
        return None
    if col in BOOL_COLUMNS:
        return 1 if str(value).strip().upper() in ("TRUE", "1", "T") else 0
    # try numeric
    try:
        if "." in value or "e" in value.lower():
            return float(value)
        return int(value)
    except ValueError:
        try:
            return float(value)
        except ValueError:
            return value


def insert_csv(conn, table, csv_path):
    if not os.path.exists(csv_path):
        print(f"  (skip) {table}: {os.path.basename(csv_path)} not found -- table left empty")
        return 0
    rows = load_csv(csv_path)
    if not rows:
        print(f"  (empty) {table}: {os.path.basename(csv_path)} has no rows")
        return 0
    cols = list(rows[0].keys())
    placeholders = ",".join(["?"] * len(cols))
    col_list = ",".join(cols)
    sql = f"INSERT INTO {table} ({col_list}) VALUES ({placeholders})"
    data = [[coerce(r[c], c) for c in cols] for r in rows]
    conn.executemany(sql, data)
    print(f"  loaded {table}: {len(data)} rows (from {os.path.basename(csv_path)})")
    return len(data)


TECHNIQUES = [
    # (name, category, description, script_reference, results_table)
    (
        "Road-network preprocessing & map matching",
        "preprocessing",
        "Snapping of georeferenced accident points to the OSM road network; "
        "construction of the spatial mesh used for later spatial modeling.",
        "01_preprocessing.R",
        "roads, neighborhoods",
    ),
    (
        "Data cleaning",
        "preprocessing",
        "Filtering to 2017-2022, standardising accident-type / weather / road-surface "
        "categories, handling missing temporal and categorical data.",
        "02_cleaning.R",
        None,
    ),
    (
        "Exploratory data analysis",
        "eda",
        "Global and per-neighborhood descriptive analysis of accident counts, "
        "day/night and weekday/weekend splits, weather conditions.",
        "03_eda_global.R, 04_eda_neighborhoods.R, descrittiva_flag.R",
        None,
    ),
    (
        "Functional Data Analysis (B-spline smoothing)",
        "modeling",
        "Fourier/B-spline smoothing of the accident-rate curve by hour of day, "
        "day of week and month of year, separately for each of Rome's 155 "
        "official urban zones.",
        "05. Functional_Analysis_Hour_Month.R",
        "fda_basis_coefficients",
    ),
    (
        "Functional PCA + k-means clustering of neighborhoods",
        "clustering",
        "Principal component analysis on the functional (FDA) temporal profiles, "
        "followed by k-means clustering of neighborhoods by their hour/day/month "
        "accident-rate shape.",
        "05_pca_clustering.R, 05.1 fda_kmeans_quartieri.R",
        "fpca_scores, fpca_variance_explained, fda_cluster_assignments",
    ),
    (
        "Zero-inflated negative-binomial GLMM (accident frequency)",
        "modeling",
        "glmmTMB negative-binomial (nbinom2) generalized linear mixed model for "
        "the daily accident count per road segment, with month / day-period / "
        "day-type / weather / traffic fixed effects, a per-road random intercept, "
        "and an offset for exposure. Nine model variants were compared by AIC "
        "(with/without road length, with/without a neighborhood random intercept, "
        "Poisson vs. zero-inflated-Poisson vs. negative-binomial).",
        "07_zi_glmm_v2_rain_quota.R, 13. GLMM_roads.R, 10_zi_sync_freq_sev_risk_maps.R",
        "model_registry, model_coefficients, glmm_predictions",
    ),
    (
        "Logistic GLMM (accident severity)",
        "modeling",
        "lme4 logistic generalized linear mixed model predicting whether an "
        "accident involved a serious/fatal injury, from the same covariate set "
        "plus traffic and weather.",
        "08_glmm_severity.r",
        "severity_model_coefficients",
    ),
    (
        "Spatial autocorrelation diagnostics (Moran's I, Getis-Ord Gi*)",
        "spatial",
        "Tests for residual spatial autocorrelation in the fitted GLMM, and "
        "local indicators of spatial association (LISA / Getis-Ord Gi*) to "
        "identify accident hot-spots and cold-spots.",
        "12.1 analisi_spaziale_preliminare.R, validazione_glmm.R",
        "spatial_autocorrelation_tests",
    ),
    (
        "Spatial GLMM with exponential covariance (robustness check)",
        "spatial",
        "A variant of the frequency GLMM adding an exponential spatial "
        "covariance term on road centroids, to verify that the simpler model's "
        "residuals are not masking unmodeled spatial structure.",
        "16. Analisi Spaziale Supporto.R",
        "model_registry (m_spatial), model_coefficients",
    ),
    (
        "Spatially-blocked cross-validation",
        "validation",
        "Leave-one-neighborhood-out and spatial-block (grid-cell) cross-validation "
        "of both the frequency and severity models, refitting the model on the "
        "training blocks and scoring out-of-sample predictions (RMSE, Pearson "
        "correlation for frequency; AUC for severity) to avoid optimistic "
        "in-sample error estimates from spatially correlated data.",
        "11_cross_validation.R, validazione_glmm.R",
        "cv_zinb_metrics, cv_zinb_by_fold, cv_severity_metrics, cv_severity_by_fold",
    ),
    (
        "DHARMa simulation-based residual diagnostics",
        "validation",
        "Simulated-residual checks (uniformity, dispersion, zero-inflation, "
        "outliers) for the GLMM, via the DHARMa package.",
        "validazione_glmm.R",
        None,
    ),
    (
        "Regression kriging on Pearson residuals",
        "spatial",
        "Ordinary kriging of the GLMM's Pearson residuals over road centroids, "
        "used to produce a spatially corrected accident-rate surface on top of "
        "the GLMM's prediction.",
        "15.regression_kriging_residui.R",
        "kriging_corrected_predictions",
    ),
    (
        "Interactive Shiny dashboards",
        "visualization",
        "leaflet/leafgl-based interactive web apps for exploring observed "
        "accidents and GLMM-predicted risk by neighborhood, month, day-period, "
        "day-type and weather.",
        "app_map.R, app_glmm.R, app.R, app/app.R, app_anno.R",
        None,
    ),
    (
        "High-resolution poster map exports",
        "visualization",
        "Export of shared-color-scale static maps (GLMM rate, observed points, "
        "smoothed density) per neighborhood, with separate legend images, for a "
        "poster presentation.",
        "export_mappe_cartellone.R",
        None,
    ),
]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--exports-dir", default=os.path.join(SCRIPT_DIR, "..", "db_exports"))
    ap.add_argument("--schema", default=os.path.join(SCRIPT_DIR, "schema.sql"))
    ap.add_argument("--out", default=os.path.join(SCRIPT_DIR, "mapping_urban_risk.db"))
    args = ap.parse_args()

    if os.path.exists(args.out):
        os.remove(args.out)

    conn = sqlite3.connect(args.out)
    with open(args.schema, encoding="utf-8") as f:
        conn.executescript(f.read())

    print(f"Loading CSVs from: {os.path.abspath(args.exports_dir)}")
    for table, csv_name in TABLE_SOURCES.items():
        insert_csv(conn, table, os.path.join(args.exports_dir, csv_name))

    conn.executemany(
        "INSERT INTO techniques (technique_name, category, description, "
        "script_reference, results_table) VALUES (?, ?, ?, ?, ?)",
        TECHNIQUES,
    )
    print(f"  loaded techniques: {len(TECHNIQUES)} rows (curated)")

    conn.commit()

    # quick sanity report
    cur = conn.cursor()
    cur.execute("SELECT name FROM sqlite_master WHERE type='table' ORDER BY name")
    tables = [r[0] for r in cur.fetchall()]
    print("\nTables in", args.out, ":")
    for t in tables:
        cur.execute(f"SELECT COUNT(*) FROM {t}")
        n = cur.fetchone()[0]
        print(f"  {t}: {n} rows")

    conn.close()
    print(f"\nDone -> {os.path.abspath(args.out)}")


if __name__ == "__main__":
    sys.exit(main())
