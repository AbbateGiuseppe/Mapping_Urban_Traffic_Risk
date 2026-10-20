#!/usr/bin/env python3
"""
MCP server for the "Mapping Urban Traffic Risk" showcase database.

Exposes the project's aggregated results (GLMM predictions, model
comparison, functional-PCA / clustering summaries, methodology
reference) as queryable tools, so Claude (or any MCP client) can
answer questions about the Rome road-accident analysis project
directly from this repo.

Run standalone for a quick check:
    python3 server.py --db ../database/mapping_urban_risk.db

Connect it to Claude Desktop/Code: see the README in this folder.
"""
import argparse
import os
import sqlite3
import sys

try:
    # mcp >= 2.0: FastMCP was renamed to MCPServer, same decorator-based API.
    from mcp.server.mcpserver import MCPServer as _MCPServerClass
except ModuleNotFoundError:
    # mcp < 2.0: original class name.
    from mcp.server.fastmcp import FastMCP as _MCPServerClass

def _default_db_path():
    """Resolve the database path with a couple of fallbacks, so the
    server works both when run in place from a git clone (repo-root
    layout: ../database/mapping_urban_risk.db) and when installed as a
    standalone desktop extension, which copies only this mcp_server/
    folder -- in that case a bundled copy of the database sitting right
    next to this script is used instead.
    """
    here = os.path.dirname(os.path.abspath(__file__))
    candidates = [
        os.path.join(here, "mapping_urban_risk.db"),                     # bundled copy (extension installs)
        os.path.join(here, "..", "database", "mapping_urban_risk.db"),   # git-clone layout
    ]
    for candidate in candidates:
        if os.path.exists(candidate):
            return candidate
    return candidates[0]


DEFAULT_DB_PATH = _default_db_path()
DB_PATH = os.environ.get("MAPPING_URBAN_RISK_DB", DEFAULT_DB_PATH)

mcp = _MCPServerClass("mapping-urban-traffic-risk")


def _connect():
    if not os.path.exists(DB_PATH):
        raise FileNotFoundError(
            f"Database not found at {DB_PATH}. Build it first with "
            f"database/build_database.py (see the repo README)."
        )
    conn = sqlite3.connect(DB_PATH)
    conn.row_factory = sqlite3.Row
    return conn


def _rows_to_dicts(rows):
    return [dict(r) for r in rows]


@mcp.tool()
def list_tables() -> list[dict]:
    """List every table in the database with its row count and a short
    description of what it holds. Call this first to see what's available."""
    descriptions = {
        "neighborhoods": "All 155 official Rome urban zones; 3 are flagged is_study_area=1 (the project's focus area).",
        "roads": "Road segments inside the 3 study neighborhoods, with type, length and centroid coordinates.",
        "model_registry": "Every GLMM variant fitted for accident frequency, with formula, family, AIC/BIC (model comparison).",
        "model_coefficients": "Fixed-effect coefficients (estimate, std. error, z, p-value) for each model in model_registry.",
        "glmm_predictions": "Core results table: predicted expected accidents/day per road x month x day-period x day-type x weather.",
        "fda_basis_coefficients": "B-spline basis coefficients of the smoothed accident-rate curve (hour/day/month) per neighborhood.",
        "fpca_scores": "Functional-PCA scores (PC1-3) per neighborhood and profile type -- input to the clustering step.",
        "fpca_variance_explained": "Variance explained by each functional principal component.",
        "cv_zinb_metrics": "Cross-validation summary metrics for the frequency model (may be empty -- see techniques table).",
        "cv_zinb_by_fold": "Per-fold cross-validation results for the frequency model (may be empty).",
        "cv_severity_metrics": "Cross-validation summary metrics for the severity model (may be empty).",
        "cv_severity_by_fold": "Per-fold cross-validation results for the severity model (may be empty).",
        "fda_cluster_assignments": "k-means cluster assignment per neighborhood from the FDA profiles (may be empty).",
        "kriging_corrected_predictions": "Regression-kriging-corrected predictions per road (may be empty).",
        "severity_model_coefficients": "Logistic GLMM coefficients for accident severity (may be empty).",
        "spatial_autocorrelation_tests": "Moran's I / Getis-Ord Gi* test results (may be empty).",
        "techniques": "Curated reference of every methodology/technique used in the project, with which table (if any) holds its numeric output.",
    }
    conn = _connect()
    try:
        cur = conn.execute("SELECT name FROM sqlite_master WHERE type='table' ORDER BY name")
        out = []
        for (name,) in cur.fetchall():
            count = conn.execute(f"SELECT COUNT(*) FROM {name}").fetchone()[0]
            out.append({
                "table": name,
                "rows": count,
                "description": descriptions.get(name, ""),
            })
        return out
    finally:
        conn.close()


@mcp.tool()
def get_schema(table_name: str) -> list[dict]:
    """Return the column definitions (name, type, nullability, default,
    whether part of the primary key) for a given table."""
    conn = _connect()
    try:
        cur = conn.execute(f"PRAGMA table_info({table_name})")
        cols = [dict(r) for r in cur.fetchall()]
        if not cols:
            raise ValueError(f"Unknown table: {table_name!r}. Call list_tables() to see valid names.")
        return cols
    finally:
        conn.close()


@mcp.tool()
def run_query(sql: str, limit: int = 200) -> list[dict]:
    """Run a read-only SQL SELECT query against the database and return
    the rows as a list of dicts. Only SELECT statements are allowed (no
    INSERT/UPDATE/DELETE/DROP/ATTACH etc.). If the query has no LIMIT
    clause, one is added automatically using the `limit` argument.

    Use list_tables() and get_schema() first to see what's available.
    """
    cleaned = sql.strip().rstrip(";")
    lowered = cleaned.lower()
    if not lowered.startswith("select") and not lowered.startswith("with"):
        raise ValueError("Only read-only SELECT (or WITH ... SELECT) queries are allowed.")
    forbidden = ["insert", "update", "delete", "drop", "alter", "attach", "pragma", "create", "replace"]
    if any(f" {kw} " in f" {lowered} " or lowered.startswith(kw) for kw in forbidden):
        raise ValueError("Only read-only SELECT queries are allowed.")
    if "limit" not in lowered:
        cleaned = f"{cleaned} LIMIT {int(limit)}"
    conn = _connect()
    try:
        cur = conn.execute(cleaned)
        return _rows_to_dicts(cur.fetchall())
    finally:
        conn.close()


@mcp.tool()
def top_risk_roads(
    neighborhood: str,
    month: str | None = None,
    day_period: str | None = None,
    day_type: str | None = None,
    weather: str | None = None,
    limit: int = 10,
) -> list[dict]:
    """Return the road segments with the highest GLMM-predicted expected
    accident rate in a given neighborhood, optionally filtered by month
    (3-letter English abbreviation e.g. 'Jun'), day_period
    (Morning/Afternoon/Evening/Night), day_type (weekday/weekend) and
    weather (Dry/Rain). Any filter left as None is not applied (results
    are averaged across that dimension... actually all matching rows
    are returned, so omitting filters widens rather than aggregates --
    pass all 4 filters for a single specific scenario).
    """
    conn = _connect()
    try:
        clauses = ["neighborhood = ?"]
        params = [neighborhood]
        for col, val in [("month", month), ("day_period", day_period), ("day_type", day_type), ("weather", weather)]:
            if val is not None:
                clauses.append(f"{col} = ?")
                params.append(val)
        where = " AND ".join(clauses)
        sql = (
            "SELECT road_id, highway_type, month, day_period, day_type, weather, "
            "expected_accidents_per_day FROM glmm_predictions "
            f"WHERE {where} ORDER BY expected_accidents_per_day DESC LIMIT ?"
        )
        params.append(int(limit))
        cur = conn.execute(sql, params)
        return _rows_to_dicts(cur.fetchall())
    finally:
        conn.close()


@mcp.tool()
def compare_models() -> list[dict]:
    """Return the full model-comparison table (every GLMM variant fitted
    for accident frequency, with its formula, family, AIC and BIC),
    sorted by AIC ascending (best-fitting first)."""
    conn = _connect()
    try:
        cur = conn.execute(
            "SELECT model_name, family, formula, aic, bic, n_obs, description "
            "FROM model_registry ORDER BY aic ASC"
        )
        return _rows_to_dicts(cur.fetchall())
    finally:
        conn.close()


@mcp.tool()
def list_techniques(category: str | None = None) -> list[dict]:
    """List the statistical/ML techniques used in the project (optionally
    filtered by category: preprocessing, eda, modeling, clustering,
    spatial, validation, visualization), with a description, which
    script implements it, and which database table (if any) holds its
    numeric results."""
    conn = _connect()
    try:
        if category:
            cur = conn.execute(
                "SELECT * FROM techniques WHERE category = ? ORDER BY technique_name", (category,)
            )
        else:
            cur = conn.execute("SELECT * FROM techniques ORDER BY category, technique_name")
        return _rows_to_dicts(cur.fetchall())
    finally:
        conn.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--db", default=None, help="Path to mapping_urban_risk.db")
    args, _ = parser.parse_known_args()
    if args.db:
        DB_PATH = args.db
    if not os.path.exists(DB_PATH):
        print(f"WARNING: database not found at {DB_PATH}", file=sys.stderr)
    mcp.run()
