"""Central configuration: paths, year ranges, and the four model feature specs.

The four model specifications mirror the original R script (legacy/Sciarrino_453project.R)
so the Python port can be validated against the published results.
"""

from __future__ import annotations

from pathlib import Path

# --- Paths -------------------------------------------------------------------
PROJECT_ROOT = Path(__file__).resolve().parents[2]
DATA_DIR = PROJECT_ROOT / "data"
RAW_DIR = DATA_DIR / "raw"
PROCESSED_DIR = DATA_DIR / "processed"
ARTIFACTS_DIR = PROJECT_ROOT / "artifacts"
LEGACY_XLSM = PROJECT_ROOT / "legacy" / "Sciarrino_453project.xlsm"

PROCESSED_TEAMS = PROCESSED_DIR / "teams.parquet"
RAW_TEAMS = RAW_DIR / "teams_raw.parquet"
LAHMAN_CACHE = RAW_DIR / "lahman_teams.csv"
RECENT_SEASONS_CSV = RAW_DIR / "recent_seasons.csv"
METRICS_JSON = ARTIFACTS_DIR / "metrics.json"

# --- Data sources ------------------------------------------------------------
# The canonical chadwickbureau/baseballdatabank repo was removed and pybaseball's
# FanGraphs/BR scrapers currently return 403/404, so we read the Lahman "Teams"
# table from a fallback chain of static CSV mirrors and cache it locally. This is
# the same dataset the original study used (Kaggle "Baseball Databank" == Lahman).
LAHMAN_MIRRORS = [
    "https://raw.githubusercontent.com/cbwinslow/baseballdatabank/master/core/Teams.csv",
    "https://raw.githubusercontent.com/orrski/baseballdatabank/master/core/Teams.csv",
]

# World Series champions for seasons beyond the Lahman mirror's coverage. Used to
# label `dWSWin` when recent-season stat lines are supplied via RECENT_SEASONS_CSV.
# (Postseason results are tiny and stable, so a hardcoded map is the robust choice.)
RECENT_WS_WINNERS = {
    2022: "HOU",  # Houston Astros
    2023: "TEX",  # Texas Rangers
    2024: "LAD",  # Los Angeles Dodgers
}

# COVID-shortened 60-game season: rate/plus stats are comparable, but raw counting
# totals (Model 1) are not. Kept by default; callers can drop it.
SHORT_SEASONS = {2020}

# Lahman -> project column renames (digit-leading names break statsmodels formulas;
# SOA is pitcher strikeouts, which the original models call "K").
COLUMN_RENAMES = {"2B": "x2B", "3B": "x3B", "SOA": "K"}

for _d in (RAW_DIR, PROCESSED_DIR, ARTIFACTS_DIR):
    _d.mkdir(parents=True, exist_ok=True)

# --- Year ranges -------------------------------------------------------------
# HBP and SF were not consistently tracked before 2000, which is why the
# original study starts there. We extend the end year to gather more World
# Series winners for the advanced models.
START_YEAR = 2000
# Faithful-replication window (matches the original 2000-2015 study).
REPLICATION_END_YEAR = 2015
# Full window used to train the advanced (XGBoost / NN) models.
FULL_END_YEAR = 2025

TARGET = "dWSWin"  # binary: 1 if the team won the World Series that season

# --- Feature specifications (one per original model) -------------------------
# Model 1 — raw counting statistics (20 predictors).
MODEL1_FEATURES = [
    "R", "x1B", "x2B", "x3B", "HR", "BB", "SO", "SB", "CS", "HBP", "SF",
    "RA", "CG", "SHO", "SV", "HA", "HRA", "BBA", "K", "E", "DP",
]
# Model 2 — calculated rate statistics (7 predictors).
MODEL2_FEATURES = ["BA", "OBP", "SLG", "ERA", "KPN", "WHIP", "FP"]
# Model 3 — Houser (2005) replication (8 predictors).
MODEL3_FEATURES = ["BA", "OBP", "SLG", "x3B", "SB", "KPN", "WHIP", "FP"]
# Model 4 — league-normalized "plus" statistics (3 predictors).
MODEL4_FEATURES = ["OPSP", "WHIPP", "FPP"]

MODEL_SPECS: dict[str, list[str]] = {
    "model1_raw": MODEL1_FEATURES,
    "model2_calc": MODEL2_FEATURES,
    "model3_houser": MODEL3_FEATURES,
    "model4_plus": MODEL4_FEATURES,
}

# Default feature set for the advanced models — the calculated rate stats plus
# the league-normalized plus stats give a compact, mostly-collinearity-free set.
ADVANCED_FEATURES = ["BA", "OBP", "SLG", "ERA", "KPN", "WHIP", "FP", "OPSP", "WHIPP", "FPP"]

# Every engineered column the pipeline must be able to produce.
ALL_FEATURES = sorted(
    set(MODEL1_FEATURES + MODEL2_FEATURES + MODEL3_FEATURES + MODEL4_FEATURES + ADVANCED_FEATURES)
)
