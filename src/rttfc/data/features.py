"""Feature engineering — reproduces the hand-built Excel columns in pandas.

Every formula here was reverse-engineered from and validated against the legacy
workbook (``legacy/Sciarrino_453project.xlsm``); see ``tests/test_features.py``
for the parity checks against 2000-2015.

    python -m rttfc.data.features        # build data/processed/teams.parquet
"""

from __future__ import annotations

import pandas as pd

from rttfc import config
from rttfc.data.collect import collect

# Columns carried through for identification / context (not model features).
ID_COLS = ["yearID", "teamID", "franchID", "lgID", "name", "mlb_id", "G", "W", "L", "WSWin", config.TARGET]


def add_batting_rates(df: pd.DataFrame) -> pd.DataFrame:
    """Singles and the classic batting rate stats."""
    df = df.copy()
    df["x1B"] = df["H"] - df["x2B"] - df["x3B"] - df["HR"]
    df["BA"] = df["H"] / df["AB"]
    df["OBP"] = (df["H"] + df["BB"] + df["HBP"]) / (df["AB"] + df["BB"] + df["HBP"] + df["SF"])
    total_bases = df["x1B"] + 2 * df["x2B"] + 3 * df["x3B"] + 4 * df["HR"]
    df["SLG"] = total_bases / df["AB"]
    df["OPS"] = df["OBP"] + df["SLG"]
    return df


def add_pitching_rates(df: pd.DataFrame) -> pd.DataFrame:
    """Innings-normalized pitching stats. ERA/KPN/WHIP per the Excel definitions."""
    df = df.copy()
    df["innings"] = df["IPouts"] / 3.0
    df["ERA"] = 9.0 * df["ER"] / df["innings"]   # recompute to match the workbook exactly
    df["KPN"] = 9.0 * df["K"] / df["innings"]     # K == SOA (pitcher strikeouts)
    df["WHIP"] = (df["BBA"] + df["HA"]) / df["innings"]
    df["BB9"] = 9.0 * df["BBA"] / df["innings"]
    df["HR9"] = 9.0 * df["HRA"] / df["innings"]
    return df


def add_fielding(df: pd.DataFrame) -> pd.DataFrame:
    """Fielding rate stats. FP is a Lahman passthrough (no PO/A in the source)."""
    df = df.copy()
    df["FP"] = pd.to_numeric(df["FP"], errors="coerce")
    df["E9"] = 9.0 * df["E"] / df["innings"]
    df["DP9"] = 9.0 * df["DP"] / df["innings"]
    return df


def add_plus_stats(df: pd.DataFrame) -> pd.DataFrame:
    """League-normalized "plus" stats.

    The league average for a season is the simple mean of that season's team
    values (verified against the workbook). Definitions:
        OPSP  = 100 * (OBP/lgOBP + SLG/lgSLG - 1)
        WHIPP = 100 * WHIP / lgWHIP
        FPP   = 100 * (FP/lgFP - 1)
    """
    df = df.copy()
    grp = df.groupby("yearID")
    lg_obp = grp["OBP"].transform("mean")
    lg_slg = grp["SLG"].transform("mean")
    lg_whip = grp["WHIP"].transform("mean")
    lg_fp = grp["FP"].transform("mean")

    df["OPSP"] = 100.0 * (df["OBP"] / lg_obp + df["SLG"] / lg_slg - 1.0)
    df["WHIPP"] = 100.0 * df["WHIP"] / lg_whip
    df["FPP"] = 100.0 * (df["FP"] / lg_fp - 1.0)
    return df


def build_features(df: pd.DataFrame) -> pd.DataFrame:
    """Run the full feature pipeline on a normalized raw team table."""
    df = add_batting_rates(df)
    df = add_pitching_rates(df)
    df = add_fielding(df)
    df = add_plus_stats(df)

    keep = ID_COLS + [c for c in config.ALL_FEATURES if c not in ID_COLS]
    keep = [c for c in dict.fromkeys(keep) if c in df.columns]
    missing = set(config.ALL_FEATURES) - set(df.columns)
    if missing:
        raise ValueError(f"feature pipeline did not produce: {sorted(missing)}")
    return df[keep].copy()


def build_dataset(force_collect: bool = False) -> pd.DataFrame:
    """Collect raw data (if needed) -> engineer features -> persist processed parquet."""
    if config.RAW_TEAMS.exists() and not force_collect:
        raw = pd.read_parquet(config.RAW_TEAMS)
    else:
        raw = collect(force=force_collect)

    feats = build_features(raw)
    feats.to_parquet(config.PROCESSED_TEAMS, index=False)
    print(
        f"[features] wrote {config.PROCESSED_TEAMS.relative_to(config.PROJECT_ROOT)} "
        f"({len(feats)} rows, {len(config.ALL_FEATURES)} engineered features)"
    )
    return feats


if __name__ == "__main__":
    build_dataset()
