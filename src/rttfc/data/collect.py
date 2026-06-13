"""Automated data collection for team-season statistics.

Primary source is the Lahman "Teams" table (the dataset the original study used,
via Kaggle's "Baseball Databank"). The canonical chadwickbureau repo was removed
and pybaseball's live FanGraphs/Baseball-Reference scrapers currently return
403/404, so we pull a static CSV from a fallback chain of mirrors and cache it
locally for reproducible, offline reruns.

Seasons newer than the mirror (2022+) can be supplied via a drop-in CSV
(``data/raw/recent_seasons.csv``) using the same Lahman column names; their World
Series winner is labelled from ``config.RECENT_WS_WINNERS``.

Run as a module:

    python -m rttfc.data.collect            # build data/raw/teams_raw.parquet
    python -m rttfc.data.collect --force    # re-download the Lahman mirror
"""

from __future__ import annotations

import argparse
import io
import sys

import pandas as pd
import requests

from rttfc import config


def download_lahman_teams(force: bool = False) -> pd.DataFrame:
    """Return the raw Lahman Teams table, downloading + caching on first use."""
    if config.LAHMAN_CACHE.exists() and not force:
        return pd.read_csv(config.LAHMAN_CACHE)

    last_err: Exception | None = None
    for url in config.LAHMAN_MIRRORS:
        try:
            resp = requests.get(url, timeout=60)
            resp.raise_for_status()
            df = pd.read_csv(io.StringIO(resp.text))
            if "WSWin" not in df.columns:
                raise ValueError(f"{url}: no WSWin column")
            config.LAHMAN_CACHE.parent.mkdir(parents=True, exist_ok=True)
            df.to_csv(config.LAHMAN_CACHE, index=False)
            print(f"[collect] downloaded Lahman Teams from {url} ({len(df)} rows)")
            return df
        except Exception as err:  # try the next mirror
            last_err = err
            print(f"[collect] mirror failed ({url}): {err}", file=sys.stderr)

    raise RuntimeError(
        "Could not download the Lahman Teams table from any mirror. "
        f"Last error: {last_err}"
    )


def load_recent_seasons() -> pd.DataFrame | None:
    """Load optional recent-season stat lines (same Lahman schema) if present."""
    if not config.RECENT_SEASONS_CSV.exists():
        return None
    df = pd.read_csv(config.RECENT_SEASONS_CSV)
    # Label the World Series winner for each supplied season.
    df["WSWin"] = "N"
    for year, team in config.RECENT_WS_WINNERS.items():
        df.loc[(df["yearID"] == year) & (df["teamID"] == team), "WSWin"] = "Y"
    print(f"[collect] merged {len(df)} recent-season rows from {config.RECENT_SEASONS_CSV.name}")
    return df


def _normalize(df: pd.DataFrame) -> pd.DataFrame:
    """Filter to the study window, rename columns, and build the binary target."""
    df = df[(df["yearID"] >= config.START_YEAR) & (df["yearID"] <= config.FULL_END_YEAR)].copy()
    df = df.rename(columns=config.COLUMN_RENAMES)
    # WSWin is 'Y'/'N'/blank in Lahman; map to a 0/1 target. Seasons with no World
    # Series played (e.g. a future in-progress year) stay 0 but should be filtered
    # by the caller when training.
    df[config.TARGET] = (df["WSWin"].astype("string").str.upper() == "Y").astype(int)
    df = df.sort_values(["yearID", "teamID"]).reset_index(drop=True)
    return df


def collect(force: bool = False) -> pd.DataFrame:
    """Build and persist the raw team-season table; return it."""
    lahman = download_lahman_teams(force=force)
    recent = load_recent_seasons()
    raw = lahman if recent is None else pd.concat([lahman, recent], ignore_index=True)

    df = _normalize(raw)
    df.to_parquet(config.RAW_TEAMS, index=False)

    n_win = int(df[config.TARGET].sum())
    print(
        f"[collect] wrote {config.RAW_TEAMS.relative_to(config.PROJECT_ROOT)} "
        f"({len(df)} team-seasons, {df['yearID'].min()}-{df['yearID'].max()}, "
        f"{n_win} World Series winners)"
    )
    return df


def main() -> None:
    parser = argparse.ArgumentParser(description="Collect team-season data (Lahman + recent).")
    parser.add_argument("--force", action="store_true", help="re-download the Lahman mirror")
    args = parser.parse_args()
    collect(force=args.force)


if __name__ == "__main__":
    main()
