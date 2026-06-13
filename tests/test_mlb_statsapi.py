"""Tests for the MLB Stats API collector.

Logic tests are offline (synthetic payloads). One network smoke test hits the
live API and is skipped automatically when there's no connectivity, so the suite
stays green offline / in CI.
"""

from __future__ import annotations

import pandas as pd
import pytest
import requests

from rttfc import config
from rttfc.data import mlb_statsapi as mlb


def test_field_maps_cover_required_columns():
    """The three group maps must together produce every raw column the
    feature pipeline needs (Lahman names, before the 2B/3B/SOA rename)."""
    produced = set(mlb.HITTING_MAP.values()) | set(mlb.PITCHING_MAP.values()) | set(mlb.FIELDING_MAP.values())
    produced |= {"G"}  # added explicitly from hitting gamesPlayed
    required = {"R", "AB", "H", "2B", "3B", "HR", "BB", "SO", "SB", "CS", "HBP", "SF",
                "RA", "ER", "CG", "SHO", "SV", "HA", "HRA", "BBA", "SOA", "IPouts",
                "E", "DP", "FP"}
    assert required <= produced


def test_tally_series_winner_counts_finals():
    payload = {"dates": [
        {"games": [
            {"status": {"abstractGameState": "Final"},
             "teams": {"home": {"isWinner": True, "team": {"id": 111}},
                       "away": {"isWinner": False, "team": {"id": 222}}}},
            {"status": {"abstractGameState": "Final"},
             "teams": {"home": {"isWinner": False, "team": {"id": 111}},
                       "away": {"isWinner": True, "team": {"id": 222}}}},
            {"status": {"abstractGameState": "Final"},
             "teams": {"home": {"isWinner": True, "team": {"id": 111}},
                       "away": {"isWinner": False, "team": {"id": 222}}}},
        ]},
    ]}
    assert mlb.tally_series_winner(payload) == 111


def test_tally_series_winner_ignores_non_final_and_handles_empty():
    assert mlb.tally_series_winner({"dates": []}) is None
    in_progress = {"dates": [{"games": [
        {"status": {"abstractGameState": "Live"},
         "teams": {"home": {"isWinner": False, "team": {"id": 1}},
                   "away": {"isWinner": False, "team": {"id": 2}}}}]}]}
    assert mlb.tally_series_winner(in_progress) is None


def _online() -> bool:
    try:
        requests.get(f"{mlb.BASE}/teams?sportId=1&season=2024", timeout=10).raise_for_status()
        return True
    except Exception:
        return False


@pytest.mark.skipif(not _online(), reason="MLB Stats API not reachable")
def test_fetch_season_live_smoke():
    df = mlb.fetch_season(2024)
    assert len(df) == 30
    # All raw columns the pipeline needs are present and non-null.
    for col in ["R", "AB", "H", "2B", "3B", "HR", "IPouts", "BBA", "HA", "E", "DP", "FP"]:
        assert col in df.columns and df[col].notna().all()
    # 2024 World Series winner is the Dodgers.
    champs = df.loc[df["WSWin"] == "Y", "name"].tolist()
    assert champs == ["Los Angeles Dodgers"]
