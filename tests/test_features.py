"""Feature-engineering tests: formula unit tests + parity against the workbook.

The parity tests join the pandas-derived features against the hand-built Excel
sheets for 2000-2015. Tolerances reflect the workbook's rounding of intermediate
values (BA/OBP/SLG stored to 3 dp; ERA/KPN/WHIP to 2 dp; the plus stats propagate
that rounding through ratios on a ~100 scale).
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from rttfc import config
from rttfc.data import excel_reference as xl
from rttfc.data import features as ft


@pytest.fixture(scope="module")
def processed() -> pd.DataFrame:
    if not config.PROCESSED_TEAMS.exists():
        ft.build_dataset()
    return pd.read_parquet(config.PROCESSED_TEAMS)


# --- Unit tests on a hand-checked row (ANA 2000) -----------------------------
def _ana_2000() -> pd.DataFrame:
    # Raw counting stats for the 2000 Anaheim Angels (from the workbook 'raw' sheet).
    return pd.DataFrame([dict(
        yearID=2000, teamID="ANA", franchID="ANA", lgID="AL", name="Anaheim Angels",
        G=162, W=82, L=80, WSWin="N", dWSWin=0,
        R=864, AB=5628, H=1574, x2B=309, x3B=34, HR=236, BB=608, SO=1024, SB=93,
        CS=52, HBP=47, SF=43, RA=869, ER=805, CG=5, SHO=3, SV=46, IPouts=4344,
        HA=1534, HRA=228, BBA=662, K=846, E=134, DP=182, FP=0.978,
    )])


def test_singles_and_rate_formulas():
    row = ft.add_batting_rates(_ana_2000()).iloc[0]
    assert row["x1B"] == 1574 - 309 - 34 - 236  # 995
    assert row["BA"] == pytest.approx(1574 / 5628, abs=1e-9)
    assert row["OBP"] == pytest.approx((1574 + 608 + 47) / (5628 + 608 + 47 + 43), abs=1e-9)
    tb = 995 + 2 * 309 + 3 * 34 + 4 * 236
    assert row["SLG"] == pytest.approx(tb / 5628, abs=1e-9)
    assert row["OPS"] == pytest.approx(row["OBP"] + row["SLG"], abs=1e-12)


def test_pitching_formulas():
    df = ft.add_pitching_rates(_ana_2000())
    row = df.iloc[0]
    innings = 4344 / 3
    assert row["innings"] == pytest.approx(innings)
    assert row["ERA"] == pytest.approx(9 * 805 / innings, abs=1e-9)
    assert row["KPN"] == pytest.approx(9 * 846 / innings, abs=1e-9)
    assert row["WHIP"] == pytest.approx((662 + 1534) / innings, abs=1e-9)


def test_plus_stats_definition():
    # Two teams in one season so the league mean is well-defined.
    base = _ana_2000()
    other = base.copy()
    other.loc[0, ["teamID", "franchID", "OBP"]] = ["TEX", "TEX", 0.300]
    df = ft.add_batting_rates(pd.concat([base, other], ignore_index=True))
    df = ft.add_pitching_rates(df)
    df = ft.add_fielding(df)
    out = ft.add_plus_stats(df)
    lg_obp = out["OBP"].mean()
    lg_slg = out["SLG"].mean()
    expected_opsp = 100 * (out["OBP"].iloc[0] / lg_obp + out["SLG"].iloc[0] / lg_slg - 1)
    assert out["OPSP"].iloc[0] == pytest.approx(expected_opsp, abs=1e-9)


# --- Parity against the Excel workbook (2000-2015) ---------------------------
TOLERANCES = {
    "x1B": 0.0, "FP": 1e-3, "FPP": 1e-3,
    "BA": 1e-3, "OBP": 1e-3, "SLG": 1e-3,
    "ERA": 1e-2, "KPN": 1e-2, "WHIP": 1e-2,
    "OPSP": 0.6, "WHIPP": 0.6,
}


@pytest.fixture(scope="module")
def merged(processed) -> pd.DataFrame:
    ref = xl.reference_features().rename(
        columns=lambda c: c if c in ("yearID", "franchID") else f"{c}_ref"
    )
    m = processed.merge(ref, on=["yearID", "franchID"])
    assert len(m) == 480, f"expected 480 joined rows for 2000-2015, got {len(m)}"
    return m


@pytest.mark.parametrize("col", list(TOLERANCES))
def test_parity_with_workbook(merged, col):
    diff = (merged[col] - merged[f"{col}_ref"]).abs().max()
    assert diff <= TOLERANCES[col], f"{col}: max abs diff {diff} exceeds {TOLERANCES[col]}"
