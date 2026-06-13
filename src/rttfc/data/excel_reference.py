"""Read the legacy Excel workbook — the validation baseline for the migration.

The workbook holds the hand-computed feature columns for 2000-2015. We join our
pandas-derived features against these sheets to prove the Python pipeline
reproduces the original data (see tests/test_features.py).
"""

from __future__ import annotations

import functools

import pandas as pd

from rttfc import config

# Sheets keyed by (yearID, franchID).
_RAW_SHEET = "2000 raw"
_CALC_SHEET = "2000 calc"
_PLUS_SHEET = "plus"


@functools.lru_cache(maxsize=None)
def _read(sheet: str) -> pd.DataFrame:
    if not config.LEGACY_XLSM.exists():
        raise FileNotFoundError(f"legacy workbook not found: {config.LEGACY_XLSM}")
    return pd.read_excel(config.LEGACY_XLSM, sheet_name=sheet)


def reference_raw() -> pd.DataFrame:
    """Raw sheet: x1B and the counting stats, keyed by (yearID, franchID)."""
    return _read(_RAW_SHEET)


def reference_calc() -> pd.DataFrame:
    """Calculated rate stats: BA, OBP, SLG, OPS, ERA, KPN, WHIP, FP."""
    return _read(_CALC_SHEET)


def reference_plus() -> pd.DataFrame:
    """League-normalized plus stats: OPSP, WHIPP, FPP."""
    df = _read(_PLUS_SHEET)
    return df.loc[:, ~df.columns.str.startswith("Unnamed")]


def reference_features() -> pd.DataFrame:
    """One tidy frame of all reference features keyed by (yearID, franchID)."""
    key = ["yearID", "franchID"]
    raw = reference_raw()[key + ["x1B"]]
    calc = reference_calc()[key + ["BA", "OBP", "SLG", "OPS", "ERA", "KPN", "WHIP", "FP"]]
    plus = reference_plus()[key + ["OPSP", "WHIPP", "FPP"]]
    return raw.merge(calc, on=key).merge(plus, on=key)
