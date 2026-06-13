"""Serving layer: score a (possibly in-progress) season for the dashboard.

The saved models use rate / league-normalized features (``ADVANCED_FEATURES``),
which are scale-invariant — so scoring a partially-played current season is
sound. Results are cached in-memory with a short TTL so the API doesn't hammer
the MLB Stats API on every request.

Alongside each team's raw model probability we report a **win share**: the
probability normalized to sum to 1 across the league (exactly one team wins the
World Series). The share is interpretable regardless of a model's calibration and
makes the three models directly comparable.
"""

from __future__ import annotations

import json
import time
from datetime import datetime

import joblib
import pandas as pd

from rttfc import config
from rttfc.data.features import build_features
from rttfc.data.mlb_statsapi import fetch_season

MODELS_DIR = config.ARTIFACTS_DIR / "models"
CACHE_TTL_SECONDS = 1800  # 30 minutes

# Stats surfaced in the per-team breakdown.
DISPLAY_STATS = ["BA", "OBP", "SLG", "ERA", "KPN", "WHIP", "FP", "OPSP", "WHIPP", "FPP"]

# Official MLB team logos, keyed by the MLB team id carried through from the Stats API.
LOGO_URL = "https://www.mlbstatic.com/team-logos/{id}.svg"

_cache: dict[tuple[int, str], tuple[float, pd.DataFrame]] = {}


def _logo(mlb_id) -> str | None:
    return None if pd.isna(mlb_id) else LOGO_URL.format(id=int(mlb_id))


def current_season() -> int:
    return datetime.now().year


def available_models() -> list[str]:
    return sorted(p.stem for p in MODELS_DIR.glob("*.joblib"))


def _load(model_name: str) -> dict:
    path = MODELS_DIR / f"{model_name}.joblib"
    if not path.exists():
        raise FileNotFoundError(
            f"model '{model_name}' not found at {path} — run `python -m rttfc.train` first."
        )
    return joblib.load(path)


def model_metrics() -> dict:
    """The training/evaluation metrics written by rttfc.train."""
    if not config.METRICS_JSON.exists():
        return {}
    return json.loads(config.METRICS_JSON.read_text())


def score_season(year: int, model_name: str = "logistic", use_cache: bool = True) -> pd.DataFrame:
    """Return all teams for a season ranked by World Series win probability."""
    key = (year, model_name)
    if use_cache and key in _cache:
        ts, cached = _cache[key]
        if time.monotonic() - ts < CACHE_TTL_SECONDS:
            return cached.copy()

    bundle = _load(model_name)
    raw = fetch_season(year).rename(columns=config.COLUMN_RENAMES)
    feats = build_features(raw)

    proba = bundle["model"].predict_proba(feats[bundle["features"]])[:, 1]
    feats = feats.assign(ws_probability=proba)
    total = feats["ws_probability"].sum()
    feats["win_share"] = feats["ws_probability"] / total if total > 0 else 0.0

    feats = feats.sort_values("win_share", ascending=False).reset_index(drop=True)
    feats.insert(0, "rank", feats.index + 1)

    _cache[key] = (time.monotonic(), feats)
    return feats.copy()


def standings_payload(year: int, model_name: str = "logistic") -> dict:
    """JSON-ready standings for the API/dashboard."""
    df = score_season(year, model_name)
    teams = [
        {
            "rank": int(r["rank"]),
            "teamID": r["teamID"],
            "name": r["name"],
            "lgID": r.get("lgID", ""),
            "mlb_id": None if pd.isna(r.get("mlb_id")) else int(r["mlb_id"]),
            "logo": _logo(r.get("mlb_id")),
            "wins": None if pd.isna(r.get("W")) else int(r["W"]),
            "losses": None if pd.isna(r.get("L")) else int(r["L"]),
            "games": None if pd.isna(r.get("G")) else int(r["G"]),
            "ws_probability": round(float(r["ws_probability"]), 4),
            "win_share": round(float(r["win_share"]), 4),
            "stats": {s: (None if pd.isna(r.get(s)) else round(float(r[s]), 3)) for s in DISPLAY_STATS},
        }
        for _, r in df.iterrows()
    ]
    return {"season": year, "model": model_name, "teams": teams}


def team_payload(year: int, team_id: str, model_name: str = "logistic") -> dict | None:
    """One team's entry from the standings (by teamID, case-insensitive)."""
    payload = standings_payload(year, model_name)
    for t in payload["teams"]:
        if t["teamID"].upper() == team_id.upper():
            return {"season": year, "model": model_name, **t}
    return None
