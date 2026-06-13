"""FastAPI service for the Road to the Fall Classic dashboard.

Thin layer over ``rttfc.serve``. Run from the repo root:

    uvicorn api.main:app --reload --port 8000

Endpoints:
    GET /healthz                          liveness check
    GET /models                           available models + training metrics
    GET /seasons                          selectable seasons (2000..current)
    GET /standings?season=&model=         ranked WS-win probabilities for all teams
    GET /team/{team_id}?season=&model=     one team's entry + stat breakdown
"""

from __future__ import annotations

from fastapi import FastAPI, HTTPException, Query
from fastapi.middleware.cors import CORSMiddleware

from rttfc import config, serve

app = FastAPI(title="Road to the Fall Classic", version="1.0.0")

# Allow the Vite dev server (and common local hosts) to call the API.
app.add_middleware(
    CORSMiddleware,
    allow_origin_regex=r"http://(localhost|127\.0\.0\.1):\d+",
    allow_methods=["GET"],
    allow_headers=["*"],
)


def _validate_model(model: str) -> None:
    available = serve.available_models()
    if not available:
        raise HTTPException(503, "No trained models found — run `python -m rttfc.train`.")
    if model not in available:
        raise HTTPException(404, f"Unknown model '{model}'. Available: {available}")


@app.get("/healthz")
def healthz() -> dict:
    return {"status": "ok", "models": serve.available_models()}


@app.get("/models")
def models() -> dict:
    metrics = serve.model_metrics()
    return {
        "models": serve.available_models(),
        "default": "logistic",
        "comparison": metrics.get("advanced_comparison", {}),
        "dataset": metrics.get("dataset", {}),
    }


@app.get("/seasons")
def seasons() -> dict:
    current = serve.current_season()
    return {"current": current, "seasons": list(range(config.START_YEAR, current + 1))}


@app.get("/standings")
def standings(
    season: int = Query(default=None, description="defaults to the current season"),
    model: str = Query(default="logistic"),
) -> dict:
    _validate_model(model)
    season = season or serve.current_season()
    try:
        return serve.standings_payload(season, model)
    except Exception as err:  # MLB API unreachable, no data for that season, etc.
        raise HTTPException(502, f"Could not score season {season}: {err}")


@app.get("/team/{team_id}")
def team(
    team_id: str,
    season: int = Query(default=None),
    model: str = Query(default="logistic"),
) -> dict:
    _validate_model(model)
    season = season or serve.current_season()
    try:
        result = serve.team_payload(season, team_id, model)
    except Exception as err:
        raise HTTPException(502, f"Could not score season {season}: {err}")
    if result is None:
        raise HTTPException(404, f"Team '{team_id}' not found for season {season}.")
    return result
