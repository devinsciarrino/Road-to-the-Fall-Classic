"""Train all models, evaluate them, and persist artifacts.

    python -m rttfc.train

Writes:
  * artifacts/metrics.json           — logistic replication + advanced comparison
  * artifacts/models/<name>.joblib   — fitted pipelines (full data) for serving
"""

from __future__ import annotations

import json

import joblib
import pandas as pd

from rttfc import config
from rttfc.data.features import build_dataset
from rttfc.models import evaluate as ev
from rttfc.models import logistic as lg

MODELS_DIR = config.ARTIFACTS_DIR / "models"


def _load_or_build() -> pd.DataFrame:
    if config.PROCESSED_TEAMS.exists():
        return pd.read_parquet(config.PROCESSED_TEAMS)
    return build_dataset()


def train(features: list[str] | None = None) -> dict:
    features = features or config.ADVANCED_FEATURES
    df = _load_or_build()
    labeled = df[df[config.TARGET].notna()].copy()

    # 1) Faithful logistic replication on 2000-2015 (matches the published study).
    rep = lg.replication_window(labeled)
    fits = lg.fit_all_logistic(rep)
    logistic_replication = {
        name: {
            "features": f.features,
            "n_obs": f.n_obs,
            "deviance": round(f.deviance, 4),
            "brier": round(f.brier, 6),
        }
        for name, f in fits.items()
    }
    # Per-variable coefficients + p-values for the dashboard's significance charts.
    logistic_coefficients = {
        name: [
            {
                "variable": var,
                "coef": round(float(row["coef"]), 5),
                "z": round(float(row["z"]), 4),
                "p_value": round(float(row["p_value"]), 5),
            }
            for var, row in f.summary_frame().iterrows()
        ]
        for name, f in fits.items()
    }

    # 2) Out-of-sample comparison of logistic / XGBoost / neural net on full data.
    comparison = ev.run_comparison(labeled, features)

    # 3) Fit production pipelines on all labeled data and persist them.
    MODELS_DIR.mkdir(parents=True, exist_ok=True)
    X, y = labeled[features], labeled[config.TARGET]
    saved = {}
    for name, make in ev.ESTIMATORS.items():
        model = make(y.to_numpy())
        model.fit(X, y)
        path = MODELS_DIR / f"{name}.joblib"
        joblib.dump({"model": model, "features": features}, path)
        saved[name] = str(path.relative_to(config.PROJECT_ROOT))

    metrics = {
        "dataset": {
            "start_year": int(labeled["yearID"].min()),
            "end_year": int(labeled["yearID"].max()),
            "n_team_seasons": int(len(labeled)),
            "n_ws_winners": int(labeled[config.TARGET].sum()),
            "replication_end_year": config.REPLICATION_END_YEAR,
        },
        "logistic_replication_2000_2015": logistic_replication,
        "logistic_coefficients": logistic_coefficients,
        "advanced_comparison": {
            "features": features,
            "random_baseline_champion_rank": round(ev.random_baseline_rank(labeled), 3),
            "models": {m: {k: round(v, 6) for k, v in row.items()}
                       for m, row in comparison.to_dict("index").items()},
        },
        "saved_models": saved,
    }
    config.METRICS_JSON.write_text(json.dumps(metrics, indent=2))

    _print_summary(metrics, comparison)
    return metrics


def _print_summary(metrics: dict, comparison: pd.DataFrame) -> None:
    d = metrics["dataset"]
    print(f"\n=== Dataset: {d['n_team_seasons']} team-seasons "
          f"{d['start_year']}-{d['end_year']}, {d['n_ws_winners']} WS winners ===")
    print("\n--- Logistic replication (2000-2015, in-sample) ---")
    for name, m in metrics["logistic_replication_2000_2015"].items():
        print(f"  {name:14} deviance={m['deviance']:>9}  brier={m['brier']:.5f}")
    print(f"\n--- Out-of-sample comparison (random champion rank "
          f"= {metrics['advanced_comparison']['random_baseline_champion_rank']}) ---")
    pd.set_option("display.width", 200, "display.max_columns", 20)
    print(comparison.round(4).to_string())
    print(f"\nWrote {config.METRICS_JSON.relative_to(config.PROJECT_ROOT)}")


if __name__ == "__main__":
    train()
