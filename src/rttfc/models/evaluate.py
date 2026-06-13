"""Out-of-sample evaluation comparing logistic / XGBoost / neural-net.

Because exactly one team wins the World Series each season, raw classification
accuracy is meaningless. We report:

  * probabilistic metrics on out-of-fold predictions — Brier, log-loss, ROC-AUC
    and PR-AUC (PR-AUC is the most informative under heavy imbalance);
  * a **champion-ranking** metric via leave-one-season-out: for each held-out
    season we rank its teams by predicted win probability and record where the
    actual champion landed (top-1 / top-3 hit rate, mean rank). Random baseline
    mean rank ~= 15.5 for a 30-team league.

Leave-one-season-out is the honest scheme here: it never lets a team from the
test season leak into training (which matters for the league-normalized stats).
"""

from __future__ import annotations

from typing import Callable

import numpy as np
import pandas as pd
from sklearn.metrics import average_precision_score, brier_score_loss, log_loss, roc_auc_score
from sklearn.model_selection import StratifiedKFold

from rttfc import config
from rttfc.models.logistic import make_logistic_pipeline
from rttfc.models.nn import make_mlp
from rttfc.models.xgb import make_xgb

# Each entry builds a fresh, unfitted estimator. XGB needs y to set class weights.
ESTIMATORS: dict[str, Callable] = {
    "logistic": lambda y=None: make_logistic_pipeline(),
    "xgboost": lambda y=None: make_xgb(y),
    "neural_net": lambda y=None: make_mlp(),
}


def _proba(model, X) -> np.ndarray:
    return model.predict_proba(X)[:, 1]


def oof_probabilities(make, X: pd.DataFrame, y: pd.Series, n_splits: int = 5) -> np.ndarray:
    """Stratified k-fold out-of-fold positive-class probabilities."""
    oof = np.zeros(len(y), dtype=float)
    skf = StratifiedKFold(n_splits=n_splits, shuffle=True, random_state=42)
    yv = y.to_numpy()
    for tr, te in skf.split(X, yv):
        model = make(yv[tr])
        model.fit(X.iloc[tr], yv[tr])
        oof[te] = _proba(model, X.iloc[te])
    return oof


def probabilistic_metrics(y: np.ndarray, p: np.ndarray) -> dict[str, float]:
    y = np.asarray(y)
    p = np.clip(np.asarray(p), 1e-9, 1 - 1e-9)
    return {
        "brier": float(brier_score_loss(y, p)),
        "log_loss": float(log_loss(y, p, labels=[0, 1])),
        "roc_auc": float(roc_auc_score(y, p)),
        "pr_auc": float(average_precision_score(y, p)),
    }


def champion_ranking_loso(make, df: pd.DataFrame, features: list[str]) -> dict[str, float]:
    """Leave-one-season-out champion ranking.

    For each season: train on all other seasons, rank that season's teams by
    predicted probability, and find the rank (1 = highest) of the true champion.
    """
    ranks: list[int] = []
    n_teams: list[int] = []
    seasons = sorted(df["yearID"].unique())
    for season in seasons:
        test = df[df["yearID"] == season]
        if int(test[config.TARGET].sum()) != 1:
            continue  # skip seasons without exactly one recorded champion
        train = df[df["yearID"] != season]
        model = make(train[config.TARGET].to_numpy())
        model.fit(train[features], train[config.TARGET])
        p = _proba(model, test[features])
        order = test.assign(_p=p).sort_values("_p", ascending=False).reset_index(drop=True)
        champ_rank = int(order.index[order[config.TARGET] == 1][0]) + 1
        ranks.append(champ_rank)
        n_teams.append(len(test))
    ranks_arr = np.array(ranks)
    return {
        "seasons_evaluated": len(ranks),
        "mean_champion_rank": float(ranks_arr.mean()),
        "median_champion_rank": float(np.median(ranks_arr)),
        "top1_hit_rate": float((ranks_arr == 1).mean()),
        "top3_hit_rate": float((ranks_arr <= 3).mean()),
        "avg_teams_per_season": float(np.mean(n_teams)),
    }


def run_comparison(df: pd.DataFrame, features: list[str] | None = None) -> pd.DataFrame:
    """Evaluate every estimator and return a tidy results table."""
    features = features or config.ADVANCED_FEATURES
    data = df.dropna(subset=features + [config.TARGET]).copy()
    X, y = data[features], data[config.TARGET]

    rows = []
    for name, make in ESTIMATORS.items():
        oof = oof_probabilities(make, X, y)
        metrics = probabilistic_metrics(y.to_numpy(), oof)
        ranking = champion_ranking_loso(make, data, features)
        rows.append({"model": name, **metrics, **ranking})
    return pd.DataFrame(rows).set_index("model")


def random_baseline_rank(df: pd.DataFrame) -> float:
    """Expected champion rank under random ordering (for context)."""
    sizes = df.groupby("yearID").size()
    return float(((sizes + 1) / 2).mean())
