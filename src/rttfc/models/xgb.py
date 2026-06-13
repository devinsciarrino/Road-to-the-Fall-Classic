"""XGBoost classifier factory.

Kept deliberately shallow and well-regularized: the target is extremely
imbalanced (~22 World Series winners across ~660 team-seasons), so an
unconstrained booster would memorize the training set. ``scale_pos_weight``
up-weights the rare positive class.
"""

from __future__ import annotations

import numpy as np
from xgboost import XGBClassifier


def pos_weight(y) -> float:
    """neg/pos ratio used for scale_pos_weight."""
    y = np.asarray(y)
    pos = max(int((y == 1).sum()), 1)
    neg = int((y == 0).sum())
    return neg / pos


def make_xgb(y_train=None) -> XGBClassifier:
    """Return a fresh, regularized XGBClassifier. Pass training y to balance classes."""
    spw = pos_weight(y_train) if y_train is not None else 1.0
    return XGBClassifier(
        n_estimators=200,
        max_depth=3,
        learning_rate=0.03,
        subsample=0.8,
        colsample_bytree=0.8,
        reg_lambda=2.0,
        min_child_weight=2,
        scale_pos_weight=spw,
        eval_metric="logloss",
        random_state=42,
        n_jobs=2,
    )
