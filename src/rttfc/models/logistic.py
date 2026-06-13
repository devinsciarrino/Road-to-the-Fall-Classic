"""Faithful R -> Python port of the four logistic regressions.

The original study fit four ``glm(dWSWin ~ ..., family = binomial)`` models in R and
reported their deviance and Brier (in-sample MSE) on 2000-2015. We reproduce that
here with statsmodels GLM so the migration can be validated against the published
numbers, and so the inferential output (coefficients, p-values) is preserved.

For out-of-sample predictive comparison against XGBoost / the neural net, see
``rttfc.models.evaluate``.
"""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd
import statsmodels.api as sm
import statsmodels.formula.api as smf
from sklearn.linear_model import LogisticRegression
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler

from rttfc import config


def make_logistic_pipeline() -> Pipeline:
    """Scaled, class-balanced logistic regression for the predictive comparison."""
    return Pipeline(
        steps=[
            ("scale", StandardScaler()),
            ("clf", LogisticRegression(class_weight="balanced", C=1.0, max_iter=2000)),
        ]
    )


def brier_score(y_true: np.ndarray, p_pred: np.ndarray) -> float:
    """Mean squared error between predicted probabilities and outcomes."""
    y_true = np.asarray(y_true, dtype=float)
    p_pred = np.asarray(p_pred, dtype=float)
    return float(np.mean((p_pred - y_true) ** 2))


@dataclass
class LogisticFit:
    name: str
    features: list[str]
    result: sm.regression.linear_model.RegressionResultsWrapper
    deviance: float
    brier: float
    n_obs: int

    def summary_frame(self) -> pd.DataFrame:
        """Coefficients, std errors, z and p-values as a tidy table."""
        r = self.result
        return pd.DataFrame(
            {"coef": r.params, "std_err": r.bse, "z": r.tvalues, "p_value": r.pvalues}
        )


def fit_logistic(df: pd.DataFrame, name: str, features: list[str], target: str = config.TARGET) -> LogisticFit:
    """Fit one binomial GLM: ``target ~ f1 + f2 + ...`` (mirrors R's glm)."""
    formula = f"{target} ~ " + " + ".join(features)
    result = smf.glm(formula=formula, data=df, family=sm.families.Binomial()).fit()
    p_pred = result.predict(df)
    return LogisticFit(
        name=name,
        features=features,
        result=result,
        deviance=float(result.deviance),
        brier=brier_score(df[target], p_pred),
        n_obs=int(result.nobs),
    )


def fit_all_logistic(df: pd.DataFrame) -> dict[str, LogisticFit]:
    """Fit all four original model specs on the given data frame."""
    return {name: fit_logistic(df, name, feats) for name, feats in config.MODEL_SPECS.items()}


def replication_window(df: pd.DataFrame) -> pd.DataFrame:
    """Subset to 2000-2015 to reproduce the published study results."""
    return df[df["yearID"] <= config.REPLICATION_END_YEAR].copy()
