"""Model tests: faithful logistic replication + sanity checks on the comparison."""

from __future__ import annotations

import pandas as pd
import pytest

from rttfc import config
from rttfc.data import features as ft
from rttfc.models import evaluate as ev
from rttfc.models import logistic as lg
from rttfc.predict import replicate_2023_rangers

# Published study values (legacy/Readme_Sciarrino_453project.docx, Tables 9-10).
ORIGINAL_DEVIANCE = {"model1_raw": 103.9006, "model2_calc": 113.1074,
                     "model3_houser": 112.8139, "model4_plus": 116.5193}
ORIGINAL_BRIER = {"model1_raw": 0.028012, "model2_calc": 0.030188,
                  "model3_houser": 0.030333, "model4_plus": 0.030196}


@pytest.fixture(scope="module")
def labeled() -> pd.DataFrame:
    if not config.PROCESSED_TEAMS.exists():
        ft.build_dataset()
    df = pd.read_parquet(config.PROCESSED_TEAMS)
    return df[df[config.TARGET].notna()].copy()


def test_logistic_replicates_published_results(labeled):
    fits = lg.fit_all_logistic(lg.replication_window(labeled))
    # Model 1 uses raw integer counts -> should reproduce essentially exactly.
    assert fits["model1_raw"].deviance == pytest.approx(ORIGINAL_DEVIANCE["model1_raw"], abs=0.05)
    assert fits["model1_raw"].brier == pytest.approx(ORIGINAL_BRIER["model1_raw"], abs=1e-4)
    # The rate/plus models differ only by the workbook's intermediate rounding.
    for name in fits:
        assert fits[name].deviance == pytest.approx(ORIGINAL_DEVIANCE[name], abs=0.4)
        assert fits[name].brier == pytest.approx(ORIGINAL_BRIER[name], abs=1e-3)


def test_model4_key_predictors_significant(labeled):
    """The headline finding: OPS+ and WHIP+ are significant predictors."""
    fit = lg.fit_all_logistic(lg.replication_window(labeled))["model4_plus"]
    pvals = fit.summary_frame()["p_value"]
    assert pvals["OPSP"] < 0.05
    assert pvals["WHIPP"] < 0.05


def test_2023_rangers_replication():
    table = replicate_2023_rangers()
    # Model 1 (raw integer inputs) reproduces the published 43.03% exactly.
    assert table.loc["model1_raw", "predicted"] == pytest.approx(0.4303, abs=1e-3)
    # All four within rounding tolerance of the published probabilities.
    assert (table["abs_diff"] < 0.01).all()


def test_models_beat_random_ranking(labeled):
    """Every model should rank the champion well above the ~15.5 random baseline."""
    baseline = ev.random_baseline_rank(labeled)
    for name, make in ev.ESTIMATORS.items():
        ranking = ev.champion_ranking_loso(make, labeled, config.ADVANCED_FEATURES)
        assert ranking["mean_champion_rank"] < baseline, name
