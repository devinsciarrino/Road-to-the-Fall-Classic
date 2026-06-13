"""Score team-seasons with the trained models.

Two entry points:
  * ``replicate_2023_rangers()`` — refits the four logistic specs on 2000-2015 and
    predicts the 2023 Rangers (the same hardcoded inputs the original R script used),
    as a regression test against the published probabilities.
  * ``predict_proba()`` — score arbitrary team-seasons with a saved pipeline.

    python -m rttfc.predict                 # 2023 Rangers replication
    python -m rttfc.predict --model xgboost # also show the saved-model probability
"""

from __future__ import annotations

import argparse

import joblib
import pandas as pd

from rttfc import config
from rttfc.models import logistic as lg
from rttfc.train import MODELS_DIR

# Published 2023 Texas Rangers probabilities from the original study (Table 9).
ORIGINAL_2023 = {
    "model1_raw": 0.4303,
    "model2_calc": 0.0828,
    "model3_houser": 0.1262,
    "model4_plus": 0.1360,
}

# 2023 Rangers feature inputs, exactly as entered in legacy/Sciarrino_453project.R.
RANGERS_2023: dict[str, dict[str, float]] = {
    "model1_raw": dict(
        R=881, x1B=893, x2B=326, x3B=18, HR=233, BB=599, SO=1416, SB=79, CS=19,
        HBP=53, SF=10, RA=716, CG=3, SHO=1, SV=30, HA=1330, HRA=198, BBA=491,
        K=1351, E=57, DP=143,
    ),
    "model2_calc": dict(BA=0.263, OBP=0.337, SLG=0.452, ERA=4.28, KPN=8.5, WHIP=1.268, FP=0.990),
    "model3_houser": dict(BA=0.263, OBP=0.337, SLG=0.452, SB=79, x3B=18, KPN=8.5, WHIP=1.268, FP=0.990),
    "model4_plus": dict(OPSP=113, WHIPP=96.43, FPP=0.406),
}


def replicate_2023_rangers() -> pd.DataFrame:
    """Fit the 4 logistic specs on 2000-2015 and predict the 2023 Rangers."""
    df = pd.read_parquet(config.PROCESSED_TEAMS)
    rep = lg.replication_window(df[df[config.TARGET].notna()])
    fits = lg.fit_all_logistic(rep)

    rows = []
    for name, fit in fits.items():
        x = pd.DataFrame([RANGERS_2023[name]])
        prob = float(fit.result.predict(x).iloc[0])
        rows.append({
            "model": name,
            "predicted": round(prob, 4),
            "original": ORIGINAL_2023[name],
            "abs_diff": round(abs(prob - ORIGINAL_2023[name]), 4),
        })
    return pd.DataFrame(rows).set_index("model")


def load_saved_model(name: str) -> dict:
    path = MODELS_DIR / f"{name}.joblib"
    if not path.exists():
        raise FileNotFoundError(f"{path} not found — run `python -m rttfc.train` first.")
    return joblib.load(path)


def predict_proba(features: dict | pd.DataFrame, name: str = "logistic") -> float:
    """Win probability for one team-season using a saved serving pipeline."""
    bundle = load_saved_model(name)
    X = pd.DataFrame([features]) if isinstance(features, dict) else features
    return float(bundle["model"].predict_proba(X[bundle["features"]])[:, 1][0])


def main() -> None:
    parser = argparse.ArgumentParser(description="Predict World Series probability.")
    parser.add_argument("--model", help="also score the 2023 Rangers with this saved model")
    args = parser.parse_args()

    print("=== 2023 Texas Rangers — logistic replication vs published study ===")
    table = replicate_2023_rangers()
    print(table.to_string())

    if args.model:
        # ADVANCED_FEATURES == calc rate stats + plus stats, so merging these two
        # input dicts supplies every column the saved pipeline expects.
        feats = {**RANGERS_2023["model2_calc"], **RANGERS_2023["model4_plus"]}
        prob = predict_proba(feats, args.model)
        print(f"\nSaved '{args.model}' model probability for 2023 Rangers: {prob:.4f}")


if __name__ == "__main__":
    main()
