# Road to the Fall Classic

**Which regular-season statistics best predict World Series success?**

A reproducible Python rewrite of a baseball-analytics study originally done in R + Excel.
It automates data collection, reproduces the original four logistic-regression models,
and adds XGBoost and a neural network with honest out-of-sample evaluation.

> **Status:** Phase 1 (data pipeline + models) is complete and verified. Phase 2 (a live
> FastAPI + React dashboard) is scaffolded under `api/` and `dashboard/` — see the roadmap below.

---

## What changed from the original

The original project (preserved in [`legacy/`](legacy/)) was a 74-line R script running
`glm(dWSWin ~ ..., family = binomial)` against data **cleaned by hand in Excel**
(`Sciarrino_453project.xlsm`). This rewrite makes it robust and reproducible:

| | Original (R/Excel) | Now (Python) |
|---|---|---|
| Data | Manual Excel cleaning, 2000–2015 | Automated Lahman pull + scripted feature engineering, 2000–2021 |
| Models | 4 logistic regressions | Same 4 (faithfully reproduced) **+ XGBoost + neural net** |
| Evaluation | In-sample Brier score | Out-of-sample CV: Brier, log-loss, ROC/PR-AUC, leave-one-season-out champion ranking |
| Reproducibility | Excel UI steps, not scriptable | One command end-to-end |

The Python pipeline reproduces the workbook's features (validated to rounding tolerance) and
the published results: Model 1's deviance/Brier match essentially exactly, and the 2023 Rangers
Model 1 probability reproduces the published **43.03%** to four decimals.

---

## Quick start

```bash
python3 -m venv .venv && source .venv/bin/activate
pip install -e .            # or: pip install -r requirements.txt

python -m rttfc.data.collect    # download Lahman -> data/raw/teams_raw.parquet
python -m rttfc.data.features   # engineer features -> data/processed/teams.parquet
python -m rttfc.train           # train + evaluate all models -> artifacts/
python -m rttfc.predict         # 2023 Rangers replication check
pytest                          # 18 tests: feature parity + faithful replication
```

`train` and `predict` will build any missing upstream data automatically, so
`python -m rttfc.train` alone works from a clean checkout.

---

## Project layout

```
src/rttfc/
  config.py              # paths, year ranges, the 4 model feature specs
  data/
    collect.py           # Lahman Teams via mirror fallback + local cache
    features.py          # reproduces the Excel-calculated columns in pandas
    excel_reference.py   # reads the legacy .xlsm as a validation baseline
  models/
    logistic.py          # statsmodels GLM (faithful port) + sklearn logistic factory
    xgb.py               # regularized XGBClassifier (scale_pos_weight for imbalance)
    nn.py                # small, regularized sklearn MLP
    evaluate.py          # CV metrics + leave-one-season-out champion ranking
  train.py               # CLI: train, evaluate, persist artifacts/metrics.json
  predict.py             # CLI: 2023 Rangers replication + score saved models
tests/                   # feature-parity + model-replication tests
legacy/                  # original R script, Excel workbook, and writeup
data/, artifacts/        # generated (gitignored)
```

---

## Data sources

The original data is the Lahman "Teams" table (distributed on Kaggle as the "Baseball
Databank"). The canonical `chadwickbureau/baseballdatabank` GitHub repo has been removed and
pybaseball's live FanGraphs/Baseball-Reference scrapers currently return 403/404, so
`collect.py` reads a static `Teams.csv` from a **fallback chain of mirrors** and caches it
locally for reproducible, offline reruns. **Baseball Reference has no official public API** —
this mirror-based approach is the robust route.

The mirror covers **2000–2021** (660 team-seasons, 22 World Series winners — already an
expansion over the original 16). Seasons beyond that can be supplied via a drop-in
`data/raw/recent_seasons.csv` (same Lahman column names); their champion is labelled from
`config.RECENT_WS_WINNERS`. Note 2020 was the 60-game COVID season, so its raw counting totals
are not directly comparable (rate and plus stats are fine).

---

## Models & findings

**Faithful logistic replication (2000–2015, in-sample).** Four `glm` specs ported to
statsmodels — raw counts, calculated rates, the Houser (2005) set, and league-normalized
"plus" stats. The headline finding reproduces: in the plus-stats model, **OPS+ (p≈0.002) and
WHIP+ (p≈0.007) are significant** predictors of World Series success; fielding (FP+) is not.

**Advanced models (2000–2021, out-of-sample).** Because exactly one team wins each season,
we use a **leave-one-season-out champion-ranking** metric alongside Brier/log-loss/ROC-AUC/PR-AUC.
Representative run (`artifacts/metrics.json`; random-baseline champion rank ≈ 15.5):

| model | ROC-AUC | PR-AUC | mean champ rank | top-1 | top-3 |
|---|---|---|---|---|---|
| logistic | 0.80 | 0.148 | 5.6 | 18% | 45% |
| xgboost | 0.81 | 0.086 | 5.7 | 14% | 32% |
| neural_net | 0.70 | 0.057 | 8.4 | 0% | 14% |

Honest takeaway: all models carry real signal (champion ranked far above random), the simple
**logistic model ranks champions best**, XGBoost is comparable and better-calibrated, and the
neural net does not help on a dataset this small and imbalanced.

---

## Phase 2 roadmap — live dashboard

- **`api/`** — FastAPI service exposing `/standings` (current-season WS-win probabilities for
  all 30 teams), `/team/{id}`, and `/models`, with a scheduled refresh.
- **`dashboard/`** — React + Vite app: a probability leaderboard, per-team drill-down, model
  comparison, and historical accuracy.
- Requires a working current-season data feed (the legacy scrapers are down); revisit once a
  maintained source is wired into `collect.py`.
