# Road to the Fall Classic

**Which regular-season statistics best predict World Series success?**

A reproducible Python rewrite of a baseball-analytics study originally done in R + Excel.
It automates data collection, reproduces the original four logistic-regression models,
and adds XGBoost and a neural network with honest out-of-sample evaluation.

> **Status:** Both phases are built and verified. Phase 1 — automated data collection
> (2000–2025) + the models. Phase 2 — a **live** FastAPI + React/Vite dashboard that scores the
> current season on demand from the MLB Stats API. See [Live dashboard](#live-dashboard-phase-2).

---

## What changed from the original

The original project (preserved in [`legacy/`](legacy/)) was a 74-line R script running
`glm(dWSWin ~ ..., family = binomial)` against data **cleaned by hand in Excel**
(`Sciarrino_453project.xlsm`). This rewrite makes it robust and reproducible:

| | Original (R/Excel) | Now (Python) |
|---|---|---|
| Data | Manual Excel cleaning, 2000–2015 | Automated collection, 2000–2025 (Lahman through 2021 + MLB Stats API for 2022–present) |
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
    collect.py           # orchestrates sources -> raw table (Lahman + MLB API backfill)
    mlb_statsapi.py      # official MLB Stats API: 2022-present + current season
    features.py          # reproduces the Excel-calculated columns in pandas
    excel_reference.py   # reads the legacy .xlsm as a validation baseline
  models/
    logistic.py          # statsmodels GLM (faithful port) + sklearn logistic factory
    xgb.py               # regularized XGBClassifier (scale_pos_weight for imbalance)
    nn.py                # small, regularized sklearn MLP
    evaluate.py          # CV metrics + leave-one-season-out champion ranking
  serve.py               # score a (live) season -> ranked WS-win probabilities (+ cache)
  train.py               # CLI: train, evaluate, persist artifacts/metrics.json
  predict.py             # CLI: 2023 Rangers replication + score saved models
api/main.py              # FastAPI service (standings/team/models/seasons)
dashboard/               # React + Vite frontend
tests/                   # feature-parity + model-replication + MLB-API tests
legacy/                  # original R script, Excel workbook, and writeup
data/, artifacts/        # generated (gitignored)
```

---

## Data sources

`collect.py` combines two sources into one table (**2000–2025**, 780 team-seasons, 26 World
Series winners — up from the original 16):

1. **Lahman "Teams" (2000–2021)** — the dataset the original study used (Kaggle's "Baseball
   Databank"). The canonical `chadwickbureau/baseballdatabank` repo was removed, so `collect.py`
   reads a static `Teams.csv` from a **fallback chain of mirrors** and caches it locally.
2. **Official MLB Stats API (2022–present)** — `statsapi.mlb.com`, the free, unauthenticated
   JSON API behind MLB.com. `mlb_statsapi.py` pulls team hitting/pitching/fielding + standings,
   maps them onto the Lahman schema, and derives each season's World Series winner from the
   postseason schedule. It is immune to the HTML-scraping breakage that took out pybaseball, and
   covers the **in-progress season** — so it can also feed a live dashboard.

**Baseball Reference has no official public API** and pybaseball's FanGraphs/BR scrapers
currently return 403/404, so neither is used. The MLB Stats API is the robust, official route
for current data; the Lahman mirror is the reproducible historical backbone. (Note: 2020 was the
60-game COVID season, so its raw counting totals aren't directly comparable; rate and plus stats
are fine.)

---

## Models & findings

**Faithful logistic replication (2000–2015, in-sample).** Four `glm` specs ported to
statsmodels — raw counts, calculated rates, the Houser (2005) set, and league-normalized
"plus" stats. The headline finding reproduces: in the plus-stats model, **OPS+ (p≈0.002) and
WHIP+ (p≈0.007) are significant** predictors of World Series success; fielding (FP+) is not.

**Advanced models (2000–2025, out-of-sample).** Because exactly one team wins each season,
we use a **leave-one-season-out champion-ranking** metric alongside Brier/log-loss/ROC-AUC/PR-AUC.
Representative run over 26 seasons (`artifacts/metrics.json`; random-baseline champion rank ≈ 15.5):

| model | ROC-AUC | PR-AUC | mean champ rank | top-1 | top-3 |
|---|---|---|---|---|---|
| logistic | 0.84 | 0.152 | 5.1 | 15% | 54% |
| xgboost | 0.81 | 0.083 | 6.0 | 12% | 19% |
| neural_net | 0.75 | 0.077 | 8.0 | 0% | 23% |

Honest takeaway: all models carry real signal (champion ranked far above random), the simple
**logistic model ranks champions best** (its top-3 contains the eventual champion in 54% of
seasons), XGBoost is comparable and better-calibrated, and the neural net does not help on a
dataset this small and imbalanced.

---

## Live dashboard (Phase 2)

A FastAPI backend serves model predictions; a React + Vite frontend renders them.

```bash
# Terminal 1 — backend (needs trained models: python -m rttfc.train)
uvicorn api.main:app --port 8000

# Terminal 2 — frontend
cd dashboard && npm install && npm run dev
# open http://localhost:5173   (Vite proxies /api -> http://localhost:8000)
```

- **API** (`api/main.py`): `GET /standings?season=&model=`, `/team/{id}`, `/models`, `/seasons`,
  `/healthz`. It fetches the requested season from the MLB Stats API, scores all 30 teams, and
  caches the result for 30 minutes. Season defaults to the current (in-progress) one.
- **Dashboard** (`dashboard/`): a live **win-share leaderboard** for all 30 teams, season + model
  selectors, a per-team stat breakdown (OPS+/WHIP+/…), and an out-of-sample model-accuracy table.
  *Win share* normalizes each model's probabilities to sum to 100% across the league, so it reads
  sensibly regardless of how calibrated a given model is.

Because the serving features are rates and league-normalized stats (scale-invariant), the models
score a **partially-played** current season meaningfully — the dashboard updates as the season
unfolds.
