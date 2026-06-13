const MODEL_LABELS = {
  logistic: "Logistic",
  xgboost: "XGBoost",
  neural_net: "Neural Net",
};

export default function Controls({
  seasons,
  season,
  onSeason,
  models,
  model,
  onModel,
  onRefresh,
  loading,
  dataset,
}) {
  return (
    <div className="controls">
      <label>
        Season
        <select value={season ?? ""} onChange={(e) => onSeason(Number(e.target.value))}>
          {seasons.map((y) => (
            <option key={y} value={y}>
              {y}
            </option>
          ))}
        </select>
      </label>

      <div className="model-toggle">
        {models.map((m) => (
          <button
            key={m}
            className={m === model ? "active" : ""}
            onClick={() => onModel(m)}
          >
            {MODEL_LABELS[m] || m}
          </button>
        ))}
      </div>

      <button className="refresh" onClick={onRefresh} disabled={loading}>
        {loading ? "Loading…" : "↻ Refresh"}
      </button>

      {dataset && (
        <span className="dataset-note">
          trained on {dataset.n_team_seasons} team-seasons ({dataset.start_year}–
          {dataset.end_year}), {dataset.n_ws_winners} champions
        </span>
      )}
    </div>
  );
}
