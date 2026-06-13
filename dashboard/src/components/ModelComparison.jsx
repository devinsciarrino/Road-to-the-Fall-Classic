const LABELS = { logistic: "Logistic", xgboost: "XGBoost", neural_net: "Neural Net" };

export default function ModelComparison({ info, activeModel }) {
  const comp = info?.comparison;
  if (!comp || !comp.models) return null;
  const baseline = comp.random_baseline_champion_rank;

  return (
    <div className="card model-comparison">
      <h3>Model accuracy (out-of-sample)</h3>
      <p className="muted">
        Leave-one-season-out over {info.dataset?.start_year}–{info.dataset?.end_year}. Random
        champion rank ≈ {baseline}.
      </p>
      <table className="cmp-table">
        <thead>
          <tr>
            <th>Model</th>
            <th>ROC-AUC</th>
            <th>Champ rank</th>
            <th>Top-3</th>
          </tr>
        </thead>
        <tbody>
          {Object.entries(comp.models).map(([name, m]) => (
            <tr key={name} className={name === activeModel ? "active" : ""}>
              <td>{LABELS[name] || name}</td>
              <td>{m.roc_auc?.toFixed(2)}</td>
              <td>{m.mean_champion_rank?.toFixed(1)}</td>
              <td>{(m.top3_hit_rate * 100)?.toFixed(0)}%</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}
