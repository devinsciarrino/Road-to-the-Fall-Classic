import { useState } from "react";

const SPEC_LABELS = {
  model1_raw: "Model 1 · raw stats",
  model2_calc: "Model 2 · rate stats",
  model3_houser: "Model 3 · Houser (2005)",
  model4_plus: "Model 4 · plus stats",
};
const SIG = 0.05;
const MAX_LOG = 4; // scale cap: -log10(p) maxes out at p = 1e-4

export default function PValueChart({ coefficients }) {
  const specs = coefficients?.specs;
  const [spec, setSpec] = useState("model1_raw");

  if (!specs || Object.keys(specs).length === 0) return null;
  const active = specs[spec] || [];

  const rows = active
    .filter((r) => r.variable !== "Intercept")
    .slice()
    .sort((a, b) => a.p_value - b.p_value);

  const negLog = (p) => Math.min(-Math.log10(Math.max(p, 1e-5)), MAX_LOG);
  const refLeft = (-Math.log10(SIG) / MAX_LOG) * 100;

  return (
    <div className="card pvalue-chart">
      <h3>Predictor significance</h3>
      <p className="muted">
        Logistic p-values (2000–2015). Longer bar = more significant; green = p &lt; 0.05; dashed
        line marks the 0.05 threshold. ▲/▼ = coefficient direction.
      </p>
      <select className="spec-select" value={spec} onChange={(e) => setSpec(e.target.value)}>
        {Object.keys(specs).map((k) => (
          <option key={k} value={k}>
            {SPEC_LABELS[k] || k}
          </option>
        ))}
      </select>

      <div className="pval-bars">
        {rows.map((r) => {
          const sig = r.p_value < SIG;
          return (
            <div className="pval-row" key={r.variable}>
              <span className="pval-var" title={`coef ${r.coef}`}>
                <span className={r.coef >= 0 ? "dir-up" : "dir-down"}>
                  {r.coef >= 0 ? "▲" : "▼"}
                </span>{" "}
                {r.variable}
              </span>
              <span className="pval-track">
                <span
                  className={`pval-bar ${sig ? "sig" : ""}`}
                  style={{ width: `${(negLog(r.p_value) / MAX_LOG) * 100}%` }}
                />
                <span className="pval-ref" style={{ left: `${refLeft}%` }} />
              </span>
              <span className={`pval-num ${sig ? "sig" : ""}`}>
                {r.p_value < 0.001 ? "<.001" : r.p_value.toFixed(3)}
              </span>
            </div>
          );
        })}
      </div>
    </div>
  );
}
