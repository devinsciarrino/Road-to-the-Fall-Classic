// League-average reference points so each stat reads as good/bad at a glance.
// Plus stats are centered at 100 (OPS+/WHIP+) or 0 (FP+) by construction.
const STAT_META = [
  { key: "OPSP", label: "OPS+", hint: "100 = league avg, higher better" },
  { key: "WHIPP", label: "WHIP+", hint: "100 = league avg, lower better" },
  { key: "FPP", label: "Fielding+", hint: "0 = league avg, higher better" },
  { key: "BA", label: "AVG", hint: "" },
  { key: "OBP", label: "OBP", hint: "" },
  { key: "SLG", label: "SLG", hint: "" },
  { key: "ERA", label: "ERA", hint: "lower better" },
  { key: "WHIP", label: "WHIP", hint: "lower better" },
  { key: "KPN", label: "K/9", hint: "" },
  { key: "FP", label: "Fld%", hint: "" },
];

export default function TeamDetail({ team, season, model }) {
  if (!team) return <div className="card placeholder">Select a team.</div>;
  return (
    <div className="card team-detail">
      <div className="detail-head">
        <h3>{team.name}</h3>
        <span className="big-share">{(team.win_share * 100).toFixed(1)}%</span>
      </div>
      <div className="detail-sub">
        #{team.rank} · {season} · {model}
        {team.wins != null && ` · ${team.wins}-${team.losses}`}
        <span className="raw-prob">raw p = {(team.ws_probability * 100).toFixed(1)}%</span>
      </div>
      <table className="stat-table">
        <tbody>
          {STAT_META.map((s) => (
            <tr key={s.key}>
              <td className="stat-label" title={s.hint}>
                {s.label}
              </td>
              <td className="stat-val">{team.stats[s.key] ?? "—"}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}
