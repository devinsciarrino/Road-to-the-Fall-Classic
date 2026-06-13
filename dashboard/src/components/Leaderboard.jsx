export default function Leaderboard({ standings, loading, selected, onSelect }) {
  if (loading && !standings) return <div className="placeholder">Loading standings…</div>;
  if (!standings) return <div className="placeholder">No data.</div>;

  const max = Math.max(...standings.teams.map((t) => t.win_share), 0.0001);

  return (
    <div className="leaderboard">
      <div className="leaderboard-head">
        <h2>
          {standings.season} World Series win share
          <span className="model-chip">{standings.model}</span>
        </h2>
      </div>
      <ol className="team-list">
        {standings.teams.map((t) => {
          const pct = (t.win_share * 100).toFixed(1);
          const isSel = selected && selected.teamID === t.teamID;
          return (
            <li
              key={t.teamID}
              className={`team-row ${isSel ? "selected" : ""}`}
              onClick={() => onSelect(t)}
            >
              <span className="rank">{t.rank}</span>
              {t.logo ? (
                <img
                  className="team-logo"
                  src={t.logo}
                  alt=""
                  loading="lazy"
                  onError={(e) => {
                    e.currentTarget.style.visibility = "hidden";
                  }}
                />
              ) : (
                <span className="team-logo logo-fallback">{t.teamID}</span>
              )}
              <span className="team-name">
                {t.name}
                <span className="record">
                  {t.wins != null ? `${t.wins}-${t.losses}` : ""}
                </span>
              </span>
              <span className="bar-wrap">
                <span
                  className="bar"
                  style={{ width: `${(t.win_share / max) * 100}%` }}
                />
              </span>
              <span className="pct">{pct}%</span>
            </li>
          );
        })}
      </ol>
    </div>
  );
}
