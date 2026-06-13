import { useEffect, useState, useCallback } from "react";
import { getSeasons, getModels, getStandings } from "./api.js";
import Controls from "./components/Controls.jsx";
import Leaderboard from "./components/Leaderboard.jsx";
import TeamDetail from "./components/TeamDetail.jsx";
import ModelComparison from "./components/ModelComparison.jsx";

export default function App() {
  const [seasons, setSeasons] = useState([]);
  const [season, setSeason] = useState(null);
  const [models, setModels] = useState([]);
  const [model, setModel] = useState("logistic");
  const [modelsInfo, setModelsInfo] = useState(null);

  const [standings, setStandings] = useState(null);
  const [selected, setSelected] = useState(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);

  // Bootstrap: seasons + model metadata.
  useEffect(() => {
    Promise.all([getSeasons(), getModels()])
      .then(([s, m]) => {
        setSeasons(s.seasons.slice().reverse());
        setSeason(s.current);
        setModels(m.models);
        setModel(m.default || m.models[0]);
        setModelsInfo(m);
      })
      .catch((e) => setError(e.message));
  }, []);

  const load = useCallback(() => {
    if (!season || !model) return;
    setLoading(true);
    setError(null);
    getStandings(season, model)
      .then((data) => {
        setStandings(data);
        setSelected(data.teams[0] || null);
      })
      .catch((e) => setError(e.message))
      .finally(() => setLoading(false));
  }, [season, model]);

  useEffect(() => {
    load();
  }, [load]);

  return (
    <div className="app">
      <header className="header">
        <h1>⚾ Road to the Fall Classic</h1>
        <p className="tagline">
          World Series win probability from regular-season stats — live via the MLB Stats API
        </p>
      </header>

      <Controls
        seasons={seasons}
        season={season}
        onSeason={setSeason}
        models={models}
        model={model}
        onModel={setModel}
        onRefresh={load}
        loading={loading}
        dataset={modelsInfo?.dataset}
      />

      {error && <div className="error">⚠ {error}</div>}

      <main className="grid">
        <section className="panel">
          <Leaderboard
            standings={standings}
            loading={loading}
            selected={selected}
            onSelect={setSelected}
          />
        </section>
        <aside className="side">
          <TeamDetail team={selected} season={season} model={model} />
          <ModelComparison info={modelsInfo} activeModel={model} />
        </aside>
      </main>

      <footer className="footer">
        Historical training: Lahman (2000–2021) + MLB Stats API (2022–present). Win share
        normalizes probabilities to sum to 100% across the league.
      </footer>
    </div>
  );
}
