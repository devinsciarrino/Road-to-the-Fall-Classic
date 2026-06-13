// Thin API client. In dev, "/api" is proxied to the FastAPI backend (see
// vite.config.js). Override with VITE_API_BASE for a deployed backend.
const BASE = import.meta.env.VITE_API_BASE || "/api";

async function get(path) {
  const res = await fetch(`${BASE}${path}`);
  if (!res.ok) {
    const detail = await res.json().catch(() => ({}));
    throw new Error(detail.detail || `${res.status} ${res.statusText}`);
  }
  return res.json();
}

export const getSeasons = () => get("/seasons");
export const getModels = () => get("/models");
export const getCoefficients = () => get("/coefficients");
export const getStandings = (season, model) =>
  get(`/standings?season=${season}&model=${model}`);
