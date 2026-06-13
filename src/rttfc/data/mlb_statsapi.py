"""Recent / current-season collection from the official MLB Stats API.

The Lahman mirror stops at 2021 and the legacy scrapers (FanGraphs / Baseball
Reference) are down, so seasons from 2022 onward come from ``statsapi.mlb.com`` —
the free, unauthenticated JSON API behind MLB.com. It is immune to the HTML
scraping breakage that took out pybaseball, exposes every field the pipeline
needs, and covers the in-progress season (so it can also feed a live dashboard).

Each team-season is mapped onto the same raw Lahman column names the rest of the
pipeline expects, so MLB-API rows and Lahman rows are interchangeable downstream.
World Series winners are derived from the postseason schedule (gameType=W) rather
than hardcoded.

    python -m rttfc.data.mlb_statsapi 2024     # print one season as a check
"""

from __future__ import annotations

import sys

import pandas as pd
import requests

BASE = "https://statsapi.mlb.com/api/v1"
_TIMEOUT = 30

# MLB Stats API field -> raw Lahman column name, per stat group.
HITTING_MAP = {
    "runs": "R", "atBats": "AB", "hits": "H", "doubles": "2B", "triples": "3B",
    "homeRuns": "HR", "baseOnBalls": "BB", "strikeOuts": "SO", "stolenBases": "SB",
    "caughtStealing": "CS", "hitByPitch": "HBP", "sacFlies": "SF",
}
PITCHING_MAP = {
    "runs": "RA", "earnedRuns": "ER", "completeGames": "CG", "shutouts": "SHO",
    "saves": "SV", "hits": "HA", "homeRuns": "HRA", "baseOnBalls": "BBA",
    "strikeOuts": "SOA", "outs": "IPouts",
}
FIELDING_MAP = {"errors": "E", "doublePlays": "DP", "fielding": "FP"}
_LEAGUE = {103: "AL", 104: "NL"}


def _get(path: str) -> dict:
    resp = requests.get(f"{BASE}/{path}", timeout=_TIMEOUT)
    resp.raise_for_status()
    return resp.json()


def team_directory(season: int) -> dict[int, dict]:
    """team_id -> {teamID (abbreviation), name, lgID}."""
    j = _get(f"teams?sportId=1&season={season}")
    out = {}
    for t in j.get("teams", []):
        out[t["id"]] = {
            "teamID": t.get("abbreviation", str(t["id"])),
            "name": t.get("name", ""),
            "lgID": _LEAGUE.get(t.get("league", {}).get("id"), ""),
        }
    return out


def _group_stats(season: int, group: str, field_map: dict[str, str]) -> dict[int, dict]:
    """team_id -> {Lahman column: numeric value} for one stat group."""
    j = _get(f"teams/stats?sportId=1&season={season}&group={group}&stats=season")
    splits = j["stats"][0]["splits"]
    out: dict[int, dict] = {}
    for s in splits:
        stat = s["stat"]
        row = {col: pd.to_numeric(stat.get(api_key), errors="coerce")
               for api_key, col in field_map.items()}
        if group == "hitting":
            row["G"] = pd.to_numeric(stat.get("gamesPlayed"), errors="coerce")
        out[s["team"]["id"]] = row
    return out


def standings(season: int) -> dict[int, dict]:
    """team_id -> {W, L} from the regular-season standings."""
    j = _get(f"standings?leagueId=103,104&season={season}&standingsTypes=regularSeason")
    out = {}
    for rec in j.get("records", []):
        for tr in rec.get("teamRecords", []):
            out[tr["team"]["id"]] = {"W": tr.get("wins"), "L": tr.get("losses")}
    return out


def tally_series_winner(schedule_json: dict) -> int | None:
    """Team id with the most wins among Final games in a schedule payload.

    Pure (no I/O) so it is unit-testable; ``world_series_winner`` supplies the
    live World Series schedule.
    """
    wins: dict[int, int] = {}
    for date in schedule_json.get("dates", []):
        for game in date["games"]:
            if game.get("status", {}).get("abstractGameState") != "Final":
                continue
            for side in ("home", "away"):
                team = game["teams"][side]
                if team.get("isWinner"):
                    tid = team["team"]["id"]
                    wins[tid] = wins.get(tid, 0) + 1
    if not wins:
        return None
    return max(wins, key=wins.get)


def world_series_winner(season: int) -> int | None:
    """Return the winning team's MLB id from the World Series (gameType=W)."""
    return tally_series_winner(_get(f"schedule?sportId=1&season={season}&gameType=W"))


def fetch_season(season: int) -> pd.DataFrame:
    """One row per team for a season, in the raw Lahman schema (WSWin labelled)."""
    directory = team_directory(season)
    hitting = _group_stats(season, "hitting", HITTING_MAP)
    pitching = _group_stats(season, "pitching", PITCHING_MAP)
    fielding = _group_stats(season, "fielding", FIELDING_MAP)
    records = standings(season)
    champ_id = world_series_winner(season)

    rows = []
    for tid, info in directory.items():
        if tid not in hitting:  # not an active MLB club that season
            continue
        row = {"yearID": season, "teamID": info["teamID"], "franchID": info["teamID"],
               "lgID": info["lgID"], "name": info["name"], "mlb_id": tid}
        row.update(hitting[tid])
        row.update(pitching.get(tid, {}))
        row.update(fielding.get(tid, {}))
        rec = records.get(tid, {})
        row["W"], row["L"] = rec.get("W"), rec.get("L")
        row["WSWin"] = "Y" if (champ_id is not None and tid == champ_id) else "N"
        rows.append(row)
    return pd.DataFrame(rows)


def fetch_seasons(years: list[int], labeled_only: bool = True) -> pd.DataFrame:
    """Fetch several seasons. With labeled_only, drop seasons that have no World
    Series winner yet (in-progress), so the result is safe to train on."""
    frames = []
    for year in years:
        df = fetch_season(year)
        has_champ = (df["WSWin"] == "Y").any()
        if labeled_only and not has_champ:
            print(f"[mlb_api] {year}: no World Series winner yet — skipping", file=sys.stderr)
            continue
        frames.append(df)
        champ = df.loc[df["WSWin"] == "Y", "name"]
        print(f"[mlb_api] {year}: {len(df)} teams"
              + (f", WS winner = {champ.iloc[0]}" if len(champ) else ""))
    return pd.concat(frames, ignore_index=True) if frames else pd.DataFrame()


if __name__ == "__main__":
    yr = int(sys.argv[1]) if len(sys.argv) > 1 else 2024
    df = fetch_season(yr)
    print(df.to_string())
