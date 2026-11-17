# Nine Cat Lab

Player values for 9-category head-to-head (most categories) fantasy basketball.

## What is here

| Path | What it does |
|---|---|
| `R/01_data_download.R` | Downloads NBA player game logs with hoopR (2001-02 to now) |
| `R/02_zscore_table.R` | Season-standardized z-scores against the rosterable pool |
| `R/03_weekly_simulation.R` | Simulated drafts and weekly matchups, category pivot weights |
| `R/04_player_value.R` | Player value (win probability added), with punt scenarios, last 5 seasons |
| `R/05_marcel_projection.R` | Marcel-style projection for the current season, with backtest |
| `R/06_export_site_data.R` | Writes one JSON file per league size for the website |
| `run_all.sh` | Runs step 1 once, then steps 2-6 for every league size |
| `site/index.html` | The website |
| `PASTE_INTO_GITHUB_daily.yml` | The daily automation (goes to `.github/workflows/daily.yml`) |

## League sizes

`run_all.sh` computes 8, 10, 12, 14 and 16 teams, each with 10 to 16 roster spots (35 leagues).
If you change these lists, change `TEAM_OPTS` and `ROSTER_OPTS` in `site/index.html` too.

## Run it on your own computer

```bash
bash run_all.sh                    # everything, about 10 minutes
TEAMS=12 ROSTERS=13 bash run_all.sh   # one league only
```

Or run a single script in RStudio. The settings at the top default to 12 teams and 13 roster
spots and to `~/nba_fantasy/data`.

## Things to update by hand

- **New season:** raise `NBA_CURRENT_SEASON` by one in the workflow file (2027 = 2026-27).
  The site always shows the last 5 seasons, so the oldest one drops out automatically.
- **NBA Cup final:** add the date to `cup_final_dates` in `R/01_data_download.R` after it is played.

## Data

Game data comes from ESPN through the [hoopR](https://hoopR.sportsdataverse.org) package.
The site publishes derived values (z-scores, rankings, projections), not raw box scores.
