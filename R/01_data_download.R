# ============================================================
# NBA Fantasy (9-cat, H2H Most Categories) project
# Step 1: Download and clean player game logs + season totals
# Source: hoopR (ESPN player box scores), 2001-02 to the current season
# Note: hoopR "season" = end year of the season (2002 = 2001-02)
# ============================================================

# install.packages(c("hoopR", "dplyr"))
library(hoopR)
library(dplyr)

# ---- Settings ----
out_dir <- Sys.getenv("NBA_DATA_DIR", "~/nba_fantasy/data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

current_season <- as.integer(Sys.getenv("NBA_CURRENT_SEASON", "2027"))   # 2027 = 2026-27, in progress
last_season    <- current_season - 1

# NBA Cup final does not count toward regular-season stats, so it is removed.
# Add the 2026-27 final date here after it is played (mid-December 2026).
cup_final_dates <- as.Date(c("2023-12-09", "2024-12-17", "2025-12-16"))


# ---- 1. Download player box scores (takes a few minutes) ----
box_raw <- load_nba_player_box(seasons = 2002:current_season)


# ---- 2. Clean game logs ----
game_logs <- box_raw %>%
  mutate(game_date = as.Date(game_date)) %>%
  filter(season_type == 2) %>%                     # regular season only (drops playoffs and play-in)
  filter(team_id <= 30) %>%                        # drops All-Star / Rising Stars exhibition teams
  filter(!(game_date %in% cup_final_dates)) %>%    # drops NBA Cup finals
  filter(did_not_play == FALSE, !is.na(minutes), minutes > 0) %>%
  transmute(
    season,
    game_id,
    game_date,
    athlete_id,
    player   = athlete_display_name,
    position = athlete_position_abbreviation,
    team     = team_abbreviation,
    opponent = opponent_team_abbreviation,
    starter,
    min  = minutes,
    pts  = points,
    reb  = rebounds,
    ast  = assists,
    stl  = steals,
    blk  = blocks,
    tov  = turnovers,
    fg3m = three_point_field_goals_made,
    fg3a = three_point_field_goals_attempted,
    fgm  = field_goals_made,
    fga  = field_goals_attempted,
    ftm  = free_throws_made,
    fta  = free_throws_attempted
  ) %>%
  arrange(season, game_date, game_id)


# ---- 3. Season totals per player ----
season_totals <- game_logs %>%
  group_by(season, athlete_id) %>%
  summarise(
    n_teams  = n_distinct(team),   # must come before team is overwritten below
    player   = last(player),
    position = last(position),
    team     = last(team),         # team at the end of the season
    gp   = n(),
    gs   = sum(starter),
    min  = sum(min),
    pts  = sum(pts),
    reb  = sum(reb),
    ast  = sum(ast),
    stl  = sum(stl),
    blk  = sum(blk),
    tov  = sum(tov),
    fg3m = sum(fg3m),
    fg3a = sum(fg3a),
    fgm  = sum(fgm),
    fga  = sum(fga),
    ftm  = sum(ftm),
    fta  = sum(fta),
    .groups = "drop"
  ) %>%
  mutate(
    season_label = paste0(season - 1, "-", substr(season, 3, 4)),
    mpg          = min / gp,
    in_progress  = season == current_season
  )


# ---- 4. Sanity checks ----
# Games per season: expect ~1189 (29 teams), 1230 (30 teams),
# 990 in 2011-12 lockout, fewer in 2019-20 and 2020-21 (COVID)
season_check <- game_logs %>%
  group_by(season) %>%
  summarise(
    games   = n_distinct(game_id),
    teams   = n_distinct(team),
    players = n_distinct(athlete_id),
    .groups = "drop"
  )
print(season_check, n = 30)

# Top 10 scorers of last season (min 50 games), should look familiar
season_totals %>%
  filter(season == last_season, gp >= 50) %>%
  mutate(ppg = round(pts / gp, 1)) %>%
  arrange(desc(ppg)) %>%
  select(player, team, gp, mpg, ppg) %>%
  head(10) %>%
  print()

# Era trend teaser: league 3PM and BLK per 36 minutes by season
season_totals %>%
  group_by(season_label) %>%
  summarise(
    fg3m_per36 = round(sum(fg3m) / sum(min) * 36, 2),
    blk_per36  = round(sum(blk) / sum(min) * 36, 2),
    .groups = "drop"
  ) %>%
  print(n = 30)


# ---- 5. Save ----
saveRDS(game_logs,     file.path(out_dir, "game_logs.rds"))
saveRDS(season_totals, file.path(out_dir, "season_totals.rds"))


# ---- 6. Current-season schedule (games per week for the Squad builder) ----
# game_date is the US Eastern date of the game; "TBD" rows are NBA Cup knockout placeholders
schedule <- load_nba_schedule(seasons = current_season) %>%
  filter(season_type == 2, home_abbreviation != "TBD", away_abbreviation != "TBD") %>%
  transmute(game_id,
            date = as.Date(game_date),
            home = home_abbreviation,
            away = away_abbreviation) %>%
  arrange(date)

saveRDS(schedule, file.path(out_dir, "schedule.rds"))
