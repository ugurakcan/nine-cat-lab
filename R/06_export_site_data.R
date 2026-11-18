# ============================================================
# NBA Fantasy (9-cat, H2H Most Categories) project
# Step 6: Export one JSON file per league size for the website
#
# Reads the outputs of steps 2-5 for the current league settings
# and writes site/data/t<teams>_r<roster>.json
# Run steps 1-5 first (with the same NBA_TEAMS / NBA_ROSTER).
# ============================================================

# install.packages(c("dplyr", "tidyr", "jsonlite"))
library(dplyr)
library(tidyr)
library(jsonlite)

# ---- Settings ----
data_dir       <- Sys.getenv("NBA_DATA_DIR", "~/nba_fantasy/data")
site_dir       <- Sys.getenv("NBA_SITE_DIR", "~/nba_fantasy/site")
current_season <- as.integer(Sys.getenv("NBA_CURRENT_SEASON", "2027"))   # 2027 = 2026-27, in progress

n_league_teams <- as.integer(Sys.getenv("NBA_TEAMS", "12"))
roster_size    <- as.integer(Sys.getenv("NBA_ROSTER", "13"))

league_tag <- paste0("t", n_league_teams, "_r", roster_size)
out_dir    <- file.path(data_dir, "leagues", league_tag)
json_dir   <- file.path(site_dir, "data")
dir.create(json_dir, recursive = TRUE, showWarnings = FALSE)

# Last 5 seasons including the current one (the oldest drops out each new season)
site_seasons <- (current_season - 4):current_season

game_logs       <- readRDS(file.path(data_dir, "game_logs.rds"))
z_table         <- readRDS(file.path(out_dir, "z_table.rds"))
player_contrib  <- readRDS(file.path(out_dir, "player_contrib.rds"))
kparams         <- readRDS(file.path(out_dir, "kparams.rds"))
proj_pg         <- readRDS(file.path(out_dir, "marcel_proj_pg.rds"))
schedule        <- readRDS(file.path(data_dir, "schedule.rds"))
punt_pivots     <- readRDS(file.path(out_dir, "punt_pivots.rds"))
proj_current    <- readRDS(file.path(out_dir, "marcel_proj_current.rds"))
pivot_by_season <- readRDS(file.path(out_dir, "pivot_by_season.rds"))


# ---- 0. Who is not playing right now ----
# missed = games the player's (latest) team has played since his last appearance.
# 3 or more usually means injured, suspended or out of the rotation.
team_dates <- game_logs %>%
  filter(season == current_season) %>%
  distinct(team, game_date)

missed_games <- game_logs %>%
  filter(season == current_season) %>%
  group_by(athlete_id) %>%
  summarise(last_date = max(game_date), team = last(team), .groups = "drop") %>%
  left_join(team_dates, by = "team", relationship = "many-to-many") %>%
  group_by(athlete_id) %>%
  summarise(missed = sum(game_date > last_date), .groups = "drop")

# Projected players who have not played at all this season: every team game so far is missed
no_games_yet <- proj_current %>%
  filter(!athlete_id %in% missed_games$athlete_id) %>%
  select(athlete_id, player, team) %>%
  left_join(team_dates %>% count(team, name = "missed"), by = "team") %>%
  mutate(missed = coalesce(missed, 0L))


# ---- 1. Player-season table (z-scores + per-category value contributions) ----
players <- z_table %>%
  filter(season %in% site_seasons) %>%
  select(season, season_label, athlete_id, player, team, position, gp, mpg, in_pool,
         ppg, rpg, apg, spg, bpg, fg3m_pg, fg_pct, ft_pct, tpg,
         z_pg_pts, z_pg_reb, z_pg_ast, z_pg_stl, z_pg_blk, z_pg_fg3m, z_pg_fg_imp, z_pg_ft_imp, z_pg_tov,
         z_p48_pts, z_p48_reb, z_p48_ast, z_p48_stl, z_p48_blk, z_p48_fg3m, z_p48_fg_imp, z_p48_ft_imp, z_p48_tov) %>%
  left_join(player_contrib %>%
              select(season, athlete_id,
                     k_pts, k_reb, k_ast, k_stl, k_blk, k_fg3m, k_fg_pct, k_ft_pct, k_tov),
            by = c("season", "athlete_id")) %>%
  left_join(missed_games %>% mutate(season = current_season), by = c("season", "athlete_id")) %>%
  mutate(across(where(is.double), ~ round(.x, 4))) %>%
  arrange(season, desc(k_pts))


# ---- 2. Current-season projection table ----
projections <- proj_current %>%
  left_join(bind_rows(missed_games, no_games_yet %>% select(athlete_id, missed)), by = "athlete_id") %>%
  select(athlete_id, player, team, missed, exp, proj_mpg_adj,
         pz_pts, pz_reb, pz_ast, pz_stl, pz_blk, pz_fg3m, pz_fg_imp, pz_ft_imp, pz_tov,
         proj_total, total_last, rank_last, change, proj_rank) %>%
  mutate(across(where(is.double), ~ round(.x, 3))) %>%
  arrange(proj_rank)


# ---- 3. Punt weights: one row per punt scenario (none, singles, pairs, trios) ----
punts <- punt_pivots %>%
  pivot_wider(names_from = cat, values_from = pivot) %>%
  select(punt, pts, reb, ast, stl, blk, fg3m, fg_pct, ft_pct, tov) %>%
  mutate(across(where(is.double), ~ round(.x, 5)))


# ---- 4. Category weights by season ----
pivots <- pivot_by_season %>%
  select(season_label, pts, reb, ast, stl, blk, fg3m, fg_pct, ft_pct, tov) %>%
  mutate(across(where(is.double), ~ round(.x, 3)))


# ---- 5. Write JSON ----
site_data <- list(
  updated        = as.character(max(game_logs$game_date)),
  current_season = current_season,
  teams          = n_league_teams,
  roster         = roster_size,
  players        = players,
  projections    = projections,
  punts          = punts,
  kparams        = kparams %>% mutate(across(where(is.double), ~ signif(.x, 6))),
  pivots         = pivots
)

write_json(site_data, file.path(json_dir, paste0(league_tag, ".json")),
           dataframe = "columns", auto_unbox = TRUE, na = "null", digits = NA)

# ---- 6. Current-season game logs for the player card ----
# Same for every league size, so it goes to one shared file (games.json)
games <- game_logs %>%
  filter(season == current_season) %>%
  transmute(athlete_id, date = as.character(game_date), team, opponent,
            min = round(min), pts, reb, ast, stl, blk, fg3m, fgm, fga, ftm, fta, tov) %>%
  arrange(athlete_id, date)

write_json(list(season = current_season, games = games), file.path(json_dir, "games.json"),
           dataframe = "columns", auto_unbox = TRUE, na = "null", digits = NA)

# ---- 7. Squad builder data (same for every league size): schedule, projected
# per-game stats, and how much each stat swings from game to game ----
proj_lines <- proj_pg %>%
  filter(target == current_season) %>%
  select(athlete_id, proj_mpg_adj, pts, reb, ast, stl, blk, fg3m, fgm, fga, ftm, fta, tov) %>%
  mutate(across(where(is.double), ~ round(.x, 3)))

# Game-to-game variance per stat, as a multiple of the player's average (last full season).
# Team weekly variance = sum over players of games x dispersion x average.
dispersion_counts <- game_logs %>%
  filter(season == current_season - 1) %>%
  select(athlete_id, pts, reb, ast, stl, blk, fg3m, tov) %>%
  pivot_longer(-athlete_id, names_to = "cat", values_to = "x") %>%
  group_by(athlete_id, cat) %>%
  filter(n() >= 20) %>%
  summarise(m = mean(x), v = var(x), .groups = "drop") %>%
  filter(m >= 0.3) %>%
  group_by(cat) %>%
  summarise(phi = median(v / m), .groups = "drop")

# For FG% and FT%: variance of (made - player's own % x attempted), per attempt
dispersion_pct <- game_logs %>%
  filter(season == current_season - 1) %>%
  group_by(athlete_id) %>%
  filter(n() >= 20, mean(fga) >= 1, mean(fta) >= 1) %>%
  mutate(fg_res = fgm - sum(fgm) / sum(fga) * fga,
         ft_res = ftm - sum(ftm) / sum(fta) * fta) %>%
  summarise(fg_pct = var(fg_res) / mean(fga),
            ft_pct = var(ft_res) / mean(fta), .groups = "drop") %>%
  summarise(fg_pct = median(fg_pct), ft_pct = median(ft_pct)) %>%
  pivot_longer(everything(), names_to = "cat", values_to = "phi")

dispersion <- bind_rows(dispersion_counts, dispersion_pct) %>%
  mutate(phi = round(phi, 4))

# Everyone who has played this season (names for the Squad builder search box)
season_players <- game_logs %>%
  filter(season == current_season) %>%
  group_by(athlete_id) %>%
  summarise(player = last(player), team = last(team), .groups = "drop") %>%
  left_join(missed_games, by = "athlete_id") %>%
  bind_rows(no_games_yet)

write_json(list(season      = current_season,
                players     = season_players,
                schedule    = schedule %>% transmute(date = as.character(date), home, away),
                projections = proj_lines,
                dispersion  = dispersion),
           file.path(json_dir, "squad.json"),
           dataframe = "columns", auto_unbox = TRUE, na = "null", digits = NA)

cat("Wrote", file.path(json_dir, paste0(league_tag, ".json")),
    "-", nrow(players), "player-seasons,", nrow(projections), "projections\n")
