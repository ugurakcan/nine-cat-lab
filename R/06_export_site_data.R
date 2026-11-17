# ============================================================
# NBA Fantasy (9-cat, H2H Most Categories) project
# Step 6: Export one JSON file per league size for the website
#
# Reads the outputs of steps 2-5 for the current league settings
# and writes site/data/t<teams>_r<roster>.json
# Run steps 1-5 first (with the same NBA_TEAMS / NBA_ROSTER).
# ============================================================

# install.packages(c("dplyr", "jsonlite"))
library(dplyr)
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
player_values   <- readRDS(file.path(out_dir, "player_values.rds"))
proj_current    <- readRDS(file.path(out_dir, "marcel_proj_current.rds"))
pivot_by_season <- readRDS(file.path(out_dir, "pivot_by_season.rds"))


# ---- 1. Player-season table (z-scores + values under each punt) ----
players <- z_table %>%
  filter(season %in% site_seasons) %>%
  select(season, season_label, athlete_id, player, team, position, gp, mpg, in_pool,
         ppg, rpg, apg, spg, bpg, fg3m_pg, fg_pct, ft_pct, tpg,
         z_pg_pts, z_pg_reb, z_pg_ast, z_pg_stl, z_pg_blk, z_pg_fg3m, z_pg_fg_imp, z_pg_ft_imp, z_pg_tov,
         z_p48_pts, z_p48_reb, z_p48_ast, z_p48_stl, z_p48_blk, z_p48_fg3m, z_p48_fg_imp, z_p48_ft_imp, z_p48_tov) %>%
  left_join(player_values %>%
              select(season, athlete_id,
                     value_none, value_pts, value_reb, value_ast, value_stl, value_blk,
                     value_fg3m, value_fg_pct, value_ft_pct, value_tov),
            by = c("season", "athlete_id")) %>%
  mutate(across(where(is.double), ~ round(.x, 3))) %>%
  arrange(season, desc(value_none))


# ---- 2. Current-season projection table ----
projections <- proj_current %>%
  select(athlete_id, player, team, exp, proj_mpg_adj,
         pz_pts, pz_reb, pz_ast, pz_stl, pz_blk, pz_fg3m, pz_fg_imp, pz_ft_imp, pz_tov,
         proj_total, total_last, rank_last, change, proj_rank) %>%
  mutate(across(where(is.double), ~ round(.x, 3))) %>%
  arrange(proj_rank)


# ---- 3. Category weights by season ----
pivots <- pivot_by_season %>%
  select(season_label, pts, reb, ast, stl, blk, fg3m, fg_pct, ft_pct, tov) %>%
  mutate(across(where(is.double), ~ round(.x, 3)))


# ---- 4. Write JSON ----
site_data <- list(
  updated        = as.character(max(game_logs$game_date)),
  current_season = current_season,
  teams          = n_league_teams,
  roster         = roster_size,
  players        = players,
  projections    = projections,
  pivots         = pivots
)

write_json(site_data, file.path(json_dir, paste0(league_tag, ".json")),
           dataframe = "columns", auto_unbox = TRUE, na = "null", digits = NA)

cat("Wrote", file.path(json_dir, paste0(league_tag, ".json")),
    "-", nrow(players), "player-seasons,", nrow(projections), "projections\n")
