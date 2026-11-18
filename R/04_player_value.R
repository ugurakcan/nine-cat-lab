# ============================================================
# NBA Fantasy (9-cat, H2H Most Categories) project
# Step 4: Player value = expected matchup wins added per week
#
# value = sum over categories of
#           pivot(cat)                       how often the category decides the matchup
#         x player's weekly edge / sigma     how much the player moves that category,
#                                            relative to its week-to-week noise
#         x dnorm(0)                         converts that shift into win probability
#
# Units: matchup wins added per week vs an average rosterable player,
# shown x100 as percentage points (e.g. 12 = +12% matchup win probability).
# Punt scenarios: punted categories are treated as always lost, and the
# pivot weights of the remaining categories are recomputed. Every punt of
# 1, 2 or 3 categories is computed (129 scenarios plus "none").
# Requires steps 2 and 3 (step 3 must save sim_team_weeks.rds).
# ============================================================

# install.packages(c("dplyr", "tidyr"))
library(dplyr)
library(tidyr)

# ---- Settings ----
data_dir       <- Sys.getenv("NBA_DATA_DIR", "~/nba_fantasy/data")
current_season <- as.integer(Sys.getenv("NBA_CURRENT_SEASON", "2027"))   # 2027 = 2026-27, in progress
last_season    <- current_season - 1

# League settings (change here, or set NBA_TEAMS / NBA_ROSTER before running)
n_league_teams <- as.integer(Sys.getenv("NBA_TEAMS", "12"))
roster_size    <- as.integer(Sys.getenv("NBA_ROSTER", "13"))

# Each league size gets its own output folder, e.g. data/leagues/t12_r13
out_dir <- file.path(data_dir, "leagues", paste0("t", n_league_teams, "_r", roster_size))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

games_per_week <- 3.4          # average NBA games per team per week

# Rolling 5-season windows: when a new season starts, the oldest one drops out
recent_seasons <- (current_season - 5):(current_season - 1)   # completed seasons, for pivot weights and weekly noise
value_seasons  <- (current_season - 4):current_season         # seasons that get player values (last 5, incl. current)

cats <- c("pts", "reb", "ast", "stl", "blk", "fg3m", "tov", "fg_pct", "ft_pct")

z_table    <- readRDS(file.path(out_dir, "z_table.rds"))
matchups   <- readRDS(file.path(out_dir, "sim_matchups.rds"))
team_weeks <- readRDS(file.path(out_dir, "sim_team_weeks.rds"))


# ---- 1. Pivot weights for each punt scenario (recent seasons) ----
# Scenario names: "none", "ft_pct", "fg_pct+tov", "ast+blk+tov", ...
punt_sets <- c("none",
               cats,
               apply(combn(cats, 2), 2, paste, collapse = "+"),
               apply(combn(cats, 3), 2, paste, collapse = "+"))

# Which categories each scenario gives up (one row per punted category)
punt_members <- tibble(punt = punt_sets, cat = punt_sets) %>%
  separate_rows(cat, sep = "\\+") %>%
  filter(cat != "none") %>%
  mutate(punted = TRUE)

# Matchups with the same 9 category results are counted once (same answer, much faster)
patterns <- matchups %>%
  filter(season %in% recent_seasons) %>%
  count(pts, reb, ast, stl, blk, fg3m, tov, fg_pct, ft_pct, name = "n") %>%
  mutate(pattern = row_number())

punt_pivots <- patterns %>%
  pivot_longer(cols = all_of(cats), names_to = "cat", values_to = "outcome") %>%
  crossing(punt = punt_sets) %>%
  left_join(punt_members, by = c("punt", "cat")) %>%
  mutate(punted  = !is.na(punted),
         outcome = ifelse(punted, -1, outcome)) %>%     # punted categories always lost
  group_by(punt, pattern) %>%
  mutate(margin = sum(outcome)) %>%
  ungroup() %>%
  mutate(others = margin - outcome,
         pivot  = (sign(others + 1) - sign(others - 1)) / 2,
         pivot  = ifelse(punted, 0, pivot)) %>%
  group_by(punt, cat) %>%
  summarise(pivot = weighted.mean(pivot, n), .groups = "drop")


# ---- 2. Weekly noise per category (SD of team-vs-team weekly difference) ----
noise <- team_weeks %>%
  filter(season %in% recent_seasons) %>%
  group_by(season) %>%
  summarise(
    fga_week = mean(fga),
    fta_week = mean(fta),
    across(c(pts, reb, ast, stl, blk, fg3m, tov, fg_pct, ft_pct), ~ sd(.x) * sqrt(2)),
    .groups = "drop"
  ) %>%
  summarise(across(-season, mean))

fga_week <- noise$fga_week   # average team FGA per week
fta_week <- noise$fta_week   # average team FTA per week

sigma <- noise %>%
  select(all_of(cats)) %>%
  pivot_longer(everything(), names_to = "cat", values_to = "sigma")


# ---- 3. Player weekly edge per category vs average pool player ----
lg_pct <- z_table %>%
  filter(season %in% value_seasons, in_pool) %>%
  group_by(season) %>%
  summarise(lg_fg = sum(fgm) / sum(fga), lg_ft = sum(ftm) / sum(fta), .groups = "drop")

player_long <- z_table %>%
  filter(season %in% value_seasons) %>%
  left_join(lg_pct, by = "season") %>%
  transmute(
    season, season_label, athlete_id, player, team, position, gp, mpg, in_pool, rank_pg,
    pts = ppg, reb = rpg, ast = apg, stl = spg, blk = bpg, tov = tpg, fg3m = fg3m_pg,
    fg_pct = (fgm - lg_fg * fga) / gp / fga_week,   # per-game change in team FG%
    ft_pct = (ftm - lg_ft * fta) / gp / fta_week    # per-game change in team FT%
  ) %>%
  pivot_longer(cols = all_of(cats), names_to = "cat", values_to = "per_game") %>%
  group_by(season, cat) %>%
  mutate(edge = (per_game - mean(per_game[in_pool])) * games_per_week) %>%
  ungroup() %>%
  mutate(edge = ifelse(cat == "tov", -edge, edge))   # fewer turnovers = positive edge


# ---- 4. Value under no punt and each single-category punt ----
values_long <- player_long %>%
  left_join(sigma, by = "cat") %>%
  inner_join(punt_pivots %>% filter(punt %in% c("none", cats)),
             by = "cat", relationship = "many-to-many") %>%
  mutate(contribution = 100 * pivot * dnorm(0) * edge / sigma)

values <- values_long %>%
  group_by(season, season_label, athlete_id, player, team, position, gp, mpg, rank_pg, punt) %>%
  summarise(value = sum(contribution), .groups = "drop") %>%
  group_by(season, punt) %>%
  mutate(rank = rank(-value, ties.method = "first")) %>%
  ungroup()

values_wide <- values %>%
  select(-rank) %>%
  pivot_wider(names_from = punt, values_from = value, names_prefix = "value_") %>%
  arrange(season, desc(value_none))

# Per-category contribution before punt weighting. The website multiplies these
# by the pivot weights of any punt combination: value = sum(pivot x k)
player_contrib <- player_long %>%
  left_join(sigma, by = "cat") %>%
  mutate(k = 100 * dnorm(0) * edge / sigma) %>%
  select(season, athlete_id, cat, k) %>%
  pivot_wider(names_from = cat, values_from = k, names_prefix = "k_")


# ---- 5. Results ----
# Pivot weights by single-category punt (rows = punted category, columns = category weight)
punt_pivots %>%
  filter(punt %in% c("none", cats)) %>%
  mutate(pivot = round(pivot, 3)) %>%
  pivot_wider(names_from = cat, values_from = pivot) %>%
  select(punt, all_of(cats)) %>%
  print(width = Inf)

# Weekly noise: how much each category swings week to week
print(round(noise, 3), width = Inf)

# Last season top 25 by value (no punt), compared with plain z-score rank
values_wide %>%
  filter(season == last_season) %>%
  mutate(rank_value = rank(-value_none, ties.method = "first")) %>%
  select(rank_value, rank_pg, player, team, gp, value_none) %>%
  mutate(value_none = round(value_none, 1)) %>%
  head(25) %>%
  print(n = 25)

# Last season top 12 under the most common punt builds
values %>%
  filter(season == last_season, punt %in% c("none", "ft_pct", "fg_pct", "tov", "ast", "blk"), rank <= 12) %>%
  select(rank, punt, player) %>%
  pivot_wider(names_from = punt, values_from = player) %>%
  arrange(rank) %>%
  print(width = Inf)

# Current season so far: top 25 (small sample early in the season)
values_wide %>%
  filter(season == current_season) %>%
  mutate(rank_value = rank(-value_none, ties.method = "first")) %>%
  select(rank_value, player, team, gp, mpg, value_none, value_ft_pct, value_fg_pct, value_tov) %>%
  mutate(across(c(mpg, starts_with("value_")), ~ round(.x, 1))) %>%
  head(25) %>%
  print(n = 25, width = Inf)


# ---- 6. Save ----
saveRDS(values_wide, file.path(out_dir, "player_values.rds"))
write.csv(values_wide, file.path(out_dir, "player_values.csv"), row.names = FALSE)
saveRDS(punt_pivots,    file.path(out_dir, "punt_pivots.rds"))      # all 130 punt scenarios
saveRDS(player_contrib, file.path(out_dir, "player_contrib.rds"))
