# ============================================================
# NBA Fantasy (9-cat, H2H Most Categories) project
# Step 3: Weekly H2H simulation and category pivotality
#
# For each completed season:
#   - simulate snake drafts of the rosterable pool into the league's teams
#   - build real weekly team totals from actual game logs (Mon-Sun weeks)
#   - play H2H matchups and record each category's result
#   - pivot = how much winning vs losing a category changes the matchup result
#       1   = the other 8 categories split evenly, this one decides win vs loss
#       0.5 = this one decides win vs tie (or tie vs loss)
#       0   = the matchup is already decided without it
# ============================================================

# install.packages(c("dplyr", "tidyr"))
library(dplyr)
library(tidyr)

set.seed(2026)

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

n_sims         <- 20    # simulated drafts per season
draft_noise_sd <- 1.5   # randomness added to draft order (z units), creates varied team builds
min_week_games <- 20    # drop partial weeks at season start/end

game_logs <- readRDS(file.path(data_dir, "game_logs.rds"))
z_table   <- readRDS(file.path(out_dir, "z_table.rds"))


# ---- 1. Fantasy weeks (Monday to Sunday), completed seasons only ----
game_logs <- game_logs %>%
  filter(season < current_season) %>%
  mutate(week_start = game_date - (as.integer(format(game_date, "%u")) - 1))

valid_weeks <- game_logs %>%
  group_by(season, week_start) %>%
  summarise(n_games = n_distinct(game_id), .groups = "drop") %>%
  filter(n_games >= min_week_games)


# ---- 2. Weekly totals for pool players ----
pool <- z_table %>%
  filter(in_pool, !in_progress) %>%
  select(season, athlete_id, total_pg)

player_weeks <- game_logs %>%
  semi_join(pool, by = c("season", "athlete_id")) %>%
  semi_join(valid_weeks, by = c("season", "week_start")) %>%
  group_by(season, week_start, athlete_id) %>%
  summarise(across(c(pts, reb, ast, stl, blk, tov, fg3m, fgm, fga, ftm, fta), sum),
            .groups = "drop")


# ---- 3. Simulated snake drafts ----
drafts <- pool %>%
  crossing(sim = 1:n_sims) %>%
  mutate(draft_score = total_pg + rnorm(n(), 0, draft_noise_sd)) %>%
  group_by(season, sim) %>%
  mutate(pick = rank(-draft_score, ties.method = "first")) %>%
  ungroup() %>%
  mutate(
    round = ceiling(pick / n_league_teams),
    slot  = (pick - 1) %% n_league_teams + 1,
    fteam = ifelse(round %% 2 == 1, slot, n_league_teams + 1 - slot)   # snake order
  ) %>%
  select(season, sim, athlete_id, fteam)


# ---- 4. Fantasy team weekly totals ----
team_weeks <- drafts %>%
  inner_join(player_weeks, by = c("season", "athlete_id"), relationship = "many-to-many") %>%
  group_by(season, sim, fteam, week_start) %>%
  summarise(across(c(pts, reb, ast, stl, blk, tov, fg3m, fgm, fga, ftm, fta), sum),
            .groups = "drop") %>%
  mutate(fg_pct = fgm / fga,
         ft_pct = ftm / fta)


# ---- 5. Weekly matchups (team 1 vs 2, 3 vs 4, ...) ----
team_a <- team_weeks %>% filter(fteam %% 2 == 1) %>% mutate(match = (fteam + 1) / 2)
team_b <- team_weeks %>% filter(fteam %% 2 == 0) %>% mutate(match = fteam / 2)

# Category result from team A's view: 1 = win, 0 = tie, -1 = loss
matchups <- team_a %>%
  inner_join(team_b, by = c("season", "sim", "week_start", "match"), suffix = c("_a", "_b")) %>%
  transmute(
    season, sim, week_start, match,
    pts    = sign(pts_a    - pts_b),
    reb    = sign(reb_a    - reb_b),
    ast    = sign(ast_a    - ast_b),
    stl    = sign(stl_a    - stl_b),
    blk    = sign(blk_a    - blk_b),
    fg3m   = sign(fg3m_a   - fg3m_b),
    tov    = sign(tov_b    - tov_a),      # fewer turnovers wins
    fg_pct = sign(fg_pct_a - fg_pct_b),
    ft_pct = sign(ft_pct_a - ft_pct_b)
  ) %>%
  mutate(margin = pts + reb + ast + stl + blk + fg3m + tov + fg_pct + ft_pct)


# ---- 6. Category pivotality ----
pivot_long <- matchups %>%
  pivot_longer(cols = c(pts, reb, ast, stl, blk, fg3m, tov, fg_pct, ft_pct),
               names_to = "cat", values_to = "outcome") %>%
  mutate(others = margin - outcome,
         pivot  = (sign(others + 1) - sign(others - 1)) / 2)

pivot_by_season <- pivot_long %>%
  group_by(season, cat) %>%
  summarise(pivot = mean(pivot), .groups = "drop") %>%
  pivot_wider(names_from = cat, values_from = pivot) %>%
  mutate(season_label = paste0(season - 1, "-", substr(season, 3, 4))) %>%
  select(season_label, pts, reb, ast, stl, blk, fg3m, tov, fg_pct, ft_pct)


# ---- 7. Results ----
# How matchups end (margin = categories won minus lost); sanity check
matchups %>%
  count(margin) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  print()

# Pivotality by season (higher = more often the deciding category)
pivot_by_season %>%
  mutate(across(-season_label, ~ round(.x, 3))) %>%
  print(n = 30, width = Inf)

# Era comparison: early 2000s vs last five seasons
pivot_long %>%
  mutate(era = case_when(season <= 2006                ~ "early_2000s",
                         season >= current_season - 5  ~ "last_5")) %>%
  filter(!is.na(era)) %>%
  group_by(era, cat) %>%
  summarise(pivot = round(mean(pivot), 3), .groups = "drop") %>%
  pivot_wider(names_from = era, values_from = pivot) %>%
  arrange(desc(last_5)) %>%
  print()

# Which categories are won together (last five seasons)? Basis for punt strategies
cat_cor <- matchups %>%
  filter(season >= current_season - 5) %>%
  select(pts, reb, ast, stl, blk, fg3m, tov, fg_pct, ft_pct) %>%
  cor()
print(round(cat_cor, 2))


# ---- 8. Save ----
saveRDS(matchups,        file.path(out_dir, "sim_matchups.rds"))
saveRDS(team_weeks,      file.path(out_dir, "sim_team_weeks.rds"))   # used in step 4
saveRDS(pivot_by_season, file.path(out_dir, "pivot_by_season.rds"))
write.csv(pivot_by_season, file.path(out_dir, "pivot_by_season.csv"), row.names = FALSE)
