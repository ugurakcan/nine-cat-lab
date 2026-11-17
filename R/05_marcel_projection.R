# ============================================================
# NBA Fantasy (9-cat, H2H Most Categories) project
# Step 5: Marcel-style next-season projection + backtest
#
# Projection for season T uses only seasons T-1, T-2, T-3:
#   1. Per-minute rates: weighted average (weights 1 / 0.5 / 0.25, by minutes),
#      regressed toward the position-group rate (G / F / C) with "phantom" minutes
#   2. Minutes per game: same idea, regressed toward league average MPG
#   3. Experience correction: average error of steps 1-2 by years in the league,
#      learned on training seasons only (captures young-player growth, vet decline)
#   4. Projected per-game stats -> 9-cat z-scores (scaled by season T-1 pool)
# Backtest: correction learned on 2010-2018, tested on 2019 to last season,
# compared with the naive "last season repeats" forecast.
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

regress_min   <- 500             # phantom position-average minutes added to each rate
regress_gp    <- 40              # phantom league-average games added to MPG
# (weights and regression amounts were tuned on 2010-2018 only)
train_targets <- 2010:2018
test_targets  <- 2019:last_season
eval_min      <- 1000            # actual minutes needed to judge rate accuracy

stats <- c("pts", "reb", "ast", "stl", "blk", "tov", "fg3m", "fgm", "fga", "ftm", "fta")

season_totals <- readRDS(file.path(data_dir, "season_totals.rds"))
z_table       <- readRDS(file.path(out_dir, "z_table.rds"))

hist <- season_totals %>%
  filter(!in_progress) %>%
  mutate(pos_group = case_when(position %in% c("PG", "SG", "G") ~ "G",
                               position == "C"                  ~ "C",
                               TRUE                             ~ "F"))


# ---- 1. Long format history and position-group rates ----
hist_long <- hist %>%
  select(season, athlete_id, pos_group, gp, min, all_of(stats)) %>%
  pivot_longer(cols = all_of(stats), names_to = "stat", values_to = "value")

lg_rates <- hist_long %>%
  group_by(season, pos_group, stat) %>%
  summarise(lg_rate = sum(value) / sum(min), .groups = "drop") %>%   # per minute
  mutate(target = season + 1) %>%
  select(target, pos_group, stat, lg_rate)

lg_mpg <- hist %>%
  group_by(season) %>%
  summarise(lg_mpg = sum(min) / sum(gp), .groups = "drop") %>%
  mutate(target = season + 1) %>%
  select(target, lg_mpg)


# ---- 2. Weighted history: each season feeds the next three targets ----
lag1 <- hist_long %>% mutate(target = season + 1, w = 1.0)
lag2 <- hist_long %>% mutate(target = season + 2, w = 0.5)
lag3 <- hist_long %>% mutate(target = season + 3, w = 0.25)
hist_w <- bind_rows(lag1, lag2, lag3)

# Position group = the one from the most recent season in the window
target_pos <- hist_w %>%
  filter(stat == "pts") %>%
  group_by(target, athlete_id) %>%
  filter(season == max(season)) %>%
  ungroup() %>%
  select(target, athlete_id, pos_group)


# ---- 3. Raw Marcel rates (per 48) and MPG ----
marcel_rates <- hist_w %>%
  group_by(target, athlete_id, stat) %>%
  summarise(w_value = sum(w * value), w_min = sum(w * min), .groups = "drop") %>%
  left_join(target_pos, by = c("target", "athlete_id")) %>%
  left_join(lg_rates, by = c("target", "pos_group", "stat")) %>%
  mutate(proj48 = (w_value + regress_min * lg_rate) / (w_min + regress_min) * 48) %>%
  select(target, athlete_id, stat, proj48)

marcel_mpg <- hist_w %>%
  filter(stat == "pts") %>%                       # one row per player-season
  group_by(target, athlete_id) %>%
  summarise(w_min = sum(w * min), w_gp = sum(w * gp), .groups = "drop") %>%
  left_join(lg_mpg, by = "target") %>%
  mutate(proj_mpg = (w_min + regress_gp * lg_mpg) / (w_gp + regress_gp)) %>%
  select(target, athlete_id, proj_mpg)


# ---- 4. Years in the league (from first season in data) ----
first_season <- hist %>%
  group_by(athlete_id) %>%
  summarise(first = min(season), .groups = "drop")

exp_table <- marcel_mpg %>%
  select(target, athlete_id) %>%
  left_join(first_season, by = "athlete_id") %>%
  mutate(
    exp = target - first + 1,
    exp_bucket = case_when(exp <= 2  ~ "2",
                           exp == 3  ~ "3",
                           exp == 4  ~ "4",
                           exp == 5  ~ "5",
                           exp <= 8  ~ "6-8",
                           exp <= 11 ~ "9-11",
                           TRUE      ~ "12+")
  ) %>%
  select(target, athlete_id, exp, exp_bucket)


# ---- 5. Actual outcomes (for learning the correction and testing) ----
actual_rates <- hist_long %>%
  transmute(target = season, athlete_id, stat,
            actual48 = value / min * 48, actual_min = min)

actual_mpg <- hist %>%
  transmute(target = season, athlete_id, actual_mpg = mpg, actual_min = min)

naive_rates <- hist_long %>%
  transmute(target = season + 1, athlete_id, stat, naive48 = value / min * 48)

naive_mpg <- hist %>%
  transmute(target = season + 1, athlete_id, naive_mpg = mpg)


# ---- 6. Experience correction ----
# Training-only correction (for honest backtest)
rate_adj_train <- marcel_rates %>%
  inner_join(actual_rates, by = c("target", "athlete_id", "stat")) %>%
  left_join(exp_table, by = c("target", "athlete_id")) %>%
  filter(target %in% train_targets, actual_min >= 500) %>%
  group_by(exp_bucket, stat) %>%
  summarise(adj = weighted.mean(actual48 - proj48, actual_min), .groups = "drop")

mpg_adj_train <- marcel_mpg %>%
  inner_join(actual_mpg, by = c("target", "athlete_id")) %>%
  left_join(exp_table, by = c("target", "athlete_id")) %>%
  filter(target %in% train_targets, actual_min >= 500) %>%
  group_by(exp_bucket) %>%
  summarise(adj_mpg = mean(actual_mpg - proj_mpg), .groups = "drop")

# Full correction (all seasons, used for the real current-season projection)
rate_adj_full <- marcel_rates %>%
  inner_join(actual_rates, by = c("target", "athlete_id", "stat")) %>%
  left_join(exp_table, by = c("target", "athlete_id")) %>%
  filter(target >= 2010, actual_min >= 500) %>%
  group_by(exp_bucket, stat) %>%
  summarise(adj = weighted.mean(actual48 - proj48, actual_min), .groups = "drop")

mpg_adj_full <- marcel_mpg %>%
  inner_join(actual_mpg, by = c("target", "athlete_id")) %>%
  left_join(exp_table, by = c("target", "athlete_id")) %>%
  filter(target >= 2010, actual_min >= 500) %>%
  group_by(exp_bucket) %>%
  summarise(adj_mpg = mean(actual_mpg - proj_mpg), .groups = "drop")

# Corrected projections: training correction for test seasons, full correction for the current season
proj_rates <- marcel_rates %>%
  left_join(exp_table, by = c("target", "athlete_id")) %>%
  left_join(rate_adj_train, by = c("exp_bucket", "stat")) %>%
  rename(adj_train = adj) %>%
  left_join(rate_adj_full, by = c("exp_bucket", "stat")) %>%
  mutate(adj = ifelse(target == current_season, adj, adj_train),
         adj = ifelse(is.na(adj), 0, adj),
         proj48_adj = pmax(0, proj48 + adj)) %>%
  select(target, athlete_id, exp, exp_bucket, stat, proj48, proj48_adj)

proj_mpg <- marcel_mpg %>%
  left_join(exp_table, by = c("target", "athlete_id")) %>%
  left_join(mpg_adj_train, by = "exp_bucket") %>%
  rename(adj_train = adj_mpg) %>%
  left_join(mpg_adj_full, by = "exp_bucket") %>%
  mutate(adj = ifelse(target == current_season, adj_mpg, adj_train),
         adj = ifelse(is.na(adj), 0, adj),
         proj_mpg_adj = pmax(0, proj_mpg + adj)) %>%
  select(target, athlete_id, proj_mpg, proj_mpg_adj)


# ---- 7. Backtest A: rate and minutes accuracy (2019 to last season) ----
rate_eval <- proj_rates %>%
  filter(target %in% test_targets) %>%
  inner_join(actual_rates, by = c("target", "athlete_id", "stat")) %>%
  inner_join(naive_rates,  by = c("target", "athlete_id", "stat")) %>%
  filter(actual_min >= eval_min) %>%
  group_by(stat) %>%
  summarise(
    n            = n(),
    rmse_naive   = sqrt(mean((naive48    - actual48)^2)),
    rmse_marcel  = sqrt(mean((proj48_adj - actual48)^2)),
    improvement  = round(100 * (1 - rmse_marcel / rmse_naive), 1),   # % error reduction
    cor_naive    = cor(naive48,    actual48),
    cor_marcel   = cor(proj48_adj, actual48),
    .groups = "drop"
  )
print(rate_eval %>% mutate(across(where(is.double), ~ round(.x, 3))), width = Inf)

mpg_eval <- proj_mpg %>%
  filter(target %in% test_targets) %>%
  inner_join(actual_mpg, by = c("target", "athlete_id")) %>%
  inner_join(naive_mpg,  by = c("target", "athlete_id")) %>%
  filter(actual_min >= 500) %>%
  summarise(
    n           = n(),
    rmse_naive  = sqrt(mean((naive_mpg    - actual_mpg)^2)),
    rmse_marcel = sqrt(mean((proj_mpg_adj - actual_mpg)^2)),
    improvement = round(100 * (1 - rmse_marcel / rmse_naive), 1)
  )
print(mpg_eval)

# Learned experience correction for minutes (young players gain, vets lose)
print(mpg_adj_full %>% mutate(adj_mpg = round(adj_mpg, 2)))


# ---- 8. Projected per-game stats -> 9-cat z-scores ----
# Scale: pool mean / SD of season T-1 (known before season T starts)
pool_lg <- z_table %>%
  filter(in_pool) %>%
  group_by(season) %>%
  summarise(lg_fg = sum(fgm) / sum(fga), lg_ft = sum(ftm) / sum(fta), .groups = "drop")

pool_scale <- z_table %>%
  filter(in_pool) %>%
  left_join(pool_lg, by = "season") %>%
  transmute(season,
            pts = ppg, reb = rpg, ast = apg, stl = spg, blk = bpg, tov = tpg, fg3m = fg3m_pg,
            fg_imp = (fgm - lg_fg * fga) / gp,
            ft_imp = (ftm - lg_ft * fta) / gp) %>%
  pivot_longer(-season, names_to = "cat", values_to = "value") %>%
  group_by(season, cat) %>%
  summarise(pool_mean = mean(value), pool_sd = sd(value), .groups = "drop") %>%
  mutate(target = season + 1) %>%
  select(target, cat, pool_mean, pool_sd)

proj_pg <- proj_rates %>%
  select(target, athlete_id, exp, stat, proj48_adj) %>%
  pivot_wider(names_from = stat, values_from = proj48_adj) %>%
  left_join(proj_mpg, by = c("target", "athlete_id")) %>%
  left_join(pool_lg %>% mutate(target = season + 1) %>% select(-season), by = "target") %>%
  mutate(
    pts    = pts  * proj_mpg_adj / 48,
    reb    = reb  * proj_mpg_adj / 48,
    ast    = ast  * proj_mpg_adj / 48,
    stl    = stl  * proj_mpg_adj / 48,
    blk    = blk  * proj_mpg_adj / 48,
    tov    = tov  * proj_mpg_adj / 48,
    fg3m   = fg3m * proj_mpg_adj / 48,
    fgm    = fgm  * proj_mpg_adj / 48,
    fga    = fga  * proj_mpg_adj / 48,
    ftm    = ftm  * proj_mpg_adj / 48,
    fta    = fta  * proj_mpg_adj / 48,
    fg_imp = fgm - lg_fg * fga,
    ft_imp = ftm - lg_ft * fta
  )

proj_z <- proj_pg %>%
  select(target, athlete_id, pts, reb, ast, stl, blk, tov, fg3m, fg_imp, ft_imp) %>%
  pivot_longer(-c(target, athlete_id), names_to = "cat", values_to = "value") %>%
  inner_join(pool_scale, by = c("target", "cat")) %>%
  mutate(z = (value - pool_mean) / pool_sd,
         z = ifelse(cat == "tov", -z, z)) %>%
  select(target, athlete_id, cat, z) %>%
  pivot_wider(names_from = cat, values_from = z, names_prefix = "pz_") %>%
  mutate(proj_total = pz_pts + pz_reb + pz_ast + pz_stl + pz_blk +
                      pz_tov + pz_fg3m + pz_fg_imp + pz_ft_imp)


# ---- 9. Backtest B: does it rank fantasy value better than "last season"? ----
value_eval <- proj_z %>%
  filter(target %in% test_targets) %>%
  select(target, athlete_id, proj_total) %>%
  inner_join(z_table %>% select(target = season, athlete_id, actual_total = total_pg),
             by = c("target", "athlete_id")) %>%
  inner_join(z_table %>% transmute(target = season + 1, athlete_id, naive_total = total_pg),
             by = c("target", "athlete_id")) %>%
  group_by(target) %>%
  mutate(rank_actual = rank(-actual_total),
         rank_proj   = rank(-proj_total),
         rank_naive  = rank(-naive_total)) %>%
  summarise(
    n             = n(),
    cor_naive     = cor(naive_total, actual_total, method = "spearman"),
    cor_marcel    = cor(proj_total,  actual_total, method = "spearman"),
    top100_naive  = sum(rank_actual <= 100 & rank_naive <= 100),
    top100_marcel = sum(rank_actual <= 100 & rank_proj  <= 100),
    .groups = "drop"
  )
print(value_eval %>% mutate(across(starts_with("cor"), ~ round(.x, 3))))

value_eval %>%
  summarise(across(-target, mean)) %>%
  mutate(across(everything(), ~ round(.x, 3))) %>%
  print()


# ---- 10. Current-season projections ----
proj_current <- proj_z %>%
  filter(target == current_season) %>%
  left_join(proj_pg %>% filter(target == current_season) %>% select(athlete_id, exp, proj_mpg_adj),
            by = "athlete_id") %>%
  left_join(z_table %>% filter(season == last_season) %>%
              select(athlete_id, player, team, total_last = total_pg, rank_last = rank_pg),
            by = "athlete_id") %>%
  left_join(hist %>%
              group_by(athlete_id) %>%
              summarise(name_last = last(player), team_last = last(team), .groups = "drop"),
            by = "athlete_id") %>%
  mutate(player = ifelse(is.na(player), name_last, player),   # players who missed most of last season
         team   = ifelse(is.na(team),   team_last, team),
         change = proj_total - total_last) %>%
  arrange(desc(proj_total)) %>%
  mutate(proj_rank = row_number())

# Top 40 projected
proj_current %>%
  select(proj_rank, player, team, exp, proj_mpg_adj, proj_total, total_last, rank_last) %>%
  mutate(across(c(proj_mpg_adj, proj_total, total_last), ~ round(.x, 1))) %>%
  head(40) %>%
  print(n = 40)

# Biggest projected risers within the projected top 150
proj_current %>%
  filter(proj_rank <= 150, !is.na(change)) %>%
  arrange(desc(change)) %>%
  select(player, team, exp, proj_rank, rank_last, proj_total, total_last, change) %>%
  mutate(across(c(proj_total, total_last, change), ~ round(.x, 1))) %>%
  head(20) %>%
  print(n = 20)


# ---- 11. Save ----
saveRDS(proj_z,       file.path(out_dir, "marcel_proj_z.rds"))
saveRDS(proj_pg,      file.path(out_dir, "marcel_proj_pg.rds"))
saveRDS(proj_current, file.path(out_dir, "marcel_proj_current.rds"))
write.csv(proj_current, file.path(out_dir, "marcel_proj_current.csv"), row.names = FALSE)
