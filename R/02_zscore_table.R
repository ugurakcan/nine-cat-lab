# ============================================================
# NBA Fantasy (9-cat, H2H Most Categories) project
# Step 2: Season-standardized 9-cat z-scores
#   z_pg  = per-game value (fantasy production, minutes included)
#   z_p48 = per-48-minute value (efficiency / talent, minutes removed)
# Each season is compared only to itself, against the rosterable pool
# (league size x roster size), so eras are directly comparable.
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

pool_size <- n_league_teams * roster_size   # rosterable players (12 x 13 = 156)

min_gp_full     <- 20   # minimum games, completed seasons
min_gp_current  <- 5    # minimum games, in-progress season
min_mpg         <- 10   # minimum minutes per game (stops tiny samples inflating per-48)

season_totals <- readRDS(file.path(data_dir, "season_totals.rds"))


# ---- 1. Qualified players ----
players <- season_totals %>%
  mutate(min_gp = ifelse(in_progress, min_gp_current, min_gp_full)) %>%
  filter(gp >= min_gp, mpg >= min_mpg) %>%
  select(-min_gp)


# ---- 2. Pass 1: z-scores against all qualified players ----
# FG% and FT% are volume-weighted "impact": made - league% * attempted
# (equals (player% - league%) * attempts, so a 2/2 shooter does not dominate)
lg1 <- players %>%
  group_by(season) %>%
  summarise(lg_fg = sum(fgm) / sum(fga), lg_ft = sum(ftm) / sum(fta), .groups = "drop")

long1 <- players %>%
  left_join(lg1, by = "season") %>%
  mutate(fg_imp = fgm - lg_fg * fga,
         ft_imp = ftm - lg_ft * fta) %>%
  select(season, athlete_id, gp, min, pts, reb, ast, stl, blk, tov, fg3m, fg_imp, ft_imp) %>%
  pivot_longer(cols = c(pts, reb, ast, stl, blk, tov, fg3m, fg_imp, ft_imp),
               names_to = "cat", values_to = "total") %>%
  mutate(pg = total / gp)

z1 <- long1 %>%
  group_by(season, cat) %>%
  mutate(z_pg = (pg - mean(pg)) / sd(pg)) %>%
  ungroup() %>%
  mutate(z_pg = ifelse(cat == "tov", -z_pg, z_pg))   # fewer turnovers = better

# Rosterable pool = top pool_size players per season by pass-1 total
pool <- z1 %>%
  group_by(season, athlete_id) %>%
  summarise(total_z1 = sum(z_pg), .groups = "drop") %>%
  group_by(season) %>%
  slice_max(total_z1, n = pool_size, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(in_pool = TRUE) %>%
  select(season, athlete_id, in_pool)


# ---- 3. Pass 2: z-scores against the rosterable pool ----
players <- players %>%
  left_join(pool, by = c("season", "athlete_id")) %>%
  mutate(in_pool = !is.na(in_pool))

lg2 <- players %>%
  filter(in_pool) %>%
  group_by(season) %>%
  summarise(lg_fg = sum(fgm) / sum(fga), lg_ft = sum(ftm) / sum(fta), .groups = "drop")

long2 <- players %>%
  left_join(lg2, by = "season") %>%
  mutate(fg_imp = fgm - lg_fg * fga,
         ft_imp = ftm - lg_ft * fta) %>%
  select(season, athlete_id, in_pool, gp, min, pts, reb, ast, stl, blk, tov, fg3m, fg_imp, ft_imp) %>%
  pivot_longer(cols = c(pts, reb, ast, stl, blk, tov, fg3m, fg_imp, ft_imp),
               names_to = "cat", values_to = "total") %>%
  mutate(pg  = total / gp,
         p48 = total / min * 48)

z2 <- long2 %>%
  group_by(season, cat) %>%
  mutate(z_pg  = (pg  - mean(pg[in_pool]))  / sd(pg[in_pool]),
         z_p48 = (p48 - mean(p48[in_pool])) / sd(p48[in_pool])) %>%
  ungroup() %>%
  mutate(z_pg  = ifelse(cat == "tov", -z_pg,  z_pg),
         z_p48 = ifelse(cat == "tov", -z_p48, z_p48))


# ---- 4. Wide table: one row per player-season ----
z_wide <- z2 %>%
  select(season, athlete_id, cat, z_pg, z_p48) %>%
  pivot_wider(names_from = cat, values_from = c(z_pg, z_p48))

z_table <- players %>%
  select(season, season_label, in_progress, athlete_id, player, position, team, n_teams,
         in_pool, gp, mpg, pts, reb, ast, stl, blk, tov, fg3m, fgm, fga, ftm, fta) %>%
  mutate(
    ppg = pts / gp, rpg = reb / gp, apg = ast / gp, spg = stl / gp,
    bpg = blk / gp, tpg = tov / gp, fg3m_pg = fg3m / gp,
    fg_pct = fgm / fga,
    ft_pct = ifelse(fta > 0, ftm / fta, NA)
  ) %>%
  left_join(z_wide, by = c("season", "athlete_id")) %>%
  mutate(
    total_pg  = z_pg_pts + z_pg_reb + z_pg_ast + z_pg_stl + z_pg_blk +
                z_pg_tov + z_pg_fg3m + z_pg_fg_imp + z_pg_ft_imp,
    total_p48 = z_p48_pts + z_p48_reb + z_p48_ast + z_p48_stl + z_p48_blk +
                z_p48_tov + z_p48_fg3m + z_p48_fg_imp + z_p48_ft_imp
  ) %>%
  group_by(season) %>%
  mutate(rank_pg  = rank(-total_pg,  ties.method = "first"),
         rank_p48 = rank(-total_p48, ties.method = "first")) %>%
  ungroup() %>%
  arrange(season, rank_pg)


# ---- 5. Checks ----
# Pool size per season should equal pool_size everywhere
z_table %>% group_by(season_label) %>% summarise(pool = sum(in_pool)) %>% print(n = 30)

# Last season top 15 by per-game value (should resemble end-of-season 9-cat rankings)
z_table %>%
  filter(season == last_season) %>%
  arrange(rank_pg) %>%
  select(rank_pg, player, team, gp, mpg, total_pg,
         z_pg_pts, z_pg_reb, z_pg_ast, z_pg_stl, z_pg_blk,
         z_pg_tov, z_pg_fg3m, z_pg_fg_imp, z_pg_ft_imp) %>%
  mutate(across(where(is.numeric) & !c(rank_pg, gp), ~ round(.x, 2))) %>%
  head(15) %>%
  print(width = Inf)

# Last season: biggest gap between per-48 rank and per-game rank among pool players
# (efficient players held back by minutes = candidates if their role grows)
z_table %>%
  filter(season == last_season, in_pool) %>%
  mutate(rank_gap = rank_pg - rank_p48) %>%
  arrange(desc(rank_gap)) %>%
  select(player, team, mpg, rank_pg, rank_p48, rank_gap) %>%
  mutate(mpg = round(mpg, 1)) %>%
  head(15) %>%
  print()


# ---- 6. Save ----
saveRDS(z_table, file.path(out_dir, "z_table.rds"))
write.csv(z_table, file.path(out_dir, "z_table.csv"), row.names = FALSE)
