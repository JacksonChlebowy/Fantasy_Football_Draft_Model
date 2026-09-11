install.packages(c(
  "nflreadr",
  "nflfastR",
  "tidyverse",
  "arrow"
))

library(nflreadr)
library(nflfastR)
library(tidyverse)

## checking what data is available
help(package = "nflreadr")

ls("package:nflreadr")


# ROSTERS BY YEAR
# these will be used to map out full team stats down to the individual players and position groups
rosters = load_rosters(seasons = 2016:2026)

library(readr)
write_csv(rosters, "rosters.csv")


# NEXT GEN STATS 2016-2025 (EARLIES YEAR AVAILABLE)

ngs_passing <- load_nextgen_stats(
  seasons = 2016:2025,
  stat_type = "passing"
)

ngs_receiving <- load_nextgen_stats(
  seasons = 2016:2025,
  stat_type = "receiving"
)

ngs_rushing <- load_nextgen_stats(
  seasons = 2016:2025,
  stat_type = "rushing"
)

# NOTE 
# reg szn week 0 = Full Season Stats

write_csv(ngs_passing, "ngs_passing.csv")
write_csv(ngs_receiving, "ngs_receiving.csv")
write_csv(ngs_rushing, "ngs_rushing.csv")


#######################
# CLEANING AND PROCESSING
#######################

## FILTERING DOWN TO ONLY WEEK 0 -- SEASON SUMMARY ROWS
## dont need week to week performance for this model, maybe in the future for a start/sit model

library(dplyr)
library(readr)

passing_raw = read_csv("ngs_passing.csv")

passing_season = passing_raw %>%
  filter(
    week == 0,
    season_type == "REG"
  )

write_csv(passing_season, "passing_season_ngs.csv")


## weekly only (for future use)

passing_weekly = passing_raw %>%
  filter(
    week != 0,
    season_type == "REG"
  )

write_csv(passing_weekly, "passing_weekly_ngs.csv")

# RUSHING

rushing_raw = read_csv("ngs_rushing.csv")

rushing_season = rushing_raw %>%
  filter(
    week == 0,
    season_type == "REG"
  )

write_csv(rushing_season, "rushing_season_ngs.csv")


## weekly only (for future use)

rushing_weekly = rushing_raw %>%
  filter(
    week != 0,
    season_type == "REG"
  )

write_csv(rushing_weekly, "rushing_weekly_ngs.csv")



# RECEIVING

receiving_raw = read_csv("ngs_receiving.csv")

receiving_season = receiving_raw %>%
  filter(
    week == 0,
    season_type == "REG"
  )

write_csv(receiving_season, "receiving_season_ngs.csv")


## weekly only (for future use)

receiving_weekly = receiving_raw %>%
  filter(
    week != 0,
    season_type == "REG"
  )

write_csv(receiving_weekly, "receiving_weekly_ngs.csv")


#####################
# PLAYER STATS
####################

player_weekly = load_player_stats(
  seasons = 2016:2025,
  stat_type = "offense"
) %>%
  filter(
    season_type == "REG",
    position %in% c("QB", "WR", "TE", "RB")
  )
write_csv(player_weekly, "player_weekly_stats.csv")



player_season_stats = player_weekly %>%
  group_by(
    player_id,
    player_display_name,
    position,
    season
  ) %>%
  summarise(
    games_played = n_distinct(week),
    
    fantasy_points_ppr_total = sum(
      fantasy_points_ppr,
      na.rm = TRUE
    ),
    
    fantasy_ppg = mean(
      fantasy_points_ppr,
      na.rm = TRUE
    ),
    
    fantasy_median = median(
      fantasy_points_ppr,
      na.rm = TRUE
    ),
    
    fantasy_sd = sd(
      fantasy_points_ppr,
      na.rm = TRUE
    ),
    
    fantasy_points_max = max(
      fantasy_points_ppr,
      na.rm = TRUE
    ),
    
    # Floor = 20th percentile -- player achieved/exceeded score in aprox 80% of games
    fantasy_floor_observed = quantile(
      fantasy_points_ppr,
      probs = 0.20,
      na.rm = TRUE,
      names = FALSE
    ),
    
    # Ceiling = 80th percentile -- player achieved/exceeded score in aprox 20% of games
    
    fantasy_ceiling_observed = quantile(
      fantasy_points_ppr,
      probs = 0.80,
      na.rm = TRUE,
      names = FALSE
    ),
    
    # Season Totals and aggregations
    ## passing
    completions = sum(completions, na.rm = TRUE),
    attempts = sum(attempts, na.rm = TRUE),
    passing_yards = sum(passing_yards, na.rm = TRUE),
    passing_tds = sum(passing_tds, na.rm = TRUE),
    interceptions = sum(passing_interceptions, na.rm = TRUE),
    
    ## Rushing
    carries = sum(carries, na.rm = TRUE),
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    rushing_tds = sum(rushing_tds, na.rm = TRUE),
    
    ## Receiving 
    targets = sum(targets, na.rm = TRUE),
    receptions = sum(receptions, na.rm = TRUE),
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    receiving_tds = sum(receiving_tds, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    completion_pct = if_else(
      attempts > 0,
      completions / attempts,
      NA_real_
    ),
    
    passing_yards_per_attempt = if_else(
      attempts > 0,
      passing_yards / attempts,
      NA_real_
    ),
    
    rushing_yards_per_carry = if_else(
      carries > 0,
      rushing_yards / carries,
      NA_real_
    ),
    
    catch_rate = if_else(
      targets > 0,
      receptions / targets,
      NA_real_
    ),
    
    receiving_yards_per_target = if_else(
      targets > 0,
      receiving_yards / targets,
      NA_real_
    )
  )
write_csv(player_season_stats, "player_season_stats.csv")



######################
# Fantasy Football opportunity stats -- Mergeing them onto player_weekly stats
######################

ff_op = load_ff_opportunity(
  seasons = 2016:2025,
  stat_type = "weekly",
  model_version = "latest"
)

ff_opportunity_selected <- ff_op %>%
  filter(
    week <= if_else(season <= 2020, 17, 18)
  ) %>%
  select(
    player_id,
    full_name,
    season,
    week,
    posteam,
    
    rec_attempt,
    pass_air_yards,
    rec_air_yards,
    
    pass_fantasy_points,
    rec_fantasy_points,
    rush_fantasy_points,
    total_fantasy_points,
    
    pass_fantasy_points_exp,
    rec_fantasy_points_exp,
    rush_fantasy_points_exp,
    total_fantasy_points_exp,
    
    pass_completions_exp,
    receptions_exp,
    pass_yards_gained_exp,
    rec_yards_gained_exp,
    rush_yards_gained_exp,
    pass_touchdown_exp,
    rec_touchdown_exp,
    rush_touchdown_exp,
    pass_first_down_exp,
    rec_first_down_exp,
    rush_first_down_exp,
    
    pass_completions_diff,
    receptions_diff,
    pass_yards_gained_diff,
    rec_yards_gained_diff,
    rush_yards_gained_diff,
    pass_touchdown_diff,
    rec_touchdown_diff,
    rush_touchdown_diff,
    pass_fantasy_points_diff,
    rec_fantasy_points_diff,
    rush_fantasy_points_diff,
    total_fantasy_points_diff,
    
    pass_fantasy_points_exp_team,
    rec_fantasy_points_exp_team,
    rush_fantasy_points_exp_team,
    total_fantasy_points_exp_team,
    
    pass_touchdown_exp_team,
    rec_touchdown_exp_team,
    rush_touchdown_exp_team,
    total_touchdown_exp_team,
    
    total_yards_gained_exp_team
  ) %>%
  mutate(
    player_rec_xfp_share = if_else(
      rec_fantasy_points_exp_team > 0,
      rec_fantasy_points_exp / rec_fantasy_points_exp_team,
      NA_real_
    ),
    
    player_rush_xfp_share = if_else(
      rush_fantasy_points_exp_team > 0,
      rush_fantasy_points_exp / rush_fantasy_points_exp_team,
      NA_real_
    ),
    
    player_team_xfp_share = if_else(
      total_fantasy_points_exp_team > 0,
      total_fantasy_points_exp / total_fantasy_points_exp_team,
      NA_real_
    ),
    
    player_rec_td_exp_share = if_else(
      rec_touchdown_exp_team > 0,
      rec_touchdown_exp / rec_touchdown_exp_team,
      NA_real_
    ),
    
    player_rush_td_exp_share = if_else(
      rush_touchdown_exp_team > 0,
      rush_touchdown_exp / rush_touchdown_exp_team,
      NA_real_
    )
  )

## This version is safe for merging on to weekly player stats 
write_csv(ff_opportunity_selected, "ff_opportunity_selected(merge_on_player_weekly).csv")

## Merging onto player_weekly data



## Aggregating on season level, handling N/A rows and merging player_season

df = read.csv("ff_opportunity_selected(merge_on_player_weekly).csv")

season_opportunity_stats = df %>%
  filter(
    !is.na(player_id),
    player_id != "",
    !is.na(full_name),
    full_name != ""
  ) %>%
  group_by(
    player_id,
    full_name,
    season
  ) %>%
  summarise(
    games_with_opportunity_data = n_distinct(week),
    
    # Air-yard opportunity
    total_rec_air_yds = sum(rec_air_yards, na.rm = TRUE),
    total_pass_air_yds = sum(pass_air_yards, na.rm = TRUE),
    
    # Actual fantasy-point components
    pass_fantasy_points = sum(
      pass_fantasy_points,
      na.rm = TRUE
    ),
    rec_fantasy_points = sum(
      rec_fantasy_points,
      na.rm = TRUE
    ),
    rush_fantasy_points = sum(
      rush_fantasy_points,
      na.rm = TRUE
    ),
    total_fantasy_points = sum(
      total_fantasy_points,
      na.rm = TRUE
    ),
    
    # Expected fantasy-point components
    pass_xfp = sum(
      pass_fantasy_points_exp,
      na.rm = TRUE
    ),
    rec_xfp = sum(
      rec_fantasy_points_exp,
      na.rm = TRUE
    ),
    rush_xfp = sum(
      rush_fantasy_points_exp,
      na.rm = TRUE
    ),
    total_xfp = sum(
      total_fantasy_points_exp,
      na.rm = TRUE
    ),
    
    # Expected production
    pass_completions_exp = sum(
      pass_completions_exp,
      na.rm = TRUE
    ),
    receptions_exp = sum(
      receptions_exp,
      na.rm = TRUE
    ),
    pass_yards_exp = sum(
      pass_yards_gained_exp,
      na.rm = TRUE
    ),
    rec_yards_exp = sum(
      rec_yards_gained_exp,
      na.rm = TRUE
    ),
    rush_yards_exp = sum(
      rush_yards_gained_exp,
      na.rm = TRUE
    ),
    
    pass_td_exp = sum(
      pass_touchdown_exp,
      na.rm = TRUE
    ),
    rec_td_exp = sum(
      rec_touchdown_exp,
      na.rm = TRUE
    ),
    rush_td_exp = sum(
      rush_touchdown_exp,
      na.rm = TRUE
    ),
    
    pass_first_down_exp = sum(
      pass_first_down_exp,
      na.rm = TRUE
    ),
    rec_first_down_exp = sum(
      rec_first_down_exp,
      na.rm = TRUE
    ),
    rush_first_down_exp = sum(
      rush_first_down_exp,
      na.rm = TRUE
    ),
    
    # Actual minus expected efficiency
    pass_completions_over_exp = sum(
      pass_completions_diff,
      na.rm = TRUE
    ),
    receptions_over_exp = sum(
      receptions_diff,
      na.rm = TRUE
    ),
    pass_yards_over_exp = sum(
      pass_yards_gained_diff,
      na.rm = TRUE
    ),
    rec_yards_over_exp = sum(
      rec_yards_gained_diff,
      na.rm = TRUE
    ),
    rush_yards_over_exp = sum(
      rush_yards_gained_diff,
      na.rm = TRUE
    ),
    
    pass_td_over_exp = sum(
      pass_touchdown_diff,
      na.rm = TRUE
    ),
    rec_td_over_exp = sum(
      rec_touchdown_diff,
      na.rm = TRUE
    ),
    rush_td_over_exp = sum(
      rush_touchdown_diff,
      na.rm = TRUE
    ),
    
    pass_fp_over_exp = sum(
      pass_fantasy_points_diff,
      na.rm = TRUE
    ),
    rec_fp_over_exp = sum(
      rec_fantasy_points_diff,
      na.rm = TRUE
    ),
    rush_fp_over_exp = sum(
      rush_fantasy_points_diff,
      na.rm = TRUE
    ),
    total_fp_over_exp = sum(
      total_fantasy_points_diff,
      na.rm = TRUE
    ),
    
    # Team denominators for the player's active weeks
    team_pass_xfp = sum(
      pass_fantasy_points_exp_team,
      na.rm = TRUE
    ),
    team_rec_xfp = sum(
      rec_fantasy_points_exp_team,
      na.rm = TRUE
    ),
    team_rush_xfp = sum(
      rush_fantasy_points_exp_team,
      na.rm = TRUE
    ),
    team_total_xfp = sum(
      total_fantasy_points_exp_team,
      na.rm = TRUE
    ),
    
    team_pass_td_exp = sum(
      pass_touchdown_exp_team,
      na.rm = TRUE
    ),
    team_rec_td_exp = sum(
      rec_touchdown_exp_team,
      na.rm = TRUE
    ),
    team_rush_td_exp = sum(
      rush_touchdown_exp_team,
      na.rm = TRUE
    ),
    
    team_total_yards_exp = sum(
      total_yards_gained_exp_team,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    # Per-game opportunity
    rec_air_yds_pg =
      total_rec_air_yds / games_with_opportunity_data,
    
    pass_air_yds_pg =
      total_pass_air_yds / games_with_opportunity_data,
    
    total_xfp_pg =
      total_xfp / games_with_opportunity_data,
    
    rec_xfp_pg =
      rec_xfp / games_with_opportunity_data,
    
    rush_xfp_pg =
      rush_xfp / games_with_opportunity_data,
    
    # Season-level fantasy-point shares
    season_rec_xfp_share = if_else(
      team_rec_xfp > 0,
      rec_xfp / team_rec_xfp,
      NA_real_
    ),
    
    season_rush_xfp_share = if_else(
      team_rush_xfp > 0,
      rush_xfp / team_rush_xfp,
      NA_real_
    ),
    
    season_team_xfp_share = if_else(
      team_total_xfp > 0,
      total_xfp / team_total_xfp,
      NA_real_
    ),
    
    # Season-level touchdown shares
    season_pass_td_exp_share = if_else(
      team_pass_td_exp > 0,
      pass_td_exp / team_pass_td_exp,
      NA_real_
    ),
    
    season_rec_td_exp_share = if_else(
      team_rec_td_exp > 0,
      rec_td_exp / team_rec_td_exp,
      NA_real_
    ),
    
    season_rush_td_exp_share = if_else(
      team_rush_td_exp > 0,
      rush_td_exp / team_rush_td_exp,
      NA_real_
    )
  )



## NEXT TIME: MERGE SEASON OPPORTUNITY DATA TO player_season_stats, WEEKLY OPPORTUNITY DATA TO player_weekly.
## Trim down columns to only the most necessary. Create weekly features that can be added to seasonal?
## look through help section to see what other useful info i can find

##############
# Draft Picks 
##############

picks = load_draft_picks(seasons = 2000:2026)

picks = picks %>% 
  select(pfr_player_name, gsis_id, season, round, pick, position) %>%
    rename(player_name = pfr_player_name,
           player_id = gsis_id,
           draft_season = season,
           draft_round = round,
           draft_pick = pick)
write_csv(picks, "draft_picks.csv")


#############
# Fantasy Rankings
#############

library(dplyr)
library(stringr)
library(lubridate)

rankings <- load_ff_rankings(type = "all") %>%
  mutate(
    scrape_date = as.Date(scrape_date),
    season = year(scrape_date)
  ) %>%
  filter(
    season %in% 2016:2026,
    str_detect(
      page_type,
      regex("^redraft", ignore_case = TRUE)
    )
  )

rankings_overall = rankings %>% 
  filter(
    pos == "QB" | pos == "RB" | pos == "TE" | pos == "WR" | pos == "DST",
    ecr_type == "ro")

# HOW SHOULD I USE THIS, COMBINE SCRAPE DATES DOWN INTO 1 ROW? AND AGAIN BY SEASON?

write_csv(rankings_overall, "ff_rankings.csv")


# Cutting down the overall rankings list to only preseason rankings (ppr cheatsheets)
rankings_all <- read.csv("ff_rankings.csv") %>%
  mutate(
    scrape_date = as.Date(scrape_date),
    season = as.integer(season)
  )

preseason_rankings <- rankings_all %>%
  filter(
    str_detect(fp_page, "ppr-cheatsheets"),
    !str_detect(fp_page, "idp"),
    pos %in% c("QB", "RB", "WR", "TE"),
    season %in% 2016:2026
  )

# finding final date of scrape eaach szn, making sure they are pre season rankings
final_preseason_dates <- preseason_rankings %>%
  group_by(season) %>%
  summarise(
    final_scrape_date = max(scrape_date, na.rm = TRUE),
    .groups = "drop"
  )

final_preseason_dates

# keeping only the final pre-season rankings for each player -- most up to date prior to league start
season_preseason_rankings <- preseason_rankings %>%
  inner_join(
    final_preseason_dates,
    by = "season"
  ) %>%
  filter(scrape_date == final_scrape_date) %>% rename(draft_season = season)

write_csv(season_preseason_rankings, "pre_season_ff_rankings.csv")


######################
# SNAP COUNTS
######################

snap_count = load_snap_counts(
  seasons = 2016:2025,
  file_type = getOption("nflreadr.prefer", default = "csv")
)

write_csv(snap_count, "snap_count.csv")



#####################
# Team Stats
####################

team_stats_weekly = load_team_stats(
  seasons = 2016:2025,
  summary_level = c("week"),
  file_type = getOption("nflreadr.prefer", default = "csv")
) %>% filter(season_type == "REG")
write_csv(team_stats_weekly, "team_stats_weekly.csv")



team_stats_reg = load_team_stats(
  seasons = 2016:2025,
  summary_level = c("reg"),
  file_type = getOption("nflreadr.prefer", default = "csv")
)
write_csv(team_stats_reg, "team_stats_reg.csv")

####################
# Injuries 
###################

injuries = load_injuries(
  seasons = 2016:2025,
  file_type = getOption("nflreadr.prefer", default = "csv")
)

# removing full participants and non football related entries
injuries %>% summarize(missing_count = sum(report_primary_injury == "", na.rm = TRUE))

injuries = injuries %>% filter(report_primary_injury != "", na.rm = TRUE)

write_csv(injuries, "injuries.csv")












###################
# MERGING DATASETS TOGETHER FOR SIMPLE 17g PPR PREDICTION MODEL -- FLOOR - MEDIAN - CEILING
###################

## SEASON-LONG DATA

df = read_csv("player_season_stats.csv")

### ROSTER 
## merging team name onto season player stats
# cleaning team names, SD (san diego chargers) & Oak (oakland raiders) to LAC and LV to match player df
standardize_team = function(team_vec) {
  case_match(
    team_vec,
    "SD" ~ "LAC",
    "OAK" ~ "LV",
    "LAR" ~ "LA",
    "AZ" ~ "ARI",
    .default = team_vec
  )
}


roster = read_csv("rosters.csv") %>% 
  mutate(
    team = standardize_team(team)
  ) %>% filter(game_type == "REG")

roster_week1 = read_csv("rosters.csv") %>% 
  mutate(
    team = standardize_team(team)
  ) %>% filter( week == 1)

# joining "opening day" dates to calculate player age from birth dates
library(lubridate)

opening_days = read_csv("nfl_opening_days_2016_2026.csv")

roster <- roster %>%
  left_join(opening_days, by = "season") %>%
  mutate(
    birth_date = as.Date(birth_date),
    opening_day = as.Date(opening_day),
    age_at_season_start = time_length(
      interval(birth_date, opening_day),
      unit = "years"
    )
  ) %>% 
  mutate(
    age_at_season_start = floor(
      time_length(interval(birth_date, opening_day), unit = "years")
    )
  )



# Name standardizing function (will likely need later)
library(dplyr)
library(stringr)
library(tidyr)


standardize_name <- function(name_vec) {
  name_vec %>%
    tolower() %>%                                # 1. Make all lowercase
    str_replace_all(" (jr\\.?|sr\\.?|iii|iv)$", "") %>% # 2. Optional: Strip suffixes like Jr. or III
    str_remove_all("[[:punct:]]") %>%            # 3. Strip all punctuation (-, ', .)
    str_remove_all("\\s+")                       # 4. Strip all spaces
}

df = df %>% mutate(clean_player_name = standardize_name(player_display_name))


roster = roster %>%
  arrange(gsis_id, season) %>%
  group_by(gsis_id) %>%
  fill(
    birth_date,
    height,
    weight,
    rookie_year,
    draft_number,
    .direction = "downup"
  ) %>%
  ungroup()




sesason_stats_roster = merge(df,roster[, c("season", "gsis_id", "team", "birth_date", "height", "weight", "age_at_season_start", "years_exp", "rookie_year", "draft_number")], 
                             by.x = c("season", "player_id"), 
                             by.y = c("season", "gsis_id"))
# reorganizing cols for my own sanity
sesason_stats_roster = sesason_stats_roster %>%
  relocate(season, team, player_id, player_display_name, height, weight, age_at_season_start, years_exp, birth_date)



### TEAM STANDINGS

team_standings = read_csv("team_standings.csv") %>% 
  mutate(
    team = case_match(team,
                      "SD" ~ "LAC",
                      "OAK" ~ "LV",
                      .default = team))

sesason_stats_roster_standings = merge(sesason_stats_roster, team_standings[, c("season", "team", "pct", "scored", "allowed", "net", "sov", "sos", "playoff")], 
                             by.x = c("season", "team"), 
                             by.y = c("season", "team"))

# playoff column into a binary playoff flag (did or did not make playoffs)
sesason_stats_roster_standings = sesason_stats_roster_standings %>%
  mutate(team_made_playoffs = as.integer(!is.na(playoff)))

# renaming for clarity
sesason_stats_roster_standings = sesason_stats_roster_standings %>% 
  rename(
    "team_win_pct" = "pct",
    "team_pts_scored" = "scored",
    "team_pts_against" = "allowed",
    "team_net_pts" = "net",
    "team_sov" = "sov",
    "team_sos" = "sos"
)


###  TEAM STATS

team_stats = read_csv("team_stats_reg.csv")

# selecting desired columns and creating useful feautures 
team_stats_selected = team_stats %>%
  select(
    season,
    team,
    games,
    completions,
    attempts,
    passing_yards,
    passing_tds,
    passing_interceptions,
    passing_first_downs,
    passing_air_yards,
    passing_yards_after_catch,
    passing_epa,
    receiving_epa,
    passing_cpoe,
    sacks_suffered,
    sack_yards_lost,
    passing_10,
    passing_20,
    passing_40,
    carries,
    rushing_yards,
    rushing_tds,
    rushing_first_downs,
    rushing_epa,
    rushing_10,
    rushing_20,
    rushing_40,
    sack_fumbles_lost,
    rushing_fumbles_lost,
    receiving_fumbles_lost
  ) %>%
  mutate(
    # total number of offensive plays
    offensive_plays = attempts + carries + sacks_suffered,
    
    # pass/rush splits
    pass_rate = attempts / offensive_plays,
    rush_rate = carries / offensive_plays,
    
    # game pace/play style
    plays_per_game = offensive_plays / games,
    pass_attempts_per_game = attempts / games,
    carries_per_game = carries / games,
    passing_yards_per_game = passing_yards / games,
    rushing_yards_per_game = rushing_yards / games,
    
    # Offensive TD production
    passing_tds_per_game = passing_tds / games,
    rushing_tds_per_game = rushing_tds / games,
    offensive_tds = passing_tds + rushing_tds,
    offensive_tds_per_game = offensive_tds / games,
    
    # Efficiency 
    yards_per_pass_attempt = passing_yards / attempts,
    yards_per_carry = rushing_yards / carries,
    
    sack_rate = sacks_suffered / (attempts + sacks_suffered),
    interception_rate = passing_interceptions / attempts,
    
    # what % of passes/rushes result in a 1st down
    explosive_pass_rate_20 = passing_20 / attempts,
    explosive_rush_rate_20 = rushing_20 / carries,
    
    total_offensive_turnovers =
      passing_interceptions +
      sack_fumbles_lost +
      rushing_fumbles_lost +
      receiving_fumbles_lost
  )

# adding team_ to features
team_stats_selected <- team_stats_selected %>%
  rename_with(
    ~ paste0("team_", .x),
    -c(season, team)
  )

season_stats_roster_standings_teamstats = sesason_stats_roster_standings %>%
  left_join(
    team_stats_selected,
    by = c("season", "team")
  )

## adding "draft season" to the data, to cut out later lagging. Player and season stats should be informing the NEXT YEARS DRAFT Decision
season_stats_roster_standings_teamstats = season_stats_roster_standings_teamstats %>%
  mutate(draft_season = season + 1)


# I NOTICED A PROBLEM, NEED TO REMOVE ANY PLAYER SZN WHERE THEY PLAYED FOR 2+ TEAMS (season long data is not agg by team, for quickest resolve deleting rows entirely)
## NEED TO RESOLVE BEFORE CALCULATING VACATED TGTS, SNAPS, RUSH ATT, ETC.

# flags players who have played for more than 1 team in a season
multi_team_seasons = player_weekly %>%
  filter(
    season_type == "REG",
    !is.na(team)
  ) %>%
  group_by(
    season,
    player_id
  ) %>%
  summarise(
    teams_played_for = n_distinct(team),
    .groups = "drop"
  ) %>%
  filter(
    teams_played_for > 1
  )

# removing those players
season_stats_roster_standings_teamstats = season_stats_roster_standings_teamstats %>%
  anti_join(
    multi_team_seasons %>%
      select(season, player_id),
    by = c("season", "player_id")
  )






### FANTASY FOOTBALL RANKINGS (only 2020-2026)

ff_preseason_rankings = read_csv("pre_season_ff_rankings.csv")

ff_preseason_rankings = ff_preseason_rankings %>% mutate(clean_player_name = standardize_name(player))

season_stats_roster_standings_teamstats_ffranks = season_stats_roster_standings_teamstats %>%
  left_join(
    ff_preseason_rankings[, c("draft_season", "clean_player_name", "ecr", "sd", "best", "worst"
    )],
    by = c("draft_season", "clean_player_name") # align on draft season
  )



### SNAP COUNT 

snap_count = read_csv("snap_count.csv") %>% 
  filter(position %in% c("WR", "QB", "TE", "RB"), game_type == "REG") %>%
  rename(player_display_name = player)
  

snap_features = snap_count %>%
  mutate(
    clean_player_name = standardize_name(player_display_name),
      team = case_match(team,
                        "SD" ~ "LAC",
                        "OAK" ~ "LV",
                        .default = team)
  ) %>%
  group_by(
    season,
    clean_player_name,
    team,
    position
  ) %>%
  summarise(
    games_with_snap_data = n_distinct(week),
    total_snaps_played = sum(offense_snaps, na.rm = TRUE),
    avg_weekly_off_snapshare = mean(offense_pct, na.rm = TRUE),
    .groups = "drop"
  )

season_stats_roster_standings_teamstats_ffranks_snapshare = season_stats_roster_standings_teamstats_ffranks %>%
  left_join(
    snap_features,
    by = c("season", "clean_player_name", "team", "position")
  )



# POSITIONAL COMPETITION FEATURES -- (used chat to clean up all the messy code)

season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features =
  season_stats_roster_standings_teamstats_ffranks_snapshare %>%
  
  
  # TEAM OPPORTUNITY SHARES / RANKINGS
  
  group_by(season, team) %>%
  mutate(
    
    # -------------------------
    # Receiving
    # -------------------------
    
    team_targets = sum(targets, na.rm = TRUE),
    
    team_receiving_yards = sum(receiving_yards, na.rm = TRUE),
    
    # % of total team targets
    team_target_share = if_else(
      team_targets > 0,
      targets / team_targets,
      NA_real_
    ),
    
    # % of total team receiving yards
    team_receiving_yard_share = if_else(
      team_receiving_yards > 0,
      receiving_yards / team_receiving_yards,
      NA_real_
    ),
    
    # Player rank across entire team
    team_target_rank = if_else(
      targets > 0,
      min_rank(desc(targets)),
      NA_integer_
    ),
    
    team_receiving_yard_rank = if_else(
      receiving_yards > 0,
      min_rank(desc(receiving_yards)),
      NA_integer_
    ),
    
    
    # -------------------------
    # Rushing
    # -------------------------
    
    team_carries = sum(carries, na.rm = TRUE),
    
    team_rushing_yards = sum(rushing_yards, na.rm = TRUE),
    
    # % of total team rushing attempts
    team_carry_share = if_else(
      team_carries > 0,
      carries / team_carries,
      NA_real_
    ),
    
    # % of total team rushing yards
    team_rushing_yard_share = if_else(
      team_rushing_yards > 0,
      rushing_yards / team_rushing_yards,
      NA_real_
    ),
    
    # Player rank across entire team
    team_carry_rank = if_else(
      carries > 0,
      min_rank(desc(carries)),
      NA_integer_
    ),
    
    team_rushing_yard_rank = if_else(
      rushing_yards > 0,
      min_rank(desc(rushing_yards)),
      NA_integer_
    ),
    
    
    # -------------------------
    # Combined opportunities
    # -------------------------
    
    player_opportunities =
      coalesce(targets, 0) + coalesce(carries, 0),
    
    team_opportunities =
      sum(player_opportunities, na.rm = TRUE),
    
    team_opportunity_share = if_else(
      position %in% c("WR", "TE", "RB") &
        team_opportunities > 0,
      player_opportunities / team_opportunities,
      NA_real_
    )
    
  ) %>%
  ungroup() %>%
  
  
  
  # POSITIONAL RECEIVING COMPETITION
  
  group_by(season, team, position) %>%
  mutate(
    
    position_targets = sum(targets, na.rm = TRUE),
    
    position_target_share = if_else(
      position_targets > 0,
      targets / position_targets,
      NA_real_
    ),
    
    position_players_targeted =
      sum(targets > 0, na.rm = TRUE),
    
    position_target_concentration =
      sum(position_target_share^2, na.rm = TRUE),
    
    # Rank within position room by targets
    position_target_rank = if_else(
      targets > 0,
      min_rank(desc(targets)),
      NA_integer_
    )
    
  ) %>%
  ungroup() %>%
  
  
  
  # POSITIONAL RUSHING COMPETITION
  
  group_by(season, team, position) %>%
  mutate(
    
    position_carries = sum(carries, na.rm = TRUE),
    
    position_carry_share = if_else(
      position_carries > 0,
      carries / position_carries,
      NA_real_
    ),
    
    position_players_with_carries =
      sum(carries > 0, na.rm = TRUE),
    
    position_carry_concentration =
      sum(position_carry_share^2, na.rm = TRUE),
    
    # Rank within position room by carries
    position_carry_rank = if_else(
      carries > 0,
      min_rank(desc(carries)),
      NA_integer_
    )
    
  ) %>%
  ungroup() %>%
  
  
  
  # OVERALL POSITIONAL OPPORTUNITY
  
  group_by(season, team, position) %>%
  mutate(
    
    # Targets + carries
    position_opportunities =
      sum(player_opportunities, na.rm = TRUE),
    
    position_opportunity_share = if_else(
      position %in% c("WR", "TE", "RB") &
        position_opportunities > 0,
      player_opportunities / position_opportunities,
      NA_real_
    ),
    
    # Simple usage based depth ranking
    position_opportunity_rank = if_else(
      position %in% c("WR", "TE", "RB") &
        player_opportunities > 0,
      min_rank(desc(player_opportunities)),
      NA_integer_
    )
    
  ) %>%
  ungroup() %>%
  
  
  
  # QB COMPETITION
  
  group_by(season, team, position) %>%
  mutate(
    
    position_pass_attempts =
      sum(attempts, na.rm = TRUE),
    
    position_pass_attempt_share = if_else(
      position == "QB" &
        position_pass_attempts > 0,
      attempts / position_pass_attempts,
      NA_real_
    ),
    
    position_qbs_with_attempts = if_else(
      position == "QB",
      sum(attempts > 0, na.rm = TRUE),
      NA_integer_
    ),
    
    # QB rank based on passing attempts
    qb_attempt_rank = if_else(
      position == "QB" &
        attempts > 0,
      min_rank(desc(attempts)),
      NA_integer_
    )
    
  ) %>%
  ungroup()



##############
# NEXT-GEN STATS
##############

ng_passing = read_csv("passing_season_ngs.csv") %>% rename(team = team_abbr,
                                                           player_id = player_gsis_id) %>% mutate(clean_player_name = standardize_name(player_display_name))
ng_rushing = read_csv("rushing_season_ngs.csv")%>% rename(team = team_abbr,
                                                          player_id = player_gsis_id,
                                                          run_efficiency = efficiency,
                                                          yards_per_carry = avg_rush_yards) %>% mutate(clean_player_name = standardize_name(player_display_name))
ng_receiving = read_csv("receiving_season_ngs.csv")%>% rename(team = team_abbr,
                                                              player_id = player_gsis_id,
                                                              avg_intended_air_yards_receiving = avg_intended_air_yards,
                                                              percent_share_of_intended_air_yards_receiving = percent_share_of_intended_air_yards) %>% mutate(clean_player_name = standardize_name(player_display_name))


# adding passing
season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features_ngstats = 
  season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features %>% left_join(
    ng_passing[, c("season", "avg_time_to_throw", "avg_completed_air_yards", "avg_intended_air_yards", "aggressiveness", "max_completed_air_distance", "passer_rating", "completion_percentage_above_expectation", "player_id")],
    by = c( "season", "player_id")
  )


# Rushing
season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features_ngstats = 
  season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features_ngstats %>% left_join(
    ng_rushing[, c("season", "player_id", "run_efficiency", "percent_attempts_gte_eight_defenders", "avg_time_to_los", 
                   "yards_per_carry", "expected_rush_yards", "rush_yards_over_expected", "rush_yards_over_expected_per_att")],
    by = c( "season", "player_id")
  )



# Receiving 

season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features_ngstats = 
  season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features_ngstats %>% left_join(
    ng_receiving[, c("season", "player_id","avg_cushion", "avg_separation", "avg_intended_air_yards_receiving", "percent_share_of_intended_air_yards_receiving", "avg_yac", "avg_expected_yac", "avg_yac_above_expectation")],
    by = c( "season", "player_id")
  )





########
# WHATS MISSING?
########

# I want to add simple coach/OC flags (new head coach/ new OC flags)
## COLLECTED ** ProFootballFocus **

# offensive line ratings ( dont feel like paying for pff )
## Proxies, sack rate, run win rate, ...

# Redzone targets / goaline rush attempts, Team Redzone Stats
# ** COLLECTED football Ref** , ** COLLECTED https://www.teamrankings.com/nfl/stat/red-zone-scoring-pct?date=2019-02-04 **

# Add injury games missed/ missed games ( assume any missed game is an injury? Use injury reports? )
## Somehow count the number of time a player was questionable/doubtfull/out during the szn.

# Seperate Rookie model (using college stats, combine, etc)



############
# LOAD AND MERGE RZ STATS (PLAYER AND TEAM)
###########

rz = read.csv("redzone_Team_clean.csv")

# keeping only the rows i really want for modeling
rz = rz %>%
  select(
    Season,
    team,
    draft_season,
    # Red zone raw volume
    Team_rz_rush_att,
    Team_rz_pass_att,
    # Red zone scoring
    Team_rz_rush_TDs,
    Team_rz_pass_TDs,
    # Inside 10
    Team_i10_rush_att,
    Team_i10_pass_att,
    # Inside 5 rushing
    Team_i5_rush_att,
    Team_i5_rush_TDs,
    # Engineered play totals
    rz_plays,
    rz_tds,
    i10_plays,
    # Team tendencies
    rz_run_pass_split,
    i10_run_pass_split,
    # Scoring tendency / efficiency
    rz_rush_td_share,
    rz_rushTD_rate,
    gl_rushTD_rate,
    # Goal-line concentration
    i5_rz_rush_share
  ) %>% 
  mutate( 
    team = if_else(team == "LAR", "LA", team) # fixing Rams Abbr error
)


season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features_ngstats_rz = season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features_ngstats %>%
  left_join(
  rz,
  by = c("draft_season", "team")
)


# joining individual player stats (used gemini here to clean up overlapping variable names)


# Prefix metric columns prior to join
rz_passing <- read_csv("Redzone_Passing.csv") %>% 
  select(clean_player_name, Season, team, Inside_20_Att, `Inside_20_Cmp%`, Inside_20_TD, Inside_10_Att, `Inside_10_Cmp%`, Inside_10_TD) %>% 
  rename(season = Season) %>% 
  rename_with(~ paste0("pass_", .x), -c(clean_player_name, season, team)) %>% mutate(team = if_else(team == "LAR", "LA", team)) # fixing abbr mistake

rz_rushing <- read_csv("Redzone_Rushing.csv") %>% 
  select(clean_player_name, Season, team, Inside_20_Att, Inside_20_TD, `Inside_20_%Rush`, Inside_5_Att, Inside_5_TD, `Inside_5_%Rush`) %>% 
  rename(season = Season) %>% 
  rename_with(~ paste0("rush_", .x), -c(clean_player_name, season, team)) %>% mutate(team = if_else(team == "LAR", "LA", team))

rz_receiving <- read_csv("Redzone_Receiving.csv") %>% 
  select(clean_player_name, Season, Team, Inside_20_Tgt, `Inside_20_Ctch%`, Inside_20_TD, `Inside_20_%Tgt`, Inside_10_Tgt, `Inside_10_Ctch%`, Inside_10_TD, `Inside_10_%Tgt`) %>% 
  rename(season = Season, team = Team) %>% 
  rename_with(~ paste0("rec_", .x), -c(clean_player_name, season, team)) %>% mutate(team = if_else(team == "LAR", "LA", team))


# Clean 1-pass join
season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features_ngstats_rz <- season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features_ngstats_rz %>%
  left_join(rz_passing,   by = c("clean_player_name", "season", "team")) %>%
  left_join(rz_rushing,   by = c("clean_player_name", "season", "team")) %>%
  left_join(rz_receiving, by = c("clean_player_name", "season", "team"))


############
# LOAD AND MERGE COACHES + COACHING CHANGE FLAGS
###########
# coaches are based off of who had the job for week 1 of each season. mid season coaching changes are not considered here, and only at the start of the next szn/for the next draft season

#loading and rearranging the data for flagging
coaches = read.csv("NFL_HC_OC_2016_2026.csv" ) %>% arrange(team) %>% rename(draft_season = season)

# using chat to help me flag HC OC changes 
coaches = coaches %>%
  group_by(team) %>%
  arrange(draft_season, .by_group = TRUE) %>%
  rename(hc = head_coach,
         oc = offensive_coordinator) %>%
  mutate(
    new_hc = if_else(
      hc != lag(hc),
      1L,
      0L,
      missing = 0L
    ),
    
    new_oc = if_else(
      oc != lag(oc),
      1L,
      0L,
      missing = 0L
    )
  ) %>%
  ungroup()

write_csv(coaches, "HCs_OCs.csv")



season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features_ngstats_rz_coaches = season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features_ngstats_rz %>%
  left_join(
    coaches[,c("draft_season", "team", "hc", "oc","new_hc", "new_oc")],
    by = c("draft_season", "team")
  )





############
# LOAD Injury reports -- Count missed games (not injury missed games), weeks as doubtful/out (maybe doubtful+out / game weeks)
###########

# loading and filtering down to regular szn and positions i care about
injuries = read.csv("injuries.csv") %>% filter(game_type == "REG" & position %in% c("RB", "FB", "WR", "TE", "QB" 
                                                                                  ))

# the goal is going to be to count the number of weeks a player appears on the injury report as doubtful or out (players often appear questionable and still play)

injury_features = injuries %>%
  filter( report_status %in% c("Doubtful", "Out")) %>%
  group_by( season, gsis_id) %>%
  summarise(
    doubtful_or_out_weeks = n_distinct(week),
    .groups = "drop"
  ) %>% rename(player_id = gsis_id)


season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features_ngstats_rz_coaches_inj = season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features_ngstats_rz_coaches %>%
  left_join(
    injury_features,
    by = c("season", "player_id")
  )

# NAs to 0s (no injury reports/injuries)
season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features_ngstats_rz_coaches_inj$doubtful_or_out_weeks[is.na(season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features_ngstats_rz_coaches_inj$doubtful_or_out_weeks)] = 0




############
# CREATING TREND FEATURES - lag 1 and 2 difference (how much did they improve from 2 years ago to 1 year ago, trajectory) target/rush/TD shares, PPR points, etc
## maybe total career points, games played, etc
###########

master_season_df =
  season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features_ngstats_rz_coaches_inj %>% # I completely forgot to merge fantasy opportunity stats earlier so doing it now. 
  left_join(
    season_opportunity_stats,
    by = c(
      "season",
      "player_id"
    )
  ) %>%
  subset(select = -c(Season))

#master_season_df = season_stats_roster_standings_teamstats_ffranks_snapshare_competition_features_ngstats_rz_coaches_inj %>% subset(select = -c(Season))

# addressing NAs and noticed data issues 
master_season_df$years_exp[is.na(master_season_df$years_exp)] = 0

master_season_df$draft_number[is.na(master_season_df$draft_number)] = 999 # undrafted flag essentially

# filling 0 opportunity share for QBs
master_season_df$team_opportunity_share[
  is.na(master_season_df$team_opportunity_share) &
    master_season_df$position == "QB"
] = 0


# Lagging and taking year-to-year +/- change
master_season_df = master_season_df %>%
  arrange(player_id, season) %>%
  group_by(player_id) %>%
  mutate(
    previous_season = lag(season),
    
    fantasy_ppg_prev = if_else(
      season - previous_season == 1,
      lag(fantasy_ppg),
      NA_real_
    ),
    
    target_share_prev = if_else(
      season - previous_season == 1,
      lag(team_target_share),
      NA_real_
    ),
    
    carry_share_prev = if_else(
      season - previous_season == 1,
      lag(team_carry_share),
      NA_real_
    ),
    
    snap_share_prev = if_else(
      season - previous_season == 1,
      lag(avg_weekly_off_snapshare),
      NA_real_
    ),
    
    fantasy_ppg_change_1yr =
      fantasy_ppg - fantasy_ppg_prev,
    
    target_share_change_1yr =
      team_target_share - target_share_prev,
    
    carry_share_change_1yr =
      team_carry_share - carry_share_prev,
    
    snap_share_change_1yr =
      avg_weekly_off_snapshare - snap_share_prev,
    
  ) %>%
  ungroup()

# adding missed games (forgot earlier)
master_season_df = master_season_df %>%
  mutate(
    season_games = case_when(
      season >= 2016 & season <= 2020 ~ 16,
      season >= 2021 & season <= 2025 ~ 17,
      TRUE ~ NA_real_
    ),
    
    games_missed = season_games - games_played
  )



############################
# ALIGN PLAYER TO DRAFT TEAM
############################

# Current `team` = team player played for during completed season
master_season_df = master_season_df %>%
  rename(prev_team = team)


############################
# DRAFT TEAM LOOKUP
############################

# Historical seasons:
# Use player's earliest observed team in the season
# as a proxy for their team entering that fantasy season.

historical_draft_team_lookup = player_weekly %>%
  filter(
    season %in% 2017:2025,
    !is.na(player_id),
    player_id != "",
    !is.na(team)
  ) %>%
  arrange(
    season,
    player_id,
    week
  ) %>%
  group_by(
    season,
    player_id
  ) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    player_id,
    draft_season = season,
    draft_team = team
  )


# 2026:
# Use current roster because this is the actual
# upcoming season we are predicting.

draft_team_2026 = roster %>%
  filter(season == 2026) %>%
  transmute(
    player_id = gsis_id,
    draft_season = season,
    draft_team = team
  ) %>%
  filter(
    !is.na(player_id),
    player_id != "",
    !is.na(draft_team)
  ) %>%
  distinct()


# Combine

draft_team_lookup = bind_rows(
  historical_draft_team_lookup,
  draft_team_2026
)

# check
draft_team_lookup %>%
  count(draft_season)


# Join future team
master_season_df = master_season_df %>%
  left_join(
    draft_team_lookup,
    by = c(
      "player_id",
      "draft_season"
    )
  ) %>%
  mutate(
    new_team = case_when(
      is.na(draft_team) ~ NA_integer_,
      draft_team != prev_team ~ 1L,
      TRUE ~ 0L
    )
  )

############################
# DRAFT TEAM ENVIRONMENT
############################

draft_team_environment = team_standings %>%
  select(
    season,
    team,
    pct,
    scored,
    allowed,
    net,
    sov,
    sos,
    playoff
  ) %>%
  mutate(
    team_made_playoffs = as.integer(!is.na(playoff))
  ) %>%
  rename(
    team_win_pct = pct,
    team_pts_scored = scored,
    team_pts_against = allowed,
    team_net_pts = net,
    team_sov = sov,
    team_sos = sos
  ) %>%
  left_join(
    team_stats_selected,
    by = c("season", "team")
  )

team_environment_cols = c(
  "team_win_pct",
  "team_pts_scored",
  "team_pts_against",
  "team_net_pts",
  "team_sov",
  "team_sos",
  "playoff",
  "team_made_playoffs",
  
  setdiff(
    names(team_stats_selected),
    c("season", "team")
  )
)

master_season_df = master_season_df %>%
  select(
    -any_of(team_environment_cols)
  )

master_season_df = master_season_df %>%
  left_join(
    draft_team_environment,
    by = c(
      "season",
      "draft_team" = "team"
    )
  )


# fixing redzone team features to match new team
team_rz_cols = setdiff(
  names(rz),
  c("Season", "team", "draft_season")
)

master_season_df = master_season_df %>%
  select(
    -any_of(team_rz_cols)
  ) %>%
  left_join(
    rz %>%
      select(-Season),
    by = c(
      "draft_season",
      "draft_team" = "team"
    )
  )


# fixing coaches to match new team
master_season_df = master_season_df %>%
  select(
    -any_of(c(
      "hc",
      "oc",
      "new_hc",
      "new_oc"
    ))
  ) %>%
  left_join(
    coaches %>%
      select(
        draft_season,
        team,
        hc,
        oc,
        new_hc,
        new_oc
      ),
    by = c(
      "draft_season",
      "draft_team" = "team"
    )
  )




############################
# VACATED TARGETS / CARRIES
############################

# Goal:
# Measure how much of each team's previous-season
# opportunity is no longer on the roster entering
# the next fantasy draft season.


# ==========================================
# 1. PLAYER-TEAM-SEASON USAGE
# ==========================================

# Use weekly stats so players who played for
# multiple teams are correctly split between teams.

player_team_usage = read_csv("player_weekly_stats.csv") %>%
  filter(
    !is.na(team),
    !is.na(player_id),
    player_id != ""
  ) %>%
  group_by(
    season,
    team,
    player_id,
    position
  ) %>%
  summarise(
    targets = sum(targets, na.rm = TRUE),
    receptions = sum(receptions, na.rm = TRUE),
    receiving_yards = sum(receiving_yards, na.rm = TRUE),
    
    carries = sum(carries, na.rm = TRUE),
    rushing_yards = sum(rushing_yards, na.rm = TRUE),
    
    attempts = sum(attempts, na.rm = TRUE),
    
    .groups = "drop"
  )


# ==========================================
# 2. PREVIOUS TEAM OPPORTUNITY TOTALS
# ==========================================

# IMPORTANT:
# Calculate these from player_team_usage rather than
# master_season_df so traded players are included
# with the correct team.

prev_team_opportunity = player_team_usage %>%
  group_by(
    season,
    team
  ) %>%
  summarise(
    prev_team_targets = sum(
      targets,
      na.rm = TRUE
    ),
    
    prev_team_carries = sum(
      carries,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  rename(
    prev_team = team
  )


# Check: one row per team-season
prev_team_opportunity %>%
  count(
    season,
    prev_team
  ) %>%
  filter(n > 1)


# ==========================================
# 3. DETERMINE WHETHER EACH PLAYER RETURNED
# ==========================================

# draft_team_lookup should represent the team
# the player was on entering the next season
# (Week 1 historically, current roster for 2026).

vacated_base = player_team_usage %>%
  select(
    season,
    prev_team = team,
    player_id,
    targets,
    carries
  ) %>%
  mutate(
    draft_season = season + 1
  ) %>%
  left_join(
    draft_team_lookup %>%
      rename(next_team = draft_team),
    by = c(
      "player_id",
      "draft_season"
    ),
    relationship = "many-to-one"
  ) %>%
  mutate(
    returning_next_year =
      !is.na(next_team) &
      next_team == prev_team
  )


# check
vacated_base %>%
  summarise(
    rows = n(),
    missing_next_team = sum(is.na(next_team)),
    pct_missing =
      mean(is.na(next_team))
  )


# ==========================================
# 4. RETURNING OPPORTUNITY
# ==========================================

returning_stats = vacated_base %>%
  group_by(
    season,
    prev_team
  ) %>%
  summarise(
    returning_targets = sum(
      targets[returning_next_year],
      na.rm = TRUE
    ),
    
    returning_carries = sum(
      carries[returning_next_year],
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )


# ==========================================
# 5. CALCULATE VACATED OPPORTUNITY
# ==========================================

vacated_team_features = prev_team_opportunity %>%
  left_join(
    returning_stats,
    by = c(
      "season",
      "prev_team"
    ),
    relationship = "one-to-one"
  ) %>%
  mutate(
    returning_targets =
      coalesce(returning_targets, 0),
    
    returning_carries =
      coalesce(returning_carries, 0),
    
    
    # Raw vacated opportunities
    team_vacated_targets =
      prev_team_targets -
      returning_targets,
    
    team_vacated_carries =
      prev_team_carries -
      returning_carries,
    
    
    # Percentage vacated
    team_vacated_target_share =
      if_else(
        prev_team_targets > 0,
        team_vacated_targets /
          prev_team_targets,
        0
      ),
    
    team_vacated_carry_share =
      if_else(
        prev_team_carries > 0,
        team_vacated_carries /
          prev_team_carries,
        0
      ),
    
    
    # Align with upcoming fantasy season
    draft_season = season + 1,
    draft_team = prev_team
  ) %>%
  select(
    draft_season,
    draft_team,
    team_vacated_targets,
    team_vacated_carries,
    team_vacated_target_share,
    team_vacated_carry_share
  )


# ==========================================
# 6. QUALITY CHECKS
# ==========================================

# Must be one row per team/draft season
vacated_team_features %>%
  count(
    draft_season,
    draft_team
  ) %>%
  filter(n > 1)


# Shares should always be between 0 and 1
vacated_team_features %>%
  filter(
    team_vacated_target_share < 0 |
      team_vacated_target_share > 1 |
      team_vacated_carry_share < 0 |
      team_vacated_carry_share > 1
  )


# Raw vacated usage should never be negative
vacated_team_features %>%
  filter(
    team_vacated_targets < 0 |
      team_vacated_carries < 0
  )


# ==========================================
# 7. JOIN ONTO MASTER DATASET
# ==========================================

master_season_df = master_season_df %>%
  left_join(
    vacated_team_features,
    by = c(
      "draft_season",
      "draft_team"
    ),
    relationship = "many-to-one"
  )


# ==========================================
# SAVE
# ==========================================

write_csv(
  master_season_df,
  "master_season_df.csv"
)



# DATASET COMPLETE (FOR NOW), TIME TO MODEL TOTAL PPR POINTS, PPR PPG (17g), THEN predict games missed, THEN adjust prediction using estimated games, THEN BUILD OUT DRAFT BOARD

############################
# FINAL MODELING DATA CHECKS
############################

# Must be one row per player-season
master_season_df %>%
  count(season, player_id) %>%
  filter(n > 1)

# Vacated opportunity bounds
master_season_df %>%
  filter(
    team_vacated_target_share < 0 |
      team_vacated_target_share > 1 |
      team_vacated_carry_share < 0 |
      team_vacated_carry_share > 1
  )

# Review unmatched future teams
master_season_df %>%
  filter(is.na(draft_team)) %>%
  count(draft_season, position)

# Spot check offseason movers
master_season_df %>%
  filter(new_team == 1) %>%
  select(
    player_display_name,
    season,
    draft_season,
    prev_team,
    draft_season,
    draft_team,
    ecr,
    team_win_pct,
    team_pass_rate,
    team_rush_rate,
    team_vacated_target_share,
    team_vacated_carry_share,
    hc,
    oc
  ) %>%
  arrange(desc(draft_season))





# All data collected and compiled here is from the NFLverse, Football Reference, and teamrankings.com


























## WEEKLY DATA  ( for later, week-to-week projections. Start-Sit, boom-bust, daily fantasy applications)







