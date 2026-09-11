# My goal is to create 3 different models, one for each stat/position group (rushing, receiving, QB).
# I will split players by Position, RBs (rushing and receiving stats), 
# WR/TE (receiving stats, select rushing stats for gadget players), QBs (passing stats)

# My target is to predict Total PPR fantasy points (calculate PPG, assuming 17 games player)
## I will later make expected games predictions (or use someone elses expected games predictions) to recalculate PPR PPG



# adjusting the data to make the "Target" variable
## data is currently set up so that the players fantasy production is the result of the "previous season" data.
## I want to predict the NEXT seasons fantasy production, meaning i need to shift total PPR pts backward 1 year to make it my target


#########
# Reading in the master data set
########

library(dplyr)
library(readr)

master_df = read_csv("master_season_df.csv") %>%
  select(
    -any_of(c(
      "Unnamed: 0",
      "...1",
      "birth_date",
      "clean_player_name",
      "full_name"
    ))
  )


#########
# Creating Future Season Target
#########

# Create one outcome row per player-season
target_lookup = master_df %>%
  group_by(player_id, season) %>%
  summarise(
    target_games_played = first(games_played),
    target_ppr_actual = first(fantasy_points_ppr_total),
    target_fantasy_ppg = first(fantasy_ppg),
    .groups = "drop"
  ) %>%
  rename(target_season = season)


# Attach next-season production to the previous-season feature row
model_df = master_df %>%
  left_join(
    target_lookup,
    by = c(
      "player_id",
      "draft_season" = "target_season"
    )
  ) %>%
  mutate(
    
    # Target assuming the player plays 17 games
    target_ppr_17 =
      target_fantasy_ppg * 17,
    
    dataset_type = case_when(
      draft_season == 2026 ~ "prediction",
      !is.na(target_ppr_17) ~ "training",
      TRUE ~ "no_next_season_data"
    )
  )



#########
# STANDARDIZING BASIC STATS ON A PER GAME BASIS
#########

model_df = model_df %>%
  mutate(
    
    # Rushing
    carries_pg = if_else(games_played > 0, carries / games_played, 0),
    rushing_yards_pg = if_else(games_played > 0, rushing_yards / games_played, 0),
    rushing_tds_pg = if_else(games_played > 0, rushing_tds / games_played, 0),
    
    # Receiving
    targets_pg = if_else(games_played > 0, targets / games_played, 0),
    receptions_pg = if_else(games_played > 0, receptions / games_played, 0),
    receiving_yards_pg = if_else(games_played > 0, receiving_yards / games_played, 0),
    receiving_tds_pg = if_else(games_played > 0, receiving_tds / games_played, 0),
    
    # Passing
    attempts_pg = if_else(games_played > 0, attempts / games_played, 0),
    passing_yards_pg = if_else(games_played > 0, passing_yards / games_played, 0),
    passing_tds_pg = if_else(games_played > 0, passing_tds / games_played, 0),
    interceptions_pg = if_else(games_played > 0, interceptions / games_played, 0)
  )

# expected rushing /rec tds per game played
model_df = model_df %>%
  mutate(
    rush_td_exp_pg = if_else(
      games_with_opportunity_data > 0,
      rush_td_exp / games_with_opportunity_data,
      0
    ),
    
    rec_td_exp_pg = if_else(
      games_with_opportunity_data > 0,
      rec_td_exp / games_with_opportunity_data,
      0
    ),
    
    total_fp_over_exp_pg = if_else(
      games_with_opportunity_data > 0,
      total_fp_over_exp / games_with_opportunity_data,
      0
    )
  )

# making changes -- adding starter flag, interactions

############################################
# RB EXPECTED STARTER FEATURES
############################################

library(stringr)


############################
# NAME CLEANING
############################

normalize_rb_name = function(x) {
  
  x %>%
    str_replace_all("\u00A0", " ") %>%
    str_to_lower() %>%
    str_squish() %>%
    
    # Remove suffix differences
    str_replace_all(
      "\\b(jr|sr|ii|iii|iv)\\b",
      ""
    ) %>%
    
    str_replace_all(
      "[^a-z0-9]",
      ""
    )
}


############################################
# HISTORICAL STARTERS
# 2017-2025
############################################

hist_rb_starters_raw =
  read_csv(
    "hist_rb_starters.csv"
  ) %>%
  
  filter(
    draft_season %in% 2017:2025,
    expected_rb1 == 1,
    !is.na(team),
    !is.na(player_display_name)
  ) %>%
  
  transmute(
    
    draft_season =
      as.integer(
        draft_season
      ),
    
    draft_team =
      team,
    
    expected_starter_name =
      player_display_name
  )


############################################
# MANUAL FIXES FOR DUPLICATE /
# MISSING HISTORICAL TEAM-SEASONS
############################################

rb_starter_overrides =
  tribble(
    
    ~draft_season, ~draft_team, ~expected_starter_name,
    
    # 2017
    2017, "GB",  "Ty Montgomery",
    2017, "MIA", "Jay Ajayi",
    2017, "TB",  "Jacquizz Rodgers",
    
    # 2019
    2019, "HOU", "Carlos Hyde",
    2019, "KC",  "Damien Williams",
    
    # 2020
    2020, "KC",  "Clyde Edwards-Helaire",
    2020, "PIT", "James Conner",
    2020, "LA",  "Malcolm Brown",
    
    # 2021
    2021, "HOU", "Mark Ingram",
    
    # 2022
    2022, "BAL", "Kenyan Drake",
    2022, "KC",  "Clyde Edwards-Helaire",
    
    # 2023
    2023, "WAS", "Brian Robinson"
  )


############################################
# REMOVE PROBLEM ROWS AND REPLACE
############################################

override_keys =
  rb_starter_overrides %>%
  select(
    draft_season,
    draft_team
  )


hist_rb_starters_clean =
  hist_rb_starters_raw %>%
  
  anti_join(
    override_keys,
    by =
      c(
        "draft_season",
        "draft_team"
      )
  ) %>%
  
  bind_rows(
    rb_starter_overrides
  ) %>%
  
  distinct(
    draft_season,
    draft_team,
    .keep_all = TRUE
  )


############################################
# 2026 MANUAL EXPECTED STARTERS
############################################

rb_starters_2026 =
  read_csv(
    "rb_expected_starters.csv"
  ) %>%
  
  transmute(
    
    draft_season =
      2026L,
    
    draft_team =
      team,
    
    expected_starter_name =
      player_display_name,
    
    # Keep this for your board.
    # Do NOT use in the model yet because
    # we do not have historical committee labels.
    rb_committee_2026 =
      as.integer(
        committee
      )
  )


############################################
# ADD EMPTY COMMITTEE COLUMN TO HISTORY
############################################

hist_rb_starters_clean =
  hist_rb_starters_clean %>%
  mutate(
    rb_committee_2026 =
      NA_integer_
  )


############################################
# FINAL 2017-2026 LOOKUP
############################################

rb_starter_lookup =
  bind_rows(
    hist_rb_starters_clean,
    rb_starters_2026
  ) %>%
  
  mutate(
    
    expected_starter_key =
      normalize_rb_name(
        expected_starter_name
      )
  )



############################################
# LOOKUP QUALITY CHECK
############################################

rb_starter_coverage =
  rb_starter_lookup %>%
  
  group_by(
    draft_season
  ) %>%
  
  summarise(
    
    teams =
      n_distinct(
        draft_team
      ),
    
    rows =
      n(),
    
    .groups =
      "drop"
  )


rb_starter_coverage

############################################
# CHECK FOR DUPLICATE TEAM-SEASONS
############################################

rb_starter_duplicates =
  rb_starter_lookup %>%
  count(
    draft_season,
    draft_team
  ) %>%
  filter(
    n != 1
  )

rb_starter_duplicates



############################################
# JOIN RB STARTER INFORMATION TO MODEL_DF
############################################

model_df =
  model_df %>%
  
  # Create comparable player name key
  mutate(
    player_name_key =
      normalize_rb_name(
        player_display_name
      )
  ) %>%
  
  # Join by UPCOMING season and team
  left_join(
    
    rb_starter_lookup %>%
      select(
        draft_season,
        draft_team,
        expected_starter_name,
        expected_starter_key,
        rb_committee_2026
      ),
    
    by =
      c(
        "draft_season",
        "draft_team"
      )
  ) %>%
  
  mutate(
    
    ########################################
    # EXPECTED STARTING RB FLAG
    ########################################
    
    expected_rb1 =
      as.integer(
        
        position == "RB" &
          
          !is.na(
            expected_starter_key
          ) &
          
          player_name_key ==
          expected_starter_key
      ),
    
    
    ########################################
    # STARTER × VACATED CARRY SHARE
    ########################################
    
    rb1_x_vacated_carry_share =
      expected_rb1 *
      coalesce(
        team_vacated_carry_share,
        0
      ),
    
    
    ########################################
    # STARTER × VACATED TARGET SHARE
    ########################################
    
    rb1_x_vacated_target_share =
      expected_rb1 *
      coalesce(
        team_vacated_target_share,
        0
      ),
    
    
    ########################################
    # STARTER × NEW TEAM
    ########################################
    
    rb1_x_new_team =
      expected_rb1 *
      coalesce(
        new_team,
        0
      )
  )



# check
############################################
# CHECK HISTORICAL STARTER MATCHES
############################################

rb_historical_match_check =
  model_df %>%
  filter(
    position == "RB",
    draft_season %in% 2017:2025
  ) %>%
  group_by(
    draft_season,
    draft_team
  ) %>%
  summarise(
    matched_starters =
      sum(
        expected_rb1,
        na.rm = TRUE
      ),
    .groups = "drop"
  ) %>%
  
  right_join(
    rb_starter_lookup %>%
      filter(
        draft_season %in% 2017:2025
      ) %>%
      select(
        draft_season,
        draft_team,
        expected_starter_name
      ),
    
    by =
      c(
        "draft_season",
        "draft_team"
      )
  ) %>%
  
  mutate(
    matched_starters =
      coalesce(
        matched_starters,
        0L
      )
  ) %>%
  
  filter(
    matched_starters != 1
  )


rb_historical_match_check # misses make sense, players who didnt have data for the prev year (CMC, Marshawn lynch retirned/unretired,...)

############################################
# CHECK 2026 STARTER MATCHES
############################################

rb_2026_match_check =
  model_df %>%
  filter(
    position == "RB",
    draft_season == 2026
  ) %>%
  group_by(
    draft_team
  ) %>%
  summarise(
    
    expected_starter_name =
      first(
        expected_starter_name
      ),
    
    matched_starters =
      sum(
        expected_rb1,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  ) %>%
  
  filter(
    matched_starters != 1
  )


rb_2026_match_check



write.csv(model_df, "season_model_df.csv")

#########
# Columns needed for tracking / evaluation
#########

id_cols = c(
  "player_id",
  "player_display_name",
  "position",
  "season",
  "draft_season",
  "prev_team",
  "draft_team",
  "dataset_type"
)

target_cols = c(
  "target_games_played",
  "target_ppr_actual",
  "target_fantasy_ppg",
  "target_ppr_17"
)





#########
# RB Predictors
#########

rb_features = c(
  
  # Player profile
  "age_at_season_start",
  "years_exp",
  "draft_number",
  "height",
  "weight",
  
  # Previous fantasy production
  "games_played",
  "fantasy_ppg",
  "avg_weekly_off_snapshare",
  "games_missed",
  
  # Rushing volume
  "carries_pg",
  "rushing_yards_pg",
  "rushing_tds_pg",
  "rushing_yards_per_carry",
  
  # Receiving volume
  "targets_pg",
  "receptions_pg",
  "receiving_yards_pg",
  "receiving_tds_pg",
  "catch_rate",
  
  # Expected fantasy opportunity
  "total_xfp_pg",
  "rush_xfp_pg",
  "rec_xfp_pg",
  "rush_td_exp_pg",
  "rec_td_exp_pg",
  
  # Opportunity conversion
  "total_fp_over_exp_pg",
  
  # Usage
  "team_carry_share",
  "team_target_share",
  "team_opportunity_share",
  "position_carry_share",
  
  # Trajectory
  "fantasy_ppg_change_1yr",
  "carry_share_change_1yr",
  "target_share_change_1yr",
  
  # Team environment
  "team_rush_rate",
  "team_carries_per_game",
  "team_rushing_tds_per_game",
  "team_plays_per_game",
  "position_target_concentration",
  "team_rushing_epa",
  
  # Upcoming situation
  "new_team",
  "new_hc",
  "new_oc",
  "team_vacated_carry_share",
  "team_vacated_target_share"
)


############################################
# SAVE ORIGINAL RB FEATURE SET
############################################

rb_features_original =
  rb_features


############################################
# ADD NEW STARTER FEATURES
############################################

rb_features_starter =
  unique(
    c(
      
      rb_features_original,
      
      # Is player expected Week 1 RB1?
      "expected_rb1",
      
      # Does expected RB1 have open carries?
      "rb1_x_vacated_carry_share",
      
      # Does expected RB1 have open targets?
      "rb1_x_vacated_target_share",
      
      # Is expected RB1 entering a new team?
      "rb1_x_new_team"
    )
  )







# creating the dataset
rb_df = model_df %>%
  filter(
    position == "RB",
    dataset_type != "no_next_season_data"
  ) %>%
  select(
    any_of(id_cols),
    any_of(rb_features_starter),
    any_of(target_cols)
  )

# removing 2026 players without a "draft_team" (not on a roster)
rb_df = rb_df %>% filter(!is.na(draft_team))

# filling NAs
rb_df$catch_rate[is.na(rb_df$catch_rate)] = 0
rb_df$avg_weekly_off_snapshare[is.na(rb_df$avg_weekly_off_snapshare)] = 0
rb_df$rushing_yards_per_carry[is.na(rb_df$rushing_yards_per_carry)] = 0

rb_df$fantasy_ppg_change_1yr[is.na(rb_df$fantasy_ppg_change_1yr)] = 0
rb_df$carry_share_change_1yr[is.na(rb_df$carry_share_change_1yr)] = 0
rb_df$target_share_change_1yr[is.na(rb_df$target_share_change_1yr)] = 0

rb_df$rush_xfp_pg[is.na(rb_df$rush_xfp_pg)] = 0
rb_df$rec_xfp_pg[is.na(rb_df$rec_xfp_pg)] = 0
rb_df$total_xfp_pg[is.na(rb_df$total_xfp_pg)] = 0

rb_df$rush_td_exp_pg[is.na(rb_df$rush_td_exp_pg)] = 0
rb_df$rec_td_exp_pg[is.na(rb_df$rec_td_exp_pg)] = 0
rb_df$total_fp_over_exp_pg[is.na(rb_df$total_fp_over_exp_pg)] = 0




rb_train = rb_df %>%
  filter(
    dataset_type == "training",
    !is.na(target_ppr_17)
  )

rb_predict_2026 = rb_df %>%
  filter(
    dataset_type == "prediction"
  )


#################
# Validation Process
################
# I am going to be using a rolling window validation starting with draft seasons
# 2017-2019 as training, 2020 as test. Expanding 1 year at a time and evaluating performance

validation_seasons = 2020:2025

rb_folds = lapply(
  validation_seasons,
  function(test_year) {
    
    train_fold = rb_train %>%
      filter(draft_season < test_year)
    
    test_fold = rb_train %>%
      filter(draft_season == test_year)
    
    list(
      test_year = test_year,
      train = train_fold,
      test = test_fold
    )
  }
)

names(rb_folds) = validation_seasons


rb_fold_check = bind_rows(
  lapply(
    rb_folds,
    function(x) {
      
      data.frame(
        test_year = x$test_year,
        
        first_train_year =
          min(x$train$draft_season),
        
        last_train_year =
          max(x$train$draft_season),
        
        n_train =
          nrow(x$train),
        
        n_test =
          nrow(x$test)
      )
    }
  )
)

rb_fold_check ## N_train grows each year, as expected 




# BASELINE PREDICTIONS (NAIVE)

rb_baseline_predictions = bind_rows(
  lapply(
    rb_folds,
    function(x) {
      
      x$test %>%
        transmute(
          player_id,
          player_display_name,
          position,
          
          season,
          draft_season,
          
          actual = target_ppr_17,
          
          prediction = fantasy_ppg * 17,
          
          model = "Previous Season PPG"
        )
    }
  )
)

#scoring funciton
calculate_metrics = function(data) {
  
  data = data %>%
    filter(
      !is.na(actual),
      !is.na(prediction),
      is.finite(actual),
      is.finite(prediction)
    )
  
  rmse =
    sqrt(
      mean(
        (data$actual - data$prediction)^2
      )
    )
  
  mae =
    mean(
      abs(data$actual - data$prediction)
    )
  
  r_squared =
    1 -
    sum(
      (data$actual - data$prediction)^2
    ) /
    sum(
      (data$actual - mean(data$actual))^2
    )
  
  spearman =
    cor(
      data$actual,
      data$prediction,
      method = "spearman"
    )
  
  data.frame(
    n = nrow(data),
    RMSE = rmse,
    MAE = mae,
    R_squared = r_squared,
    Spearman = spearman
  )
}

# evaluate by year
rb_baseline_by_year = rb_baseline_predictions %>%
  group_by(draft_season) %>%
  group_modify(
    ~ calculate_metrics(.x)
  ) %>%
  ungroup()

rb_baseline_by_year

# Results DF
rb_baseline_overall =
  calculate_metrics(
    rb_baseline_predictions
  )

rb_baseline_overall

# RMSE = 68.5 pts
# MAE = 51.2
# R^2 = .51
# Spearman = .75 -- Strong ranking agreement


#############
# RB ELASTIC NET
#############

library(dplyr)
library(purrr)
library(tibble)
library(glmnet)

# cleaning function

prepare_glmnet_data = function(
    train_data,
    test_data,
    features
) {
  
  # Only use features that actually exist
  features = intersect(
    features,
    names(train_data)
  )
  
  
  # Check for non-numeric variables
  non_numeric = features[
    !sapply(
      train_data[features],
      function(x) {
        is.numeric(x) ||
          is.integer(x) ||
          is.logical(x)
      }
    )
  ]
  
  if (length(non_numeric) > 0) {
    stop(
      paste(
        "Non-numeric predictors found:",
        paste(non_numeric, collapse = ", ")
      )
    )
  }
  
  
  # Pull predictor data
  train_x = train_data %>%
    select(all_of(features)) %>%
    mutate(
      across(
        everything(),
        as.numeric
      )
    )
  
  test_x = test_data %>%
    select(all_of(features)) %>%
    mutate(
      across(
        everything(),
        as.numeric
      )
    )
  
  
  # Replace Inf / -Inf with NA
  train_x = train_x %>%
    mutate(
      across(
        everything(),
        ~ ifelse(is.finite(.x), .x, NA_real_)
      )
    )
  
  test_x = test_x %>%
    mutate(
      across(
        everything(),
        ~ ifelse(is.finite(.x), .x, NA_real_)
      )
    )
  
  
  # Drop variables that are completely missing
  # in THIS training window
  usable_features = names(train_x)[
    sapply(
      train_x,
      function(x) any(!is.na(x))
    )
  ]
  
  train_x = train_x %>%
    select(all_of(usable_features))
  
  test_x = test_x %>%
    select(all_of(usable_features))
  
  
  # Remove zero-variance predictors
  predictor_sd = sapply(
    train_x,
    sd
  )
  
  kept_features = names(
    predictor_sd[
      is.finite(predictor_sd) &
        predictor_sd > 0
    ]
  )
  
  
  train_x = train_x %>%
    select(all_of(kept_features))
  
  test_x = test_x %>%
    select(all_of(kept_features))
  
  
  list(
    x_train = as.matrix(train_x),
    x_test = as.matrix(test_x),
    kept_features = kept_features
  )
}







# HYPERPARAMENT TUNING 
tune_elastic_temporal = function(
    train_data,
    features,
    target = "target_ppr_17",
    alpha_grid = c(0, .25, .5, .75, 1),
    lambda_grid = 10^seq(2.5, -3, length.out = 75)
) {
  
  available_years =
    sort(
      unique(
        train_data$draft_season
      )
    )
  
  
  # Need at least 2 seasons before first
  # inner validation season
  validation_years =
    available_years[
      available_years >= min(available_years) + 2
    ]
  
  
  tuning_results = list()
  
  counter = 1
  
  
  for (validation_year in validation_years) {
    
    inner_train = train_data %>%
      filter(
        draft_season < validation_year
      )
    
    inner_validation = train_data %>%
      filter(
        draft_season == validation_year
      )
    
    
    prepared = prepare_glmnet_data(
      train_data = inner_train,
      test_data = inner_validation,
      features = features
    )
    
    
    y_train =
      inner_train[[target]]
    
    y_validation =
      inner_validation[[target]]
    
    
    for (alpha_value in alpha_grid) {
      
      fit = glmnet(
        x = prepared$x_train,
        y = y_train,
        
        family = "gaussian",
        
        alpha = alpha_value,
        lambda = lambda_grid,
        
        standardize = TRUE
      )
      
      
      predictions = predict(
        fit,
        newx = prepared$x_test,
        s = lambda_grid
      )
      
      
      # RMSE for every lambda
      rmse_values = apply(
        predictions,
        2,
        function(pred) {
          
          sqrt(
            mean(
              (y_validation - pred)^2,
              na.rm = TRUE
            )
          )
        }
      )
      
      
      tuning_results[[counter]] =
        tibble(
          validation_year =
            validation_year,
          
          alpha =
            alpha_value,
          
          lambda =
            lambda_grid,
          
          RMSE =
            rmse_values
        )
      
      counter = counter + 1
    }
  }
  
  
  tuning_results =
    bind_rows(tuning_results)
  
  
  # Average performance across historical
  # validation years
  tuning_summary =
    tuning_results %>%
    group_by(
      alpha,
      lambda
    ) %>%
    summarise(
      mean_RMSE =
        mean(RMSE, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    arrange(mean_RMSE)
  
  
  best_parameters =
    tuning_summary %>%
    slice(1)
  
  
  list(
    best_alpha =
      best_parameters$alpha,
    
    best_lambda =
      best_parameters$lambda,
    
    tuning_summary =
      tuning_summary,
    
    yearly_results =
      tuning_results
  )
}


# FULL EXPANDING WINDOW ELASTIC NET FUNCTION

run_elastic_net_rolling = function(
    data,
    features,
    validation_seasons = 2020:2025,
    target = "target_ppr_17",
    alpha_grid = seq(
      0,
      1,
      by = 0.1
    ),
    lambda_grid = 10^seq(2.5, -3, length.out = 75)
) {
  
  all_predictions = list()
  all_tuning = list()
  
  
  for (test_year in validation_seasons) {
    
    cat(
      "\nRunning Elastic Net for",
      test_year,
      "\n"
    )
    
    
    # -------------------------
    # Outer training/test split
    # -------------------------
    
    train_fold = data %>%
      filter(
        draft_season < test_year
      )
    
    test_fold = data %>%
      filter(
        draft_season == test_year
      )
    
    
    # -------------------------
    # Tune Elastic Net using
    # historical training data
    # -------------------------
    
    tuning =
      tune_elastic_temporal(
        train_data = train_fold,
        features = features,
        target = target,
        alpha_grid = alpha_grid,
        lambda_grid = lambda_grid
      )
    
    
    best_alpha =
      tuning$best_alpha
    
    best_lambda =
      tuning$best_lambda
    
    
    cat(
      "Best alpha:",
      best_alpha,
      "\n"
    )
    
    cat(
      "Best lambda:",
      round(best_lambda, 5),
      "\n"
    )
    
    
    # -------------------------
    # Prepare outer split
    # -------------------------
    
    prepared =
      prepare_glmnet_data(
        train_data = train_fold,
        test_data = test_fold,
        features = features
      )
    
    
    # -------------------------
    # Fit final model for fold
    # -------------------------
    
    final_fit =
      glmnet(
        x =
          prepared$x_train,
        
        y =
          train_fold[[target]],
        
        family =
          "gaussian",
        
        alpha =
          best_alpha,
        
        lambda =
          lambda_grid,
        
        standardize =
          TRUE
      )
    
    
    # -------------------------
    # Predict unseen season
    # -------------------------
    
    predictions =
      as.numeric(
        predict(
          final_fit,
          newx =
            prepared$x_test,
          s =
            best_lambda
        )
      )
    
    
    # -------------------------
    # Save predictions
    # -------------------------
    
    all_predictions[[
      as.character(test_year)
    ]] =
      test_fold %>%
      transmute(
        player_id,
        player_display_name,
        position,
        season,
        draft_season,
        
        actual =
          .data[[target]],
        
        prediction =
          predictions,
        
        model =
          "Elastic Net",
        
        alpha =
          best_alpha,
        
        lambda =
          best_lambda,
        
        n_features =
          length(
            prepared$kept_features
          )
      )
    
    
    # Save tuning information
    all_tuning[[
      as.character(test_year)
    ]] =
      tuning$tuning_summary %>%
      mutate(
        outer_test_year =
          test_year
      )
  }
  
  
  list(
    predictions =
      bind_rows(all_predictions),
    
    tuning =
      bind_rows(all_tuning)
  )
}


# RB ELASTIC NET MODEL

rb_elastic_results =
  run_elastic_net_rolling(
    data = rb_train,
    features = rb_features,
    validation_seasons = 2020:2025
  )

rb_elastic_predictions =
  rb_elastic_results$predictions


rb_elastic_predictions %>%
  select(
    player_display_name,
    draft_season,
    actual,
    prediction,
    alpha,
    lambda
  ) %>%
  head(20)


# SCORING 

rb_elastic_by_year =
  rb_elastic_predictions %>%
  group_by(draft_season) %>%
  group_modify(
    ~ calculate_metrics(.x)
  ) %>%
  ungroup()

rb_elastic_by_year


rb_elastic_overall =
  calculate_metrics(
    rb_elastic_predictions
  )

rb_elastic_overall


# COMPARE

rb_model_comparison =
  bind_rows(
    rb_baseline_predictions,
    rb_elastic_predictions
  ) %>%
  group_by(model) %>%
  group_modify(
    ~ calculate_metrics(.x)
  ) %>%
  ungroup()

rb_model_comparison

# model                   n  RMSE   MAE R_squared Spearman
# Elastic Net           620  61.7  48.3     0.607    0.777
# Previous Season PPG   620  68.6  51.2     0.515    0.753





# which alpha/lambda performed best
rb_elastic_predictions %>%
  distinct(
    draft_season,
    alpha,
    lambda,
    n_features
  )



#############
# RB XGBoost
############
install.packages("xgboost")
library(xgboost)
library(dplyr)
library(purrr)
library(tibble)


# cleaning function

prepare_xgb_data = function(
    train_data,
    test_data,
    features
) {
  
  # Keep features that actually exist
  features = intersect(
    features,
    names(train_data)
  )
  
  
  # Check for non-numeric predictors
  non_numeric = features[
    !sapply(
      train_data[features],
      function(x) {
        is.numeric(x) ||
          is.integer(x) ||
          is.logical(x)
      }
    )
  ]
  
  if (length(non_numeric) > 0) {
    stop(
      paste(
        "Non-numeric predictors found:",
        paste(non_numeric, collapse = ", ")
      )
    )
  }
  
  
  # Predictor data
  train_x = train_data %>%
    select(all_of(features)) %>%
    mutate(
      across(
        everything(),
        as.numeric
      )
    )
  
  test_x = test_data %>%
    select(all_of(features)) %>%
    mutate(
      across(
        everything(),
        as.numeric
      )
    )
  
  
  # Turn Inf / -Inf into NA
  train_x = train_x %>%
    mutate(
      across(
        everything(),
        ~ ifelse(is.finite(.x), .x, NA_real_)
      )
    )
  
  test_x = test_x %>%
    mutate(
      across(
        everything(),
        ~ ifelse(is.finite(.x), .x, NA_real_)
      )
    )
  
  
  # Remove predictors with no usable values
  usable_features = names(train_x)[
    sapply(
      train_x,
      function(x) {
        any(!is.na(x))
      }
    )
  ]
  
  train_x = train_x %>%
    select(all_of(usable_features))
  
  test_x = test_x %>%
    select(all_of(usable_features))
  
  
  # Remove zero-variance features
  variable_features = names(train_x)[
    sapply(
      train_x,
      function(x) {
        
        values =
          unique(
            x[!is.na(x)]
          )
        
        length(values) > 1
      }
    )
  ]
  
  train_x = train_x %>%
    select(all_of(variable_features))
  
  test_x = test_x %>%
    select(all_of(variable_features))
  
  
  list(
    x_train = as.matrix(train_x),
    x_test = as.matrix(test_x),
    kept_features = variable_features
  )
}


# grid

xgb_grid = expand.grid(
  max_depth = c(2, 5, 10),
  learning_rate = c(0.03, 0.07),
  min_child_weight = c(1, 5, 10),
  subsample = 0.8,
  colsample_bytree = 0.8
)

nrow(xgb_grid)


# tuning
tune_xgb_temporal = function(
    train_data,
    features,
    target = "target_ppr_17",
    xgb_grid,
    nrounds_max = 1000,
    early_stopping_rounds = 50
) {
  
  available_years =
    sort(
      unique(
        train_data$draft_season
      )
    )
  
  
  # Require at least two earlier seasons
  validation_years =
    available_years[
      available_years >=
        min(available_years) + 2
    ]
  
  
  tuning_results = list()
  counter = 1
  
  
  for (validation_year in validation_years) {
    
    inner_train =
      train_data %>%
      filter(
        draft_season < validation_year
      )
    
    inner_validation =
      train_data %>%
      filter(
        draft_season == validation_year
      )
    
    
    prepared =
      prepare_xgb_data(
        train_data = inner_train,
        test_data = inner_validation,
        features = features
      )
    
    
    dtrain =
      xgb.DMatrix(
        data = prepared$x_train,
        label = inner_train[[target]],
        missing = NA
      )
    
    dvalidation =
      xgb.DMatrix(
        data = prepared$x_test,
        label = inner_validation[[target]],
        missing = NA
      )
    
    
    for (i in seq_len(nrow(xgb_grid))) {
      
      params = list(
        booster = "gbtree",
        objective = "reg:squarederror",
        eval_metric = "rmse",
        tree_method = "hist",
        
        max_depth =
          xgb_grid$max_depth[i],
        
        learning_rate =
          xgb_grid$learning_rate[i],
        
        min_child_weight =
          xgb_grid$min_child_weight[i],
        
        subsample =
          xgb_grid$subsample[i],
        
        colsample_bytree =
          xgb_grid$colsample_bytree[i],
        
        reg_lambda = 1,
        reg_alpha = 0
      )
      
      
      set.seed(57)
      
      
      fit =
        xgb.train(
          params = params,
          data = dtrain,
          nrounds = nrounds_max,
          
          evals = list(
            validation = dvalidation
          ),
          
          early_stopping_rounds =
            early_stopping_rounds,
          
          verbose = 0
        )
      
      
      ############################
      # GET EVALUATION LOG
      ############################
      
      evaluation_log =
        attr(
          fit,
          "evaluation_log"
        )
      
      
      # Backup for XGBoost versions
      # exposing it differently
      if (is.null(evaluation_log)) {
        
        evaluation_log =
          fit$evaluation_log
      }
      
      
      if (is.null(evaluation_log)) {
        
        stop(
          paste(
            "No evaluation log found for validation year",
            validation_year
          )
        )
      }
      
      
      ############################
      # FIND VALIDATION RMSE COLUMN
      ############################
      
      rmse_col =
        grep(
          "validation.*rmse",
          names(evaluation_log),
          value = TRUE,
          ignore.case = TRUE
        )
      
      
      if (length(rmse_col) == 0) {
        
        stop(
          paste(
            "Validation RMSE column not found.",
            "Columns available:",
            paste(
              names(evaluation_log),
              collapse = ", "
            )
          )
        )
      }
      
      
      rmse_col =
        rmse_col[1]
      
      
      ############################
      # FIND BEST ITERATION
      ############################
      
      best_index =
        which.min(
          evaluation_log[[rmse_col]]
        )
      
      
      # Use actual boosting iteration
      # if evaluation log contains it
      if ("iter" %in% names(evaluation_log)) {
        
        best_nrounds =
          as.integer(
            evaluation_log$iter[best_index]
          )
        
      } else {
        
        best_nrounds =
          as.integer(
            best_index
          )
      }
      
      
      best_rmse =
        as.numeric(
          evaluation_log[[rmse_col]][best_index]
        )
      
      
      ############################
      # SAFETY CHECK
      ############################
      
      if (
        length(best_nrounds) != 1 ||
        is.na(best_nrounds) ||
        best_nrounds < 1
      ) {
        
        stop(
          paste(
            "Invalid best_nrounds for",
            validation_year
          )
        )
      }
      
      
      if (
        length(best_rmse) != 1 ||
        is.na(best_rmse) ||
        !is.finite(best_rmse)
      ) {
        
        stop(
          paste(
            "Invalid RMSE for",
            validation_year
          )
        )
      }
      
      
      ############################
      # STORE RESULTS
      ############################
      
      tuning_results[[counter]] =
        tibble(
          validation_year =
            validation_year,
          
          max_depth =
            xgb_grid$max_depth[i],
          
          learning_rate =
            xgb_grid$learning_rate[i],
          
          min_child_weight =
            xgb_grid$min_child_weight[i],
          
          subsample =
            xgb_grid$subsample[i],
          
          colsample_bytree =
            xgb_grid$colsample_bytree[i],
          
          best_nrounds =
            best_nrounds,
          
          RMSE =
            best_rmse
        )
      
      
      counter =
        counter + 1
    }
  }
  
  
  tuning_results =
    bind_rows(
      tuning_results
    )
  
  
  ############################
  # SUMMARIZE HYPERPARAMETERS
  ############################
  
  tuning_summary =
    tuning_results %>%
    group_by(
      max_depth,
      learning_rate,
      min_child_weight,
      subsample,
      colsample_bytree
    ) %>%
    summarise(
      
      mean_RMSE =
        mean(
          RMSE,
          na.rm = TRUE
        ),
      
      median_nrounds =
        as.integer(
          round(
            median(
              best_nrounds,
              na.rm = TRUE
            )
          )
        ),
      
      .groups = "drop"
    ) %>%
    arrange(
      mean_RMSE
    )
  
  
  best_parameters =
    tuning_summary %>%
    slice(1)
  
  
  list(
    best_parameters =
      best_parameters,
    
    tuning_summary =
      tuning_summary,
    
    yearly_results =
      tuning_results
  )
}


# xgboost function

run_xgboost_rolling = function(
    data,
    features,
    validation_seasons = 2020:2025,
    target = "target_ppr_17",
    xgb_grid,
    nrounds_max = 1000,
    early_stopping_rounds = 50
) {
  
  all_predictions = list()
  all_tuning = list()
  selected_parameters = list()
  
  
  for (test_year in validation_seasons) {
    
    cat(
      "\nRunning XGBoost for",
      test_year,
      "\n"
    )
    
    
    ###########################
    # OUTER SPLIT
    ###########################
    
    train_fold = data %>%
      filter(
        draft_season < test_year
      )
    
    test_fold = data %>%
      filter(
        draft_season == test_year
      )
    
    
    ###########################
    # TEMPORAL TUNING
    ###########################
    
    tuning =
      tune_xgb_temporal(
        train_data =
          train_fold,
        
        features =
          features,
        
        target =
          target,
        
        xgb_grid =
          xgb_grid,
        
        nrounds_max =
          nrounds_max,
        
        early_stopping_rounds =
          early_stopping_rounds
      )
    
    
    best =
      tuning$best_parameters
    
    
    best_depth =
      best$max_depth
    
    best_learning_rate =
      best$learning_rate
    
    best_min_child_weight =
      best$min_child_weight
    
    best_subsample =
      best$subsample
    
    best_colsample =
      best$colsample_bytree
    
    best_nrounds =
      best$median_nrounds
    
    
    cat(
      "Depth:",
      best_depth,
      "| Learning rate:",
      best_learning_rate,
      "| Min child:",
      best_min_child_weight,
      "| Trees:",
      best_nrounds,
      "\n"
    )
    
    
    ###########################
    # PREPARE OUTER DATA
    ###########################
    
    prepared =
      prepare_xgb_data(
        train_data =
          train_fold,
        
        test_data =
          test_fold,
        
        features =
          features
      )
    
    
    dtrain =
      xgb.DMatrix(
        data =
          prepared$x_train,
        
        label =
          train_fold[[target]],
        
        missing =
          NA
      )
    
    
    dtest =
      xgb.DMatrix(
        data =
          prepared$x_test,
        
        missing =
          NA
      )
    
    
    ###########################
    # FINAL OUTER MODEL
    ###########################
    
    params = list(
      booster =
        "gbtree",
      
      objective =
        "reg:squarederror",
      
      eval_metric =
        "rmse",
      
      tree_method =
        "hist",
      
      max_depth =
        best_depth,
      
      learning_rate =
        best_learning_rate,
      
      min_child_weight =
        best_min_child_weight,
      
      subsample =
        best_subsample,
      
      colsample_bytree =
        best_colsample,
      
      reg_lambda =
        1,
      
      reg_alpha =
        0
    )
    
    
    final_fit =
      xgb.train(
        params =
          params,
        
        data =
          dtrain,
        
        nrounds =
          best_nrounds,
        
        verbose =
          0
      )
    
    
    ###########################
    # UNSEEN TEST PREDICTIONS
    ###########################
    
    predictions =
      as.numeric(
        predict(
          final_fit,
          dtest
        )
      )
    
    
    ###########################
    # SAVE RESULTS
    ###########################
    
    all_predictions[[
      as.character(test_year)
    ]] =
      test_fold %>%
      transmute(
        player_id,
        player_display_name,
        position,
        season,
        draft_season,
        
        actual =
          .data[[target]],
        
        prediction =
          predictions,
        
        model =
          "XGBoost",
        
        max_depth =
          best_depth,
        
        learning_rate =
          best_learning_rate,
        
        min_child_weight =
          best_min_child_weight,
        
        nrounds =
          best_nrounds,
        
        n_features =
          length(
            prepared$kept_features
          )
      )
    
    
    all_tuning[[
      as.character(test_year)
    ]] =
      tuning$tuning_summary %>%
      mutate(
        outer_test_year =
          test_year
      )
    
    
    selected_parameters[[
      as.character(test_year)
    ]] =
      best %>%
      mutate(
        outer_test_year =
          test_year
      )
  }
  
  
  list(
    predictions =
      bind_rows(
        all_predictions
      ),
    
    tuning =
      bind_rows(
        all_tuning
      ),
    
    selected_parameters =
      bind_rows(
        selected_parameters
      )
  )
}


# rb xgb
rb_xgb_results =
  run_xgboost_rolling(
    data =
      rb_train,
    
    features =
      rb_features,
    
    validation_seasons =
      2020:2025,
    
    xgb_grid =
      xgb_grid
  )

rb_xgb_predictions =
  rb_xgb_results$predictions


rb_xgb_predictions %>%
  select(
    player_display_name,
    draft_season,
    actual,
    prediction,
    max_depth,
    learning_rate,
    min_child_weight,
    nrounds
  ) %>%
  head(20)


# score

rb_xgb_overall =
  calculate_metrics(
    rb_xgb_predictions
  )

rb_xgb_by_year =
  rb_xgb_predictions %>%
  group_by(
    draft_season
  ) %>%
  group_modify(
    ~ calculate_metrics(.x)
  ) %>%
  ungroup()

rb_xgb_by_year


rb_xgb_results$selected_parameters

# combine all

rb_all_model_predictions =
  bind_rows(
    rb_baseline_predictions,
    rb_elastic_predictions,
    rb_xgb_predictions
  )




# leaderboard

rb_model_leaderboard =
  rb_all_model_predictions %>%
  group_by(
    model
  ) %>%
  group_modify(
    ~ calculate_metrics(.x)
  ) %>%
  ungroup() %>%
  arrange(
    RMSE
  )

rb_model_leaderboard

#   model                   n  RMSE   MAE R_squared Spearman
#1 Elastic Net           620  61.7  48.3     0.607    0.777
#2 XGBoost               620  63.6  49.8     0.582    0.773
# Previous Season PPG   620  68.6  51.2     0.515    0.753


rb_model_leaderboard_by_year =
  rb_all_model_predictions %>%
  group_by(
    model,
    draft_season
  ) %>%
  group_modify(
    ~ calculate_metrics(.x)
  ) %>%
  ungroup()

rb_model_leaderboard_by_year



############
# INVESTIGATING : predictions are very conservative, especially for top tier players
###########

rb_all_model_predictions =
  rb_all_model_predictions %>%
  mutate(
    residual =
      actual - prediction
  ) ## suspicion confirmed, nearly 65% of rows have negative residual (under predicted)

rb_residual_by_decile =
  rb_all_model_predictions %>%
  group_by(
    model,
    draft_season
  ) %>%
  mutate(
    actual_decile =
      ntile(
        actual,
        10
      )
  ) %>%
  ungroup() %>%
  group_by(
    model,
    actual_decile
  ) %>%
  summarise(
    n = n(),
    
    avg_actual =
      mean(actual),
    
    avg_prediction =
      mean(prediction),
    
    avg_residual =
      mean(residual),
    
    .groups = "drop"
  )

view(rb_residual_by_decile)


# do i drop players? -- binned count of games played

rb_target_games_summary =
  rb_train %>%
  summarise(
    n = n(),
    games_1_to_3 =
      sum(target_games_played <= 3),
    games_4_to_6 =
      sum(
        target_games_played >= 4 &
          target_games_played <= 6
      ),
    games_7_to_9 =
      sum(
        target_games_played >= 7 &
          target_games_played <= 9
      ),
    games_10_plus =
      sum(target_games_played >= 10)
  )

rb_target_games_summary









##########
# QUANTILE MODEL
###########

# defining quantiles -- floor - median - ceiling
quantile_levels =
  c(
    q20 = 0.20, # 80% of outcomes >
    q50 = 0.50, # median total ppr per 17
    q80 = 0.80 # 20% of outcomes >
  )


# evaluation metric function -- pinball loss
pinball_loss = function(
    actual,
    prediction,
    tau
) {
  
  error =
    actual - prediction
  
  loss =
    ifelse(
      error >= 0,
      tau * error,
      (1 - tau) * (-error)
    )
  
  mean(
    loss,
    na.rm = TRUE
  )
}

pinball_loss(
  actual = c(200, 250, 300),
  prediction = c(180, 260, 270),
  tau = 0.80
)




# quantile evaluation funciton 

calculate_quantile_metrics = function(data) {
  
  data =
    data %>%
    filter(
      !is.na(actual),
      !is.na(q20),
      !is.na(q50),
      !is.na(q80)
    )
  
  
  tibble(
    n =
      nrow(data),
    
    
    # Pinball loss
    pinball_q20 =
      pinball_loss(
        data$actual,
        data$q20,
        0.20
      ),
    
    pinball_q50 =
      pinball_loss(
        data$actual,
        data$q50,
        0.50
      ),
    
    pinball_q80 =
      pinball_loss(
        data$actual,
        data$q80,
        0.80
      ),
    
    
    # Combined quantile performance
    mean_pinball =
      mean(
        c(
          pinball_loss(
            data$actual,
            data$q20,
            0.20
          ),
          
          pinball_loss(
            data$actual,
            data$q50,
            0.50
          ),
          
          pinball_loss(
            data$actual,
            data$q80,
            0.80
          )
        )
      ),
    
    
    # Calibration
    q20_below_rate =
      mean(
        data$actual <= data$q20
      ),
    
    q50_below_rate =
      mean(
        data$actual <= data$q50
      ),
    
    q80_below_rate =
      mean(
        data$actual <= data$q80
      ),
    
    
    # Q20-Q80 interval coverage
    interval_coverage =
      mean(
        data$actual >= data$q20 &
          data$actual <= data$q80
      ),
    
    
    # Interval width
    mean_interval_width =
      mean(
        data$q80 - data$q20
      ),
    
    
    # Crossing
    crossing_rate =
      mean(
        data$q20 > data$q50 |
          data$q50 > data$q80
      )
  )
}





######
# TESTING SIMPLE MODEL ON 2025

quant_train =
  rb_train %>%
  filter(
    draft_season < 2025
  )

quant_test =
  rb_train %>%
  filter(
    draft_season == 2025
  )

prepared_quant =
  prepare_xgb_data(
    train_data = quant_train,
    test_data = quant_test,
    features = rb_features
  )



# data prep

dtrain_quant =
  xgb.DMatrix(
    data =
      prepared_quant$x_train,
    
    label =
      quant_train$target_ppr_17,
    
    missing =
      NA
  )


dtest_quant =
  xgb.DMatrix(
    data =
      prepared_quant$x_test,
    
    missing =
      NA
  )



# params and seed



quant_params =
  list(
    booster =
      "gbtree",
    
    objective =
      "reg:quantileerror",
    
    quantile_alpha =
      c(
        0.20,
        0.50,
        0.80
      ),
    
    tree_method =
      "hist",
    
    max_depth =
      3,
    
    learning_rate =
      0.03,
    
    min_child_weight =
      5,
    
    subsample =
      0.8,
    
    colsample_bytree =
      0.8,
    
    reg_lambda =
      1,
    
    reg_alpha =
      0
  )

set.seed(57)

quant_fit =
  xgb.train(
    params =
      quant_params,
    
    data =
      dtrain_quant,
    
    nrounds =
      300,
    
    verbose =
      0
  )

# PREDICTIONS 

quant_predictions =
  predict(
    quant_fit,
    dtest_quant
  )

dim(quant_predictions) # should be 99x3




rb_quant_test_predictions =
  quant_test %>%
  transmute(
    player_id,
    player_display_name,
    position,
    season,
    draft_season,
    
    actual =
      target_ppr_17,
    
    q20 =
      quant_predictions[, 1],
    
    q50 =
      quant_predictions[, 2],
    
    q80 =
      quant_predictions[, 3]
  )

# inspecting high end players 
rb_quant_test_predictions %>%
  arrange(
    desc(q80)
  ) %>%
  select(
    player_display_name,
    actual,
    q20,
    q50,
    q80
  ) %>%
  head(30) # visually predictions look pretty solid


# scoring simple 2025 predictions 
calculate_quantile_metrics(
  rb_quant_test_predictions
)

##############
# RESULTS - lower pinball loss is better. Below rate we want as close to the number as possible (q50, as close to .50 as possible)
#      n pinball_q20 pinball_q50 pinball_q80 mean_pinball q20_below_rate q50_below_rate q80_below_rate interval_coverage
# <int>       <dbl>       <dbl>       <dbl>        <dbl>          <dbl>          <dbl>          <dbl>             <dbl>
#   1    99        13.4        20.3        16.0         16.6          0.293          0.606          0.859             0.566

# pinball loss is minimal fine for a first pass. below rates are pretty close. q20 29.3% of actual outcomes finished below predicted q20 (overprediction)
# q50 60% of actual outcomes finished below their predicted median outcome (over prediction)
# q80 85.9% of actual outcomes finished below predicted q80 (over prediction)


# removing garbage results ("floor" is below median, "ceiling" below media)
rb_quant_test_predictions %>%
  filter(
    q20 > q50 |
      q50 > q80 # Ideally this = 0 rows
  )


#  decile to compareto previous test
rb_quant_by_decile =
  rb_quant_test_predictions %>%
  mutate(
    actual_decile =
      ntile(
        actual,
        10
      )
  ) %>%
  group_by(
    actual_decile
  ) %>%
  summarise(
    n = n(),
    
    avg_actual =
      mean(actual),
    
    avg_q20 =
      mean(q20),
    
    avg_q50 =
      mean(q50),
    
    avg_q80 =
      mean(q80),
    
    q20_below_rate =
      mean(actual <= q20),
    
    q50_below_rate =
      mean(actual <= q50),
    
    q80_below_rate =
      mean(actual <= q80),
    
    .groups = "drop"
  )

rb_quant_by_decile




############
# ROLLING WINDOW MODEL
############

# grid
quant_grid = expand.grid(
  max_depth = c(2, 3, 4, 5),
  learning_rate = c(0.01, 0.015, 0.02, 0.03),
  min_child_weight = c(10, 20, 25, 30, 50),
  nrounds = c(200, 500, 700, 900, 1000),
  subsample = 0.8,
  colsample_bytree = 0.8
)

nrow(quant_grid)




# temporal quantile tuning function

tune_quantile_temporal = function(
    train_data,
    features,
    quant_grid,
    target = "target_ppr_17"
) {
  
  available_years =
    sort(
      unique(
        train_data$draft_season
      )
    )
  
  
  validation_years =
    available_years[
      available_years >=
        min(available_years) + 2
    ]
  
  
  tuning_results = list()
  counter = 1
  
  
  for (validation_year in validation_years) {
    
    inner_train =
      train_data %>%
      filter(
        draft_season < validation_year
      )
    
    inner_validation =
      train_data %>%
      filter(
        draft_season == validation_year
      )
    
    
    prepared =
      prepare_xgb_data(
        train_data = inner_train,
        test_data = inner_validation,
        features = features
      )
    
    
    dtrain =
      xgb.DMatrix(
        data = prepared$x_train,
        label = inner_train[[target]],
        missing = NA
      )
    
    dvalidation =
      xgb.DMatrix(
        data = prepared$x_test,
        missing = NA
      )
    
    
    for (i in seq_len(nrow(quant_grid))) {
      
      params = list(
        booster = "gbtree",
        objective = "reg:quantileerror",
        
        quantile_alpha =
          c(0.20, 0.50, 0.80),
        
        tree_method = "hist",
        
        max_depth =
          quant_grid$max_depth[i],
        
        learning_rate =
          quant_grid$learning_rate[i],
        
        min_child_weight =
          quant_grid$min_child_weight[i],
        
        subsample =
          quant_grid$subsample[i],
        
        colsample_bytree =
          quant_grid$colsample_bytree[i],
        
        reg_lambda = 1,
        reg_alpha = 0
      )
      
      
      set.seed(57)
      
      
      fit =
        xgb.train(
          params = params,
          data = dtrain,
          nrounds = quant_grid$nrounds[i],
          verbose = 0
        )
      
      
      predictions =
        predict(
          fit,
          dvalidation
        )
      
      
      # Safety check
      if (is.null(dim(predictions)) ||
          ncol(predictions) != 3) {
        
        stop(
          paste(
            "Expected 3 quantile columns for",
            validation_year
          )
        )
      }
      
      
      q20 =
        predictions[, 1]
      
      q50 =
        predictions[, 2]
      
      q80 =
        predictions[, 3]
      
      actual =
        inner_validation[[target]]
      
      
      loss_q20 =
        pinball_loss(
          actual,
          q20,
          0.20
        )
      
      loss_q50 =
        pinball_loss(
          actual,
          q50,
          0.50
        )
      
      loss_q80 =
        pinball_loss(
          actual,
          q80,
          0.80
        )
      
      
      tuning_results[[counter]] =
        tibble(
          validation_year =
            validation_year,
          
          max_depth =
            quant_grid$max_depth[i],
          
          learning_rate =
            quant_grid$learning_rate[i],
          
          min_child_weight =
            quant_grid$min_child_weight[i],
          
          nrounds =
            quant_grid$nrounds[i],
          
          subsample =
            quant_grid$subsample[i],
          
          colsample_bytree =
            quant_grid$colsample_bytree[i],
          
          pinball_q20 =
            loss_q20,
          
          pinball_q50 =
            loss_q50,
          
          pinball_q80 =
            loss_q80,
          
          mean_pinball =
            mean(
              c(
                loss_q20,
                loss_q50,
                loss_q80
              )
            )
        )
      
      
      counter =
        counter + 1
    }
  }
  
  
  tuning_results =
    bind_rows(
      tuning_results
    )
  
  
  tuning_summary =
    tuning_results %>%
    group_by(
      max_depth,
      learning_rate,
      min_child_weight,
      nrounds,
      subsample,
      colsample_bytree
    ) %>%
    summarise(
      mean_pinball =
        mean(
          mean_pinball,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    ) %>%
    arrange(
      mean_pinball
    )
  
  
  best_parameters =
    tuning_summary %>%
    slice(1)
  
  
  list(
    best_parameters =
      best_parameters,
    
    tuning_summary =
      tuning_summary,
    
    yearly_results =
      tuning_results
  )
}


# rolling function

run_quantile_rolling = function(
    data,
    features,
    quant_grid,
    validation_seasons = 2020:2025,
    target = "target_ppr_17"
) {
  
  all_predictions =
    list()
  
  all_tuning =
    list()
  
  selected_parameters =
    list()
  
  
  for (test_year in validation_seasons) {
    
    cat(
      "\nRunning Quantile XGBoost for",
      test_year,
      "\n"
    )
    
    
    #########################
    # OUTER SPLIT
    #########################
    
    train_fold =
      data %>%
      filter(
        draft_season < test_year
      )
    
    test_fold =
      data %>%
      filter(
        draft_season == test_year
      )
    
    
    #########################
    # TEMPORAL TUNING
    #########################
    
    tuning =
      tune_quantile_temporal(
        train_data =
          train_fold,
        
        features =
          features,
        
        quant_grid =
          quant_grid,
        
        target =
          target
      )
    
    
    best =
      tuning$best_parameters
    
    
    cat(
      "Depth:",
      best$max_depth,
      "| Learning rate:",
      best$learning_rate,
      "| Min child:",
      best$min_child_weight,
      "| Trees:",
      best$nrounds,
      "\n"
    )
    
    
    #########################
    # PREPARE OUTER DATA
    #########################
    
    prepared =
      prepare_xgb_data(
        train_data =
          train_fold,
        
        test_data =
          test_fold,
        
        features =
          features
      )
    
    
    dtrain =
      xgb.DMatrix(
        data =
          prepared$x_train,
        
        label =
          train_fold[[target]],
        
        missing =
          NA
      )
    
    
    dtest =
      xgb.DMatrix(
        data =
          prepared$x_test,
        
        missing =
          NA
      )
    
    
    #########################
    # FINAL QUANTILE MODEL
    #########################
    
    params =
      list(
        booster =
          "gbtree",
        
        objective =
          "reg:quantileerror",
        
        quantile_alpha =
          c(
            0.20,
            0.50,
            0.80
          ),
        
        tree_method =
          "hist",
        
        max_depth =
          best$max_depth,
        
        learning_rate =
          best$learning_rate,
        
        min_child_weight =
          best$min_child_weight,
        
        subsample =
          best$subsample,
        
        colsample_bytree =
          best$colsample_bytree,
        
        reg_lambda =
          1,
        
        reg_alpha =
          0
      )
    
    
    set.seed(57)
    
    
    final_fit =
      xgb.train(
        params =
          params,
        
        data =
          dtrain,
        
        nrounds =
          best$nrounds,
        
        verbose =
          0
      )
    
    
    #########################
    # OUTER PREDICTIONS
    #########################
    
    predictions =
      predict(
        final_fit,
        dtest
      )
    
    
    if (
      is.null(dim(predictions)) ||
      ncol(predictions) != 3
    ) {
      
      stop(
        paste(
          "Prediction matrix failed for",
          test_year
        )
      )
    }
    
    
    #########################
    # SAVE
    #########################
    
    all_predictions[[
      as.character(test_year)
    ]] =
      test_fold %>%
      transmute(
        player_id,
        player_display_name,
        position,
        season,
        draft_season,
        
        actual =
          .data[[target]],
        
        q20 =
          predictions[, 1],
        
        q50 =
          predictions[, 2],
        
        q80 =
          predictions[, 3]
      )
    
    
    all_tuning[[
      as.character(test_year)
    ]] =
      tuning$tuning_summary %>%
      mutate(
        outer_test_year =
          test_year
      )
    
    
    selected_parameters[[
      as.character(test_year)
    ]] =
      best %>%
      mutate(
        outer_test_year =
          test_year
      )
  }
  
  
  list(
    predictions =
      bind_rows(
        all_predictions
      ),
    
    tuning =
      bind_rows(
        all_tuning
      ),
    
    selected_parameters =
      bind_rows(
        selected_parameters
      )
  )
}




# RUN THE MODEL

rb_quant_results =
  run_quantile_rolling(
    data =
      rb_train,
    
    features =
      rb_features,
    
    quant_grid =
      quant_grid,
    
    validation_seasons =
      2020:2025
  )

# PREDICTION
rb_quant_predictions =
  rb_quant_results$predictions


# EVAL

rb_quant_overall =
  calculate_quantile_metrics(
    rb_quant_predictions
  )

rb_quant_overall

# by season
rb_quant_by_year =
  rb_quant_predictions %>%
  group_by(
    draft_season
  ) %>%
  group_modify(
    ~ calculate_quantile_metrics(.x)
  ) %>%
  ungroup()


# check crossing

rb_quant_crossings =
  rb_quant_predictions %>%
  filter(
    q20 > q50 |
      q50 > q80
  )

rb_quant_crossings


# tuning decisions
rb_quant_results$selected_parameters

rb_quant_by_year






# revising elite RB question -- are they still under predicted?

rb_quant_by_decile =
  rb_quant_predictions %>%
  group_by(
    draft_season
  ) %>%
  mutate(
    actual_decile =
      ntile(
        actual,
        10
      )
  ) %>%
  ungroup() %>%
  group_by(
    actual_decile
  ) %>%
  summarise(
    n =
      n(),
    
    avg_actual =
      mean(actual),
    
    avg_q20 =
      mean(q20),
    
    avg_q50 =
      mean(q50),
    
    avg_q80 =
      mean(q80),
    
    q20_below_rate =
      mean(
        actual <= q20
      ),
    
    q50_below_rate =
      mean(
        actual <= q50
      ),
    
    q80_below_rate =
      mean(
        actual <= q80
      ),
    
    .groups =
      "drop"
  )

rb_quant_by_decile

# comparing to previous grid searches 
rb_quant_overall
rb_quant_by_year
rb_quant_results$selected_parameters

# result: this large grid did uncover marginally better performance. q80 below rate is almost perfect, and 51.8 interval coverage (whcih should be 60) is okay. Driven mostly by q20 and q50 level predictions and the skew toward 0.
# i am okay with these results as the players at the top are the most important to get "right", there is more uncertainty later in the draft compared to the leagues stars


#########
# MAKING FINAL PREDICTIONS - ELASTIC NET AND QUANTILE 
#########

## ELASTIC NET ---------------------

# final tuning, retraining the model on ALL draft seasons (2017 - 2025)
rb_elastic_final_tuning =
  tune_elastic_temporal(
    train_data = rb_train,
    features = rb_features,
    target = "target_ppr_17"
  )
# best metrics
rb_elastic_final_tuning$best_alpha
rb_elastic_final_tuning$best_lambda

# store best metrics 
rb_final_alpha =
  rb_elastic_final_tuning$best_alpha

rb_final_lambda =
  rb_elastic_final_tuning$best_lambda





# PREPARING THE DATA + FITTING (trimming the features)

rb_elastic_final_data =
  prepare_glmnet_data(
    train_data = rb_train,
    test_data = rb_predict_2026,
    features = rb_features
  )



# final fit

rb_elastic_final_fit =
  glmnet(
    x =
      rb_elastic_final_data$x_train,
    
    y =
      rb_train$target_ppr_17,
    
    family =
      "gaussian",
    
    alpha =
      rb_final_alpha,
    
    lambda =
      rb_final_lambda,
    
    standardize =
      TRUE
  )



rb_2026_mean =
  as.numeric(
    predict(
      rb_elastic_final_fit,
      newx =
        rb_elastic_final_data$x_test,
      s =
        rb_final_lambda
    )
  )



## QUANTILE ------------------------


# final tuning, retraining the model on ALL draft seasons (2017 - 2025)
rb_quant_final_tuning =
  tune_quantile_temporal(
    train_data =
      rb_train,
    
    features =
      rb_features,
    
    quant_grid =
      quant_grid,
    
    target =
      "target_ppr_17"
  )

rb_quant_final_tuning$best_parameters


# best params
rb_quant_final_params =
  rb_quant_final_tuning$best_parameters



## PREPARE DATA + FIT

rb_quant_final_data =
  prepare_xgb_data(
    train_data =
      rb_train,
    
    test_data =
      rb_predict_2026,
    
    features =
      rb_features
  )


rb_quant_dtrain =
  xgb.DMatrix(
    data =
      rb_quant_final_data$x_train,
    
    label =
      rb_train$target_ppr_17,
    
    missing =
      NA
  )


rb_quant_dtest =
  xgb.DMatrix(
    data =
      rb_quant_final_data$x_test,
    
    missing =
      NA
  )




## FIT

rb_quant_params =
  list(
    booster =
      "gbtree",
    
    objective =
      "reg:quantileerror",
    
    quantile_alpha =
      c(
        0.20,
        0.50,
        0.80
      ),
    
    tree_method =
      "hist",
    
    max_depth =
      rb_quant_final_params$max_depth,
    
    learning_rate =
      rb_quant_final_params$learning_rate,
    
    min_child_weight =
      rb_quant_final_params$min_child_weight,
    
    subsample =
      rb_quant_final_params$subsample,
    
    colsample_bytree =
      rb_quant_final_params$colsample_bytree,
    
    reg_lambda =
      1,
    
    reg_alpha =
      0
  )

set.seed(57)

rb_quant_final_fit =
  xgb.train(
    params =
      rb_quant_params,
    
    data =
      rb_quant_dtrain,
    
    nrounds =
      rb_quant_final_params$nrounds,
    
    verbose =
      0
  )



## PREDICT

rb_2026_quantiles =
  predict(
    rb_quant_final_fit,
    rb_quant_dtest
  )




###########
# 2026 PREDICTION COMPARISON TABLE 
###########

rb_2026_predictions =
  rb_predict_2026 %>%
  transmute(
    player_id,
    player_display_name,
    position,
    draft_team,
    
    projected_ppr_17 =
      rb_2026_mean,
    
    q20 =
      rb_2026_quantiles[, 1],
    
    q50 =
      rb_2026_quantiles[, 2],
    
    q80 =
      rb_2026_quantiles[, 3]
  )

# ADDING PER GAME DISTINCTION
rb_2026_predictions =
  rb_2026_predictions %>%
  mutate(
    projected_ppg =
      projected_ppr_17 / 17,
    
    q20_ppg =
      q20 / 17,
    
    q50_ppg =
      q50 / 17,
    
    q80_ppg =
      q80 / 17
  ) %>% arrange(desc(projected_ppr_17) )

# checking crossing
rb_2026_predictions %>%
  filter(
    q20 > q50 |
      q50 > q80
  )


# adding rankings based on the projected mean (elastic net prediction)
rb_2026_predictions =
  rb_2026_predictions %>%
  mutate(
    rb_rank_mean =
      min_rank(
        desc(projected_ppr_17)
      ),
    
    rb_rank_ceiling =
      min_rank(
        desc(q80)
      ),
    
    rb_rank_floor =
      min_rank(
        desc(q20)
      )
  ) %>%
  arrange(
    rb_rank_mean
  )

#write_csv(rb_2026_predictions, "rb_2026_predictions.csv")

write_csv(rb_2026_predictions, "rb_2026_predictions_2.csv")


##### ---------------------------------------------------------------------------------------------------------------------------------
## WIDE RECEIVERS AND TIGHT ENDS








