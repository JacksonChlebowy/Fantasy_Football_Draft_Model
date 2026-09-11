# My goal is to create 3 different models, one for each stat/position group (rushing, receiving, QB).
# I will split players by Position, RBs (rushing and receiving stats), 
# WR/TE (receiving stats, select rushing stats for gadget players), QBs (passing stats)

# My target is to predict Total PPR fantasy points (calculate PPG, assuming 17 games player)
## I will later make expected games predictions (or use someone elses expected games predictions) to recalculate PPR PPG


model_df = read_csv("season_model_df.csv")

# qb stats on a per game basis
model_df = model_df %>%
  mutate(
    completions_pg = if_else(
      games_played > 0,
      completions / games_played,
      0
    ),
    
    passing_td_rate = if_else(
      attempts > 0,
      passing_tds / attempts,
      0
    ),
    
    interception_rate = if_else(
      attempts > 0,
      interceptions / attempts,
      0
    )
  )


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
# QB Predictors
#########

qb_features = c(
  
  ####################
  # PLAYER PROFILE
  ####################
  
  "age_at_season_start",
  "years_exp",
  "draft_number",
  
  
  ####################
  # PREVIOUS FANTASY PRODUCTION
  ####################
  
  "games_played",
  "fantasy_ppg",
  "games_missed",
  
  ####################
  # PASSING VOLUME
  ####################
  
  "attempts_pg",
  "completions_pg",
  "passing_yards_pg",
  "passing_tds_pg",
  "interceptions_pg",
  
  
  ####################
  # PASSING EFFICIENCY
  ####################
  
  "completion_pct",
  "passing_yards_per_attempt",
  "passing_td_rate",
  "interception_rate",
  "position_pass_attempt_share",
  
  
  ####################
  # RUSHING VALUE
  ####################
  
  "carries_pg",
  "rushing_yards_pg",
  "rushing_tds_pg",
  "rushing_yards_per_carry",
  
  
  ####################
  # EXPECTED FANTASY PRODUCTION
  ####################
  
  "total_xfp_pg",
  "rush_xfp_pg",
  "rush_td_exp_pg",
  "total_fp_over_exp_pg",
  
  
  ####################
  # TRAJECTORY
  ####################
  
  "fantasy_ppg_change_1yr",
  
  
  ####################
  # TEAM ENVIRONMENT
  ####################
  
  "team_plays_per_game",
  "team_pass_rate",
  "team_pass_attempts_per_game",
  "team_offensive_tds_per_game",
  "team_sack_rate",
  
  
  
  ####################
  # UPCOMING SITUATION
  ####################
  
  "new_team",
  "new_hc",
  "new_oc",
  "team_win_pct",
  "team_pts_scored",
  "team_made_playoffs",
  
  
  ####################
  # ADVANCED STATS
  ####################
  
  "passer_rating",
  "avg_time_to_throw",
  "aggressiveness",
  "avg_completed_air_yards",
  "avg_intended_air_yards",
  "completion_percentage_above_expectation",
  "qb_attempt_rank"
)
  
  
# check

setdiff(
  qb_features,
  names(model_df)
) # none missing
  
  
  
  
  
# qb df

qb_df = model_df %>%
  filter(
    position == "QB",
    dataset_type != "no_next_season_data"
  ) %>%
  select(
    any_of(id_cols),
    any_of(qb_features),
    any_of(target_cols)
  )


# 2nd smaller df, removed a lot of 3/4 string backup noise
qb_df_top2 = qb_df %>%
  filter(
    qb_attempt_rank <= 2
  )



# train/test

qb_train = qb_df %>%
  filter(
    dataset_type == "training",
    !is.na(target_ppr_17)
  )

qb_predict_2026 = qb_df %>%
  filter(
    dataset_type == "prediction"
  )


qb_train_top2 = qb_df_top2 %>%
  filter(
    dataset_type == "training",
    !is.na(target_ppr_17)
  )

qb_predict_2026_top2 = qb_df_top2 %>%
  filter(
    dataset_type == "prediction"
  )




# check -- non 0
nrow(qb_train) # 523
nrow(qb_predict_2026) # 80 (seems like a lot, probably many backup QBs)



## BASELINE PREDICTION -- NAIVE

qb_folds = lapply(
  2020:2025,
  function(test_year) {
    
    train_fold = qb_train %>%
      filter(draft_season < test_year)
    
    test_fold = qb_train %>%
      filter(draft_season == test_year)
    
    list(
      test_year = test_year,
      train = train_fold,
      test = test_fold
    )
  }
)

names(qb_folds) = 2020:2025

# top 2 only folds
qb_folds_top2 = lapply(
  2020:2025,
  function(test_year) {
    
    train_fold = qb_train_top2 %>%
      filter(draft_season < test_year)
    
    test_fold = qb_train_top2 %>%
      filter(draft_season == test_year)
    
    list(
      test_year = test_year,
      train = train_fold,
      test = test_fold
    )
  }
)

names(qb_folds_top2) = 2020:2025




# predictions
qb_baseline_predictions =
  bind_rows(
    lapply(
      qb_folds,
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


# evaluate

calculate_metrics(
  qb_baseline_predictions
)

#    n     RMSE      MAE R_squared  Spearman
#   367 110.9732 81.90593 0.1783699 0.6013454
# NAIVE = NOT GOOD




# ELASTIC NET

qb_elastic_results =
  run_elastic_net_rolling(
    data = qb_train,
    features = qb_features,
    validation_seasons = 2020:2025
  )

qb_elastic_results_top2 =
  run_elastic_net_rolling(
    data = qb_train_top2,
    features = qb_features,
    validation_seasons = 2020:2025
  )

qb_elastic_predictions =
  qb_elastic_results$predictions

qb_elastic_predictions_top2 =
  qb_elastic_results_top2$predictions

calculate_metrics(
  qb_elastic_predictions
    )

#    n     RMSE      MAE R_squared  Spearman
#   367 93.41555 73.44143 0.4177916 0.6614869
# better than naive but not by much


calculate_metrics(
  qb_elastic_predictions_top2
)

#     n     RMSE      MAE R_squared  Spearman
#   312 93.28483 72.25377 0.4010982 0.6592449
# it does not seem that trimming down the df helped very much




# XGBOOST

qb_xgb_grid = expand.grid(
  max_depth = c(2, 3),
  learning_rate = c(0.03, 0.04, 0.05),
  min_child_weight = c(3, 5, 7, 10),
  subsample = 0.8,
  colsample_bytree = 0.8
)


qb_xgb_results =
  run_xgboost_rolling(
    data = qb_train,
    features = qb_features,
    validation_seasons = 2020:2025,
    xgb_grid = qb_xgb_grid
  )

qb_xgb_results_top2 =
  run_xgboost_rolling(
    data = qb_train_top2,
    features = qb_features,
    validation_seasons = 2020:2025,
    xgb_grid = qb_xgb_grid
  )


qb_xgb_predictions =
  qb_xgb_results$predictions

qb_xgb_predictions_top2 =
  qb_xgb_results_top2$predictions

calculate_metrics(
  qb_xgb_predictions
)

#     n     RMSE      MAE R_squared  Spearman
#   367 90.63237 70.83385 0.4519669 0.6838058

calculate_metrics(
  qb_xgb_predictions_top2
)



# LEADERBOARD


qb_all_model_predictions =
  bind_rows(
    qb_baseline_predictions,
    qb_elastic_predictions,
    qb_xgb_predictions,
    qb_elastic_predictions_top2,
    qb_xgb_predictions_top2
  )

qb_model_leaderboard =
  qb_all_model_predictions %>%
  group_by(model) %>%
  group_modify(
    ~ calculate_metrics(.x)  # THIS DOES NOT WORK THE SAME AFTER ADDING _top2 ****
  ) %>%
  ungroup() %>%
  arrange(RMSE)

qb_model_leaderboard

# yearly

qb_model_leaderboard_by_year =
  qb_all_model_predictions %>%
  group_by(
    model,
    draft_season # # THIS DOES NOT WORK THE SAME AFTER ADDING _top2 ****
  ) %>%
  group_modify(
    ~ calculate_metrics(.x)
  ) %>%
  ungroup()

qb_model_leaderboard_by_year




# FOR THE FINAL MEAN PREDICTION I WILL USE ALL QBS, XGBOOST MODEL


# FINAL XGB PREDICTIONS 2026

qb_xgb_grid_original = expand.grid(
  max_depth = c(2, 3, 4, 6),
  learning_rate = c(0.01, 0.03, 0.05),
  min_child_weight = c(5, 10, 20),
  subsample = 0.8,
  colsample_bytree = 0.8
)

# tuning
qb_xgb_final_tuning =
  tune_xgb_temporal(
    train_data = qb_train,
    features = qb_features,
    xgb_grid = qb_xgb_grid_original,
    target = "target_ppr_17"
  )

qb_xgb_final_params =
  qb_xgb_final_tuning$best_parameters

# data prep
qb_xgb_final_data =
  prepare_xgb_data(
    train_data = qb_train,
    test_data = qb_predict_2026,
    features = qb_features
  )


qb_xgb_dtrain =
  xgb.DMatrix(
    data = qb_xgb_final_data$x_train,
    label = qb_train$target_ppr_17,
    missing = NA
  )

qb_xgb_dtest =
  xgb.DMatrix(
    data = qb_xgb_final_data$x_test,
    missing = NA
  )


qb_xgb_params =
  list(
    booster = "gbtree",
    objective = "reg:squarederror",
    tree_method = "hist",
    
    max_depth =
      qb_xgb_final_params$max_depth,
    
    learning_rate =
      qb_xgb_final_params$learning_rate,
    
    min_child_weight =
      qb_xgb_final_params$min_child_weight,
    
    subsample =
      qb_xgb_final_params$subsample,
    
    colsample_bytree =
      qb_xgb_final_params$colsample_bytree
  )
set.seed(57)

# fit
qb_xgb_final_fit =
  xgb.train(
    params = qb_xgb_params,
    data = qb_xgb_dtrain,
    nrounds = qb_xgb_final_params$median_nrounds,
    verbose = 0
  )

# predict

qb_2026_mean =
  as.numeric(
    predict(
      qb_xgb_final_fit,
      qb_xgb_dtest
    )
  )

# final 2026 table

qb_2026_predictions =
  qb_predict_2026 %>%
  transmute(
    player_id,
    player_display_name,
    position,
    draft_team,
    
    projected_ppr_17 =
      pmax(qb_2026_mean, 0),
    
    projected_ppg =
      pmax(qb_2026_mean, 0) / 17
  ) %>%
  arrange(
    desc(projected_ppr_17)
  ) %>% mutate(
    qb_rank_mean =
      min_rank(
        desc(projected_ppr_17)
      )
  )

# predictions look alright. players going early in drafts are generally near the top. Not a lot of variance in predictions at the top

write_csv(
  qb_2026_predictions,
  "qb_2026_mean_predictions.csv"
)



# QUANTILE

qb_quant_grid = expand.grid(
  max_depth = c(2, 3, 4),
  learning_rate = c(0.01, 0.02, 0.03, 0.05),
  min_child_weight = c(5, 10, 20),
  nrounds = c(200, 500, 800, 1000),
  subsample = 0.8,
  colsample_bytree = 0.8
)

qb_quant_results =
  run_quantile_rolling(
    data = qb_train,
    features = qb_features,
    quant_grid = qb_quant_grid,
    validation_seasons = 2020:2025
  )

# predictions
qb_quant_predictions =
  qb_quant_results$predictions



# evaluation
qb_quant_overall =
  calculate_quantile_metrics(
    qb_quant_predictions
  )

# mean pinball is a little high, but not unexpected. q20 is 9% too high (essentially, q20 predictions are slightly optimistic, 29% of actual outcomes fin below). q50 q80 are almost perfect 







#######
# FINAL QB QUANTILE MODEL
#######

qb_quant_final_tuning =
  tune_quantile_temporal(
    train_data = qb_train,
    features = qb_features,
    quant_grid = qb_quant_grid,
    target = "target_ppr_17"
  )

qb_quant_final_params =
  qb_quant_final_tuning$best_parameters

qb_quant_final_params


# data prep
qb_quant_final_data =
  prepare_xgb_data(
    train_data = qb_train,
    test_data = qb_predict_2026,
    features = qb_features
  )

qb_quant_dtrain =
  xgb.DMatrix(
    data = qb_quant_final_data$x_train,
    label = qb_train$target_ppr_17,
    missing = NA
  )

qb_quant_dtest =
  xgb.DMatrix(
    data = qb_quant_final_data$x_test,
    missing = NA
  )

dim(qb_quant_final_data$x_test)
nrow(qb_predict_2026)


# fit

qb_quant_params =
  list(
    booster = "gbtree",
    objective = "reg:quantileerror",
    
    quantile_alpha = c(
      0.20,
      0.50,
      0.80
    ),
    
    tree_method = "hist",
    
    max_depth =
      qb_quant_final_params$max_depth,
    
    learning_rate =
      qb_quant_final_params$learning_rate,
    
    min_child_weight =
      qb_quant_final_params$min_child_weight,
    
    subsample =
      qb_quant_final_params$subsample,
    
    colsample_bytree =
      qb_quant_final_params$colsample_bytree,
    
    reg_lambda = 1,
    reg_alpha = 0
  )
set.seed(57)

qb_quant_final_fit =
  xgb.train(
    params = qb_quant_params,
    data = qb_quant_dtrain,
    nrounds = qb_quant_final_params$nrounds,
    verbose = 0
  )

# predict

qb_2026_quantiles =
  predict(
    qb_quant_final_fit,
    qb_quant_dtest
  )


# final dataset 

qb_2026_predictions =
  qb_predict_2026 %>%
  transmute(
    player_id,
    player_display_name,
    position,
    draft_team,
    
    projected_ppr_17 =
      qb_2026_mean,
    
    q20_raw =
      qb_2026_quantiles[, 1],
    
    q50_raw =
      qb_2026_quantiles[, 2],
    
    q80_raw =
      qb_2026_quantiles[, 3]
  )

# no negatives
qb_2026_predictions =
  qb_2026_predictions %>%
  mutate(
    projected_ppr_17 =
      pmax(projected_ppr_17, 0),
    
    q20 =
      pmax(q20_raw, 0),
    
    q50 =
      pmax(q50_raw, 0),
    
    q80 =
      pmax(q80_raw, 0)
  )




# checking for crossings 

qb_2026_crossings =
  qb_2026_predictions %>%
  filter(
    q20 > q50 |
      q50 > q80
  ) %>%
  select(
    player_display_name,
    draft_team,
    q20,
    q50,
    q80
  )

qb_2026_crossings

qb_2026_predictions =
  qb_2026_predictions %>%
  rowwise() %>%
  mutate(
    q20_display =
      min(q20, q50, q80),
    
    q50_display =
      median(c(q20, q50, q80)),
    
    q80_display =
      max(q20, q50, q80)
  ) %>%
  ungroup()

# everythin to PPG

qb_2026_predictions =
  qb_2026_predictions %>%
  mutate(
    projected_ppg =
      projected_ppr_17 / 17,
    
    q20_ppg =
      q20_display / 17,
    
    q50_ppg =
      q50_display / 17,
    
    q80_ppg =
      q80_display / 17
  )

# position rankings 
qb_2026_predictions =
  qb_2026_predictions %>%
  group_by(position) %>%
  mutate(
    position_rank_mean =
      min_rank(
        desc(projected_ppr_17)
      ),
    
    position_rank_floor =
      min_rank(
        desc(q20_display)
      ),
    
    position_rank_median =
      min_rank(
        desc(q50_display)
      ),
    
    position_rank_ceiling =
      min_rank(
        desc(q80_display)
      )
  ) %>%
  ungroup()




# FINAL QB FILE + SAVE

qb_2026_final =
  qb_2026_predictions %>%
  select(
    player_id,
    player_display_name,
    position,
    draft_team,
    
    position_rank_mean,
    
    projected_ppr_17,
    projected_ppg,
    
    q20_display,
    q50_display,
    q80_display,
    
    q20_ppg,
    q50_ppg,
    q80_ppg,
    
    position_rank_floor,
    position_rank_median,
    position_rank_ceiling
  ) %>%
  arrange(position_rank_mean)


write_csv(
  qb_2026_final,
  "qb_2026_predictions.csv"
)



#####
# COMBINING ALL PREDICTIONS INTO 1 DATASET
#####

# editing rb names to match others 
rb_2026_final = rb_2026_predictions %>%
  rename(
    position_rank_mean = rb_rank_mean, 
    position_rank_floor = rb_rank_floor,
    position_rank_ceiling = rb_rank_ceiling,

    q20_display = q20,
    q50_display = q50,
    q80_display = q80
  ) %>% mutate( position_rank_median =  min_rank(desc(q50_display) ) ) # adding this in for RB, didnt calculate it earlier




all_2026_predictions =
  bind_rows(
    rb_2026_final,
    receiver_2026_final,
    qb_2026_final
  )


# renaming cols
all_2026_predictions =
  all_2026_predictions %>%
  rename(
    expected_ppr_17 =
      projected_ppr_17,
    
    expected_ppg =
      projected_ppg,
    
    downside_ppr_17 =
      q20_display,
    
    median_ppr_17 =
      q50_display,
    
    upside_ppr_17 =
      q80_display,
    
    downside_ppg =
      q20_ppg,
    
    median_ppg =
      q50_ppg,
    
    upside_ppg =
      q80_ppg
  )

# adding overall fantasy points ranking

all_2026_predictions =
  all_2026_predictions %>%
  mutate(
    overall_raw_points_rank =
      min_rank(
        desc(expected_ppr_17)
      )
  )

# rearranging and saving

all_2026_predictions =
  all_2026_predictions %>%
  select(
    player_id,
    player_display_name,
    position,
    draft_team,
    
    position_rank_mean,
    overall_raw_points_rank,
    
    expected_ppr_17,
    expected_ppg,
    
    downside_ppr_17,
    median_ppr_17,
    upside_ppr_17,
    
    downside_ppg,
    median_ppg,
    upside_ppg,
    
    position_rank_floor,
    position_rank_median,
    position_rank_ceiling
  ) %>%
  arrange(
    position,
    position_rank_mean
  )

write_csv(
  all_2026_predictions,
  "2026_fantasy_player_predictions.csv"
)
