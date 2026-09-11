# My goal is to create 3 different models, one for each stat/position group (rushing, receiving, QB).
# I will split players by Position, RBs (rushing and receiving stats), 
# WR/TE (receiving stats, select rushing stats for gadget players), QBs (passing stats)

# My target is to predict Total PPR fantasy points (calculate PPG, assuming 17 games player)
## I will later make expected games predictions (or use someone elses expected games predictions) to recalculate PPR PPG


model_df = read_csv("season_model_df.csv")

# is TE flag - production different between TE/WRs, want to make that distinction
model_df = model_df %>%
  mutate(
    is_te = as.integer(position == "TE")
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
# WR/TE Predictors
#########


receiver_features = c(
  
  ####################
  # POSITION
  ####################
  
  "is_te",
  
  
  ####################
  # PLAYER PROFILE
  ####################
  
  "age_at_season_start",
  "years_exp",
  "draft_number",
  "height",
  "weight",
  "fantasy_sd",
  
  
  ####################
  # PREVIOUS FANTASY PRODUCTION
  ####################
  
  "games_played",
  "fantasy_ppg",
  "avg_weekly_off_snapshare",
  "games_missed",
  
  
  ####################
  # RECEIVING VOLUME
  ####################
  
  "targets_pg",
  "receptions_pg",
  "receiving_yards_pg",
  "receiving_tds_pg",
  "catch_rate",
  
  
  ####################
  # EXPECTED OPPORTUNITY
  ####################
  
  "total_xfp_pg",
  "rec_xfp_pg",
  "rec_td_exp_pg",
  "total_fp_over_exp_pg",
  
  
  ####################
  # USAGE / MARKET SHARE
  ####################
  
  "team_target_share",
  "team_opportunity_share",
  "position_target_share",
  
  
  ####################
  # TRAJECTORY
  ####################
  
  "fantasy_ppg_change_1yr",
  "target_share_change_1yr",
  
  
  ####################
  # TEAM ENVIRONMENT
  ####################
  
  "team_plays_per_game",
  "position_target_concentration",
  "team_pass_rate",
  "team_pass_attempts_per_game",
  "team_passing_tds_per_game",
  "team_yards_per_pass_attempt",
  "team_win_pct",
  "team_pts_scored",
  "team_passing_epa",
  "team_passing_cpoe",
  "team_passing_tds_per_game",
  "team_yards_per_pass_attempt",
  "team_explosive_pass_rate_20",

  
  ####################
  # UPCOMING SITUATION
  ####################
  
  "new_team",
  "new_hc", # does the team have a new hc or oc, not player
  "new_oc",
  "team_vacated_target_share",
  
  
  ####################
  # GADGET / RUSHING USAGE
  ####################
  
  "carries_pg",
  "rushing_yards_pg",
  "rushing_tds_pg"
  
  
  
  #####################
  # OTHERS 
  #####################
  
  #"avg_intended_air_yards_receiving",
  #"percent_share_of_intended_air_yards_receiving",
  #"avg_cushion",
  #"avg_separation",
  #"catch_rate",
  #"avg_yac",
  #"avg_expected_yac",
  # "avg_yac_above_expectation"
  
)

missing_receiver_features =
  setdiff(
    receiver_features,
    names(model_df)
  )

missing_receiver_features






#####
# RECEIVER DF
#####

receiver_df = model_df %>%
  filter(
    position %in% c("WR", "TE"),
    dataset_type != "no_next_season_data"
  ) %>%
  select(
    any_of(id_cols),
    any_of(receiver_features),
    any_of(target_cols)
  ) %>%
  filter(
    !is.na(draft_team) # FILTERING OUT PLAYERS WITHOUT A TEAM IN 2026
  )


# filling NAs
receiver_df$catch_rate[is.na(receiver_df$catch_rate)] = 0
receiver_df$fantasy_ppg_change_1yr[is.na(receiver_df$fantasy_ppg_change_1yr)] = 0
receiver_df$target_share_change_1yr[is.na(receiver_df$target_share_change_1yr)] = 0
receiver_df$avg_weekly_off_snapshare[is.na(receiver_df$avg_weekly_off_snapshare)] = 0
receiver_df$total_fp_over_exp_pg[is.na(receiver_df$total_fp_over_exp_pg)] = 0



# removing rows without a target (did not play next season)
receiver_df = receiver_df %>%
  filter(
    avg_weekly_off_snapshare != 0, # useless players if they did not play a single percent of team snaps
    fantasy_ppg > 0   # again, useless players. Dont contribute anything to the model
    ) 



# TRAIN / TEST SPLIT
receiver_train =
  receiver_df %>%
  filter(
    dataset_type == "training",
    !is.na(target_ppr_17)
  )


receiver_predict_2026 =
  receiver_df %>%
  filter(
    dataset_type == "prediction"
  )



#####
## RECEIVER ROLLING FOLDS FUNCTION

receiver_folds =
  lapply(
    2020:2025,
    function(test_year) {
      
      train_fold =
        receiver_train %>%
        filter(
          draft_season < test_year
        )
      
      test_fold =
        receiver_train %>%
        filter(
          draft_season == test_year
        )
      
      list(
        test_year = test_year,
        train = train_fold,
        test = test_fold
      )
    }
  )

names(receiver_folds) =
  2020:2025



#####
## BASELINE PREDICTIONS -- NAIVE 

#function
receiver_baseline_predictions =
  bind_rows(
    lapply(
      receiver_folds,
      function(x) {
        
        x$test %>%
          transmute(
            player_id,
            player_display_name,
            position,
            season,
            draft_season,
            
            actual =
              target_ppr_17,
            
            prediction =
              fantasy_ppg * 17,
            
            model =
              "Previous Season PPG"
          )
      }
    )
  )

# EVALUATE
receiver_baseline_overall =
  calculate_metrics(
    receiver_baseline_predictions
  )

receiver_baseline_overall



#####
## ELASTIC NET

# using functions created earlier for the RB model

receiver_elastic_results =
  run_elastic_net_rolling(
    data =
      receiver_train,
    
    features =
      receiver_features,
    
    validation_seasons =
      2020:2025
  )

# predictions 

receiver_elastic_predictions =
  receiver_elastic_results$predictions


#####
## XGBOOST

# same as elastic net, using function from RB modeling

#defining a new grid
xgb_grid = expand.grid(
  max_depth = c(2, 3, 4, 6, 8, 12),
  learning_rate = c(0.01, 0.03, 0.05),
  min_child_weight = c(1, 5, 10, 20),
  subsample = 0.8,
  colsample_bytree = 0.8
)


receiver_xgb_results =
  run_xgboost_rolling(
    data =
      receiver_train,
    
    features =
      receiver_features,
    
    validation_seasons =
      2020:2025,
    
    xgb_grid =
      xgb_grid
  )

# prediction    
receiver_xgb_predictions =
  receiver_xgb_results$predictions





#####
## LEADERBOARD -- evaluating RMSE

receiver_all_model_predictions =
  bind_rows(
    receiver_baseline_predictions,
    receiver_elastic_predictions,
    receiver_xgb_predictions
  )

receiver_model_leaderboard =
  receiver_all_model_predictions %>%
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

receiver_model_leaderboard


# year by year leaderboard
receiver_model_leaderboard_by_year =
  receiver_all_model_predictions %>%
  group_by(
    model,
    draft_season
  ) %>%
  group_modify(
    ~ calculate_metrics(.x)
  ) %>%
  ungroup()

receiver_model_leaderboard_by_year




#####
## WR VS TE PERFORMANCE


receiver_model_by_position =
  receiver_all_model_predictions %>%
  group_by(
    model,
    position
  ) %>%
  group_modify(
    ~ calculate_metrics(.x)
  ) %>%
  ungroup()

receiver_model_by_position

# TE performance has less error than WR as expected, production is more narrow for that position so it makes sense
# overall performance is good, spearman is hovering around .8 which is strong. BEST MODEL IS ELASTIC NET MAE 38.6 pts, RMSE 48.8


#####
## QUANTILE MODEL

# grid
receiver_quant_grid = expand.grid(
  max_depth = c(2, 3, 4, 6),
  learning_rate = c(0.01, 0.02, 0.03, 0.05),
  min_child_weight = c(10, 20, 30),
  nrounds = c(200, 500, 800, 1000),
  subsample = 0.8,
  colsample_bytree = 0.8
)

# fit
receiver_quant_results =
  run_quantile_rolling(
    data = receiver_train,
    features = receiver_features,
    quant_grid = receiver_quant_grid,
    validation_seasons = 2020:2025
  )

# predictions
receiver_quant_predictions =
  receiver_quant_results$predictions



# evaluation
receiver_quant_overall =
  calculate_quantile_metrics(
    receiver_quant_predictions
  )

receiver_quant_overall

# selected params
receiver_quant_results$selected_parameters



# evaluating by position WR / TE
receiver_quant_by_position =
  receiver_quant_predictions %>%
  group_by(position) %>%
  group_modify(
    ~ calculate_quantile_metrics(.x)
  ) %>%
  ungroup()

receiver_quant_by_position
# compared to RBs, much lower pinball loss, q20, 50 and 80 rates are also very close. I am happy with these results


# evaluate crossing

receiver_quant_predictions %>%
  filter(
    q20 > q50 |
      q50 > q80
  ) %>%
  select(
    player_display_name,
    position,
    draft_season,
    actual,
    q20,
    q50,
    q80
  ) # minimal crossing 4 total players 





#####
## FINAL PREDICTIONS

# ELASTIC NET

# final tuning 
receiver_elastic_final_tuning =
  tune_elastic_temporal(
    train_data = receiver_train,
    features = receiver_features,
    target = "target_ppr_17"
  )

receiver_elastic_final_tuning$best_alpha
receiver_elastic_final_tuning$best_lambda

receiver_final_alpha =
  receiver_elastic_final_tuning$best_alpha

receiver_final_lambda =
  receiver_elastic_final_tuning$best_lambda

# data prep

receiver_elastic_final_data =
  prepare_glmnet_data(
    train_data = receiver_train,
    test_data = receiver_predict_2026,
    features = receiver_features
  )


# fit
receiver_elastic_final_fit =
  glmnet(
    x = receiver_elastic_final_data$x_train,
    y = receiver_train$target_ppr_17,
    family = "gaussian",
    alpha = receiver_final_alpha,
    lambda = receiver_final_lambda,
    standardize = TRUE
  )

# predict
receiver_2026_mean =
  as.numeric(
    predict(
      receiver_elastic_final_fit,
      newx = receiver_elastic_final_data$x_test,
      s = receiver_final_lambda
    )
  )




# QUANTILE

# tuning 

receiver_quant_final_tuning =
  tune_quantile_temporal(
    train_data = receiver_train,
    features = receiver_features,
    quant_grid = receiver_quant_grid,
    target = "target_ppr_17"
  )


receiver_quant_final_params =
  receiver_quant_final_tuning$best_parameters


# data prep

receiver_quant_final_data =
  prepare_xgb_data(
    train_data = receiver_train,
    test_data = receiver_predict_2026,
    features = receiver_features
  )

receiver_quant_dtrain =
  xgb.DMatrix(
    data = receiver_quant_final_data$x_train,
    label = receiver_train$target_ppr_17,
    missing = NA
  )

receiver_quant_dtest =
  xgb.DMatrix(
    data = receiver_quant_final_data$x_test,
    missing = NA
  )

# fit
receiver_quant_params =
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
      receiver_quant_final_params$max_depth,
    
    learning_rate =
      receiver_quant_final_params$learning_rate,
    
    min_child_weight =
      receiver_quant_final_params$min_child_weight,
    
    subsample =
      receiver_quant_final_params$subsample,
    
    colsample_bytree =
      receiver_quant_final_params$colsample_bytree,
    
    reg_lambda = 1,
    reg_alpha = 0
  )
set.seed(57)

receiver_quant_final_fit =
  xgb.train(
    params = receiver_quant_params,
    data = receiver_quant_dtrain,
    nrounds = receiver_quant_final_params$nrounds,
    verbose = 0
  )

# predict
receiver_2026_quantiles =
  predict(
    receiver_quant_final_fit,
    receiver_quant_dtest
  )


# FINAL PREDICTION DATASET 

receiver_2026_predictions =
  receiver_predict_2026 %>%
  transmute(
    player_id,
    player_display_name,
    position,
    draft_team,
    
    projected_ppr_17 =
      receiver_2026_mean,
    
    q20_raw =
      receiver_2026_quantiles[, 1],
    
    q50_raw =
      receiver_2026_quantiles[, 2],
    
    q80_raw =
      receiver_2026_quantiles[, 3]
  )


receiver_2026_predictions =
  receiver_2026_predictions %>%
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

receiver_2026_predictions =
  receiver_2026_predictions %>%
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


# PPG
receiver_2026_predictions =
  receiver_2026_predictions %>%
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


# SEPERATE WR / TE RANKINGS

receiver_2026_predictions =
  receiver_2026_predictions %>%
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



# receiver
receiver_2026_predictions %>%
  filter(position == "WR") %>%
  arrange(position_rank_mean)

# tight end
receiver_2026_predictions %>%
  filter(position == "TE") %>%
  arrange(position_rank_mean)


# FINAL LARGE PREDICTION DATASET 
receiver_2026_final =
  receiver_2026_predictions %>%
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
  arrange(
    position,
    position_rank_mean
  )

write_csv(
  receiver_2026_final,
  "receiver_2026_predictions.csv"
)


