# Fantasy Football Draft Model

A machine learning project built to create a personalized draft board for a 10-team PPR fantasy football league.

The project predicts next-season fantasy production for quarterbacks, running backs, wide receivers, and tight ends. Along with expected production, quantile regression is used to estimate each player's **floor (Q20), median (Q50), and ceiling (Q80)**.

## Project Goals

* Project next-season fantasy production
* Estimate player floor, median, and ceiling outcomes
* Create positional rankings and draft tiers
* Compare model rankings with ADP to identify potential value
* Incorporate positional value and roster construction into draft decisions

## Data & Modeling

NFL data was collected for the **2016–2026 seasons** from multiple sources, including player statistics, opportunity metrics, snap counts, injuries, team performance, rosters, coaching changes, red-zone usage, and Next Gen Stats.

The primary modeling dataset uses **2019–2026 data** due to data inconsistency for important features in earlier seasons. Additional historical data was collected with future projects and analyses in mind and is not necessarily used in the current models.

Separate models were developed by position group using:

* Elastic Net
* XGBoost
* Quantile XGBoost
* Expanding year-by-year validation

## Model Performance

Historical performance was evaluated using expanding year-by-year validation.

| Position Group | Model       | RMSE |  MAE |    R² | Spearman |
| -------------- | ----------- | ---: | ---: | ----: | -------: |
| RB             | Elastic Net | 61.8 | 48.4 | 0.605 |    0.776 |
| WR/TE          | Elastic Net | 48.8 | 38.6 | 0.682 |    0.798 |
| QB             | XGBoost     | 90.6 | 70.8 | 0.452 |    0.684 |

Quantile models were also evaluated using pinball loss and prediction interval coverage to assess the quality of floor, median, and ceiling estimates.

## Final Outputs

The project produces:

* 2026 player projections
* Floor, median, and ceiling rankings
* Positional rankings and tiers
* Model vs. ADP comparisons
* Draft value analysis
* Final fantasy draft board

## Tools

**R • XGBoost • glmnet • nflverse • Excel**
