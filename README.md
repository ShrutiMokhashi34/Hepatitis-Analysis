# Prediction Model for Analysis of Hepatitis Dataset for Diagnostic Accuracy

Predicting patient survival from clinical data using a Random Forest model in R,
with a focus on handling incomplete data before modeling.

**Tools:** R (mice, Amelia, randomForest, ggplot2)

## Overview
- **Dataset:** 156 patients, 19 clinical attributes (symptoms and lab values), with a survival outcome.
- **Data quality:** 48% of patient records had at least one missing value, mostly in lab results. Instead of dropping them, missing values were filled using MICE (Multiple Imputation by Chained Equations).
- **Modeling:** Trained a Random Forest classifier and ranked all 19 variables by their importance in predicting survival.

## Files
| File | Description |
|---|---|
| `Original data with missing values.csv` | Raw dataset |
| `Consolidated Data 1.csv` | Dataset after imputation |
| `hepatitis_random_forest.R` | Imputation and modeling script |
| `*.jpeg` | Missingness maps, model error plot, variable importance |
