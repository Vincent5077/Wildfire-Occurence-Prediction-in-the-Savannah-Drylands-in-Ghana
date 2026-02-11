# ============================================================
# FIRE MODELLING: RANDOM FOREST WITH ALL PREDICTORS
# ============================================================

# Libraries
library(data.table)
library(caret)
library(ranger)
library(ggplot2)

# ------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------
data <- fread("D:/EDU DOCS/Peoples Project/Fire Modelling/merged_fire_data_2025_2000.csv")

# ------------------------------------------------------------
# 2. SELECT VARIABLES
# ------------------------------------------------------------
features <- c(
  'brightness', 'bright_t31', "Temperature_C", 
  "elev", "roads", "nbr", "ndvi",
  "ndwi", "precip", "slope", "settlement",
  "wind",
  "aspect", "Lulc", "confidence"
)
target <- "frp"

data <- data[, c(features, target), with = FALSE]

# ------------------------------------------------------------
# 3. DATA TYPE FIXES
# ------------------------------------------------------------
data[, Lulc := as.factor(Lulc)]
num_cols <- setdiff(features, "Lulc")
data[, (num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
data[, frp := as.numeric(frp)]

# ------------------------------------------------------------
# 4. CLEANING
# ------------------------------------------------------------
data <- na.omit(data)

# Cap extreme FRP values (optional)
cap <- quantile(data$frp, 0.99, na.rm = TRUE)
data[frp > cap, frp := cap]

# ------------------------------------------------------------
# 5. TRAIN–TEST SPLIT
# ------------------------------------------------------------
set.seed(123)
train_index <- createDataPartition(data$frp, p = 0.7, list = FALSE)
train <- data[train_index]
test  <- data[-train_index]

# ------------------------------------------------------------
# 6. RANDOM FOREST MODEL (ALL PREDICTORS)
# ------------------------------------------------------------
final_predictors <- c(num_cols, "Lulc")
final_formula <- as.formula(paste("frp ~", paste(final_predictors, collapse = " + ")))

rf_model <- ranger(
  final_formula,
  data = train,
  num.trees = 500,                  # increase for stability
  importance = "impurity",
  respect.unordered.factors = "order"
)

# ------------------------------------------------------------
# 7. PREDICTION
# ------------------------------------------------------------
pred <- predict(rf_model, data = test)$predictions
obs  <- test$frp

# ------------------------------------------------------------
# 8. MODEL EVALUATION
# ------------------------------------------------------------
errors <- pred - obs
MSE  <- mean(errors^2)
RMSE <- sqrt(MSE)
MAE  <- mean(abs(errors))
SSE <- sum(errors^2)
SST <- sum((obs - mean(obs))^2)
R2  <- 1 - (SSE / SST)

metrics <- list(
  MSE  = MSE,
  RMSE = RMSE,
  MAE  = MAE,
  R2   = R2
)
print(metrics)

# ------------------------------------------------------------
# 9. OBSERVED vs PREDICTED PLOT
# ------------------------------------------------------------
plot_df <- data.table(Observed = obs, Predicted = pred)

ggplot(plot_df, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  labs(
    title = "Observed vs Predicted FRP (Random Forest)",
    subtitle = paste0(
      "R² = ", round(R2, 3),
      " | RMSE = ", round(RMSE, 2),
      " | MAE = ", round(MAE, 2)
    ),
    x = "Observed FRP",
    y = "Predicted FRP"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 10. FEATURE IMPORTANCE
# ------------------------------------------------------------
feat_imp <- as.data.table(rf_model$variable.importance, keep.rownames = "Feature")
setnames(feat_imp, c("Feature", "Importance"))
feat_imp <- feat_imp[order(-Importance)]
print(feat_imp)

# Plot feature importance
ggplot(feat_imp, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Random Forest Feature Importance (All Predictors)",
    x = "Predictor",
    y = "Importance (Impurity)"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 11. SELECT TOP 50% IMPORTANT FEATURES
# ------------------------------------------------------------

feat_imp <- as.data.table(rf_model$variable.importance, keep.rownames = "Feature")
setnames(feat_imp, c("Feature", "Importance"))
feat_imp <- feat_imp[order(-Importance)]

# Number of predictors to keep (top 50%)
n_keep <- ceiling(nrow(feat_imp) * 0.5)

top_features <- feat_imp[1:n_keep, Feature]

print("Top predictors selected:")
print(top_features)


# ------------------------------------------------------------
# 12. RETRAIN MODEL USING TOP PREDICTORS
# ------------------------------------------------------------

top_formula <- as.formula(
  paste("frp ~", paste(top_features, collapse = " + "))
)

rf_model_top <- ranger(
  top_formula,
  data = train,
  num.trees = 500,
  importance = "impurity",
  respect.unordered.factors = "order"
)

# ------------------------------------------------------------
# 13. PREDICTION & METRICS (TOP FEATURES)
# ------------------------------------------------------------

pred_top <- predict(rf_model_top, data = test)$predictions
obs_top  <- test$frp

errors_top <- pred_top - obs_top

MSE_top  <- mean(errors_top^2)
RMSE_top <- sqrt(MSE_top)
MAE_top  <- mean(abs(errors_top))
SSE_top  <- sum(errors_top^2)
SST_top  <- sum((obs_top - mean(obs_top))^2)
R2_top   <- 1 - (SSE_top / SST_top)

metrics_top <- list(
  MSE  = MSE_top,
  RMSE = RMSE_top,
  MAE  = MAE_top,
  R2   = R2_top
)

print("Performance using top 50% predictors:")
print(metrics_top)
