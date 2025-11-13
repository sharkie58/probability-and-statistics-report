# Probability and Statistics
# Final Assessment: Regression Modelling

# Load packages --------------------------------------------------------------
library(rsample) # for splitting data into test and training datasets
library(ggplot2) # for plotting data
library(ggfortify) # for checking assumptions
library(gtsummary) # for regression summary table
library(broom.helpers) # for regression summary table
library(gt) # to save gtsummary tables


# Load data ------------------------------------------------------------------

# Load merged dataset and set the first column as index
biomarker <- read.csv("data/biomarkers_covariates_clean.csv", row.names = 1)

# Split data ---------------------------------------------------------------

# Set seed for reproduction of random sampling 
set.seed(1212) 

# Perform a random 80/20 split
split <- initial_split(biomarker, prop = 0.8) 

# 80% of values in training dataset
train <- training(split)

# 20% of calues in testing dataset
test <- testing(split)


# Construct a multiple regression model --------------------------------------

# Response variable: 12 month VAS 
# Explanatory variables: all 9 biomarker levels at inclusion, age, sex, smoker 
# and VAS at inclusion

# 2 observations with missing VAS at 12 months deleted.

# Fit the model
model <- lm(
  vas_12 ~ il_8 + vegf_a + opg + tgf_beta_1 + il_6 + cxcl9 + cxcl1 + il_18 + 
    csf_1 + age + sex + smoker + vas_0, 
  data = train
  )

# See model summary
summary(model)

# Evaluate the model by checking assumptions  --------------------------------

# Check assumptions (taken from Thulin (2025)):

# Plot residuals using ggfortify package:
residual_plots <- autoplot(model, which = 1:6, ncol = 2, label.size = 3)
residual_plots

# 1. The model is linear in the parameters 
# Assumption fulfilled: Residuals vs fitted show a straight line.

# 2. The observations are independent
# We assume they are independent as this is harder to assess visually.

# 3. Homoscedasticity (Random errors have the same variance)
# Assumption fulfilled: Scale-Location plot shows approximately even spread of residuals

# 4. Normally distributed random errors
# Assumption fulfilled: Residuals (estimates of random errors) follow a normal 
# distribution shown in the Normal QQ plot.


# Create a table for regression results for training data---------------------

# Create table
regression_tbl <- tbl_regression(model,
                                 intercept = TRUE,
                                 conf.level = 0.95,
                                 tidy_fun = broom.helpers::tidy_with_broom_or_parameters)
# See table
regression_tbl

# Generate predictions and compare to actual test data ------------------------
predictions <- predict(model, test)

pred_plot <- ggplot(test, aes(x=predictions, y=vas_12)) +
  geom_point() +
  theme_classic() +
  geom_abline(intercept=0, 
              slope=1,
              colour = "red") +
  labs(x='Predicted VAS (1-10)', y='Actual VAS (1-10)')

pred_plot

# Check RMSE
sqrt(mean((test$vas_12 - predictions)^2))


# Create other models with less explanatory variables (not included in report) ----

# Create a model with only variables that have a significant relationship with vas_12
model_narrow1 <- lm(
  vas_12 ~ il_8 + opg + tgf_beta_1 + il_6 + vas_0, 
  data = train
)

# See model 1 summary
summary(model_narrow1)

# The p-values of il_8 and tgf_beta_1 having a relationship with vas_12 are not 
# significant in the second model. 

model_narrow2 <- lm(
  vas_12 ~ opg + il_6 + vas_0, 
  data = train
)

# See model 2 summary
summary(model_narrow2)

# Discard opg from the model as it is not significant in the last model.
model_narrow3 <- lm(
  vas_12 ~ il_6 + vas_0, 
  data = train
)

# See model 3 summary
summary(model_narrow3)

# The highest Adjusted R-squared was reported in the first model including all
# biomarker levels and all covariates.

# Create results table with model_narrow3 ------------------------------------
regression3_tbl <- tbl_regression(model_narrow3,
                                 intercept = TRUE,
                                 conf.level = 0.95,
                                 tidy_fun = broom.helpers::tidy_with_broom_or_parameters)
# See table
regression3_tbl

# Fit model 3 to test data ---------------------------------------------------
predictions_model3 <- predict(model_narrow3, test)

pred3_plot <- ggplot(test, aes(x=predictions_model3, y=vas_12)) +
  geom_point() +
  theme_classic() +
  geom_abline(intercept=0, 
              slope=1,
              colour = "red") +
  labs(x='Predicted VAS (1-10)', y='Actual VAS (1-10)')

pred3_plot

# Check RMSE
sqrt(mean((test$vas_12 - predictions_model3)^2))

# Save results ---------------------------------------------------------------

regression_tbl %>%
  as_gt() %>%
  gt::gtsave('figures/regression_table.png')

regression3_tbl %>%
  as_gt() %>%
  gt::gtsave('figures/regression_table_small_model.png')

ggsave(residual_plots,
       filename = "figures/residual_plots.png",
       device = "png")

ggsave(pred_plot, 
       filename = "figures/prediction_vs_actual.png",
       device = "png")

ggsave(pred3_plot,
       filename = "figures/prediction_vs_actual_smaller_model.png",
       device = "png")

