# Probability and Statistics
# Final Assessment: Regression Modelling

# Load packages --------------------------------------------------------------
library(rsample) # for splitting data into test and training datasets
library(ggplot2) # for plotting data


# Load data ------------------------------------------------------------------

# Load merged dataset and set the first column as index
biomarker <- read.csv("data/biomarkers_covariates_clean.csv", row.names = 1)

# Split data ---------------------------------------------------------------
set.seed(1212) # set seed for reproduction of random sampling 
split <- initial_split(biomarker, prop = 0.8) # perform a random 80/20 split
train <- training(split) # 80% of values in training dataset
test <- testing(split) # 20% of calues in testing dataset

# Plot the data without a model
ggplot(biomarker, 
       aes(il_8, vegf_a, opg, tgf_beta_1, il_6, cxcl9, cxcl1, il_18, 
           csf_, age, sex, smoker, vas_0, vas_12))
geom_point()

# Construct a multiple regression model --------------------------------------

# Response variable: 12 month VAS 
# Explanatory variables: all 9 biomarker levels at inclusion, age, sex, smoker 
# and VAS at inclusion
# 2 NA observations deleted.

model <- lm(
  vas_12 ~ il_8 + vegf_a + opg + tgf_beta_1 + il_6 + cxcl9 + cxcl1 + il_18 + 
    csf_1 + age + sex + smoker + vas_0, 
  data = train
  )

# See model summary
summary(model)

# Create a model with only variables that have a signifficant relationship with vas_12
model_narrow <- lm(
  vas_12 ~ il_8 + opg + tgf_beta_1 + il_6 + vas_0, 
  data = train
  )

# See model summary
summary(model_narrow)

# the p-values of il_8 and tgf_beta_1 having a relationship with vas_12 are not 
# significant in the second model. 

model_narrow2 <- lm(
  vas_12 ~ opg + il_6 + vas_0, 
  data = train
)

# See model summary
summary(model_narrow2)

# Discard opg from the model as it is not significant in the last model.
model_narrow3 <- lm(
  vas_12 ~ il_6 + vas_0, 
  data = train
)

# See model summary
summary(model_narrow3)

# The highest R-squared was reported in the first model including all biomarker 
# levels and all covariates.

# Evaluate the model by checking assumptions  --------------------------------


# Check assumptions (taken from Thulin (2025)):
# 1. The model is linear in the parameters

# Get the model residuals
model_residuals = model$residuals

# Plot the residuals
hist(model_residuals) # residuals appear normal

# Plot the residuals on a plot with a QQ line
qqnorm(model_residuals)
qqline(model_residuals) # residuals follow the QQ line, suggesting they are normal


# Create a table for regression results --------------------------------------


# Plot regression model ------------------------------------------------------



# Save results ---------------------------------------------------------------







