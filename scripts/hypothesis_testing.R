# Probability and Statistics
# Final Assessment: Statistical Hypothesis Testing


# Load packages ---------------------------------------------------------------
library(dplyr) # for pipes and manipulation
library(rstatix) # for a t_test (wrapper for t.test)

# Load data -------------------------------------------------------------------

# Load merged dataset and set the first column as index
biomarker <- read.csv("data/biomarkers_covariates_clean.csv", row.names = 1)

# Hypothesis ------------------------------------------------------------------

# Question:
# Do the levels of each biomarker vary between males and females at inclusion?

# Hypotheses:
# H0: Mean level of biomarker X at inclusion is the same for males and females.
# H0: μ1 = μ2.

# H1: Mean level of biomarker X at inclusion differs between males and females.
# H1: μ1 ≠ μ2

# Welch two sample t-tests ----------------------------------------------------

# Use the Welch test to compare the means of each of 9 biomarkers

# To perform t-tests in a single loop, convert data from wide to long format 
# The script to perform all 9 tests together was taken from Datanovia (2020).
biomarker_long <- biomarker %>%
  select(il_8:csf_1,sex) %>% # remove columns that won't be used in testing
  pivot_longer(-sex, names_to = "biomarker", values_to = "value")

# See the long dataset
biomarker_long

# Construct Welch t-tests for all 9 biomarkers
h_test <- biomarker_long %>%
  group_by(biomarker) %>%
  t_test(value ~ sex, detailed = TRUE)

# See test results
h_test

# Polish the table
h_test_tbl <- h_test %>% 
  # select columns to report
  select(biomarker, estimate, estimate1, estimate2, p, conf.low, conf.high) %>%
  # adjust number of significant figures
  mutate(
    estimate = round(estimate, digits = 3),
    estimate1 = round(estimate1, digits = 2),
    estimate2 = round(estimate2, digits = 2),
    p = round(p, digits = 3),
    conf.low = round(conf.low, digits = 2),
    conf.high = round(conf.high, digits = 2)
    ) %>%
  # rename columns to meaningful names
  rename(
    c(difference = estimate,
      male = estimate1,
      female = estimate2)
    )

h_test_tbl

# Bonferroni Correction ------------------------------------------------------

# The probability of making a Type I error is 0.05 across all 9 t-tests, lowering 
# the p-value necessary to reach significance for each individual test to 0.006.

# Adjust the hypothesis test with Bonferroni correction
h_test_bonferroni <- biomarker_long %>%
  group_by(biomarker) %>%
  t_test(value ~ sex, detailed = TRUE) %>%
  adjust_pvalue(method = "bonferroni")

# See test results
h_test_bonferroni

# Create a table for test results with Bonferroni correction
bonferroni_tbl <- h_test_bonferroni %>% 
  # select columns to report
  select(biomarker, estimate, estimate1, estimate2, p.adj, conf.low, conf.high) %>%
  # adjust number of significant figures
  mutate(
    estimate = round(estimate, digits = 3),
    estimate1 = round(estimate1, digits = 2),
    estimate2 = round(estimate2, digits = 2),
    p.adj = round(p.adj, digits = 3),
    conf.low = round(conf.low, digits = 2),
    conf.high = round(conf.high, digits = 2)
  ) %>%
  # rename columns to meaningful names
  rename(
    c(difference = estimate,
      male = estimate1,
      female = estimate2,
      p_adj = p.adj)
  )


# See table
bonferroni_tbl

# None of the tests fulfill significance for the alternative hypothesis with
# Bonferroni correction.

# Save tables for report -----------------------------------------------------
write.csv(h_test_tbl, "data/hypotheses_tests_table.csv")
write.csv(bonferroni_tbl, "data/hypotheses_tests_table_bonferroni.csv")

