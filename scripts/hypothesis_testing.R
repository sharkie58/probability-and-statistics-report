# Probability and Statistics
# Final Assessment: Statistical Hypothesis Testing


# Load packages ---------------------------------------------------------------
library(dplyr) # for pipes and manipulation

# Load data -------------------------------------------------------------------
biomarker <- read.csv("data/biomarkers_covariates_clean.csv")

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

# 1. Biomarker IL-8
biomarker_test_il_8 <- t.test(il_8 ~ sex, biomarker)
biomarker_test_il_8 # H1 p-value 0.33

# 2. Biomarker VEGF-A
biomarker_test_vegf_a <- t.test(vegf_a ~ sex, biomarker)
biomarker_test_vegf_a # H1 p-value 0.04

# 3. Biomarker OPG
biomarker_test_opg <- t.test(opg ~ sex, biomarker)
biomarker_test_opg # H1 p-value 0.13

# 4. Biomarker TGF-beta-1
biomarker_test_tgf_beta_1 <- t.test(tgf_beta_1 ~ sex, biomarker)
biomarker_test_tgf_beta_1 # H1 p-value 0.05

# 5. Biomarker IL-6
biomarker_test_il_6 <- t.test(il_6 ~ sex, biomarker)
biomarker_test_il_6 # H1 p-value 0.25

# 6. Biomarker CXCL9
biomarker_test_cxcl9 <- t.test(cxcl9 ~ sex, biomarker)
biomarker_test_cxcl9 # H1 p-value 0.98

# 7. Biomarker CXCL1
biomarker_test_cxcl1 <- t.test(cxcl1 ~ sex, biomarker)
biomarker_test_cxcl1 # H1 p-value 0.01

# 8. Biomarker IL-18
biomarker_test_il_18 <- t.test(il_18 ~ sex, biomarker)
biomarker_test_il_18 # H1 p-value 0.25

# 9. Biomarker CSF-1
biomarker_test_csf_1 <- t.test(csf_1 ~ sex, biomarker)
biomarker_test_csf_1 # H1 p-value 0.01
