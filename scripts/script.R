# Probability and Statistics
# Final Assessment
# exam number?

# install.packages("tidyverse")
library(readxl) # for reading excel files
library(dplyr) # for pipe operators and manipulating data frames
library(tidyr) # for manipulating data frames

# Exploratory analysis

# Load data
biomarkers <- read_excel("data/biomarkers.xlsx")
covariates <- read_excel("data/covariates.xlsx")

# Explore data
biomarkers
covariates

# Separate the column "Biomarkers" to two columns "PatientID" and "Time"
biomarkers_id <- separate_wider_delim(biomarkers, 
                                      Biomarker, 
                                      delim = "-", 
                                      names = c("PatientID", "Time"))
biomarkers_id

# Calculate number of patients in each dataset
n_patients <- length(unique(biomarkers_id$PatientID))
sprintf("The number of patients in the biomarkers dataset is %s.", n_patients)

# Check that the number of patients in the covariates dataset is the same
n_patients2 <- length(unique(covariates$PatientID))
sprintf("The number of patients in the covariates dataset is %s.", n_patients2)

# Choice of question for Part 1: Statistical Analysis
# Do the biomarker levels at inclusion for patients with high VAS (≥ 5) differ 
# from those for patients with low VAS (< 5)?

# To answer this, we need to connect the two datasets

# Convert Patient ID in covariates to character
covariates$PatientID <- as.numeric(covariates$PatientID)
biomarkers_id$PatientID <- as.numeric(biomarkers_id$PatientID)

full_dataset <- full_join(biomarkers_id, covariates, by = "PatientID")
summary(full_dataset)
