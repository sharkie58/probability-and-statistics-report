# Probability and Statistics
# Final Assessment: Data Manipulation Script

# Load packages -----------------------------------------------------------
library(readxl) # for reading excel files
library(dplyr) # for pipe operators and manipulating data frames
library(tidyr) # for manipulating data frames, separate_wider_delim()
library(janitor) # for cleaning column names


# Load data ---------------------------------------------------------------
biomarkers <- read_excel("data/biomarkers.xlsx")
covariates <- read_excel("data/covariates.xlsx")

# Explore data ------------------------------------------------------------
biomarkers
covariates


# Connect biomarkers with covariates datasets -----------------------------

# Separate the column "Biomarkers" to two columns "PatientID" and "Time"
biomarkers_id <- separate_wider_delim(biomarkers, 
                                      Biomarker, 
                                      delim = "-", 
                                      names = c("PatientID", "Time"))
biomarkers_id

# Convert Patient ID in both datasets to numeric
covariates$PatientID <- as.numeric(covariates$PatientID)
biomarkers_id$PatientID <- as.numeric(biomarkers_id$PatientID)

# Calculate number of patients in each dataset
n_patients <- length(unique(biomarkers_id$PatientID))
sprintf("The number of patients in the biomarkers dataset is %s.", n_patients)

# Check that the number of patients in the covariates dataset is the same
n_patients2 <- length(unique(covariates$PatientID))
sprintf("The number of patients in the covariates dataset is %s.", n_patients2)

# Left join datasets to only keep data for patients with biomarker measurements
full_dataset <- left_join(biomarkers_id, covariates, by = "PatientID")
summary(full_dataset)


# Prepare data for Statistical Analysis and Regression Modelling ----------

# Create a clean dataset of biomarkers at inclusion and all covariates
full_dataset_clean <- full_dataset %>%
  
  # Limit Time to only at inclusion
  filter(Time == "0weeks") %>%

  # Remove redundant Time column
  select(-Time) %>%
  
  # Reorder the dataset by PatientID
  arrange(PatientID) %>%
  
  # Rename columns with long names
  rename("Sex" = "Sex (1=male, 2=female)",
         "Smoker" = "Smoker (1=yes, 2=no)",
         "VAS-0" = "VAS-at-inclusion",
         "VAS-12" = "Vas-12months") %>%
  
  # Clean all column names with the janitor package
  clean_names()


# Check that the number of patients in the clean dataset is the same
n_patients3 <- length(unique(full_dataset_clean$patient_id))
sprintf("The number of patients in the clean dataset is %s.", n_patients3)

# 1 patient is missing data for inclusion.

# Check for NAs
nas <- sum(is.na(full_dataset_clean))
sprintf("There are %s NAs in full_dataset_clean.", nas)

# Find the NAs
colSums(is.na(full_dataset_clean)) # 2 missing values in VAS after 12 months.

# Check distributions -----------------------------------------------------

female <- full_dataset_clean %>%
  filter(sex == 2)

hist(female$il_8) # normal
hist(female$vegf_a) # normal
hist(female$opg) # normal
hist(female$tgf_beta_1) # not normal
hist(female$il_6) # right skew
hist(female$cxcl9) # right skew
hist(female$cxcl1) # left skew
hist(female$il_18) # normal
hist(female$csf_1) # normal

male <- full_dataset_clean %>%
  filter(sex == 1)

hist(male$il_8) # normal
hist(male$vegf_a) # normal
hist(male$opg) # normal
hist(male$tgf_beta_1) # not normal
hist(male$il_6) # right skew
hist(male$cxcl9) # right skew
hist(male$cxcl1) # left skew
hist(male$il_18) # not normal
hist(male$csf_1) # normal

# Save data ---------------------------------------------------------------
write.csv(full_dataset_clean, "data/biomarkers_covariates_clean.csv")

