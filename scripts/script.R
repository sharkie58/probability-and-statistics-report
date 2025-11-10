# Probability and Statistics
# Final Assessment: Data Manipulation Script



# Load packages -----------------------------------------------------------
library(readxl) # for reading excel files
library(dplyr) # for pipe operators and manipulating data frames
library(tidyr) # for manipulating data frames, separate_wider_delim()


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
         "VAS-12" = "Vas-12months")


# Check that the number of patients in the clean dataset is the same
n_patients3 <- length(unique(full_dataset_clean$PatientID))
sprintf("The number of patients in the clean dataset is %s.", n_patients3)

# 1 patient is missing data for inclusion.


# Save data ---------------------------------------------------------------
write.csv(full_dataset_clean, "data/biomarkers_covariates_clean.csv")
