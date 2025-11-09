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


# Connect biomarkers with covariates --------------------------------------

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



# Prepare data for Part 1: Statistical Analysis ---------------------------

# Question: Do the levels at inclusion vary between males and females?

# Create a clean dataset with a set of the relevant variables at inclusion
inclusion <- full_dataset %>%
  # Select relevant columns
  select(`PatientID`:`CSF-1`, `Sex (1=male, 2=female)`) %>%
  
  # Limit Time to only at inclusion
  filter(Time == "0weeks") %>%

  # Remove redundant Time column
  select(-Time) %>%
  
  # Reorder the dataset by PatientID
  arrange(PatientID) %>%
  
  # Rename VAS column and sex columns
  rename("Sex" = "Sex (1=male, 2=female)")

# Check that the number of patients in the clean dataset is the same
n_patients3 <- length(unique(biomarkers_vas$PatientID))
sprintf("The number of patients in the clean dataset is %s.", n_patients3)

# 1 patient is missing - no data for inclusion.



