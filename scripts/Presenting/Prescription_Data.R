##
# Summarising the number of prescriptions for each drug for years 2022-2025 and yearly average
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)
library(stringr)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")
processed_data <- Sys.getenv("processeddatadir")

#######################################################
# Prescription Data
#######################################################

yearly_prescriptions <- fread(file.path(interim_data, "prescription_data/drug_yearly_prescriptions.csv"))

yearly_prescriptions <- yearly_prescriptions[,-1]

yearly_prescriptions <- yearly_prescriptions %>%
  arrange(Category, desc(Median))

#######################################################
# Save
#######################################################

fwrite(yearly_prescriptions, file.path(processed_data, "Presenting/Prescription_data.csv"))
