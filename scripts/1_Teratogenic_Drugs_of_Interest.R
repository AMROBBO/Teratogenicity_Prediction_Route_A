##
# Collating manually extracted files to create a list of teratogenic drugs
# of interest
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(dplyr)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

raw_data <- Sys.getenv("rawdatadir")
interim_data <- Sys.getenv("interimdatadir")

input_data <- file.path(raw_data, "drug_extracted_data")
output_data <- file.path(interim_data, "teratogenic_drugs.txt")

#######################################################
# Creating list of Teratogenic drugs to look at
#######################################################

drug_list <- list.files(input_data, pattern = ".csv") %>%
  as.list() %>%
  gsub(pattern = ".csv$", replacement = "")

#######################################################
# Save
#######################################################

writeLines(drug_list, output_data)
