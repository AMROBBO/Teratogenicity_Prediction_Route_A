##
# Reading in suggested drug side effects from Bumps 
# and assigning to teratogenic drugs of interest
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

raw_data <- Sys.getenv("rawdatadir")
interim_data <- Sys.getenv("interimdatadir")

drug_list_file <- file.path(interim_data, "teratogenic_drugs.txt")

onsides_raw <- file.path(raw_data, "drug_extracted_data")
onsides_output <- file.path(interim_data, "reported_outcomes/Bumps_outcomes")

#######################################################
# Reading in teratogenic drug list of interest
#######################################################

drug_list <- readLines(drug_list_file)
