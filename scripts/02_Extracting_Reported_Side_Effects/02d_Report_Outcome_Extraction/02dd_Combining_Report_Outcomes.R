## 
# Combining Extracted outcomes from reports
# 1. UKTIS Monographs
# 2. TERIS
# 3. Reprotox
##

#######################################################
# Load libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")

uktis_dir <- file.path(interim_data, "reported_outcomes/UKTIS_outcomes/5_Final_Outcomes")
teris_dir <- file.path(interim_data, "reported_outcomes/TERIS_outcomes/5_Final_Outcomes")
reprotox_dir <- file.path(interim_data, "reported_outcomes/Reprotox_outcomes/5_Final_Outcomes")

output_dir <- file.path(interim_data, "reported_outcomes/All_report_outcomes")

#######################################################
# UKTIS Monograph Extracted Outcomes
#######################################################

# For each drug
# Somehow link the file names to the drugs
# Read in extracted outcomes
# Normalise dataset to combine with other tw  o
#   Maybe by defining the column names first?

uktis_outcomes <- fread(file.path(uktis_dir, "THALIDOMIDE_LENALIDOMIDE_AND_POMALIDOMIDE_combined_outcomes.csv"))

