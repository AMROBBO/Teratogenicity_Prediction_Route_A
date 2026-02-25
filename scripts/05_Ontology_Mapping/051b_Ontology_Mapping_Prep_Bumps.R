##
# Formatting outcome datasets for ontology mapping - Bumps
##

#######################################################
# Load in libraries
#######################################################

library(dplyr)
library(tidyr)
library(readr)
library(data.table)
library(tidyverse)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")

bumps_input_dir <- file.path(interim_data, "reported_outcomes/Bumps_outcomes")
bumps_output_dir <- file.path(interim_data, "ontology_mapping/input_data")

#######################################################
# Read in Bumps Outcome Datas
#######################################################

bumps_outcomes <- fread(file.path(bumps_input_dir, "Bumps_reported_outcomes.csv"))

#######################################################
# Save outcomes to separate drug files
#######################################################

for (i in unique(bumps_outcomes$Drug)){
  outcome <- bumps_outcomes[bumps_outcomes$Drug == i, 1]
  
  output_dir <- file.path(bumps_output_dir, str_replace(tolower(i), " ", "_"))
  
  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  }
  
  output <- file.path(output_dir, paste0(str_replace(tolower(i), " ", "_"), "_bumps.csv"))
  
  fwrite(outcome, output)
}
