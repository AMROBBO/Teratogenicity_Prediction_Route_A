##
# Formatting outcome datasets for ontology mapping - OMIM
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

omim_input_dir <- file.path(interim_data, "predicted_outcomes/OMIM_outcomes")
omim_output_dir <- file.path(interim_data, "ontology_mapping/input_data")

#######################################################
# Reading in OMIM outcomes
#######################################################

omim_outcomes <- fread(file.path(omim_input_dir, "OMIM_outcomes_processed.csv"))

#######################################################
# Save outcomes to separate drug files
#######################################################

for (i in unique(omim_outcomes$Drug)){
  # Extract outcomes
  outcome <- omim_outcomes %>% 
    filter(Drug == i) %>% 
    select(Outcome)
  
  # Save
  output_dir <- file.path(omim_output_dir, str_replace(tolower(i), " ", "_"))
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  output <- file.path(output_dir, paste0(str_replace(tolower(i), " ", "_"), "_omim.csv"))
  
  fwrite(outcome, output)
}
