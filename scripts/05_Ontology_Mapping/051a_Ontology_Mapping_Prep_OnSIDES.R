##
# Formatting outcome datasets for ontology mapping - OnSIDES
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

onsides_input_dir <- file.path(interim_data, "reported_outcomes/OnSIDES_outcomes")
onsides_output_dir <- file.path(interim_data, "ontology_mapping/input_data")

#######################################################
# Read in OnSIDES outcomes
#######################################################
# Going to use preferred terms

onsides_outcomes <- fread(file.path(onsides_input_dir, "OnSIDES_reported_outcomes.csv")) %>% 
  filter(meddra_term_type == "PT")

#######################################################
# Save outcomes to separate drug files
#######################################################

for (i in unique(onsides_outcomes$ingredient_name)){
  drug_se <- onsides_outcomes[onsides_outcomes$ingredient_name == i, c(1, 13)]
  drug_se <- unique(drug_se)
  colnames(drug_se)[2] <- "Outcome"
  
  output_dir <- file.path(onsides_output_dir, str_replace(i, " ", "_"))
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  fwrite(drug_se, file.path(output_dir, paste0(str_replace(i, " ", "_"), "_onsides.csv")))
}
