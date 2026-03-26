##
# Reading in reported side effects emc smPCs and assigning to teratogenic
# drugs of interest
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)
library(drugbankR)
library(tidyr)
library(tidyverse)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")

emc_input_dir <- file.path(interim_data, "reported_outcomes/SmPC_outcomes")
emc_output_dir <- file.path(interim_data, "ontology_mapping/input_data")

#######################################################
# Read in SmPC outcomes
#######################################################

smpc_outcomes <- fread(file.path(emc_input_dir, "SmPC_Drug_outcomes.csv")) 

#######################################################
# Exploring outcomes
#######################################################

outcome_summary <- as.data.frame(table(smpc_outcomes$`Associated Outcomes`))

#######################################################
# Save outcomes to separate drug files
#######################################################

for (i in unique(smpc_outcomes$Drug)){
  drug_se <- smpc_outcomes[smpc_outcomes$Drug == i, 1:2]
  drug_se <- unique(drug_se)
  colnames(drug_se)[2] <- "Outcome"
  
  output_dir <- file.path(emc_output_dir, str_replace(tolower(i), " ", "_"))
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  fwrite(drug_se, file.path(output_dir, paste0(str_replace(tolower(i), " ", "_"), "_smpc.csv")))
}
