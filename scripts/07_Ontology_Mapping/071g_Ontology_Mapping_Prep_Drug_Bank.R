##
# Formatting outcome datasets for ontology mapping - OnSIDES
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
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

drugbank_input_dir <- file.path(interim_data, "reported_outcomes/Drug_Bank_outcomes")
drugbank_output_dir <- file.path(interim_data, "ontology_mapping/input_data")

#######################################################
# Read in Drug Bank Indications
#######################################################

drugbank_outcomes <- fread(file.path(drugbank_input_dir, "Drug_Bank_Indications.csv"))

#######################################################
# Save outcomes to separate drug files
#######################################################

for (i in unique(drugbank_outcomes$Drug)){
  
  indication <- drugbank_outcomes %>% 
    filter(Drug == i) %>% 
    select(Indication) %>% 
    unique()
  colnames(indication) <- "Outcome"
  
  if (i == "Folate"){
    i <- "folic_acid"
  }
  
  output_dir <- file.path(drugbank_output_dir, tolower(i))
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  fwrite(indication, file.path(output_dir, paste0(tolower(i), "_drugbank.csv")))
}
