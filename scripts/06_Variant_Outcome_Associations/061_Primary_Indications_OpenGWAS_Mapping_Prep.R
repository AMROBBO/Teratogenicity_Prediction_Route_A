##
# Using openGWAS data to test association between drug target gene
# and primary indications of drugs
# 1. Read in primary drug indications sourced from DrugBank
# 2. Identify those that can be genetically proxied
# 3. Extract all available outcomes from openGWAS

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)
library(tidyr)
library(gpmapr)
library(ieugwasr)
library(TwoSampleMR)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")

indications_file <- file.path(interim_data, "reported_outcomes/Drug_Bank_outcomes/Drug_Bank_Indications.csv")
variants_file <- file.path(interim_data, "predicted_outcomes/G_P_Map_Variants/Drug_Target_Variant.csv")

#######################################################
# 1. Read in primary drug indications sourced from DrugBank
#######################################################

# Prioritise prescription use - looking at primary indication only

indications <- fread(indications_file) %>% 
  filter(`Approval Level` == "Prescription")

#######################################################
# 2. Identify those that can be genetically proxied
#######################################################

indications_list <- indications %>% 
  select(Indication) %>% 
  unique() 

colnames(indications_list) <- "Outcome"

# Looks like they all have the potential to be genetically proxied

#######################################################
# 3. Extract all available outcomes from openGWAS
#######################################################

opengwas_outcomes <-TwoSampleMR::available_outcomes() %>% 
  filter(population == "European")

opengwas_outcomes_list <- opengwas_outcomes %>% 
  select(trait) %>% 
  unique()

colnames(opengwas_outcomes_list) <- "Outcome"

#######################################################
# 4. Map openGWAS outcomes to drugbank indications
#######################################################

# Saving traits to read in and map to next script - 062_Primary_Indications_Mapping.py

fwrite(opengwas_outcomes_list, file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/Biobert_input/openGWAS_outcomes.csv"))
fwrite(indications_list, file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/Biobert_input/primary_indications.csv"))
