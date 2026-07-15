##
# Collating Drug - Primary Target - Colocalised Traits from Genotype-Phenotype Map
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

interim_data <- Sys.getenv("interimdatadir")
processed_data <- Sys.getenv("processeddatadir")

targets_file <- file.path(interim_data, "predicted_outcomes/Drug_Bank_targets/Drug_Bank_targets.csv")
coloc_traits_file <- file.path(interim_data, "predicted_outcomes/G_P_Map_Outcomes/all_coloc_traits.csv")

output_dir <- file.path()

#######################################################
# Reading in Primary Drug Target Genes
#######################################################

target_genes <- fread(targets_file) %>%
  filter(Primary == "Primary") %>%
  select(Drug, Target, Gene_Name, Primary)

#######################################################
# Reading in Colocalised Traits
#######################################################

coloc_traits <- fread(coloc_traits_file) %>%
  select(drug_target, trait_name, trait_category, data_type, min_p, rsid, 
         beta, se, p, eaf, h4_connectedness, h3_connectedness)

#######################################################
# Drug - Primary Drug Target - Colocalised Trait
#######################################################

drug_coloc <- target_genes %>%
  left_join(coloc_traits, by = c("Gene_Name" = "drug_target"), relationship = "many-to-many")
