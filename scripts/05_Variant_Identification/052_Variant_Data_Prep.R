## 
# Creating a list of variant rsIDs for MR PREG and 100,000 Genomes data extraction
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(data.table)
library(dplyr)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")

targets_file <- file.path(interim_data, "predicted_outcomes/Drug_Bank_targets/Drug_Bank_targets.csv")
variants_file <- file.path(interim_data, "predicted_outcomes/G_P_Map_Variants/unique_variants.csv")
output_dir <- file.path(interim_data, "predicted_outcomes/G_P_Map_Variants")

#######################################################
# Creating a list of all the unique variants associated 
# with all the target protein genes
#######################################################

variants <- fread(variants_file) 

variants_list <- variants %>% 
  select(rsid.x) %>% 
  unique() %>% 
  unlist()

writeLines(variants_list, file.path(output_dir, "variants_list.txt"))

#######################################################
# Summary of Drug -> Target -> Gene -> Variant
#######################################################

targets <- fread(targets_file) %>% 
  select("Drug", "Target", "Gene_Name", "Primary")

colnames(variants)[colnames(variants) == "rsid.x"] <- "rsid"

summary <- targets %>% 
  full_join(variants, by = c("Gene_Name" = "drug_target"), relationship = "many-to-many") %>% 
  mutate(Primary = factor(Primary,
                          levels = c("Primary", "Alternative"))) %>% 
  mutate(variant_type = factor(variant_type,
                               levels = c("common", "rare", "other"))) %>% 
  arrange(Drug, Primary, Target, variant_type)

fwrite(summary, file.path(output_dir, "Drug_Target_Variant.csv"))
