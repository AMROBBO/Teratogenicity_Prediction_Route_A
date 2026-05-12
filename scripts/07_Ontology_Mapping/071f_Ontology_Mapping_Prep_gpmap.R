##
# Formatting outcome datasets for ontology mapping - GP Map Colocalised Traits
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

gpmap_input_dir <- file.path(interim_data, "predicted_outcomes/G_P_Map_Outcomes")
gpmap_output_dir <- file.path(interim_data, "ontology_mapping/input_data")

#######################################################
# Drug Target Genes
#######################################################

targets_file <- file.path(interim_data, "predicted_outcomes/Drug_Bank_targets/Drug_Bank_targets.csv")

target_genes <- fread(targets_file) %>% 
  select(Gene_Name, Drug, Primary)

#######################################################
# Read in GP Map Colocalised Traits
#######################################################

gpmap_traits <- fread(file.path(gpmap_input_dir, "unique_coloc_traits.csv")) 

#######################################################
# Save outcomes to separate drug files
#######################################################

gpmap_traits <- gpmap_traits %>% 
  full_join(target_genes, by = c("drug_target" = "Gene_Name"), relationship = "many-to-many")

# Changing Valproic acid to valproate for consistent naming

gpmap_traits$Drug <- recode(gpmap_traits$Drug,
                            "Valproic acid" = "Valproate")

gpmap_traits <- gpmap_traits[!is.na(gpmap_traits$trait_name)]

for (i in unique(gpmap_traits$Drug)){
  traits <- gpmap_traits[gpmap_traits$Drug == i, ]
  traits <- unique(traits)
  colnames(traits)[2] <- "Outcome"

  output_dir <- file.path(gpmap_output_dir, str_replace(tolower(i), " ", "_"))
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  fwrite(traits, file.path(output_dir, paste0(str_replace(tolower(i), " ", "_"), "_gpmap.csv")))
}

# Seems to sometime leave behind the tmp file, which prevents the next step working. Use:
# find . -type f -name ".*_gpmap.csv" -print -delete