##
# Using openGWAS data to test association between drug target gene
# and primary indications of drugs
# 4. Map openGWAS outcomes to drugbank indications
##

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
library(jsonlite)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")

mapped_file <- file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/Qwen_output/Qwen_output_combined.json")
opengwas_correction_file <- file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/OpenGWAS_mispellings.csv")

#######################################################
# Converting mapped json file -> csv
#######################################################

# DrugBank Indications mapped to openGWAS outcomes
mapped_outcomes <- fromJSON(mapped_file, simplifyVector = F)

# Converting mapped outcomes to csv format
rows <- list()

# For each indication
for(outcome_name in names(mapped_outcomes)){
  
  # Extract matches
  matches <- mapped_outcomes[[outcome_name]]$matches
  
  # Skip if there are no matches
  if (length(matches$openGWAS_outcome) == 0) {
    next
  }
  
  # Create data frame for this indication
  df <- data.frame(
    indication = outcome_name,
    openGWAS_outcome = unlist(matches$openGWAS_outcome),
    cosine_similarity = sapply(
      matches$cosine_similarity,
      function(x) if (is.null(x)) NA_real_ else as.numeric(x)
    ),
    similarity_score = as.numeric(unlist(matches$similarity_score)),
    match_decision = as.logical(unlist(matches$match_decision)),
    brief_rationale = unlist(matches$brief_rationale),
    stringsAsFactors = FALSE
  )
  
  # Store it in a list
  rows[[outcome_name]] <- df
}
outcome_table <- bind_rows(rows)

#######################################################
# Correcting some errors introduced by LLM - This step is very manual and will have to be curated for each rerun of LLM step
#######################################################

# This file was manually corrected by finding the original openGWAS terms in available_outcomes()
opengwas_corrections <- fread(opengwas_correction_file) 

# Correcting some terms
opengwas_corrections$mapped_openGWAS <- gsub('""', '"', opengwas_corrections$mapped_openGWAS)
opengwas_corrections$original_openGWAS <- gsub('""', '"', opengwas_corrections$original_openGWAS)

outcome_table$openGWAS_outcome <- ifelse(
  grepl("^c\\(", outcome_table$openGWAS_outcome),
  sub("^c\\((.*)\\)$", "\\1", outcome_table$openGWAS_outcome),
  outcome_table$openGWAS_outcome
)

outcome_table$openGWAS_outcome <- gsub("^c\\(", "", outcome_table$openGWAS_outcome)
outcome_table$openGWAS_outcome <- gsub('""', '"', outcome_table$openGWAS_outcome)

# Changing incorrect openGWAS terms in mapped data
outcome_table <- left_join(outcome_table, 
                           opengwas_corrections, 
                           by = join_by("openGWAS_outcome" == "mapped_openGWAS"), 
                           relationship = "many-to-many")

outcome_table$openGWAS_outcome <- ifelse(!is.na(outcome_table$original_openGWAS),
                                         outcome_table$original_openGWAS,
                                         outcome_table$openGWAS_outcome)

#######################################################
# Checking the terms do match
#######################################################

all_outcomes <- available_outcomes() %>% 
  filter(population == "European")

outcome_table[(!outcome_table$openGWAS_outcome %in% all_outcomes$trait),]

#######################################################
# Match to IDs
#######################################################

openGWAS_outcomes <- left_join(outcome_table,
                               all_outcomes,
                               by = join_by("openGWAS_outcome" == "trait"),
                               relationship = "many-to-many") 

#######################################################
# Save
#######################################################

fwrite(openGWAS_outcomes, file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/Mapped_outcomes.csv"))
