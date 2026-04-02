##
# Simplifying the OMIM predicted outcomes to enhance the performance of 
# ontology mapping
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

drug_targets_file <- file.path(interim_data, "predicted_outcomes/Drug_Bank_targets/Drug_Bank_targets.csv")

omim_dir <- file.path(interim_data, "predicted_outcomes/OMIM_outcomes")

#######################################################
# Reading in primary teratogenic drug targets of interest
#######################################################

primary_targets <- fread(drug_targets_file) %>% 
  filter(Primary == "Primary")

#######################################################
# Reading in OMIM outcome data and processing instructions
#######################################################

omim_outcomes <- fread(file.path(omim_dir, "OMIM_outcomes_pre_processed.csv"))
process_instructions <- fread(file.path(omim_dir, "OMIM_outcomes_process_instructions.csv"))[,2:3]

#######################################################
# Process outcome data
#######################################################

omim_outcomes <- omim_outcomes %>% 
  left_join(process_instructions, by = "Outcome", relationship = "many-to-many")

omim_outcomes$Replacement <- ifelse(is.na(omim_outcomes$Replacement),
                                    omim_outcomes$Outcome,
                                    omim_outcomes$Replacement)

colnames(omim_outcomes) <- c("Gene_Name", "Outcome_Detail", "Outcome")

omim_outcomes <- omim_outcomes %>% 
  filter(omim_outcomes$Outcome != "Remove")

#######################################################
# Assign to drug names
#######################################################

predicted_outcomes <- merge(primary_targets, 
                            omim_outcomes, 
                            by = "Gene_Name", 
                            allow.cartesian = TRUE)

predicted_outcomes[predicted_outcomes$Drug == "Valproic acid",]$Drug <- "Valproate"

#######################################################
# Save
#######################################################

fwrite(predicted_outcomes, file.path(omim_dir, "OMIM_outcomes_processed.csv"))
