## 
# Assigning outcomes to drugs
##

#######################################################
# Load libraries
#######################################################

library(dotenv)
library(jsonlite)
library(dplyr)
library(data.table)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")

input_dir <- file.path(interim_data, "reported_outcomes/Reprotox_outcomes/4_Combined_Outcomes")
output_dir <- file.path(interim_data, "reported_outcomes/Reprotox_outcomes/5_Final_Outcomes")

outcome_files <- list.files(input_dir, full.names = T)

#######################################################
# Converting JSON -> CSV
#######################################################

for (file in outcome_files){
  
  # Extract Drug Name
  drug <- unlist(strsplit(file, split = "[/\\.]"))
  drug <- drug[10:(length(drug)-1)]
  drug <- paste(drug, collapse = "_")
  
  # LLM output
  outcome_json <- fromJSON(file)
  
  # Turning outcomes into csv format
  outcomes <- outcome_json$Outcomes %>% 
    as.data.frame()
  
  # Label with drug
  outcomes$Drug <- drug
  
  # Convert to characters for saving
  outcomes <- outcomes %>% 
    mutate(across(where(is.list), as.character))

  # Save

  output_file <- file.path(output_dir, paste0(drug, "_combined_outcomes.csv"))
  
  fwrite(outcomes, output_file)
  
}
