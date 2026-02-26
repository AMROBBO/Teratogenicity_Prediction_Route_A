##
# Formatting outcome datasets for ontology mapping - FAERS
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

faers_input_dir <- file.path(interim_data, "reported_outcomes/FAERS_outcomes")
faers_output_dir <- file.path(interim_data, "ontology_mapping/input_data")

#######################################################
# Reading in FAERS outcomes
#######################################################

faers_outcomes <- list.files(faers_input_dir, full.names = T)

#######################################################
# Save outcomes to separate drug files
#######################################################

for (f in faers_outcomes){
  
  drug <- unlist(strsplit(f, split = "/"))[length(unlist(strsplit(f, split = "/")))]
  
  if (drug == "Folate"){
    drug <- "folic acid"
  }
  
  output_dir <- file.path(faers_output_dir, str_replace(tolower(drug), " ", "_"))
  
  # All FAERS outcomes
  faers_outcomes_all_file <- list.files(f, full.names = T, pattern = c("all_Reaction.+csv"))
  
  if (length(faers_outcomes_all_file) != 0){
    # Read file
    faers_outcomes_all <- fread(faers_outcomes_all_file)
    
    # Reformat
    colnames(faers_outcomes_all)[1] <- "Outcome"
    faers_outcomes_all$`Number of Cases` <- as.numeric(gsub(",", "", faers_outcomes_all$`Number of Cases`))
    
    # Filter for more common outcomes
    faers_outcomes_all <- faers_outcomes_all %>% 
      filter(`Number of Cases` > 5)
    
    # Save
    output_all <- file.path(output_dir, paste0(str_replace(tolower(drug), " ", "_"), "_faers_all.csv"))
    
    fwrite(faers_outcomes_all, output_all)
  }
  
  # Congenital Outcomes
  faers_outcomes_cong_file <- list.files(f, full.names = T, pattern = c("Preg_Cong.+csv"))
  
  if (length(faers_outcomes_cong_file) != 0){
    # Read file
    faers_outcomes_cong <- fread(faers_outcomes_cong_file) %>% 
      filter(Reaction != "Number of Cases") %>% 
      filter(Reaction != "")
    
    colnames(faers_outcomes_cong)[2] <- "Outcome"
    
    # Save
    output_cong <- file.path(output_dir, paste0(str_replace(tolower(drug), " ", "_"), "_faers_cong.csv"))
    fwrite(faers_outcomes_cong, output_cong)
  }
}

# Azilsartan medoxomil did not have any pregnancy or congenital adverse outcomes on FAERS