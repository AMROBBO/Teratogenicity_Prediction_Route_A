##
# Reading in suggested drug side effects from Bumps 
# and assigning to teratogenic drugs of interest
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

raw_data <- Sys.getenv("rawdatadir")
interim_data <- Sys.getenv("interimdatadir")

drug_list_file <- file.path(interim_data, "teratogenic_drugs.txt")

bumps_raw <- file.path(raw_data, "drug_extracted_data")
bumps_output <- file.path(interim_data, "reported_outcomes/Bumps_outcomes")

#######################################################
# Reading in teratogenic drug list of interest
#######################################################

drug_list <- readLines(drug_list_file)

#######################################################
# Reading in files containg Bumps side effects
#######################################################

drug_files <- list.files(bumps_raw, pattern = ".csv", full.names = T)

#######################################################
# Assigning side effects to drugs and combining
#######################################################

bumps_outcome = data.frame(matrix(nrow = 0, ncol = 2)) 
bumps_colnames = c("Outcome", "Drug") 
colnames(bumps_outcome) = bumps_colnames

for (f in drug_files){
  drug_out <- fread(f, header = T) %>% 
    select(c("DrugBank Name", "Bumps Predicted Side Effects"))
  drug_out <- drug_out[!apply(drug_out == "", 1, all), ]
  
  drug_out$Drug <- drug_out[1,1]
  drug_out <- drug_out %>% 
    select(!"DrugBank Name")
  colnames(drug_out)[1] <- "Outcome"
  
  bumps_outcome <- rbind(bumps_outcome, drug_out)
}

# Renaming for consistency
bumps_outcome[bumps_outcome$Drug == "Valproic acid"]$Drug <- "Valproate"

# Removing punctuation
bumps_outcome$Outcome <- bumps_outcome$Outcome %>%  
  gsub(pattern = "[[:punct:]]", replacement = "")

#######################################################
# Save
#######################################################

fwrite(bumps_outcome, file.path(bumps_output, "Bumps_reported_outcomes.csv"))
