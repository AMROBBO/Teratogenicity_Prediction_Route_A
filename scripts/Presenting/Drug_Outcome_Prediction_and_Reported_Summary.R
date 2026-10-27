##
# Summarising the number of predicted and reported outcomes from all sources
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)
library(stringr)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

raw_data <- Sys.getenv("rawdatadir")
interim_data <- Sys.getenv("interimdatadir")
processed_data <- Sys.getenv("processeddatadir")

outcome_files <- file.path(interim_data, "ontology_mapping/input_data") %>%
  list.files(., full.names = T)

outcome_files <- outcome_files[-grep("test", outcome_files)]

indications_file <- file.path(interim_data, "reported_outcomes/Drug_Bank_outcomes/Drug_Bank_Indications.csv")

UKTIS_files <- file.path(interim_data, "reported_outcomes/UKTIS_outcomes/5_Final_Outcomes") %>%
  list.files(., full.names = T)

reprotox_files <- file.path(interim_data, "reported_outcomes/Reprotox_outcomes/5_Final_Outcomes") %>%
  list.files(., full.names = T)

TERIS_files <- file.path(interim_data, "reported_outcomes/TERIS_outcomes/5_Final_Outcomes") %>%
  list.files(., full.names = T)

output_dir <- file.path(processed_data, "Presenting")

#######################################################
# Drug Names and Categories
#######################################################

drug_details <- fread(file.path(raw_data, "Drug_details.csv")) %>%
  select(Drug, Category)

drug_details[which(drug_details$Drug == "Folate"), 1] <- "Folic acid"

#######################################################
# Read in and Filter Drug Indications
#######################################################

indications <- fread(indications_file) %>%
  filter(`Approval Level` == "Prescription")

#######################################################
# Extracting Number of Each Outcome
#######################################################

drug_details$FDA_AEMS_all <- 0
drug_details$FDA_AEMS_cong <- 0
drug_details$DrugBank <- 0
drug_details$SmPCs <- 0
drug_details$UKTIS <- 0
drug_details$Reprotox <- 0
drug_details$TERIS <- 0
drug_details$OMIM <- 0
drug_details$GPMap <- 0
drug_details$OpenGWAS <- 0
drug_details$MR_PREG <- 0
drug_details$`100,000 Genomes Project` <- 0

for(f in outcome_files){
  
  # Extract drug name
  
  drug <- unlist(strsplit(f, split = "/"))
  drug <- drug[length(drug)]
  drug <- gsub("_", " ", drug)
  drug <- str_to_sentence(drug)
  
  row <- which(drug_details$Drug == drug)
  
#######################################################
# Number of FDA AEMS Reported Outcomes - All
#######################################################
  
  fda_all <- list.files(f, full.names = T) %>%
    grep("faers_all", ., value = T)
  
  if (length(fda_all) != 0){
    
    fda_all_outcomes <- fread(fda_all)
    
    fda_all_outcomes <- fda_all_outcomes[!which(fda_all_outcomes$Outcome == "Totals"),] %>%
      select(Outcome) %>%
      unique()
    
    drug_details[row, "FDA_AEMS_all"] <- as.numeric(nrow(fda_all_outcomes))
    
  }

#######################################################
# Number of FDA AEMS Reported Outcomes - Congenital
#######################################################
  
  fda_cong <- list.files(f, full.names = T) %>%
    grep("faers_cong", ., value = T) 
  
  if (length(fda_cong) != 0){
    
    fda_cong_outcomes <- fread(fda_cong) %>%
      select(Outcome) %>%
      unique()
    
    drug_details[row, "FDA_AEMS_cong"] <- as.numeric(nrow(fda_cong_outcomes))

  } 

#######################################################
# Number of SmPCs Reported Outcomes
#######################################################

  drugbank <- indications %>%
    filter(Drug == gsub(" ", "_", drug)) %>%
    select(Indication) %>%
    unique() %>%
    nrow()
    
  drug_details[row, "DrugBank"] <- as.numeric(drugbank)
  
#######################################################
# Number of SmPCs Reported Outcomes
#######################################################
  
  smpc <- list.files(f, full.names = T) %>%
    grep("smpc", ., value = T) 
  
  if (length(smpc) != 0){
    
    smpc_outcomes <- fread(smpc) %>%
      select(Outcome) %>%
      unique()
    
    drug_details[row, "SmPCs"] <- as.numeric(nrow(smpc_outcomes))
    
    }
  
#######################################################
# Number of OMIM Predicted Outcomes
#######################################################
  
  omim <- list.files(f, full.names = T) %>%
    grep("omim", ., value = T) 
  
  if (length(omim) != 0){
    
    omim_outcomes <- read.csv(omim) %>%
      select(Outcome) %>%
      unique()
    
    drug_details[row, "OMIM"] <- as.numeric(nrow(omim_outcomes))
    
  }

#######################################################
# Number of GPMap Predicted Outcomes
#######################################################
  
  gpmap <- list.files(f, full.names = T) %>%
    grep("gpmap", ., value = T) 
  
  if (length(gpmap) != 0){
    
    gpmap_outcomes <- fread(gpmap) %>%
      select(Outcome) %>%
      unique()
    
    drug_details[row, "GPMap"] <- as.numeric(nrow(gpmap_outcomes))
    
  }
  
#######################################################
# Number of UKTIS Monographs Reported Outcomes
#######################################################
  
  uktis_search <- str_to_upper(drug)
  
  drug_category <- drug_details[which(drug_details$Drug == drug), "Category"]
  
  if(uktis_search == "LITHIUM CARBONATE"){
    uktis_search = "LITHIUM"
  } else if (uktis_search == "VALPROATE"){
    uktis_search = "SODIUM VALPROATE"
  }
  
  if(drug_category == "ACE Inhibitor"){
    uktis_search = "ANGIOTENSIN_CONVERTING_ENZYME_INHIBITORS"
  } else if(drug_category == "Angiotensin II receptor blocker (ARB)"){
    uktis_search = "ANGIOTENSIN_II_RECEPTOR_BLOCKERS"
  }
  
  uktis_search <- gsub(" ", "_", uktis_search)
  uktis_search <- paste0("[_/]", uktis_search)
  
  uktis <- grep(uktis_search, UKTIS_files, value = T)
  
  if(length(uktis) != 0){
    
    uktis <- fread(uktis) %>%
      select(Outcome) %>%
      unique()
  
    drug_details[row, "UKTIS"] <- as.numeric(nrow(uktis))
  }
    
#######################################################
# Number of Reprotox Reported Outcomes
#######################################################

  reprotox_search <- str_to_upper(drug)
  
  if(reprotox_search == "LITHIUM CARBONATE"){
    reprotox_search = "LITHIUM"
  } else if (reprotox_search == "AZILSARTAN MEDOXOMIL"){
    reprotox_search = "AZILSARTAN"
  } else if (reprotox_search == "CANDESARTAN CILEXETIL"){
    reprotox_search = "CANDESARTAN"
  }
  
  reprotox_search <- gsub(" ", "_", reprotox_search)
  reprotox_search <- paste0("[_/]", reprotox_search)
  
  reprotox <- grep(reprotox_search, reprotox_files, value = T)
  
  if(length(reprotox) != 0){
    
    reprotox <- fread(reprotox) %>%
      select(Outcome) %>%
      unique()
  
    drug_details[row, "Reprotox"] <- as.numeric(nrow(reprotox))
  }
  
#######################################################
# Number of TERIS Reported Outcomes
#######################################################

  teris_search <- str_to_upper(drug)
  
  if(teris_search == "LITHIUM CARBONATE"){
    teris_search = "LITHIUM"
  } else if (teris_search == "AZILSARTAN MEDOXOMIL"){
    teris_search = "AZILSARTAN"
  } else if (teris_search == "CANDESARTAN CILEXETIL"){
    teris_search = "CANDESARTAN"
  } else if (teris_search == "VALPROATE"){
    teris_search = "VALPROIC ACID"
  }
  
  teris_search <- gsub(" ", "_", teris_search)
  teris_search <- paste0("[_/]", teris_search)
  
  teris <- grep(teris_search, TERIS_files, value = T) 
  
  if (length(teris) != 0){
    
    teris <- fread(teris) %>%
      select(Outcome) %>%
      unique()
  
  drug_details[row, "TERIS"] <- as.numeric(nrow(teris))
  
  }
  
#######################################################
# Number of OpenGWAS Predicted Outcomes
#######################################################


#######################################################
# Number of MR PREG Predicted Outcomes
#######################################################


#######################################################
# Number of 100,000 Genomes Project Predicted Outcomes
#######################################################

}

#######################################################
# Save
#######################################################

drug_details <- drug_details %>%
  arrange(Category, Drug)

fwrite(drug_details, file.path(output_dir, "All_Outcomes.csv"))

fwrite(drug_details %>%
         select("Drug", "Category", "FDA_AEMS_all", "FDA_AEMS_cong", "DrugBank", "SmPCs", "UKTIS", "Reprotox", "TERIS"),
       file.path(output_dir, "Reported_Outcomes.csv"))

fwrite(drug_details %>%
         select("Drug", "Category", "OMIM", "GPMap", "OpenGWAS", "MR_PREG", "100,000 Genomes Project"),
       file.path(output_dir, "Predicted_Outcomes.csv"))
