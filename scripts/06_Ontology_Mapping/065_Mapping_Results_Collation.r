##
# Reading in and combining Qwen-DeepSeek output with biobert output
# Qwen and DeepSeek have assigned confidence scores to each pair according to the closeness of the traits:
# 5: Exact or near-perfect conceptual match
# 4: Strong pathophysiological relationship (same disease category, direct complications)
# 3: Moderate relationship (shared mechanisms, risk factors, or related conditions)
# 2: Weak relationship (indirect connections, shared symptoms)
# 1: Very loose association
# 0: No association
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(data.table)
library(tidyverse)
library(jsonlite)
library(dplyr)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")
processed_data <- Sys.getenv("processeddatadir")

input_dir <- file.path(interim_data, "ontology_mapping/output_data")
output_base_dir <- file.path(processed_data, "OMIM_FAERS_outcomes")

#######################################################
# Set outcome of interest
#######################################################

#outcome_cat <- "all"
outcome_cat <- "cong"

#######################################################
# Read in data, drug by drug
# Combine datasets based on OMIM:FAERS pair, to give:
# 1. Full dataset with Biobert, Qwen and Deepseek confidence columns
# 2. Refined dataset containing only the pairs accepted by DeepSeek
#######################################################

biobert_results <- file.path(input_dir, "UMCU/SapBERT-from-PubMedBERT-fulltext_bf16")
qwen_results <- file.path(input_dir, "Qwen")
deepseek_results <- file.path(input_dir, "DeepSeek")

for (f in list.files(deepseek_results, full.names = T)){
  
  drug <- unlist(strsplit(f, split = "/"))[length(unlist(strsplit(f, split = "/")))]
  
  ## Biobert output
  
  biobert_file <- list.files(biobert_results, pattern = drug, full.names = T) %>% 
    list.files(., full.names = T) %>% 
    grep(paste("omim_faers", outcome_cat, "top_30.csv", sep = "_"), ., value = T)
  
  if(length(biobert_file) > 0){
    
    biobert <- fread(biobert_file)
    
    ## Qwen output
    
    qwen_all <- data.frame(matrix(ncol = 4, nrow = 0))
    qwen_colnames <- c("Predicted_term", "Observed_term", "Confidence_Qwen", "Rationale_Qwen")
    colnames(qwen_all) <- qwen_colnames
    
    qwen_files <- list.files(qwen_results, pattern = drug, full.names = T) %>% 
      list.files(., pattern = outcome_cat, full.names = T) %>% 
      list.files(., pattern = ".json", full.names = T)
    
    for (i in qwen_files){
      qwen <- jsonlite::fromJSON(i)
      
      if(length(qwen$matches) > 0){
        qwen <- as.data.frame(qwen)
        colnames(qwen) <- qwen_colnames
      
        qwen_all <- rbind(qwen_all, qwen)
      }
    }
    ## Deepseek output
    
    deepseek_all <- data.frame(matrix(ncol = 5, nrow = 0))
    deepseek_colnames <- c("Predicted_term", "Observed_term", "Decision_Deepseek", "Confidence_Deepseek", "Rationale_Deepseek")
    colnames(deepseek_all) <- deepseek_colnames
    
    deepseek_files <- list.files(f, pattern = outcome_cat, full.names = T) %>% 
      list.files(., pattern = ".json", full.names = T)
    
    for (j in deepseek_files){
      deepseek <- jsonlite::fromJSON(j) %>%  
        as.data.frame()
      colnames(deepseek) <- deepseek_colnames
      
      deepseek_all <- rbind(deepseek_all, deepseek)
    }
    
    ## Merge all results
    
    results_merge <- merge(biobert, qwen_all, by = c("Predicted_term", "Observed_term"), all = T) 
    results_merge <- merge(results_merge, deepseek_all, by = c("Predicted_term", "Observed_term"), all = T) %>% 
      group_by(Predicted_term) %>% 
      arrange(desc(Confidence_Deepseek), .by_group = T)
    
    results_merge_sig <- results_merge %>% filter(!is.na(Confidence_Qwen))
    
    results_merge_accept <- results_merge %>% filter(Decision_Deepseek == "ACCEPT")
    
    ## Save
    
    output_dir <- file.path(output_base_dir, drug)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
    
    output_dir <- file.path(output_dir, outcome_cat)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
    
    output_file_full <- file.path(output_dir, paste(drug, "omim_faers", outcome_cat, "full.csv", sep = "_"))
    output_file_sig <- file.path(output_dir, paste(drug, "omim_faers_LLM", outcome_cat, "sig.csv", sep = "_"))
    output_file_accept <- file.path(output_dir, paste(drug, "omim_faers_LLM", outcome_cat, "accepted.csv", sep = "_"))
    
    fwrite(results_merge, output_file_full)
    fwrite(results_merge_sig, output_file_sig)
    fwrite(results_merge_accept, output_file_accept)
  }
}
