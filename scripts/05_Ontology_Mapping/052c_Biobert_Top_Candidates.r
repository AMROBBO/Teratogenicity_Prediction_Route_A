## 
# Extracting the top matches identified by UMCU/SapBERT-from-PubMedBERT-fulltext_bf16
# biobert model
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

input_dir <- file.path(interim_data, "ontology_mapping/output_data")

#######################################################
# Function to extract the top Matches and Cosine values
#######################################################

get_top <- function(sim_matrix, n = 10) {
  results <- do.call(rbind, lapply(1:nrow(sim_matrix), function(i) {
    omim_term <- rownames(sim_matrix)[i]
    sim_values <- sim_matrix[i, ]
    
    # Get indices of top N similarities
    top_matches <- order(sim_values, decreasing = TRUE)[1:n]
    
    valid_idx <- which(!is.na(top_matches))
    # Create a data.frame with TermA, TermB, and cosine similarity
    data.frame(
      Predicted_term = omim_term,
      Observed_term = colnames(sim_matrix)[top_matches[valid_idx]],
      CosineSimilarity = sim_values[top_matches[valid_idx]],
      stringsAsFactors = FALSE
    )
  }))
  
  return(results)
}

#######################################################
# Choose model of interest - Based of output from script 052b
#######################################################

model <- "UMCU/SapBERT-from-PubMedBERT-fulltext_bf16"

results_dir <- file.path(input_dir, model)

#######################################################
# Choose datasets of interest - Ensure only one of each is uncommented
#######################################################

predicted <- "omim"
#observed <- "onsides"
#observed <- "bumps"
#observed <- "faers_all"
observed <- "faers_cong"

top_candidates <- 30

#######################################################
# Selecting top matches - for all outcomes, but FAERS
#######################################################

for (i in 1:length(list.files(results_dir, full.names = T))){
  
  drug_files <- list.files(list.files(results_dir, full.names = T)[i], full.names = T)
  
  similarity_file <- grep(paste(predicted, observed, "similarity_matrix.csv", sep = "_"), drug_files, value = T)
  
  if (length(similarity_file) > 0){
    
    sim_matrix <- as.matrix(fread(similarity_file), rownames = 1)
    
    top_match_df <- get_top(sim_matrix, top_candidates)
    
    drug_name <- list.files(results_dir)[i]
    
    output <- paste(list.files(results_dir, full.names = T)[i], paste(drug_name, predicted, observed, "top", paste0(top_candidates, ".csv"), sep = "_"), sep = "/")
    
    fwrite(top_match_df, output)
  }
}

#######################################################
# Selecting top matches - For FAERS outcomes
#######################################################

raw_dir <- file.path(interim_data, "ontology_mapping/input_data")

for (i in 1:length(list.files(results_dir, full.names = T))){
  
  drug_name <- list.files(results_dir)[i]
  
  drug_files <- list.files(list.files(results_dir, full.names = T)[i], full.names = T)
  
  case_file <- grep(paste0("/", drug_name), list.files(raw_dir, full.names = T), value = T)
  case_file <- list.files(case_file,full.names = T, pattern = observed)
  
  sim_file <- grep(paste(predicted, observed, "similarity_matrix.csv", sep = "_"), drug_files, value = T)
  
  if (length(sim_file) > 0){
    
    case_no <- fread(case_file)[,-"Reaction Group"] # Only applies to Cong outcomes, ignore warning for all outcomes
    colnames(case_no)[1] <- "Observed_term"
    
    sim_matrix <- as.matrix(fread(sim_file), rownames = 1)
    
    top_match_df <- get_top(sim_matrix, n = top_candidates)
    
    top_match_df <- top_match_df %>% 
      left_join(case_no, by = "Observed_term")
    
    output <- paste(list.files(results_dir, full.names = T)[i], paste(drug_name, predicted, observed, "top", paste0(top_candidates, ".csv"), sep = "_"), sep = "/")
    
    fwrite(top_match_df, output)
  }
}
