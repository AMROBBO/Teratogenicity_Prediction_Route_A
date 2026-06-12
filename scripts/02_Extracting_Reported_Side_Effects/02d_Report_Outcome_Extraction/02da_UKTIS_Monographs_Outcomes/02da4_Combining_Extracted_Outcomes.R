## 
# Reading in adjudicated outcome chunks from UKTIS Monographs and combining
# into one dataset per drug
##
# As the LLMs do not have a 100% regular output, some chunks had to be rerun through
# 02d2_Outcome_Extraction to produce an output that included a valid json format
##

#######################################################
# Load libraries
#######################################################

library(dotenv)
library(jsonlite)
library(dplyr)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")

input_dir <- file.path(interim_data, "reported_outcomes/UKTIS_outcomes/3_Adjudicated_Outcomes")
output_dir <- file.path(interim_data, "reported_outcomes/UKTIS_outcomes/4_Combined_Outcomes")

#######################################################
# Reading in outcome chunks
#######################################################

adjudicating_outcomes <- list.files(input_dir, full.names = T)

for (f in adjudicating_outcomes){
  
  # Extract drug chunk
  drug_name <- unlist(strsplit(f, split = "/"))[10]
  
  # Each outcome chunk
  chunks <- list.files(f, full.names = T)
  
  # Remove references
  chunks <- chunks[-(grep("References", chunks))]
  
  all_chunks <- lapply(chunks, readLines)
  
#######################################################
# Cleaning each chunk to include json format only: marked by ```
#######################################################
  
  for (i in 1:length(all_chunks)){
    
    chunk <- all_chunks[[i]]
    
    json_header <- grep("```", chunk)

    if(length(json_header) > 0){
      
      # Extracting section marked by ``` only
      # For those with multiple markers -> usually final section will be the final output
      start <- json_header[length(json_header)-1]
      end <- json_header[length(json_header)]
      
      # Flag those with multiple markers
      if(length(json_header) > 2){
        print("###################################")
        print(paste0("Multiplie sections in ", drug_name," chunk ", i, ": Check"))
        print("###################################")
        print(all_chunks[[i]])
        }
      
      chunk_json <- chunk[(start+1):(end-1)]
      
      
      all_chunks[[i]] <- chunk_json
    }
  }
  
#######################################################
# Combining all chunks into one json format
#######################################################

    combined <- list()
  
  for (chunk in all_chunks){
    
    # Make sure the formatting is correct
    chunk_formatted <- gsub('\": NR,', '": "NR",', chunk)
    
    # Convert to json
    json_text <- paste(chunk_formatted, collapse = "\n")
    
    obj <- fromJSON(json_text, simplifyVector = FALSE)
    
    # Extract drug name
    drug <- toupper(obj$Drug)
    drug <- gsub(" ", "_", drug)
    
    if(length(drug) == 0){
      drug <- drug_name
    }
    
    # Combine all outcomes for given drug
    if (drug %in% names(combined)) {
      
      combined[[drug]]$Outcomes <- c(
        combined[[drug]]$Outcomes,
        obj$Outcomes
      )
      
    } else {
      
      # First occurrence of drug
      combined[[drug]] <- obj
    }
  }
  
  final_output <- unname(combined)
  
#######################################################
# Save
#######################################################
  
  output_file <- file.path(output_dir, paste0(drug, ".json"))
  
  write_json(final_output, output_file, pretty = TRUE, auto_unbox = TRUE)
  
}

