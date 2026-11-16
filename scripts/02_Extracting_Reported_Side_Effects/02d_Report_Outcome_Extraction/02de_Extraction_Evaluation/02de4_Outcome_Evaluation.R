## 
# Assessing how well the LLM method has extracted the outcomes from the text
# compared to those manually extracted.
#   For each matched row on Evidence_Text:
#     -> Order the extracted outcomes from most similar to least similar using the 
#     cosine similarities
#     -> Manually assess similarities
##
#######################################################
# Load libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)
library(tidyr)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")

evaluation_dir <- file.path(interim_data, "reported_outcomes/Extraction_Evaluation")

#######################################################
# For each data source
#######################################################

sources <- c("Reprotox", "TERIS", "UKTIS")

for(source in sources){

  message("Processing: ", source)
  
  mapped_outcomes_dir <- file.path(interim_data, "reported_outcomes/Extraction_Evaluation", source, "2_mapped_extractions") 
  outcome_similarites_dir <- file.path(interim_data, "reported_outcomes/Extraction_Evaluation", source, "3_similarity_results")
  
  output_dir <- file.path(interim_data, "reported_outcomes/Extraction_Evaluation", source, "4_mapped_ordered")

  #######################################################
  # Read in data
  #######################################################

  mapped_outcomes <- mapped_outcomes_dir %>%
    list.files(., pattern = "mapped_extractions.csv", full.names = T)
    
  outcome_similarites <- outcome_similarites_dir %>%
    list.files(., full.names = T)
  
  for(f in mapped_outcomes){
    
    drug <- basename(f) %>%
      gsub("_mapped_extractions.csv", "", .)
    
    mapped <- f %>%
      fread() %>%
      select(Outcome_LLM, Outcome_manual, Evidence_Text_LLM, Evidence_Text_manual,
            Risk_Window_LLM, Risk_Window_manual, Confidence_LLM, Confidence_manual,
            Source_LLM, Source_manual, Exposure_LLM, Exposure_manual, Rate_LLM, Rate_manual,
            Study_Details_LLM, Study_Details_manual, Limitations_LLM, Limitations_manual,
            Human_or_Animal_LLM, Human_or_Animal_manual, Drug_LLM, Drug_manual, match_status)
    
    similarities <- outcome_similarites %>%
      grep(paste0("/", drug, "_"), ., value = T) %>%
      fread()
    
    #######################################################
    # Match mapped outcomes to similarity scores
    #######################################################

    similarities_long <- similarities %>%
      pivot_longer(cols = 2:ncol(similarities), names_to = "Manual", values_to = "Similarity")
    
    mapped <- mapped %>%
      left_join(similarities_long, by = c("Outcome_LLM" = "V1", "Outcome_manual" = "Manual"))

    #######################################################
    # Order similarities scores from most similar to least
    #######################################################
    
    mapped <- mapped %>%
      arrange(Evidence_Text_manual, desc(Similarity))

    #######################################################
    # Save
    #######################################################

    output_file <- file.path(output_dir, paste0(drug, "_mapped_ordered.csv"))
    fwrite(mapped, output_file)
    
  }
}
