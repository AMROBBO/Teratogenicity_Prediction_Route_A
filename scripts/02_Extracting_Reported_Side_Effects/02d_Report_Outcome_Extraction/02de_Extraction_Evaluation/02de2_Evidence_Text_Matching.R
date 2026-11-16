## 
# String matching "Evidence Text" columns for corresponding LLM extracted and 
# manually extracted outcomes.
# Rows can be classified as:
#     - Matched pairs
#     - LLM texts matched
#     - Manual texts matched
#     - In LLM Extraction only
#     - In manual extraction only"
##
#######################################################
# Load libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)
library(stringdist)
library(fuzzyjoin)
library(tidytext)
library(gt)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")
raw_data <- Sys.getenv("rawdatadir")

extraction_base_files <- file.path(interim_data, "reported_outcomes") 

drug_classes <- file.path(raw_data, "Drug_details.csv") %>%
  fread() %>%
  select(Drug, Category) %>%
  filter(Category %in% c("Angiotensin II receptor blocker (ARB)", "ACE Inhibitor"))

#######################################################
# For each report source
#######################################################

sources <- c("Reprotox", "TERIS", "UKTIS")

for(source in sources){
  
  #######################################################
  # Initialise file paths
  #######################################################
  
  LLM_output_files <- file.path(extraction_base_files, paste0(source, "_outcomes"), "5_Final_Outcomes") %>%
    list.files(, full.names = T)
  
  manual_output_files <- file.path(extraction_base_files, "Extraction_Evaluation", source, "1_manual_extractions") %>%
    list.files(, full.names = T)
  
  output_dir <- file.path(extraction_base_files, "Extraction_Evaluation", source, "2_mapped_extractions")
  output_table <- file.path(extraction_base_files, "Extraction_Evaluation", source, "2_mapped_extractions/extraction_success.csv")
  
  summary_tbl <- data.table(
    match_status = c("Matched pairs", "LLM texts matched", "Manual texts matched", "In LLM Extraction only", "In manual extraction only")
    )
  
  message("--- Processing: ", source, " ---")

  #######################################################
  # For each manually extracted file
  #######################################################
  
  for(f in manual_output_files){
    
    drug <- basename(f) %>%
      gsub(".xlsx", "", .)
    
    message(drug)
    
    #######################################################
    # Read in data
    #######################################################
    
    if(source == "UKTIS" & drug %in% drug_classes$Drug){
      
      drug_search <- drug_classes[which(drug_classes$Drug == drug), Category]
      
      if(drug_search == "ACE Inhibitor"){
        drug_search <- "ANGIOTENSIN_CONVERTING_ENZYME_INHIBITORS"
        
      }else if(drug_search == "Angiotensin II receptor blocker (ARB)"){
        drug_search <- "ANGIOTENSIN_II_RECEPTOR_BLOCKERS"
        
      }
    }else{
      drug_search <- paste0("[/_]", gsub(" ", "_", drug), "_")
    }
    
    LLM_output <- grep(drug_search, LLM_output_files, ignore.case = T, value = T)
    if(length(LLM_output) == 0){
      cat("No LLM Output for", source, drug)
      next
    }else{
      LLM_output <- LLM_output %>%
        fread()
    }
    
    manual_output <- f %>%
      readxl::read_xlsx()
  
    #######################################################
    # Match Evidence text columns (with containment)
    #######################################################
  
    LLM_output <- LLM_output %>%
      mutate(id_llm = row_number())
    manual_output <- manual_output %>%
      mutate(id_manual = row_number())
  
    # Break each text into overlapping 3-word sequences ("shingles").
    # unnest_tokens lowercases and strips punctuation, so small formatting
    # differences between the two extractions don't matter.
  
    LLM_shingles <- LLM_output %>%
      select(id_llm, Evidence_Text) %>%
      unnest_tokens(ngram, Evidence_Text, token = "ngrams", n = 3) %>%
      filter(!is.na(ngram)) %>%
      distinct()
  
    manual_shingles <- manual_output %>%
      select(id_manual, Evidence_Text) %>%
      unnest_tokens(ngram, Evidence_Text, token = "ngrams", n = 3) %>%
      filter(!is.na(ngram)) %>%
      distinct()
  
    # Count shared shingles for every pair that has at least one in common
    
    matches <- inner_join(LLM_shingles, manual_shingles, by = "ngram", relationship = "many-to-many") %>% # Match shingles
      count(id_llm, id_manual, name = "shared") %>% # Count how many shingles are shared between each row
      left_join(count(LLM_shingles, id_llm, name = "n_llm"), by = "id_llm") %>%
      left_join(count(manual_shingles, id_manual, name = "n_manual"), by = "id_manual") %>%
      mutate(
        containment = shared / pmin(n_llm, n_manual),     # ~1 if the shorter text sits inside the longer one
        jaccard     = shared / (n_llm + n_manual - shared) # how similar they are overall
      )
  
    # Filter for containment and distance
    
    all_matches <- matches %>%
      filter(containment >= 0.5 | jaccard >= 0.5) %>%
      arrange(id_llm, desc(containment))
  
    # Match matches back to extracted data
    
    result <- LLM_output %>%
      rename_with(~ paste0(.x, "_LLM"), -id_llm) %>%
      left_join(all_matches, by = "id_llm") %>%
      full_join(rename_with(manual_output, ~ paste0(.x, "_manual"), -id_manual), 
                by = "id_manual") %>%
      mutate(
        match_status = case_when(
          !is.na(id_llm) & !is.na(id_manual) ~ "Matched",
          is.na(id_manual)                ~ "Only in LLM",
          is.na(id_llm)                ~ "Only in manual"
        )
      ) %>%
      arrange(id_llm, id_manual)
    
    result <- result %>%
      select(Outcome_LLM, Risk_Window_LLM, Confidence_LLM, Source_LLM, Exposure_LLM,
             Rate_LLM, Evidence_Text_LLM, Study_Details_LLM, Limitations_LLM,
             Human_or_Animal_LLM, Drug_LLM, 
             Outcome_manual, Risk_Window_manual, Confidence_manual, Source_manual,
             Exposure_manual, Rate_manual, Evidence_Text_manual, Study_Details_manual,
             Limitations_manual, Human_or_Animal_manual, Drug_manual, match_status, id_llm, id_manual)
  
    #######################################################
    # Summary Table for Matching success
    #######################################################
    
    summary_tbl <- tibble(
      match_status = c("Matched pairs", "LLM texts matched", "Manual texts matched",
                       "In LLM Extraction only", "In manual extraction only"),
      n = c(
        sum(result$match_status == "Matched"),
        n_distinct(result$id_llm[result$match_status == "Matched"]),
        n_distinct(result$id_manual[result$match_status == "Matched"]),
        sum(result$match_status == "Only in LLM"),
        sum(result$match_status == "Only in manual")
      )
    ) %>%
      setNames(., c("match_status", drug)) %>%
      merge(summary_tbl, by = "match_status", sort = F)
    
    #######################################################
    # Save
    #######################################################
  
    output_file <- file.path(output_dir, paste0(drug, "_mapped_extractions.csv"))
    fwrite(result, output_file)
    
  }
  
  fwrite(summary_tbl, output_table)

}
