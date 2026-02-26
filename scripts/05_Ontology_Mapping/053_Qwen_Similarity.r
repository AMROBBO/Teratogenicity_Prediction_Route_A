##
# Reading in output from biobert model
# Creating a prompt to ask Ollama Qwen LLM to assess the similarity between the terms
# and assign a similarity score for each pairing
# Send query to ollama and saving output
##

#######################################################
# Load libraries
#######################################################

library(dotenv)
library(glue)
library(jsonlite)
library(rollama)
library(dplyr)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")

input_dir <- file.path(interim_data, "ontology_mapping/output_data/UMCU/SapBERT-from-PubMedBERT-fulltext_bf16")
output_dir <- file.path(interim_data, "ontology_mapping/output_data/Qwen")

#######################################################
# Set outcome of interest
#######################################################

outcome_cat <- "all"
#outcome_cat <- "cong"

#######################################################
# Create functions
#######################################################

# Creating a query that will cycle through each OMIM term and its corresponding FAERS terms

qwen_prompt <- glue("You are performing controlled conceptual alignment between OMIM disease terms and FAERS adverse event terms.

Rules:
- You may ONLY select terms from the provided FAERS list.
- Do NOT invent or rephrase FAERS terms.
- This is a similarity task, not strict equivalence.
- Return all conceptually related FAERS terms.
- If none are meaningfully related, return an empty list.
- Be conservative. Avoid weak associations.

For each FAERS term, assign a similarity score from 0 to 5 following the provided rationale:
5: Exact or near-perfect conceptual match
4: Strong pathophysiological relationship (same disease category, direct complications)
3: Moderate relationship (shared mechanisms, risk factors, or related conditions)
2: Weak relationship (indirect connections, shared symptoms)
1: Very loose association
0: No association (Don't include in the output)

For each match provide:
- omim_term
- faers_term (exact string from list)
- similarity_score (0 to 5)
- brief_rationale (1 sentence grounded in shared pathology or anatomy)
")

make_qwen_query <- function(term, matches){
  glue(qwen_prompt,
       '
       
       The terms are:
       
       OMIM term: {term}
       
       FAERS candidate terms (choose only from this list):
       {matches}
       
       Return in a strict JSON format:
       {{
       "omim_term": "..."
       "matches": [
       {{
       "faers_term": "..."
       "similarity_score": 0.0-1.0
       "brief_rationale": "..."
       }}
       ]
       }}
       
       Encase the json format with ```
       '
  )
  }

# Send query in to ollama - and save

submit_qwen_query <- function(qwen_query, drug, outcome){
  result <- query(qwen_query)
  content <- result[[1]]$message$content
  
  output_path <- file.path(output_dir, drug)
  
  if (!dir.exists(output_path)) {
    dir.create(output_path)
  }
  
  output_path <- file.path(output_path, outcome_cat)
  
  if (!dir.exists(output_path)) {
    dir.create(output_path)
  }
  
  outcome_collapsed <- gsub(" ", "_", outcome)
  outcome_collapsed <- gsub("/", "_", outcome_collapsed)
  
  output_file_full <- file.path(output_path, paste(outcome_collapsed, outcome_cat, "full.txt", sep = "_"))
  output_file_json <- file.path(output_path, paste0(outcome_collapsed, "_", outcome_cat, ".json"))
  
  writeLines(content, output_file_full)
  
  text <- readLines(output_file_full, warn = F)
  
  if(length(grep("```", text)) > 1){
    
    start <- grep("```", text)[1]
    end <- grep("```", text)[2]
    
    json_content <- paste(text[(start+1):(end-1)], collapse = "\n")
    
    data <- fromJSON(json_content)
    
  } else{
    
    data <- fromJSON(text)
    
  }
  
  write_json(data, output_file_json, pretty = TRUE)
  
}

#######################################################
# Run model - qwen
#######################################################

pull_model("qwen2.5:7b")

for (f in list.files(input_dir, full.names = T)){
  
  drug <- unlist(strsplit(f, split = "/"))[length(unlist(strsplit(f, split = "/")))]
  file <- grep(paste("omim_faers", outcome_cat, "top_30.csv", sep = "_"), list.files(f, full.names = T), value = T)

  if (length(file) > 0){
    biobert_terms <- read.csv(file)
    
    OMIM_terms <- unique(biobert_terms$Predicted_term)
    
    for (i in OMIM_terms){
      OMIM <- i
      FAERS <- biobert_terms %>% 
        filter(biobert_terms$Predicted_term == OMIM) %>% 
        select(Observed_term)
      
      query <- make_qwen_query(OMIM, FAERS)
      
      submit_qwen_query(query, drug, OMIM)
    }
  }
}
