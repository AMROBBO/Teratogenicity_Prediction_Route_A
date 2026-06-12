##
# Using openGWAS data to test association between drug target gene
# and primary indications of drugs
# 4. Map openGWAS outcomes to drugbank indications
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)
library(tidyr)
library(gpmapr)
library(ieugwasr)
library(TwoSampleMR)
library(glue)
library(jsonlite)
library(rollama)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")

mapped_file <- file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/Biobert_output/indication_opengwas_similarity_matrix.csv")
top_10_output_dir <- file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/Biobert_output")
qwen_output_dir <- file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/Qwen_output")

#######################################################
# 4. Map openGWAS outcomes to drugbank indications
#######################################################

similarity_matrix <- as.matrix(fread(mapped_file), rownames = 1)

#######################################################
# Extracting the top Matches and Cosine values
#######################################################

get_top <- function(sim_matrix, n = 10) {
  results <- do.call(rbind, lapply(1:nrow(sim_matrix), function(i) {
    indication_term <- rownames(sim_matrix)[i]
    sim_values <- sim_matrix[i, ]
    
    # Get indices of top N similarities
    top_matches <- order(sim_values, decreasing = TRUE)[1:n]
    
    valid_idx <- which(!is.na(top_matches))
    # Create a data.frame with TermA, TermB, and cosine similarity
    data.frame(
      Indication_term = indication_term,
      OpenGWAS_term = colnames(sim_matrix)[top_matches[valid_idx]],
      CosineSimilarity = sim_values[top_matches[valid_idx]],
      stringsAsFactors = FALSE
    )
  }))
  
  return(results)
}

top_matches <- get_top(similarity_matrix, 10)

# Save
fwrite(top_matches, file.path(top_10_output_dir, "mapped_outcomes_top_10.csv"))

#######################################################
# Extracting appropriate matches
#######################################################

similarity_prompt <- glue('This is a proxy phenotype mapping task.

                    The goal is to determine whether the OpenGWAS outcome can reasonably act as a proxy phenotype for the DrugBank indication.
                    
                    Shared risk factors alone are insufficient.
                    
                    Shared symptoms alone are insufficient.
                    
                    Anatomical proximity alone is insufficient.
                    
                    The outcome should represent:
                    - the same disease,
                    - a recognized subtype,
                    - a recognized synonym,
                    - a diagnosis that would commonly be used as a proxy phenotype in genetic or epidemiological studies,
                    - a direct manifestation that would commonly be considered the same disease process.
                    
                    Rules:
                    - You may ONLY select terms from the provided data.
                    - Do NOT invent or rephrase terms.
                    - Be conservative. Avoid weak associations.
                    - Evaluate each openGWAS outcome independently.
                    - Do not allow the score assigned to one outcome to influence the score assigned to another outcome.
                    
                    For each openGWAS outcome, assign a similarity score from 0 to 5 following the provided rationale:
                    
                    5 = Same disease, accepted synonym, identical phenotype, or clearly equivalent clinical diagnosis.
                    
                    4 = Very close disease subtype, recognized spectrum disorder, or clinically interchangeable phenotype.
                    
                    3 = Related disease with substantial overlap but not generally considered equivalent.
                    
                    2 = Shared pathology, risk factors, anatomy, or complications only.
                    
                    1 = Weak conceptual relationship.
                    
                    0 = No meaningful disease-level relationship.
                    
                    The provided cosine similarity score is supporting evidence only.
                    Use it to prioritize review but do not assume high cosine similarity implies a true disease match.
                    Disease meaning takes precedence over cosine similarity.
                    
                    Do NOT score highly solely because:
                    - both involve the same organ system
                    - one is a risk factor for the other
                    - one is a complication of the other
                    - one is a biomarker of the other
                    - one commonly co-occurs with the other
                    - one is a treatment target of the other
                    
                    For each match provide:
                    - DrugBank_term
                    - openGWAS_term (exact string from list)
                    - CosineSimilarity
                    - similarity_score (0 to 5)
                    - match_decision (must be determined solely from similarity_score):
                      - similarity_score >= 4 -> TRUE
                      - similarity_score < 4 -> FALSE
                    - brief_rationale (Explain why the two terms do or do not represent the same underlying disease phenotype.)
                    
                    Output:
                    
                    Return in a strict JSON format:
                    ```
                    {{{{
                      "DrugBank_Indication": "...",
                      "matches": [
                      {{{{
                        "openGWAS_outcome": "...",
                        "cosine_similarity": 0.0-1.0,
                        "similarity_score": 0.0-5.0,
                        "match_decision": "...",
                        "brief_rationale": "..."
                      }}}}
                    ]
                    }}}}
                    ```
                    
                    Return all evaluated outcomes.
                    Preserve the input order of the openGWAS outcomes.
                    Do not reorder outcomes by similarity_score.
                    
                    A score of 0 is acceptable and should be used whenever no meaningful disease-level relationship exists.
                    Do not assign non-zero scores simply because both terms are medical conditions.
                    ')

similarity_query <- function(indication, opengwas_outcomes){
  glue(similarity_prompt,
       '
       
       The terms are:
       DrugBank Indication: {indication}
       
       OpenGWAS Outcomes: {opengwas_outcomes}
       
       ')
}

combined <- list()

for (i in unique(top_matches$Indication_term)){
  indication <- i
  
  opengwas_outcomes <- top_matches %>% 
    filter(Indication_term == i) %>% 
    select(OpenGWAS_term)
  
  query <- similarity_query(indication, opengwas_outcomes)
  result <- query(query)
  content <- result[[1]]$message$content
  
  output_path <- file.path(qwen_output_dir, paste0(gsub(" ", "_", indication), ".json"))
  writeLines(content, output_path)
  
  text <- readLines(output_path, warn = F)
  
  if(length(grep("```", text)) > 1){
    
    start <- grep("```", text)[1]
    end <- grep("```", text)[2]
    
    json_content <- paste(text[(start+1):(end-1)], collapse = "\n")
    
    data <- fromJSON(json_content)
    
  } else{
    
    data <- fromJSON(text)
    
  }
  
  approved <- data$matches %>% 
    filter(match_decision == TRUE)
  
  combined[[indication]]$matches <- c(
    combined[[indication]]$matches,
    approved
  )
}

#######################################################
# Save
#######################################################

write_json(combined, file.path(qwen_output_dir, "Qwen_output_combined.json"))
