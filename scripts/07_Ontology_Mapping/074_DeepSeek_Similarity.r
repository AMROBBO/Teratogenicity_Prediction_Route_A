##
# Reading in output from Ollama Qwen LLM
# Creating prompt to ask ollama deepseek model to dig deeper into the conceptual
# similarities between the pairs, assign a score and either accept or reject the 
# pairing according to the score
# Send prompt to ollama and save output
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

input_dir <- file.path(interim_data, "ontology_mapping/output_data/Qwen")
output_dir <- file.path(interim_data, "ontology_mapping/output_data/DeepSeek")

#######################################################
# Set outcome of interest
#######################################################

#predicted <- "omim"
#predicted <- "gpmap"
predicted <- "one_test"

#observed <- "all"
#observed <- "cong"
observed <- "two_test"

if (predicted == "gpmap"){
  dataset = "Genotype-Phenotype Map"
} else if (predicted == "omim"){
  dataset = "OMIM"
} else {
  dataset <- "test"
}

if (observed == "all" | observed == "cong"){
  observed_name <- "FAERS"
  observed_search <- paste(tolower(observed_name), observed, sep = "_")
} else if (observed == "two_test"){
  observed_name <- "test"
  observed_search <- "two_test"
}

#######################################################
# Create functions
#######################################################

# Creating a query that will cycle through each OMIM term and its corresponding FAERS terms

deepseek_prompt <- glue("
You are adjudicating conceptual similarity between ", dataset, " disease terms and ", observed_name, " adverse event terms.
Your task is STRICT conceptual evaluation.

Use the following conceptual similarity rationale to assign a similarity score: 
5: Exact or near-perfect conceptual match (ACCEPT)
4: Strong pathophysiological relationship (same disease category, direct complications) (ACCEPT)
3: Moderate relationship (shared mechanisms, risk factors, or related conditions) (ACCEPT)
2: Weak relationship (indirect connections, shared symptoms) (ACCEPT)
1: Very loose association (REJECT)
0: No association (REJECT)

Rules:
- The ", observed_name, " term must represent the same core pathology or a direct clinical manifestation of the ", dataset, " condition.
- Broader, narrower, or loosely related conditions should be rejected.
- Shared anatomy alone is insufficient.
- Prefer rejection over weak similarity.

You must evaluate each proposed match independently.

You are provided with a similarity score from a prior model. Treat this score as weak prior information only. 
You must independently evaluate conceptual similarity. Do not increase your confidence solely because the prior score is high. 
If your reasoning disagrees with the prior score, ignore it.

For each candidate:
- Decide ACCEPT or REJECT.
- Provide confidence (0-5).
- Provide a short explanation grounded in conceptual overlap or mismatch.
                        ")

make_deepseek_query <- function(term, matches){
  glue(deepseek_prompt,
       '
       The terms are:
       
       ', dataset, ' term: {term}
       
       ', observed_name, ' candidate terms (choose only from this list): {matches}
       
       Return in a strict JSON format:
       {{
       "', predicted, '_term": "..."
       "adjudicated_matches": [
       {{
       "',tolower(observed_name),'_term": "..."
       "decision": "ACCEPT" or "REJECT"
       "confidence": 0-5
       "brief_rationale": "..."
       }}
       ]
       }}
       
       Encase the json format with ```
       '
  )
  }

# Send query in to ollama - and save

submit_query <- function(query, drug, outcome){
  result <- query(query)
  content <- result[[1]]$message$content
  
  output_path <- file.path(output_dir, drug)
  
  if (!dir.exists(output_path)) {
    dir.create(output_path)
  }
  
  output_path <- file.path(output_path, predicted)
  
  if (!dir.exists(output_path)) {
    dir.create(output_path)
  }
  
  output_path <- file.path(output_path, observed)
  
  if (!dir.exists(output_path)) {
    dir.create(output_path)
  }
  
  outcome_collapsed <- gsub(" ", "_", outcome)
  outcome_collapsed <- gsub("/", "_", outcome_collapsed)
  outcome_collapsed <- gsub(",", "", outcome_collapsed)
  outcome_collapsed <- gsub("\\(", "", outcome_collapsed)
  outcome_collapsed <- gsub("\\)", "", outcome_collapsed)
  
  output_file_full <- file.path(output_path, paste(outcome_collapsed, observed, "full.txt", sep = "_"))
  output_file_json <- file.path(output_path, paste0(outcome_collapsed, "_", observed, ".json"))
  
  writeLines(content, output_file_full)
  
  text <- readLines(output_file_full, warn = F)
  
  start <- grep("```", text)[1]
  end <- grep("```", text)[2]
  
  json_content <- paste(text[(start+1):(end-1)], collapse = "\n")
  
  data <- fromJSON(json_content)
  
  write_json(data, output_file_json, pretty = TRUE)
}

#######################################################
# Run model
#######################################################

pull_model("deepseek-r1:8b")

for (f in list.files(input_dir, full.names = T)){
  
  drug <- unlist(strsplit(f, split = "/"))[length(unlist(strsplit(f, split = "/")))]
  files <- list.files(file.path(f, predicted, observed), pattern = ".json", full.names = T)

  if (length(files) > 0){
    for (i in files) {
      
      qwen_output <- jsonlite::fromJSON(i) %>% 
        as.data.frame()
      
      prediction_column <- paste0(predicted, "_term")
      observation_column <- paste0("matches.", tolower(observed_name), "_term")
      
      prediction <- unique(qwen_output[[prediction_column]])
      observation <- qwen_output %>% 
        filter(qwen_output[[prediction_column]] == prediction) %>% 
        select(all_of(observation_column)) %>% 
        as.list()
      
      message(glue("Submitting query for {prediction} for {drug}"))
      
      query <- make_deepseek_query(prediction, observation)
      
      submit_query(query, drug, prediction)
    }
  }
}
