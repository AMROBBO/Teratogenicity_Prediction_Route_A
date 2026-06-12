## 
# Reading in extracted outcomes from UKTIS Monographs and using Deepseek to 
# assess the strength of evidence backing each outcome.
##

#######################################################
# Load libraries
#######################################################

library(dotenv)
library(pdftools)
library(glue)
library(jsonlite)
library(rollama)
library(dplyr)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")

input_dir <- file.path(interim_data, "reported_outcomes/UKTIS_outcomes/2_Extracted_Outcomes")
output_dir <- file.path(interim_data, "reported_outcomes/UKTIS_outcomes/3_Adjudicated_Outcomes")

#######################################################
# Initialise model
#######################################################

pull_model("deepseek-r1:32b")

#######################################################
# Assigning Data Source Options
#######################################################

dataset <- "UKTIS Monographs"

#######################################################
# Creating Confidence Prompt + Query Function
#######################################################

# Prompt to assess the strength of evidence for each outcomes using study type, 
# sample sizes, human v animal data ect

adjudicating_prompt <- glue('Task: Adjudicate and normalize extracted teratology evidence.
                          
                          You will receive extracted evidence from a ', dataset,' in a json format.
                          
                          Your task:
                          - standardize formatting
                          - remove true duplicates
                          - preserve conflicting findings
                          - preserve negative findings
                          - assign confidence scores from 1-5
                          - maintain evidence traceability
                          
                          Do NOT remove findings solely because evidence is weak, uncertain, conflicting, negative, or statistically non-significant.
                          
                          Confidence Scoring Framework:
                          
                          5:
                          - large replicated human studies
                          - meta-analyses
                          - strong statistical support
                          - good confounder adjustment
                          - clear magnitude of effect
                          
                          4:
                          - moderate observational evidence
                          - some replication
                          - moderate limitations
                          
                          3:
                          - mixed findings
                          - confounding concerns
                          - limited replication
                          - uncertain significance
                          
                          2:
                          - case reports only
                          - small cohorts
                          - major limitations
                          - sparse evidence
                          
                          1:
                          - theoretical concern only
                          - insufficient evidence
                          - isolated anecdotal findings
                          - no demonstrated increased risk
                          
                          Confidence Rules:
                          - Base confidence ONLY on evidence explicitly described
                          - Human data generally outweighs animal data
                          - Replicated findings increase confidence
                          - Explicit confounding lowers confidence
                          - Non-significant findings reduce confidence
                          - Large controlled studies outweigh case reports
                          
                          Deduplication Rules:
                          - Remove only truly duplicate rows
                          - Preserve distinct:
                            - studies
                            - exposure windows
                            - populations
                            - conflicting conclusions
                            - evidence sources
                          
                          Output schema:
                          
                          {{{{
                            "Drug": "..."
                            "Outcomes": [
                              {{{{
                                "Outcome": "..."
                                "Risk_Window": "..."
                                "Confidence": 0
                                "Source": "..."
                                "Exposure": "..."
                                "Rate": "..."
                                "Evidence_Text": "..."
                                "Study_Details": "..."
                                "Limitations": "..."
                                "Human_or_Animal": "..."
                              }}}}
                            ]
                          }}}}
                          
                          Important:
                          - Do NOT invent evidence.
                          - Preserve uncertainty.
                          - Preserve exact terminology.

                          Output Rules:
                          - Output ONLY valid JSON
                          - No commentary
                          - No markdown code fences
                          - No summaries
                          ')

make_adjudicating_query <- function(report_chunk, drug_name){
  glue(adjudicating_prompt,
       '
       
       Drug name: {drug_name}
       
       # Raw Text
       {report_chunk}
       ')
}

#######################################################
# Reading in and adjudicating each Monograph chunk
#######################################################

# Extracted outcomes
extracted_report <- list.files(input_dir, full.names = T)

for (f in extracted_report){
  
  # Extract drug chunk
  drug <- unlist(strsplit(f, split = "/"))[10]
  
  # Each outcome chunk
  chunks <- list.files(f, full.names = T)
  
  for (i in chunks){
    
    # Extract subtitle
    chunk_name <- unlist(strsplit(i, split = "/"))[11]
    chunk_name <- unlist(strsplit(chunk_name, split = "[_\\.]"))
    chunk_name <- chunk_name[-(length(chunk_name))]
    chunk_name <- paste(chunk_name, collapse = "_")
    
    # Read chunk
    chunk <- readLines(i, warn = F)
    
    report <- paste(chunk, collapse = "\n")
    
    # Assess strength of outcome evidence
    adjudicating_query <- make_adjudicating_query(report, drug)
    adjudicating_result <- query(adjudicating_query)
    adjudicating_content <- adjudicating_result[[1]]$message$content
    
    # Save adjudicated outcomes
    output_path <- file.path(output_dir, drug)
    
    if (!dir.exists(output_path)){
      dir.create(output_path)
    }
    
    output_file <- file.path(output_path, paste0(chunk_name, ".txt"))
    
    writeLines(adjudicating_content, output_file)
  }
}
