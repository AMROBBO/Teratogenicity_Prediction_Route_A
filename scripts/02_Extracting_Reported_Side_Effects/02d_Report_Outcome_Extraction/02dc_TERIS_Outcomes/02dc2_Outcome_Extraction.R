##
# Reading in Formatted Monographs and extracting all reported teratogenic drug 
# outcomes
#
# For every chunk of monograph, the model is asked in extract information on any
# reported teratogenic outcome, risk window, rates, source of evidence,
# limitations, and whether the evidence is from humans or animals
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

input_dir <- file.path(interim_data, "reported_outcomes/TERIS_outcomes/1_Cleaned_PDFs")
output_dir <- file.path(interim_data, "reported_outcomes/TERIS_outcomes/2_Extracted_Outcomes")

#######################################################
# Initialise model
#######################################################

pull_model("qwen3:32b")

#######################################################
# Assigning Data Source Options
#######################################################

dataset <- "TERIS reports"

#######################################################
# Creating Extraction Prompt + Query Function
#######################################################

# Prompt to extract every teratogenic outcome, including supporting evidence

extract_prompt <- glue('Task: Extract ALL pregnancy-related evidence from a ', dataset, ' section.
          
                        Inputs:
                        1. ', dataset,' section
                        2. Drug name
                        
                        This is an EXHAUSTIVE extraction task.
                        
                        Extract ALL explicitly stated:
                        - pregnancy outcomes
                        - congenital malformations
                        - miscarriage findings
                        - fetal outcomes
                        - neonatal outcomes
                        - breastfeeding findings
                        - paternal exposure findings
                        - animal reproductive findings
                        - background-rate comparisons
                        - negative findings
                        - statistically non-significant findings
                        - conflicting findings
                        - uncertain findings
                        
                        Extraction Rules (Mandatory):
                        - Only extract information explicitly stated in the text
                        - Do NOT hallucinate or infer outcomes
                        - Do NOT omit weak, uncertain, conflicting, negative, or non-significant findings
                        - Preserve exact medical terminology whenever possible
                        - Preserve exact quantitative wording whenever possible
                        - Preserve exact uncertainty wording whenever possible
                        
                        Each JSON object must represent exactly ONE:
                        - outcome
                        - exposure window
                        - study finding
                        - evidence source
                        
                        Create separate rows when:
                        - studies differ
                        - conclusions differ
                        - exposure windows differ
                        - evidence sources differ
                        - populations differ
                        - animal and human findings differ
                        
                        If uncertain whether a finding is relevant, include it.
                        
                        If a field is absent, use "NR".
                        
                        Do NOT deduplicate findings.
                        
                        Required JSON schema:
                        
                        {{{{
                          "Drug": "..."
                          "Outcomes": [
                            {{{{
                              "Outcome": "..."
                              "Risk_Window": "..."
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
                        
                      Field Definitions:
                      
                      Drug:
                      - Use exact provided drug name
                      
                      Outcome:
                      - Use exact wording from report
                      - Do not normalize terminology
                      
                      Risk_Window:
                      - Preserve exact gestational timing wording when available
                      - Examples:
                        - First trimester
                        - Late pregnancy
                        - Throughout pregnancy
                        - Periconception
                        - NR
                      
                      Source:
                      Use one or more of:
                        - Case Report
                        - Case Series
                        - Observational study
                        - Cohort study
                        - Case-control study
                        - Meta-analysis
                        - Registry data
                        - Pharmacokinetics data
                        - Safety Review
                        - Prospective therapeutic exposure data
                        - Retrospective therapeutic exposure data
                        - Animal models (specify species)
                        - Other
                        
                        Exposure:
                        - Maternal exposure
                        - Paternal exposure
                        - Breastfeeding exposure
                        - NR
                        
                        Rate:
                        - Preserve exact reported risk wording
                        - Include odds ratios, prevalence, frequencies, confidence intervals, background-rate comparisons if stated
                        
                        Evidence_Text:
                        - Copy exact supporting sentence(s)
                        - Do NOT paraphrase
                        
                        Study_Details:
                        - Extract sample size, comparator, study type, statistical significance, cohort description if stated
                        
                        Limitations:
                        - Extract explicit caveats including:
                          - confounding
                          - limited data
                          - inconsistent findings
                          - observational limitations
                          - lack of replication
                          - small sample size
                        
                        Human_or_Animal:
                        - Human
                        - Animal
                        - Mixed
                        
                        Output Rules:
                        - Output ONLY valid JSON
                        - No commentary
                        - No summaries
                        - No markdown code fences
                        - No explanatory text')

make_extraction_query <- function(report_chunk, drug_name){
  glue(extract_prompt,
       '
       
       Drug name: {drug_name}
       
       # Raw Text
       {report_chunk}
       ')
}

#######################################################
# Reading in and extracting outcomes from each Monograph chunk
#######################################################

# Cleaned and formatted text
cleaned_report <- list.files(input_dir, full.names = T)

for (f in cleaned_report){
  
  # Extract drug name
  drug <- unlist(strsplit(f, split = "/"))[10]
  
  # Each text chunk
  chunks <- list.files(f, full.names = T)
  
  for (i in chunks){
    
    # Extract subtitle
    chunk_name <- unlist(strsplit(i, split = "/"))[11]
    chunk_name <- unlist(strsplit(chunk_name, split = "[_\\.]"))
    chunk_name <- chunk_name[-((length(chunk_name)-1):(length(chunk_name)))]
    chunk_name <- paste(chunk_name, collapse = "_")
    
    chunk_name <- gsub(":", "", chunk_name)
    
    # Read report chunk
    chunk <- readLines(i)
    
    report <- paste(chunk, collapse = "\n")
    
    # Extract outcomes
    extracting_query <- make_extraction_query(report, drug)
    extracting_result <- query(extracting_query)
    extracting_content <- extracting_result[[1]]$message$content
    
    # Save extracted outcomes
    output_path <- file.path(output_dir, drug)
  
    if (!dir.exists(output_path)) {
      dir.create(output_path)
      }
  
    output_file <- file.path(output_path, paste0(chunk_name, ".txt"))
    
    writeLines(extracting_content, output_file)
  }
}
 