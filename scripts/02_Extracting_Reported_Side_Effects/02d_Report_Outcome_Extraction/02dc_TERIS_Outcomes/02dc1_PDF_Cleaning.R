##
# Reading in TERIS Reports as PDFs, cleaning and formatting.
#
# Breaking down the text into chunks, removing headers, page numbers, fixing 
# breaks in words and sentences and reformatting some of the structure to help 
# the extraction model later.
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
library(stringr)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

raw_data <- Sys.getenv("rawdatadir")
interim_data <- Sys.getenv("interimdatadir")

input_dir <- file.path(raw_data, "reported_outcomes/TERIS")
output_dir <- file.path(interim_data, "reported_outcomes/TERIS_outcomes/1_Cleaned_PDFs")

#######################################################
# Initialise Model
#######################################################

pull_model("qwen2.5:14b")

#######################################################
# Assigning Data Source Options
#######################################################

dataset <- "TERIS reports"

#######################################################
# SubTitles
#######################################################

# Common subtitles within the Monographs which will be used to break the text
# down into manageable chunks

subtitles <- c(
  "Summary of Available Literature:",
  "MAJOR CONGENITAL ANOMALIES",
  "PREGNANCY AND NEONATAL OUTCOMES",
  "NEURODEVELOPMENTAL OUTCOMES",
  "ANIMAL TERATOLOGY STUDIES",
  "CONCLUSION",
  "Drug Class Summary: Angiotensin-Converting Enzyme \\(ACE\\) Inhibitors",
  "ACE INHIBITOR FETOPATHY",
  "EFFECTS OF ACE INHIBITOR EXPOSURE DURING EMBRYOGENESIS",
  "DRUG CLASS SUMMARY: ANGIOTENSIN II RECEPTOR BLOCKING AGENTS",
  "ARBs FETOPATHY",
  "EFFECTS OF ARBs DURING EMBRYOGENESIS",
  "FETAL METHOTREXATE SYNDROME",
  "NEURODEVELOPMENTAL AND CYTOGENETIC OUTCOMES",
  "ORAL CLEFTS",
  "OTHER SPECIFIC MALFORMATIONS",
  "SPINA BIFIDA",
  "MAJOR CONGENITAL ANOMALIES OVERALL",
  "OTHER CONGENITAL ANOMALIES",
  "FETAL VALPROATE SYNDROME",
  "Drug Class Summary: Angiotensin II Receptor Blocking Agents",
  "References:"
)

# Search pattern that will chunk by subtitles and preserve subtitles

subtitles_search <- paste0("(?=(", paste(subtitles, collapse = "\n|"), "))")

#######################################################
# To Remove
#######################################################

# Common patterns in the text, such as headers, dates, urls ect which are not
# needed and will confuse the model in extraction

patterns <- c(
  "^\\d{2}/\\d{2}/\\d{4},\\s*\\d{2}:\\d{2}\\s+.*",
  "TERIS Agent",
  "Updated: \\d{2}/\\d{4}",
  "^https://terisweb.pear-net.org",
  "     Agent Metadata",
  "      Bibliographic Search Date: *",
  "      CAS: *",
  "   © 2026 PEAR-Net Society"
)

pattern <- paste(patterns, collapse = "|")

#######################################################
# Creating Cleaning Prompt + Query Function
#######################################################

# Prompt to clean the text, such as repairing broken works and sentences

cleaning_prompt <- glue('Task: LOSSLESS OCR cleanup of ',dataset,' text.
                        
                        Your task is ONLY to repair OCR and formatting issues.
                        
                        This is NOT summarization.
                        
                        Critical Rules:
                        
                        Preserve EVERY medically relevant word, sentence, numerical value, citation, study detail, limitation, and uncertainty statement exactly as written.
                        
                        If any scientific content is omitted, paraphrased, generalized, merged, shortened, or rewritten, the output is incorrect.
                        
                        Treat every sentence as atomic.
                        
                        Do not rewrite, shorten, merge, split, reinterpret, or reorder sentences unless required to repair obvious OCR corruption.
                        
                        Only repair text when the intended wording is highly certain from immediate context.
                        
                        If uncertain, preserve the original wording exactly.
                        
                        Allowed Operations ONLY:
                        - repair obvious OCR corruption
                        - repair broken words
                        - repair broken line breaks
                        - join sentences split across lines
                        - rejoin paragraphs split by OCR/PDF artifacts
                        - preserve section headings
                        - preserve citations
                        - preserve bullet lists
                        - preserve tables
                        - preserve all study text exactly

                        Do NOT:
                        - summarize
                        - simplify
                        - interpret
                        - compress
                        - paraphrase
                        - deduplicate scientific statements
                        - rewrite wording
                        - merge studies
                        - remove repeated findings
                        - remove caveats
                        - remove uncertainty language
                        - remove statistical information
                        - remove negative findings
                        - remove animal data
                        - remove background rate discussion
                        
                        # Important
                        
                        Preserve verbatim every sentence discussing:
                        - pregnancy outcomes
                        - congenital malformations
                        - miscarriage
                        - fetal outcomes
                        - neonatal outcomes
                        - breastfeeding
                        - paternal exposure
                        - trimester effects
                        - odds ratios
                        - confidence intervals
                        - cohort size
                        - confounding
                        - study limitations
                        - statistical significance
                        
                        Output rules:
                        - output only cleaned document text
                        - preserve ALL scientific content.
                        - no explanations
                        - no commentary
                        - no prompt repetition
                        - no markdown code fences
                        - no labels
                        
                        The response must begin immediately with cleaned text and end immediately after cleaned text.

                        If any text outside the cleaned document is included, the output is incorrect.
                        
                        Before returning output, verify that:
                        
                        - every study described in the input remains present
                        - every numerical value remains present
                        - every outcome discussed remains present
                        - every uncertainty statement remains present
                        - every animal study remains present
                        - every exposure window remains present
                        
                        If anything is missing, the output is incorrect.
                        
                        # Raw Text
                        
                        ')

make_cleaning_query <- function(report){
  glue(cleaning_prompt,
       '
       {report}
       ')
  }

#######################################################
# Creating Formatting Prompt + Query function
#######################################################

# Prompt to normalise the text format, to make data extraction easier for the 
# model in the next step

formatting_prompt <- glue('Task: LOSSLESS STRUCTURAL NORMALIZATION of cleaned ', dataset,' text.
                          
                          Preserve ALL scientific and medical content exactly.
                          
                          This is NOT summarization, interpretation, or rewriting.
                          
                          Treat every sentence as atomic.
                          
                          Do not rewrite, merge, split, reorder, reinterpret, summarize, paraphrase, simplify, or compress sentences.                          
                          
                          All:
                          - study findings
                          - numerical values
                          - confidence intervals
                          - risk estimates
                          - caveats
                          - uncertainty statements
                          - negative findings
                          - trimester information
                          - exposure timing
                          - confounding statements
                          - statistical significance
                          - study limitations
                          must remain unchanged.
                          
                          Statements reporting:
                          - no increased risk
                          - no association
                          - no statistically significant increase
                          - comparable background rates
                          must remain unchanged.

                          Allowed operations ONLY:
                          - add markdown headings already supported by the source text
                          - separate paragraphs
                          - repair markdown table formatting
                          - preserve bullet lists
                          - preserve citations
                          - preserve study groupings
                          - preserve section hierarchy
                          - convert existing tabular text into markdown tables without changing wording, values, or row relationships
                          
                          Do NOT:
                          - summarize
                          - paraphrase
                          - merge findings
                          - merge studies
                          - remove duplicated scientific findings
                          - simplify wording
                          - rewrite conclusions
                          - introduce inferred structure
                          - introduce new headings unsupported by source text
                          - infer missing table cells
                          - summarize tables
                          
                          Maintain exact separation between:
                          - cohort studies
                          - registries
                          - case reports
                          - case series
                          - meta-analyses
                          - observational studies
                          - animal studies
                          
                          Output rules:
                          - output only normalized markdown text
                          - no explanations
                          - no commentary
                          - no summaries
                          - no labels
                          - no prompt repetition
                          - no markdown code fences
                          
                          The response must begin immediately with normalized document text and end immediately after normalized document text.                          
                          
                          Internally verify before responding that:                          
                          - every study remains present
                          - every numerical value remains present
                          - every outcome remains present
                          - every uncertainty statement remains present
                          - every negative finding remains present
                          - every animal study remains present
                          - every exposure window remains present
                          
                          If anything is missing, the output is incorrect.
                          
                          # Raw Text
                          
                          ')

make_formatting_query <- function(cleaned_report){
  glue(formatting_prompt,
       '
       {cleaned_report}
       ')
}

#######################################################
# Reading in, Cleaning and Formatting TERIS Reports
#######################################################

# Raw TERIS PDFs
teris_files <- list.files(input_dir, full.names = T)

for (f in teris_files){
  
  # Extract drug name
  drug_name <- unlist(strsplit(f, split = "[ \\.]"))
  drug_name <- drug_name[3:(length(drug_name)-1)]
  drug_name <- paste(drug_name, collapse = "_")
  
  # Read report
  teris <- pdftools::pdf_text(f)
  
  report <- paste(teris, collapse = "\n")
  
  # Remove headers, footers ect
  lines <- strsplit(report, "\n")[[1]]
  lines <- lines[!grepl(pattern, lines, ignore.case = F)]
  report <- paste(lines, collapse = "\n")
  
  # Split into chunks
  chunks <- str_split(report, subtitles_search)[[1]]
  
  # Run through each chunk
  for (chunk in chunks){
    
    # Subtitle
    subtitle <- unlist(strsplit(chunk, split = "\n"))[1]
    
    #subtitle <- gsub("         ", "", subtitle)
    subtitle <- gsub(" ", "_", subtitle)

    if (subtitle == ""){
      subtitle <- "Introduction"
    }

    # Clean chunk
    cleaning_query <- make_cleaning_query(chunk)
    cleaning_result <- query(cleaning_query)
    cleaning_content <- cleaning_result[[1]]$message$content
    
    # Format chunk
    formatting_query <- make_formatting_query(cleaning_content)
    formatting_result <- query(formatting_query)
    formatting_content <- formatting_result[[1]]$message$content
    
    # Save formatted chunk
    output_path <- file.path(output_dir, drug_name)
    
    if (!dir.exists(output_path)) {
      dir.create(output_path)
    }
    
    output_file <- file.path(output_path, paste(drug_name, subtitle, "cleaned.txt", sep = "_"))
    
    writeLines(formatting_content, output_file)
  }
}
