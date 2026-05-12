##
# Reading in Monographs and extracting the predicted drug outcomes into a data
# table
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

raw_data <- Sys.getenv("rawdatadir")
interim_data <- Sys.getenv("interimdatadir")

input_dir <- file.path(raw_data, "reported_outcomes/UKTIS_Monographs")
output_dir <- file.path(interim_data, "reported_outcomes/UKTIS_outcomes")

#######################################################
# Reading in Monographs
#######################################################

monographs <- list.files(input_dir, full.names = T)

monograph <- pdftools::pdf_text(monographs[1])

cat(monograph)

#######################################################
# Assigning Data Source Options
#######################################################

#######################################################
# Confidence levels Guidance
#######################################################

#######################################################
# Output format
#######################################################

#######################################################
# Creating Prompt
#######################################################

#######################################################
#######################################################
#######################################################
#######################################################
### Trying to glue together the prompt and monograph to then send off query
prompt <- glue('You will be extracting teratogological evidence from UKTIS reports.
               
               You will be provided with:
               1. A UKTIS Monograph Report
               2. The name of the drug the report is referring to
               Your task is to extract all reported pregnancy-related outcomes associated with the specified drug and output them in a structured json format.
               
               Core Extraction Rules:
               - Only extract information explicitly stated by the report
               - Do not hallicinate or invent outcomes, risks, or rates
               - Preserve the exact medical terminology used in the report for outcomes whenever possible
               - Extract every distinct outcome separately
               - If multiple studies report the same outcome with differing conclusions, create separate rows
               - If an outcome is discussed for multiple exposure windows, create separate rows
               - If confidence differs by study/source, create separate rows
               - If a field is absent, use "NR", do not invent missing values
               
               Output Columns:
               Produce the output in this exact JSON format:
               {{"Drug": "..."
               "Outcomes": [
               {{
               "Outcome": "..."
               "Risk_window": "..."
               "Confidence": 0-5
               "Source": "..."
               "Exposure": "..."
               "Rate": "..."
               }}
               ]
               }}
               Encase the json format with ```

               Column Definitions:
               Drug:
               - Use the exact drug name provided by the user
               
               Outcome:
               - Extract the adverse or reported pregnancy outcome using the exact wording from the report
               - Do not generalise or normalise terminology
               
               Risk window:
               - Extract the trimester or gestational exposure period associated with the risk if stated
               - Examples:
                - First Trimester
                - Second Trimester
                - Third Trimester
                - NR (If not reported)
               
               Confidence:
               Assign a confidence score from 1-5 based on the strength and reliability of the evidence presented in the report.
               Use the following framework:
               
               Confidence 5 — Strong evidence
               - Large well-controlled cohort/meta-analysis
               - Consistent findings across studies
               - Statistically significant
               - Clear magnitude of risk
               - Good adjustment for confounders
               - Human therapeutic exposure data
               - Replicated evidence
               
               Confidence 4 — Moderately strong evidence
               - Moderate-sized observational data
               - Some consistency across studies
               - Reasonable control for confounding
               - Signal likely meaningful but limitations exist
               
               Confidence 3 — Moderate/uncertain evidence
               - Mixed findings
               - Limited cohort size
               - Some confounding concerns
               - Non-significant trends
               - Sparse replication
               
               Confidence 2 — Weak evidence
               - Case reports/series only
               - Limited animal data
               - Small sample sizes
               - Poorly controlled studies
               - Major uncertainty acknowledged
               
               Confidence 1 — Very weak or speculative evidence
               - Theoretical concern only
               - Isolated anecdotal evidence
               - No demonstrated increased risk
               - Explicit statement that evidence is insufficient
               
               When assigning confidence, consider:
               - Confounding
               - Limited data
               - Comparison to other studies
               - Background rate discussion
               - Magnitude of increased risk
               - Cohort size
               - Statistical significance
               - Whether findings are replicated
               - Whether evidence is human or animal data
               - Uncertain language in the report
               
               Source:
               Classify the source of evidence using one of the following categories exactly:
               - Primary Indication (treatment of ...)
               - Off-label use
               - Animal models (Specify Animal if stated eg. mice, rats, rabbits, sheep, primates)
               - Case Report
               - Case Series
               - Pharmacokinetics data
               - Observational study (Specify study if stated eg. cohort, case-control, Randomised control, meta-analysis)
               - Safety Review
               - Prospective therapeutic exposure data
               - Retrospective therapeutic exposure data
               - Reporting Data
               - Other (clearly state what the source is/associated if thats all thats said)
               If multiple evidence sources support one statment, include all relevant sources separated by semicolons.
               
               Exposure:
               Classify exposure as one of:
               - Maternal exposure
               - Paternal exposure
               - Breastfeeding exposure
               - NR (if not specified)
               
               Rate:
               Extract the reported frequency, relative risk, odds ratio, prevalence, or increase in risk if stated.
               Preserve the exact quantitative wording and comparisons to background rate where possible
               
               Final Output Requirements:
               - Output ONLY the final structed json
               - No narrative summary
               - Include every relevant outcome mentioned in the report
               - Ensure rows are non-duplicative unless representing distinct evidence sources or findings
               - Maintain consistent formatting
               ')

test <- paste(monograph, collapse = "\n")

pull_model("qwen3.5:0.8b")

query_mon <- glue(prompt,
                  'The UKTIS monograph is:',
              test)

result <- query(query)



