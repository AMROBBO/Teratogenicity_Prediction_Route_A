## 
# Reading in assessed LLM and manual extractions and quanitifying the concordance between the two extraction methods.
#   - Outcomes extracted by both methods
#       - Extacted by both methods and assessed as similar
#       - Extracted by both methods and assessed as dissimilar
#   - Outcomes extracted by only LLM
#   - Outcomes extracted by only manual extraction
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

evaluation_dir <- file.path(interim_data, "reported_outcomes/Extraction_Evaluation/Reprotox/5_mapped_ordered_agreement")

#######################################################
# Read in data
#######################################################

evaluation_files <- evaluation_dir %>%
  list.files(, pattern = "mapped_ordered_agreement.csv", full.names = T)

f <- evaluation_files[1]

evaluation <- f %>%
  fread()

#######################################################
# Report detection
#######################################################

# Creating a summary table of the number of outcomes extracted by both methods, only LLM, and only manual extraction

only_llm <- evaluation %>%
    filter(Agreement == "Only_in_LLM") %>%
    select(Outcome_LLM) %>%
    unique() %>%
    nrow()

only_manual <- evaluation %>%
    filter(Agreement == "Only_in_manual") %>%
    select(Outcome_manual) %>%
    unique() %>%
    nrow()

both_methods <- evaluation %>%
    filter(Agreement != "Only_in_LLM" & Agreement != "Only_in_manual")

## Somehow go through and identify outcomes that were extracted by both methods without duplicating