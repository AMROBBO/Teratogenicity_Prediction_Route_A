##
# Reading in and filtering drug indications extracted from DrugBank.
# These outcomes have been extracted from Associated Conditions section on the
# drug's page on DrugBank
# Approval level includes indication, off lable use and over the counter use
##

#######################################################
# Load libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

raw_data <- Sys.getenv("rawdatadir")
interim_data <- Sys.getenv("interimdatadir")

input_dir <- file.path(raw_data, "reported_outcomes/Drug_Bank")
output_dir <- file.path(interim_data, "reported_outcomes/Drug_Bank_outcomes")

#######################################################
# Read in Drug Bank Associated Conditions
#######################################################

drug_bank <- fread(file.path(input_dir, "Drug_Bank_Indications.csv"))

#######################################################
# Filter
#######################################################

indications_interest <- c("Treatment of",
                          "Prevention of",
                          "Management of",
                          "Prophylaxis of")

indications <- drug_bank %>%
  filter(`Indication Type` %in% indications_interest) %>%
  select(Drug, `Indication Type`, Indication, `Approval Level`) %>%
  unique()
  
#######################################################
# Save
#######################################################

fwrite(indications, file.path(output_dir, "Drug_Bank_Indications.csv"))
