##
# Using DrugBank data to identify targets of teratogenic drugs of interest
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)
library(drugbankR)
library(tidyr)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

raw_data <- Sys.getenv("rawdatadir")
interim_data <- Sys.getenv("interimdatadir")

drug_list_file <- file.path(interim_data, "teratogenic_drugs.txt")

drugbank_raw <- file.path(raw_data, "predicted_outcomes/Drug_Bank")
drugbank_output <- file.path(interim_data, "predicted_outcomes/Drug_Bank_targets")

#######################################################
# Reading in teratogenic drug list of interest
#######################################################

drug_list <- readLines(drug_list_file)

#######################################################
# Read in full dataset from DrugBank
#######################################################

# Read in DrugBank full dataset as a dataframe
drugbank_dataframe <- dbxml2df(xmlfile = file.path(drugbank_raw, "full database.xml"), version="5.1.3")

# Store drugbank dataframe into a SQLite database
df2SQLite(dbdf=drugbank_dataframe, version="5.1.3")

#######################################################
# Extracting DrugBank ids for drugs of interest
#######################################################

all_db <- queryDB(type = "getAll", db_path="drugbank_5.1.3.db")

ter_db_id <- all_db[grep(paste((str_replace(drug_list, "_", " ")), collapse = "$|^"), 
                         all_db$name, 
                         ignore.case = T), 1:2]

# Folate -> Folic acid
# Valproate -> Valproic acid
ter_db_id <- rbind(ter_db_id, all_db[grep("^Folic acid$|^Valproic acid$", all_db$name, ignore.case = T),1:2 ])

#######################################################
# Extracting All Targets using DrugBank IDs
#######################################################

# All targets
targets_all <- fread(file.path(drugbank_raw, "drugbank_all_target_polypeptide_ids.csv/all.csv"))

targets_all_long <- targets_all %>% 
  mutate(`Drug IDs` = strsplit(`Drug IDs`, ";")) %>%  # split into list
  unnest(`Drug IDs`) %>%                           # expand rows
  mutate(`Drug IDs` = trimws(`Drug IDs`))             # remove spaces

# Extract drug ids of interest and filter for human data only
all_targets <- targets_all_long[grep(paste(ter_db_id$`drugbank-id`, collapse = "|"), 
                                     targets_all_long$`Drug IDs`),] %>% 
  filter(Species == "Humans") %>% 
  select(c("Name", "Gene Name", "Drug IDs"))

#######################################################
# Extracting targets with known pharmacological action using DrugBank IDs
#######################################################

# Targets with known pharmacological action
targets_pharma <- fread(file.path(drugbank_raw, "drugbank_all_target_polypeptide_ids.csv/pharmacologically_active.csv"))

targets_pharma_long <- targets_pharma %>% 
  mutate(`Drug IDs` = strsplit(`Drug IDs`, ";")) %>%  # split into list
  unnest(`Drug IDs`) %>%                           # expand rows
  mutate(`Drug IDs` = trimws(`Drug IDs`))             # remove spaces

# Extract drug ids of interest and filter for human data only
pharma_targets <- targets_pharma_long[grep(paste(ter_db_id$`drugbank-id`, collapse = "|"), 
                                           targets_pharma_long$`Drug IDs`),] %>% 
  filter(Species == "Humans") %>% 
  select(c("Name", "Gene Name", "Drug IDs"))

#######################################################
# Merge
#######################################################

# Label drugs according to pharmacological action
ter_targets <- all_targets %>%
  mutate(`Pharmacolocial Action` = if_else(
    paste0(Name, `Drug IDs`) %in% paste0(pharma_targets$Name, pharma_targets$`Drug IDs`),
    TRUE, FALSE
  ))
colnames(ter_db_id)[1] <- "Drug IDs"

ter_targets <- merge(ter_targets, ter_db_id, by = "Drug IDs")
colnames(ter_targets) <- c("DrugBank_IDs", "Target", "Gene_Name", "Pharmacological_Action", "Drug")

#######################################################
# Catagorising targets as Primary and non Primary
#######################################################

ter_targets$Primary <- ifelse(ter_targets$Pharmacological_Action == TRUE,
                              "Primary",
                              "Alternative")

#######################################################
# Save
#######################################################

fwrite(ter_targets, file.path(drugbank_output, "Drug_Bank_targets.csv"))
