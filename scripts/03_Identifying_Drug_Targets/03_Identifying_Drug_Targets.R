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
library(tidyverse)

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
                         ignore.case = T), c("drugbank-id", "name", "targets")]

# Folate -> Folic acid
# Valproate -> Valproic acid
ter_db_id <- rbind(ter_db_id, 
                   all_db[grep("^Folic acid$|^Valproic acid$", 
                               all_db$name, 
                               ignore.case = T), 
                          c("drugbank-id", "name", "targets")])

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
colnames(ter_targets) <- c("DrugBank_IDs", "Target", "Gene_Name", "Pharmacological_Action", "Drug", "Drug_Action")

#######################################################
# Extracting Method of Action of Drug on Target
#######################################################

ter_targets$extracted_text <- NA
ter_targets$Target_x <- NA
ter_targets$Species <- NA
ter_targets$Action <- NA

# All possible actions in DrugBank
actions <- fread("Action, Direction
                 agonist, positive
                 antagonist, negative
                 inhibitor, negative
                 modulator, unknown
                 binder, unknown
                 activator, positive
                 regulator, unknown
                 downregulator, negative
                 negative modulator, negative
                 stimulator, positive
                 inhibitorinducer, unknown
                 partial agonist, positive
                 substrate, unknown
                 upregulator, positive
                 agonistother/unknown, positive
                 other/unknown, unknown
                 intercalation, unknown
                 inducer, positive
                 unknown, unknown
                 ")
action_pattern <- paste(actions$Action, collapse = "|")

# Organisms of interest in DrugBank
organism <- c(
  "Humans",
  ""
)
organism_pattern <- paste(organism, collapse = "|")

# For drug, extract the target of interest, along with the method of action
for (i in 1:nrow(ter_targets)){

  target <- ter_targets$Target[i]

  pattern <- paste0(
    "(",
    target,
    ")(",
    organism_pattern,
    ")(",
    action_pattern,
    ")"
  )

  matches <- regexec(pattern, ter_targets$Drug_Action[i])
  results <- regmatches(ter_targets$Drug_Action[i], matches)

  ter_targets$extracted_text[i] <- sapply(results, function(x) {
    if (length(x) > 0) x[1] else NA_character_
  })

  ter_targets$Target_x[i] <- sapply(results, function(x) {
    if (length(x) > 1) x[2] else NA_character_
  })

  ter_targets$Species[i] <- sapply(results, function(x) {
    if (length(x) > 2) x[3] else NA_character_
  })

  ter_targets$Action[i] <- sapply(results, function(x) {
    if (length(x) > 3) x[4] else NA_character_
  })
  
}

# Take information of interest
ter_targets <- ter_targets %>%
  select("DrugBank_IDs", "Target", "Gene_Name", "Pharmacological_Action", "Drug", "Action")

#######################################################
# Some Targets didn't get picked up
# Manually adding them, bu searching in DrugBank
#######################################################

missing_targets <- fread("Target|Drug|Action
                         Folate receptor alpha|Folic acid|binder
                         B1 bradykinin receptor|Ramipril|activator
                         Retinoic acid receptor alpha|Adapalene|agonist
                         Sodium channel protein type 5 subunit alpha|Topiramate|inhibitor
                         Sodium channel protein type 10 subunit alpha|Topiramate|inhibitor
                         Glutamate receptor ionotropic, kainate 2|Topiramate|antagonist
                         Sodium channel protein type 4 subunit alpha|Topiramate|inhibitor
                         Sodium channel protein type 1 subunit alpha|Topiramate|inhibitor
                         Sodium channel protein type 3 subunit alpha|Topiramate|inhibitor
                         Sodium channel protein type 2 subunit alpha|Topiramate|inhibitor
                         Voltage-dependent L-type calcium channel subunit alpha-1F|Topiramate|antagonist
                         Voltage-dependent L-type calcium channel subunit beta-3|Topiramate|antagonist
                         Voltage-dependent L-type calcium channel subunit beta-4|Topiramate|antagonist
                         Sodium channel protein type 9 subunit alpha|Topiramate|inhibitor
                         Sodium channel protein type 7 subunit alpha|Topiramate|inhibitor
                         Voltage-dependent L-type calcium channel subunit alpha-1S|Topiramate|antagonist
                         Sodium channel protein type 11 subunit alpha|Topiramate|inhibitor
                         Voltage-dependent L-type calcium channel subunit beta-2|Topiramate|antagonist
                         Voltage-dependent R-type calcium channel subunit alpha-1E|Topiramate|antagonist
                         Glutamate receptor ionotropic, kainate 3|Topiramate|antagonist
                         Voltage-dependent L-type calcium channel subunit alpha-1C|Topiramate|antagonist
                         Sodium channel protein type 8 subunit alpha|Topiramate|inhibitor
                         Voltage-dependent L-type calcium channel subunit alpha-1D|Topiramate|antagonist
                         Voltage-dependent L-type calcium channel subunit beta-1|Topiramate|antagonist
                         Glutamate receptor ionotropic, kainate 5|Topiramate|antagonist
                         Glutamate receptor ionotropic, kainate 4|Topiramate|antagonist
                         Peroxisome proliferator-activated receptor gamma|Valproic acid|agonist
                         Histone deacetylase 8|Valproic acid|inhibitor
                         Sodium channel protein type 5 subunit alpha|Valproic acid|inhibitor
                         Sodium channel protein type 3 subunit alpha|Valproic acid|inhibitor
                         Polyamine deacetylase HDAC10|Valproic acid|inhibitor
                         Peroxisome proliferator-activated receptor delta|Valproic acid|agonist
                         Sodium channel protein type 9 subunit alpha|Valproic acid|inhibitor
                         Histone deacetylase 11|Valproic acid|inhibitor
                         Sodium channel regulatory subunit beta-2|Valproic acid|inhibitor
                         Sodium channel regulatory subunit beta-3|Valproic acid|inhibitor
                         Sodium channel regulatory subunit beta-4|Valproic acid|inhibitor
                         Peroxisome proliferator-activated receptor alpha|Valproic acid|agonist
                         Sodium channel protein type 10 subunit alpha|Valproic acid|inhibitor
                         Sodium channel protein type 11 subunit alpha|Valproic acid|inhibitor
                         Histone deacetylase 4|Valproic acid|inhibitor
                         Sodium channel protein type 1 subunit alpha|Valproic acid|inhibitor
                         Histone deacetylase 6|Valproic acid|inhibitor
                         Histone deacetylase 7|Valproic acid|inhibitor
                         Sodium channel protein type 2 subunit alpha|Valproic acid|inhibitor
                         Sodium channel protein type 8 subunit alpha|Valproic acid|inhibitor
                         Sodium channel regulatory subunit beta-1|Valproic acid|inhibitor
                         Sodium channel protein type 7 subunit alpha|Valproic acid|inhibitor
                         Sodium channel protein type 4 subunit alpha|Valproic acid|inhibitor
                         Histone deacetylase 1|Valproic acid|inhibitor
                         Histone deacetylase 5|Valproic acid|inhibitor
                         Histone deacetylase 3|Valproic acid|inhibitor
                         Nuclear receptor subfamily 1 group I member 2|Warfarin|agonist
                         [Pyruvate dehydrogenase (acetyl-transferring)] kinase isozyme 4, mitochondrial|Tretinoin|upregulator
                         Alpha-1-acid glycoprotein 2|Thalidomide|binder
                         Alpha-1-acid glycoprotein 1|Thalidomide|binder
                         B1 bradykinin receptor|Captopril|activator
                         Glycogen synthase kinase-3 beta|Lithium carbonate|unknown
                         Inositol monophosphatase 2|Lithium carbonate|unknown
                         Glutamate receptor 3|Lithium carbonate|unknown
                         Inositol monophosphatase 1|Lithium carbonate|unknown
                         ",
                         sep = "|")


ter_targets <- merge(ter_targets, missing_targets, by = c("Target", "Drug"), all = T)

ter_targets$Action <- ifelse(is.na(ter_targets$Action.x) | ter_targets$Action.x == "",
                               ter_targets$Action.y,
                               ter_targets$Action.x)

ter_targets <- ter_targets %>% 
  select(-c(Action.y, Action.x)) 

#######################################################
# Assigning Method of Action as:
#       Positive (Increases target activity)
#       Negative (Decreases target activity)
#       Neutral (Uncertain of change)
#######################################################

ter_targets <- ter_targets %>% 
  left_join(actions, by = "Action")

#######################################################
# Categorising targets as Primary and non Primary
#######################################################

ter_targets$Primary <- ifelse(ter_targets$Pharmacological_Action == TRUE,
                              "Primary",
                              "Alternative")

#######################################################
# Save
#######################################################

fwrite(ter_targets, file.path(drugbank_output, "Drug_Bank_targets.csv"))
