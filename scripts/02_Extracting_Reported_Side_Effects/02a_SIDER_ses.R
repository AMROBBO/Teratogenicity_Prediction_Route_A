##
# Reading in reported drug side effects from SIDER 
# and assigning to teratogenic drugs of interest
##

#######################################################
# Load in libraries
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

drug_list_file <- file.path(interim_data, "teratogenic_drugs.txt")

sider_raw <- file.path(raw_data, "reported_outcomes/SIDER_data")
sider_output <- file.path(interim_data, "reported_outcomes/SIDER_outcomes")

#######################################################
# Reading in teratogenic drug list of interest
#######################################################

drug_list <- readLines(drug_list_file)

#######################################################
# SIDER drug names and corresponding codes
#######################################################

SIDER_drugs <- fread(file.path(sider_raw, "drug_names.tsv"), header = F)

SIDER_header <- c("STITCH_compound_ids_flat", "Drug_name")
colnames(SIDER_drugs) <- SIDER_header 

#######################################################
# SIDER reported side effects and frequencies
#######################################################

SIDER_ses <- fread(file.path(sider_raw, "meddra_freq.tsv"), header = F)

SIDER_ses_header <- c("STITCH_compound_ids_flat", "STITCH_compound_ids_stereo",
                      "UMLS_concept_id_label", "placebo", "frequency", "frequency_lower",
                      "frequency_upper", "MedDRA_concept_type", "UMLS_concept_id",
                      "side effect name")
colnames(SIDER_ses) <- SIDER_ses_header

#######################################################
# Assigning SIDER reported side effects to teratogenic drugs
#######################################################

# Extracting SIDER codes for teratogenic drugs of interest
drug_list_codes <- SIDER_drugs[grep(paste(drug_list, collapse = "$|^"), 
                                    SIDER_drugs$Drug_name, 
                                    ignore.case = T),]

# Due to naming in drug_names.tsv, some of the drugs have been duplicated/missed
# Here, I manually correct this:

# Adding missing drugs
drug_list_codes <- drug_list_codes %>% 
  add_row(STITCH_compound_ids_flat = "CID111238823", Drug_name = "azilsartan_medoxomil") %>%
  add_row(STITCH_compound_ids_flat = "CID100002540", Drug_name = "candesartan_cilexetil") %>%
  add_row(STITCH_compound_ids_flat = "CID100003222", Drug_name = "enalapril") %>%
  add_row(STITCH_compound_ids_flat = "CID100005538", Drug_name = "isotretinion") %>%
  add_row(STITCH_compound_ids_flat = "CID100005538", Drug_name = "tretinion") %>%
  add_row(STITCH_compound_ids_flat = "CID100011125", Drug_name = "lithium_carbonate") %>%
  add_row(STITCH_compound_ids_flat = "CID100004044", Drug_name = "mefenamic_acid") %>%
  add_row(STITCH_compound_ids_flat = "CID100004271", Drug_name = "mycophenolate_mofetil")

#################################
#Imidapril - Not in SIDER
#Isotretinion - Retinoic Acid in SIDER
#Tretinoin - Retinoic acid in SIDER
#Enalapril - CAS 76095-16-4 in SIDER
#################################

# Removing doubles
# Olmesartan - CID100158781
drug_list_codes <- filter(drug_list_codes, STITCH_compound_ids_flat != "CID100130881")

# Perindopril - CID100060184
drug_list_codes <- filter(drug_list_codes, STITCH_compound_ids_flat != "CID100060183")

# Merge datasets
reported_outcomes <- merge(drug_list_codes, SIDER_ses, by = "STITCH_compound_ids_flat")

#######################################################
# Save
#######################################################

write.csv(reported_outcomes, file.path(sider_output, "SIDER_reported_outcomes.csv"), row.names = F)
