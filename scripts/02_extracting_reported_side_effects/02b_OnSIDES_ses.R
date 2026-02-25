##
# Reading in reported drug side effects from OnSIDES 
# and assigning to teratogenic drugs of interest
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)
library(stringr)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

raw_data <- Sys.getenv("rawdatadir")
interim_data <- Sys.getenv("interimdatadir")

drug_list_file <- file.path(interim_data, "teratogenic_drugs.txt")

onsides_raw <- file.path(raw_data, "reported_outcomes/OnSIDES_data/onsides-v3.1.0/csv")
onsides_output <- file.path(interim_data, "reported_outcomes/OnSIDES_outcomes")

#######################################################
# Reading in teratogenic drug list of interest
#######################################################

drug_list <- readLines(drug_list_file)

#######################################################
# Extracting Ingredients of interest
#######################################################

# Raw ingredient data
onside_ingredient_codes <- fread(file.path(onsides_raw, "vocab_rxnorm_ingredient.csv"))

# Extract ingredients for drugs of interest
onside_ter_ingredient_codes <- onside_ingredient_codes[
  grep(paste((str_replace(drug_list, "_", " ")), collapse = "$|^"), 
       onside_ingredient_codes$rxnorm_name, 
       ignore.case = T),]

# Folate -> is folic acid
onside_ter_ingredient_codes <- rbind(onside_ter_ingredient_codes, 
                                     onside_ingredient_codes[grep("folic acid", onside_ingredient_codes$rxnorm_name),])

#################################
# Imidapril - not in OnSIDES
#################################

colnames(onside_ter_ingredient_codes)[1] <- "ingredient_id"

#######################################################
# Extracting Drugs of interest
#######################################################

# Raw drug id file
onside_product_codes <- fread(file.path(onsides_raw, "vocab_rxnorm_ingredient_to_product.csv"))

# Merge
onside_ter_product_codes <- merge(onside_ter_ingredient_codes, onside_product_codes, by = "ingredient_id")

colnames(onside_ter_product_codes)[2] <- "ingredient_name"

#######################################################
# Add missing drugs 
# (Some Teratogenic drugs of interest are listed as products, not ingredients)
#######################################################

# Raw product file
onside_drugs <- fread(file.path(onsides_raw, "vocab_rxnorm_product.csv"))

# Azilsartan_medoxomil
azil <- onside_drugs[grep("azilsartan medoxomil", onside_drugs$rxnorm_name),]
azil$ingredient_name <- "azilsartan_medoxomil"

# "Candesartan_cilexetil"
cande <- onside_drugs[grep("candesartan cilexetil", onside_drugs$rxnorm_name),]
cande$ingredient_name <- "candesartan_cilexetil"

# Mefenamic_acid 
mefen <- onside_drugs[grep("mefenamic acid", onside_drugs$rxnorm_name),]
mefen$ingredient_name <- "mefenamic_acid"

extra_drugs <- rbind(azil, cande)
extra_drugs <- rbind(extra_drugs, mefen)

colnames(extra_drugs)[1:2] <- c("product_id", "product_name")

# Merge
onside_ter_product_codes <- rbind(onside_ter_product_codes, extra_drugs, fill = T)

#######################################################
# Linking drug ids to their label ids
#######################################################

# Raw label file
onside_label_product <- fread(file.path(onsides_raw, "product_to_rxnorm.csv"))

colnames(onside_label_product)[2] <- "product_id"

# Merge
onside_ter_label_codes <- merge(onside_ter_product_codes, onside_label_product, by = "product_id")

#######################################################
# Linking label ids to their side effects MedDRA ids
#######################################################

# Raw side effect id file
onside_se_codes <- fread(file.path(onsides_raw, "product_adverse_effect.csv"))

colnames(onside_se_codes)[1] <- "label_id"

# Merge
onside_ter_ses <- merge(onside_ter_label_codes, onside_se_codes, by = "label_id")

#######################################################
# Linking side effect MedDRA ids to names
#######################################################

# Raw side effect name file
onside_se_names <- fread(file.path(onsides_raw, "vocab_meddra_adverse_effect.csv"))

colnames(onside_se_names)[1] <- "effect_meddra_id"

# Merge
onside_ter_outcomes <- merge(onside_ter_ses, onside_se_names, by = "effect_meddra_id")

#######################################################
# Save
#######################################################

fwrite(onside_ter_outcomes, file.path(onsides_output, "OnSIDES_reported_outcomes.csv"))

## Would be good to filter these further to look at side effects with some genetic insight
## See - 10.1371/journal.pgen.1011638  for method on this
