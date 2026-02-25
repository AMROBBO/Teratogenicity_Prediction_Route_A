##
# Extracting phenotypic outcomes due to genetic variation of the genes coding for the 
# drug targets
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

drug_targets_file <- file.path(interim_data, "predicted_outcomes/Drug_Bank_targets/Drug_Bank_targets.csv")

omim_raw <- file.path(raw_data, "predicted_outcomes/OMIM_data")
omim_output <- file.path(interim_data, "predicted_outcomes/OMIM_outcomes")

#######################################################
# Reading in teratogenic drug targets of interest
#######################################################

drug_targets <- fread(drug_targets_file)

#######################################################
# Read in OMIM data
#######################################################

# OMIM Variant tables, manually downloaded from OMIM
OMIM <- list.files(omim_raw, full.names = T)

#######################################################
# Extracting outcomes for primary target perturbation
#######################################################

# Filter for primary targets
primary_targets <- drug_targets %>% 
  filter(Primary == "Primary")

# Extract from OMIM data
primary_OMIM <- OMIM[grep(paste(primary_targets$Gene_Name, collapse = "|"), 
                          OMIM, 
                          ignore.case = T)]

# Merge to one file
targ_outcomes <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(targ_outcomes) <- c("Gene", "Outcome")

for (f in primary_OMIM){
  
  # Read in OMIM data
  OMIM <- read.delim(f, skip = 8, header = TRUE, sep = "\t", na.strings = c("", "NA"))
  
  # Phenotypic outcomes of target genetic variation
  phen <- OMIM$Phenotype
  pheno <- unlist(strsplit(phen, split = ";;")) # May be necessary for some listed phenotypes
  
  # Target name
  targ <- unlist(strsplit(f, split = "/"))[length(unlist(strsplit(f, split = "/")))] %>%
    gsub(pattern = ".tsv$", replacement = "")
  targ <- unlist(strsplit(targ, split = "-"))
  targ <- targ[length(targ)]
  
  # Assigning phenotype to target name
  outcomes <- data.table(Gene = targ, Outcome = pheno)
  targ_outcomes <- rbind(targ_outcomes, outcomes)
}

# Remove duplicates
targ_outcomes <- unique(targ_outcomes)

#######################################################
# Save
#######################################################

fwrite(targ_outcomes, file.path(omim_output, "OMIM_outcomes_pre_processed.csv"))

###
# I manually went through these outcomes, creating replacements more appropriate for ontology mapping, including removing:
# - Descriptions of outcome onset, eg. Susceptibility to, Progression of...
# - Genetic subtypes of outcome, eg. Numbers, Familial, Digenic
# - Lacking information, eg, Removed from database, variant of unknown significance
# - Molecular changes, that are unlikely to be reported as drug side effects, eg. ORM1*F1, Carbonic anhydrase II variant
###
