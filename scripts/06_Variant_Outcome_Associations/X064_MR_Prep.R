##
# Using openGWAS data to test association between drug target gene
# and primary indications of drugs
# 5. Read in common variants associated with drug target genes (rare too to see if there are any?)
# 6. Make sure they are all associated with gene (p value threshold?) 5x10-8
# 7. Extract all association data for variants in all outcomes in openGWAS (PheWAS)
# 8. Match to mapped openGWAS outcomes
# 9. Harmonise with variant association data
# 10. MR
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)
library(tidyr)
library(gpmapr)
library(ieugwasr)
library(TwoSampleMR)
library(jsonlite)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")

indications_file <- file.path(interim_data, "reported_outcomes/Drug_Bank_outcomes/Drug_Bank_Indications.csv")
variants_file <- file.path(interim_data, "predicted_outcomes/G_P_Map_Variants/all_variants.csv")
targets_file <- file.path(interim_data, "predicted_outcomes/Drug_Bank_targets/Drug_Bank_targets.csv")
mapped_file <- file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/Qwen_output/Qwen_output_combined.json")
opengwas_correction_file <- file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/OpenGWAS_mispellings.csv")
output_dir <- file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/MR_input")

#######################################################
# 5. Read in common variants associated with drug target genes
# 6. Make sure they are all associated with gene (p value threshold?) 5x10-8
#######################################################

targets <- fread(targets_file) %>% 
  filter(Primary == "Primary")

cols <- c("drug_target", "rsid.x", "chr.x", "bp.x", "ea", "oa", "ref_allele",
          "beta", "se", "p", "eaf", "min_p", "variant_type", "trait_name",
          "data_type", "tissue")

# Is eaf european?

variants <- fread(variants_file) %>% 
  filter(variant_type == "common") %>% 
  filter(!is.na(beta)) %>% 
  filter(cis_trans == "cis") %>% 
  filter(p < 5e-8) %>% 
  filter(drug_target %in% targets$Gene_Name) %>% 
  select(all_of(cols))

#######################################################
# 7. Extract all association data for variants in all outcomes in openGWAS (PheWAS)
#######################################################

variants_list <- variants$rsid.x

opengwas_assoc <- phewas(variants_list, pval = 0.01) # This is the maximum p value allowed, still ok for MR?

# Filter for European data

opengwas_studies <-TwoSampleMR::available_outcomes() %>% 
  filter(population == "European")

opengwas_assoc <- opengwas_assoc %>% 
  filter(id %in% opengwas_studies$id)

#######################################################
# 8. Match to mapped openGWAS outcomes
#######################################################
#######################################################
# Converting mapped json file -> csv
#######################################################

# DrugBank Indications mapped to openGWAS outcomes
mapped_outcomes <- fromJSON(mapped_file, simplifyVector = F)

# Converting mapped outcomes to csv format
rows <- list()

# For each indication
for(outcome_name in names(mapped_outcomes)){
  
  # Extract matches
  matches <- mapped_outcomes[[outcome_name]]$matches
  
  # Skip if there are no matches
  if (length(matches$openGWAS_outcome) == 0) {
    next
  }
  
  # Create data frame for this indication
  df <- data.frame(
    indication = outcome_name,
    openGWAS_outcome = unlist(matches$openGWAS_outcome),
    cosine_similarity = sapply(
      matches$cosine_similarity,
      function(x) if (is.null(x)) NA_real_ else as.numeric(x)
    ),
    similarity_score = as.numeric(unlist(matches$similarity_score)),
    match_decision = as.logical(unlist(matches$match_decision)),
    brief_rationale = unlist(matches$brief_rationale),
    stringsAsFactors = FALSE
  )
  
  # Store it in a list
  rows[[outcome_name]] <- df
}
outcome_table <- bind_rows(rows)

#######################################################
# Correcting some errors introduced by LLM - This step is very manual and will have to be curated for each rerun of LLM step
#######################################################

# This file was manually corrected by finding the original openGWAS terms in available_outcomes()
opengwas_corrections <- fread(opengwas_correction_file) 

# Correcting some terms
opengwas_corrections$mapped_openGWAS <- gsub('""', '"', opengwas_corrections$mapped_openGWAS)
opengwas_corrections$original_openGWAS <- gsub('""', '"', opengwas_corrections$original_openGWAS)

outcome_table$openGWAS_outcome <- ifelse(
  grepl("^c\\(", outcome_table$openGWAS_outcome),
  sub("^c\\((.*)\\)$", "\\1", outcome_table$openGWAS_outcome),
  outcome_table$openGWAS_outcome
)

outcome_table$openGWAS_outcome <- gsub("^c\\(", "", outcome_table$openGWAS_outcome)
outcome_table$openGWAS_outcome <- gsub('""', '"', outcome_table$openGWAS_outcome)

# Changing incorrect openGWAS terms in mapped data
outcome_table <- left_join(outcome_table, 
                           opengwas_corrections, 
                           by = join_by("openGWAS_outcome" == "mapped_openGWAS"), 
                           relationship = "many-to-many")

outcome_table$openGWAS_outcome <- ifelse(!is.na(outcome_table$original_openGWAS),
                                         outcome_table$original_openGWAS,
                                         outcome_table$openGWAS_outcome)

# To check that for those terms not in the association data is only becuase there isn't association data in PheWAS for specified SNPs (Not in phewas)
print(outcome_table[which(!outcome_table$openGWAS_outcome %in% opengwas_assoc$trait),])

# Save
write.csv(outcome_table[,1:6], file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/Qwen_output_combined.csv"), row.names = FALSE)

#######################################################
# Mapping Indication -> openGWAS term -> openGWAS association data
#######################################################

mapped_assocs <- left_join(outcome_table, 
                           opengwas_assoc, 
                           by = join_by("openGWAS_outcome" == "trait"), 
                           relationship = "many-to-many")

#######################################################
# Save
#######################################################

fwrite(mapped_assocs, file.path(output_dir, "outcome_associations.csv"))
fwrite(variants, file.path(output_dir, "exposure_associations.csv"))
