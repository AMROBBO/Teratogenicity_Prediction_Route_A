##
# Using openGWAS data to test association between drug target gene
# and primary indications of drugs
# 5. Read in common variants associated with drug target genes (rare too to see if there are any?)
# 6. Make sure they are all associated with gene (p value threshold?) 5x10-8
# 7. Extract all association data for variants and all mapped openGWAS outcomes
# 8. Identify largest sample size outcome dataset for each indication
# 9. Harmonise with variant association data
# 10. MR
##

#######################################################
# Load in libraries
#######################################################

pak::pak("MRCIEU/gpmapr")

library(dotenv)
library(dplyr)
library(data.table)
library(tidyr)
library(gpmapr)
library(ieugwasr)
library(TwoSampleMR)
library(jsonlite)
library(ggplot2)
library(genetics.binaRies)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")
processed_data <- Sys.getenv("processeddatadir")

targets_file <- file.path(interim_data, "predicted_outcomes/Drug_Bank_targets/Drug_Bank_targets.csv")
variants_file <- file.path(interim_data, "predicted_outcomes/G_P_Map_Variants/all_variants.csv")
indications_file <- file.path(interim_data, "reported_outcomes/Drug_Bank_outcomes/Drug_Bank_Indications.csv")
mapped_file <- file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/Mapped_outcomes.csv")
output_dir <- file.path(processed_data, "Primary_indications_MR_Results")

# For Clumping
plink <- genetics.binaRies::get_plink_binary()
bfile_path <- Sys.getenv("bfiledir")

#######################################################
# Reading in data files
#######################################################

# Drug Targets
targets <- fread(targets_file) %>% 
  filter(Primary == "Primary")

# Target variants
variants <- fread(variants_file)

# Drug Indications
indications <- fread(indications_file)

# OpenGWAS outcomes
outcome_table <- fread(mapped_file)

#######################################################
# Formatting data files
#######################################################

# Selecting
#   - Common
#   - Cis
#   - Genome-wide significant (p<5x10-8)
#   - Relevant
# SNPs associated with all drug targets

cols <- c("drug_target", "rsid.x", "chr.x", "bp.x", "ea", "oa", "ref_allele",
          "beta", "se", "p", "eaf", "min_p", "variant_type", "trait_name",
          "data_type", "tissue", "source_name")

variants <- variants %>% 
  filter(variant_type == "common") %>% 
  filter(!is.na(beta)) %>% 
  filter(cis_trans == "cis") %>% 
  filter(p < 5e-8) %>% 
  filter(drug_target %in% targets$Gene_Name) %>% 
  select(all_of(cols))

# Selecting
#   - Primary (Prescription)
# Indications associated with drugs

indications <- indications %>% 
  filter(`Approval Level` == "Prescription")
indications[which(indications$Drug == "Valproate"), "Drug"] <- "Valproic_acid" # Normalise some drug naming

# Mapped outcomes - correcting some formatting

outcome_table$openGWAS_outcome <- gsub('""', '"', outcome_table$openGWAS_outcome)

#######################################################
# MR Analysis for each drug
#######################################################

for(drug in unique(targets$Drug)){
  
  message("Processing drug: ", drug)
  #######################################################
  # Identify Exposure (Drug Target)
  #######################################################
  
  drug_targets <- targets %>%
    filter(Drug == drug) %>%
    select(Drug, Target, Gene_Name, Pharmacological_Action)

  drug_targets_list <- paste0("^", 
                              paste(drug_targets$Gene_Name, 
                                    collapse = "$|^"), 
                              "$")
  
  #######################################################
  # Instrument Selection
  #######################################################
  
  # Associated Variants
  drug_variants <- variants[grep(drug_targets_list, variants$drug_target),] %>% 
    as.data.frame()
  
  if (nrow(drug_variants) == 0){
    message("No variants for drug ", drug)
    next
  }
  
  # Format data
  exposure_data <- drug_variants %>% 
    format_data(
      type = "exposure",
      phenotype_col = "trait_name",
      snp_col = "rsid.x",
      beta_col = "beta",
      se_col = "se",
      eaf_col = "eaf",
      effect_allele_col = "ea",
      other_allele_col = "oa",
      pval_col = "p",
      gene_col = "drug_target",
      min_pval = 1e-200,
      chr_col = "chr.x",
      pos_col = "bp.x",
      info_col = "source_name"
    )
  exposure_data$id.exposure = exposure_data$exposure
  
  # Clump
  exposure_clumped <- clump_data(exposure_data,
                                 clump_kb = 10000,
                                 clump_r2 = 0.001,
                                 clump_p1 = 1,
                                 pop = "EUR",
                                 bfile = file.path(bfile_path, "EUR"),
                                 plink_bin = plink)
  
  
  drug_variants_list <- paste0("^",
                               paste(unique(exposure_clumped$SNP),
                                     collapse = "$|^"),
                               "$")
  
  #######################################################
  # Test Instrument Strength
  #######################################################
  
  exposure_clumped <- exposure_clumped %>% 
    mutate(f = ((beta.exposure/se.exposure)^2),
           r2 = 2 * (beta.exposure^2) * eaf.exposure * (1-eaf.exposure)
    ) 
  
  #######################################################
  # Extract Outcome Asssociation Data
  #######################################################
  
  # Drug Indications
  drug_indication <- indications %>% 
    filter(Drug == gsub(" ", "_", drug))
  
  drug_indication_list <- paste0("^",
                                 paste(drug_indication$Indication %>%
                                         gsub(" ", "_", .) %>%
                                         gsub("\\(", "\\\\(", .) %>%
                                         gsub("\\)", "\\\\)", .),
                                       collapse = "$|^"),
                                 "$")
  
  # Corresponding OpenGWAS Outcomes
  drug_outcomes <- outcome_table[grep(gsub(" ", "_", drug_indication_list), outcome_table$indication)] %>% 
    unique() %>% 
    select(indication, openGWAS_outcome, id, ncase, sample_size, category, consortium)
  
  drug_outcomes_list <- drug_outcomes %>% 
    select(id) %>% 
    unique()
  
  # Only continue if there is enough data
  
  if (nrow(exposure_clumped) == 0 | nrow(drug_outcomes) == 0){
    message(drug, " is missing Instruments/Indications")
    next
  }
  
  # Outcome Association data
  
  # Chunking outcomes for outcome extraction
  outcomes <- unique(drug_outcomes$id)
  chunk_size <- 10
  outcome_chunks <- split(
    outcomes, 
    ceiling(seq_along(outcomes) / chunk_size)
  )
  
  results <- vector("list", length(outcome_chunks))
  
  for (i in seq_along(outcome_chunks)){
    message("Processing chunk ", i, "/", length(outcome_chunks))
    
    res <- try(
      extract_outcome_data(
        snps = unique(exposure_clumped$SNP),
        outcomes = outcome_chunks[[i]]
      ),
      silent = TRUE
    )
    
    if (inherits(res, "try-error")){
      message("Chunk ", i, " failed")
      next
    }
    
    results[[i]] <- res
  }
  
  drug_outcome_assoc <- bind_rows(results)

  #######################################################
  # Harmonise
  #######################################################
  
  harmonised <- harmonise_data(exposure_clumped, drug_outcome_assoc)
  
  #######################################################
  # MR
  #######################################################
  
  mr_res <- mr(harmonised)
  
  #######################################################
  # Reattaching Data Info
  #######################################################
  
  # Adding info on exposure data:
  #   - Gene (gene.exposure)
  #   - SNP
  #   - Source (info.exposure)
  
  mr_res <- mr_res %>% 
    left_join(.,
              exposure_clumped,
              by = join_by("id.exposure" == "exposure"),
              relationship = "many-to-many") %>% 
    select(id.exposure, id.outcome, method, nsnp, b, se, pval, gene.exposure, 
           SNP, info.exposure, f, r2)
  
  # Adding info on outcome data:
  #   - Indication
  #   - ncase
  #   - sample_size
  #   - Category (Continuous vs binary)
  #   - Consortium
  
  drug_outcomes <- drug_outcomes %>% 
    rename_with(~ paste0(.x, ".outcome"), -c(indication, openGWAS_outcome, id))
  
  mr_res <- mr_res %>% 
    left_join(.,
              drug_outcomes,
              by = join_by("id.outcome" == "id"),
              relationship = "many-to-many") %>% 
    select(id.exposure, id.outcome, method, nsnp, b, se, pval, SNP, f, r2, 
           gene.exposure, info.exposure, indication, openGWAS_outcome, 
           ncase.outcome, sample_size.outcome, category.outcome, consortium.outcome)

  output_path <- file.path(output_dir, paste(drug, "Primary_Indication_MR_Res.csv", sep = "_"))
  
  fwrite(mr_res, output_path)
}
  