##
# Association data for drug target variants and MR PREG Outcomes
##
# 1. Read in common variants associated with drug target genes - Use primary targets only
# 2. Make sure they are all associated with gene (p value threshold?) 5x10-8
# 3. Extract all association data for variants and all MR PREG outcomes
# 4. Harmonise with variant association data
# 5. MR

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
library(ggplot2)
library(genetics.binaRies)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

raw_data <- Sys.getenv("rawdatadir")
interim_data <- Sys.getenv("interimdatadir")
processed_data <- Sys.getenv("processeddatadir")

targets_file <- file.path(interim_data, "predicted_outcomes/Drug_Bank_targets/Drug_Bank_targets.csv")
variants_file <- file.path(interim_data, "predicted_outcomes/G_P_Map_Variants/all_variants.csv")
mrpreg_file <- file.path(raw_data, "predicted_outcomes/MR_PREG/ma_out_dat.txt")

output_dir <- file.path(processed_data, "MR_PREG_MR_Results")

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

# MR PREG data
mr_preg <- fread(mrpreg_file)

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

#######################################################
# MR Analysis for each drug
#######################################################

for (drug in unique(targets$Drug)){
  
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
  # Extract Outcome Association Data
  #######################################################
  
  outcome_associations <- mr_preg[grep(drug_variants_list, mr_preg$SNP),] %>% 
    as.data.frame()
  
  outcome_formatted <- outcome_associations %>% 
    format_data(
      type = "outcome",
      phenotype_col = "Phenotype",
      snp_col = "SNP",
      beta_col = "beta",
      se_col = "se",
      eaf_col = "eaf",
      effect_allele_col = "effect_allele",
      other_allele_col = "other_allele",
      pval_col = "pval",
      min_pval = 1e-200,
      chr_col = "chr",
      pos_col = "pos", # pos or pos_38?
      ncase_col = "ncase",
      ncontrol_col = "ncontrol",
      samplesize_col = "samplesize",
      info_col = "study"
    )
  outcome_formatted$id.outcome <- outcome_formatted$outcome
  
  #######################################################
  # Harmonise
  #######################################################
  
  harmonised <- harmonise_data(exposure_clumped, outcome_formatted) %>% 
    filter(mr_keep == TRUE)
  
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
  #   - F statistic
  #   = R^2
  #   - Source (info.exposure)
  
  mr_res <- mr_res %>% 
    left_join(.,
              exposure_clumped,
              by = join_by("id.exposure" == "exposure"),
              relationship = "many-to-many") %>% 
    select(id.exposure, id.outcome, method, nsnp, b, se, pval, gene.exposure, 
           SNP, info.exposure, f, r2)
  
  # Adding info on outcome data:
  #   - ncase
  #   - ncontrol
  #   - sample_size
  #   - study
  
  outcome_info <- outcome_formatted %>% 
    select(samplesize.outcome, ncase.outcome, ncontrol.outcome, info.outcome, outcome)
  
  mr_res <- mr_res %>% 
    left_join(.,
              outcome_info,
              by = join_by("id.outcome" == "outcome"),
              relationship = "many-to-many") %>% 
    select(id.exposure, id.outcome, method, nsnp, b, se, pval, SNP, f, r2, 
           gene.exposure, info.exposure, ncase.outcome, 
           ncontrol.outcome, samplesize.outcome, info.outcome)
  
  #######################################################
  # Save
  #######################################################
  
  output_path <- file.path(output_dir, paste(gsub(" ", "_", drug), "MR_PREG_MR_Res.csv", sep = "_"))
  
  fwrite(mr_res, output_path)
}
