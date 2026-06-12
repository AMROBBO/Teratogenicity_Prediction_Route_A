##
# Using openGWAS data to test association between drug target gene
# and primary indications of drugs
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
library(ggplot2)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")

exposure_file <- file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/MR_input/exposure_associations.csv")
outcome_file <- file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/MR_input/outcome_associations.csv")
targets_file <- file.path(interim_data, "predicted_outcomes/Drug_Bank_targets/Drug_Bank_targets.csv")
indications_file <- file.path(interim_data, "reported_outcomes/Drug_Bank_outcomes/Drug_Bank_Indications.csv")
output_dir <- file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/MR_output")

#######################################################
# Reading in data
#######################################################

# Target of each drug
drug_targets <- fread(targets_file) %>% 
  filter(Primary == "Primary")

# Indication of each drug
drug_indications <- fread(indications_file) %>% 
  filter(`Approval Level` == "Prescription")

# Normalise some drug naming
drug_indications[which(drug_indications$Drug == "Valproate"), "Drug"] <- "Valproic acid"

# Drug target assocation data
exposure_data <- fread(exposure_file)

# Outcome association data
outcome_data <- fread(outcome_file) %>% 
  select("indication", "openGWAS_outcome", "id", "chr", "position", "rsid", 
         "ea", "nea", "eaf", "beta", "se", "p", "n")

#######################################################
# 9. Harmonise with variant association data
#######################################################

# For each drug

drug <- unique(drug_targets$Drug)[7]

# What are the indications
indications <- drug_indications %>% 
  filter(Drug == drug)

indication_list <- paste0("^", paste(indications$Indication, collapse = "$|^"), "$")

opengwas_outcomes <- outcome_data
opengwas_outcomes$indication <- gsub("_", " ", opengwas_outcomes$indication)
opengwas_outcomes <- opengwas_outcomes[grep(indication_list, opengwas_outcomes$indication),] %>% 
  select(c("indication","openGWAS_outcome")) %>% 
  unique()

# What are the targets
targets <- drug_targets %>% 
  filter(Drug == drug) %>% 
  select(c("Target", "Gene_Name")) %>% 
  unique()

target_list <- paste0("^", paste(targets$Gene_Name, collapse = "$|^"), "$")

exposure_SNPs <- exposure_data[grep(target_list, exposure_data$drug_target)] %>% 
  as.data.frame()
  
# Extract all associated SNPs

SNP_list <- paste0("^", paste(unique(exposure_SNPs$rsid.x), collapse = "$|^"), "$")
  
# Extract association data for all SNPs and all available outcomes
  
outcome_SNPs <- outcome_data[grep(SNP_list, outcome_data$rsid),] %>% 
  as.data.frame()
  
  # Format data
  
exposure_formatted <- exposure_SNPs %>% 
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
    pos_col = "bp.x"
  )
  
outcome_formatted <- outcome_SNPs %>% 
   format_data(
    type = "outcome",
    phenotype_col = "openGWAS_outcome",
    snp_col = "rsid",
    beta_col = "beta",
    se_col = "se",
    eaf_col = "eaf",
    effect_allele_col = "ea",
    other_allele_col = "nea",
    pval_col = "p",
    samplesize_col = "n",
    id_col = "id",
    min_pval = 1e-200,
    info_col = "indication",
    chr_col = "chr",
    pos_col = "position"
  )
  
# Harmonise
  
harmonised <- harmonise_data(exposure_formatted, outcome_formatted)

#######################################################
# 10. MR
#######################################################

mr_res <- mr(harmonised)

mr_res$indication <- ifelse(
  mr_res$outcome %in% opengwas_outcomes$openGWAS_outcome,
  TRUE,
  FALSE
)

#######################################################
# 11. Plotting
#######################################################

# Bubble plot

ggplot(mr_res,
       aes(x = outcome,
           y = exposure,
           size = -log10(pval),
           colour = indication)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_size_continuous(name = "-log10(P)") +
  labs(
    x = "Outcome",
    y = "Exposure",
    colour = "Drug Indication"
  )


# Heatmap - not very good for significance

ggplot(mr_res,
       aes(x = outcome,
           y = exposure,
           fill = b)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
# Forest plot - too busy

ggplot(mr_res,
       aes(x = b,
           y = exposure)) +
  geom_point() +
  geom_errorbarh(
    aes(
      xmin = b - 1.96 * se,
      xmax = b + 1.96 * se
    )
  ) +
  facet_wrap(~ outcome)

# Make more obvious what is significant and what isnt
# Repeat for each drug and save