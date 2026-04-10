##
# Using Genotype-Phenotype Map to identify colocalised traits and variants 
# associated with drug target genes
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)
library(tidyr)
library(gpmapr)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")

targets_file <- file.path(interim_data, "predicted_outcomes/Drug_Bank_targets/Drug_Bank_targets.csv")
output_dir <- file.path(interim_data, "predicted_outcomes")

#######################################################
# Reading in Drug Target Genes
#######################################################

target_genes <- (fread(targets_file))$Gene_Name %>% 
  unique()

#######################################################
# Filter target genes for those available in gpmap
#######################################################

genes_avail <- all_genes()

target_genes <- target_genes[target_genes %in% genes_avail$gene]

#######################################################
# Searching Drug Target Genes Information
#######################################################

all_traits <- data.frame()
all_associations <- data.frame()

for (i in target_genes){
  
  drug_target <- i
  
  gene_output <- gene(i, include_associations = T, include_trans = F)
  
# Set to not include trans associations

#######################################################
# All Variants associated with the target
#######################################################

  variants <- gene_output$variants

  # Q: What does symbol mean here? Why is it not TNF? 
  # - This is position based, they are the most proximal genes
  
#######################################################
# Rare Variants associated with the target
#######################################################
 
  variant_betas <- gene_output$rare_results
  
  variant_betas$variant_type <- "rare"
  
  # Adding columns here to match those in common variant data frame
  variant_betas$coloc_group_id <- NA
  variant_betas$h4_connectedness <- NA
  variant_betas$h3_connectedness <- NA
  
#######################################################
# Common Variants associated with the target
#######################################################
  
  coloc_groups <- gene_output$coloc_groups
  
  if (!is.null(coloc_groups)) { 
    
    common_betas <- coloc_groups %>% 
      filter(gene == drug_target)
    
    if (nrow(common_betas) > 0){
    
      common_betas$variant_type <- "common"
    
      # What does it mean that gene is NA here?
    
      # Combine Rare and common betas
    
      common_columns <- colnames(variant_betas)[(colnames(variant_betas) %in% colnames(common_betas))]
    
      variant_betas <- variant_betas %>% 
        full_join(common_betas, by = common_columns)
    }
      
#######################################################
# Traits colocalising with gene
#######################################################

    available_data_types <- c("protein",
                            "phenotype",
                            "cell_trait",
                            "gene_expression",
                            "splice_variant",
                            "plasma_protein",
                            "methylation",
                            "summary")

    coloc_traits <- coloc_groups %>% 
      filter(data_type == "Phenotype")
    
    if(nrow(coloc_traits) > 0){
      
      coloc_traits$drug_target <- drug_target

      all_traits <- rbind(all_traits, coloc_traits)
    }
    
  }

#######################################################
# Non-colocalising variants associated with gene
#######################################################

# Betas for other variants

  other_variants <- gene_output$associations[!gene_output$associations$snp_id %in% variant_betas$snp_id,]

  other_variants$variant_type <- "other"

# Combine other betas with rare and common

  common_columns <- colnames(variant_betas)[colnames(variant_betas) %in% colnames(other_variants)]

  variant_betas <- variant_betas %>% 
    full_join(other_variants, by = common_columns)

#######################################################
# Assigning betas to variants
#######################################################
  
  colnames(variants)[1] <- "snp_id"

  variants_assoc <- variants %>% 
    full_join(variant_betas, by = "snp_id")

#######################################################
# Cleaning up columns
#######################################################
  
  variants_assoc$gene_id <- ifelse(is.na(variants_assoc$gene_id.x),
                                   variants_assoc$gene_id.y,
                                   variants_assoc$gene_id.x)

  variants_assoc$drug_target <- drug_target
  
  to_keep <- c("drug_target", "rsid.x", "snp_id", "snp", "chr.x", "bp.x", "ea", "oa", "ref_allele", "beta", 
               "se", "p", "eaf", "min_p", "imputed", "variant_type", "display_snp.x",
               "flipped", "gene.x", "feature_type", "consequence", "cdna_position",
               "cds_position", "protein_position", "amino_acids", "codons", "impact", 
               "symbol", "biotype", "strand", "canonical", "all_af", "eur_af", "eas_af", "amr_af", 
               "afr_af", "sas_af", "distinct_trait_categories", "distinct_protein_coding_genes", 
               "coloc_group_id", "study_id", "study_extraction_id", "ld_block_id", "h4_connectedness",
               "h3_connectedness", "cis_trans", "ld_block", "trait_id", "trait_name",
               "trait_category", "data_type", "tissue", "cell_type", "source_id", "source_name", 
               "source_url", "rare_result_group_id", 
               "situated_gene_id", "situated_gene", "gene_id")
  
  variants_assoc <- variants_assoc %>% 
    select(all_of(to_keep))

  all_associations <- rbind(all_associations, variants_assoc)
}

#######################################################
# Extracting unique variants
#######################################################

unique_variants <- all_associations %>% 
  select(drug_target, rsid.x, beta, se, p, eaf, variant_type) %>% 
  unique()

#######################################################
# Extracting unique traits
#######################################################

unique_traits <- all_traits %>% 
  select("rsid", "trait_name", "trait_category", "beta", "se", "p", "eaf", "drug_target") %>% 
  unique()

#######################################################
# Save
#######################################################

fwrite(all_associations, file.path(output_dir, "G_P_Map_Variants", "all_variants.csv"))
fwrite(unique_variants, file.path(output_dir, "G_P_Map_Variants", "unique_variants.csv"))

fwrite(all_traits, file.path(output_dir, "G_P_Map_Outcomes", "all_coloc_traits.csv"))
fwrite(unique_traits, file.path(output_dir, "G_P_Map_Outcomes", "unique_coloc_traits.csv"))

# Q: Difference between id and snp_id and study_id?
# - associations$snp_id = variants$id = coloc_groups$snp_id = rare_variants$snp_id = study_extractions$snp_id
# - study_extraction$id = coloc_group$study_extraction_id

# Q: Only seems to be 1 study per rsID, where has the rest of the data in associations come from?

# Q: For eg SCN4A, none of the rows in coloc_groups correspond to the gene? Is it one of the NAs?
# The gene for the variants in coloc_groups in variants output, have gene name that doesn't match the metadata for the gene?
# These variants are situated within the gene, but the association values don't correspond to it? - Should I include these? I have excluded for now

# Do I need to clump the results?

# Filter for canonical = YES?
# Filter for eaf?
