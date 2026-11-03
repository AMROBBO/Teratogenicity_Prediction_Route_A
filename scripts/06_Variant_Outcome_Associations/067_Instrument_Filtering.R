##
# Filtering for drugs with instruments that show a significant causal effect on drug indications
##
#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(data.table)
library(tidyverse)
library(jsonlite)
library(dplyr)
library(tidyr)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_dir <- Sys.getenv("interimdatadir")
processed_dir <- Sys.getenv("processeddatadir")
plots_dir <- Sys.getenv("plotsdir")

chosen_openGWAS_dir <- file.path(interim_dir, "predicted_outcomes/Primary_Indication_Mapping/Open_GWAS_Chosen_Outcomes")
targets_file <- file.path(interim_dir, "predicted_outcomes/Drug_Bank_targets/Drug_Bank_targets.csv")
harmonised_dir <- file.path(interim_dir, "predicted_outcomes/Primary_Indication_Mapping/MR_output")
MR_res_dir <- file.path(processed_dir, "Primary_indications_MR_Results")

plot_out_dir <- file.path(plots_dir, "Primary_Indication_Plots")
snp_out_dir <- file.path(interim_dir, "predicted_outcomes/Primary_Indication_Mapping/Primary_Indication_Significant_SNPs")

#######################################################
# Reading in data
#######################################################

# All drugs
drugs <- readLines(file.path(interim_dir, "teratogenic_drugs.txt"))

# Target of each drug
drug_targets <- fread(targets_file) %>% 
  filter(Primary == "Primary")

# Chosen Mapped Outcome data
chosen_openGWAS_files <- list.files(chosen_openGWAS_dir, full.names = TRUE)

# Harmonised Exposure-Outcome data (Orientated to direction of drug action)
harmonised_files <- list.files(harmonised_dir, full.names = TRUE)

# MR results
MR_res_files <- list.files(MR_res_dir, full.names = TRUE)

# Normalise terminology
drug_targets[which(drug_targets$Drug == "Folic acid"), "Drug"] <- "Folate"
drugs[which(drugs == "Valproate")] <- "Valproic_acid"

#######################################################
# Create Functions
#######################################################

extract_targets <- function(drug){
  targets <- drug_targets %>%
    filter(Drug == gsub("_", " ", drug)) %>%
    select(c("Target", "Gene_Name", "Action", "Direction")) %>%
    unique()
  
  target_list <- paste0("^", paste(targets$Gene_Name, collapse = "$|^"), "$")
  
  if (nrow(targets) == 0){
    message(drug, " has no primary targets")
    return(NULL)
  } else{
    return(list(targets = targets, target_list = target_list))
  }
}

extract_openGWAS_outcomes <- function(drug){
  
  studies <- grep(paste0("/", drug, "_"), chosen_openGWAS_files, value = T)
  
  if (length(studies) == 0){
    message(drug, " has no appropriate openGWAS outcomes")
    return(NULL)
  } else{
    
    studies <- studies %>% 
      fread()
    
    outcome_ids <- paste0("^", paste(unique(studies$opengwas_id), collapse = "$|^"), "$")
    
    return(list(openGWAS_studies = studies, outcome_ids = outcome_ids))
  }
}

extract_MR_result <- function(drug, outcome_ids){
  mr_res_all <- grep(paste0("/", drug, "_"), MR_res_files, value = T)
  
  if(length(mr_res_all) == 0){
    message(drug, " has no MR results")
    return(NULL)
  } else{
    
    mr_res_all <- mr_res_all %>%
      fread()
    
    mr_res <- mr_res_all[grep(outcome_ids, mr_res_all$id.outcome),] %>%
      as.data.frame()
  
  return(mr_res)
  }
}

create_plot_df <- function(mr_res){
  
  # Adjust p values for multiple testing (per outcome)
  mr_res <- mr_res %>%
    group_by(openGWAS_outcome) %>%
    mutate(pval_adj = p.adjust(pval, method = "BH")
    ) %>%
    ungroup()
  
  # Align direction of effect to the direction of drug effect on the target
  for(g in unique(mr_res$gene.exposure)){
    drug_direction <- mr_res[which(mr_res$gene.exposure == g), "Direction"] %>%
      unique()
    if(drug_direction == "negative"){
      mr_res[which(mr_res$gene.exposure == g), "b"] <- -1 * mr_res[which(mr_res$gene.exposure == g), "b"]
    }
  }
  
  # Converting to Odds ratio (as are all binary outcomes) and adding CIs
  plot_df <- mr_res %>%
    mutate(
      OR = exp(b),
      lower = exp(b - 1.96 * se),
      upper = exp(b + 1.96 * se),
      outcome = factor(openGWAS_outcome),
      sig = pval_adj < 0.05
    )
  
  # Creating a gene(SNP) column
  plot_df$gene.snp <- paste0(plot_df$gene.exposure, "(", plot_df$SNP, ")")
  
  return(plot_df)
}

create_plot <- function(plot_df){
  
  # Assigning a unique colour to each gene(SNP)
  snps <- unique(plot_df$gene.snp)
  
  gene_cols <- setNames(
    scales::hue_pal()(length(snps)),
    snps
  )
  
  # Only fill significant points
  plot_df$fill_col <- ifelse(
    plot_df$sig,
    gene_cols[plot_df$gene.snp],
    "white"
  )
  
  # Individual plot for each target gene
  genes <- unique(plot_df$gene.exposure)
  
  pdf(file.path(plot_out_dir, paste(drug, "MR_Res.pdf", sep = "_")))
  
  for (g in genes){
    
    df <- subset(plot_df, gene.exposure == g)
    
    # If the drug is an inhibitor of the target, the plot will show decrease in exposure
    effect_direction <- df$Direction %>%
      unique()
    
    if (effect_direction == "negative"){
      x_label <- "Odds Ratio per 1 SD decrease in QTL"
    } else {
      x_label <- "Odds Ratio per 1 SD increase in QTL"
    }
    
    # Plot
    p <- ggplot(
      df,
      aes(
        y = id.exposure,
        x = OR,
        xmin = lower,
        xmax = upper
        )
      ) +
      
      geom_errorbarh(
        aes(colour = gene.snp), 
        height = 0.2
      ) +
      
      geom_point(
        aes(colour = gene.snp),
        shape = 21,
        fill = df$fill_col,
        size = 2
      ) +
      
      geom_vline(
        xintercept = 1,
        linetype = 2
      ) +
      
      scale_x_log10() +
      
      facet_wrap(
        ~openGWAS_outcome,
        scales = "free_x",
        ncol = 2
      ) +
      
      scale_colour_manual(
        values = gene_cols
      ) +
      
      theme_bw() +
      
      theme(
        axis.text.y = element_text(size = 8)
      ) +
      
      labs(
        title = g,
        x = x_label,
        y = "Drug Target Measure",
        colour = "Drug Target Measure"
      )
    
    print(p)
  }
  dev.off()
}

extract_sig_SNP <- function(drug, openGWAS_studies){
  
  # Extract harmonised exposure-outcome data
  harmonised_data <- grep(drug, harmonised_files, value = T) %>%
    fread() %>%
    filter(id.outcome %in% unique(openGWAS_studies$opengwas_id)) %>%
    left_join(., openGWAS_studies, by = c("id.outcome" = "opengwas_id"), relationship = "many-to-many") %>%
    select(exposure, SNP, beta.exposure, beta.outcome, pval.exposure, pval.outcome, indication, originalname.outcome, gene.exposure, f, r2, Action, Direction)
  
  # Adjust pvalue for multiple testing (per outcome)
  harmonised_data <- harmonised_data %>%
    group_by(originalname.outcome) %>%
    mutate(
      pval.outcome_adj = p.adjust(pval.outcome, method = "BH")
    ) %>%
    ungroup()
    
  # Filtering for those with a significant effect on outcome
  harmonised_data <- harmonised_data %>%
    mutate(
      sig = pval.outcome_adj < 0.05
    ) %>%
    filter(sig == TRUE)
  
  # Assigning risk due to drug target effect
  harmonised_data$Risk <- ifelse(
    harmonised_data$beta.outcome < 0,
    "Decreased Risk",
    "Increased Risk"
  )
  
  harmonised_data <- harmonised_data[order(harmonised_data$originalname.outcome, harmonised_data$SNP, harmonised_data$Risk),]
  
  # Save
  
  if(nrow(harmonised_data) != 0) {
    print(harmonised_data)
    
    output_file <- file.path(snp_out_dir, paste(drug, "significant_snps.csv", sep = "_"))
    fwrite(harmonised_data, output_file)
  } else {
    cat("No significant SNP-Outcome associations found for", drug, "after multiple testing correction.")
  }
}

#######################################################
# Run through each drug
#######################################################

for(drug in drugs){
  
  message("\n=== ", drug, " ===")
  
  # Drug Targets
  targets <- extract_targets(drug)
  
  if (is.null(targets)) 
    next

  # Chosen OpenGWAS outcomes
  openGWAS_outcomes <- extract_openGWAS_outcomes(drug)
  
  if (is.null(openGWAS_outcomes)) 
    next

  # MR Results
  MR_res <- extract_MR_result(drug, openGWAS_outcomes$outcome_ids)
  
  if (is.null(MR_res)) 
    next

  # Create Data Frame to Plot
  plot_df <- create_plot_df(MR_res)
  
  # Plot
  create_plot(plot_df)
  
  # Extract and save all SNPs showing a significant effect on the outcomes
  extract_sig_SNP(drug, openGWAS_outcomes$openGWAS_studies)

}
