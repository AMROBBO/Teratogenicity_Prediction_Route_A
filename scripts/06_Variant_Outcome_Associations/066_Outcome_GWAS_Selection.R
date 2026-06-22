##
# Using openGWAS data to test association between drug target gene
# and primary indications of drugs
# 11. Plotting
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")
processed_data <- Sys.getenv("processeddatadir")
results_data <- Sys.getenv("resultsdir")

indications_file <- file.path(interim_data, "reported_outcomes/Drug_Bank_outcomes/Drug_Bank_Indications.csv")
MR_res_dir <- file.path(processed_data, "Primary_indications_MR_Results")
output_dir <- file.path(results_data, "")

#######################################################
# Reading in data
#######################################################

indications <- fread(indications_file)

MR_results <- list.files(MR_res_dir, full.names = T)

#######################################################
# Exploring openGWAS outcome datasets - Captopril
#######################################################

# Specify drug
drug <- "Captopril"

# Reading in MR results
drug_res <- MR_results[grep(drug, MR_results)] %>% 
  fread()

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[4]

ind_res <- drug_res %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Hypertension", "Heart failure", "Diabetic nephropathy", "Heart failure"),
  opengwas_id = c("ieu-b-5144", "ebi-a-GCST009541", "ebi-a-GCST90018832", "ebi-a-GCST009541"),
  sample_size = c("462826", "977323", "452280", "977323"),
  ncases = c("133680", "47309", "1032", "47309"),
  outcome_type = c("Binary", "Binary", "Binary", "Binary")
)

#######################################################
# Exploring openGWAS outcome datasets - Methotrexate
#######################################################

drug <- "Methotrexate"

# Reading in MR results
drug_res <- MR_results[grep(drug, MR_results)] %>% 
  fread()

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[8]

ind_res <- drug_res %>% 
  filter(indication == gsub(" ", "_", ind))

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Psoriasis vulgaris", "Mature T/NK-cell lymphomas", "Rheumatoid arthritis",
                       "Juvenile rheumatoid arthritis", "Psoriasis vulgaris", "Head and neck cancer",
                       "Malignant lymphoma", "Meningitis"),
  opengwas_id = c("ebi-a-GCST90018907", "finn-b-CD2_TNK_LYMPHOMA", "ebi-a-GCST90018910",
                  "ebi-a-GCST90018873", "ebi-a-GCST90018907", "ieu-b-4912", "ebi-a-GCST90018878",
                  "finn-b-MENINGITIS"),
  sample_size = c("483174", "NA", "417256", "409217", "483174", "373122", "490803",
                  "NA"),
  ncases = c("5072", "150", "8255", "216", "5072", "1106", "3546", "770"),
  outcome_type = c("Binary", "Binary", "Binary", "Binary", "Binary", "Binary", "Binary", "Binary")
)


#######################################################
# Exploring openGWAS outcome datasets - Thalidomide
#######################################################

drug <- "Thalidomide"

# Reading in MR results
drug_res <- MR_results[grep(drug, MR_results)] %>% 
  fread()

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[3]

ind_res <- drug_res %>% 
  filter(indication == gsub(" ", "_", ind))

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Erythema nodosum", "Erythema nodosum", "Erythema nodosum"),
  opengwas_id = c("finn-b-L12_ERYTHEMANODOSUM", "finn-b-L12_ERYTHEMANODOSUM", "finn-b-L12_ERYTHEMANODOSUM"),
  sample_size = c("NA", "NA", "NA"),
  ncases = c("433", "433", "433"),
  outcome_type = c("Binary", "Binary", "Binary")
)


#######################################################
# Exploring openGWAS outcome datasets - Tretinoin
#######################################################

drug <- "Tretinoin"

# Reading in MR results
drug_res <- MR_results[grep(drug, MR_results)] %>% 
  fread()

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[2]

ind_res <- drug_res %>% 
  filter(indication == gsub(" ", "_", ind))

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Acne vulgaris", "Acute myeloid leukaemia"),
  opengwas_id = c("finn-b-L12_ACNE_VULGARIS", "finn-b-C3_AML"),
  sample_size = c("NA", "NA"),
  ncases = c("1092","90"),
  outcome_type = c("Binary", "Binary")
)


#######################################################
# Exploring openGWAS outcome datasets - Valproate
#######################################################

drug <- "Valproic acid"

# Reading in MR results
drug_res <- MR_results[grep(drug, MR_results)] %>% 
  fread()

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == "Valproate") %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[4]

ind_res <- drug_res %>% 
  filter(indication == gsub(" ", "_", ind))

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Epilepsy", "Manic episode", "Epilepsy", "Diagnoses - main ICD10: G43 Migraine"),
  opengwas_id = c("ebi-a-GCST90018840", "finn-b-F5_MANIA", "ebi-a-GCST90018840", "ukb-d-G43"),
  sample_size = c("458310", "NA", "458310", "361194"),
  ncases = c("4382", "631", "4382", "1072"),
  outcome_type = c("Binary", "Binary", "Binary", "Binary")
)

#######################################################
# Exploring openGWAS outcome datasets - Warfarin
#######################################################

drug <- "Warfarin"

# Reading in MR results
drug_res <- MR_results[grep(drug, MR_results)] %>% 
  fread()

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[3]

ind_res <- drug_res %>% 
  filter(indication == gsub(" ", "_", ind))

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Non-cancer illness code, self-reported: deep venous thrombosis (dvt)", "Venous thromboembolism",
                       "Pulmonary embolism"),
  opengwas_id = c("ukb-b-12040", "ukb-d-I9_VTE", "finn-b-I9_PULMEMB"),
  sample_size = c("462933", "361194", "NA"),
  ncases = c("9241", "4620", "4185"),
  outcome_type = c("Binary", "Binary", "Binary")
)


#######################################################
# Plotting...
#######################################################
# TO DO:
# Loop through drug, plot forest plot and save

id_search <- paste0("^",
                    paste(unique(studies$opengwas_id), collapse = "$|^"),
                    "$")

outcomes <- Cap_res[grep(id_search, Cap_res$id.outcome), ]

plot_df <- outcomes %>% 
  mutate(
    OR = exp(b),
    lower = exp(b - 1.96 * se),
    upper = exp(b + 1.96 * se)
  )

plot_df <- plot_df %>%
  mutate(
    outcome = factor(openGWAS_outcome),
    sig = pval < 0.05
  )

ggplot(plot_df,
       aes(
         y = id.exposure,
         x = OR,
         xmin = lower,
         xmax = upper
       )) +
  
  geom_errorbarh(aes(color = sig), height = 0.2) +
  
  geom_point(aes(fill = sig),
             shape = 21,
             size = 2,
             colour = "black") +
  
  geom_vline(xintercept = 1, linetype = 2) +
  
  scale_x_log10() +
  
  facet_wrap(~ openGWAS_outcome) +
  
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "black")) +
  scale_color_manual(values = c("FALSE" = "grey70", "TRUE" = "black")) +
  
  theme_bw() +
  
  labs(x = "Odds Ratio (log scale)", y = NULL)















plot1 <- ggplot((plot_df %>%
                   filter(id.outcome == unique(studies$opengwas_id)[1])),
                aes(x = OR, 
                    y = id.exposure,
                    xmin = lower,
                    xmax = upper,
                    colour = gene.exposure)) +
  geom_pointrange(
    aes(fill = sig)
  ) +
  geom_vline(xintercept = 1, linetype = 2) +
  theme_bw() +
  theme(
    axis.title.y = element_blank()
  )

plot2 <- ggplot((plot_df %>%
                   filter(id.outcome == unique(studies$opengwas_id)[2])),
                aes(x = b, 
                    y = id.exposure,
                    xmin = b - 1.96 * se,
                    xmax = b + 1.96 * se,
                    colour = gene.exposure,
                    fill = sig)) +
  geom_pointrange(
    aes(fill = sig)
  ) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw() +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )



plot3 <- ggplot((plot_df %>%
                   filter(id.outcome == unique(studies$opengwas_id)[3])),
                aes(x = OR, 
                    y = id.exposure,
                    xmin = lower,
                    xmax = upper,
                    colour = gene.exposure,
                    fill = sig)) +
  geom_pointrange() +
  geom_vline(xintercept = 1, linetype = 2) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank())

png(file.path("/Users/wi24989/Documents", "test_plot.png"), height = 1000, width = 1200)

plot1 + plot2 + plot3 +
  plot_annotation(title = 'Grouped IVW MR Analysis for 72 Metabolites Against all Gestational Traits')

dev.off()



ggplot(plot_df,
       aes(x = b, y = id.exposure,
           xmin = b - 1.96 * se,
           xmax = b + 1.96 * se,
           colour = gene.exposure)) +
  geom_pointrange() +
  geom_vline(xintercept = 0, linetype = 2) +
  facet_wrap(~ outcome, scales = "free_y") +
  theme_bw() +
  ggh4x::facet_wrap2(~ outcome, axes = "y")





ggforestplot::forestplot(
  df = plot_df,
  name = id.exposure,
  estimate = b,
  pvalue = pval,
  psignif = 0.05,
  colour = gene.exposure,
  logodds = T
) +
  facet_wrap(~ openGWAS_outcome, scales = "free_y")


