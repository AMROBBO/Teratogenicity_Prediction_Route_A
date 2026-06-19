##
# Using openGWAS data to test association between drug target gene
# and primary indications of drugs
# 11. Plotting
##

#######################################################
# Load in libraries
#######################################################

#pak::pak("NightingaleHealth/ggforestplot")

library(dotenv)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(ggforestplot)
library(ggh4x)
library(patchwork)

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
# 11. Plotting
#######################################################

Cap_res <- MR_results[grep("Captopril", MR_results)] %>% 
  fread()

Cap_indications <- indications %>% 
  filter(Drug == "Captopril") %>% 
  filter(`Approval Level` == "Prescription")

ind <- unique(Cap_indications$Indication)[4]

Cap_MR <- Cap_res %>% 
  filter(indication == gsub(" ", "_", ind))

studies <- data.table(
  indication = unique(Cap_indications$Indication),
  opengwas_outcome = c("Hypertension", "Heart failure", "Diabetic nephropathy", "Heart failure"),
  opengwas_id = c("ieu-b-5144", "ebi-a-GCST009541", "ebi-a-GCST90018832", "ebi-a-GCST009541"),
  sample_size = c("462826", "977323", "452280", "977323"),
  outcome_type = c("Binary", "NA", "NA", "NA")
)

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


