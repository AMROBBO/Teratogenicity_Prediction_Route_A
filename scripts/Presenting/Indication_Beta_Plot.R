##
# Plotting distribution of predicted causal effects of drug targets on drug indications
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(grid)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

processed_dir <- Sys.getenv("processeddatadir")
interim_dir <- Sys.getenv("interimdatadir")
plots_dir <- Sys.getenv("plotsdir")

mr_res_files <- file.path(processed_dir, "Primary_indications_MR_Results") %>%
  list.files(., full.names = T)
chosen_openGWAS_dir <- file.path(interim_dir, "predicted_outcomes/Primary_Indication_Mapping/Open_GWAS_Chosen_Outcomes") %>%
  list.files(., full.names = T)

out_plot <- file.path(plots_dir, "Primary_Indication_Plots/Summary_Plot.pdf")

#######################################################
# Read in MR Results for all chosen outcomes and combine
#######################################################

all_res <- data.frame()

for(i in 1:length(mr_res_files)){
  
  # Read in MR Results
  mr_res_file <- mr_res_files[i]

  # Extract drug name
  drug <- mr_res_file %>%
    gsub(".*/", "", .) %>%
    gsub("_Primary_Indication_MR_Res.csv", "", .)

  # Chosen openGWAS outcomes to proxy drug indications
  chosen_openGWAS <- chosen_openGWAS_dir %>%
    grep(paste0("/", drug, "_"), ., value = T) %>%
    fread()

  # Extract MR Results for chosen openGWAS outcomes
  mr_res <- mr_res_file %>%
    fread() %>%
    filter(id.outcome %in% unique(chosen_openGWAS$opengwas_id)) %>%
    select(id.exposure, id.outcome, b, se, pval, SNP, Direction, gene.exposure, 
           openGWAS_outcome) %>%
    unique()

  # Align predicted effect to direction of drug effect on the target
  for(g in unique(mr_res$gene.exposure)){
    drug_direction <- mr_res[which(mr_res$gene.exposure == g), "Direction"] %>%
      unique()
    if(drug_direction == "negative"){
      mr_res[which(mr_res$gene.exposure == g), "b"] <- -1 * mr_res[which(mr_res$gene.exposure == g), "b"]
    }
  }

  # Calculate odds ratio (binary outcomes)
  mr_res <- mr_res %>%
    mutate(
      OR = exp(b),
      lower = exp(b - 1.96 * se),
      upper = exp(b + 1.96 * se)
    )

  # Add to full dataset
  all_res <- all_res %>%
    rbind(., mr_res) %>%
    unique() %>% 
    filter(Direction != "unknown") # Removing results that are not alligned to drug direction
}
 
#######################################################
# Histogram
#######################################################

p_hist <- ggplot(all_res, aes(x = OR)) +
  geom_histogram(binwidth = 0.02, boundary = 0,
                 colour = "white", fill = "black") +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_x_log10(breaks = c(0.2, 0.25, 0.33, 0.4, 0.5, 0.67, 0.8, 1, 
                           1.25, 1.5, 2, 2.5, 3, 4, 5)) +
  labs(x = "Odds ratio (aligned to drug action, log scale)", y = "Count") +
  theme_classic() 

#######################################################
# Counts either side of 1 + sign test
#######################################################

n_below <- sum(all_res$OR < 1)
n_above <- sum(all_res$OR > 1)
n_equal <- sum(all_res$OR == 1)
sign_test <- binom.test(n_below, n_below + n_above, p = 0.5)

tab_sign <- data.frame(
  Measure = c("Estimates", "OR < 1 (drug action lowers odds)", "OR > 1 (drug action raises odds)",
              "OR = 1 exactly (excluded from test)", "Proportion below 1 (95% CI)",
              "Sign test p-value"),
  Value = c(nrow(all_res),
            sprintf("%d (%.1f%%)", n_below, 100 * n_below / nrow(all_res)),
            sprintf("%d (%.1f%%)", n_above, 100 * n_above / nrow(all_res)),
            n_equal,
            sprintf("%.3f (%.3f to %.3f)", sign_test$estimate,
                    sign_test$conf.int[1], sign_test$conf.int[2]),
            format.pval(sign_test$p.value, digits = 2))
)

#######################################################
# Wilcoxon signed-rank test on beta
#######################################################

fmt_or <- function(x) formatC(x, format = "f", digits = 3)
fmt_p  <- function(p) format.pval(p, digits = 2, eps = 1e-4)

wt <- wilcox.test(all_res$b, mu = 0, conf.int = TRUE)

tab_wilcox <- data.frame(
  Measure = c("Pseudo-median OR (Hodges-Lehmann)", "95% CI",
              "Median OR", "Geometric mean OR", "V statistic", "Wilcoxon p-value"),
  Value = c(fmt_or(exp(wt$estimate)),
            paste(fmt_or(exp(wt$conf.int[1])), "to", fmt_or(exp(wt$conf.int[2]))),
            fmt_or(exp(median(all_res$b))),
            fmt_or(exp(mean(all_res$b))),
            format(wt$statistic, big.mark = ","),
            fmt_p(wt$p.value))
)

#######################################################
# Write PDF 
#######################################################

tab_theme <- ttheme_minimal(base_size = 11,
                            core = list(fg_params = list(hjust = 0, x = 0.02)),
                            colhead = list(fg_params = list(hjust = 0, x = 0.02, fontface = "bold")))

titled_table <- function(df, title) {
  arrangeGrob(tableGrob(df, rows = NULL, theme = tab_theme),
              top = textGrob(title, x = 0.02, hjust = 0, gp = gpar(fontsize = 13, fontface = "bold")))
}

pdf(out_plot, width = 8.27, height = 11.69)   # A4 portrait
grid.arrange(p_hist,
             titled_table(tab_sign,   "Direction: counts either side of OR = 1 and sign test"),
             titled_table(tab_wilcox, "Location: Wilcoxon signed-rank test on log(OR)"),
             ncol = 1, heights = c(5, 2.6, 2.6, 0.6))
invisible(dev.off())

