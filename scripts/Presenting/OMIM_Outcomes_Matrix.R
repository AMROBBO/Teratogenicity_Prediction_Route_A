##
# Creating a matrix to display number of OMIM outcomes associated with each Drug and it's targets
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)
library(stringr)
library(maditr)
library(ComplexHeatmap)
library(grid)
library(ggplot2)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

raw_data <- Sys.getenv("rawdatadir")
interim_data <- Sys.getenv("interimdatadir")
processed_data <- Sys.getenv("processeddatadir")

drug_file <- file.path(raw_data, "Drug_details.csv")
targets_file <- file.path(interim_data, "predicted_outcomes/Drug_Bank_targets/Drug_Bank_targets.csv")
omim_file <- file.path(interim_data, "predicted_outcomes/OMIM_outcomes/OMIM_outcomes_processed.csv")

target_matrix_file <- file.path(processed_data, "Presenting/Targets_Matrix.csv")

output_file <- file.path(processed_data, "Presenting/OMIM_Matrix.csv")
output_plot <- file.path(processed_data, "Presenting/OMIM_Matrix.pdf")

#######################################################
# Reading in Drug Class Data
#######################################################

drug_list <- fread(drug_file) %>%
  select(Drug, Category)

#######################################################
# Reading in Drug Target Data
#######################################################

drug_targets <- fread(targets_file) %>%
  filter(Primary == "Primary") %>%
  select(Gene_Name, Drug, Direction) %>%
  unique()

drug_targets[Drug == "Valproic acid", Drug := "Valproate"]
drug_targets[Drug == "Folic acid", Drug := "Folate"]

#######################################################
# Reading in OMIM Outcome Data
#######################################################

omim_outcomes <- fread(omim_file) %>%
  filter(Pharmacological_Action == "TRUE") %>%
  select(Gene_Name, Drug, Outcome) 

omim_outcomes[Drug == "Folic acid", Drug := "Folate"]

#######################################################
# Summing the number of Drug/Target Outcomes
#######################################################

outcome_totals <- omim_outcomes %>%
  select(Gene_Name, Drug) %>%
  unique()

outcome_totals$OMIM_Outcomes <- numeric()

for(i in 1:nrow(outcome_totals)){
  n_outcomes <- omim_outcomes %>%
    filter(Gene_Name == outcome_totals[i]$Gene_Name) %>%
    filter(Drug == outcome_totals[i]$Drug) %>%
    nrow()
  
  outcome_totals[i]$OMIM_Outcomes <- n_outcomes
}

#######################################################
# Combining to a Single Dataset
#######################################################

outcome_totals <- outcome_totals %>% 
  full_join(., drug_targets, by = c("Drug", "Gene_Name")) %>%
  full_join(., drug_list, by = "Drug")

outcome_totals$OMIM_Outcomes <- outcome_totals$OMIM_Outcomes %>%
  replace(is.na(.), 0)

outcome_totals <- outcome_totals[, c("Drug", "Category", "Gene_Name", "Direction", "OMIM_Outcomes")]

#######################################################
# Save
#######################################################

fwrite(outcome_totals, output_file)

#######################################################
# Reading in Target Matrix and extracting ordering
#######################################################

target_matrix <- fread(target_matrix_file)

# Extract direction matrix
mat <- target_matrix[-c(1, 2), -1, with = FALSE] %>%
  as.matrix()

mat <- apply(mat, 2, as.numeric)

# Add row and column names
rownames(mat) <- target_matrix$Target[-c(1, 2)]
colnames(mat) <- as.character(target_matrix[2, -1, with = FALSE])

# Extract ordering directly from direction matrix
target_order <- rownames(mat)
drug_order <- colnames(mat)

# Drug categories
drug_categories <- as.character(
  target_matrix[1, -1, with = FALSE]
)

#######################################################
# Creating OMIM outcome count matrix
#######################################################

dt <- as.data.table(copy(outcome_totals))

# Remove rows without a valid Drug/Gene pair
dt <- dt[
  !is.na(Gene_Name) & Gene_Name != "" &
  !is.na(Drug) & Drug != ""
]

# Aggregate OMIM outcome counts for each Drug/Gene pair
side_effect_wide <- dcast(
  dt,
  Gene_Name ~ Drug,
  value.var = "OMIM_Outcomes",
  fun.aggregate = function(x) sum(x, na.rm = TRUE),
  fill = 0
)

# Convert to matrix
count_mat <- as.matrix(
  side_effect_wide[, -1, with = FALSE]
)

storage.mode(count_mat) <- "numeric"

rownames(count_mat) <- side_effect_wide$Gene_Name

#######################################################
# Align count matrix to direction matrix
#######################################################

# Initialise a matrix of zeroes with exactly the same
# dimensions and names as the direction matrix
side_effect_mat <- matrix(
  0,
  nrow = nrow(mat),
  ncol = ncol(mat),
  dimnames = dimnames(mat)
)

# Identify matching genes and drugs
common_genes <- intersect(
  rownames(count_mat),
  rownames(side_effect_mat)
)

common_drugs <- intersect(
  colnames(count_mat),
  colnames(side_effect_mat)
)

# Transfer counts into the correctly aligned matrix
side_effect_mat[
  common_genes,
  common_drugs
] <- count_mat[
  common_genes,
  common_drugs,
  drop = FALSE
]

#######################################################
# Create Heatmap
#######################################################

# Colours for drug categories
category_colours <- setNames(
  grDevices::hcl.colors(
    length(unique(drug_categories)),
    palette = "Spectral"
  ),
  unique(drug_categories)
)

# Colours for direction of action
direction_colours <- c(
  "0" = "white",       # No relationship
  "1" = "#36b9e4",   # Positive
  "2" = "#ea8cc6",        # Negative
  "3" = "grey70"       # Unknown
)

# Drug category annotation
ha <- HeatmapAnnotation(
  Category = drug_categories,
  
  col = list(
    Category = category_colours
    ),
  
  show_annotation_name = FALSE,
  
  annotation_legend_param = list(
    Category = list(
      title = "Drug category"
    )
  )
)

matrix_plot <- Heatmap(

  mat,

  name = "Direction",

  cluster_rows = FALSE,
  cluster_columns = FALSE,

  column_split = drug_categories,
  column_gap = grid::unit(0.5, "mm"),

  # Direction colours
  col = direction_colours,

  # Drug category colours
  bottom_annotation = ha,

  # Drug names
  show_column_names = TRUE,
  column_names_gp = gpar(fontsize = 10),
  column_title = "Drug",
  column_title_side = "bottom",

  # Target names
  show_row_names = TRUE,
  row_names_gp = gpar(fontsize = 10),
  row_title = "Target",
  row_title_side = "right",

  # Display side-effect counts inside cells
  cell_fun = function(j, i, x, y, width, height, fill) {

    value <- side_effect_mat[i, j]

    if (mat[i, j] != 0) {

      grid.text(
        label = as.character(value),
        x = x,
        y = y,
        gp = gpar(
          fontsize = 8,
          col = "white"
        )
      )
    }
  },

  show_heatmap_legend = TRUE,

  heatmap_legend_param = list(
    title = "Direction of action",
    at = c(1, 2, 3),
    labels = c("Positive", "Negative", "Unknown")
  ),

  rect_gp = grid::gpar(
    col = "grey90",
    lwd = 0.2
  ),

  border = TRUE,

  border_gp = grid::gpar(
    col = "grey30",
    lwd = 1
  )
)

#######################################################
# Save
#######################################################

pdf(
  output_plot,
  width = 14,
  height = 14
)

draw(matrix_plot)

dev.off()
