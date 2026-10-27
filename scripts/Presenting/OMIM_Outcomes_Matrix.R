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
  select(Gene_Name, Drug) %>%
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

outcome_totals <- outcome_totals[, c("Drug", "Category", "Gene_Name", "OMIM_Outcomes")]

#######################################################
# Save
#######################################################

fwrite(outcome_totals, output_file)


#######################################################
# Create Heatmap
#######################################################

#######################################################
# Setting Drug and Target Orders to Match Target Matrix
#######################################################

# Reading in Target Matrix
target_matrix <- fread(target_matrix_file)

# Drug Order
drug_order <- as.character(
  target_matrix[2, -1, with = F]
)

# Drug categories order
drug_categories <- as.character(
  target_matrix[1, -1, with = FALSE]
)

# Target order
target_order <- as.character(
  target_matrix$Target[-c(1, 2)]
)

#######################################################
# Creating Drug/Target Presence Matrix
#######################################################

presence_dt <- outcome_totals %>%
  select(Drug, Gene_Name)

# Mark every observed Drug/Gene pair as present
presence_dt[, present := 1L]

# Convert to wide format
presence_wide <- dcast(
  presence_dt,
  Gene_Name ~ Drug,
  value.var = "present",
  fill = 0
)

presence_wide <- presence_wide[!is.na(presence_wide$Gene_Name)]

# Convert to matrix
presence_mat <- as.matrix(
  presence_wide[, -1, with = FALSE]
)

rownames(presence_mat) <- presence_wide$Gene_Name

# Apply the original target matrix ordering
presence_mat <- presence_mat[
  target_order,
  drug_order,
  drop = FALSE
]

#######################################################
# Create the OMIM outcome matrix
#######################################################

outcome_wide <- dcast(
  outcome_totals,
  Gene_Name ~ Drug,
  value.var = "OMIM_Outcomes",
  fun.aggregate = function(x) {
    
    # Keep NA if there are no actual values
    if (all(is.na(x))) {
      NA_real_
    } else {
      sum(x, na.rm = TRUE)
    }
  }
)

outcome_wide <- outcome_wide[!is.na(outcome_wide$Gene_Name)]

# Convert to matrix
outcome_mat <- as.matrix(
  outcome_wide[, -1, with = FALSE]
)

rownames(outcome_mat) <- outcome_wide$Gene_Name

# Apply the original target matrix ordering
outcome_mat <- outcome_mat[
  target_order,
  drug_order,
  drop = FALSE
]

#######################################################
# Drug category colours
#######################################################

category_levels <- unique(drug_categories)

category_colours <- setNames(
  grDevices::hcl.colors(
    length(category_levels),
    palette = "Spectral"
  ),
  category_levels
)

# Drug category annotation

ha <- HeatmapAnnotation(
  
  Category = factor(
    drug_categories,
    levels = category_levels
  ),
  
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

#######################################################
# Create heatmap
#######################################################

matrix_plot <- Heatmap(
  
  presence_mat,
  
  name = "Drug-Gene",
  
  # Keep original ordering
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  
  # Group drugs according to their category
  column_split = factor(
    drug_categories,
    levels = category_levels
  ),
  
  # No large gaps between categories
  column_gap = unit(0.5, "mm"),
  
  # One colour for presence, white for absence
  col = c(
    "0" = "white",
    "1" = "steelblue"
  ),
  
  # Category annotation
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
  
  
  # Add OMIM outcome count inside each present cell

  cell_fun = function(
    j, i, x, y, width, height, fill
  ) {
    
    # Only write text if the Drug/Gene pair exists
    if (presence_mat[i, j] == 1) {
      
      value <- outcome_mat[i, j]
      
      # Only display a value if OMIM_Outcomes is available
      if (!is.na(value)) {
        
        grid.text(
          label = format(
            value,
            trim = TRUE
          ),
          x = x,
          y = y,
          gp = gpar(
            fontsize = 10,
            col = "white"
          )
        )
      }
    }
  },
  
  # Don't show the binary heatmap legend
  show_heatmap_legend = FALSE,
  
  # Thin light gridlines
  rect_gp = gpar(
    col = "grey90",
    lwd = 0.2
  ),

  # Dark borders around the column slices
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
