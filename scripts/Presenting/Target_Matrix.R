##
# Creating a matrix to display targets for each drug
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

targets_file <- file.path(interim_data, "predicted_outcomes/Drug_Bank_targets/Drug_Bank_targets.csv")
drug_file <- file.path(raw_data, "Drug_details.csv")

output_file <- file.path(processed_data, "Presenting/Targets_Matrix.csv")
output_plot <- file.path(processed_data, "Presenting/Targets_Matrix.pdf")

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

#######################################################
# Assign code to each direction of action
#######################################################

direction_codes <- c(
  "positive" = 1,
  "negative" = 2,
  "unknown"  = 3
)

drug_targets[, Direction_code := direction_codes[Direction]]

#######################################################
# Creating Drug X Target Matrix
#######################################################

# Creating matrix
target_matrix <- data.table::dcast(drug_targets,
                                   Drug ~ Gene_Name,
                                   value.var = "Direction_code",
                                   fill = 0)

# Cluster targets based on shared drugs
target_mat <- as.matrix(target_matrix[, -1])

target_clust <- hclust(
  dist(t(target_mat), method = "binary"),
  method = "average"
)

target_matrix <- target_matrix[
  ,
  c(1, target_clust$order + 1),
  with = FALSE
]

target_matrix[Drug == "Folic acid", Drug := "Folate"]
target_matrix[Drug == "Valproic acid", Drug := "Valproate"]

#######################################################
# Grouping and Ordering Drugs by Class
#######################################################

target_matrix <- full_join(drug_list,
                           target_matrix,
                           by = "Drug")

target_matrix <- target_matrix %>%
  replace(is.na(.), 0)

target_matrix <- target_matrix %>%
  arrange(Category, Drug) %>%
  relocate(Category) %>%
  transpose(., keep.names = "Target")

#######################################################
# Save
#######################################################

fwrite(target_matrix, output_file)

#######################################################
# Target Matrix Heatmap
#######################################################

# Extract the 0/1 matrix
mat <- target_matrix[-c(1, 2), -1, with = FALSE] %>%
  as.matrix()

mat <- apply(mat, 2, as.numeric)

# Add names
rownames(mat) <- target_matrix$Target[-c(1, 2)]
colnames(mat) <- as.character(target_matrix[2, -1, with = FALSE])

# Add drug categories
drug_categories <- as.character(
  target_matrix[1, -1, with = FALSE]
)

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
  "1" = "lightblue",   # Positive
  "2" = "pink",   # Negative
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
  
  name = "matrix",
  
  # Don't cluster either dimension
  cluster_rows = FALSE,
  cluster_columns = FALSE,
  
  # Split column according to drug category
  column_split = drug_categories,
  
  # Gap between categories
  column_gap = grid::unit(0.5, "mm"),
  
  # Colour the relationships
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
  
  # No heatmap legend
  show_heatmap_legend = TRUE,
  heatmap_legend_param = list(
    title = "Direction of action",
    at = c(1, 2, 3),
    labels = c("Positive", "Negative", "Unknown")
  ),
  
  # Thin Grid lines
  rect_gp = grid::gpar(
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

draw(matrix_plot,
     heatmap_legend_side = "right",
     annotation_legend_side = "right",
     merge_legends = TRUE,
     legend_grouping = "original")

dev.off()
