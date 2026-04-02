#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)
library(xml2)
library(XML)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

raw_data <- Sys.getenv("rawdatadir")
interim_data <- Sys.getenv("interimdatadir")

clinvar_path <- file.path(raw_data, "predicted_outcomes/ClinVar_data/ClinVarVCVRelease_00-latest.xml")
targets_file <- file.path(interim_data, "predicted_outcomes/Drug_Bank_targets/Drug_Bank_targets.csv")

#######################################################
# Reading in Drug Target Genes
#######################################################

targets_data <- fread(targets_file)

target_genes <- unique(targets_data$Gene_Name)

#######################################################
# Parsing through ClinVar data
#######################################################

#target_genes <- c("AGTR1", "ABAT")  # example

results <- list()
current <- list()

capture <- FALSE
in_trait <- FALSE
in_gene <- FALSE

xmlEventParse(clinvar_path,
              handlers = list(
                
                startElement = function(name, attrs) {
                  
                  # Start of a record
                  if (name == "VariationArchive") {
                    capture <<- TRUE
                    current <<- list(
                      variation_id = attrs["VariationID"],
                      genes = character(),
                      name = NA,
                      traits = character(),
                      significance = NA
                    )
                  }
                  
                  if (!capture) return()
                  
                  # Gene symbol (attribute)
                  if (name == "Gene") {
                    in_gene <<- TRUE
                    current$genes <<- c(current$genes, attrs["Symbol"])
                  }
                  
                  # Disease section only
                  if (name == "TraitSet" && attrs["Type"] == "Disease") {
                    in_trait <<- TRUE
                  }
                },
                
                text = function(text) {
                  if (!capture) return()
                  current$last_text <<- trimws(text)
                },
                
                endElement = function(name) {
                  if (!capture) return()
                  
                  # Variant name
                  if (name == "Name" && is.na(current$name)) {
                    current$name <<- current$last_text
                  }
                  
                  # Preferred disease name
                  if (name == "ElementValue" && in_trait) {
                    current$traits <<- c(current$traits, current$last_text)
                  }
                  
                  # Clinical significance
                  if (name == "Description" && is.na(current$significance)) {
                    current$significance <<- current$last_text
                  }
                  
                  # Leaving sections
                  if (name == "TraitSet") in_trait <<- FALSE
                  if (name == "Gene") in_gene <<- FALSE
                  
                  # End of record
                  if (name == "VariationArchive") {
                    capture <<- FALSE
                    
                    # Only save records with target genes
                    if (any(current$genes %in% target_genes)) {
                      results[[length(results) + 1]] <<- current
                    }
                  }
                }
              )
)








con <- file(clinvar_path, "r")

lines <- readLines(con, n = 300)
