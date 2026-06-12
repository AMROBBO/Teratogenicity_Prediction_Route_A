## 
# Assigning outcomes to drugs
##

#######################################################
# Load libraries
#######################################################

library(dotenv)
library(jsonlite)
library(dplyr)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")

input_dir <- file.path(interim_data, "reported_outcomes/UKTIS_outcomes/4_Combined_Outcomes")

outcome_files <- list.files(input_dir, full.names = T)

outcomes <- read_json(outcome_files[1])

test <- fromJSON(outcome_files[1]) %>% as.data.frame()
test_e <- test$Outcomes %>% as.data.frame()
