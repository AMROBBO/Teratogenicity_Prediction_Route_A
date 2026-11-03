#######################################################
# Load libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")


mycophenolate_LLM <- fread(file.path(interim_data, "reported_outcomes/UKTIS_outcomes/5_Final_Outcomes/MYCOPHENOLATE_MOFETIL_combined_outcomes.csv"))
 
                          