## 
# Taking a random selection of drugs to manually carry out outcome extraction 
# to compare to model output
##

#######################################################
# Load libraries
#######################################################

library(dotenv)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

interim_data <- Sys.getenv("interimdatadir")

output_dir <- file.path(interim_data, "reported_outcomes/Extraction_Evaluation")

#######################################################
# Read in list of drugs
#######################################################

drugs <- readLines(file.path(interim_data, "teratogenic_drugs.txt"))

#######################################################
# Take random sample of 15 drugs
#######################################################

drugs_sample <- sample(drugs, 15)

#######################################################
# Save
#######################################################

fwrite(as.list(drugs_sample), file.path(output_dir, "Drugs_Sample.csv"))
