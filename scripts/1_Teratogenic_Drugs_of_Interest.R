#######################################################
# Set Working Directory
#######################################################
install.packages("dotenv")

library(dotenv)

load_dot_env("config.env")

data_dir <- Sys.getenv("datadir")
results_dir <- Sys.getenv("resultsdir")

