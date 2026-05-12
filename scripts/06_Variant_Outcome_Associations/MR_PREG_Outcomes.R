##
# Association data for drug target variants and MR PREG Outcomes
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)
library(tidyr)
library(gpmapr)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

raw_data <- Sys.getenv("rawdatadir")
interim_data <- Sys.getenv("interimdatadir")

#######################################################
# Reading in MR PREG data
#######################################################

MR_PREG <- fread(file.path(raw_data, "predicted_outcomes/MR_PREG/ma_out_dat.txt"))

#######################################################
# Reading in Drug Target Variant data
#######################################################

variant_data <- fread(file.path(interim_data, "predicted_outcomes/G_P_Map_Variants/Drug_Target_Variant.csv"))
