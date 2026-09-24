##
# Reading in and summing prescription counts for each drug across England from
# 1/07/2021 - 01/06/2026. Data accessed from OpenPrescribing
# To report:
# Annual mean from 2022-2025 (full available years)
##

#######################################################
# Load in libraries
#######################################################

library(dotenv)
library(dplyr)
library(data.table)
library(tidyr)

#######################################################
# Initialising file paths
#######################################################

load_dot_env("config.env")

raw_data <- Sys.getenv("rawdatadir")
interim_data <- Sys.getenv("interimdatadir")

drug_list <- file.path(interim_data, "teratogenic_drugs.txt")
prescription_files <- file.path(raw_data, "prescription_data/OpenPrescribing") %>%
  list.files(., full.names = T)

output_dir <- file.path(interim_data, "prescription_data")

#######################################################
# Reading in drugs of interest
#######################################################

drugs <- fread(drug_list, header = F)
colnames(drugs)[1] <- "Drug"

#######################################################
# Reading in prescription data and assigning to drugs
#######################################################

drug_yearly_prescriptions <- data.table(
  Drug = character(),
  "2022 Total" = numeric(),
  "2023 Total" = numeric(),
  "2024 Total" = numeric(),
  "2025 Total" = numeric()
)
  
for (i in prescription_files){
  
  # Get drug name
  drug <- unlist(strsplit(basename(i), split = " "))
  drug <- paste(drug[3:(length(drug) - 1)], collapse = "_")
  
  # Read drug file
  prescription_data <- fread(i)
  
  # Create Year column
  prescription_data <- tidyr::separate(prescription_data, 
                                       col = date, 
                                       sep = "-", 
                                       into = c("Year", "Month", "Day"), 
                                       remove = F) 
  
  prescription_data$Year <- as.numeric(prescription_data$Year)
  
  # Sum and add yearly prescriptions totals
  tmp <- data.frame(matrix(ncol = 0, nrow = 1))
  
  tmp$Drug <- drug
  
  for (yr in 2022:2025){
    
    year_data <- prescription_data %>%
      filter(Year == yr)
    
    year_total <- sum(year_data$y_items)
    
    colname <- paste(yr, "Total")
    
    tmp[, colname] <- year_total
    
  }
  
  drug_yearly_prescriptions <- rbind(drug_yearly_prescriptions, tmp)
  
}

drug_yearly_prescriptions$Mean <- rowMeans(drug_yearly_prescriptions[,2:5],)

drug_mapping <- c(
  "adapalene" = "Adapalene",
  "azilsartan_medoxomil" = "Azilsartan_medoxomil",
  "candesartan_cilexetil" = "Candesartan_cilexetil",
  "captopril diclofenac_sodium_(systemic)" = "Captopril",
  "diclofenac_sodium_(topical)" = "Diclofenac",
  "enalapril_hydrochlorothiazide" = "Enalapril",
  "eprosartan" = "Eprosartan",
  "folic_acid" = "Folate",
  "fosinopril_sodium" = "Fosinopril",
  "ibuprofen_(nsaid)" = "Ibuprofen",
  "imidapril_hydrochloride" = "Imidapril",
  "irbesartan" = "Irbesartan",
  "isotretinoin_(systemic)" = "Isotretinoin",
  "isotretinoin_(topical)" = "Isotretinoin",
  "lenalidomide" = "Lenalidomide",
  "lisinopril" = "Lisinopril",
  "lithium_carbonate" = "Lithium_carbonate",
  "losartan_potassium" = "Losartan",
  "mefenamic_acid" = "Mefenamic_acid",
  "meloxicam" = "Meloxicam",
  "methotrexate" = "Methotrexate",
  "modafinil" = "Modafinil",
  "mycophenolate_mofetil_(systemic)" = "Mycophenolate_mofetil",
  "nabumetone" = "Nabumetone",
  "naproxen" = "Naproxen",
  "olmesartan_medoxomil" = "Olmesartan",
  "perindopril_arginine" = "Perindopril",
  "quinapril_hydrochloride" = "Quinapril",
  "ramipril" = "Ramipril",
  "sodium_valproate" = "Valproate",
  "telmisartan" = "Telmisartan",
  "thalidomide_(immunomodulating)" = "Thalidomide",
  "topiramate" = "Topiramate",
  "trandolapril" = "Trandolapril",
  "tretinoin_(systemic)" = "Tretinoin",
  "valsartan" = "Valsartan",
  "warfarin_sodium" = "Warfarin"
)


## Make this a data table to then save it for reference later
# Check that all the prescription files are the best ones for that drug - some seem very small numbers
# Match drugs to prescription files
# Save