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

drug_list <- fread(file.path(raw_data, "Drug_details.csv")) %>%
  select(Drug, Category)

prescription_files <- file.path(raw_data, "prescription_data/OpenPrescribing") %>%
  list.files(., full.names = T)

output_dir <- file.path(interim_data, "prescription_data")

#######################################################
# Reading in drugs of interest
#######################################################

drugs <- fread(drug_list, header = F)
colnames(drugs)[1] <- "Drug"

#######################################################
# Reading in prescription data and summing yearly prescriptions
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

# Calculate the yearly average
drug_yearly_prescriptions$Mean <- rowMeans(drug_yearly_prescriptions[,2:5],)

#######################################################
# Creating Map between Prescription Data and Drugs
#######################################################

# Drug name map
drug_mapping <- data.table(
  prescribing_names = c(
    "adapalene_+_adapalene_benzoyl_peroxide",
    "azilsartan_medoxomil",
    "candesartan_cilexetil",
    "captopril",
    "clindamycin_tretinoin_+_tretinoin_(acne)",
    "diclofenac_diethylammonium_+_diclofenac_sodium_(topical)",
    "diclofenac_potassium_+_diclofenac_sodium_(systemic)",
    "enalapril_hydrochlorothiazide_+_enalapril_maleate",
    "eprosartan",
    "felodipine_ramipril_+_ramipril",
    "folic_acid_+_iron_and_folic_acid",
    "fosinopril_sodium",
    "ibuprofen_(nsaid)",
    "imidapril_hydrochloride",
    "irbesartan_+_irbesartan_hydchlorothiazide",
    "isotretinoin_(systemic)",
    "isotretinoin_(topical)",
    "lenalidomide",
    "lisinopril_+_lisinopril_hydrochlorothiazide",
    "lithium_carbonate",
    "losartan_potassium_+_losartan_potassium_hydchlorothiazide",
    "mefenamic_acid",
    "meloxicam",
    "methotrexate_+_methotrexate_(rheumatism)",
    "modafinil",
    "mycophenolate_mofetil_(systemic)",
    "nabumetone",
    "naproxen_+_sumatriptan_succinate_naproxen_sodium",
    "olmesartan_medoxomil_+_olmesartan_medoxomil_amlodipine_+_olmesartan_me…",
    "perindopril_arginine_+_perindopril_arginine_indapamide_+_perindopril_e…",
    "quinapril_hydrochloride_+_quinapril_hydrochloride_hydchlorothiazide",
    "sacubitril_valsartan_+_valsartan_+_valsartan_amlodipine_+_valsartan_hy…",
    "sodium_valproate",
    "telmisartan_+_telmisartan_hydrochlorothiazide",
    "thalidomide_(immunomodulating)",
    "topiramate",
    "trandolapril",
    "tretinoin_(systemic)",
    "warfarin_sodium"
  ),
  Drug = c(
  "Adapalene",
  "Azilsartan_medoxomil",
  "Candesartan_cilexetil",
  "Captopril",
  "Tretinoin",
  "Diclofenac",
  "Diclofenac",
  "Enalapril",
  "Eprosartan",
  "Ramipril",
  "Folate",
  "Fosinopril",
  "Ibuprofen",
  "Imidapril",
  "Irbesartan",
  "Isotretinoin",
  "Isotretinoin",
  "Lenalidomide",
  "Lisinopril",
  "Lithium_carbonate",
  "Losartan",
  "Mefenamic_acid",
  "Meloxicam",
  "Methotrexate",
  "Modafinil",
  "Mycophenolate_mofetil",
  "Nabumetone",
  "Naproxen",
  "Olmesartan",
  "Perindopril",
  "Quinapril",
  "Valsartan",
  "Valproate",
  "Telmisartan",
  "Thalidomide",
  "Topiramate",
  "Trandolapril",
  "Tretinoin",
  "Warfarin"
  )
)

fwrite(drug_mapping, file.path(raw_data, "prescription_data/OpenPrescription_Mapping.csv"))

#######################################################
# Match drugs to prescription files
#######################################################

drug_yearly_prescriptions <- full_join(drug_mapping,
                                       drug_yearly_prescriptions, 
                                       by = c("prescribing_names" = "Drug"))

drug_yearly_prescriptions$Drug <- gsub("_", " ", drug_yearly_prescriptions$Drug)

drug_yearly_prescriptions <- full_join(drug_yearly_prescriptions,
                                       drug_list,
                                       by = "Drug")

drug_yearly_prescriptions <- drug_yearly_prescriptions %>% 
  relocate(Category, .after = Drug)

#######################################################
# Adding Topical/Systematic Labels
#######################################################

drug_yearly_prescriptions$Drug[
  drug_yearly_prescriptions$prescribing_names == 
    "diclofenac_diethylammonium_+_diclofenac_sodium_(topical)"] <- "Diclofenac (Topical)"

drug_yearly_prescriptions$Drug[
  drug_yearly_prescriptions$prescribing_names == 
    "diclofenac_potassium_+_diclofenac_sodium_(systemic)"] <- "Diclofenac (Systematic)"

drug_yearly_prescriptions$Drug[
  drug_yearly_prescriptions$prescribing_names == 
    "isotretinoin_(systemic)"] <- "Isotretinoin (Systematic)"

drug_yearly_prescriptions$Drug[
  drug_yearly_prescriptions$prescribing_names == 
    "isotretinoin_(topical)"] <- "Isotretinoin (Topical)"


#######################################################
# Save
#######################################################

fwrite(drug_yearly_prescriptions, file.path(interim_data, "prescription_data/drug_yearly_prescriptions.csv"))
