##
# Selecting appropriate openGWAS outcomes to use in MR
# Choice was based on:
#   - Term similarity
#   - Sample size
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

interim_data <- Sys.getenv("interimdatadir")

indications_file <- file.path(interim_data, "reported_outcomes/Drug_Bank_outcomes/Drug_Bank_Indications.csv")
mapped_file <- file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/Mapped_outcomes.csv")

#MR_res_dir <- file.path(processed_data, "Primary_indications_MR_Results")
output_dir <- file.path(interim_data, "predicted_outcomes/Primary_Indication_Mapping/Open_GWAS_Chosen_Outcomes")

#######################################################
# Reading in data
#######################################################

indications <- fread(indications_file)

# Normalise some drug naming
indications[which(indications$Drug == "Valproate"), "Drug"] <- "Valproic acid"

mapped_outcomes <- fread(mapped_file) %>%
  select(indication, openGWAS_outcome, id, ncase, year, sex, population, unit, nsnp, sample_size, build, ncontrol, category, consortium)
#MR_results <- list.files(MR_res_dir, full.names = T)

#######################################################
# Exploring openGWAS outcome datasets - Adapalene
#######################################################

# Specify drug
drug <- "Adapalene"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[1]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size - Manually made

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Acne vulgaris"),
  opengwas_id = c("finn-b-L12_ACNE_VULGARIS"),
  sample_size = c("NA"),
  ncases = c("1092"),
  ncontrols = c("211139"),
  consortium = c("FinnGen"),
  outcome_type = c("Binary")
)

# Save

output_file <- file.path(output_dir, paste(drug, "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Azilsartan medoxomil
#######################################################

# Specify drug
drug <- "Azilsartan medoxomil"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == gsub(" ", "_", drug)) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[1]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size - Manually made

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Hypertension"),
  opengwas_id = c("ieu-b-5144"),
  sample_size = c("462826"),
  ncases = c("133680"),
  ncontrols = c("329146"),
  consortium = c("UK Biobank"),
  outcome_type = c("Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Candesartan cilexetil
#######################################################

# Specify drug
drug <- "Candesartan cilexetil"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == gsub(" ", "_", drug)) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[4]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size - Manually made

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Hypertension", "Heart failure", "Heart failure", "Heart failure"),
  opengwas_id = c("ieu-b-5144", "ebi-a-GCST009541", "ebi-a-GCST009541", "ebi-a-GCST009541"),
  sample_size = c("462826", "977323", "977323", "977323"),
  ncases = c("133680", "47309", "47309", "47309"),
  ncontrols = c("329146", "930014", "930014", "930014"),
  consortium = c("UK Biobank", "EBI", "EBI", "EBI"),
  outcome_type = c("Binary", "Binary", "Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Captopril
#######################################################

# Specify drug
drug <- "Captopril"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[4]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Hypertension", "Heart failure", "Diabetic nephropathy", "Heart failure"),
  opengwas_id = c("ieu-b-5144", "ebi-a-GCST009541", "ebi-a-GCST90018832", "ebi-a-GCST009541"),
  sample_size = c("462826", "977323", "452280", "977323"),
  ncases = c("133680", "47309", "1032", "47309"),
  ncontrols = c("329146", "930014", "451248", "930014"),
  consortium = c("UK Biobank", "EBI", "EBI", "EBI"),
  outcome_type = c("Binary", "Binary", "Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Diclofenac
#######################################################

# Specify drug
drug <- "Diclofenac"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[14]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("NA", 
                       "Pain (limb, back, neck, head abdominally)", 
                       "Injury of muscle and tendon at shoulder and upper arm level", 
                       "Myalgia", 
                       "Pain, not elsewhere classified",
                       "Pain in joint",
                       "NA", 
                       "Conjunctivitis", 
                       "Visual disturbances and blindness", 
                       "NA", 
                       "NA", 
                       "Migraine", 
                       "Actinic keratosis", 
                       "Excessive, freguent and irrelgular menstruation"),
  opengwas_id = c("NA", 
                  "finn-b-PAIN", 
                  "finn-b-ST19_INJURY_MUSCLE_TENDON_SHOUL_UPPER_ARM_LEVEL", 
                  "finn-b-M13_MYALGIA", 
                  "finn-b-R18_PAIN_NOT_ELSEW_CLASSIFIED",
                  "finn-b-JOINTPAIN",
                  "NA",
                  "finn-b-H7_CONJUNCTIVITIS",
                  "finn-b-H7_VISUALDISTBLIND",
                  "NA",
                  "NA",
                  "finn-b-G6_MIGRAINE",
                  "finn-b-L12_ACTINKERA",
                  "finn-b-N14_MESNRUIRREG"),
  sample_size = c("NA", 
                  "NR", 
                  "NR", 
                  "NR",
                  "NR",
                  "NR",
                  "NA",
                  "NR",
                  "NR",
                  "NA",
                  "NA",
                  "NR",
                  "NR",
                  "NR"),
  ncases = c("NA", 
             "87242", 
             "5637", 
             "3021", 
             "3811",
             "13419",
             "NA",
             "13655",
             "7926",
             "NA",
             "NA",
             "8547",
             "4817",
             "17873"),
  ncontrols = c("NA", 
                "131127", 
                "202866", 
                "167641", 
                "172999",
                "131550",
                "NA",
                "203517",
                "210866",
                "NA",
                "NA",
                "176107",
                "213273",
                "68969"),
  consortium = c("NA", 
                 "FinnGen", 
                 "FinnGen", 
                 "FinnGen", 
                 "FinnGen",
                 "FinnGen",
                 "NA",
                 "FinnGen",
                 "FinnGen",
                 "NA",
                 "NA",
                 "FinnGen",
                 "FinnGen",
                 "FinnGen"),
  outcome_type = c("NA", 
                   "Binary", 
                   "Binary", 
                   "Binary", 
                   "Binary",
                   "Binary",
                   "NA",
                   "Binary",
                   "Binary",
                   "NA",
                   "NA",
                   "Binary",
                   "Binary",
                   "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Enalapril
#######################################################

# Specify drug
drug <- "Enalapril"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[3]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Diagnoses - secondary ICD10: I50.1 Left ventricular failure", "Heart failure", "Hypertension"),
  opengwas_id = c("ukb-b-10555", "ebi-a-GCST009541", "ieu-b-5144"),
  sample_size = c("463010", "977323", "462826"),
  ncases = c("2046", "47309", "133680"),
  ncontrols = c("460964", "930014", "329146"),
  consortium = c("MRC-IEU", "EBI", "UK Biobank"),
  outcome_type = c("Binary", "Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Eprosartan
#######################################################

# Specify drug
drug <- "Eprosartan"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[1]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Hypertension"),
  opengwas_id = c("ieu-b-5144"),
  sample_size = c("462826"),
  ncases = c("133680"),
  ncontrols = c("329146"),
  consortium = c("UK Biobank"),
  outcome_type = c("Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Folate
#######################################################

# Specify drug
drug <- "Folate"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# No prescription uses

#######################################################
# Exploring openGWAS outcome datasets - Fosinopril
#######################################################

# Specify drug
drug <- "Fosinopril"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[1]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Hypertension"),
  opengwas_id = c("ieu-b-5144"),
  sample_size = c("462826"),
  ncases = c("133680"),
  ncontrols = c("329146"),
  consortium = c("UK Biobank"),
  outcome_type = c("Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Ibuprofen
#######################################################

# Specify drug
drug <- "Ibuprofen"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[6]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Fever of other and unknown origin", 
                       "Pain, not elsewhere classified", 
                       "Myalgia", 
                       "Pain, not elsewhere classified", 
                       "NA", 
                       "Excessive, freguent and irrelgular menstruation"),
  opengwas_id = c("finn-b-R18_FEVER_OTHER_UNKNO_ORIGIN",
                  "finn-b-R18_PAIN_NOT_ELSEW_CLASSIFIED",
                  "finn-b-M13_MYALGIA",
                  "finn-b-R18_PAIN_NOT_ELSEW_CLASSIFIED",
                  "NA",
                  "finn-b-N14_MESNRUIRREG"),
  sample_size = c("NR",
                  "NR",
                  "NR",
                  "NR",
                  "NA",
                  "NR"),
  ncases = c("9745",
             "3811",
             "3021",
             "3811",
             "NA",
             "17873"),
  ncontrols = c("172999",
                "172999",
                "167641",
                "172999",
                "NA",
                "68969"),
  consortium = c("FinnGen",
                 "FinnGen",
                 "FinnGen",
                 "FinnGen",
                 "NA", 
                 "FinnGen"),
  outcome_type = c("Binary",
                   "Binary",
                   "Binary",
                   "Binary",
                   "NA",
                   "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Imidapril
#######################################################

# Specify drug
drug <- "Imidapril"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[1]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Hypertension"),
  opengwas_id = c("ieu-b-5144"),
  sample_size = c("462826"),
  ncases = c("133680"),
  ncontrols = c("329146"),
  consortium = c("UK Biobank"),
  outcome_type = c("Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Irbesartan
#######################################################

# Specify drug
drug <- "Irbesartan"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[2]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Diabetic nephropathy", "Hypertension"),
  opengwas_id = c("finn-b-DM_NEPHROPATHY", "ieu-b-5144"),
  sample_size = c("NR", "462826"),
  ncases = c("3283", "133680"),
  ncontrols = c("210463", "329146"),
  consortium = c("FinnGen", "UK Biobank"),
  outcome_type = c("Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Isotretinoin
#######################################################

# Specify drug
drug <- "Isotretinoin"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[2]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Acne vulgaris", "Acne"),
  opengwas_id = c("finn-b-L12_ACNE_VULGARIS", "finn-b-L12_ACNE"),
  sample_size = c("NR", "NR"),
  ncases = c("1092", "1299"),
  ncontrols = c("211139", "211139"),
  consortium = c("FinnGen", "FinnGen"),
  outcome_type = c("Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Lenalidomide
#######################################################

# Specify drug
drug <- "Lenalidomide"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[2]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Myeloproliferative neoplasms", "NA"),
  opengwas_id = c("ebi-a-GCST90000032", "NA"),
  sample_size = c("408241", "NA"),
  ncases = c("1086", "NA"),
  ncontrols = c("407155", "NA"),
  consortium = c("EBI", "NA"),
  outcome_type = c("Binary", "NA")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Lisinopril
#######################################################

# Specify drug
drug <- "Lisinopril"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[2]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Hypertension", "Myocardial infarction"),
  opengwas_id = c("ieu-b-5144", "ebi-a-GCST90018877"),
  sample_size = c("462826", "461823"),
  ncases = c("133680", "20917"),
  ncontrols = c("329146", "440906"),
  consortium = c("UK Biobank", "EBI"),
  outcome_type = c("Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Lithium carbonate
#######################################################

# Specify drug
drug <- "Lithium carbonate"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == gsub(" ", "_", drug)) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[3]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Bipolar disorder bip2021", "NA", "Manic episode"),
  opengwas_id = c("ieu-b-5110", "NA", "finn-b-F5_MANIA"),
  sample_size = c("413466", "NA", "NR"),
  ncases = c("41917", "NA", "631"),
  ncontrols = c("371549", "NA", "192220"),
  consortium = c("PGC Bipolar Disorder Working Group of the Psychiatric Genomics Consortium", "NA", "FinnGen"),
  outcome_type = c("Binary", "NA", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Losartan
#######################################################

# Specify drug
drug <- "Losartan"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == gsub(" ", "_", drug)) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[3]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Ischemic stroke", "Hypertension", "Diabetic nephropathy"),
  opengwas_id = c("ebi-a-GCST006908", "ieu-b-5144", "finn-b-DM_NEPHROPATHY"),
  sample_size = c("440328", "462826", "NR"),
  ncases = c("34217", "133680", "3283"),
  ncontrols = c("406111", "329146", "210463"),
  consortium = c("EBI", "UK Biobank", "FinnGen"),
  outcome_type = c("Binary", "Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Mefenamic acid
#######################################################

# Specify drug
drug <- "Mefenamic acid"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == gsub(" ", "_", drug)) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[3]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Pain, not elsewhere classified", "Excessive, freguent and irrelgular menstruation", "Myalgia"),
  opengwas_id = c("finn-b-R18_PAIN_NOT_ELSEW_CLASSIFIED", "finn-b-N14_MESNRUIRREG", "finn-b-M13_MYALGIA"),
  sample_size = c("NR", "NR", "NR"),
  ncases = c("3811", "17873", "3021"),
  ncontrols = c("172999", "68969", "167641"),
  consortium = c("FinnGen", "FinnGen", "FinnGen"),
  outcome_type = c("Binary", "Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Meloxicam
#######################################################

# Specify drug
drug <- "Meloxicam"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == gsub(" ", "_", drug)) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[1]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Pain, not elsewhere classified"),
  opengwas_id = c("finn-b-R18_PAIN_NOT_ELSEW_CLASSIFIED"),
  sample_size = c("NR"),
  ncases = c("3811"),
  ncontrols = c("172999"),
  consortium = c("FinnGen"),
  outcome_type = c("Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Methotrexate
#######################################################

drug <- "Methotrexate"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[8]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Psoriasis vulgaris", "Mature T/NK-cell lymphomas", "Rheumatoid arthritis",
                       "Juvenile rheumatoid arthritis", "Psoriasis vulgaris", "Head and neck cancer",
                       "Malignant lymphoma", "Meningitis"),
  opengwas_id = c("ebi-a-GCST90018907", "finn-b-CD2_TNK_LYMPHOMA", "ebi-a-GCST90018910",
                  "ebi-a-GCST90018873", "ebi-a-GCST90018907", "ieu-b-4912", "ebi-a-GCST90018878",
                  "finn-b-MENINGITIS"),
  sample_size = c("483174", "NR", "417256", "409217", "483174", "373122", "490803",
                  "NR"),
  ncases = c("5072", "150", "8255", "216", "5072", "1106", "3546", "770"),
  ncontrols = c("478102", "218642", "409001", "409001", "478102", "372016", "487257", "216967"),
  consortium = c("EBI", "FinnGen", "EBI", "EBI", "EBI", "UK Biobank", "EBI", "FinnGen"),
  outcome_type = c("Binary", "Binary", "Binary", "Binary", "Binary", "Binary", "Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Modafinil
#######################################################

# Specify drug
drug <- "Modafinil"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == gsub(" ", "_", drug)) %>% 
  filter(`Approval Level` == "Prescription")

# No prescription uses

#######################################################
# Exploring openGWAS outcome datasets - Naproxen
#######################################################

# Specify drug
drug <- "Naproxen"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == gsub(" ", "_", drug)) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[2]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Pain (limb, back, neck, head abdominally)", "Excessive, freguent and irrelgular menstruation"),
  opengwas_id = c("finn-b-PAIN", "finn-b-N14_MESNRUIRREG"),
  sample_size = c("NR", "NR"),
  ncases = c("87242", "17873"),
  ncontrols = c("131127", "68969"),
  consortium = c("FinnGen", "FinnGen"),
  outcome_type = c("Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Olmesartan
#######################################################

# Specify drug
drug <- "Olmesartan"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == gsub(" ", "_", drug)) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[1]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Hypertension"),
  opengwas_id = c("ieu-b-5144"),
  sample_size = c("462826"),
  ncases = c("133680"),
  ncontrols = c("329146"),
  consortium = c("UK Biobank"),
  outcome_type = c("Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Perindopril
#######################################################

# Specify drug
drug <- "Perindopril"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == gsub(" ", "_", drug)) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[4]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Diagnoses - secondary ICD10: I10 Essential (primary) hypertension",
                       "Heart failure", "All-cause Heart Failure", "Heart failure"),
  opengwas_id = c("ukb-b-12493", "ebi-a-GCST009541", "finn-b-I9_HEARTFAIL_ALLCAUSE", "ebi-a-GCST009541"),
  sample_size = c("463010", "977323", "NR", "977323"),
  ncases = c("54358", "47309", "23397", "47309"),
  ncontrols = c("408652", "930014", "194811", "930014"),
  consortium = c("MRC-IEU", "EBI", "FinnGen", "EBI"),
  outcome_type = c("Binary", "Binary", "Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Pomalidomide
#######################################################

# Specify drug
drug <- "Pomalidomide"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == gsub(" ", "_", drug)) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[1]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# No matches in openGWAS outcomes

#######################################################
# Exploring openGWAS outcome datasets - Quinapril
#######################################################

# Specify drug
drug <- "Quinapril"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == gsub(" ", "_", drug)) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[1]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Hypertension"),
  opengwas_id = c("ieu-b-5144"),
  sample_size = c("462826"),
  ncases = c("133680"),
  ncontrols = c("329146"),
  consortium = c("UK Biobank"),
  outcome_type = c("Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Ramipril
#######################################################

# Specify drug
drug <- "Ramipril"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == gsub(" ", "_", drug)) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[5]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Hypertension", "Myocardial infarction", "Cardiovascular diseases", "Stroke", "Heart failure"),
  opengwas_id = c("ieu-b-5144", "ebi-a-GCST90018877", "finn-b-I9_CVD", "ebi-a-GCST005838", "ebi-a-GCST009541"),
  sample_size = c("462826", "461823", "NR", "446696", "977323"),
  ncases = c("133680", "20917", "111108", "40585", "47309"),
  ncontrols = c("329146", "440906", "107684", "406111", "930014"),
  consortium = c("UK Biobank", "EBI", "FinnGen", "EBI", "EBI"),
  outcome_type = c("Binary", "Binary", "Binary", "Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Telmisartan
#######################################################

# Specify drug
drug <- "Telmisartan"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == gsub(" ", "_", drug)) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[2]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

# Table containing the chosen study, openGWAS id and sample size

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Hypertension", "Cardiovascular diseases"),
  opengwas_id = c("ieu-b-5144", "finn-b-I9_CVD"),
  sample_size = c("462826", "NR"),
  ncases = c("133680", "111108"),
  ncontrols = c("329146", "107684"),
  consortium = c("UK Biobank", "FinnGen"),
  outcome_type = c("Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Thalidomide
#######################################################

drug <- "Thalidomide"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[3]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Erythema nodosum", "Erythema nodosum", "Erythema nodosum"),
  opengwas_id = c("finn-b-L12_ERYTHEMANODOSUM", "finn-b-L12_ERYTHEMANODOSUM", "finn-b-L12_ERYTHEMANODOSUM"),
  sample_size = c("NA", "NA", "NA"),
  ncases = c("433", "433", "433"),
  ncontrols = c("212464", "212464", "212464"),
  consortium = c("FinnGen", "FinnGen", "FinnGen"),
  outcome_type = c("Binary", "Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Topiramate
#######################################################

drug <- "Topiramate"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[5]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Migraine", "Epilepsy", "Focal epilepsy", "Epilepsy", "Migraine"),
  opengwas_id = c("finn-b-G6_MIGRAINE", "finn-b-G6_EPLEPSY", "finn-b-FE", "finn-b-G6_EPLEPSY", "finn-b-G6_MIGRAINE"),
  sample_size = c("NR", "NR", "NR", "NR", "NR"),
  ncases = c("8547", "6260", "929", "6260", "8547"),
  ncontrols = c("176107", "176107", "212532", "176107", "176107"),
  consortium = c("FinnGen", "FinnGen", "FinnGen", "FinnGen", "FinnGen"),
  outcome_type = c("Binary", "Binary", "Binary", "Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Trandolapril
#######################################################

drug <- "Trandolapril"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[1]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Hypertension"),
  opengwas_id = c("ieu-b-5144"),
  sample_size = c("462826"),
  ncases = c("133680"),
  ncontrols = c("329146"),
  consortium = c("UK Biobank"),
  outcome_type = c("Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Tretinoin
#######################################################

drug <- "Tretinoin"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[2]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Acne vulgaris", "Acute myeloid leukaemia"),
  opengwas_id = c("finn-b-L12_ACNE_VULGARIS", "finn-b-C3_AML"),
  sample_size = c("NR", "NR"),
  ncases = c("1092","90"),
  ncontrols = c("211139", "218702"),
  consortium = c("FinnGen", "FinnGen"),
  outcome_type = c("Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Valproic acid
#######################################################

drug <- "Valproic acid"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[4]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Epilepsy", "Manic episode", "Epilepsy", "Diagnoses - main ICD10: G43 Migraine"),
  opengwas_id = c("ebi-a-GCST90018840", "finn-b-F5_MANIA", "ebi-a-GCST90018840", "ukb-d-G43"),
  sample_size = c("458310", "NR", "458310", "361194"),
  ncases = c("4382", "631", "4382", "1072"),
  ncontrols = c("453928", "192220", "453928", "360122"),
  consortium = c("EBI", "FinnGen", "EBI", "MRC-IEU"),
  outcome_type = c("Binary", "Binary", "Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Valsartan
#######################################################

drug <- "Valsartan"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[5]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Hypertension", "Death due to cardiac causes", "Heart failure", "Heart failure", "Heart failure"),
  opengwas_id = c("ieu-b-5144", "finn-b-I9_K_CARDIAC", "ebi-a-GCST009541", "ebi-a-GCST009541", "ebi-a-GCST009541"),
  sample_size = c("462826", "NR", "977323", "977323", "977323"),
  ncases = c("133680", "7563", "47309", "47309", "47309"),
  ncontrols = c("329146", "211229", "930014", "930014", "930014"),
  consortium = c("UK Biobank", "FinnGen", "EBI", "EBI", "EBI"),
  outcome_type = c("Binary", "Binary", "Binary", "Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

#######################################################
# Exploring openGWAS outcome datasets - Warfarin
#######################################################

drug <- "Warfarin"

# Reading in drug indications
drug_indications <- indications %>% 
  filter(Drug == drug) %>% 
  filter(`Approval Level` == "Prescription")

# One by one going through indications and choosing openGWAS outcome, based on:
#   - Term similarity
#   - Sample size

ind <- unique(drug_indications$Indication)[3]

ind_res <- mapped_outcomes %>% 
  filter(indication == gsub(" ", "_", ind))

studies <- data.table(
  indication = unique(drug_indications$Indication),
  opengwas_outcome = c("Non-cancer illness code, self-reported: deep venous thrombosis (dvt)", "Venous thromboembolism",
                       "Pulmonary embolism"),
  opengwas_id = c("ukb-b-12040", "ukb-d-I9_VTE", "finn-b-I9_PULMEMB"),
  sample_size = c("462933", "361194", "NR"),
  ncases = c("9241", "4620", "4185"),
  ncontrols = c("453692", "356574", "214228"),
  consortium = c("MRC-IEU", "MRC-IEU", "FinnGen"),
  outcome_type = c("Binary", "Binary", "Binary")
)

# Save

output_file <- file.path(output_dir, paste(gsub(" ", "_", drug), "chosen_openGWAS_outcomes.csv", sep = "_"))
fwrite(studies, output_file)

