# Teratogenicity_Prediction_Route_A
Using genetic data to predict drug teratogenicity via intended interaction between the drug and it's targets (Route A)

# Config.env setup
Update path name for data and results to direct the scripts to appropriate directories using config-template.env

# Virtual Environment

Before running a python script, enter the virtual environment.
To enter the virtual environment, run the following command in the terminal:
source venv/bin/activate

# Data Structure

```
project-dir/
│
├── data/
│   ├── raw/
│   │   ├── drug_extracted_data/
│   │   ├── reported_outcomes/
│   │   │   ├── SIDER_data/
│   │   │   ├── OnSIDES_data/
│   │   │   ├── FAERS/
│   │   ├── predicted_outcomes/
│   │   │   ├── Drug_Bank/
│   │   │   ├── OMIM_data/
│   │
│   ├── interim/
│   │   ├── reported_outcomes/
│   │   │   ├── SIDER_outcomes/
│   │   │   ├── OnSIDES_outcomes/
│   │   │   ├── Bumps_outcomes/
│   │   │   ├── FAERS_outcomes/
│   │   ├── predicted_outcomes/
│   │   │   ├── Drug_Bank_targets/
│   │   │   ├── OMIM_outcomes/
│   │   ├── ontology_mapping/
│   │   │   ├── input_data/
│   │   │   ├── output_data/
│   │   │   │   ├── Qwen/
│   │   │   │   ├── DeepSeek/
│   │
│   ├── processed/
│
├── results/
│   ├── docs/
│
├── scripts/       
```

# Data Sources

## Teratogenic Drugs of Interest

Do I want to put my list of drugs of interest here?

## Reported Drug Outcomes

### SIDER

http://sideeffects.embl.de/download/

Files: 
drug_names.tsv 
meddra_freq.tsv

### OnSIDES

https://onsidesdb.org/download
Version - V3.1.0

### Bumps

https://www.medicinesinpregnancy.org/leaflets-a-z/?letter=A

Side Effects were extracted manually into the columns: 
"DrugBank Name" 
"Bumps Predicted Side Effects"

### FAERS

https://fis.fda.gov/sense/app/95239e26-e0be-42d9-a960-9a5f7f1c25ee/sheet/33a0f68e-845c-48e2-bc81-8141c6aaf772/state/analysis

1. Search drug of interest (in search by product)
2. Download Case count by reaction in Demographics section (as Data)
3. Reaction section -> Select “Pregnancy, Puerperium and Perinatal Conditions” and “Congenital, Familial and Genetic Disorders”      Reaction Groups
4.	Download table as Data (making sure to expand the sections to list out all the individual reactions)

File naming:
All outcomes: FAERS/{drug_name}/Case_Count_all_Reaction_{drug_name}.csv
Congenital outcomes: FAERS/{drug_name}/Case_Count_Preg_Cong_{drug_name}.csv

## Predicting Outcomes

### DrugBank

https://go.drugbank.com/releases/latest?_gl=1*90rq5s*_up*MQ..*_ga*MjYzMDMxMjQ4LjE3NjM2NDc4NjQ.*_ga_DDLJ7EEV9M*czE3NjM2NDc4NjQkbzEkZzAkdDE3NjM2NDc4NjQkajYwJGwwJGgw

All drugs
Version - V5.1.15

Protein Identifiers -> All
Version - V5.1.15

### OMIM

https://www.omim.org/

1. Search Target
2. Allelic Variants
3. Table View
4. Save as tsv
5. Add target name to end of file name

## BioBert Models

Include test file with positive and negative controls?

Models have been sourced from: 
 - https://sbert.net/docs/sentence_transformer/pretrained_models.html or
 - https://huggingface.co/models?p=1&sort=trending&search=PubMedBERT

## LLM Models

Models were sources from:
 - https://ollama.com/search