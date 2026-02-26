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
│   │
│   ├── interim/
│   │   ├── reported_outcomes/
│   │   │   ├── SIDER_outcomes/
│   │   │   ├── OnSIDES_outcomes/
│   │   │   ├── Bumps_outcomes/
│   │
│   ├── processed/
│
├── results/
│
├── scripts/       
```

# Data Sources

## Reported Drug Outcomes

### SIDER

### OnSIDES

### Bumps

### FAERS

# BioBert Models - Link
Include test file with positive and negative controls