from pathlib import Path
from dotenv import load_dotenv
from sentence_transformers import SentenceTransformer
import numpy as np
import pandas as pd
from sentence_transformers.util import cos_sim
import os

# Load environment variables
load_dotenv("config.env")
base_dir = Path(os.getenv("interimdatadir"))

mapped_file = base_dir / "predicted_outcomes/Primary_Indication_Mapping/Qwen_output/Qwen_output_combined.json"

print(f"Reading mapped file: {mapped_file}")

df = pd.read_json(mapped_file)



#json_normalized = pd.json_normalize(df)
#json_normalized.to_csv(base_dir / "predicted_outcomes/Primary_Indication_Mapping/Qwen_output_combined.csv", index=False)


