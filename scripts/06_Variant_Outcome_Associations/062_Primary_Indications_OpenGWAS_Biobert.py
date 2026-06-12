"""
Script to calculate text embeddings for DrugBank indications and OpenGWAS outcomes using Sentence Transformers and comparing their 
cosine similarity.
"""
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

input_dir = base_dir / "predicted_outcomes/Primary_Indication_Mapping/Biobert_input/"
output_dir = base_dir / "predicted_outcomes/Primary_Indication_Mapping/Biobert_output/"

# Defining Model 
model_name = "UMCU/SapBERT-from-PubMedBERT-fulltext_bf16" # Lightweight version of SapBERT (16-bit) as a sentence-transformer, built on PubMedBERT. Useful for UMLS-concept embedding with lower memory.

# Models have been sourced from: 
#   https://sbert.net/docs/sentence_transformer/pretrained_models.html or
#   https://huggingface.co/models?p=1&sort=trending&search=PubMedBERT

# Initialize the model
model = SentenceTransformer(model_name)

###
# Drug primary indications - DrugBank
###
indication_file = f"{input_dir}/primary_indications.csv"
print(f"Reading indication file: {indication_file}")

indication_terms = pd.read_csv(indication_file)["Outcome"].tolist()

# Calculate embeddings
print("\nCalculating embeddings for indication terms...")
indication_embeddings = model.encode(indication_terms)

# Display results
print(f"Number of terms: {len(indication_terms)}")
print(f"Embedding dimension: {indication_embeddings.shape[1]}")
print(f"Embeddings shape: {indication_embeddings.shape}")

# Save embeddings to file - uncomment if you want to save predicted embeddings
#indication_output_file = f"{output_dir}/primary_indication_embeddings.csv"
#np.savetxt(indication_output_file, indication_embeddings, delimiter=",", fmt="%.6f")
#print(f"\nEmbeddings saved to: {indication_output_file}")

###
# OpenGWAS outcomes
###
opengwas_file = f"{input_dir}/openGWAS_outcomes.csv"
print(f"Reading OpenGWAS file: {opengwas_file}")

opengwas_terms = pd.read_csv(opengwas_file)["Outcome"].tolist()

# Calculate embeddings
print("\n" + "=" * 60)
print("Calculating embeddings for openGWAS terms...")
opengwas_embeddings = model.encode(opengwas_terms)

# Display results
print(f"Number of terms: {len(opengwas_terms)}")
print(f"Embedding dimension: {opengwas_embeddings.shape[1]}")
print(f"Embeddings shape: {opengwas_embeddings.shape}")
    
# Save embeddings to file - uncomment is you want to save observed embeddings
#opengwas_output_file = f"{output_dir}/openGWAS_outcomes_embeddings.csv"
#np.savetxt(opengwas_output_file, opengwas_embeddings, delimiter=",", fmt="%.6f")
#print(f"\nEmbeddings saved to: {opengwas_output_file}")

###
# Calculate similarity between terms
###
print("\n" + "=" * 60)
print("Similarity Matrix (cosine similarity):")
print("=" * 60)
    
similarity_matrix = cos_sim(indication_embeddings, opengwas_embeddings)
    
# Display similarity between first few terms
for i in range(min(3, len(indication_terms))):
    for j in range(min(3, len(opengwas_terms))):
        print(
            f"Similarity between '{indication_terms[i]}' and '{opengwas_terms[j]}': {similarity_matrix[i][j]:.4f}"
        )
    
# Convert to DataFrame with row and column names
similarity_df = pd.DataFrame(
    similarity_matrix.cpu().numpy() if hasattr(similarity_matrix, 'cpu') else similarity_matrix,
    index=indication_terms,
    columns=opengwas_terms
)

###  
# Save to CSV
###
similarity_output_file = f"{output_dir}/indication_opengwas_similarity_matrix.csv"
similarity_df.to_csv(similarity_output_file)
    
print(f"\nSimilarity matrix saved to: {similarity_output_file}")
