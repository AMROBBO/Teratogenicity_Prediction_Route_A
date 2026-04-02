"""
Script to calculate text embeddings for predicted and observed terms using Sentence Transformers and comparing their 
cosine similarity.

Predicted and Observed outcomes must have column "Outcome" containing the terms.
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

# Defining Model 
#   - Uncomment the model you want to use for embedding calculation, or choose your own from HuggingFace or Sentence Transformers library.
#model_name = "all-mpnet-base-v2" # 	All-round model tuned for many use-cases. Trained on a large and diverse dataset of over 1 billion training pairs.
#model_name = "all-MiniLM-L6-v2" # All-round model tuned for many use-cases. Trained on a large and diverse dataset of over 1 billion training pairs.
#model_name = "pritamdeka/S-PubMedBert-MS-MARCO"  # Biomedical domain-specific model, trained on PubMed articles and MS MARCO dataset.
#model_name = "kamalkraj/BioSimCSE-BioLinkBERT-BASE" # BioSimCSE model using the BioLinkBERT base model. It’s trained via contrastive learning on biomedical text. Very good for semantic similarity, sentence-level embedding in biomedical domain.
model_name = "UMCU/SapBERT-from-PubMedBERT-fulltext_bf16" # Lightweight version of SapBERT (16-bit) as a sentence-transformer, built on PubMedBERT. Useful for UMLS-concept embedding with lower memory.

# Models have been sourced from: 
#   https://sbert.net/docs/sentence_transformer/pretrained_models.html or
#   https://huggingface.co/models?p=1&sort=trending&search=PubMedBERT

# Initialize the model
model = SentenceTransformer(model_name)

# Initialize input dataset 
#   - Ensure only one predicted and one observed dataset is uncommented at a time
predicted_dataset = "omim"
#observed_dataset = "onsides"
#observed_dataset = "bumps"
#observed_dataset = "faers_all"
observed_dataset = "faers_cong"

# Base directories
input_base_dir = base_dir / "ontology_mapping/input_data"
output_base_dir = base_dir / "ontology_mapping/output_data"

# Loop through all folders in Input_data
for drug_name in os.listdir(input_base_dir):
    # Skip macOS metadata files
    if drug_name.endswith(".DS_Store"):
        continue
    
    input_dir = f"{input_base_dir}/{drug_name}"
    
    # Skip if not a directory
    if not os.path.isdir(input_dir):
        continue
    
    output_dir = f"{output_base_dir}/{model_name}/{drug_name}"
    
    # Make output directory if it doesn't exist
    if not os.path.isdir(output_dir):
        os.makedirs(output_dir)
    
    print(f"\n{'='*60}")
    print(f"Processing drug: {drug_name}")
    print(f"{'='*60}")
    
    # Find OMIM file
    predicted_file = None
    observed_file = None
    
    for filename in os.listdir(input_dir):
        if filename.endswith(f"{predicted_dataset}.csv"):
            predicted_file = f"{input_dir}/{filename}"
        elif filename.endswith(f"{observed_dataset}.csv"):
            observed_file = f"{input_dir}/{filename}"
    
    # Skip if files not found
    if predicted_file is None or observed_file is None:
        print(f"Warning: Missing Predicted or Observed file for {drug_name}. Skipping...")
        continue
    
    # Predicted terms
    predicted_terms = pd.read_csv(predicted_file)["Outcome"].tolist()
    
    # Calculate embeddings
    print("\nCalculating embeddings for predicted terms...")
    predicted_embeddings = model.encode(predicted_terms)
    
    # Display results
    print(f"Number of terms: {len(predicted_terms)}")
    print(f"Embedding dimension: {predicted_embeddings.shape[1]}")
    print(f"Embeddings shape: {predicted_embeddings.shape}")
        
    # Save embeddings to file - uncomment is you want to save predicted embeddings
    #predicted_output_file = f"{output_dir}/{drug_name}_{predicted_dataset}_embeddings.csv"
    #np.savetxt(predicted_output_file, predicted_embeddings, delimiter=",", fmt="%.6f")
    #print(f"\nEmbeddings saved to: {predicted_output_file}")
    
    # Observed terms
    observed_terms = pd.read_csv(observed_file)["Outcome"].tolist()
    
    # Calculate embeddings
    print("\n" + "=" * 60)
    print("Calculating embeddings for Observed terms...")
    observed_embeddings = model.encode(observed_terms)
    
    # Display results
    print(f"Number of terms: {len(observed_terms)}")
    print(f"Embedding dimension: {observed_embeddings.shape[1]}")
    print(f"Embeddings shape: {observed_embeddings.shape}")
    
    # Save embeddings to file - uncomment is you want to save observed embeddings
    #observed_output_file = f"{output_dir}/{drug_name}_{observed_dataset}_embeddings.csv"
    #np.savetxt(observed_output_file, observed_embeddings, delimiter=",", fmt="%.6f")
    #print(f"\nEmbeddings saved to: {observed_output_file}")
    
    # Calculate similarity between terms
    print("\n" + "=" * 60)
    print("Similarity Matrix (cosine similarity):")
    print("=" * 60)
    
    similarity_matrix = cos_sim(predicted_embeddings, observed_embeddings)
    
    # Display similarity between first few terms
    for i in range(min(3, len(predicted_terms))):
        for j in range(min(3, len(observed_terms))):
            print(
                f"Similarity between '{predicted_terms[i]}' and '{observed_terms[j]}': {similarity_matrix[i][j]:.4f}"
            )
    
    # Convert to DataFrame with row and column names
    similarity_df = pd.DataFrame(
        similarity_matrix.cpu().numpy() if hasattr(similarity_matrix, 'cpu') else similarity_matrix,
        index=predicted_terms,
        columns=observed_terms
    )
    
    # Save to CSV
    similarity_output_file = f"{output_dir}/{drug_name}_{predicted_dataset}_{observed_dataset}_similarity_matrix.csv"
    similarity_df.to_csv(similarity_output_file)
    
    print(f"\nSimilarity matrix saved to: {similarity_output_file}")

print(f"\n{'='*60}")
print("All drugs processed successfully!")
print(f"{'='*60}")