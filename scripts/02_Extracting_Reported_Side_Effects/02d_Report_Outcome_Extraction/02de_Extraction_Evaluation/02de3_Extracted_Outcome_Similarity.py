"""
Script to calculate text embeddings for extracted outcomes by LLM and manually using Sentence Transformers and comparing their 
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
base_dir = Path(os.getenv("interimdatadir"), "reported_outcomes/Extraction_Evaluation")

# Defining Model 
model_name = "UMCU/SapBERT-from-PubMedBERT-fulltext_bf16" # Lightweight version of SapBERT (16-bit) as a sentence-transformer, built on PubMedBERT. Useful for UMLS-concept embedding with lower memory.

# # Initialize the model
model = SentenceTransformer(model_name)

# # Initialize input dataset 
sources = ["Reprotox", "TERIS", "UKTIS"]

# Loop through all sources and process each drug
for source in sources:

    print(f"\n{'='*60}")
    print(f"Processing source: {source}")
    print(f"{'='*60}")

    # Define input and output directories
    input_dir = base_dir / source / "2_mapped_extractions"
    output_dir = base_dir / source / "3_similarity_results"

    for drug_file in os.listdir(input_dir):

        input_file_path = input_dir / drug_file
        output_file_path = output_dir / drug_file.replace("mapped_extractions.csv", "similarity.csv")

        # Skip Extraction Success file
        if drug_file == "extraction_success.csv":
            continue

        drug_name = drug_file.replace(".csv", "")
        drug_name = drug_name.replace("_mapped_extractions", "")

        print(f"\n{'='*60}")
        print(f"Processing drug: {drug_name}")
        print(f"{'='*60}")

        mapping_df = pd.read_csv(input_file_path)[["Outcome_LLM", "Outcome_manual", "id_llm", "id_manual"]]

        # LLM Extracted terms
        llm_outcomes = mapping_df["Outcome_LLM"].dropna().unique().tolist()

        # Calculate embeddings
        print("\nCalculating embeddings for LLM extracted terms")
        llm_embeddings = model.encode(llm_outcomes)

        # Display results
        print(f"Number of terms: {len(llm_outcomes)}")
        print(f"Embedding dimension: {llm_embeddings.shape[1]}")
        print(f"Embeddings shape: {llm_embeddings.shape}")

        # Manual Extracted terms
        manual_outcomes = mapping_df["Outcome_manual"].dropna().unique().tolist()

        # Calculate embeddings
        print("\nCalculating embeddings for manually extracted terms")
        manual_embeddings = model.encode(manual_outcomes)

        # Display results
        print(f"Number of terms: {len(manual_outcomes)}")
        print(f"Embedding dimension: {manual_embeddings.shape[1]}")
        print(f"Embeddings shape: {manual_embeddings.shape}")

        # Calculate cosine similarity between terms
        print("\n" + "=" * 60)
        print("Similarity Matrix (cosine similarity):")
        print("=" * 60)

        similarity_matrix = cos_sim(llm_embeddings, manual_embeddings)

        # Display similarity between first few terms
        for i in range(min(3, len(llm_outcomes))):
            for j in range(min(3, len(manual_outcomes))):
                print(
                    f"Similarity between '{llm_outcomes[i]}' and '{manual_outcomes[j]}': {similarity_matrix[i][j]:.4f}"
                    )
           
        # Convert to DataFrame with row and column names
        similarity_df = pd.DataFrame(
            similarity_matrix.cpu().numpy() if hasattr(similarity_matrix, 'cpu') else similarity_matrix,
            index=llm_outcomes,
            columns=manual_outcomes
            )
    
        # Save to CSV
        similarity_df.to_csv(output_file_path)
    
        print(f"\nSimilarity matrix saved to: {output_file_path}")

print(f"\n{'='*60}")
print("All drugs processed successfully!")
print(f"{'='*60}")