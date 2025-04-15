# DataScienceRBox 📊

A curated collection of R scripts for data science, covering statistical analysis, machine learning, NLP, and visualization. Designed for reproducibility and modularity.

## Structure
- **`data/`**: Raw and processed datasets.
- **`R/`**: Core R scripts organized by functionality:
    - `ml/`: Machine learning (SVM, kNN, Random Forest, etc.).
    - `stats/`: Statistical tests (ANOVA, correlations, distributions).
    - `nlp/`: Text analysis (topic modeling, similarity metrics).
    - `visualization/`: PCA, boxplots, and other plots.
- **`scripts/`**: Utility/data prep scripts.
- **`docs/`**: Documentation and notebooks.

## Key Features
- **Modular Design**: Reusable functions (e.g., `knn_find_best_k()`).
- **Documented**: Each script includes comments and usage examples.
- **Reproducible**: Seed values set for random processes.

## Usage
1. Clone the repo:
   ```bash
   git clone https://github.com/aStesh891/DataScienceRBox.git
   
2. Download GloVe 6B 50d from https://nlp.stanford.edu/data/glove.6B.zip and place `glove.6B.50d.txt` into `./data/pretrained_models/`.
