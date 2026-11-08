# Cluster Analysis on Company Financials

**Machine Learning in Finance – University of Deusto**

## About the Project

I developed this project for my Machine Learning in Finance course at the University of Deusto.

The aim was to analyse financial data from 340 publicly traded companies and identify groups of firms with similar financial characteristics. The dataset included variables such as stock returns, price volatility, profitability, liquidity and valuation ratios.

Rather than focusing only on the final clusters, I wanted to compare different methods, understand how they behaved with the data and see whether the resulting groups had meaningful financial characteristics.

## Methodology

I carried out the analysis in R, mainly using two clustering methods: K-means and hierarchical clustering with Ward's method.

I started by exploring and preparing the dataset, selecting relevant variables, applying logarithmic transformations and standardising the data. I also examined potential outliers and tested how removing certain observations affected the results.

I then compared different numbers of clusters and evaluated the methods using Silhouette scores, Dunn indices and connectivity. I used PCA plots to visualise the groups and better understand their structure.

To explore the differences between clusters, I also performed ANOVA tests and logistic regression, looking at which financial variables were most relevant and how the groups related to different industry sectors.

The R script contains the full process, including the initial exploration, alternative approaches, comparisons and methods that I eventually discarded. I kept these steps because they were an important part of understanding the data and deciding which approach worked best.

## Results

Hierarchical clustering using Ward's method produced the best overall results, particularly after removing a group of outliers. The selected solution identified four clusters and achieved a Silhouette score of 0.5854.

The clusters showed different financial profiles. Some companies were characterised by stronger profitability and lower volatility, while others showed higher valuations, greater market risk or weaker financial performance.

I also compared the clusters with industry sectors to see whether companies from similar industries tended to share financial characteristics.

Overall, the project helped me understand how different clustering techniques can produce different results from the same financial dataset, and why data preparation and model evaluation are important when interpreting those results.

## Files

- **`clustering_analysis.R`** — Complete R script, including data preparation, exploratory analysis, clustering methods, statistical tests and visualisations.
- **`companies_financials.csv`** — Dataset containing financial information for 340 publicly traded companies.
- **`FinancialClusteringReport.pdf`** — Full academic report with the methodology, results, graphs and conclusions.

## Tools and Libraries

The analysis was developed in **R using Posit Cloud**.

Main libraries used:

- `ggplot2`, `factoextra`, `ggrepel` — Data visualisation and clustering plots.
- `dplyr`, `tidyr`, `purrr` — Data manipulation and preparation.
- `cluster`, `clValid`, `fpc` — Clustering and model evaluation.
- `psych`, `clue`, `cowplot` — Statistical analysis and additional visualisations.

To explore the project, open `clustering_analysis.R` in RStudio or Posit Cloud, install the required packages and keep `companies_financials.csv` in the same working directory.
