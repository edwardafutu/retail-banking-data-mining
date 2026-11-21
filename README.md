# Association Rule Mining and Customer Value Segmentation in Retail Banking

## Project Overview

This group project used unsupervised data mining techniques to identify product cross-selling opportunities and segment retail banking customers into actionable value tiers. Using a simulated portfolio of 100,000 retail banking customers (generated with seed 757), we explored product ownership patterns and grouped customers based on the money they earn, hold, and move through the bank.

After data cleaning—which removed 300 duplicate records, imputed missing and non-positive monthly incomes, and winsorised extreme upper-tail entry errors—99,900 customer records were used for association rule mining and 100,000 unique records were used for customer value segmentation.

## Methods and Tools

- **Programming:** R
- **Data Cleaning & Manipulation:** `tidyverse`, `dplyr`, `tidyr`, `lubridate`, and `skimr`.
- **Association Rule Mining:** Apriori algorithm (`arules` and `arulesViz`) with market-basket encoding and consequent restriction to seven retail banking products.
- **Clustering & Segmentation:** k-means clustering (`k = 3`) on `log1p`-transformed and Z-score standardized features, validated with Agglomerative Hierarchical Clustering (Ward's linkage and Euclidean distance).
- **Cluster Evaluation:** Elbow method (WSS), average silhouette width (`cluster` and `factoextra`), and external risk/churn profiling.

## Key Findings

- **Cross-Selling Drivers:** Customers with a **Premium account** and **High income** (> GHS 6,000) are over twice as likely to hold an investment account (**lift = 2.09**) and 1.7 times more likely to hold a fixed deposit (**lift = 1.71**) compared to the average customer.
- **Three Value Tiers:** k-means clustering partitioned the portfolio into three balanced segments explaining **38.4%** of total variance (average silhouette width = **0.23**): **Affluent Savers** (36,242), **Affluent Transactors** (31,069), and **Mass-Market** customers (32,689).
- **Strategic Revenue Opportunities:** Affluent Savers hold high savings balances (median GHS 15,527) and carry the lowest loan default rate (**2.40%**) but hold no current account (median GHS 0), representing a primary target for cross-selling transactional products.
- **Algorithm Agreement:** Hierarchical clustering on a 2,000-customer subsample independently corroborated the three-tier customer structure.

## My Contribution

I contributed to data cleaning and preprocessing, and led the Association Rule Mining (Task 1) component of the group project, including variable discretisation, transaction basket encoding, Apriori parameter tuning, sensitivity analysis, and rule visualizations.

## Repository Contents

| File | Description |
|---|---|
| `projectScript1_association.R` | R script containing the dataset simulation (`seed = 757`), data cleaning, variable discretisation, Apriori rule mining, sensitivity analysis, and visualizations for Task 1. |
| `Association_Rule_Mining_Report.pdf` | Project report documenting the Task 1 cross-selling methodology, association rules, and business recommendations. |
| `Customer_Value_Segmentation_Report.pdf` | Project report documenting the Task 2 clustering methodology, segment profiles, and tiered banking strategies. |

## Running the Analysis

The Task 1 script (`projectScript1_association.R`) is designed to be run from top to bottom in a single R session because later sections reuse objects created in earlier steps.

The project uses R packages including `tidyverse`, `dplyr`, `tidyr`, `lubridate`, `skimr`, `arules`, `arulesViz`, `cluster`, `factoextra`, `e1071`, `ggplot2`, `knitr`, and `flextable`.

**Note:** The script generates the simulated retail banking dataset internally using `simulate_bdat602_project(n = 100000, seed = 757)`. The project does not use an external banking dataset.

## Academic Context

- **Course:** BDAT 602 – Data Mining and Applications
- **Institution:** Department of Statistics and Actuarial Science, University of Ghana
- **Academic Year:** 2025/2026
- **Project Type:** Group project
- **Group Members:** Obeng De-Graft Kofi, Edward Afutu Amoni, and Samuel Opoku

*The analysis uses simulated data. Its findings demonstrate data mining methods and should not be interpreted as empirical evidence about actual retail banking populations.*
