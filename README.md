# ABCD-Substance-Initiation
This page contains analyses from the article "Predictors of Substance Use Initiation by Early Adolescence". Data used in the present study were pulled from the Adolescent Brain Cognitive Development (ABCD) Study (5.1 data release).


## Repository Structure
Brief description of the files included in this repository:
- data_wrangling_dataset_creation.R: Import ABCD datasets, create final sample size based on inclusion and exclusion criteria, and create relevant summary scores.
- data_wrangling_model_prep.R: Split dataset into test and training, run multiple imputation for missing data, center and scale predictors, create dummy codes, and subset data for 3 models.
- model_correlations.R: Correlations among predictors selected in the present study.
- model_elastic_net.R: Stacked adaptive elastic net regression predicting substance use initiation.
- model_logistic_regression.R: Logistic regression predicting substance use initiation.
- model_random_forest.R: Random forest predicting substance use initiation.
