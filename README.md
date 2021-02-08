
# Introduction

Code for the paper: Incorporating real-world evidence into the development of patient blood glucose prediction algorithms for the ICU. 

# Access to MIMIC-III database

See https://mimic.physionet.org/gettingstarted/access/

# Dataset creation 

## SQL 

For this analysis the MIMIC-III database was stored locally using a postreSQL RDBMS. 
There are details [here](https://mimic.physionet.org/tutorials/install-mimic-locally-ubuntu/) on how to set it up. As a first step towards creating an analysis dataset a series of views were created. The scripts for these views are found in the `./sql` folder. 

The code in [https://github.com/MIT-LCP/mimic-code](https://github.com/MIT-LCP/mimic-code) formed the basis for much of the SQL code used in the project. 

The following table or views were output as CSVs to a folder `./data/raw/VIEWNAME`:

* icustays  
* pt_icu_outcome
* admissions
* patients  
* diabetes  
* pv_mechvent  
* pv_nutrition  
* ent_nutrition  
* vasopressors  
* output_hourly 
* glycaemic_analysis_hr  

The process for saving to CSV will differ between database management software. I used [DBeaver](https://dbeaver.io/).

## R 

The CSVs created from the database views were combined into an analysis file using 
the code in `scripts/create-dataset/static-dataset.R` and `scripts/create-dataset/analysis-dataset-discrete-time.R`. 

The test-train split was performed using the file `scripts/create-dataset/test-train-discrete-time.R`. 

# Model creation and evaluation

Since there was a bug in the R version of Catboost 0.24.2 preventing use of the 
'UncertaintyRegression' loss function I switched to using the Python version through R's reticulate package. Installation details for reticulate can be found [here](https://rstudio.github.io/reticulate/). 

The experiments in determining model parameters are found in `scripts/modeling/experiments.R`. 

The file `scripts/modeling/models.R` fits the final models as evaulated in the paper. The file `scripts/modeling/evaluation.R` outputs the evaluation metrics. 

# Table 1 

The numbers in the table can be found by running the file `./scripts/table1.R`

# Appendices

The hypoglycaemia prediction model code is found in `./scripts/appendices/hypoglycaemia-discrete-time-models.R`
