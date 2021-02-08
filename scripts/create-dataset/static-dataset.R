############################################################################################
#
# Glycaemic control in the ICU
# Join static dataset
# 
# Contents:
# a) CREATE FEATURES
# b) Make DISCRETE
#
#
#
###########################################################################################

library(data.table)
source("src/utils.R")
source("src/variable-utils.R")

# icustays
icustays <- fread("data/raw/icustays_202003131320.csv")
n_stays <- nrow(icustays)
convert_datetime(icustays,"time")

# join patient outcome
pt_outcome <- fread("data/raw/pt_icu_outcome_202003131320.csv")
convert_datetime(pt_outcome,"time")
# delete one duplicate
pt_outcome <- pt_outcome[!(icustay_id == 229922 & 
                             admittime == lubridate::as_datetime("2185-12-02 23:53:00 UTC"))]
core <- merge(icustays[,c("icustay_id","intime","outtime","dbsource","first_careunit","last_careunit",
                          "first_wardid","last_wardid")],
              pt_outcome[,c("icustay_id","subject_id","hadm_id","age_years","admittime","dischtime",
                            "icu_expire_flag","hospital_expire_flag","expire_flag","ttd_days")],
              by="icustay_id")
if (nrow(core) == n_stays) cat("success,joined outcome tab\n");rm(icustays);gc()

# join admission info
admissions <- fread("data/raw/admissions_202003131320.csv")
core <- merge(core,admissions[,c("hadm_id","admission_type","admission_location",
                                     "discharge_location","insurance","ethnicity","diagnosis")],
                by = c("hadm_id"),all.x=TRUE)
if (nrow(core) == n_stays) cat("success,joined admissioned tab\n");rm(admissions);gc()

# patient info (just gender)
patients <- fread("data/raw/patients_202003131320.csv")
core <- merge(core,patients[,c("subject_id","gender")],by = c("subject_id"),all.x=TRUE)
if (nrow(core) == n_stays) cat("success, joined patients tab\n");rm(patients);gc()

# diabetic status
diabetes <- fread("data/raw/diabetes_202003241701.csv")
core <- merge(core,diabetes[,c("hadm_id","dm","dmcx")],by = c("hadm_id"),all.x=TRUE)
if (nrow(core) == n_stays) cat("success,joined diabetes\n");rm(diabetes);gc()

# save
fname <- paste0("data/raw/static_variables_",format(Sys.time(),"%Y%m%d%H%M"))
fwrite(core,file = paste0(fname,".csv"))
saveRDS(core,file=paste0(fname,".rds"),compress = FALSE)
