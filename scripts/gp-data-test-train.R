############################################################################################
#
# Glycaemic control in the ICU
# Create patient based test/train dataset
# 
# Contents:
# A) NEW FEATURES
# B) 70/30 PATIENT SPLIT
#     - train/test
# C) AVERAGE (or other) VALUE IMPUTATION
# D) VARIABLE CATEGORIES
#     - patient stay
#     - bedside and physiological vars
#     - glycaemic vars
# E) SAVE
#     - train and test sets
#
# To do:
#
# Init: Oisin (09/07/2020)
###########################################################################################

rm(list = ls())
gc()

# load packages
library(data.table)

# load data
glycaemic_data <- fread("data/processed/glycaemic_analysis_mvonly202007101213.csv")
dim(glycaemic_data)

# A) FEATURES ----------------------------------------------------------------------------
# type
glycaemic_data$outtime <- lubridate::as_datetime(glycaemic_data$outtime)
glycaemic_data$intime <- lubridate::as_datetime(glycaemic_data$intime)

# create
glycaemic_data[,los := as.numeric(difftime(outtime,intime,units="days"))]


# B) 70/30 PATIENT SPLIT -----------------------------------------------------------------

# number of measures per patient
patients <- glycaemic_data[,.(.N,los=mean(los)),by=icustay_id]
patients$Ncat <- cut(patients$N,c(0,1,2,3,4,5,10,20,50,100,1000,Inf),right=FALSE)
table(patients$Ncat)

# sample 
set.seed(123)
Np <- nrow(patients)
patients[,test := rbinom(Np,1,0.3)]
(tab <- table(patients$test))
prop.table(tab)

# check balance
patients[,.(los_av = mean(los),los_m = median(los), N_av = mean(N), N_m = median(N)),by=test]

# train/test split
set.seed(123)
glycaemic_train <- glycaemic_data[icustay_id %in% patients[test == 0,icustay_id]]
glycaemic_test <- glycaemic_data[icustay_id %in% patients[test == 1,icustay_id]]

# C) AVERAGE (or other) VALUE IMPUTATION ----------------------------------------------------

# weight
glycaemic_train[,patientweight_both := nafill(patientweight_locf,"nocb"),by=icustay_id]
glycaemic_test[,patientweight_both := nafill(patientweight_locf,"nocb"),by=icustay_id]

# imputation variables and data
mimic_sugar <- fread("data/raw/glycaemic_analysis_hr_202006210942.csv")
locf_vars <- c("diasbp","fio2_b","heartrate","meanarterialpressure",   
               "resprate","spo2","sysbp","temperature","neutrophil",              
               "creactiveprotein","whitebloodcell","partialpressureo2","bicarbonate",
               "lactate","bloodureanitrogen","creatinine","alaninetransaminase", 
               "aspartatetransaminase","hemoglobin","intnormalisedratio",      
               "platelets","albumin","chloride","sodium","bilirubin_l",             
               "hematocrit","fio2_l","patientweight")
locf_vars1 <- paste0(locf_vars[-length(locf_vars)],"_locf")
locf_vars1 <- c(locf_vars1,"patientweight_both")

# imputation values
mimic_means <- mimic_sugar[,lapply(.SD, mean, na.rm=TRUE),.SDcols = locf_vars]
mimic_medians <- mimic_sugar[,lapply(.SD, median, na.rm=TRUE),.SDcols = locf_vars]
rm(mimic_sugar)
gc()

# impute
for (i in 1:length(locf_vars)) {
  txt <- paste0(locf_vars1[i],":= nafill(",locf_vars1[i],",fill=mimic_medians[,get(locf_vars[i])])")
  glycaemic_train[,eval(parse(text = txt))]
  glycaemic_test[,eval(parse(text = txt))]
}

glycaemic_train[,ventilated := nafill(ventilated,fill = 0)]
glycaemic_test[,ventilated := nafill(ventilated,fill = 0)]

# D) VARIABLE CATEGORIES ------------------------------------------------------------------

source("scripts/gp-modelling-vars.R")

vars <- c(key_vars,pt_stay_vars,glycaemic_vars,lab_bedside_vars)                                                       
glycaemic_train <- glycaemic_train[,..vars]
glycaemic_test <- glycaemic_test[,..vars]

# E) SAVE -------------------------------------------------------------------------------

# train
fname <- paste0("data/processed/glycaemic_train_mvonly",format(Sys.time(),"%Y%m%d%H%M"),".csv")
fwrite(glycaemic_train,file = fname)

# test
fname <- paste0("data/processed/glycaemic_test_mvonly",format(Sys.time(),"%Y%m%d%H%M"),".csv")
fwrite(glycaemic_test,file = fname)

## END
