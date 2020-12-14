
# this script is now defunct

############################################################################################
#
# Glycaemic control in the ICU
# Create patient based test/train dataset
# 
# Contents:
# Metavision patients
# 0A) IMPORT DATA
# 0B) NEW FEATURES
# 0C) 70/30 PATIENT SPLIT
#     - train/test
# 0D) VARIABLE CATEGORIES
#     - column reordering
# 0E) SAVE THE DATA
#
# TPN + Metavision patients
# 1A) IMPORT DATA
# 1B)
# 1C) 
#
# To do:
#
# Init: Oisin (09/07/2020)
###########################################################################################

rm(list = ls());gc()

# load packages
library(mltools)
library(data.table)
source("src/ml-pipe-data-processing.R")
library(Matrix)

# 0A) IMPORT DATA ------------------------------------------------------------------------

# load data
filename <- "data/processed/glycaemic_analysis_mvonly_dtime_2.rds"
glycaemic_data <- readRDS(filename)
dim(glycaemic_data)
names(glycaemic_data)
cat("data size:\n")
lobstr::obj_addr(glycaemic_data)
cat(lobstr::obj_size(glycaemic_data)/1e9,"GB\n")

LENGTH_INTERVAL <- glycaemic_data[start_tstep > 0,end_tstep - start_tstep][1]

# 0B) FEATURES ----------------------------------------------------------------------------

source("scripts/gp-discrete/gp-variable-lists.R")

# glucose variables
glycaemic_data[,glucose := NULL]
glycaemic_data[,glucose_b_shift1 := shift(glucose_b,-1),by=icustay_id]

outcome_vars <- c("glucose_b_shift1")

# use unique in case of potential duplication
vars <- unique(c(key_vars,
                 outcome_vars,
                 pt_stay_vars,
                 glycaemic_vars_x,
                 insulin_vars_x,
                 nutrition_vars_x,
                 lab_bedside_vars_x,
                 vent_vars_x,
                 epi_vars_x))
glycaemic_data <- glycaemic_data[,..vars]
pryr::mem_used()

# mark predictors as xx_varname
glycaemic_data_pt_vars <- glycaemic_data[,..pt_stay_vars_x]
vars_x <- unique(c(pt_stay_vars_x,
                   glycaemic_vars_x,
                   insulin_vars_x,
                   nutrition_vars_x,
                   lab_bedside_vars_x,
                   vent_vars_x,
                   epi_vars_x))
msk <- names(glycaemic_data) %in% vars_x
names(glycaemic_data)[msk] <- paste0("xx",sep="_",names(glycaemic_data)[msk])
glycaemic_data <- cbind(glycaemic_data,glycaemic_data_pt_vars)

# 0C) 70/30 PATIENT SPLIT -----------------------------------------------------------------

# # 
# bg_by_pt <- glycaemic_data[!is.na(glucose_b),.(.N),by=icustay_id][order(N)]
# glycaemic_data <- glycaemic_data[icustay_id %in% bg_by_pt[N >= 3,icustay_id]]

# number of measures per patient
patients <- glycaemic_data[,.(.N,los=mean(los_dy),tpn=sum(xx_tpn > 0,na.rm=TRUE)),by=icustay_id]
patients$Ncat <- cut(patients$N,c(0,1,2,3,4,5,10,20,50,100,1000,Inf),right=FALSE)
table(patients$Ncat)

# sample 
set.seed(1234)
Np <- nrow(patients)
patients[,test := rbinom(Np,1,0.3)]
(tab <- table(patients$test))
prop.table(tab)

# check balance
patients[,.(los_av = mean(los),los_m = median(los), N_av = mean(N), N_m = median(N),
            tpn = mean(tpn)),by=test]

# train/test split
glycaemic_train <- glycaemic_data[icustay_id %in% patients[test == 0,icustay_id]]
glycaemic_test <- glycaemic_data[icustay_id %in% patients[test == 1,icustay_id]]

# 0D) AVERAGE (or other) VALUE IMPUTATION ----------------------------------------------------

### TRAINING
## one hot encoding
# reduce number of categories
convert_xx_ethnicity(glycaemic_train)
convert_xx_diagnosis(glycaemic_train)
# convert factor
string_vars <- c("xx_ethnicity","xx_gender","xx_insurance",
                 "xx_first_careunit","xx_admission_type",
                 "xx_admission_location",
                 "xx_diagnosis")
for(col in string_vars)
  set(glycaemic_train, j = col, value = as.factor(make.names(glycaemic_train[[col]])))
tmp <- one_hot(glycaemic_train[,..string_vars])
glycaemic_train <- cbind(glycaemic_train,tmp)
glycaemic_train[,(string_vars) := NULL]
# # convert to matrix
# xx_train <- (data.matrix(glycaemic_train[,..analysis_vars]))
nrow(glycaemic_train)

### TESTING
## one hot encoding
# reduce number of categories
convert_xx_ethnicity(glycaemic_test)
convert_xx_diagnosis(glycaemic_test)
# convert factor
for(col in string_vars)
  set(glycaemic_test, j = col, value = as.factor(glycaemic_test[[col]]))
glycaemic_test <- one_hot(glycaemic_test,cols=string_vars,sparsifyNAs = TRUE)
# # convert to matrix
# xx_test <- (data.matrix(glycaemic_test[,..analysis_vars]))
nrow(glycaemic_test)

# 0E) VARIABLE CATEGORIES ------------------------------------------------------------------


# 0F) SAVE -------------------------------------------------------------------------------

# train
fname <- paste0("data/processed/glycaemic_train_mvonly_dtime_",
                gsub(pattern = "[\\.]",replacement = "_",x = LENGTH_INTERVAL))
fwrite(glycaemic_train,file = paste0(fname,".csv"))
saveRDS(glycaemic_train,file = paste0(fname,".rds"),compress=FALSE)
#saveRDS(xx_train,file = paste0(fname,"_matrix",".rds"),compress=FALSE)
#saveRDS(glycaemic_train,file = paste0(fname,"_sMatrix",".rds"),compress=FALSE)

# test
fname <- paste0("data/processed/glycaemic_test_mvonly_dtime_",
                gsub(pattern = "[\\.]",replacement = "_",x = LENGTH_INTERVAL))
fwrite(glycaemic_test,file = paste0(fname,".csv"))
saveRDS(glycaemic_test,file = paste0(fname,".rds"),compress=FALSE)
#saveRDS(xx_test,file = paste0(fname,"_matrix",".rds"),compress=FALSE)

# 1A) IMPORT DATA ------------------------------------------------------------------------

tpn_patients <- unique(glycaemic_data[xx_tpn > 0,icustay_id])

# 0C) 70/30 PATIENT SPLIT -----------------------------------------------------------------

# done above

# 1E) VARIABLE CATEGORIES ------------------------------------------------------------------


# 1F) SAVE -------------------------------------------------------------------------------

# train
fname <- paste0("data/processed/glycaemic_train_tpn_mvonly_dtime_",
                gsub(pattern = "[\\.]",replacement = "_",x = LENGTH_INTERVAL))
fwrite(glycaemic_train[icustay_id %in% tpn_patients],file = paste0(fname,".csv"))
saveRDS(glycaemic_train[icustay_id %in% tpn_patients],file = paste0(fname,".rds"),compress=FALSE)
#saveRDS(xx_train[xx_train[,'icustay_id'] %in% tpn_patients,],file = paste0(fname,"_matrix",".rds"),compress=FALSE)

# test
fname <- paste0("data/processed/glycaemic_test_tpn_mvonly_dtime_",
                gsub(pattern = "[\\.]",replacement = "_",x = LENGTH_INTERVAL))
fwrite(glycaemic_test[icustay_id %in% tpn_patients],file = paste0(fname,".csv"))
saveRDS(glycaemic_test[icustay_id %in% tpn_patients],file = paste0(fname,".rds"),compress=FALSE)
#saveRDS(xx_test[xx_test['icustay_id'] %in% tpn_patients,],file = paste0(fname,"_matrix",".rds"),compress=FALSE)

