############################################################################################
#
# Glycaemic control in the ICU
# Create patient based test/train dataset
# 
# Contents:
# A) PREPARE DATA
# B) HYPERPARAMETER SELECTION
# C) TRAIN XGB
# D) SHOW EXAMPLE PREDICTIONS
#
#
# To do:
#
# Init: Oisin (09/07/2020)
###########################################################################################

rm(list = ls())
gc()

# load packages
library(data.table)
library(xgboost)
library(ggplot2)

# load data
glycaemic_train <- fread("data/processed/glycaemic_train_mvonly202007101222.csv")
dim(glycaemic_train)

# variable lists
source("scripts/gp-modelling-vars.R")

## A) PREPARE DATA ------------------------------------------------------------------------

# insulin vars
glycaemic_train[,insulin_amount_t1 := shift(insulin_amount,1),by=icustay_id]

# delete end observations
glycaemic_train_ <- glycaemic_train[!is.na(glucose_t1)]

# remove some IDs with wrong values
rm_ids <- unique(glycaemic_train_$icustay_id[glycaemic_train_$glucose > 800])
glycaemic_train_ <- glycaemic_train_[!icustay_id %in% rm_ids]

# feature editing
glycaemic_train_[is.na(insulin_amount),insulin_amount := 0]
glycaemic_train_[is.na(insulin_amount_t1),insulin_amount_t1 := 0]
glycaemic_train_[,insulin_diff := insulin_amount_t1-insulin_amount]
glycaemic_train_[is.na(insulin_rate),insulin_rate := 0]
glycaemic_train_[,insulin_bolus_injection := ifelse(insulin_admin == "BOLUS_INJECTION",1,0)]
glycaemic_train_[,insulin_infusion := ifelse(insulin_admin == "INFUSION",1,0)]
glycaemic_train_[,insulin_bolus_push := ifelse(insulin_admin == "BOLUS_PUSH",1,0)]
glycaemic_train_[,insulin_intermediate := ifelse(insulin_type == "Intermediate",1,0)]
glycaemic_train_[,insulin_long := ifelse(insulin_type == "Long",1,0)]
glycaemic_train_[,insulin_short := ifelse(insulin_type == "Short",1,0)]

# prepare xgboost dataset
y <- as.numeric(glycaemic_train_$glucose_t1)

# covariates
covar <- c("glucose",
           "delta_t",
           "insulin_amount","insulin_amount_t1","insulin_diff","insulin_rate","insulin_bolus_injection",
           "insulin_infusion",
           "insulin_bolus_push","insulin_intermediate","insulin_long","insulin_short",
           "age_years",
           "dmcx","dm",
           "tos_hr","timeofday_hr",
           lab_bedside_vars)
xx <- as.matrix(glycaemic_train_[,..covar])

# xgboost data type
dtrain <- xgb.DMatrix(data = xx, label = y)

## B) HYPERPARAMETER SELECTION -------------------------------------------------------------

# grid search
set.seed(124)
cv <- xgb.cv(data = dtrain, max.depth = 3, nfold=5,
                      eta = 0.1, nthread = 4, nrounds = 100, 
                      objective = "reg:squarederror")

## C) FIT MODEL ---------------------------------------------------------------------------

xgbst <- xgboost(data = dtrain, max.depth = 3,
             eta = 0.3, nthread = 4, nrounds = 100, 
             objective = "reg:squarederror")
varimp <- xgb.importance(model=xgbst)
xgb.plot.importance(varimp,top_n=10)

# D) SHOW EXAMPLE PREDICTIONS -------------------------------------------------------------

# predict on test data
glycaemic_train_$glucose_t1_ <- predict(xgbst,xx)

# show results for select patients
set.seed(35)
patients <- glycaemic_train_[,.N,by=icustay_id]
samp6 <- sample(patients[N>3,icustay_id],6,replace=FALSE)

# results
ggplot(data=glycaemic_train_[icustay_id %in% samp6]) +
  geom_point(aes(x = tos_hr,y=glucose_t1)) +
  geom_point(aes(x = tos_hr,y=glucose_t1_),col="red") +
  geom_line(aes(x = tos_hr,y=glucose_t1)) +
  geom_line(aes(x = tos_hr,y=glucose_t1_),col="red") +
  facet_wrap(~icustay_id,ncol=3,scales = "free") +
  theme_bw() +
  labs(x = "Time in ICU (hours)",y = "Blood glucose (mg/dL)")
ggsave("plots/example_xgboost_predictions.png",width = 11)


