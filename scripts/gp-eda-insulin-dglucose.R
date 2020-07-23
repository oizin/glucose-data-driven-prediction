############################################################################################
#
# Glycaemic control in the ICU
# Create patient based test/train dataset
# 
# Contents:
# A) PREPARE DATA
# B) GRAPHS -
#
#
# To do:
#
# Init: Oisin (10/07/2020)
###########################################################################################

rm(list = ls())
gc()

# load packages
library(data.table)
library(ggplot2)
library(mgcv)

# load data
glycaemic_train <- fread("data/processed/glycaemic_train_mvonly202007101222.csv")
dim(glycaemic_train)

# variable lists
source("scripts/gp-modelling-vars.R")

## A) PREPARE DATA ------------------------------------------------------------------------

# delete end observations
glycaemic_train_ <- glycaemic_train[!is.na(glucose_t1)]

# add next glucose measure
glycaemic_train_[,glucose_t2 := shift(glucose_t1,-1),by=icustay_id]

# remove some IDs with wrong values
rm_ids <- unique(glycaemic_train_$icustay_id[glycaemic_train_$glucose > 800])
glycaemic_train_ <- glycaemic_train_[!icustay_id %in% rm_ids]

## B) GRAPHS -----------------------------------------------------------------------------

# types of insulin
glycaemic_train_[,.N,by=insulin_type]
glycaemic_train_[,.N,by=insulin_admin]
glycaemic_train_[,.N,by=.(insulin_admin,insulin_type)]

glycaemic_train_$delta_t_brk <- cut(glycaemic_train_$delta_t,breaks = -c(0,10,20,30,60,120,Inf),right=FALSE)

# bolus injection + short
ggplot(data=glycaemic_train_[insulin_admin == "BOLUS_INJECTION" & 
                               insulin_type == "Short" &
                               delta_t < -0.5 & delta_t > -2 &
                               insulin_amount < 50 &
                               glucose_t1 != glucose]) +
  geom_point(aes(x=insulin_amount/patientweight_both,y=(glucose_t1-glucose))) +
  geom_smooth(aes(x=insulin_amount/patientweight_both,y=(glucose_t1-glucose))) +
  theme_bw() +
  coord_cartesian(xlim = c(0,0.2)) +
  labs(x = "Insulin (units/kg)",y = "Change in blood glucose (mg/dL)")
ggsave("plots/change_glucose_insulin.png",width=6)

# bolus injection + short
ggplot(data=glycaemic_train_[insulin_admin == "BOLUS_INJECTION" & 
                               insulin_type == "Short" &
                               delta_t < -0.5 & delta_t > -2 &
                               insulin_amount < 50 &
                               glucose_t1 != glucose]) +
  geom_point(aes(x=insulin_amount/patientweight_both,y=(glucose_t1-glucose))) +
  geom_smooth(aes(x=insulin_amount/patientweight_both,y=(glucose_t1-glucose))) +
  facet_wrap(~ventilated + dm) +
  theme_bw()


# INFUSION + short
ggplot(data=glycaemic_train_[insulin_admin == "INFUSION" & 
                               insulin_type == "Short" &
                               insulin_amount < 50 &
                               glucose_t1 != glucose][1:5000]) +
  geom_point(aes(x=insulin_amount,y=(glucose_t1-glucose))) +
  geom_smooth(aes(x=insulin_amount,y=(glucose_t1-glucose))) +
  facet_wrap(~ventilated + dm)

# BOLUS_PUSH + Short
ggplot(data=glycaemic_train_[insulin_admin == "BOLUS_PUSH" & 
                               insulin_type == "Short" &
                               insulin_amount < 50 &
                               glucose_t1 != glucose][1:5000]) +
  geom_point(aes(x=insulin_amount,y=(glucose_t1-glucose))) +
  geom_smooth(aes(x=insulin_amount,y=(glucose_t1-glucose))) +
  facet_wrap(~ventilated + dm)

# short
ggplot(data=glycaemic_train_[insulin_type == "Short" &
                               insulin_amount < 50 & delta_t < -0.5 & delta_t > -2][1:5000]) +
  geom_point(aes(x=insulin_amount,y=(glucose_t1-glucose))) +
  geom_smooth(aes(x=insulin_amount,y=(glucose_t1-glucose))) +
  facet_wrap(~ventilated + dm)

ggplot(data=glycaemic_train_[insulin_admin == "BOLUS_INJECTION" &
                               insulin_type == "Short" &
                               delta_t < -0.5 & 
                               delta_t > -2][1:5000]) +
  geom_histogram(aes(x=(glucose_t1-glucose))) 


## C) TEST MODELS -----------------------------------------------------------------------

# one step ahead GAM w/ glucose and insulin
gam1 <- gam(glucose_t1 ~ s(glucose) + s(insulin_amount),
            data=glycaemic_train_[insulin_type == "Short"])
summary(gam1)
plot(gam1)

