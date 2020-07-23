#############################################################################
#
# Glycaemic control in the ICU
# Table 1
# 
# Contents:
#
#
# Init: Oisin (24/03/2020)
#############################################################################

# load packages
library(data.table)
library(ggplot2)
library(magrittr)
library(lubridate)

# load data
# own
mimic_sugar = fread("data/raw/glycaemic_analysis_hr_202003241207.csv")
mimic_sugar = mimic_sugar[order(icustay_id,hr)]
dim(mimic_sugar)
pt_outcome = fread("data/raw/pt_icu_outcome_202003131320.csv")
pt_outcome = pt_outcome[pt_outcome$icustay_id %in% mimic_sugar$icustay_id,]
dim(pt_outcome)
# mimic
icustays = fread("data/raw/icustays_202003131320.csv")
icustays = icustays[icustays$icustay_id %in% mimic_sugar$icustay_id,]
dim(icustays)
admissions = fread("data/raw/admissions_202003131320.csv")
admissions = admissions[admissions$hadm_id %in% mimic_sugar$hadm_id,]
dim(admissions)
patients = fread("data/raw/patients_202003131320.csv")
patients = patients[patients$subject_id %in% mimic_sugar$subject_id,]
dim(patients)
services = fread("data/raw/services_202003241639.csv")
services = services[services$hadm_id %in% mimic_sugar$hadm_id,]
dim(services)
diabetes = fread("data/raw/diabetes_202003241701.csv")
diabetes = diabetes[diabetes$hadm_id %in% mimic_sugar$hadm_id,]
dim(diabetes)
mechvent = fread("data/raw/pv_mechvent_202003131320.csv")
mechvent = mechvent[mechvent$icustay_id %in% mimic_sugar$icustay_id,]
dim(mechvent)


## A) FEATURES ETC ---------------------------------------------------------



## B) PATIENTS/DEMOGRAPHICS ------------------------------------------------

## ADMISSIONS

# number of ICU admissions
length(unique(pt_outcome$icustay_id))

# number of hospital stays
length(unique(pt_outcome$hadm_id))

# number of patients
length(unique(pt_outcome$subject_id))

# average number of readmission to ICU
readmit_icu = pt_outcome[,.N,by=subject_id]
mean(readmit_icu$N)
sd(readmit_icu$N)
median(readmit_icu$N)
quantile(readmit_icu$N,seq(0,1,0.05))

# number of patients with one admission
nrow(readmit_icu[N == 1])

# average number of redmissions to hospital
readmit_hosp = pt_outcome[,.N,by=hadm_id]
mean(readmit_hosp$N)
sd(readmit_hosp$N)
median(readmit_hosp$N)
quantile(readmit_hosp$N,seq(0,1,0.05))

# length of ICU stay
mean(pt_outcome$los)
sd(pt_outcome$los)
quantile(pt_outcome$los,seq(0,1,0.05))

# stayed for under one day
table(pt_outcome$los < 12/24)

# had not left hospital at extract
table(is.na(pt_outcome$dischtime))

# length of hospital stay (censored)
pt_outcome$dischtime = as_datetime(pt_outcome$dischtime)
pt_outcome$admittime = as_datetime(pt_outcome$admittime)
pt_outcome$hlos = difftime(pt_outcome$dischtime,pt_outcome$admittime,
                           unit="days")
mean(pt_outcome$hlos,na.rm=TRUE)
sd(pt_outcome$hlos,na.rm=TRUE)
quantile(pt_outcome$hlos,seq(0,1,0.05),na.rm=TRUE)


## DEMOGRAPHICS

# age
mean(pt_outcome$age_years)
sd(pt_outcome$age_years)
quantile(pt_outcome$age_years,seq(0,1,0.05))

# gender
table(patients$gender,useNA="always")

# insurance
table(admissions$insurance,useNA="always")

# language not english
table(admissions$language != "ENGL")

# diabetes
table(diabetes$dm)
table(diabetes$dmcx)

## C) ICU/HOSPITAL STAY ----------------------------------------------

# type of ICU
#CCU  Cardiac/Coronary Care Unit
#CSRU  Cardiac surgery recovery unit
#MICU  Medical ICU
#SICU  Surgical Intensive Care Unit 
#TSICU  Trauma Surgical Intensive Care Unit 
table(icustays$first_careunit)

# hospital admission type
table(admissions$admission_type)

# admission location
table(admissions$admission_location)

# type of ICU
table(services$curr_service)

# on mechanical ventilation
length(unique(mechvent$icustay_id))
