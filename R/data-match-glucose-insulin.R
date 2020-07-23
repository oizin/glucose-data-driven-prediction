########################################################################################
#
# Glycaemic control in the ICU
# Match insulin and glucose
# 
# Contents:
# a) features
# b) matching
#
# Init: Oisin (04/06/2020)
########################################################################################

rm(list = ls())
gc()

# load packages
library(data.table)

# load data
mimic_sugar <- fread("data/glycaemic_analysis_hr_202006210942.csv")
mimic_sugar <- mimic_sugar[order(icustay_id,hr)]
dim(mimic_sugar)

# remove missing ICU in and exit time
remove_id <- unique(mimic_sugar$icustay_id[is.na(mimic_sugar$intime)])
length(remove_id)
mimic_sugar <- mimic_sugar[!icustay_id %in% remove_id,]
setkey(mimic_sugar,icustay_id,time)

## A) FEATURES ETC ----------------------------------------------------------------------

# indicator variables
mimic_sugar[,insulin_given := as.numeric(!is.na(insulin_amount))]
mimic_sugar[,bg_measure := as.numeric(!is.na(glucose_b))]

# time/date variables
mimic_sugar[,time := lubridate::as_datetime(time)]

## B) MATCHING ALGORITHM -----------------------------------------------------------------

# The below code is designed for speed rather than readability due 
# to the slowness of for loops in R.

# we select the nearest (past/future) and most recent (past only), choosing the
# nearest unless it is more than 10 min into the future

# blood glucose dataset
bg <- mimic_sugar[bg_measure == 1,c("icustay_id","time","glucose_b","glucose_l")]
bg[,most_recent_bg := rowMeans(cbind(glucose_b,glucose_l),na.rm=TRUE)]
bg[,glucose_b := NULL]
bg[,glucose_l := NULL]
bg[,time_bg := time]
bg <- unique(bg)
bg <- bg[,.(most_recent_bg = mean(most_recent_bg)),by=c("icustay_id","time","time_bg")]
setkey(bg,icustay_id,time)

# insulin dataset
insulin <- mimic_sugar[insulin_given == 1,c("icustay_id","time")]
insulin <- unique(insulin)
setkey(insulin,icustay_id,time)

# join 1
insulin_bg1 <- bg[insulin,roll="nearest"]
insulin_bg1[,timediff := as.numeric(difftime(time,time_bg,units="min"))]
insulin_bg1 <- insulin_bg1[,c("icustay_id","time","most_recent_bg","time_bg","timediff")]
setnames(insulin_bg1,c("icustay_id","time","most_recent_bg1","time_bg1","timediff1"))
# join 2
insulin_bg2 <- bg[insulin,roll=Inf]
insulin_bg2[,timediff := as.numeric(difftime(time,time_bg,units="min"))]
insulin_bg2 <- insulin_bg2[,c("icustay_id","time","most_recent_bg","time_bg","timediff")]
setnames(insulin_bg2,c("icustay_id","time","most_recent_bg2","time_bg2","timediff2"))

# join to main dataset
n1 <- nrow(mimic_sugar)
mimic_sugar <- merge(mimic_sugar,insulin_bg1,all.x = TRUE,by = c("icustay_id","time"))
mimic_sugar <- merge(mimic_sugar,insulin_bg2,all.x = TRUE,by = c("icustay_id","time"))
nrow(mimic_sugar) == n1

# choose best (most recent) glucose
mimic_sugar[timediff1 < -10,timediff1 := NA]
mimic_sugar[,most_recent_bg := numeric()]
mimic_sugar[,time_recent_bg := numeric()]
# if they are equal doesn't matter
msk <- mimic_sugar$timediff1 == mimic_sugar$timediff2
msk[is.na(msk)] <- FALSE
mimic_sugar[msk,most_recent_bg := most_recent_bg1]
mimic_sugar[msk,time_recent_bg := timediff1]
# if time1 is NA use bg2
msk <- is.na(mimic_sugar$timediff1) & !is.na(mimic_sugar$timediff2)
mimic_sugar[msk,most_recent_bg := most_recent_bg2]
mimic_sugar[msk,time_recent_bg := timediff2]
# for remainder use bg1
msk <- is.na(mimic_sugar$most_recent_bg) & !is.na(mimic_sugar$timediff1)
mimic_sugar[msk,most_recent_bg := most_recent_bg1]
mimic_sugar[msk,time_recent_bg := timediff1]

# # if you want a more raw dataset
# fname <- paste0("data/processed/glycaemic_analysis_",format(Sys.time(),"%Y%m%d%H%M"),".csv")
# fwrite(mimic_sugar,file = fname)

## C) BRING FORWARD/AVERAGE LAB/BED VALUES -------------------------------------------------


mimic_sugar_pre_icu <- 


## C) ADD IN TIME VARIABLE ----------------------------------------------------------------


mimic_sugar[time_d := hour(time) + minute(time)/60]
b15 <- seq(0,24,by = 0.25)
mimic_sugar$time_d_15 <- cut(mimic_sugar$time_d,breaks = b15,right=FALSE,
                             labels=b15[-length(b15)])
mimic_sugar$time_d_15


## D) JOIN IN OTHER DATASETS --------------------------------------------------------------

# load data
# own
pt_outcome <- fread("data/pt_icu_outcome_202003131320.csv")
pt_outcome <- pt_outcome[pt_outcome$icustay_id %in% mimic_sugar$icustay_id,]
# mimic
icustays <- fread("data/icustays_202003131320.csv")
icustays <- icustays[icustays$icustay_id %in% mimic_sugar$icustay_id,]
admissions <- fread("data/admissions_202003131320.csv")
admissions <- admissions[admissions$hadm_id %in% mimic_sugar$hadm_id,]
patients <- fread("data/patients_202003131320.csv")
patients <- patients[patients$subject_id %in% mimic_sugar$subject_id,]
services <- fread("data/services_202003241639.csv")
services <- services[services$hadm_id %in% mimic_sugar$hadm_id,]
diabetes <- fread("data/diabetes_202003241701.csv")
diabetes <- diabetes[diabetes$hadm_id %in% mimic_sugar$hadm_id,]
mechvent <- fread("data/pv_mechvent_202003131320.csv")
mechvent <- mechvent[mechvent$icustay_id %in% mimic_sugar$icustay_id,]

# join



## E) SAVE --------------------------------------------------------------------------------

fname <- paste0("data/processed/glycaemic_analysis_",format(Sys.time(),"%Y%m%d%H%M"),".csv")
fwrite(mimic_sugar,file = fname)




