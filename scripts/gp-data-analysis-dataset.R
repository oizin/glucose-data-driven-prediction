############################################################################################
#
# Glycaemic control in the ICU
# Create glucose prediction analysis dataset
# 
# Contents:
# a) CREATE FEATURES
# b) MATCH INSULIN AND MOST RECENT GLUCOSE
# c) BRING FORWARD/AVERAGE LAB/BED VALUES
#     - to next glucose measure
# d) SUMMARISE TO WHEN GLUCOSE MEASURED / INSULIN GIVEN
#     - select only rows where glucose/insulin event
# e) JOIN IN OTHER DATASETS
#     - patient outcome, diabetes, mechanical ventilation
# f) MISSING DATA MASKING VARIABLE AND TIMESTAMPS
#     - calendar time and time since ICU entre
# g) SAVE
#     - all and metavision only
#
# To do:
# - (soft) allow other options compared to locf (e.g. average; max; min)
#
# Init: Oisin (04/06/2020)
###########################################################################################

rm(list = ls())
gc()

# load packages
library(data.table)

# load data
mimic_sugar <- fread("data/raw/glycaemic_analysis_hr_202006210942.csv")
mimic_sugar <- mimic_sugar[order(icustay_id,hr)]
dim(mimic_sugar)

# remove missing ICU in and exit time
remove_id <- unique(mimic_sugar$icustay_id[is.na(mimic_sugar$intime)])
length(remove_id)
mimic_sugar <- mimic_sugar[!icustay_id %in% remove_id,]
setkey(mimic_sugar,icustay_id,time)

## A) FEATURES ETC ------------------------------------------------------------------------

# indicator variables
mimic_sugar[,insulin_given := as.numeric(!is.na(insulin_amount))]
mimic_sugar[,bg_measure := as.numeric(!is.na(glucose_b))]
mimic_sugar[,bg_or_insulin := as.numeric(insulin_given == 1 | bg_measure == 1)]

# time/date variables
mimic_sugar[,time := lubridate::as_datetime(time)]

## B) MATCHING ALGORITHM ------------------------------------------------------------------

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

# cumulative number of glucose or insulin event
mimic_sugar[,glycaemic_n := 0]
mimic_sugar[dy >= 1,glycaemic_n := cumsum(bg_or_insulin),by="icustay_id"]
down <- c(FALSE,mimic_sugar$glycaemic_n[-1] > mimic_sugar$glycaemic_n[-nrow(mimic_sugar)] & 
  mimic_sugar$icustay_id[-1] == mimic_sugar$icustay_id[-nrow(mimic_sugar)])
mimic_sugar[down,glycaemic_n := glycaemic_n-1]

# fill in these variables
locf_vars <- c("diasbp","fio2_b","heartrate","meanarterialpressure",   
               "resprate","spo2","sysbp","temperature","patientweight","neutrophil",              
               "creactiveprotein","whitebloodcell","partialpressureo2","bicarbonate",
               "lactate","bloodureanitrogen","creatinine","alaninetransaminase", 
               "aspartatetransaminase","hemoglobin","intnormalisedratio",      
               "platelets","albumin","chloride","sodium","bilirubin_l",             
               "hematocrit","fio2_l")
for (i in 1:length(locf_vars)) {
  cat(i,";",locf_vars[i],"\n")
  txt1 <- paste0(locf_vars[i],"_locf := nafill(",locf_vars[i],",type = 'locf')")
  mimic_sugar[,eval(parse(text = txt1)),by="icustay_id"]
  txt2a <- paste0("!is.na(",locf_vars[i],")")
  txt2b <- paste0(locf_vars[i],"_locf_time := time")
  mimic_sugar[eval(parse(text = txt2a)),eval(parse(text = txt2b))]
  txt3 <- paste0(locf_vars[i],"_locf_time := nafill(",locf_vars[i],
                 "_locf_time,type = 'locf')")
  mimic_sugar[,eval(parse(text = txt3)),by="icustay_id"]
  txt4 <- paste0(locf_vars[i],"_locf_tdiff := as.numeric(difftime(time,",
                 locf_vars[i],"_locf_time",",units = 'mins'))")
  mimic_sugar[,eval(parse(text = txt4))]
}

## D) SUMMARISE TO WHEN GLUCOSE MEASURED / INSULIN GIVEN -----------------------------------
vars <- c("time","icustay_id",
          "hr","dy",
          "intime","outtime",
          "glycaemic_n","glucose","glucose_b","glucose_l",
          "most_recent_bg","time_recent_bg",
          "med_starttime","med_endtime","storetime",
          "insulin_rate","originalrate","rateuom",
          "insulin_amount","amountuom","insulin_type",
          "insulin_admin","inf_stop","ordercategoryname","ordercategorydescription",
          names(mimic_sugar)[grepl("locf",names(mimic_sugar))])
vars <- vars[!grepl("locf_time",vars)]
glycaemic_data <- mimic_sugar[bg_or_insulin == 1,..vars]

# create next step glucose and timegap
glycaemic_data[is.na(most_recent_bg),most_recent_bg := glucose_b]
glycaemic_data[,glucose := most_recent_bg]
glycaemic_data[,glucose_t1 := shift(glucose,-1),by=icustay_id]
glycaemic_data[,time_1 := shift(time,-1),by=icustay_id]
glycaemic_data[,delta_t := as.numeric(difftime(time,time_1,units = "hours"))]

## E) JOIN IN OTHER DATASETS --------------------------------------------------------------
rm(mimic_sugar);gc()

# load data
# patient outcome
n1 <- nrow(glycaemic_data)
pt_outcome <- fread("data/raw/pt_icu_outcome_202003131320.csv")
pt_outcome <- pt_outcome[pt_outcome$icustay_id %in% glycaemic_data$icustay_id,]
# delete one duplicate
pt_outcome <- pt_outcome[!(icustay_id == 229922 & admittime == "2185-12-02 23:53:00")]
glycaemic_data <- merge(glycaemic_data,
      pt_outcome[,c("icustay_id","intime","subject_id","hadm_id","age_years","admittime","dischtime",
              "icu_expire_flag","hospital_expire_flag","expire_flag","ttd_days")],
      by = c("icustay_id","intime"),all.x=TRUE)
nrow(glycaemic_data) == n1
rm(pt_outcome);gc()

# icustays
n1 <- nrow(glycaemic_data)
icustays <- fread("data/raw/icustays_202003131320.csv")
icustays <- icustays[icustays$icustay_id %in% glycaemic_data$icustay_id,]
glycaemic_data <- merge(glycaemic_data,
      icustays[,c("icustay_id","intime","dbsource","first_careunit","last_careunit")],
      by = c("icustay_id","intime"),all.x=TRUE)
rm(icustays);gc()

# remove carevue or find insulin data!
glycaemic_data[,.(.N,no_insulin = sum(is.na(insulin_amount))),by=dbsource
               ][,.(dbsource,N,no_insulin,no_insulin_p = no_insulin/N)]

# admission info
n1 <- nrow(glycaemic_data)
admissions <- fread("data/raw/admissions_202003131320.csv")
admissions <- admissions[admissions$hadm_id %in% glycaemic_data$hadm_id,]
glycaemic_data <- merge(glycaemic_data,
          admissions[,c("hadm_id","admission_type","admission_location",
                        "discharge_location","insurance","ethnicity","diagnosis")],
          by = c("hadm_id"),all.x=TRUE)
nrow(glycaemic_data) == n1
rm(admissions);gc()

# patient info (just gender)
n1 <- nrow(glycaemic_data)
patients <- fread("data/raw/patients_202003131320.csv")
patients <- patients[patients$subject_id %in% glycaemic_data$subject_id,]
glycaemic_data <- merge(glycaemic_data,
                        patients[,c("subject_id","gender")],
                        by = c("subject_id"),all.x=TRUE)
nrow(glycaemic_data) == n1
rm(patients);gc()

# diabetic status
n1 <- nrow(glycaemic_data)
diabetes <- fread("data/raw/diabetes_202003241701.csv")
diabetes <- diabetes[diabetes$hadm_id %in% glycaemic_data$hadm_id,]
glycaemic_data <- merge(glycaemic_data,
                        diabetes[,c("hadm_id","dm","dmcx")],
                        by = c("hadm_id"),all.x=TRUE)
nrow(glycaemic_data) == n1
rm(diabetes);gc()

# mechanical ventilation
n1 <- nrow(glycaemic_data)
mechvent <- fread("data/raw/pv_mechvent_202003131320.csv")
mechvent <- mechvent[mechvent$icustay_id %in% glycaemic_data$icustay_id,]
mechvent <- unique(mechvent[,c("icustay_id","starttime","endtime")])
mechvent[,starttime := lubridate::as_datetime(starttime)]
mechvent[,endtime := lubridate::as_datetime(endtime)]
setkeyv(mechvent,cols=c("icustay_id","starttime","endtime"))
tmp <- glycaemic_data[,c("icustay_id","time")][mechvent, 
         .(icustay_id,time=x.time,starttime,endtime),
               on = .(icustay_id,
                      time >= starttime, 
                      time <= endtime)]
tmp[,ventilated := 1]
tmp <- unique(tmp[,c("icustay_id","time","ventilated")])
glycaemic_data <- merge(glycaemic_data,tmp,
                        by=c("icustay_id","time"),all.x=TRUE)
nrow(glycaemic_data) == n1

## F) MASKING AND TIMESTAMPS ---------------------------------------------------------------

# masking indicators
locf_vars <- c("diasbp","fio2_b","heartrate","meanarterialpressure",   
               "resprate","spo2","sysbp","temperature","patientweight","neutrophil",              
               "creactiveprotein","whitebloodcell","partialpressureo2","bicarbonate",
               "lactate","bloodureanitrogen","creatinine","alaninetransaminase", 
               "aspartatetransaminase","hemoglobin","intnormalisedratio",      
               "platelets","albumin","chloride","sodium","bilirubin_l",             
               "hematocrit","fio2_l")
for (i in 1:length(locf_vars)) {
  cat(i,";",locf_vars[i],"\n")
  txt1 <- paste0(locf_vars[i],"_missing_m := as.numeric(is.na(",locf_vars[i],"_locf))")
  glycaemic_data[,eval(parse(text = txt1))]
}

# time variables
glycaemic_data$tos_hr <- round(as.numeric(difftime(glycaemic_data$time,
                                             glycaemic_data$intime,units = "mins"))/60,2)
glycaemic_data$timeofday_hr <- round(lubridate::hour(glycaemic_data$time) + 
  lubridate::minute(glycaemic_data$time)/60,2)

## G) SAVE ---------------------------------------------------------------------------------

# all
fname <- paste0("data/processed/glycaemic_analysis_",
                format(Sys.time(),"%Y%m%d%H%M"),".csv")
fwrite(glycaemic_data,file = fname)

# metavision only
fname <- paste0("data/processed/glycaemic_analysis_mvonly",
                format(Sys.time(),"%Y%m%d%H%M"),".csv")
fwrite(glycaemic_data[dbsource %in% c("metavision")],file = fname)

## END
