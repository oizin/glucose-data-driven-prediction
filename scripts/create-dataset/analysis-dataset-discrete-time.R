############################################################################################
#
# Glycaemic control in the ICU
# Create glucose prediction analysis dataset: discrete time version
#
# Contents:
# a) CREATE FEATURES
# b) Make DISCRETE
#
#
# FIXES:
# a) If insulin + glucose are measured close in time (< 15 min) set to same time (earliest)
#
# Init: Oisin (27/08/2020)
###########################################################################################


# Make the data discrete with a step every:
LENGTH_INTERVAL <- 1.0
MV_ONLY <- TRUE

# load packages
library(data.table)
source("src/utils.R")
source("src/variable-utils.R")

# load data
filename <- "data/raw/glycaemic_analysis_hr_updated_202006210942.rds"
mimic_sugar <- readRDS(filename)
dim(mimic_sugar)
N_init <- nrow(mimic_sugar)

## FEATURES/EXCLUSIONS/SPLITS --------------------------------------------------------------------------

# # indicator variables
# mimic_sugar[,insulin_given := as.numeric(!is.na(insulin_amount))]
# mimic_sugar[,bg_measure := as.numeric(!is.na(glucose_b))]
# mimic_sugar[,bg_or_insulin := as.numeric(insulin_given == 1 | bg_measure == 1)]

# remove patients who spend less than 6 hours in ICU
rm_patient <- unique(mimic_sugar[los_dy*24 < 6.0,icustay_id])
mimic_sugar <- mimic_sugar[!(icustay_id %in% rm_patient),]
cat("removed less than 6 hours; total rows lost:",N_init-nrow(mimic_sugar),"\n")

# remove patients who spend more than 30 days in ICU
rm_patient <- unique(mimic_sugar[los_dy >= 30.0,icustay_id])
mimic_sugar <- mimic_sugar[!(icustay_id %in% rm_patient),]
cat("removed over 30 days; total rows lost:",N_init-nrow(mimic_sugar),"\n")

# remove patients with <3 glucose measures
tab_n_bg = mimic_sugar[!is.na(glucose),.N,by=.(icustay_id)]
tab_n_bg_2 = tab_n_bg[,.N,by=N][order(N)]
sum(tab_n_bg_2[N > 2])
mimic_sugar <- mimic_sugar[icustay_id %in% tab_n_bg[N >= 3,icustay_id]]
cat("removed <3 glucose measures; total rows lost:",N_init-nrow(mimic_sugar),"\n")

## ADD DISCRETE VARIABLE -------------------------------------------------------------------------------

# new discrete time variable
max_stay_length <- max(mimic_sugar$tos_hr) + LENGTH_INTERVAL*2
time_vals <- c(-24.0,seq(0,max_stay_length,by=LENGTH_INTERVAL))
mimic_sugar[,tstep := as.numeric(cut(tos_hr, breaks = time_vals,right = FALSE))]
mimic_sugar[,tstep := tstep - 1]

# discrete time lookup
tstep_lookup <- data.table(start_tstep = time_vals, tstep = 0:(length(time_vals)-0.5))
mimic_sugar <- merge(mimic_sugar,tstep_lookup,all.x=TRUE,by="tstep")
mimic_sugar[,end_tstep := start_tstep + LENGTH_INTERVAL]
mimic_sugar[end_tstep < 0,end_tstep := 0]
setkey(mimic_sugar,icustay_id,tstep)

## SUMMARISE BEDSIDE, INSULIN LABS TO TIME STEP --------------------------------------------------------

## summarise lab and bedside values over each time step
source("./scripts/gp-modelling-vars.R")
print(lab_bedside_vars_dt)
lab_bedside_vars_dt <- c(lab_bedside_vars_dt,
                         "timeofday_hr","hr","dy",
                         "intime","outtime",
                         "los_dy","tos_hr",
                         "time","start_tstep",
                         "end_tstep")
bed_lab_data_15 <- mimic_sugar[,lapply(.SD,mean,na.rm=TRUE),
                               by=.(icustay_id,tstep),
                               .SDcols=lab_bedside_vars_dt]

## summarise insulin data over each time step
# injections
insulin_data_15 <- mimic_sugar[!is.na(insulin_amount),
                               .(
                                 insulin_push_amount = sum(insulin_push_amount,na.rm=TRUE),
                                 insulin_push_n = sum(insulin_push_time > 0,na.rm=TRUE),
                                 insulin_s_inject_amount = sum(insulin_s_inject_amount,na.rm=TRUE),
                                 insulin_s_inject_n = sum(insulin_s_inject_time > 0,na.rm=TRUE),
                                 insulin_m_inject_amount = sum(insulin_m_inject_amount,na.rm=TRUE),
                                 insulin_m_inject_n = sum(insulin_m_inject_time > 0,na.rm=TRUE),
                                 insulin_l_inject_amount = sum(insulin_l_inject_amount,na.rm=TRUE),
                                 insulin_l_inject_n = sum(insulin_l_inject_time > 0,na.rm=TRUE)),
                               by=.(icustay_id,tstep)]
# infusions
insulin_infusion_15 <- mimic_sugar[!is.na(insulin_infusion_amount),
                                   .(icustay_id,
                                     insulin_infusion_rate,
                                     insulin_infusion_amount,
                                     insulin_infusion_start,
                                     insulin_infusion_end)]

## JOIN IN DATASETS --------------------------------------------------------------------------------------

## build full table skeleton
core15 <- mimic_sugar[,.(max = max(tstep)+1),by=icustay_id]
core15 <- core15[rep(seq(.N), max),]
core15[,tstep := seq(from = 0, to = .N-1),by=icustay_id]
core15 <- merge(core15,tstep_lookup,all.x=TRUE,by="tstep")
core15[,end_tstep := start_tstep + LENGTH_INTERVAL]

rm(mimic_sugar); gc()

#### STATIC VARIABLES ####

# join patient outcome
n1 <- nrow(core15)
static_vars <- fread_ids("data/raw/static_variables_202009041031.csv","icustay_id",core15$icustay_id)
convert_datetime(static_vars,"time")
core15 <- merge(core15,static_vars,by=c("icustay_id"),all.x=TRUE)
check_join(n1,core15,"static")

# static bedside
core15 <- merge(core15,unique(bed_lab_data_15[,.(icustay_id,los_dy)]),
                by=c("icustay_id"),all.x=TRUE)
check_join(n1,core15,"static bedside ")

#### TIME VARYING VARIABLES ####

# bedside and lab data
bed_labs_vars <- names(bed_lab_data_15)[!names(bed_lab_data_15) %in%
                                          c(names(core15),
                                            "hr","dy","tos_hr","time","timeofday_hr")]
bed_labs_vars <- c("icustay_id","tstep",bed_labs_vars)
core15 <- merge(core15,bed_lab_data_15[,..bed_labs_vars],
                        by=c("icustay_id","tstep"),all.x=TRUE)
core15[,timeofday_hr := (hour(intime) + minute(intime)/60 + start_tstep) %% 24]
check_join(n1,core15,"bedside and labs")
rm(bed_lab_data_15);gc()

# insulin infusion data
setkey(insulin_infusion_15,icustay_id, insulin_infusion_start, insulin_infusion_end)
insulin_infusion_15 <- foverlaps(core15[,c("icustay_id","start_tstep","end_tstep")],
                                 insulin_infusion_15,
                  by.x=c("icustay_id", "start_tstep","end_tstep"),
                  nomatch=NULL)
# summarise by tstep
setnafill(insulin_infusion_15,type="const",fill=0,
          cols=names(insulin_infusion_15)[grep("rate|amount",names(insulin_infusion_15))])
insulin_infusion_15[,w1 := pmax(insulin_infusion_end - insulin_infusion_start)]
insulin_infusion_15[,w2 := pmin(insulin_infusion_end,end_tstep) - pmax(insulin_infusion_start,start_tstep)]
insulin_infusion_15[,pw := w2/w1]
insulin_infusion_15 <- insulin_infusion_15[,
                                  .(insulin_infusion_amount = sum(insulin_infusion_amount*pw),
                                    time_insulin_infusion = max(start_tstep - min(insulin_infusion_start),0)),
             by=.(icustay_id,start_tstep)]
core15 <- merge(core15,insulin_infusion_15,by=c("icustay_id","start_tstep"),all.x=TRUE)
check_join(n1,core15,"insulin infusions")
rm(insulin_infusion_15);gc()

# other insulin data
core15 <- merge(core15,insulin_data_15,by=c("icustay_id","tstep"),all.x=TRUE)
check_join(n1,core15,"insulin injections")
rm(insulin_data_15);gc()

# mechanical ventilation
mechvent <- fread_ids("data/raw/pv_mechvent_202003131320.csv","icustay_id",core15$icustay_id)
convert_datetime(mechvent,"time")
mechvent <- merge(mechvent,unique(core15[,c("icustay_id","intime")]),by="icustay_id")
mechvent[,vent_start := round(as.numeric(difftime(starttime,intime,units = "mins"))/60,2)]
mechvent[,vent_end := round(as.numeric(difftime(endtime,intime,units = "mins"))/60,2)]
mechvent[,vent_setting := round(as.numeric(difftime(charttime,intime,units = "mins"))/60,2)]
# ventilation timeframes
mechvent_times <- unique(mechvent[,c("icustay_id","vent_start","vent_end","ventnum")])
setkeyv(mechvent_times,cols=c("icustay_id","vent_start","vent_end"))
mechvent_times <- foverlaps(core15[,c("icustay_id","start_tstep","end_tstep")],
                 mechvent_times,
                       by.x=c("icustay_id", "start_tstep","end_tstep"),
                       nomatch=NULL)
mechvent_times[,ventilated := 1]
mechvent_times[,time_ventilated := pmax(start_tstep - vent_start,0)]
mechvent_times <- mechvent_times[,.(time_ventilated = max(time_ventilated),
                                    vent_start = min(vent_start),
                                    vent_end = max(vent_end)),
                                 by=.(icustay_id,start_tstep,end_tstep)]
# ventilation settings
setkeyv(mechvent,cols=c("icustay_id","vent_setting"))
mechvent <- merge(mechvent,unique(core15[,c("icustay_id","intime")]),by="icustay_id")
mechvent[,start_tstep := (vent_setting - vent_setting %% LENGTH_INTERVAL)]
mechvent[,end_tstep := start_tstep + LENGTH_INTERVAL]
# summarise by timestep
mechvent[,w1 := pmin(vent_end - vent_start,LENGTH_INTERVAL)]
mechvent[,w2 := pmin(vent_end,end_tstep) - pmax(vent_start,start_tstep)]
mechvent[,pw := w2/w1]
mechvent <- mechvent[,.(minutevolume = sum(minutevolume*pw,na.rm=TRUE)/sum(pw,na.rm=TRUE),
                        settidalvolume = sum(settidalvolume*pw,na.rm=TRUE)/sum(pw,na.rm=TRUE),
                        obstidalvolume = sum(obstidalvolume*pw,na.rm=TRUE)/sum(pw,na.rm=TRUE),
                        sponttidalvolume = sum(sponttidalvolume*pw,na.rm=TRUE)/sum(pw,na.rm=TRUE),
                        setpeep = sum(setpeep*pw,na.rm=TRUE)/sum(pw,na.rm=TRUE),
                        totalpeep = sum(totalpeep*pw,na.rm=TRUE)/sum(pw,na.rm=TRUE),
                        pressurehighaprv = sum(pressurehighaprv*pw,na.rm=TRUE)/sum(pw,na.rm=TRUE),
                        pressurelowaprv = sum(pressurelowaprv*pw,na.rm=TRUE)/sum(pw,na.rm=TRUE),
                        # timehighaprv = sum(timehighaprv*pw,na.rm=TRUE)/sum(pw,na.rm=TRUE),
                        # timelowaprv = sum(timelowaprv*pw,na.rm=TRUE)/sum(pw,na.rm=TRUE),
                        meanairwaypressure = sum(meanairwaypressure*pw,na.rm=TRUE)/sum(pw,na.rm=TRUE),
                        peakinsppressure = sum(peakinsppressure*pw,na.rm=TRUE)/sum(pw,na.rm=TRUE),
                        neginspforce = sum(neginspforce*pw,na.rm=TRUE)/sum(pw,na.rm=TRUE),
                        plateaupressure = sum(plateaupressure*pw,na.rm=TRUE)/sum(pw,na.rm=TRUE)),
                     by=.(icustay_id,start_tstep)]

# join timeframes and settings
mechvent <- merge(mechvent_times,mechvent,by=c("icustay_id","start_tstep"),all.x=TRUE)
# fill forward these variables
setkeyv(mechvent,cols=c("icustay_id","start_tstep"))
locf_vars <- c("minutevolume","settidalvolume","obstidalvolume","sponttidalvolume",
               "setpeep","totalpeep","pressurehighaprv","pressurelowaprv","meanairwaypressure")
for (i in 1:length(locf_vars)) {
  cat(i,";",locf_vars[i],"\n")
  txt1 <- paste0(locf_vars[i]," := nafill(",locf_vars[i],",type = 'locf')")
  mechvent[,eval(parse(text = txt1)),by=c("icustay_id","vent_start")]
  txt2 <- paste0(locf_vars[i]," := nafill(",locf_vars[i],",type = 'nocb')")
  mechvent[,eval(parse(text = txt2)),by=c("icustay_id","vent_start")]
}
mechvent[,ventilated := 1]
# join to core
core15 <- merge(core15,mechvent, by=c("icustay_id","start_tstep","end_tstep"),all.x=TRUE)
check_join(n1,core15,"mechvent")
rm(mechvent);gc()

# tpn and fluid data
nutrition <- fread_ids("data/raw/pv_nutrition_202008281609.csv","icustay_id",core15$icustay_id)
convert_datetime(nutrition,"time")
nutrition <- merge(nutrition,unique(core15[,c("icustay_id","intime")]),by="icustay_id")
nutrition[,nutr_start := round(as.numeric(difftime(starttime,intime,units = "mins"))/60,2)]
nutrition[,nutr_end := round(as.numeric(difftime(endtime,intime,units = "mins"))/60,2)]
nutrition <- nutrition[nutr_end >= nutr_start]
setkey(nutrition,icustay_id, nutr_start, nutr_end)
nutrition <- foverlaps(core15[,c("icustay_id","start_tstep","end_tstep")],nutrition,
                  by.x=c("icustay_id", "start_tstep","end_tstep"),
                  nomatch=NULL)
setnafill(nutrition,type="const",fill=0)
# summarise by tstep
nutrition[,w1 := pmin(nutr_end - nutr_start)]
nutrition[,w2 := pmin(nutr_end,end_tstep) - pmax(nutr_start,start_tstep)]
nutrition[,pw := w2/w1]
nutrition <- nutrition[,.(dextrose_fluid  = sum(dextrose_fluid*pw),
                          tpn = sum(tpn*pw),
             dextrose_tpn  = sum(dextrose_tpn *pw),
             lipids_tpn = sum(lipids_tpn*pw),
             amino_acids_tpn  = sum(amino_acids_tpn*pw),
             lipids_10_20 = sum(lipids_10_20*pw),
             tpn_lipids = max(tpn_lipids)),
              by=.(icustay_id,start_tstep)]
core15 <- merge(core15,nutrition, by=c("icustay_id","start_tstep"),all.x=TRUE)
check_join(n1,core15,"nutrition")
rm(nutrition);gc()

# enteral nutrition
enteral <- fread_ids("data/raw/ent_nutrition_202009231716.csv","icustay_id",core15$icustay_id)
convert_datetime(enteral,"time")
enteral <- merge(enteral,unique(core15[,c("icustay_id","intime")]),by="icustay_id")
enteral[,ent_start := round(as.numeric(difftime(starttime,intime,units = "mins"))/60,2)]
enteral[,ent_end := round(as.numeric(difftime(endtime,intime,units = "mins"))/60,2)]
enteral <- enteral[ent_end >= ent_start]
setkey(enteral,icustay_id, ent_start, ent_end)
enteral <- foverlaps(core15[,c("icustay_id","start_tstep","end_tstep")],enteral,
                       by.x=c("icustay_id", "start_tstep","end_tstep"),
                       nomatch=NULL)
#setnafill(enteral,type="const",fill=0)
# summarise by tstep
enteral[,w1 := ent_end - ent_start]
enteral[,w2 := pmin(ent_end,end_tstep) - pmax(ent_start,start_tstep)]
enteral[,pw := w2/w1]
enteral <- enteral[,.(amount_enteral   = sum(amount_enteral*pw),
                      cho_enteral = sum(cho_enteral*pw),
                      dextrose_enteral = sum(dextrose_enteral*pw),
                      fat_enteral = sum(fat_enteral*pw),
                      protein_enteral  = sum(protein_enteral*pw),
                      calorie_enteral = sum(calorie_enteral*pw)),
                       by=.(icustay_id,start_tstep)]
core15 <- merge(core15,enteral, by=c("icustay_id","start_tstep"),all.x=TRUE)
check_join(n1,core15,"enteral")
rm(enteral);gc()

# vasopressors
vaso <- fread_ids("data/raw/vasopressors_202003131320.csv","icustay_id",core15$icustay_id)
convert_datetime(vaso,"time")
vaso <- merge(vaso,unique(core15[,c("icustay_id","intime")]),by="icustay_id")
vaso[is.na(endtime),endtime := shift(starttime,-1),by=icustay_id]
vaso[,vaso_start := round(as.numeric(difftime(starttime,intime,units = "mins"))/60,2)]
vaso[,vaso_end := round(as.numeric(difftime(endtime,intime,units = "mins"))/60,2)]
vaso[is.na(vaso_end),vaso_end := vaso_start + 1.3,by=icustay_id]
# join
vaso <- vaso[starttime < endtime]
setkey(vaso,icustay_id, vaso_start, vaso_end)
vaso <- foverlaps(core15[,c("icustay_id","start_tstep","end_tstep")],vaso,
          by.x=c("icustay_id", "start_tstep","end_tstep"),
          nomatch=NULL)
# summarise by tstep
setnafill(vaso,type="const",fill=0,cols=names(vaso)[grep("rate|amount",names(vaso))])
vaso[,w1 := pmin(vaso_end - vaso_start)]
vaso[,w2 := pmin(vaso_end,end_tstep) - pmax(vaso_start,start_tstep)]
vaso[,pw := w2/w1]
vaso <- vaso[,.(norepinephrine_amount = sum(norepinephrine_amount*pw),
    epinephrine_amount = sum(epinephrine_amount*pw),
    dopamine_amount = sum(dopamine_amount*pw),
    dobutamine_amount = sum(dobutamine_amount*pw)),
    by=.(icustay_id,start_tstep)]
# join
core15 <- merge(core15,vaso, by=c("icustay_id","start_tstep"),all.x=TRUE)
check_join(n1,core15,"vaso");rm(vaso);gc()

# fluid output
output <- fread_ids("data/raw/output_hourly_202003131320.csv","icustay_id",core15$icustay_id)
output[,tstep := ceiling(hr/LENGTH_INTERVAL)]
output <- output[,.(urineoutput = sum(urineoutput)),by=.(icustay_id,tstep)]
core15 <- merge(core15,output,by=c("icustay_id","tstep"),all.x=TRUE)
check_join(n1,core15,"urine");rm(output);gc()

# number of empty measure
core15[,no_glucose := ifelse(is.na(glucose) & is.na(glucose_b),1,0)]
core15[tstep > 0,.(miss = mean(no_glucose),.N),by=first_careunit][order(miss)]
core15[tstep > 0 & dbsource == "metavision",.(miss = mean(no_glucose),.N),by=first_careunit][order(miss)]
core15[tstep > 0 & tstep < 5*(24/LENGTH_INTERVAL),.(miss = mean(no_glucose)),by=first_careunit][order(miss)]

## LOCF/BACK -------------------------------------------------------------------------------

invisible(lapply(names(core15),function(.name) set(core15, which(is.nan(core15[[.name]])), j = .name,value =NA)))

# fill in these variables
locf_vars <- c("diasbp","fio2_b","heartrate","meanarterialpressure",
               "resprate","spo2","sysbp","temperature","neutrophil",
               "creactiveprotein","whitebloodcell","partialpressureo2","bicarbonate",
               "lactate","bloodureanitrogen","creatinine","alaninetransaminase",
               "aspartatetransaminase","hemoglobin","intnormalisedratio",
               "platelets","albumin","chloride","sodium","bilirubin_l",
               "hematocrit","fio2_l")
for (i in 1:length(locf_vars)) {
  cat(i,";",locf_vars[i],"\n")
  txt1 <- paste0(locf_vars[i]," := nafill(",locf_vars[i],",type = 'locf')")
  core15[,eval(parse(text = txt1)),by="icustay_id"]
  txt2a <- paste0("!is.na(",locf_vars[i],")")
  txt2b <- paste0(locf_vars[i],"_locf_time := tstep")
  core15[eval(parse(text = txt2a)),eval(parse(text = txt2b))]
  txt3 <- paste0(locf_vars[i],"_locf_time := nafill(",locf_vars[i],
                 "_locf_time,type = 'locf')")
  core15[,eval(parse(text = txt3)),by="icustay_id"]
  txt4 <- paste0(locf_vars[i],"_locf_tdiff := `-`(tstep,",
                 locf_vars[i],"_locf_time",")")
  core15[,eval(parse(text = txt4))]
}

# weight
core15[,patientweight := nafill(patientweight,"nocb"),by=icustay_id]
core15[,patientweight := nafill(patientweight,"locf"),by=icustay_id]

# ventilated
core15[,ventilated := nafill(ventilated,fill = 0)]
core15[,time_ventilated := nafill(time_ventilated,fill = 0)]

# insulin
setnafill(core15,type = "const",fill = 0,cols = c("insulin_infusion_amount",
                                                  "insulin_push_amount",
                                                  "insulin_push_n",
                                                  "insulin_s_inject_amount",
                                                  "insulin_s_inject_n",
                                                  "insulin_m_inject_amount",
                                                  "insulin_m_inject_n",
                                                  "insulin_l_inject_amount",
                                                  "insulin_l_inject_n"))

# IV nutrition
setnafill(core15,type = "const",fill = 0,cols = c("tpn","dextrose_tpn","amino_acids_tpn",
                                                  "lipids_tpn","lipids_10_20","tpn_lipids",
                                                  "dextrose_fluid"))
core15[tpn > 0 & dextrose_tpn == 0,dextrose_tpn := 0.17*tpn]
core15[tpn > 0 & amino_acids_tpn == 0,amino_acids_tpn := 0.06*tpn]
core15[tpn > 0 & lipids_tpn == 0 & tpn_lipids == 1,lipids_tpn := 0.03*tpn]

# enteral nutrition
setnafill(core15,type = "const",fill = 0,cols = c("amount_enteral","cho_enteral",
                                                  "fat_enteral","protein_enteral","calorie_enteral"))
setnafill(core15[amount_enteral == 0],type = "const",fill = 0,cols = c("dextrose_enteral"))

# urine
setnafill(core15,type = "const",fill = 0,cols = c("urineoutput"))

## OUTLIERS #################################################################################
# fix order of magnitude error
# enteral
msk <- core15$calorie_enteral > 200
core15[msk,calorie_enteral := calorie_enteral/10.0]
core15[msk,amount_enteral := amount_enteral/10.0]
core15[msk,dextrose_enteral := dextrose_enteral/10.0]
core15[msk,fat_enteral := fat_enteral/10.0]
core15[msk,protein_enteral := protein_enteral/10.0]
hist(core15$calorie_enteral[core15$calorie_enteral > 0])

# tpn

# delete obvious errors
core15[glucose_l > 1000,glucose_l := NA]
core15[glucose_b > 1000,glucose_b := NA]

## OUTPUT ####################################################################################
# all
fname <- paste0("data/processed/glycaemic_analysis_",
                "dtime_",
                gsub(pattern = "[\\.]",replacement = "_",x = LENGTH_INTERVAL))
if_exists_archive(paste0(fname,".csv"))
fwrite(core15,file = paste0(fname,".csv"))
if_exists_archive(paste0(fname,".rds"))
saveRDS(core15,file = paste0(fname,".rds"),compress=FALSE)

# metavision only
fname <- paste0("data/processed/glycaemic_analysis_mvonly_",
                "dtime_",gsub(pattern = "[\\.]",replacement = "_",x = LENGTH_INTERVAL))
if_exists_archive(paste0(fname,".csv"))
fwrite(core15[dbsource %in% c("metavision")],file = paste0(fname,".csv"))
if_exists_archive(paste0(fname,".rds"))
saveRDS(core15[dbsource %in% c("metavision")],file = paste0(fname,".rds"),compress=FALSE)

## TRAIN/TEST SPLIT ######################################################################

if (MV_ONLY) {
  core15 <- core15[dbsource %in% c("metavision")]
  gc()
}

# load packages
library(mltools)
source("src/ml-pipe-data-processing.R")
library(Matrix)

# 0A) IMPORT DATA ------------------------------------------------------------------------

# data
dim(core15)
names(core15)
cat("data size:\n")
lobstr::obj_addr(core15)
cat(lobstr::obj_size(core15)/1e9,"GB\n")

# 0B) FEATURES ----------------------------------------------------------------------------

source("scripts/gp-variable-lists.R")

# glucose variables
core15[,glucose := NULL]
core15[,glucose_b_shift1 := shift(glucose_b,-1),by=icustay_id]

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
glycaemic_data <- core15[,..vars]
rm(core15);gc()
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

# tpn_patients <- unique(glycaemic_data[xx_tpn > 0,icustay_id])
#
# # 0C) 70/30 PATIENT SPLIT -----------------------------------------------------------------
#
# # done above
#
# # 1E) VARIABLE CATEGORIES ------------------------------------------------------------------
#
#
# # 1F) SAVE -------------------------------------------------------------------------------
#
# # train
# fname <- paste0("data/processed/glycaemic_train_tpn_mvonly_dtime_",
#                 gsub(pattern = "[\\.]",replacement = "_",x = LENGTH_INTERVAL))
# fwrite(glycaemic_train[icustay_id %in% tpn_patients],file = paste0(fname,".csv"))
# saveRDS(glycaemic_train[icustay_id %in% tpn_patients],file = paste0(fname,".rds"),compress=FALSE)
# #saveRDS(xx_train[xx_train[,'icustay_id'] %in% tpn_patients,],file = paste0(fname,"_matrix",".rds"),compress=FALSE)
#
# # test
# fname <- paste0("data/processed/glycaemic_test_tpn_mvonly_dtime_",
#                 gsub(pattern = "[\\.]",replacement = "_",x = LENGTH_INTERVAL))
# fwrite(glycaemic_test[icustay_id %in% tpn_patients],file = paste0(fname,".csv"))
# saveRDS(glycaemic_test[icustay_id %in% tpn_patients],file = paste0(fname,".rds"),compress=FALSE)
# #saveRDS(xx_test[xx_test['icustay_id'] %in% tpn_patients,],file = paste0(fname,"_matrix",".rds"),compress=FALSE)
