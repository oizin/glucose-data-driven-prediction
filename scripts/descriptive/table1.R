################################################################################
# Table 1 statistics for Data driven glucose prediction
#
#
###############################################################################

## libraries
library(data.table)

## import data
glycaemic_train <- readRDS("scripts/gp-discrete/glycaemic_train_2_catboost.rds")
glycaemic_test <- readRDS( "scripts/gp-discrete/glycaemic_test_2_catboost.rds")
glycaemic_train <- rbind(glycaemic_train,glycaemic_test[,names(glycaemic_train),with=FALSE])

## statistics
# number of patients
length(unique(glycaemic_train$icustay_id))
length(unique(glycaemic_train$subject_id))

# glucose quantiles
glycaemic_train[dm == 1,quantile(glucose_b_shift1,c(0.05,0.95)),]
glycaemic_train[dm == 0,quantile(glucose_b_shift1,c(0.05,0.95)),]


# glucose by patient average
glycaemic_train[dm == 1,mean(glucose_b_shift1,na.rm=TRUE),by=icustay_id][,.N,by=.(V1>150)]
glycaemic_train[dm == 0,mean(glucose_b_shift1,na.rm=TRUE),by=icustay_id][,.N,by=.(V1>150)]
glycaemic_train[dm == 1,mean(glucose_b_shift1,na.rm=TRUE),by=icustay_id][,.N,by=.(V1>120)]
glycaemic_train[dm == 0,mean(glucose_b_shift1,na.rm=TRUE),by=icustay_id][,.N,by=.(V1>120)]


# age, sex
glycaemic_train[,min(age_years),by=icustay_id][,median(V1)]
glycaemic_train[,.N,by=.(icustay_id,gender)][,.N,by=gender]
glycaemic_train[,.N,by=.(icustay_id,dm)][,.N,by=dm]

# number of glucose measurements
glycaemic_train[!is.na(glucose_b_shift1) | !is.na(xx_glucose_b_lag_0),.N]
glycaemic_train[glucose_b_shift1 < 70,.N]
glycaemic_train[glucose_b_shift1 > 170,.N]

# number of glucose measures per person
tmp = glycaemic_train[!is.na(glucose_b_shift1) | !is.na(xx_glucose_b_lag_0),.N,
                      by=.(icustay_id,dy)]
quantile(tmp$N)

# average 
glycaemic_train[,.(mean(glucose_b_shift1)),by=icustay_id][,.(mean(V1))]
glycaemic_train[tstep < 12,.(mean(glucose_b_shift1)),by=icustay_id][,.(mean(V1))]
glycaemic_train[tstep < 12,.(mean(glucose_b_shift1)),by=icustay_id][V1 < 70,.N]
glycaemic_train[tstep < 12,.(mean(glucose_b_shift1)),by=icustay_id][V1 > 170,.N]
glycaemic_train[tstep < 12,.(mean(glucose_b_shift1)),by=icustay_id][,.N]
# insulin
# total 
glycaemic_train[xx_insulin_infusion_amount > 0 | xx_insulin_l_inject_amount > 0 |
                  xx_insulin_s_inject_amount > 0 | xx_insulin_m_inject_amount > 0 |
                  xx_insulin_push_amount > 0,.N,by=icustay_id]

# infusion
glycaemic_train[xx_insulin_infusion_amount > 0,.N,by=icustay_id]
glycaemic_train[xx_insulin_infusion_amount > 0,.N]

# injection
glycaemic_train[ xx_insulin_l_inject_amount > 0 |
                  xx_insulin_s_inject_amount > 0 | xx_insulin_m_inject_amount > 0,.N,by=icustay_id]
glycaemic_train[ xx_insulin_l_inject_amount > 0 |
                   xx_insulin_s_inject_amount > 0 | xx_insulin_m_inject_amount > 0,.N]

# catheter
glycaemic_train[xx_insulin_push_amount > 0,.N,by=icustay_id]
glycaemic_train[xx_insulin_push_amount > 0,.N]

# enteral
glycaemic_train[xx_calorie_enteral > 0,.N,by=icustay_id]

# parenteral
glycaemic_train[xx_tpn > 0,.N,by=icustay_id]

# mechanical vent
glycaemic_train[xx_time_ventilated > 0,.N,by=icustay_id]

# vasopressors
glycaemic_train[xx_epinephrine_amount > 0 | xx_dobutamine_amount > 0 | xx_dopamine_amount > 0 | xx_norepinephrine_amount > 0,.N,by=icustay_id]

# icu type
glycaemic_train[,.N,by=.(icustay_id,first_careunit)][,.N,by=first_careunit]

# length of stay
glycaemic_train[,.(los_dy=max(los_dy)),by=.(icustay_id)][,.(mean(los_dy))]

# died
glycaemic_train[,.(hospital_expire_flag=unique(hospital_expire_flag)),by=.(icustay_id)][,.(mean(hospital_expire_flag,na.rm=TRUE))]
glycaemic_train[,.(expire_flag=unique(expire_flag)),by=.(icustay_id)][,.(mean(expire_flag,na.rm=TRUE))]
glycaemic_train[,.(expire_flag=unique(icu_expire_flag)),by=.(icustay_id)][,.(mean(expire_flag,na.rm=TRUE))]
glycaemic_train[,.(expire_flag=mean(ttd_days < 30 & !is.na(ttd_days))),by=.(icustay_id)][,.(mean(expire_flag,na.rm=TRUE))]


