################################################################################
# CONTENTS:
# A. VARIABLE CONVERSION: specific
#     specific to a dataset/variable e.g. ethinicity
# B. VARIABLE CONVERSION: general
# C. DATASET pipeline
###############################################################################

# A. VARIABLE CONVERSION: specific --------------------------------------------

#' Convert the MIMIC III ethnicity variable to format used in models
#'
convert_xx_ethnicity <- function(dt) {
  dt[,xx_ethnicity := trimws(gsub("-.*","",xx_ethnicity))]
  dt[,xx_ethnicity := trimws(gsub("/.*","",xx_ethnicity))]
  dt[xx_ethnicity %in% c("HISPANIC OR LATINO",
                      "SOUTH AMERICAN",
                      "PORTUGUESE",
                      "CARIBBEAN ISLAND"), xx_ethnicity := "HISPANIC"]
  levels_ethnicity <- c("ASIAN",                                    
                        "WHITE",                                    
                        "OTHER",                                    
                        "BLACK",                                    
                        "UNKNOWN",                                  
                        "HISPANIC",                                 
                        "UNABLE TO OBTAIN",                         
                        "PATIENT DECLINED TO ANSWER" ,              
                        "MULTI RACE ETHNICITY",                  
                        "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER",
                        "MIDDLE EASTERN",                         
                        "AMERICAN INDIAN")
  dt[!xx_ethnicity %in% levels_ethnicity, xx_ethnicity := "UNKNOWN"]     
  dt[,xx_ethnicity := factor(xx_ethnicity,levels=levels_ethnicity)]
}

#' Convert the diagnosis variable to format used in models
#'
convert_xx_diagnosis <- function(dt) {
  
  # keep top 30 diagnoses; else -> other
  # diagnoses <- unique(glycaemic_train[,.(icustay_id,diagnosis)])
  # diagnoses <- diagnoses[,.N,by=diagnosis][order(-N)]
  # cat(diagnoses[1:30]$diagnosis,sep = "',\n'")
  
  levels_diagnosis <- c('PNEUMONIA',
                        'SEPSIS',
                        'CONGESTIVE HEART FAILURE',
                        'ALTERED MENTAL STATUS',
                        'CORONARY ARTERY DISEASE\\CORONARY ARTERY BYPASS GRAFT /SDA',
                        'INTRACRANIAL HEMORRHAGE',
                        'CORONARY ARTERY DISEASE',
                        'UPPER GI BLEED',
                        'ABDOMINAL PAIN',
                        'CHEST PAIN',
                        'FEVER',
                        'DIABETIC KETOACIDOSIS',
                        'LOWER GI BLEED',
                        'HYPOTENSION',
                        'STROKE;TELEMETRY;TRANSIENT ISCHEMIC ATTACK',
                        'GASTROINTESTINAL BLEED',
                        'SUBARACHNOID HEMORRHAGE',
                        'S/P FALL',
                        'ACUTE RENAL FAILURE',
                        'SEIZURE',
                        'AORTIC STENOSIS\\AORTIC VALVE REPLACEMENT /SDA',
                        'ASTHMA;COPD EXACERBATION',
                        'PANCREATITIS',
                        'DYSPNEA',
                        'URINARY TRACT INFECTION;PYELONEPHRITIS',
                        'ACUTE SUBDURAL HEMATOMA',
                        'HYPONATREMIA',
                        'BLUNT TRAUMA',
                        'UPPER GASTROINTESTINAL BLEED',
                        'LIVER FAILURE',
                        'OTHER')
  dt[!xx_diagnosis %in% levels_diagnosis,xx_diagnosis := 'OTHER']
  dt[,xx_diagnosis := factor(xx_diagnosis,levels=levels_diagnosis)]
  
}

#' Convert the discharge variable to format used in models
#'
convert_xx_discharge <- function(dt) {
  dt[,xx_discharge_location := factor(xx_discharge_location,
                                                levels=c("SNF","HOME","DEAD/EXPIRED","REHAB/DISTINCT PART HOSP",
                                                         "HOME HEALTH CARE",         
                                                         "SHORT TERM HOSPITAL","HOSPICE-HOME",
                                                         "LONG TERM CARE HOSPITAL","HOSPICE-MEDICAL FACILITY",
                                                         "DISCH-TRAN TO PSYCH HOSP", 
                                                         "LEFT AGAINST MEDICAL ADVI","ICF","OTHER FACILITY",
                                                         "DISC-TRAN TO FEDERAL HC"))]
}

# A. VARIABLE CONVERSION: general ----------------------------------------------

#' Create a matrix of lagged versions of a variable
#' As many versions as values for lags (e.g. 1:2 creates a lag1 and lag2 variable)
create_lagged <- function(timestep,ID,y,lags,var_name) {
  require(data.table)
  dt <- data.table(ID,timestep,y)
  names(dt)[3] = var_name
  dt <- dt[order(ID,timestep)]
  for (i in 1:length(lags)) {
    expr <- paste0(var_name,"_lag",lags[i]," := shift(",var_name,",",lags[i],")")
    dt[,eval(parse(text = expr)),by=ID]
  }
  return(data.matrix(dt[,4:ncol(dt)]))
}

#' Return a data.table with lagged versions of a variable
#' As many versions as values for lags (e.g. 1:2 creates a lag1 and lag2 variable)
create_lagged_dt <- function(dt,ID,lags,var_name) {
  for (i in 1:length(lags)) {
    expr <- paste0(var_name,"_lag",lags[i]," := shift(",var_name,",",lags[i],")")
    dt[,eval(parse(text = expr)),by=ID]
  }
  dt
}

#' Return a data.table with summation of lagged versions of a variable
#' As many versions as values for lags (e.g. 1:2 creates a sum lag1:1 and sum lag1:2 variable)
create_lagged_cumsum_dt <- function(dt,ID,lags,var_name) {
  for (i in 1:length(lags)) {
    expr <- paste0(var_name,"_csum_lags_0_",lags[i]," := Reduce(`+`, shift(",var_name,",",0:lags[i],"))")
    dt[,eval(parse(text = expr)),by=ID]
  }
  dt
}

#' Return a data.table with min/max of a variable over past X timesteps
#' 
create_lagged_min_max_dt <- function(dt,ID,lags,var_name) {
  for (i in 1:length(lags)) {
    expr_min <- paste0(var_name,"_min_nlag_",lags[i]," := frollapply(",var_name,",",lags[i],",min)")
    dt[,eval(parse(text = expr_min)),by=ID]
    expr_max <- paste0(var_name,"_max_nlag_",lags[i]," := frollapply(",var_name,",",lags[i],",max)")
    dt[,eval(parse(text = expr_max)),by=ID]
  }
  dt
}

#' Return a data.table with mean of a variable over past X timesteps
#'
create_lagged_mean_dt <- function(dt,ID,lags,var_name) {
  for (i in 1:length(lags)) {
    expr_mean <- paste0(var_name,"_mean_nlag_",lags[i]," := frollmean(",var_name,",",lags[i],")")
    dt[,eval(parse(text = expr_mean)),by=ID]
  }
  dt
}

shift_forward <- function(dt,var_name,n=1,rt=FALSE) {
  expr0a <- paste0("!is.na(",var_name,")")
  expr0b <- paste0(".(icustay_id,tstep,",var_name,")")
  tmp <- dt[eval(parse(text=expr0a)),eval(parse(text=expr0b))]
  for (i in 1:n) {
    expr1 <- paste0(var_name,"_lag",i," := shift(",var_name,",",i,")")
    expr2 <- paste0(var_name,"_lag",i,"_tdiff := tstep - shift(tstep,",i,")")
    tmp[,eval(parse(text=expr1)),by=icustay_id]
    tmp[,eval(parse(text=expr2)),by=icustay_id]
    if (rt) {
      var_name <- gsub("xx_","",var_name)
      expr3 <- paste0(var_name,"_lag",i,"_time := shift(tstep,",i,")")
      tmp[,eval(parse(text=expr3)),by=icustay_id]
    }
  }
  tmp
}

# C. DATASET PIPELINE ----------------------------------------------------------

#' Dataset preparation for XGBoost prediction model
#'
prepare_mimic_xgboost_outcome <- function(dt) {
  # add lagged glucose 
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_glucose_b")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_glucose_l")
  # add lagged insulin
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_infusion_amount")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_push_amount")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_s_inject_amount")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_m_inject_amount")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_l_inject_amount")
  # add lagged nutriton
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_cho_enteral")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_dextrose_enteral")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_amount_enteral")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_fat_enteral")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_protein_enteral")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_calorie_enteral")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_dextrose_tpn")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_tpn_lipids")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_amino_acids_tpn")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_tpn")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_dextrose_fluid")
  # cumulative sums of nutriton
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_cho_enteral")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_dextrose_enteral")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_amount_enteral")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_fat_enteral")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_protein_enteral")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_calorie_enteral")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_dextrose_tpn")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_tpn_lipids")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_amino_acids_tpn")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_tpn")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_dextrose_fluid")
  # cumulative sums of insulin amounts in previous 
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_infusion_amount")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_push_amount")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_s_inject_amount")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_m_inject_amount")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_l_inject_amount")
  # cumulative sums of urine output
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_urineoutput")
  dt <- create_lagged_mean_dt(dt,ID = "icustay_id",3:6,"xx_urineoutput")
  # rolling functions
  dt <- create_lagged_min_max_dt(dt,ID = "icustay_id",3:6,"xx_glucose_b")
  dt <- create_lagged_min_max_dt(dt,ID = "icustay_id",3:6,"xx_glucose_l")
  dt <- create_lagged_mean_dt(dt,ID = "icustay_id",3:6,"xx_glucose_b")
  dt <- create_lagged_mean_dt(dt,ID = "icustay_id",3:6,"xx_glucose_l")
  # return
  return(dt)
}

prepare_mimic_xgboost_outcome_min <- function(dt) {
  # add lagged glucose 
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:24,var_name = "xx_glucose_b")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:24,var_name = "xx_glucose_l")
  # return
  return(dt)
}


prepare_mimic_catboost_outcome <- function(dt) {
  # add lagged insulin
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_infusion_amount")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_push_amount")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_s_inject_amount")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_m_inject_amount")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_l_inject_amount")
  # add lagged nutriton
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_cho_enteral")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_dextrose_enteral")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_amount_enteral")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_fat_enteral")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_protein_enteral")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_calorie_enteral")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_dextrose_tpn")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_tpn_lipids")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_amino_acids_tpn")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_tpn")
  dt <- create_lagged_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_dextrose_fluid")
  # cumulative sums of nutriton
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_cho_enteral")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_dextrose_enteral")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_amount_enteral")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_fat_enteral")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_protein_enteral")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_calorie_enteral")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_dextrose_tpn")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_tpn_lipids")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_amino_acids_tpn")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_tpn")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_dextrose_fluid")
  # cumulative sums of insulin amounts in previous 
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_infusion_amount")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_push_amount")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_s_inject_amount")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_m_inject_amount")
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_insulin_l_inject_amount")
  # cumulative sums of urine output
  dt <- create_lagged_cumsum_dt(dt,ID = "icustay_id",lags = 1:12,var_name = "xx_urineoutput")
  dt <- create_lagged_mean_dt(dt,ID = "icustay_id",3:6,"xx_urineoutput")
  # return
  return(dt)
}


