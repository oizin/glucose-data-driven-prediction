
# variables
key_vars <- c("icustay_id","tstep")

pt_stay_vars <- c("hadm_id","subject_id",
                  "timeofday_hr",
                  "start_tstep","end_tstep",
                  "intime","outtime",
                  "age_years","ethnicity","gender","insurance",
                  "dmcx","dm",
                  "discharge_location",
                  "icu_expire_flag","hospital_expire_flag",
                  "expire_flag","ttd_days",
                  "los_dy",
                  "admittime","dischtime",
                  "first_careunit","last_careunit",
                  "admission_type","admission_location",
                  "diagnosis",
                  "dbsource")

glycaemic_vars_x <- c("glucose_b","glucose_l")    

# non-glucose variables for in the modelling

pt_stay_vars_x <- c("timeofday_hr",
                  "age_years",
                  "first_careunit",
                  "admission_type",
                  "admission_location",
                  "ethnicity",
                  "gender",
                  "insurance",
                  "dmcx",
                  "dm",
                  "diagnosis")

vent_vars_x <- c("time_ventilated","vent_start","vent_end",
                      "minutevolume","settidalvolume","obstidalvolume",
                      "sponttidalvolume","setpeep","totalpeep",
                      "pressurehighaprv","pressurelowaprv","meanairwaypressure",
                      "peakinsppressure","neginspforce","plateaupressure",
                      "ventilated")

                     
insulin_vars_x <- c("time_insulin_infusion",                     
                    "insulin_infusion_amount",  
                    "insulin_push_amount","insulin_push_n",
                    "insulin_s_inject_amount","insulin_s_inject_n","insulin_m_inject_amount",
                    "insulin_m_inject_n","insulin_l_inject_amount","insulin_l_inject_n")

lab_bedside_vars_x <- c("patientweight",
                      "diasbp","diasbp_locf_tdiff",
                      "fio2_b","fio2_b_locf_tdiff",
                      "heartrate","heartrate_locf_tdiff",
                      "meanarterialpressure","meanarterialpressure_locf_tdiff", 
                      "resprate","resprate_locf_tdiff",
                      "spo2","spo2_locf_tdiff",
                      "sysbp","sysbp_locf_tdiff",
                      "temperature","temperature_locf_tdiff",         
                      "neutrophil","neutrophil_locf_tdiff",
                      "creactiveprotein","creactiveprotein_locf_tdiff",
                      "whitebloodcell","whitebloodcell_locf_tdiff",
                      "partialpressureo2","partialpressureo2_locf_tdiff",    
                      "bicarbonate","bicarbonate_locf_tdiff",
                      "lactate","lactate_locf_tdiff",
                      "bloodureanitrogen","bloodureanitrogen_locf_tdiff",
                      "creatinine","creatinine_locf_tdiff",           
                      "alaninetransaminase","alaninetransaminase_locf_tdiff",
                      "aspartatetransaminase","aspartatetransaminase_locf_tdiff",
                      "hemoglobin","hemoglobin_locf_tdiff",
                      "intnormalisedratio","intnormalisedratio_locf_tdiff",
                      "platelets","platelets_locf_tdiff",
                      "albumin","albumin_locf_tdiff",
                      "chloride","chloride_locf_tdiff",
                      "sodium","sodium_locf_tdiff",            
                      "bilirubin_l","bilirubin_l_locf_tdiff",
                      "hematocrit","hematocrit_locf_tdiff",
                      "fio2_l","fio2_l_locf_tdiff",
                      "urineoutput")

nutrition_vars_x <- c("tpn","tpn_lipids","dextrose_tpn","lipids_tpn","lipids_10_20",             
                    "amino_acids_tpn","dextrose_fluid",
                    "amount_enteral","cho_enteral","dextrose_enteral",                
                    "fat_enteral","protein_enteral","calorie_enteral")

epi_vars_x <- c("norepinephrine_amount","epinephrine_amount","dopamine_amount","dobutamine_amount")

