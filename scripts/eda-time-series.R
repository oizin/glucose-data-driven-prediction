#############################################################################
#
# Glycaemic control in the ICU
# Exploratory data analysis: time series
# 
# Contents:
# preparation
#  A) FEATURES
#  B) FUNCTIONS
# clinical time series
#  C) HEART RATE
#  D) BLOOD PRESSURE (MAP)
#  E) TEMPERATURE
#  F) RESPIRATORY RATE
#
#
# Init: Oisin (01/07/2020)
#############################################################################

rm(list = ls())
gc()

# load packages
library(data.table)
library(ggplot2)
library(magrittr)
library(lubridate)

# load data
# data
mimic_sugar = fread("data/raw/glycaemic_analysis_hr_202006210942.csv")
#pt_outcome = fread("data/pt_icu_outcome_202003131320.csv")
#mimic_sugar = fread("data/glycaemic_analysis_202003111632.csv")
mimic_sugar = mimic_sugar[order(icustay_id,hr)]
dim(mimic_sugar)
# outcome
pt_outcome <- fread("data/raw/pt_icu_outcome_202003131320.csv")
pt_outcome <- pt_outcome[pt_outcome$icustay_id %in% mimic_sugar$icustay_id,]
# mechanical ventilation
mechvent <- fread("data/raw/pv_mechvent_202003131320.csv")
mechvent <- mechvent[mechvent$icustay_id %in% mimic_sugar$icustay_id,]

# joins
mimic_sugar <- merge(mimic_sugar,
                     pt_outcome[,c("icustay_id","expire_flag","icu_expire_flag",
                                   "hospital_expire_flag","ttd_days")],
                     by = "icustay_id",all.x=TRUE)
mimic_sugar[,ever_ventilated := 0]
mimic_sugar[icustay_id %in% mechvent$icustay_id,ever_ventilated := 1]

# delete unused
rm(pt_outcome)
rm(mechvent)
gc()

## A) FEATURES ETC ---------------------------------------------------------

# for row with bediside xor lab value use non-NULL val
mimic_sugar[,glucose_bl := fcoalesce(glucose_b,glucose_l)]

# take some samples 
set.seed(14)
icustay_id <- mimic_sugar[dy>0,.N,by=icustay_id]
icustay_id <- icustay_id[N > 30]
samp1 <- sample(icustay_id$icustay_id,size=1,replace=FALSE)
samp4 <- sample(icustay_id$icustay_id,size=4,replace=FALSE)
samp8 <- sample(icustay_id$icustay_id,size=8,replace=FALSE)
samp20 <- sample(icustay_id$icustay_id,size=20,replace=FALSE)
samp100 <- sample(icustay_id$icustay_id,size=100,replace=FALSE)
samp500 <- sample(icustay_id$icustay_id,size=500,replace=FALSE)
samp1000 <- sample(icustay_id$icustay_id,size=1000,replace=FALSE)

# time of stay variable
mimic_sugar[,time := as_datetime(time)]
setkey(mimic_sugar,icustay_id,time)
mimic_sugar[,intime := as_datetime(intime)]
mimic_sugar[,outtime := as_datetime(outtime)]
mimic_sugar[,tos_min := as.numeric(difftime(time,intime,units = "mins"))]
mimic_sugar[,tos_hr := tos_min/60]
mimic_sugar[,tos_dy := tos_hr/24]

# time of day variable
mimic_sugar[,timeofday:= lubridate::hour(time) + lubridate::minute(time)/60]
mimic_sugar[,indate := lubridate::as_datetime(lubridate::date(intime))]
mimic_sugar[,dy1 := difftime(time,indate,units = "days")]
mimic_sugar[,dy1 := as.numeric(floor(dy1)*24)]                  
mimic_sugar[,timeofday1 := timeofday + dy1]
mimic_sugar[,timeofday_hr := timeofday1/24]

## B) FUNCTIONS (FOR BELOW) ----------------------------------------------------------

# the data arg


plot_stc <- function(var,data,span,p1,p2,var_name,ylim_cent=NULL,trend_only=FALSE,id=NULL) {
  # var: variable of interest
  # data: data filtered to no missing in variable of interest
  # span: span for loess smoothing
  # p1: path to spectral
  # p2: path to trend and centred folders
  # ylim_cent
  
  # patients in dataset
  samp_id <- unique(data$icustay_id)
  
  # loop over patients in dataset (unless too few)
  for (i in 1:length(samp_id)) {
    print(i)
    pat_data <- data[icustay_id %in% samp_id[i],]
    if (nrow(pat_data) < 5) {
      next()
    }
    
    # sample trend
    pout1 <- ggplot(pat_data) +
      geom_smooth(aes_(x=~timeofday_hr,y=as.name(var),col=as.name(id)),span=span) +
      geom_point(aes_(x=~timeofday_hr,y=as.name(var),col=as.name(id))) +
      geom_line(aes_(x=~timeofday_hr,y=as.name(var),col=as.name(id))) +
      theme_bw() +
      labs(title = paste("patient:",samp_id[i],";",
                         "ventilated:",pat_data$ever_ventilated[1],
                         "outcome:",pat_data$expire_flag[1]),
           x="Day of stay (calendar time)",
           y=var_name) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
    ggsave(filename = paste0(p2,'trend_',samp_id[i],'.png'),plot = pout1)
    
    if (trend_only == TRUE) {
      next()
    }
    
    # centre process
    ff <- as.formula(paste(var,"~ timeofday1"))
    mean_process <- loess(ff, pat_data,span=span)
    pat_data$var_c <- pat_data[[var]] - predict(mean_process,pat_data)
    
    # periodogram
    spec_res <- spec.lomb(y = pat_data$var_c, x = pat_data$timeofday1, freq = NULL)
    spec_res_df <- data.frame(spec = spec_res$spec,freq = spec_res$freq)
    pout <- ggplot(spec_res_df) + 
      geom_line(aes(y = spec,x=freq)) +
      theme_bw() +
      labs(title = samp_id[i])
    ggsave(filename = paste0(p1,'spec_',samp_id[i],'.png'),plot = pout)
    
    
    # sample variance
    pout2 <- ggplot(pat_data) +
      geom_point(aes_(x=~timeofday_hr,y=~var_c)) +
      geom_line(aes_(x=~timeofday_hr,y=~var_c)) +
      theme_bw() +
      coord_cartesian(ylim=ylim_cent) +
      labs(title = paste("patient:",samp_id[i],";",
                         "ventilated:",pat_data$ever_ventilated[1],
                         "outcome:",pat_data$expire_flag[1]),
           x="Day of stay (calendar time)",
           y=var_name) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 8))
    ggsave(filename = paste0(p2,'centr_',samp_id[i],'.png'),plot = pout2)
  }
}

## C) TIME SERIES: HEART RATE -------------------------------------------------------
dir.create('plots/spectral/heartrate/')
dir.create('plots/trends/heartrate/')

# subset to HEART RATE present
hr_msk <- !is.na(mimic_sugar$heartrate)

# plot a sample
pout = mimic_sugar[hr_msk & icustay_id %in% samp8 & dy < 5,] %>%
  ggplot() +
  geom_smooth(aes(x=tos_dy,y=heartrate),fill="gray80",col="black",linetype=2,span=0.2) +
  geom_point(aes(x=tos_dy,y=heartrate,col=factor(icustay_id)),size=1.0) +
  geom_line(aes(x=tos_dy,y=heartrate,col=factor(icustay_id))) +
  theme_bw() +
  coord_cartesian(ylim=NULL)+
  theme(legend.position = "none") +
  labs(y = "Heart rate (bpm)", x = "Time in ICU (days)") +
  facet_wrap(~icustay_id,scales="free",ncol=4) 
pout
loc = paste0("plots/heartrate_sample_",Sys.Date(),".png")
ggsave(loc,plot = pout)

# frequency domain trends
library(nlts)
pat_250075 <- mimic_sugar[hr_msk & icustay_id %in% 250075,]
spec_250075 <- spec.lomb(y = pat_250075$heartrate, x = pat_250075$timeofday1, freq = NULL)
plot(spec_250075)

# plot frequency and trend analysis for 100 patients
# plot frequency and trend analysis for 100 patients
plot_stc("heartrate",
         mimic_sugar[hr_msk & icustay_id %in% samp100],
         span=0.2,
         p1='plots/spectral/heartrate/',
         p2='plots/trends/heartrate/',
         var_name="Heart rate (bpm)",
         ylim_cent = c(-30,30))

## E) TIME SERIES: BLOOD PRESSURE (MAP) -------------------------------------------------------

dir.create('plots/spectral/map/')
dir.create('plots/trends/map/')

# subset to MAP present
map_msk <- !is.na(mimic_sugar$meanarterialpressure)

# plot a sample
pout = mimic_sugar[map_msk & icustay_id %in% samp4 & dy < 5,] %>%
  ggplot() +
  geom_smooth(aes(x=tos_dy,y=meanarterialpressure),fill="gray80",col="black",linetype=2,span=0.3) +
  geom_point(aes(x=tos_dy,y=meanarterialpressure,col=factor(icustay_id)),size=1.0) +
  geom_line(aes(x=tos_dy,y=meanarterialpressure,col=factor(icustay_id))) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "MAP (mmHg)", x = "Time in ICU (days)") +
  facet_wrap(~icustay_id,scales="free",ncol=2) 
pout
loc = paste0("plots/map_sample_",Sys.Date(),".png")
ggsave(loc,plot = pout)

# plot frequency and trend analysis for 100 patients
plot_stc("meanarterialpressure",
         mimic_sugar[map_msk & icustay_id %in% samp100],
         span=0.3,
         p1='plots/spectral/map/',
         p2='plots/trends/map/',
         var_name="MAP (mmHg)",
         ylim_cent = NULL)

## F) TIME SERIES: TEMPERATURE -------------------------------------------------------

dir.create('plots/spectral/temp/')
dir.create('plots/trends/temp/')

# subset to temp present
tmp_msk <- !is.na(mimic_sugar$temperature)

# plot a sample
pout = mimic_sugar[tmp_msk & icustay_id %in% samp4 & dy < 5,] %>%
  ggplot() +
  geom_smooth(aes(x=tos_dy,y=temperature),fill="gray80",col="black",linetype=2,span=0.5) +
  geom_point(aes(x=tos_dy,y=temperature,col=factor(icustay_id)),size=1.0) +
  geom_line(aes(x=tos_dy,y=temperature,col=factor(icustay_id))) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Temperature (Celcius)", x = "Time in ICU (days)") +
  facet_wrap(~icustay_id,scales="free",ncol=2) 
pout
loc = paste0("plots/temp_sample_",Sys.Date(),".png")
ggsave(loc,plot = pout)

# plot frequency and trend analysis for 100 patients
plot_stc("temperature",
         mimic_sugar[tmp_msk & icustay_id %in% samp100],
         span=1,
         p1='plots/spectral/temp/',
         p2='plots/trends/temp/',
         var_name="Temperature (Celcius)",
         ylim_cent = NULL)

## D) TIME SERIES: RESP RATE -------------------------------------------------------

dir.create('plots/spectral/resprate/')
dir.create('plots/trends/resprate/')

# subset to resp present
resp_msk <- !is.na(mimic_sugar$resprate)

# plot a sample
pout = mimic_sugar[resp_msk & icustay_id %in% samp4 & dy < 5,] %>%
  ggplot() +
  geom_smooth(aes(x=tos_dy,y=resprate),fill="gray80",col="black",linetype=2,span=0.5) +
  geom_point(aes(x=tos_dy,y=resprate,col=factor(icustay_id)),size=1.0) +
  geom_line(aes(x=tos_dy,y=resprate,col=factor(icustay_id))) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Respiratory rate (bpm)", x = "Time in ICU (days)") +
  facet_wrap(~icustay_id,scales="free",ncol=2) 
pout
loc = paste0("plots/rr_sample_",Sys.Date(),".png")
ggsave(loc,plot = pout)

# plot frequency and trend analysis for 100 patients
plot_stc("resprate",
         mimic_sugar[resp_msk & icustay_id %in% samp100],
         span=0.5,
         p1='plots/spectral/resprate/',
         p2='plots/trends/resprate/',
         var_name="Respiratory rate (bpm)",
         ylim_cent = NULL)

## D) TIME SERIES: SPO2 -------------------------------------------------------

dir.create('plots/spectral/spo2/')
dir.create('plots/trends/spo2/')

# subset to SPo2 present
spo2_msk <- !is.na(mimic_sugar$spo2)

# plot a sample
pout = mimic_sugar[spo2_msk & icustay_id %in% samp4 & dy < 5,] %>%
  ggplot() +
  geom_smooth(aes(x=tos_dy,y=spo2),fill="gray80",col="black",linetype=2,span=0.5) +
  geom_point(aes(x=tos_dy,y=spo2,col=factor(icustay_id)),size=1.0) +
  geom_line(aes(x=tos_dy,y=spo2,col=factor(icustay_id))) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Oxygen saturation (%)", x = "Time in ICU (days)") +
  facet_wrap(~icustay_id,scales="free",ncol=2) 
pout
loc = paste0("plots/spo2_sample_",Sys.Date(),".png")
ggsave(loc,plot = pout)

# plot frequency and trend analysis for 100 patients
plot_stc("spo2",
         mimic_sugar[spo2_msk & icustay_id %in% samp100],
         span=0.5,
         p1='plots/spectral/spo2/',
         p2='plots/trends/spo2/',
         var_name="Oxygen saturation (%)",
         ylim_cent = NULL)

## D) TIME SERIES: BLOOD PRESSURE (SYS/DIA) -----------------------------------------------

dir.create('plots/spectral/bp/')
dir.create('plots/trends/bp/')

# subset to blood pressure present
bp_msk <- !is.na(mimic_sugar$sysbp) & !is.na(mimic_sugar$diasbp)

# plot a sample
pout = mimic_sugar[bp_msk & icustay_id %in% samp4 & dy < 5,c("icustay_id","tos_dy","sysbp","diasbp")] %>%
  melt(id.vars = c("icustay_id", "tos_dy"),measure.vars = c("sysbp", "diasbp"),
       variable.name = "measure", value.name = "value") %>%
  ggplot() +
  geom_smooth(aes(x=tos_dy,y=value,col=factor(measure),fill=NULL),fill="gray80",linetype=2,span=0.5) +
  geom_point(aes(x=tos_dy,y=value,col=factor(measure)),size=1.0) +
  geom_line(aes(x=tos_dy,y=value,col=factor(measure))) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  labs(y = "Blood pressure (mmHg)", x = "Time in ICU (days)") +
  facet_wrap(~icustay_id,scales="free",ncol=2) 
pout
loc = paste0("plots/bp_sample_",Sys.Date(),".png")
ggsave(loc,plot = pout)

# plot frequency and trend analysis for 100 patients
bp_df <- mimic_sugar[bp_msk & icustay_id %in% samp100,
                     c("icustay_id","timeofday_hr","ever_ventilated","expire_flag","sysbp","diasbp")] %>%
  melt(id.vars = c("icustay_id", "timeofday_hr","ever_ventilated","expire_flag"),measure.vars = c("sysbp", "diasbp"),
       variable.name = "measure", value.name = "value")

plot_stc("value",
         bp_df,
         span=0.5,
         p1='plots/spectral/bp/',
         p2='plots/trends/bp/',
         var_name="Blood pressure (mmHg)",
         ylim_cent = NULL,
         trend_only = TRUE,
         id = "measure")



