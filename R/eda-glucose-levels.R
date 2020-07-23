#############################################################################
#
# Glycaemic control in the ICU
# Exploratory data analysis: blood glucose
# 
# Contents:
# A) Features: new feature creation
#
#
# Init: Oisin (17/03/2020)
#############################################################################

rm(list = ls())

# load packages
library(data.table)
library(ggplot2)
library(magrittr)
library(lubridate)

# load data
mimic_sugar = fread("data/raw/glycaemic_analysis_hr_202003241207.csv")
#mimic_sugar = fread("data/raw/glycaemic_analysis_202003111632.csv")
mimic_sugar = mimic_sugar[order(icustay_id,hr)]
dim(mimic_sugar)

# remove missing ICU in and exit time
remove_id = unique(mimic_sugar$icustay_id[is.na(mimic_sugar$intime)])
length(remove_id)
mimic_sugar = mimic_sugar[!icustay_id %in% remove_id,]

## A) FEATURES ETC ---------------------------------------------------------
# for row with bediside xor lab value use non-NULL val
mimic_sugar$glucose_bl = fcoalesce(mimic_sugar$glucose_b,
                                  mimic_sugar$glucose_l)

# take some samples 
set.seed(12)
samp1 = sample(unique(mimic_sugar$icustay_id),size=1,replace=FALSE)
samp4 = sample(unique(mimic_sugar$icustay_id),size=4,replace=FALSE)
samp6 = sample(unique(mimic_sugar$icustay_id),size=6,replace=FALSE)
samp20 = sample(unique(mimic_sugar$icustay_id),size=20,replace=FALSE)
samp100 = sample(unique(mimic_sugar$icustay_id),size=100,replace=FALSE)
samp500 = sample(unique(mimic_sugar$icustay_id),size=500,replace=FALSE)
samp1000 = sample(unique(mimic_sugar$icustay_id),size=1000,replace=FALSE)

# time of stay variable
mimic_sugar$time = as_datetime(mimic_sugar$time)
setkey(mimic_sugar,icustay_id,time)
mimic_sugar$intime = as_datetime(mimic_sugar$intime)
mimic_sugar$outtime = as_datetime(mimic_sugar$outtime)
mimic_sugar$tos_min = as.numeric(difftime(mimic_sugar$time,
         mimic_sugar$intime,units = "mins"))
mimic_sugar$tos_hr = mimic_sugar$tos_min/60
mimic_sugar$tos_dy = mimic_sugar$tos_hr/24

# add a 30 time counting min variable
mimic_sugar$tos_30 = cut(mimic_sugar$tos_hr,
                         seq(min(mimic_sugar$tos_hr,na.rm=TRUE)-1,
                             max(mimic_sugar$tos_hr,na.rm=TRUE)+1,
                             by = 0.5),right=FALSE)
# add a 15 time counting min variable
mimic_sugar$tos_15 = cut(mimic_sugar$tos_hr,
                         seq(min(mimic_sugar$tos_hr,na.rm=TRUE)-1,
                             max(mimic_sugar$tos_hr,na.rm=TRUE)+1,
                             by = 0.25),right=FALSE)

# treated with insulin
mimic_sugar$insulin_rate[is.na(mimic_sugar$insulin_rate)] = 0
mimic_sugar$insulin_amount[is.na(mimic_sugar$insulin_amount)] = 0
treated_ids = unique(mimic_sugar$icustay_id[mimic_sugar$insulin_amount > 0])
mimic_sugar$insulin_ever = 1*(mimic_sugar$icustay_id %in% treated_ids)
mimic_sugar$insulin_ever = ifelse(mimic_sugar$insulin_ever==1,"treated","untreated")

## B) TEMPORAL TREND -------------------------------------------------------
# subset to rows with a glucose measure
mimic_sugar_c = mimic_sugar[!is.na(glucose_bl),]

# plot a sample
pout = mimic_sugar_c[icustay_id %in% samp6,] %>%
  ggplot() +
  geom_ribbon(mapping=aes(x=tos_dy,ymin=80,ymax=101),fill="grey50",alpha=0.4) +
  geom_ribbon(mapping=aes(x=tos_dy,ymin=40,ymax=180),fill="pink",alpha=0.4) +
  geom_point(aes(x=tos_dy,y=glucose_bl,col=factor(icustay_id)),size=1.0) +
  geom_line(aes(x=tos_dy,y=glucose_bl,col=factor(icustay_id))) +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Blood glucose (mg/dL)", x = "Time in ICU (days)") +
  facet_wrap(~icustay_id,scales="free_x") +
  coord_cartesian(ylim=c(0,250))
pout
loc = paste0("output/graphs/glucose_sample_",Sys.Date(),".png")
ggsave(loc,plot = pout)

# number of measures per time period
measure_hr = mimic_sugar_c[,.N,by=.(hr,icustay_id)]
table(measure_hr$N)
measure_30 = mimic_sugar_c[,.N,by=.(tos_30,icustay_id)]
table(measure_30$N)
measure_15 = mimic_sugar_c[,.N,by=.(tos_15,icustay_id)]
table(measure_15$N)

# frequency domain trends
mimic_sugar_c$timeofday <- lubridate::hour(mimic_sugar_c$time) + lubridate::minute(mimic_sugar_c$time)/60
library(nlts)
pat_250075 <- mimic_sugar_c[icustay_id %in% 250075,]
spec_250075 <- spec.lomb(y = pat_250075$glucose_bl, x = pat_250075$tos_min, freq = NULL)
plot(spec_250075)
for (i in 1:length(samp100)) {
  pat_data <- mimic_sugar_c[icustay_id %in% samp100[i],]
  glu_mp <- loess(glucose_bl ~ tos_min, pat_data)
  pat_data$glucose_c <- pat_data$glucose_bl - predict(glu_mp,pat_data)
  spec_res <- spec.lomb(y = pat_data$glucose_c, x = pat_data$tos_min, freq = NULL)
  spec_res_df <- data.frame(spec = spec_res$spec,freq = spec_res$freq)
  pout <- ggplot(spec_res_df) + 
    geom_line(aes(y = spec,x=freq)) +
    theme_bw() +
    labs(title = samp100[i])
  ggsave(filename = paste0('output/graphs/spectral/glucose/spec_',samp100[i],'.png'),plot = pout)
  n <- length(pat_data)
  pout <- ggplot(pat_data) +
    geom_point(aes(x=timeofday,y=glucose_bl,col=factor(dy))) +
    geom_smooth(aes(x=timeofday,y=glucose_bl)) +
    theme_bw() +
    labs(title = samp100[i])
  ggsave(filename = paste0('output/graphs/trends/glucose/trend_',samp100[i],'.png'),plot = pout)
}

# average and quantiles
av_glucose_t = mimic_sugar_c[glucose_bl < 1000,
                  .(mean = mean(glucose_bl),
                    q25 = quantile(glucose_bl,probs = 0.25),
                    q75 = quantile(glucose_bl, probs = 0.75)),
                             by=hr]
av_glucose_t$dy = av_glucose_t$hr/24
pout = ggplot(av_glucose_t) +
  geom_line(data = mimic_sugar_c[icustay_id %in% samp1000],
            aes(x = tos_dy, y = glucose_bl), col = "lightblue") +
  geom_line(aes(x = dy, y = mean),size=1.05) +
  geom_line(aes(x = dy, y = q25),linetype=2) +
  geom_line(aes(x = dy, y = q75),linetype=2) +
  scale_x_continuous(limits = c(0,7),breaks=0:10) +
  coord_cartesian(ylim=c(0,500)) +
  theme_bw() +
  labs(y = "Blood glucose (mg/dL)", x = "Time in ICU (days)")
pout
loc = paste0("output/graphs/glucose_all_",Sys.Date(),".png")
ggsave(loc,plot = pout)

# average and quantiles per insulin
av_glucose_t_i = mimic_sugar_c[glucose_bl < 1000,
                             .(mean = mean(glucose_bl),
                               q25 = quantile(glucose_bl,probs = 0.25),
                               q75 = quantile(glucose_bl, probs = 0.75)),
                             by=.(hr,insulin_ever)]
av_glucose_t_i[hr>0][order(hr)]
av_glucose_t_i$dy = av_glucose_t_i$hr/24
pout = ggplot(av_glucose_t_i) +
  geom_line(data = mimic_sugar_c[icustay_id %in% samp1000],
            aes(x = tos_dy,y = glucose_bl,group=icustay_id),col="lightblue",alpha=0.5) +
  geom_line(aes(x = dy, y = mean),size=1.05) +
  geom_line(aes(x = dy, y = q25),linetype=2) +
  geom_line(aes(x = dy, y = q75),linetype=2) +
  scale_x_continuous(limits = c(0,5),breaks=0:10) +
  coord_cartesian(ylim=c(0,500)) +
  theme_bw() +
  labs(y = "Blood glucose (mg/dL)", x = "Time in ICU (days)") +
  facet_wrap(~insulin_ever)
pout
loc = paste0("output/graphs/glucose_all_tmt_",Sys.Date(),".png")
ggsave(loc,plot = pout)

## C) DISTRIBUTION ---------------------------------------------------------
# average glucose
summary(mimic_sugar_c$glucose_bl)
av_glucose = mimic_sugar_c[,.(glucose = mean(glucose_bl),
                              n=.N,
                              insulin=max(insulin_ever)),
                           by=icustay_id]
# distribution of average glucose (all)
pout = av_glucose[glucose < 350] %>%
  ggplot() +
  geom_density(aes(x = glucose),fill="lightblue",col="lightblue") +
  theme_bw() +
  labs(x = "Blood glucose (mg/dL)")
pout
loc = paste0("output/graphs/glucose_dist_",Sys.Date(),".png")
ggsave(loc,plot = pout)

# distribution of average glucose (by insulin)
insulin_labs =  c("Treated with insulin",
                  "Not treated with insulin")
names(insulin_labs) <- c("treated", "untreated")
pout = av_glucose[glucose < 350] %>%
  ggplot() +
  geom_density(aes(x = glucose),fill="lightblue",col="lightblue") +
  theme_bw() +
  labs(x = "Blood glucose (mg/dL)") +
  facet_wrap(~insulin,
             labeller = labeller(insulin = insulin_labs))
pout
loc = paste0("output/graphs/glucose_dist_tmt_",Sys.Date(),".png")
ggsave(loc,plot = pout)

## D) TIME BETWEEN MEASURES ---------------------------------------------------
diabetes <- fread("data/diabetes_202003241701.csv")
diabetes <- diabetes[diabetes$hadm_id %in% mimic_sugar$hadm_id,]
diabetes <- diabetes[dm == 1]
mechvent <- fread("data/pv_mechvent_202003131320.csv")
mechvent <- mechvent[mechvent$icustay_id %in% mimic_sugar$icustay_id,]
icustays <- fread("data/icustays_202003131320.csv")
icustays <- icustays[icustays$icustay_id %in% mimic_sugar$icustay_id,]

# restrict to time in ICU
mimic_sugar_icu <- mimic_sugar_c[time >= intime & time < outtime]
mimic_sugar_icu$difftime <- c(NA,diff(mimic_sugar_icu$time)/60)
setNA <- c(FALSE,mimic_sugar_icu$icustay_id[-nrow(mimic_sugar_icu)] != mimic_sugar_icu$icustay_id[-1])
mimic_sugar_icu$difftime[setNA] <- NA
mimic_sugar_icu$difftime[mimic_sugar_icu$difftime > 1440] <- NA
prop.table(table(mimic_sugar_icu$difftime < 180))

# mark mechvent and diabetes
mimic_sugar_icu$mechvent <- mimic_sugar_icu$icustay_id %in% mechvent$icustay_id
mimic_sugar_icu$mechvent <- ifelse(mimic_sugar_icu$mechvent,"Ventilated","Nonventilated")
mimic_sugar_icu$diabetes <- mimic_sugar_icu$hadm_id %in% diabetes$hadm_id
mimic_sugar_icu$diabetes <- ifelse(mimic_sugar_icu$diabetes,"Diabetic","Nondiabetic")

# merge to get stay unit
mimic_sugar_icu <- merge(mimic_sugar_icu,icustays[,c("icustay_id","first_careunit",
                                                     "first_wardid")],
                         all.x=TRUE,by = "icustay_id")

# distribution of time between blood glucose
mimic_sugar_icu %>%
  ggplot() +
  geom_histogram(aes(x = difftime),fill="lightblue",col="white") +
  scale_x_continuous(breaks = c(0,120,seq(240,900,120)),limits = c(0,900),
                     labels = c(0,120,seq(240,900,120))/60) +
  theme_bw() +
  labs(x = "Time between blood glucose measurements (hours)") +
  facet_wrap(~insulin_ever,
             labeller = labeller(insulin_ever = insulin_labs),ncol=1,scales = "free_y")

# by diabetes
mimic_sugar_icu %>%
  ggplot() +
  geom_histogram(aes(x = difftime),fill="lightblue",col="white") +
  scale_x_continuous(breaks = c(0,120,seq(240,900,120)),limits = c(0,900),
                     labels = c(0,120,seq(240,900,120))/60) +
  theme_bw() +
  labs(x = "Time between blood glucose measurements (hours)") +
  facet_wrap(~diabetes*insulin_ever,
             labeller = labeller(insulin_ever = insulin_labs),ncol=2,scales = "free_y")

# by mech vent
mimic_sugar_icu %>%
  ggplot() +
  geom_histogram(aes(x = difftime),fill="lightblue",col="white") +
  scale_x_continuous(breaks = c(0,120,seq(240,900,120)),limits = c(0,900),
                     labels = c(0,120,seq(240,900,120))/60) +
  theme_bw() +
  labs(x = "Time between blood glucose measurements (hours)") +
  facet_wrap(~mechvent*insulin_ever,
             labeller = labeller(insulin_ever = insulin_labs),ncol=2,scales = "free_y")

# by day/night
mimic_sugar_icu$daytime <- hour(mimic_sugar_icu$time) > 8 & hour(mimic_sugar_icu$time) < 20
mimic_sugar_icu[daytime == TRUE] %>%
  ggplot() +
  geom_histogram(aes(x = difftime),fill="lightblue",col="white") +
  scale_x_continuous(breaks = c(0,120,seq(240,900,120)),limits = c(0,900),
                     labels = c(0,120,seq(240,900,120))/60) +
  theme_bw() +
  labs(x = "Time between blood glucose measurements (hours)") +
  facet_wrap(~insulin_ever,
             labeller = labeller(insulin_ever = insulin_labs),ncol=1,scales = "free_y")

# by person
difftime_persn <- mimic_sugar_icu[,.(max = max(difftime,na.rm=TRUE),
                                     mean = mean(difftime,na.rm=TRUE)),by=icustay_id]
difftime_persn[difftime_persn$difftime == -Inf] <- NA
hist(difftime_persn$max)
hist(difftime_persn$mean)

summary(difftime_persn$difftime)

## E) TIME OF GLUCOSE MEASURES ---------------------------------------------------
mimic_sugar_icu$time_hour <- hour(mimic_sugar_icu$time)
mimic_sugar_icu %>%
  ggplot() +
  geom_bar(aes(x = time_hour),fill="lightblue",col="white") +
  theme_bw() +
  labs(x = "Time of blood glucose measurements (24 hour)") +
  facet_wrap(~insulin_ever,
             labeller = labeller(insulin_ever = insulin_labs),ncol=1,scales = "free_y")

# by diabetes
mimic_sugar_icu %>%
  ggplot() +
  geom_bar(aes(x = time_hour),fill="lightblue",col="white") +
  theme_bw() +
  labs(x = "Time of blood glucose measurements (24 hour)") +
  facet_wrap(~diabetes*insulin_ever,
             labeller = labeller(insulin_ever = insulin_labs),ncol=2,scales = "free_y")

# by mech vent
mimic_sugar_icu %>%
  ggplot() +
  geom_bar(aes(x = time_hour),fill="lightblue",col="white") +
  theme_bw() +
  labs(x = "Time of blood glucose measurements (24 hour)") +
  facet_wrap(~insulin_ever*mechvent,
             labeller = labeller(insulin_ever = insulin_labs),ncol=2,scales = "free_y")

# by ICU type
mimic_sugar_icu %>%
  ggplot() +
  geom_bar(aes(x = time_hour),fill="lightblue",col="white") +
  theme_bw() +
  labs(x = "Time of blood glucose measurements (24 hour)") +
  facet_wrap(~first_careunit,
             labeller = labeller(insulin_ever = insulin_labs),ncol=2,scales = "free_y")

# by ward ID
mimic_sugar_icu %>%
  ggplot() +
  geom_bar(aes(x = time_hour),fill="lightblue",col="white") +
  theme_bw() +
  labs(x = "Time of blood glucose measurements (24 hour)") +
  facet_wrap(~first_wardid,
             labeller = labeller(insulin_ever = insulin_labs),ncol=2,scales = "free_y")

# by ward ID (treated)
mimic_sugar_icu[insulin_ever == "treated"] %>%
  ggplot() +
  geom_bar(aes(x = time_hour),fill="lightblue",col="white") +
  theme_bw() +
  labs(x = "Time of blood glucose measurements (24 hour)",
       title = "Received insulin") +
  facet_wrap(~first_wardid,
             labeller = labeller(insulin_ever = insulin_labs),ncol=2,scales = "free_y")

# by ward ID (untreated)
mimic_sugar_icu[insulin_ever == "untreated"] %>%
  ggplot() +
  geom_bar(aes(x = time_hour),fill="lightblue",col="white") +
  theme_bw() +
  labs(x = "Time of blood glucose measurements (24 hour)",
       title = "Never received insulin") +
  facet_wrap(~first_wardid,
             labeller = labeller(insulin_ever = insulin_labs),ncol=2,scales = "free_y")

## F) NUMBER OF GLUCOSE MEASURES ---------------------------------------------------

mimic_sugar_icu$year <- lubridate::year(mimic_sugar_icu$time)
glucose_dy <- mimic_sugar_icu[,.N,by=c("icustay_id","dy")][order(N)]
glucose_dy[dy < 7] %>%
  ggplot() +
  geom_bar(aes(x = N),fill="lightblue",col="white") +
  facet_wrap(~dy)
  
