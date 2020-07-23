#################################################################################
#
# Glycaemic control in the ICU
# Exploratory data analysis: insulin dosing
# 
# Contents:
#
#
# Init: Oisin (18/03/2020)
##################################################################################

rm(list = ls())

# load packages
library(data.table)
library(ggplot2)
library(ggridges)
library(magrittr)
library(lubridate)

# load data
mimic_sugar = fread("data/raw/glycaemic_analysis_hr_202003241207.csv")
mimic_sugar = mimic_sugar[order(icustay_id,hr)]
dim(mimic_sugar)

# remove missing ICU in and exit time
remove_id = unique(mimic_sugar$icustay_id[is.na(mimic_sugar$intime)])
length(remove_id)
mimic_sugar = mimic_sugar[!icustay_id %in% remove_id,]

## A) FEATURES ETC -------------------------------------------------------------

# time of stay variable
mimic_sugar$time = as_datetime(mimic_sugar$time)
mimic_sugar$intime = as_datetime(mimic_sugar$intime)
mimic_sugar$outtime = as_datetime(mimic_sugar$outtime)
mimic_sugar$med_starttime = as_datetime(mimic_sugar$med_starttime)
mimic_sugar$med_endtime = as_datetime(mimic_sugar$med_endtime)
mimic_sugar$tos_min = as.numeric(difftime(mimic_sugar$time,
                                          mimic_sugar$intime,units = "mins"))
mimic_sugar$tos_hr = mimic_sugar$tos_min/60
mimic_sugar$tos_dy = mimic_sugar$tos_hr/24
mimic_sugar$med_start_min = as.numeric(difftime(mimic_sugar$med_starttime,
                                          mimic_sugar$intime,units = "mins"))
mimic_sugar$med_start_hr = mimic_sugar$med_start_min/60
mimic_sugar$med_start_dy = mimic_sugar$med_start_hr/24
mimic_sugar$med_end_min = as.numeric(difftime(mimic_sugar$med_endtime,
                                        mimic_sugar$intime,units = "mins"))
mimic_sugar$med_end_hr = mimic_sugar$med_end_min/60
mimic_sugar$med_end_dy = mimic_sugar$med_end_hr/24

## B) INSULIN AVERAGE ------------------------------------------------------------
# subset to rows with an insulin
mimic_sugar_c = mimic_sugar[!is.na(insulin_amount) & insulin_amount > 0,]
# take some samples 
set.seed(12)
samp1 = sample(unique(mimic_sugar_c$icustay_id),size=1,replace=FALSE)
samp1000 = sample(unique(mimic_sugar_c$icustay_id),size=1000,replace=FALSE)

# average amount of insulin per patient per day
total_i_dy_p = mimic_sugar_c[,.(total_insulin=sum(insulin_amount)),
                           by=.(icustay_id,dy)]
total_i_dy = total_i_dy_p[dy>0,
                  .(insulin_dose_med = median(total_insulin), 
                    insulin_dose_mean = mean(total_insulin),    
                  q5 = quantile(total_insulin,0.10),
                  q95 = quantile(total_insulin,0.90),
                  q75 = quantile(total_insulin,0.75),
                  q25 = quantile(total_insulin,0.25)),by=dy] 

# plot of above
pout = total_i_dy %>%
  ggplot() +
  geom_ribbon(aes(x=dy,ymin=q25,ymax=q75),
              fill="darkblue",alpha=0.5) +
  geom_ribbon(aes(x=dy,ymin=q5,ymax=q95),
              fill="lightblue",alpha=0.5) +
  geom_line(aes(x=dy,y=insulin_dose_med),size=1) +
  coord_cartesian(xlim=c(1,10),ylim=c(0,100)) +
  theme_bw() +
  scale_x_continuous(breaks=1:14) +
  labs(x = "Time in ICU (days)", y = "Insulin dose (units/day)",
       caption = "Figure X: ") +
  theme(plot.caption=element_text(size=12,hjust=0,margin=margin(15,0,0,0)))
pout
loc = paste0("plots/insulin_dist_time_",Sys.Date(),".png")
ggsave(loc,plot = pout)

# distribution over time
# version 1
total_i_dy_p[dy < 11 & dy > 0] %>%
  ggplot() +
  geom_density_ridges(aes(x=total_insulin,y=factor(dy))) +
  coord_cartesian(xlim=c(0,100))
# version 2
dy_labs =  paste0("Day ",1:5)
names(dy_labs) <- 1:5
pout = total_i_dy_p[dy < 6 & dy > 0 & total_insulin < 500] %>%
  ggplot() + 
  geom_histogram(aes(x=total_insulin,group=dy,y=..density..),
                 bins=100,col="white",fill="lightblue") +
  facet_wrap(~dy,scale="free_y",ncol=1,
             labeller = labeller(dy=dy_labs)) +
  coord_cartesian(xlim=c(0,100)) +
  theme_minimal() +
  labs(x="Insulin dose (units/day)") +
  scale_x_continuous(breaks=seq(0,100,20))
pout
loc = paste0("plots/insulin_dy_",Sys.Date(),".png")
ggsave(loc,plot = pout)

## C) INSULIN + GLUCOSE ----------------------------------------------------------
# subset to rows of patients who took insulin
pts = unique(mimic_sugar$icustay_id[mimic_sugar$insulin_amount > 0])
mimic_sugar_p = mimic_sugar[icustay_id %in% pts,]

# take some samples 
samp1 = sample(unique(mimic_sugar_p$icustay_id),size=1,replace=FALSE)
samp1 = 298323
#samp1 = 259799

# prep for the plot
DAY = 1
df_g = mimic_sugar_p[dy %in% DAY & icustay_id %in% samp1 & !is.na(glucose_b)]
df_i = mimic_sugar_p[dy %in% DAY & icustay_id %in% samp1 & !is.na(insulin_amount)]
df_i$y_coord = 50
cont_msk = df_i$ordercategorydescription %in% c("Continuous Med")
df_i$ordercategorydescription = 
  factor(df_i$ordercategorydescription,levels=c("Continuous Med","Drug Push"))
ncon = sum(cont_msk)
df_i$y_coord[cont_msk] = seq(from=0,to=40,length.out = ncon)
push_msk = (df_i$med_end_hr - df_i$med_start_hr < 0.2) & !cont_msk
df_i$med_end_hr_plt = df_i$med_end_hr
df_i$med_end_hr_plt[push_msk] = df_i$med_end_hr[push_msk] + 0.5

entry_time = min(hour(df_g$intime)+minute(df_g$intime)/60)

# the plot
p1 = ggplot(df_g) +
  geom_line(aes(x = tos_hr+entry_time, y = glucose_b),size=0.5) +
  geom_point(aes(x = tos_hr+entry_time, y = glucose_b)) +
  geom_segment(data=df_i,
               aes(x=med_start_hr+entry_time,xend=med_end_hr_plt+entry_time,
                             y=y_coord,yend=y_coord,
                   col=ordercategorydescription,
                   alpha=insulin_rate),size=2.0) +
  coord_cartesian(ylim = c(0,max(df_g$glucose_b)),
                  xlim=c(entry_time,entry_time+24)) +
  scale_x_continuous(breaks = seq(0,48,4),
                     labels=c(0,rep(seq(4,24,4),2))) +
scale_alpha_continuous(range = c(0.5,1)) +
  theme_bw() +
  labs(x = "Calendar time (24h)", y = "Blood glucose\n(mg/dL)",
       title = "First 24 hours in ICU") +
  theme(legend.position = "bottom",legend.title = element_blank()) +
  guides(alpha=FALSE) +
  scale_color_manual(values = c("#F8766D", "#00BFC4"),
                    labels = c("Continuous Med", "Drug Push"), 
                    drop = FALSE)
df_i = df_i[order(med_end_hr)]
df_i$c_insulin_amount = cumsum(df_i$insulin_amount)
tmp = df_i[nrow(df_i)]
tmp$med_end_hr = tmp$med_end_hr + 12
df_i = rbind(df_i,tmp)
p2 = ggplot(df_i) +
  geom_line(aes(x = med_end_hr+entry_time, y = c_insulin_amount),size=0.5)+
  theme_bw() +
  labs(y = "Cumulative\nInsulin (units)",x = "Calendar time (24h)") +
  coord_cartesian(xlim=c(entry_time,entry_time+24)) +
  scale_x_continuous(breaks = seq(0,48,4),
                     labels=c(rep(seq(0,20,4),2),0))
pout = gridExtra::grid.arrange(p1,p2,heights=c(2,1))
loc = paste0("plots/insulin_glucose_example_",Sys.Date(),".png")
ggsave(loc,plot = pout)

## D) BY ICU TYPE/ID -----------------------------------------------------------
icustays <- fread("data/raw/icustays_202003131320.csv")
icustays <- icustays[icustays$icustay_id %in% mimic_sugar$icustay_id,]
# merge to get stay unit
mimic_sugar <- merge(mimic_sugar,icustays[,c("icustay_id","first_careunit","first_wardid")],
                         all.x=TRUE,by = "icustay_id")
# ever given insulin
mimic_sugar$insulin_ever <- !is.na(mimic_sugar$insulin_amount)

# table by care unit
t1 <- mimic_sugar[insulin_ever==TRUE,.(N_treat = .N),by=c("first_careunit")][order(first_careunit)]
t2 <- mimic_sugar[insulin_ever==FALSE,.(N_notreat = .N),by=c("first_careunit")][order(first_careunit)]
tab <- merge(t1,t2)
tab$p_treat <- tab$N_treat/(tab$N_treat + tab$N_notreat)
tab

# table by ward
t1 <- mimic_sugar[insulin_ever==TRUE,.(N_treat = .N),by=c("first_wardid")][order(first_wardid)]
t2 <- mimic_sugar[insulin_ever==FALSE,.(N_notreat = .N),by=c("first_wardid")][order(first_wardid)]
tab <- merge(t1,t2)
tab$p_treat <- tab$N_treat/(tab$N_treat + tab$N_notreat)
tab



