#############################################################################
#
# Glycaemic control in the ICU
# Exploratory data analysis: mortality rates
# 
# Contents:
#
#
# Init: Oisin (19/03/2020)
#############################################################################

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
pt_outcome = fread("data/raw/pt_icu_outcome_202003131320.csv")
pt_outcome = pt_outcome[pt_outcome$icustay_id %in% mimic_sugar$icustay_id,]
dim(pt_outcome)

# remove missing ICU in and exit time
remove_id = unique(mimic_sugar$icustay_id[is.na(mimic_sugar$intime)])
length(remove_id)
mimic_sugar = mimic_sugar[!icustay_id %in% remove_id,]

## A) FEATURES ETC ---------------------------------------------------------
# for row with bediside xor lab value use non-NULL val
mimic_sugar$glucose_bl = fcoalesce(mimic_sugar$glucose_b,
                                     mimic_sugar$glucose_l)
# subset to rows with a glucose measure
mimic_sugar_c = mimic_sugar[!is.na(glucose_bl),]

# treated with insulin
mimic_sugar_c$insulin_rate[is.na(mimic_sugar_c$insulin_rate)] = 0
mimic_sugar_c$insulin_amount[is.na(mimic_sugar_c$insulin_amount)] = 0
treated_ids = unique(mimic_sugar_c$icustay_id[mimic_sugar_c$insulin_amount > 0])
mimic_sugar_c$insulin_ever = 1*(mimic_sugar_c$icustay_id %in% treated_ids)
mimic_sugar_c$insulin_ever = ifelse(mimic_sugar_c$insulin_ever==1,"treated","untreated")

# mortality outcomes
pt_outcome$dod = as_datetime(pt_outcome$dod)
pt_outcome$intime = as_datetime(pt_outcome$intime)
pt_outcome$died30 = as.numeric(pt_outcome$ttd_days < 30)
pt_outcome$died30[is.na(pt_outcome$died30)] = 0
pt_outcome$died_icu = as.numeric(pt_outcome$ttd_days <= pt_outcome$los)

## B) MORTALITY -----------------------------------------------------------
pt_outcome[,.(died = mean(expire_flag))]
pt_outcome[,.(died30 = mean(died30))]

## C) MORTALITY BY GLUCOSE -------------------------------------------------
# average glucose
av_glucose = mimic_sugar_c[,.(glucose = mean(glucose_bl),
                              n=.N,
                              insulin=max(insulin_ever)),
                           by=icustay_id]

# join dataset
pt_outcome_m = merge(pt_outcome, av_glucose, by="icustay_id", all.x=TRUE)
pt_outcome_c = pt_outcome_m[glucose < 500 & glucose > 0 & los > 0.25]
hist(pt_outcome_c$glucose)

# mortality by glucose group
pt_outcome_c$glucose_groups = cut(pt_outcome_c$glucose,breaks = 20,
                                  ordered=TRUE)
mort_glucose = pt_outcome_c[,.(died30=mean(died30,na.rm = T),.N),
             by=glucose_groups][order(glucose_groups)]
mort_glucose$se = sqrt(mort_glucose$died30*(1-mort_glucose$died30)*1/(mort_glucose$N))
mort_glucose$glucose_groups_plt = mort_glucose$glucose_groups
levels(mort_glucose$glucose_groups_plt) = seq(50,480,length=20)
mort_glucose$glucose_groups_plt = as.numeric(as.character(mort_glucose$glucose_groups_plt))
br = 500/21
brks = seq(0,500,100)
pout = mort_glucose[N>10 & !is.na(glucose_groups_plt)] %>%
  ggplot(aes(x=glucose_groups_plt,y=died30)) +
  geom_point() +
  geom_pointrange(aes(ymax=died30+2*se,ymin=died30-2*se)) +
  geom_smooth(method="loess",formula="y~x",se = F) +
  theme_bw() +
  coord_cartesian(xlim=c(0,400)) +
  labs(x="Blood glucose (ml/dL)",y="Mortality rate (30 day)")
pout
loc = paste0("plots/mortality_glucose_",Sys.Date(),".png")
ggsave(loc,plot = pout)

# accouting for insulin treatment
pt_outcome_c$glucose_groups1 = cut(pt_outcome_c$glucose,breaks = 20,
                                  ordered=TRUE)

mort_glucose_insulin = pt_outcome_c[,.(died30=mean(died30,na.rm = T),.N),
                      by=.(glucose_groups1,insulin)][order(glucose_groups1)]
mort_glucose_insulin$se = sqrt(mort_glucose_insulin$died30*
                                 (1-mort_glucose_insulin$died30)*
                                 1/(mort_glucose_insulin$N))

mort_glucose_insulin$glucose_groups1_plt = mort_glucose_insulin$glucose_groups1
levels(mort_glucose_insulin$glucose_groups1_plt) = seq(50,480,length=20)
mort_glucose_insulin$glucose_groups1_plt = as.numeric(as.character(mort_glucose_insulin$glucose_groups1_plt))
pout = mort_glucose_insulin[N>10 & !is.na(glucose_groups1_plt)] %>%
  ggplot(aes(x=glucose_groups1_plt,y=died30,col=insulin)) +
  geom_point() +
  geom_pointrange(aes(ymax=died30+2*se,ymin=died30-2*se)) +
  geom_smooth(method="loess",formula="y~x",se = F) +
  scale_color_discrete(name="Treated with\ninsulin",labels=c("Yes","No")) + 
  theme_bw() +
  coord_cartesian(xlim=c(0,400)) +
  labs(x="Average blood glucose (ml/dL)",y="Mortality rate (30 day)")
pout
loc = paste0("plots/mortality_glucose_tmt_",Sys.Date(),".png")
ggsave(loc,plot = pout)



