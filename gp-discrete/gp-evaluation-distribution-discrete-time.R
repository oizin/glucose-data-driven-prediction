rm(list = ls())

library(catboost)
library(data.table)
library(ggplot2)
library(magrittr)
library(ega)  # for clarke error grid
source("src/ml-pipe-data-processing.R")
source("src/ml-pipe-evaluation.R")

glycaemic_test <- readRDS(file = "data/processed/glycaemic_test_mvonly_dtime_2.rds")
glycaemic_train <- readRDS("scripts/gp-discrete/glycaemic_train_2_catboost.rds")
glycaemic_train <- glycaemic_train[,!grepl("xx_glucose_l",names(glycaemic_train)),with=FALSE]
vars <- names(glycaemic_train)[grepl(pattern = "xx_",names(glycaemic_train))]

length(unique(glycaemic_test$icustay_id))

## prepare dataset
glycaemic_test[,glucose_n := cumsum(!is.na(xx_glucose_b)),by=icustay_id]
glycaemic_test[!is.na(glucose_b_shift1),glucose_n_time := tstep]
#glycaemic_test[,glucose_n_time := nafill(glucose_n_time,"nocb"),by=icustay_id]

# glucose
tmp = glycaemic_test[,.(icustay_id,tstep,glucose_n,glucose_b_shift1,xx_glucose_b,xx_glucose_l)]
tmp[!is.na(xx_glucose_b),xx_glucose_b_time := tstep]
tmp[!is.na(xx_glucose_l),xx_glucose_l_time := tstep]
tmp = tmp[,.(tstep=max(tstep),
             glucose_b_shift1=mean(glucose_b_shift1,na.rm=TRUE),
             xx_glucose_b=mean(xx_glucose_b,na.rm=TRUE),
             xx_glucose_l=mean(xx_glucose_l,na.rm=TRUE),
             xx_glucose_b_time=mean(xx_glucose_b_time,na.rm=TRUE),
             xx_glucose_l_time=mean(xx_glucose_l_time,na.rm=TRUE)),
          by=.(icustay_id,glucose_n)]
tmp = cbind(tmp,
            tmp[,shift(xx_glucose_b,1:12,give.names=TRUE),by=icustay_id][,-1],
            tmp[,shift(xx_glucose_l,1:12,give.names=TRUE),by=icustay_id][,-1],
            tmp[,shift(xx_glucose_b_time,1:12,give.names=TRUE),by=icustay_id][,-1] - tmp$xx_glucose_b_time,
            tmp[,shift(xx_glucose_l_time,1:12,give.names=TRUE),by=icustay_id][,-1] - tmp$xx_glucose_l_time
)
tmp[,xx_glucose_b_time := xx_glucose_b_time - tstep]
tmp[,xx_glucose_l_time := xx_glucose_l_time - tstep]
tmp[,xx_glucose_l_time_lag_0 := xx_glucose_l_time]
tmp[,xx_glucose_b_time_lag_0 := xx_glucose_b_time]
tmp[,xx_glucose_b_time := NULL]
tmp[,xx_glucose_l_time := NULL]
tmp[,xx_glucose_b_lag_0 := xx_glucose_b]
tmp[,xx_glucose_l_lag_0 := xx_glucose_l]
tmp[,xx_glucose_b := NULL]
tmp[,xx_glucose_l := NULL]
tmp$glucose_b_shift1 = NULL

# other variables
glycaemic_test <- prepare_mimic_catboost_outcome(glycaemic_test)

# join in lagged glucose
glycaemic_test[,xx_glucose_b := NULL]
glycaemic_test[,xx_glucose_l := NULL]
nrow(glycaemic_test)
glycaemic_test <- merge(glycaemic_test,tmp,
                         by=c("icustay_id","tstep","glucose_n"),
                         all.x=TRUE)
nrow(glycaemic_test)
names(glycaemic_test) <- make.names(names(glycaemic_test))

# Delete and subset ------------------------------------------------------------

# subset to measured bedside glucose
glycaemic_test[glucose_b_shift1 > 1000,glucose_b_shift1 := glucose_b_shift1/100.0] 
glycaemic_test[,glucose_measured := ifelse(is.na(glucose_b_shift1),0,1)]

# subset
glycaemic_test <- glycaemic_test[glucose_measured == 1]

# fix NaNs
invisible(lapply(names(glycaemic_test),function(.name) set(glycaemic_test, which(is.nan(glycaemic_test[[.name]])), j = .name,value =NA)))

# keep for later
#saveRDS(object = glycaemic_test,file = "scripts/gp-discrete/glycaemic_test_2_catboost.rds",compress=FALSE)
#glycaemic_test <- readRDS( "scripts/gp-discrete/glycaemic_test_2_catboost.rds")

## training/validation set split
y_test <- as.numeric(glycaemic_test[["glucose_b_shift1"]])

# convert to matrix
X_test <- data.matrix(glycaemic_test[,vars, with=FALSE])
test_pool <- catboost.load_pool(data = X_test)

## locf -------------------------------------------------------------------------

glycaemic_test[,glucose_locf := shift(glucose_b_shift1),by=icustay_id]

glycaemic_test[!is.na(glucose_locf),error1 := ((glucose_locf - glucose_b_shift1))]
glycaemic_test[!is.na(glucose_locf),error2 := ((glucose_locf - glucose_b_shift1)^2)]
glycaemic_test[!is.na(glucose_locf),deviation := abs((glucose_locf - glucose_b_shift1)/glucose_b_shift1)]

mape <- mean(glycaemic_test$deviation[glycaemic_test$tstep > 0])
rmse <- sqrt(mean(glycaemic_test$error2[glycaemic_test$tstep > 0]))
mad <- median(abs(glycaemic_test$error1[glycaemic_test$tstep > 0]))
cat("MAPE: ",mape,"\n")
cat("RMSE: ",rmse,"\n")
cat("MAD: ",mad,"\n")

## catboost - outcome model -----------------------------------------------------
bst <- catboost.load_model("data/models/cb.cbm")

glycaemic_test[,glucose_bst := catboost.predict(bst,test_pool)]
#glycaemic_test[,glucose_bst := (pred)]
glycaemic_test$glucose_b_shift1 <- y_test
glycaemic_test[,error1 := ((glucose_bst - glucose_b_shift1))]
glycaemic_test[,error2 := ((glucose_bst - glucose_b_shift1)^2)]
glycaemic_test[,deviation := abs((glucose_bst - glucose_b_shift1)/glucose_b_shift1)]

mad <- mean(glycaemic_test$deviation[glycaemic_test$tstep > 0])
rmse <- sqrt(mean(glycaemic_test$error2[glycaemic_test$tstep > 0]))
cat("MAD: ",mad,"\n")
cat("RMSE: ",rmse,"\n")

# hypoglycaemic
glycaemic_test[glucose_b_shift1 < 70,.(.N,sum((glucose_bst) < 70),sum((glucose_bst) > 70))]

## catboost - outcome + uncertainty prediction model ----------------------------

library(reticulate)
source_python('scripts/gp-discrete/catboost_prediction.py')

# join in clusters
cluster_df <- fread("data/models/clustering_k3.csv")
#cluster_df <- cluster_df[mimic==TRUE]
glycaemic_test <- merge(glycaemic_test,cluster_df[,.(icustay_id,cluster)],by="icustay_id",all.x=TRUE)

glycaemic_test[,glucose_bst := exp(prediction_catboost(X_test)[,1])]
#glycaemic_test[,glucose_bst := (pred)]
glycaemic_test$glucose_b_shift1 <- y_test
glycaemic_test[,error1 := ((glucose_bst - glucose_b_shift1))]
glycaemic_test[,error2 := ((glucose_bst - glucose_b_shift1)^2)]
glycaemic_test[,deviation := abs((glucose_bst - glucose_b_shift1)/glucose_b_shift1)]

mape <- mean(glycaemic_test$deviation[glycaemic_test$tstep > 0])
rmse <- sqrt(mean(glycaemic_test$error2[glycaemic_test$tstep > 0]))
mad <- median(abs(glycaemic_test$error1[glycaemic_test$tstep > 0]))
cat("MAPE: ",mape,"\n")
cat("RMSE: ",rmse,"\n")
cat("MAD: ",mad,"\n")

# by cluster
glycaemic_test[tstep > 0,.(mape = mean(deviation),
                           rmse=sqrt(mean(error2)),
                           mad = median(abs(error1)),
                           .N),by=cluster]
glycaemic_test[tstep == 0,.N,by=cluster]
glycaemic_test[tstep == 0,sum(ttd_days < 30,na.rm=TRUE)/.N,by=cluster]
glycaemic_test[tstep == 0,.(mean(dm == 1)),by=cluster]
glycaemic_test[tstep > 0 & tstep < 12,.(mean(glucose_b_shift1),sd(glucose_b_shift1)),by=cluster]
glycaemic_test[tstep > 12 & tstep < 24,.(mean(glucose_b_shift1),sd(glucose_b_shift1)),by=cluster]
glycaemic_test[tstep > 24 & tstep < 36,.(mean(glucose_b_shift1),sd(glucose_b_shift1)),by=cluster]

# by ICU
glycaemic_test[tstep > 0,.(mad = mean(deviation)),by=first_careunit]
glycaemic_test[tstep > 0 & first_careunit %in% c("CSRU","TSICU","SICU"),.(mad = mean(deviation))]
              
# blood glucose level
msk <- glycaemic_test$tstep > 0 & glycaemic_test$glucose_b_shift1 < 70
glycaemic_test[msk,.(mad = mean(deviation),.N)]
msk <- glycaemic_test$tstep > 0 & glycaemic_test$glucose_b_shift1 > 200
glycaemic_test[msk,.(mad = mean(deviation),.N)]
msk <- glycaemic_test$tstep > 0 & glycaemic_test$glucose_b_shift1 > 100 &
  glycaemic_test$glucose_b_shift1 < 200
glycaemic_test[msk,.(mad = mean(deviation),.N)]

# keto
msk <- grepl(pattern = "ketoacidosis",tolower(glycaemic_test$diagnosis))
glycaemic_test[msk,.(mad = mean(deviation),.N)]

# emergency
glycaemic_test[admission_type == "EMERGENCY",.(mad = mean(deviation),.N)]
glycaemic_test[admission_type == "ELECTIVE",.(mad = mean(deviation),.N)]


# coverage
glycaemic_test[,glucose_bst := (prediction_catboost(X_test)[,1])]
glycaemic_test[,glucose_bst_sd := (prediction_catboost(X_test)[,2])]
glycaemic_test[,glucose_bst_l := exp(glucose_bst - 2*(glucose_bst_sd))]
glycaemic_test[,glucose_bst_u := exp(glucose_bst + 2*(glucose_bst_sd))]
glycaemic_test$glucose_b_shift1 <- y_test

# evaluation
cat("coverage:\n")
sum((y_test > (glycaemic_test$glucose_bst_l)) & 
      (y_test < (glycaemic_test$glucose_bst_u)))/length(y_test)

# hypoglycaemia
msk <- glycaemic_test$tstep > 0 & glycaemic_test$glucose_b_shift1 < 70
sum((y_test[msk] > (glycaemic_test$glucose_bst_l[msk])) & 
      (y_test[msk] < (glycaemic_test$glucose_bst_u[msk])))/length(y_test[msk])

# hyperglycaemia
msk <- glycaemic_test$tstep > 0 & glycaemic_test$glucose_b_shift1 > 200
sum((y_test[msk] > (glycaemic_test$glucose_bst_l[msk])) & 
      (y_test[msk] < (glycaemic_test$glucose_bst_u[msk])))/length(y_test[msk])

# hypoglycaemia
glycaemic_test[glucose_b_shift1 < 70,.(.N,sum(exp(glucose_bst) < 70),sum(exp(glucose_bst) > 70))]

# cluster
msk <- glycaemic_test$cluster == 1 & !is.na(glycaemic_test$cluster)
sum((y_test[msk] > (glycaemic_test$glucose_bst_l[msk])) & 
      (y_test[msk] < (glycaemic_test$glucose_bst_u[msk])),na.rm=TRUE)/length(y_test[msk])
msk <- glycaemic_test$cluster == 2 & !is.na(glycaemic_test$cluster)
sum((y_test[msk] > (glycaemic_test$glucose_bst_l[msk])) & 
      (y_test[msk] < (glycaemic_test$glucose_bst_u[msk])),na.rm=TRUE)/length(y_test[msk])
msk <- glycaemic_test$cluster == 3 & !is.na(glycaemic_test$cluster)
sum((y_test[msk] > (glycaemic_test$glucose_bst_l[msk])) & 
      (y_test[msk] < (glycaemic_test$glucose_bst_u[msk])),na.rm=TRUE)/length(y_test[msk])


if(!dir.exists("data/logs/cb_with_u")) {
  dir.create("data/logs/cb_with_u")
}
bst <- catboost.load_model("data/models/catboost_with_uncertainty.cbm")
glycaemic_test[,glucose_bst := exp(prediction_catboost(X_test)[,1])]

X_train <- X_test # hack
evaluation_plots_outcome(folder_path = "data/logs/cb_with_u",
                         dt = glycaemic_test,
                         model_name = "cb_with_u")

glycaemic_test$glucose_bins1 <- cut(glycaemic_test$glucose_b_shift1,c(0,70,100,140,170,200,Inf),ordered_result = TRUE,right=FALSE)
tmp = glycaemic_test[,.(coverage=sum((glucose_b_shift1 > glucose_bst_l) & 
                        (glucose_b_shift1 < glucose_bst_u))/.N),by=glucose_bins1]

p1 <- ggplot(tmp,aes(x=glucose_bins1,y=coverage)) +
  geom_point(size=2) +
  geom_hline(yintercept = 0.95,col="red",linetype=2) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(x = "Blood glucose (mg/dL)",
       y= "95% prediction interval coverage",title = "(A)") +
  scale_x_discrete(labels = c("< 70","70-99","100-139","140-169","170-199","\u2265 200")) +
  scale_y_continuous(labels=seq(0.4,1.0,0.1)*100,breaks=seq(0.4,1.0,0.1))
ggsave("data/logs/cb_with_u/interval_coverage.png",p1)

tmp = glycaemic_test[,.(MAD=mean(deviation),.N),by=glucose_bins1]
tmp$cut <- ordered(tmp$cut)
p2 <- ggplot(tmp,aes(x = glucose_bins1,y = MAD)) +
  geom_point(aes(size=N)) +
  scale_y_continuous(breaks = seq(0,1,0.2),labels=seq(0,1,0.2)*100) +
  coord_cartesian(ylim = c(0,1)) +
  theme_minimal(base_size = 14) +
  labs(x = "Blood glucose (mg/dL)",y = "Mean absolute\npercentage error (%)",title = "(B)") +
  scale_size_continuous(name = "Number of\nmeasurements") +
  theme(axis.text.x = element_text(angle = 45),legend.position = "top") +
  scale_x_discrete(labels = c("< 70","70-99","100-139","140-169","170-199","\u2265 200"))
  
g <- gridExtra::arrangeGrob(p1,p2,ncol = 2) #generates g
ggsave(filename = "data/logs/cb_with_u/summary_performance.png",plot = g,width = 9,height = 5)

p2 <- ggplot(tmp,aes(x = glucose_bins1,y = MAD)) +
  geom_point(aes(size=N)) +
  scale_y_continuous(breaks = seq(0,1,0.2),labels=seq(0,1,0.2)*100) +
  coord_cartesian(ylim = c(0,1)) +
  theme_minimal(base_size = 20) +
  labs(x = "Blood glucose (mg/dL)",y = "Mean absolute\npercentage error (%)",title = "") +
  scale_size_continuous(name = "Number of\nmeasurements") +
  theme(axis.text.x = element_text(angle = 45,hjust=1),legend.position = "top") +
  scale_x_discrete(labels = c("< 70","70-99","100-139","140-169","170-199","\u2265 200"))

ggsave(plot=p2,filename = "data/logs/cb_with_u/summary_performance_p2.png",width=7)

# examples: patient 209048,299956,200034 
# fill in missing
fix_missing <- function(dt,n) {
  for (i in 0:n) {
    # time
    expr1 <- paste0("xx_glucose_b_time_lag_",i,":= xx_glucose_b_time_lag_",i,"[1] - (1:.N) + 1")
    expr2 <- paste0("by=.(icustay_id , cumsum(!is.na(xx_glucose_b_time_lag_",i,")))")
    dt[,eval(parse(text = expr1)),eval(parse(text = expr2)[[1L]])]
    expr3 <- paste0("xx_glucose_l_time_lag_",i,":= xx_glucose_l_time_lag_",i,"[1] - (1:.N) + 1")
    expr4 <- paste0("by=.(icustay_id, cumsum(!is.na(xx_glucose_l_time_lag_",i,")))")
    dt[,eval(parse(text = expr3)),eval(parse(text = expr4)[[1L]])]
    # values
    expr1 <- paste0("xx_glucose_b_lag_",i,":= nafill(xx_glucose_b_lag_",i,",'locf')")
    expr2 <- paste0("by=.(icustay_id)")
    dt[,eval(parse(text = expr1)),eval(parse(text = expr2)[[1L]])]
    expr1 <- paste0("xx_glucose_l_lag_",i,":= nafill(xx_glucose_l_lag_",i,",'locf')")
    expr2 <- paste0("by=.(icustay_id)")
    dt[,eval(parse(text = expr3)),eval(parse(text = expr4)[[1L]])]
  }
  dt
}

glycaemic_full <- readRDS("scripts/gp-discrete/glycaemic_test_2_catboost.rds")



# new

c_df <- glycaemic_test[,.(glucose_b_shift1 = mean(glucose_b_shift1)),by=.(tstep,cluster)]
# cluster 1
glycaemic_pat <- glycaemic_test[icustay_id == sample(glycaemic_test$icustay_id[glycaemic_test$cluster == 1 & glycaemic_test$tstep > 40],1)]
invisible(lapply(names(glycaemic_pat),function(.name) set(glycaemic_pat, which(is.nan(glycaemic_pat[[.name]])), j = .name,value =NA)))
glycaemic_pat <- fix_missing(glycaemic_pat,12)
glycaemic_pat[,.(icustay_id,tstep,xx_glucose_b_lag_0,xx_glucose_b_time_lag_0)]
p1 <- glycaemic_test[tstep >= 0 &icustay_id == 293960 ] %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,48)) +
  geom_point(aes(x=tstep,y=glucose_b_shift1)) +
  #geom_line(aes(x = tstep,y=exp(glucose_bst))) +
  geom_smooth(aes(x = tstep,y=exp(glucose_bst)),span=0.15,se=FALSE) +
  geom_line(data=c_df[cluster == 1],aes(x=tstep,y=glucose_b_shift1),linetype=2,col="red") +
  geom_ribbon(aes(x=tstep,ymin=glucose_bst_l, ymax=glucose_bst_u),alpha=0.4,fill="lightblue") +
  labs(x = "Time in ICU (hours)", y = "Blood glucose (mg/dL)",title = "(D)") +
  theme_minimal() + 
  coord_cartesian(ylim=c(0,350))

# cluster 2
p2 <- glycaemic_test[tstep >= 0  & icustay_id == 232536] %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,48)) +
  geom_point(aes(x=tstep,y=glucose_b_shift1)) +
  #geom_line(aes(x = tstep,y=exp(glucose_bst))) +
  geom_smooth(aes(x = tstep,y=exp(glucose_bst)),span=0.2,se=FALSE) +
  geom_line(data=c_df[cluster == 2],aes(x=tstep,y=glucose_b_shift1),linetype=2,col="red") +
  geom_ribbon(aes(x=tstep,ymin=glucose_bst_l, ymax=glucose_bst_u),alpha=0.4,fill="lightblue") +
  labs(x = "Time in ICU (hours)", y = "Blood glucose (mg/dL)",title = "(A)") +
  theme_minimal() + 
  coord_cartesian(ylim=c(0,350))

# cluster 3
# glycaemic_pat <- glycaemic_test[icustay_id == sample(glycaemic_test$icustay_id[glycaemic_test$cluster == 3 & glycaemic_test$tstep > 40],1)]
# invisible(lapply(names(glycaemic_pat),function(.name) set(glycaemic_pat, which(is.nan(glycaemic_pat[[.name]])), j = .name,value =NA)))
# glycaemic_pat <- fix_missing(glycaemic_pat,12)
# glycaemic_pat[,.(icustay_id,tstep,xx_glucose_b_lag_0,xx_glucose_b_time_lag_0)]
p3 <- glycaemic_test[tstep >= 0 & icustay_id == 299407 ] %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,48)) +
  geom_point(aes(x=tstep,y=glucose_b_shift1)) +
  #geom_line(aes(x = tstep,y=exp(glucose_bst))) +
  geom_smooth(aes(x = tstep,y=exp(glucose_bst)),span=0.2,se=FALSE) +
  geom_line(data=c_df[cluster == 3],aes(x=tstep,y=glucose_b_shift1),linetype=2,col="red") +
  geom_ribbon(aes(x=tstep,ymin=glucose_bst_l, ymax=glucose_bst_u),alpha=0.4,fill="lightblue") +
  labs(x = "Time in ICU (hours)", y = "Blood glucose (mg/dL)",title = "(B)") +
  theme_minimal() + 
  coord_cartesian(ylim=c(0,350))

# cluster 3
p4 <- glycaemic_test[tstep >= 0 & icustay_id == 217318] %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,48)) +
  geom_point(aes(x=tstep,y=glucose_b_shift1)) +
  #geom_line(aes(x = tstep,y=exp(glucose_bst))) +
  geom_line(data=c_df[cluster == 3],aes(x=tstep,y=glucose_b_shift1),linetype=2,col="red") +
  geom_smooth(aes(x = tstep,y=exp(glucose_bst)),span=0.2,se=FALSE) +
  geom_ribbon(aes(x=tstep,ymin=glucose_bst_l, ymax=glucose_bst_u),alpha=0.4,fill="lightblue") +
  labs(x = "Time in ICU (hours)", y = "Blood glucose (mg/dL)",title = "(C)") +
  theme_minimal() + 
  coord_cartesian(ylim=c(0,350))

gridExtra::grid.arrange(p2,p3,p4,p1)
g <- gridExtra::arrangeGrob(p2,p3,p4,p1) #generates g
ggsave(filename = "data/logs/cb_with_u/cluster_examples.png",plot = g,width = 9,height = 5)

# old
225951
238191 
261612 
240212 
224886 

p1 <- glycaemic_full[tstep > 0 &icustay_id == 240212] %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,48)) +
  geom_point(aes(x=tstep,y=glucose_b_shift1)) +
  geom_smooth(aes(x = tstep,y=(glucose_bst)),span=0.08,se=FALSE) +
  geom_ribbon(aes(x=tstep,ymin=glucose_bst_l, ymax=glucose_bst_u),alpha=0.4,fill="lightblue") +
  labs(x = "Time in ICU (hours)", y = "Blood glucose (mg/dL)",title="(A)") +
  theme_minimal(base_size = 12) + 
  coord_cartesian(ylim=c(0,400))


p2 <- glycaemic_full[tstep > 0 &icustay_id == 224886] %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,48)) +
  geom_point(aes(x=tstep,y=glucose_b_shift1)) +
  geom_smooth(aes(x = tstep,y=(glucose_bst)),span=0.08,se=FALSE) +
  geom_ribbon(aes(x=tstep,ymin=glucose_bst_l, ymax=glucose_bst_u),alpha=0.4,fill="lightblue") +
  labs(x = "Time in ICU (hours)", y = "Blood glucose (mg/dL)",title="(B)") +
  theme_minimal(base_size = 12) + 
  coord_cartesian(ylim=c(0,400))

p3 <- glycaemic_full[tstep > 0 &icustay_id == 261612] %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,48)) +
  geom_point(aes(x=tstep,y=glucose_b_shift1)) +
  geom_smooth(aes(x = tstep,y=(glucose_bst)),span=0.08,se=FALSE) +
  geom_ribbon(aes(x=tstep,ymin=glucose_bst_l, ymax=glucose_bst_u),alpha=0.4,fill="lightblue") +
  labs(x = "Time in ICU (hours)", y = "Blood glucose (mg/dL)",title="(C)") +
  theme_minimal(base_size = 12) + 
  coord_cartesian(ylim=c(0,400))

p4 <- glycaemic_full[tstep > 0 &icustay_id == 225951] %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,48)) +
  geom_point(aes(x=tstep,y=glucose_b_shift1)) +
  #geom_line(aes(x = tstep,y=(glucose_bst))) +
  geom_smooth(aes(x = tstep,y=(glucose_bst)),span=0.1,se=FALSE) +
  geom_ribbon(aes(x=tstep,ymin=glucose_bst_l, ymax=glucose_bst_u),alpha=0.4,fill="lightblue") +
  labs(x = "Time in ICU (hours)", y = "Blood glucose (mg/dL)",title="(D)") +
  theme_minimal(base_size = 12) + 
  coord_cartesian(ylim=c(0,400))

gridExtra::grid.arrange(p1,p2,p3,p4)
g <- gridExtra::arrangeGrob(p1,p2,p3,p4) #generates g
ggsave(filename = "data/logs/cb_with_u/examples.png",plot = g,width = 9,height = 5)

## catboost - quantile prediction model -----------------------------------------

# models
bst_m <- catboost.load_model("data/models/bst_m.cbm")
bst_l <- catboost.load_model("data/models/bst_l.cbm")
bst_u <- catboost.load_model("data/models/bst_u.cbm")

#bst <- catboost.load_model("data/models/catboost_with_uncertainty.cbm")


# data
test_pool <- catboost.load_pool(data = X_test)

# prediction
predl <- catboost.predict(bst_l, test_pool)
pred <- catboost.predict(bst_m, test_pool)
predu <- catboost.predict(bst_u, test_pool)
glycaemic_test[,glucose_bst_l := exp(predl)]
glycaemic_test[,glucose_bst := exp(pred)]
glycaemic_test[,glucose_bst_u := exp(predu)]
glycaemic_test$glucose_b_shift1 <- y_test

# prediction
glycaemic_test$glucose_b_shift1 <- y_test
glycaemic_test[,error1 := ((glucose_bst - glucose_b_shift1))]
glycaemic_test[,error2 := ((glucose_bst - glucose_b_shift1)^2)]
glycaemic_test[,deviation := abs((glucose_bst - glucose_b_shift1)/glucose_b_shift1)]

mape <- mean(glycaemic_test$deviation[glycaemic_test$tstep > 0])
rmse <- sqrt(mean(glycaemic_test$error2[glycaemic_test$tstep > 0]))
mad <- median(abs(glycaemic_test$error1[glycaemic_test$tstep > 0]))
cat("MAPE: ",mape,"\n")
cat("RMSE: ",rmse,"\n")
cat("MAD: ",mad,"\n")


# evaluation
cat("coverage:\n")
msk <- glycaemic_test$tstep > 0
sum((y_test[msk] > exp(predl[msk])) & 
      (y_test[msk] < exp(predu[msk])))/length(y_test[msk])

# hypoglycaemia
msk <- glycaemic_test$tstep > 0 & glycaemic_test$glucose_b_shift1 < 70
sum((y_test[msk] > exp(predl[msk])) & 
      (y_test[msk] < exp(predu[msk])))/length(y_test[msk])

msk <- glycaemic_test$tstep > 0 & glycaemic_test$glucose_b_shift1 < 70

sum(predl < 70 & y_test < 70)
sum(predl < 70 & y_test > 70)


# hyperglycaemia
msk <- glycaemic_test$tstep > 0 & glycaemic_test$glucose_b_shift1 > 170
sum((y_test[msk] > exp(predl[msk])) & 
      (y_test[msk] < exp(predu[msk])))/length(y_test[msk])

# hypoglycaemia
glycaemic_test[glucose_b_shift1 < 70,.(.N,sum(glucose_bst < 70),sum(glucose_bst > 70))]

bst <- bst_m

if(!dir.exists("data/logs/cb_q")) {
  dir.create("data/logs/cb_q")
}

X_train <- X_test # hack for colnames(X_train)
evaluation_plots_outcome(folder_path = "data/logs/cb_q",
                         dt = glycaemic_test,
                         model_name = "cb_q")

tmp = glycaemic_test[,.(coverage=sum((glucose_b_shift1 > glucose_bst_l) & 
                                       (glucose_b_shift1 < glucose_bst_u))/.N),by=glucose_bins1]

ggplot(tmp,aes(x=glucose_bins1,y=coverage)) +
  geom_point(size=2) +
  geom_hline(yintercept = 0.95,col="red",linetype=2) +
  theme_bw(base_size = 12) +
  labs(x = "Measured blood glucose (mg/dL)",
       y= "95% prediction interval coverage") +
  scale_x_discrete(labels = c("< 70","70-99","100-139","140-169","170-199","\u2265 200")) +
  scale_y_continuous(labels=seq(0.4,1.0,0.1)*100,breaks=seq(0.4,1.0,0.1))
ggsave("data/logs/cb_q/interval_coverage.png")
