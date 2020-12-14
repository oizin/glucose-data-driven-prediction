rm(list = ls())

library(catboost)
library(imputeTS)
library(data.table)
library(ggplot2)
library(magrittr)
library(ega)  # for clarke error grid
source("src/ml-pipe-data-processing.R")
source("src/ml-pipe-evaluation.R")

glycaemic_train <- readRDS(file = "data/processed/glycaemic_train_mvonly_dtime_2.rds")
length(unique(glycaemic_train$icustay_id))

## prepare dataset
glycaemic_train[,glucose_n := cumsum(!is.na(xx_glucose_b)),by=icustay_id]
glycaemic_train[!is.na(glucose_b_shift1),glucose_n_time := tstep]
#glycaemic_train[,glucose_n_time := nafill(glucose_n_time,"nocb"),by=icustay_id]

# glucose
tmp = glycaemic_train[,.(icustay_id,tstep,glucose_n,glucose_b_shift1,xx_glucose_b,xx_glucose_l)]
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
glycaemic_train <- prepare_mimic_catboost_outcome(glycaemic_train)

# join in lagged glucose
glycaemic_train[,xx_glucose_b := NULL]
glycaemic_train[,xx_glucose_l := NULL]
nrow(glycaemic_train)
glycaemic_train <- merge(glycaemic_train,tmp,
                         by=c("icustay_id","tstep","glucose_n"),
                         all.x=TRUE)
nrow(glycaemic_train)

# Delete and subset ------------------------------------------------------------

# subset to measured bedside glucose
glycaemic_train[glucose_b_shift1 > 1000,glucose_b_shift1 := glucose_b_shift1/100.0] 
glycaemic_train[,glucose_measured := ifelse(is.na(glucose_b_shift1),0,1)]

# subset
glycaemic_train <- glycaemic_train[glucose_measured == 1]

# fix NaNs
invisible(lapply(names(glycaemic_train),function(.name) set(glycaemic_train, which(is.nan(glycaemic_train[[.name]])), j = .name,value =NA)))

# keep for later
saveRDS(object = glycaemic_train,file = "scripts/gp-discrete/glycaemic_train_2_catboost.rds",
        compress=FALSE)
#glycaemic_train <- readRDS( "scripts/gp-discrete/glycaemic_train_2_catboost.rds")

# remove lab glucose
#glycaemic_train <- glycaemic_train[,!grepl("xx_glucose_l",names(glycaemic_train)),with=FALSE]

# A: Train/validation split ----------------------------------------------------

## training/validation set split
IDs <- unique(glycaemic_train$icustay_id)
J <- length(unique(glycaemic_train$icustay_id))
set.seed(1234)
validation_IDs <- sample(IDs,floor(J*0.1))
training_IDs <- IDs[!IDs %in% validation_IDs]
glycaemic_validation <- glycaemic_train[icustay_id %in% validation_IDs]
glycaemic_train <- glycaemic_train[icustay_id %in% training_IDs]
y_train <- as.numeric(glycaemic_train[["glucose_b_shift1"]])
y_validation <- as.numeric(glycaemic_validation[["glucose_b_shift1"]])
set.seed(NULL)

# weights
train_weight <- 1
validation_weight <- 1

## catboost - outcome + uncertainty prediction model ----------------------------

# convert to matrix
X_train <- data.matrix(glycaemic_train[,grepl(pattern = "xx_",names(glycaemic_train)), with=FALSE])
X_validation <- data.matrix(glycaemic_validation[,grepl(pattern = "xx_",names(glycaemic_train)), with=FALSE])

library(reticulate)

# convert to catboost pool
train_pool <- catboost.load_pool(data = X_train, label = y_train)
validation_pool <- catboost.load_pool(data = X_validation, label = y_validation)
bst_cv <- catboost.cv(train_pool,  fold_count = 3,
                      params = list(loss_function = 'RMSE',custom_loss=c('MAPE','MAE'),
                                    iterations = 200, metric_period=10))

source_python('scripts/gp-discrete/catboost_fitting.py')
pred_py <- fit_catboost(X_train,log(y_train),X_validation,log(y_validation))
#pred <- pred_py[,1]
#bst <- catboost.load_model("data/models/catboost_with_uncertainty.cbm")
source_python('scripts/gp-discrete/catboost_prediction.py')

# prediction
#pred <- catboost.predict(bst, validation_pool)[,1]
glycaemic_validation[,glucose_bst := exp(prediction_catboost(X_validation)[,1])]
#glycaemic_validation[,glucose_bst := (pred)]
glycaemic_validation$glucose_b_shift1 <- y_validation
glycaemic_validation[,error1 := ((glucose_bst - glucose_b_shift1))]
glycaemic_validation[,error2 := ((glucose_bst - glucose_b_shift1)^2)]
glycaemic_validation[,deviation := abs((glucose_bst - glucose_b_shift1)/glucose_b_shift1)]

mad <- mean(glycaemic_validation$deviation[glycaemic_validation$tstep > 0])
rmse <- sqrt(mean(glycaemic_validation$error2[glycaemic_validation$tstep > 0]))
cat("MAD: ",mad,"\n")
cat("RMSE: ",rmse,"\n")

# coverage
glycaemic_validation[,glucose_bst := (prediction_catboost(X_validation)[,1])]
glycaemic_validation[,glucose_bst_sd := (prediction_catboost(X_validation)[,2])]
glycaemic_validation[,glucose_bst_l := exp(glucose_bst - 2*(glucose_bst_sd))]
glycaemic_validation[,glucose_bst_u := exp(glucose_bst + 2*(glucose_bst_sd))]
glycaemic_validation$glucose_b_shift1 <- y_validation
 
# evaluation
cat("coverage:")
sum((y_validation > (glycaemic_validation$glucose_bst_l)) & 
      (y_validation < (glycaemic_validation$glucose_bst_u)))/length(y_validation)

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

glycaemic_full <- readRDS("scripts/gp-discrete/glycaemic_train_2_catboost.rds")

glycaemic_pat <- glycaemic_full[icustay_id == sample(glycaemic_full$icustay_id,1)]
invisible(lapply(names(glycaemic_pat),function(.name) set(glycaemic_pat, which(is.nan(glycaemic_pat[[.name]])), j = .name,value =NA)))

glycaemic_pat <- fix_missing(glycaemic_pat,12)
glycaemic_pat[,.(icustay_id,tstep,xx_glucose_b_lag_0,xx_glucose_b_time_lag_0)]
pat_train <- data.matrix(glycaemic_pat[,colnames(X_train),with=FALSE])
# pat_pool <- catboost.load_pool(data = pat_train)
# glycaemic_pat[,glucose_bst := (catboost.predict(bst, pat_pool)[,1])]
# glycaemic_pat[,glucose_bst_sd := (catboost.predict(bst, pat_pool)[,2])]

glycaemic_pat[,glucose_bst := (prediction_catboost(pat_train)[,1])]
glycaemic_pat[,glucose_bst_sd := (prediction_catboost(pat_train)[,2])]

glycaemic_pat[tstep > 0] %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,48)) +
  geom_point(aes(x=tstep,y=log(glucose_b_shift1))) +
  geom_line(aes(x = tstep,y=glucose_bst)) +
  geom_line(aes(x = tstep,y=glucose_bst+(glucose_bst_sd)*2)) +
  geom_line(aes(x = tstep,y=glucose_bst-(glucose_bst_sd)*2))



## catboost - quantile prediction model ----------------------------------------


# convert to catboost pool
train_pool <- catboost.load_pool(data = X_train, label = log(y_train))
validation_pool <- catboost.load_pool(data = X_validation, label = log(y_validation))
bst_m <- catboost.train(train_pool,  validation_pool,
                       params = list(loss_function = 'Quantile:alpha=0.5',
                                     iterations = 3000, metric_period=10,
                                     use_best_model=TRUE))
bst_l <- catboost.train(train_pool,  validation_pool,
                      params = list(loss_function = 'Quantile:alpha=0.025',
                                    iterations = 3000, metric_period=10,
                                    use_best_model=TRUE))
bst_u <- catboost.train(train_pool,  validation_pool,
                      params = list(loss_function = 'Quantile:alpha=0.975',
                                    iterations = 3000, metric_period=10,
                                    use_best_model=TRUE))

# save models
catboost.save_model(bst_m,"data/models/bst_m.cbm")
catboost.save_model(bst_l,"data/models/bst_l.cbm")
catboost.save_model(bst_u,"data/models/bst_u.cbm")

# prediction
predl <- catboost.predict(bst_l, validation_pool)
pred <- catboost.predict(bst_m, validation_pool)
predu <- catboost.predict(bst_u, validation_pool)
glycaemic_validation[,glucose_bst_l := exp(predl)]
glycaemic_validation[,glucose_bst := exp(pred)]
glycaemic_validation[,glucose_bst_u := exp(predu)]
glycaemic_validation$glucose_b_shift1 <- y_validation

# prediction
glycaemic_validation$glucose_b_shift1 <- y_validation
glycaemic_validation[,error1 := ((glucose_bst - glucose_b_shift1))]
glycaemic_validation[,error2 := ((glucose_bst - glucose_b_shift1)^2)]
glycaemic_validation[,deviation := abs((glucose_bst - glucose_b_shift1)/glucose_b_shift1)]

mad <- mean(glycaemic_validation$deviation[glycaemic_validation$tstep > 0])
rmse <- sqrt(mean(glycaemic_validation$error2[glycaemic_validation$tstep > 0]))
cat("MAD: ",mad,"\n")
cat("RMSE: ",rmse,"\n")

# evaluation
cat("coverage:")
sum((y_validation > exp(predl)) & (y_validation < exp(predu)))/length(y_validation)

glycaemic_pat <- glycaemic_full[icustay_id == sample(glycaemic_full$icustay_id,1)]
invisible(lapply(names(glycaemic_pat),function(.name) set(glycaemic_pat, which(is.nan(glycaemic_pat[[.name]])), j = .name,value =NA)))

glycaemic_pat <- fix_missing(glycaemic_pat,12)
glycaemic_pat[,.(icustay_id,tstep,xx_glucose_b_lag_0,xx_glucose_b_time_lag_0)]
pat_train <- data.matrix(glycaemic_pat[,colnames(X_train),with=FALSE])
pat_pool <- catboost.load_pool(data = pat_train)
# glycaemic_pat[,glucose_bst := (catboost.predict(bst, pat_pool)[,1])]
# glycaemic_pat[,glucose_bst_sd := (catboost.predict(bst, pat_pool)[,2])]

predl <- catboost.predict(bst_l, pat_pool)
pred <- catboost.predict(bst_m, pat_pool)
predu <- catboost.predict(bst_u, pat_pool)
glycaemic_pat[,glucose_bst_l := exp(predl)]
glycaemic_pat[,glucose_bst := exp(pred)]
glycaemic_pat[,glucose_bst_u := exp(predu)]

glycaemic_pat[tstep > 0] %>% 
  ggplot() + 
  scale_x_continuous(limits = c(0,48)) +
  geom_point(aes(x=tstep,y=glucose_b_shift1)) +
  geom_ribbon(aes(x=tstep,ymin=glucose_bst_l, ymax=glucose_bst_u),alpha=0.4,fill="lightblue") +
  geom_line(aes(x = tstep,y=glucose_bst),col="blue",size=1.0) +
  labs(x = "Time in ICU (hours)", y = "Blood glucose (mg/dL)") +
  theme_minimal() + 
  coord_cartesian(ylim=c(0,300))

## predict hypoglycaemia -------------------------------------------------------

y_train_b <- as.numeric(y_train < 70)
train_pool <- catboost.load_pool(data = X_train, label = y_train_b)
bst_cv_hypo <- catboost.cv(train_pool,
                        params = list(loss_function = 'Logloss',
                                      custom_loss = c('Accuracy','Recall','F1','AUC',
                                                      'BalancedAccuracy','BalancedErrorRate'),
                                      iterations = 100, metric_period=10,
                                      use_best_model=TRUE))
bst_cv_hypo