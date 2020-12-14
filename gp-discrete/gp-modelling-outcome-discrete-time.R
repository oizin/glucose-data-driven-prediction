############################################################################################
#
# Glycaemic control in the ICU
# CONTENTS: 
# A. Predict patient glucose level (mg/dL) with XGBoost
# B. Predict change in patient glucose level (mg/dL) with XGBoost
#
# NOTES:
# 
############################################################################################

rm(list = ls());gc()

SAVE = FALSE

library(data.table)
library(xgboost)
library(ggplot2)
library(magrittr)
library(ega)  # for clarke error grid
source("src/ml-pipe-data-processing.R")
source("src/ml-pipe-evaluation.R")


# import data -----------------------------------------------------------------
glycaemic_train <- readRDS(file = "data/processed/glycaemic_train_mvonly_dtime_1.rds")
LENGTH_INTERVAL <- glycaemic_train[start_tstep > 0,end_tstep - start_tstep][1]

dim(glycaemic_train)
names(glycaemic_train)

# columns present in the dataset to be removed in the training matrix
# rm_cols <- c("icustay_id","subject_id","hadm_id","start_tstep","end_tstep",
#              "icu_expire_flag",                                                     
#              "hospital_expire_flag",                                                
#              "expire_flag",                                                         
#              "ttd_days",                                                            
#              "los_dy",
#              "dbsource",
#              "glucose_measured",
#              "glucose_b_shift1","glucose_shift1",
#              "intime",                                                             
#              "outtime",                                                            
#              "admittime",                                                          
#              "dischtime",                                                          
#              "last_careunit",                                                      
#              "dbsource")                                                              

# new variable creation --------------------------------------------------------
## add lagged variables 
names(glycaemic_train)
glycaemic_train <- prepare_mimic_xgboost_outcome(glycaemic_train)
names(glycaemic_train)

# Delete and subset ------------------------------------------------------------

# subset to measured bedside glucose
glycaemic_train[glucose_b_shift1 > 1000,glucose_b_shift1 := NA] 
glycaemic_train[,glucose_measured := ifelse(is.na(glucose_b_shift1),0,1)]
glycaemic_train <- glycaemic_train[glucose_measured == 1]

# rename existing glucose variables
glycaemic_train[,xx_glucose_b_lag0 := xx_glucose_b]
glycaemic_train[,xx_glucose_l_lag0 := xx_glucose_l]
glycaemic_train$xx_glucose_l <- NULL
glycaemic_train$xx_glucose_b <- NULL

# SAVE -------------------------------------------------------------------------

if (SAVE) {
  fname <- paste0("data/processed/glycaemic_train_mvonly_dtime_",
                  gsub(pattern = "[\\.]",replacement = "_",x = LENGTH_INTERVAL),
                  "_xgboost_input.csv")
  fwrite(glycaemic_train, file = fname)
}

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

if (SAVE) {
  fname <- paste0("data/processed/glycaemic_train_t_mvonly_dtime_",
                  gsub(pattern = "[\\.]",replacement = "_",x = LENGTH_INTERVAL),
                  "_xgboost_input.csv")
  fwrite(glycaemic_train, file = fname)
  fname <- paste0("data/processed/glycaemic_train_v_mvonly_dtime_",
                  gsub(pattern = "[\\.]",replacement = "_",x = LENGTH_INTERVAL),
                  "_xgboost_input.csv")
  fwrite(glycaemic_validation, file = fname)
}

## XG Boost - outcome prediction model ----------------------------------------

# convert to matrix
X_train <- data.matrix(glycaemic_train[,grepl(pattern = "xx_",names(glycaemic_train)), with=FALSE])
X_validation <- data.matrix(glycaemic_validation[,grepl(pattern = "xx_",names(glycaemic_train)), with=FALSE])

# fit model
dtrain <- xgb.DMatrix(X_train,label=y_train)
dvalidation <- xgb.DMatrix(X_validation,
                           label=y_validation)
watchlist <- list(train = dtrain, eval = dvalidation)

search_space <- expand.grid(eta = c(0.1),
                            max_depth=c(3,5,10,15),
                            colsample_bytree=c(0.5,0.7),
                            subsample = c(0.5,0.8))[1,]

# hyperparameter training
# here start some loop
for (s in 1:nrow(search_space)) {
  cat("model ",s," of ",nrow(search_space),"\n")
  model_name <- stringi::stri_rand_strings(1, 10)
  cat(model_name,"\n")
  
  param <- list(max_depth = search_space$max_depth[s], 
                eta = search_space$eta[s], 
                nthread = 4,
                objective = "reg:squarederror", eval_metric = "rmse",
                colsample_bytree = search_space$colsample_bytree[s],
                subsample = search_space$subsample[s])
  print(param)
  bst <- xgb.train(param, dtrain, nrounds = 200, watchlist,
                   early_stopping_rounds = 10)
  xgboost::xgb.save(bst,paste0("data/models/",model_name,".model"))
  #plot(bst$evaluation_log$iter,bst$evaluation_log$eval_rmse,type="l")

  ## evaluate
  # prepare data
  glycaemic_validation$glucose_bst <- predict(bst,X_validation)
  glycaemic_validation$glucose_b_shift1 <- y_validation
  glycaemic_validation[,error1 := ((glucose_bst - glucose_b_shift1))]
  glycaemic_validation[,error2 := ((glucose_bst - glucose_b_shift1)^2)]
  glycaemic_validation[,deviation := abs((glucose_bst - glucose_b_shift1)/glucose_b_shift1)]
  
  # default error
  glycaemic_validation[,glucose_naive := nafill(xx_glucose_b_lag0,type = "locf"),by="icustay_id"]
  glycaemic_validation[,error2_naive := ((glucose_naive - glucose_b_shift1)^2)]
  glycaemic_validation[,deviation_naive := abs((glucose_naive - glucose_b_shift1)/glucose_b_shift1)]
  
  # summary measures
  if(!file.exists("data/logs/gp-discrete-xg-summary.csv")) file.create("data/logs/gp-discrete-xg-summary.csv")
  mad <- mean(glycaemic_validation$deviation[glycaemic_validation$tstep > 0])
  rmse <- sqrt(mean(glycaemic_validation$error2[glycaemic_validation$tstep > 0]))
  cat("MAD: ",mad,"\n")
  cat("RMSE: ",rmse,"\n")
  mad_naive <- mean(glycaemic_validation$deviation_naive[glycaemic_validation$tstep > 0],na.rm=TRUE)
  rmse_naive <- sqrt(mean(glycaemic_validation$error2_naive[glycaemic_validation$tstep > 0],na.rm=TRUE))
  cat("MAD (naive): ",mad_naive,"\n")
  cat("RMSE (naive): ",rmse_naive,"\n")
  
  write.table(x = data.frame(model_name=model_name,mad=mad,rmse=rmse,
                             mad_naive=mad_naive,rmse_naive=rmse_naive),
              file="data/logs/gp-discrete-xg-summary.csv",
            append=TRUE,row.names = FALSE,sep = ",",col.names = FALSE)

  # plots
  model_plots <- paste0("data/logs/",model_name)
  if (!dir.exists(model_plots)) dir.create(model_plots)
    
  evaluation_plots_outcome(model_plots,
                           dt=glycaemic_validation,
                          model_name = model_name)
}


# test on a specific patient
# 200131
glycaemic_full <- readRDS(file = "data/processed/glycaemic_train_mvonly_dtime_1.rds")
glycaemic_pat <- glycaemic_full[icustay_id == 200131]
glycaemic_pat <- prepare_mimic_xgboost_outcome(glycaemic_pat)
glycaemic_pat[,xx_glucose_b_lag0 := xx_glucose_b]
glycaemic_pat[,xx_glucose_l_lag0 := xx_glucose_l]
X_pat <- data.matrix(glycaemic_pat[,colnames(X_train),with=FALSE])
colnames(X_pat) == colnames(X_train)
glycaemic_pat$glucose_bst <- predict(bst,X_pat)
ggplot(data=glycaemic_pat) +
  geom_line(aes(x=tstep,y=glucose_bst)) +
  geom_point(aes(x=tstep,y=glucose_b_shift1))

## CATboost - outcome prediction model ----------------------------------------------
#devtools::install_url('https://github.com/catboost/catboost/releases/download/v0.24.1/catboost-R-Windows-0.24.1.tgz', INSTALL_opts = c("--no-multiarch"))

library(catboost)

train_pool <- catboost.load_pool(data = X_train, label = y_train)
validation_pool <- catboost.load_pool(data = X_validation, label = y_validation)

# model
model_name <- stringi::stri_rand_strings(1, 10)
cat("model ",model_name,"\n")
bst <- catboost.train(train_pool,  validation_pool,
                        params = list(loss_function = 'RMSE',
                                      iterations = 200, metric_period=10))
# 
# evaluation
prediction <- catboost.predict(bst, validation_pool)
glycaemic_validation$glucose_bst <- prediction
glycaemic_validation[,error1 := ((glucose_bst - glucose_b_shift1))]
glycaemic_validation[,error2 := ((glucose_bst - glucose_b_shift1)^2)]
glycaemic_validation[,deviation := abs((glucose_bst - glucose_b_shift1)/glucose_b_shift1)]
mad <- mean(glycaemic_validation$deviation[glycaemic_validation$tstep > 0])
rmse <- sqrt(mean(glycaemic_validation$error2[glycaemic_validation$tstep > 0]))
cat("MAD: ",mad,"\n")
cat("RMSE: ",rmse,"\n")

# plots
model_plots <- paste0("data/logs/",model_name)
if (!dir.exists(model_plots)) dir.create(model_plots)

evaluation_plots_outcome(model_plots,
                         dt=glycaemic_validation,
                         model_name = model_name)

# test on a specific patient
# 200131
glycaemic_full <- readRDS(file = "data/processed/glycaemic_train_mvonly_dtime_1.rds")
glycaemic_pat <- glycaemic_full[icustay_id == 200131]
glycaemic_pat <- prepare_mimic_xgboost_outcome(glycaemic_pat)
glycaemic_pat[,xx_glucose_b_lag0 := xx_glucose_b]
glycaemic_pat[,xx_glucose_l_lag0 := xx_glucose_l]
X_pat <- data.matrix(glycaemic_pat[,colnames(X_train),with=FALSE])
all(colnames(X_pat) == colnames(X_train))
pat_pool <- catboost.load_pool(data = X_pat)
glycaemic_pat$glucose_bst <- catboost.predict(bst,pat_pool)
ggplot(data=glycaemic_pat) +
  geom_line(aes(x=tstep,y=glucose_bst)) +
  geom_point(aes(x=tstep,y=glucose_b_shift1))

# ## CATboost - quantile prediction model ----------------------------------------------
# 
# #devtools::install_url('https://github.com/catboost/catboost/releases/download/v0.24.1/catboost-R-Windows-0.24.1.tgz', INSTALL_opts = c("--no-multiarch"))
# 
# rm(list = ls())
# 
# library(catboost)
# library(imputeTS)
# library(data.table)
# library(ggplot2)
# library(magrittr)
# library(ega)  # for clarke error grid
# source("src/ml-pipe-data-processing.R")
# source("src/ml-pipe-evaluation.R")
# 
# glycaemic_train <- readRDS(file = "data/processed/glycaemic_train_mvonly_dtime_1.rds")
# length(unique(glycaemic_train$icustay_id))
# 
# ## data setup
# # keep if 2+ blood glucose
# tmp <- glycaemic_train[,.(N=sum(!is.na(xx_glucose_b))),by=icustay_id][order(N)]
# glycaemic_train <- glycaemic_train[!icustay_id %in% tmp[N < 2,icustay_id]]
# length(unique(glycaemic_train$icustay_id))
# # keep if 2+ lab glucose
# tmp <- glycaemic_train[,.(N=sum(!is.na(xx_glucose_l))),by=icustay_id][order(N)]
# glycaemic_train <- glycaemic_train[!icustay_id %in% tmp[N < 2,icustay_id]]
# length(unique(glycaemic_train$icustay_id))
# # keep if 2+ outcome glucose
# tmp <-glycaemic_train[,.(N=sum(!is.na(glucose_b_shift1))),by=icustay_id][order(N)]
# glycaemic_train <- glycaemic_train[!icustay_id %in% tmp[N < 2,icustay_id]]
# length(unique(glycaemic_train$icustay_id))
# 
# ## imputations
# glycaemic_train[,imputed_outcome := ifelse(is.na(glucose_b_shift1),1,0)]
# glycaemic_train[,imputed_bbg := ifelse(is.na(xx_glucose_b),1,0)]
# glycaemic_train[,imputed_lbg := ifelse(is.na(xx_glucose_l),1,0)]
# glycaemic_train[,xx_glucose_b := na_interpolation(xx_glucose_b),by=icustay_id]
# glycaemic_train[,xx_glucose_l := na_interpolation(xx_glucose_l),by=icustay_id]
# glycaemic_train[,glucose_b_shift1 := na_interpolation(glucose_b_shift1),by=icustay_id]
# 
# ## keep until last outcome
# tmp <- glycaemic_train[!is.na(glucose_b_shift1),.(limit=max(tstep)),by=icustay_id]
# glycaemic_train <- merge(glycaemic_train,tmp,by="icustay_id",all.x=TRUE)
# glycaemic_train <- glycaemic_train[tstep < limit + 1]
# 
# ## add lagged 
# glycaemic_train <- prepare_mimic_xgboost_outcome_min(glycaemic_train)
# 
# ## rename existing glucose variables
# glycaemic_train[,xx_glucose_b_lag0 := xx_glucose_b]
# glycaemic_train[,xx_glucose_l_lag0 := xx_glucose_l]
# glycaemic_train$xx_glucose_l <- NULL
# glycaemic_train$xx_glucose_b <- NULL
# 
# ## training/validation set split
# IDs <- unique(glycaemic_train$icustay_id)
# J <- length(unique(glycaemic_train$icustay_id))
# set.seed(1234)
# validation_IDs <- sample(IDs,floor(J*0.1))
# training_IDs <- IDs[!IDs %in% validation_IDs]
# glycaemic_validation <- glycaemic_train[icustay_id %in% validation_IDs]
# glycaemic_train <- glycaemic_train[icustay_id %in% training_IDs]
# y_train <- as.numeric(glycaemic_train[["glucose_b_shift1"]])
# y_validation <- as.numeric(glycaemic_validation[["glucose_b_shift1"]])
# set.seed(NULL)
# 
# # convert to matrix
# X_train <- data.matrix(glycaemic_train[,grepl(pattern = "xx_",names(glycaemic_train)), with=FALSE])
# X_validation <- data.matrix(glycaemic_validation[,grepl(pattern = "xx_",names(glycaemic_train)), with=FALSE])
# 
# # convert to catboost pool
# train_pool <- catboost.load_pool(data = X_train, label = log(y_train),
#                                  weight = 1-glycaemic_train$imputed_outcome)
# validation_pool <- catboost.load_pool(data = X_validation, label = log(y_validation),
#                                       weight = 1-glycaemic_validation$imputed_outcome)
# 
# # model(s)
# model_name <- stringi::stri_rand_strings(1, 10)
# cat("model ",model_name,"\n")
# bstl <- catboost.train(train_pool,  validation_pool,
#                       params = list(loss_function = 'Quantile:alpha=0.025',
#                                     iterations = 100, metric_period=10))
# bst <- catboost.train(train_pool,  validation_pool,
#                        params = list(loss_function = 'Quantile:alpha=0.5',
#                                      iterations = 100, metric_period=10))
# bstu <- catboost.train(train_pool,  validation_pool,
#                       params = list(loss_function = 'Quantile:alpha=0.975',
#                                     iterations = 100, metric_period=10))
# 
# # prediction
# predl <- catboost.predict(bstl, validation_pool)
# pred <- catboost.predict(bst, validation_pool)
# predu <- catboost.predict(bstu, validation_pool)
# glycaemic_validation[,glucose_bst_l := exp(predl)]
# glycaemic_validation[,glucose_bst := exp(pred)]
# glycaemic_validation[,glucose_bst_u := exp(predu)]
# glycaemic_validation$glucose_b_shift1 <- y_validation
# 
# # evaluation
# cat("coverage:")
# sum((y_validation > exp(predl)) & (y_validation < exp(predu)))/length(y_validation)
# 
# glycaemic_pat <- glycaemic_validation[icustay_id == 200131 & tstep > 0]
# pat_train <- data.matrix(glycaemic_pat[,grepl(pattern = "xx_",names(glycaemic_pat)), with=FALSE])
# pat_pool <- catboost.load_pool(data = pat_train)
# glycaemic_pat[,glucose_bst_l := exp(catboost.predict(bstl, pat_pool))]
# glycaemic_pat[,glucose_bst := exp(catboost.predict(bst, pat_pool))]
# glycaemic_pat[,glucose_bst_u := exp(catboost.predict(bstu, pat_pool))]
# glycaemic_validation[icustay_id == 200131 & tstep > 0] %>% 
#   ggplot() + 
#   geom_point(aes(x=tstep,y=glucose_b_shift1,col=factor(imputed_outcome))) +
#   geom_line(aes(x = tstep+1,y=glucose_bst)) +
#   geom_line(aes(x = tstep+1,y=glucose_bst_l)) +
#   geom_line(aes(x = tstep+1,y=glucose_bst_u)) +
#   scale_x_continuous(limits = c(0,48)) +
#   coord_cartesian(ylim = c(100,300))
