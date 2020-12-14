# final models
rm(list = ls())
library(catboost)
library(data.table)

glycaemic_train <- readRDS("scripts/gp-discrete/glycaemic_train_2_catboost.rds")
setDT(glycaemic_train)

# remove lab glucose
glycaemic_train <- glycaemic_train[,!grepl("xx_glucose_l",names(glycaemic_train)),with=FALSE]

X_train <- data.matrix(glycaemic_train[,grepl(pattern = "xx_",names(glycaemic_train)), with=FALSE])
y_train <- as.numeric(glycaemic_train[["glucose_b_shift1"]])

## catboost - outcome  prediction model ----------------------------------------

train_pool <- catboost.load_pool(data = X_train, label = log(y_train))
bst <- catboost.train(train_pool,
                      params = list(loss_function = 'RMSE',custom_loss='MAPE',
                                    iterations = 3000, metric_period=10))
catboost.save_model(bst,"data/models/cb.cbm")

shap <- catboost.get_feature_importance(bst,type = "ShapValues",pool=train_pool)
tmp1 <- data.table(vars=colnames(X_train),shap=round(colSums(shap)[1:593],3))
tmp1[order(-shap)][1:30]


tmp <- data.table(vars=colnames(X_train),gain=catboost.get_feature_importance(bst)[,1])
tmp[order(-gain)][1:30]

## catboost - outcome + uncertainty prediction model ----------------------------

library(reticulate)
source_python('scripts/gp-discrete/catboost_fitting.py')
fit_catboost_final(X_train,log(y_train),3000)

## catboost - quantile prediction model ----------------------------------------

# convert to catboost pool
train_pool <- catboost.load_pool(data = X_train, label = log(y_train))

bst_m <- catboost.train(train_pool,
                        params = list(loss_function = 'Quantile:alpha=0.5',
                                      iterations = 3000, metric_period=10,
                                      use_best_model=TRUE))
bst_l <- catboost.train(train_pool,
                        params = list(loss_function = 'Quantile:alpha=0.025',
                                      iterations = 1000, metric_period=10,
                                      use_best_model=TRUE))
bst_u <- catboost.train(train_pool,
                        params = list(loss_function = 'Quantile:alpha=0.975',
                                      iterations = 1000, metric_period=10,
                                      use_best_model=TRUE))

# save models
catboost.save_model(bst_m,"data/models/bst_m.cbm")
catboost.save_model(bst_l,"data/models/bst_l.cbm")
catboost.save_model(bst_u,"data/models/bst_u.cbm")
