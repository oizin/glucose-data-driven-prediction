################################################################################
# Hypoglycaemic prediction for Data driven glucose prediction
#
#
###############################################################################

## libraries
library(data.table)
library(catboost)

## import data
glycaemic_train <- readRDS("scripts/gp-discrete/glycaemic_train_2_catboost.rds")
glycaemic_test <- readRDS( "scripts/gp-discrete/glycaemic_test_2_catboost.rds")
glycaemic_train <- rbind(glycaemic_train,glycaemic_test[,names(glycaemic_train),with=FALSE])

## define target
glycaemic_train[,target := glucose_b_shift1 < 70]

## train and evaluate
X_train <- data.matrix(glycaemic_train[,grepl(pattern = "xx_",names(glycaemic_train)), with=FALSE])
y_train <- as.numeric(glycaemic_train[["target"]])
train_pool <- catboost.load_pool(data = X_train, label = y_train)
bst_cv <- catboost.cv(train_pool,
                      params = list(loss_function = 'Logloss',
                                    custom_loss=c('Precision','Recall','F1','AUC','BrierScore'),
                                    iterations = 1000, metric_period=10,early_stopping_rounds = 20))

plot(bst_cv$test.Precision.mean)
plot(bst_cv$test.Recall.mean)
plot(bst_cv$test.F1.mean)
plot(bst_cv$test.AUC.mean)
plot(bst_cv$test.BrierScore.mean)


bst_cv[100,]
