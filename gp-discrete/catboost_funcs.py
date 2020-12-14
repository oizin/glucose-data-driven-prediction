import catboost

from catboost import CatBoostRegressor, Pool

def fit_catboost(X_train,y_train,X_validation,y_validation):
  # initialize data
  train_pool = Pool(X_train,y_train)
  validation_pool = Pool(X_validation)

  model = CatBoostRegressor(iterations=1000,
                             loss_function='RMSEWithUncertainty',
                             verbose=True)
  # train the model
  model.fit(train_pool)
  model.save_model("data/models/catboost_with_uncertainty.cbm",
           format="cbm",
           export_parameters=None,
           pool=None)
  # make the prediction using the resulting model
  preds = model.predict(validation_pool)
  return(preds)


def prediction_catboost(X):
  model = CatBoostRegressor()
  model.load_model("data/models/catboost_with_uncertainty.cbm")
  X_pool = Pool(X)
  preds = model.predict(X_pool)
  return(preds)

