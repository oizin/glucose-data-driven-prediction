import catboost

from catboost import CatBoostRegressor, Pool

def prediction_catboost(X):
  model = CatBoostRegressor()
  model.load_model("data/models/catboost_with_uncertainty.cbm")
  X_pool = Pool(X)
  preds = model.predict(X_pool)
  return(preds)
