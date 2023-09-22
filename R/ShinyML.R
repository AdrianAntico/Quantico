# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Evaluation Metrics, Loss Functions, Families, Links                                        ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Shiny.ML.CatBoost.GridEvalMetricsOptions
#'
#' @description Grid Metrics for Tuning
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @export
Shiny.ML.CatBoost.GridEvalMetricsOptions <- function(TT) {
  if(TT == 'Regression') {
    choices <- c('mae','mape','rmse','r2')
    default <- 'rmse'
  } else if(TT == 'Binary Classification') {
    choices <- c('Utility','MCC','Acc','F1_Score','F2_Score','F0.5_Score','TPR','TNR','FNR','FPR','FDR','FOR','NPV','PPV','ThreatScore')
    default <- 'Utility'
  } else if(TT == 'MultiClass') {
    choices <- c('accuracy','microauc','logloss')
    default <- 'microauc'
  }
  return(list(Choices = choices, Default = default))
}

#' @title Shiny.ML.CatBoost.EvalMetricOptions
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @export
Shiny.ML.CatBoost.EvalMetricOptions <- function(TT) {
  print(TT)
  if(TT == 'Regression') {
    choices <- c('RMSE','MAE','MAPE','R2','Poisson','MedianAbsoluteError','SMAPE','MSLE','NumErrors','FairLoss','Tweedie','Huber','LogLinQuantile','Quantile','Lq','Expectile','MultiRMSE')
    default <- 'RMSE'
  } else if(TT == 'Binary Classification') {
    choices <- c('Logloss','CrossEntropy','Precision','Recall','F1','BalancedAccuracy','BalancedErrorRate','MCC','Accuracy','CtrFactor','AUC','BrierScore','HingeLoss','HammingLoss','ZeroOneLoss','Kappa','WKappa','LogLikelihoodOfPrediction','TotalF1','PairLogit','PairLogitPairwise','PairAccuracy','QueryCrossEntropy','QuerySoftMax','PFound','NDCG','AverageGain','PrecisionAt','RecallAt','MAP')
    default <- 'MCC'
  } else if(TT == 'MultiClass') {
    choices <- c('MultiClass','MultiClassOneVsAll','AUC','TotalF1','MCC','Accuracy','HingeLoss','HammingLoss','ZeroOneLoss','Kappa','WKappa')
    default <- 'MultiClassOneVsAll'
  }
  return(list(Choices = choices, Default = default))
}

#' @title Shiny.ML.XGBoost.EvalMetricOptions
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @export
Shiny.ML.XGBoost.EvalMetricOptions <- function(TT) {
  print(TT)
  if(TT == 'Regression') {
    choices <- c('rmse','mae','mape')
    default <- 'rmse'
  } else if(TT == 'Binary Classification') {
    choices <- c('logloss','error','aucpr','auc')
    default <- 'logloss'
  } else if(TT == 'MultiClass') {
    choices <- c('merror','mlogloss')
    default <- 'mlogloss'
  }
  return(list(Choices = choices, Default = default))
}

#' @title Shiny.ML.H2O_DRF.EvalMetricOptions
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @export
Shiny.ML.H2O_DRF.EvalMetricOptions <- function(TT) {
  print(TT)
  if(TT == 'Regression') {
    choices <- c("MSE","RMSE","MAE","RMSLE")
    default <- 'RMSE'
  } else if(TT == 'Binary Classification') {
    choices <- c("AUC","logloss")
    default <- 'logloss'
  } else if(TT == 'MultiClass') {
    choices <- c('logloss')
    default <- 'logloss'
  }
  return(list(Choices = choices, Default = default))
}

#' @title Shiny.ML.H2O_GBM.EvalMetricOptions
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @export
Shiny.ML.H2O_GBM.EvalMetricOptions <- function(TT) {
  print(TT)
  if(TT == 'Regression') {
    choices <- c("MSE","RMSE","MAE","RMSLE")
    default <- 'RMSE'
  } else if(TT == 'Binary Classification') {
    choices <- c("AUC","logloss")
    default <- 'logloss'
  } else if(TT == 'MultiClass') {
    choices <- c('logloss')
    default <- 'logloss'
  }
  return(list(Choices = choices, Default = default))
}

#' @title Shiny.ML.H2O_GLM.EvalMetricOptions
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @export
Shiny.ML.H2O_GLM.EvalMetricOptions <- function(TT) {
  print(TT)
  if(TT == 'Regression') {
    choices <- c("MSE","RMSE","MAE","RMSLE")
    default <- 'RMSE'
  } else if(TT == 'Binary Classification') {
    choices <- c("AUC","logloss")
    default <- 'logloss'
  } else if(TT == 'MultiClass') {
    choices <- c('logloss')
    default <- 'logloss'
  }
  return(list(Choices = choices, Default = default))
}

#' @title Shiny.ML.CatBoost.LossFunctionOptions
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @export
Shiny.ML.CatBoost.LossFunctionOptions <- function(TT) {
  print(TT)
  if(TT == 'Regression') {
    choices <- c('MAPE','MAE','RMSE','Poisson','Tweedie','Huber','LogLinQuantile','Quantile','Lq','Expectile','MultiRMSE')
    default <- 'RMSE'
  } else if(TT == 'Binary Classification') {
    choices <- c('Logloss','CrossEntropy','Lq','PairLogit','PairLogitPairwise','YetiRank','YetiRankPairwise','QueryCrossEntropy','QuerySoftMax')
    default <- 'Logloss'
  } else if(TT == 'MultiClass') {
    choices <- c('MultiClass','MultiClassOneVsAll')
    default <- 'MultiClassOneVsAll'
  }
  return(list(Choices = choices, Default = default))
}

#' @title Shiny.ML.XGBoost.LossFunctionOptions
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @export
Shiny.ML.XGBoost.LossFunctionOptions <- function(TT) {
  print(TT)
  if(TT == 'Regression') {
    choices <- c('reg:squarederror', 'reg:squaredlogerror', 'reg:pseudohubererror', 'count:poisson', 'survival:cox', 'survival:aft', 'aft_loss_distribution', 'reg:gamma', 'reg:tweedie')
    default <- 'reg:squarederror'
  } else if(TT == 'Binary Classification') {
    choices <- c('reg:logistic', 'binary:logistic')
    default <- 'reg:logistic'
  } else if(TT == 'MultiClass') {
    choices <- c('multi:softprob')
    default <- 'multi:softprob'
  }
  return(list(Choices = choices, Default = default))
}

#' @title LightGBMLossFunctionOptions
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @export
Shiny.ML.LightGBM.LossFunctionOptions <- function(TT) {
  print(TT)
  if(TT == 'Regression') {
    choices <- c('rmse', 'l1', 'l2', 'quantile', 'mape', 'huber', 'fair', 'poisson', 'gamma', 'gamma_deviance', 'tweedie', 'ndcg')
    default <- 'rmse'
  } else if(TT == 'Binary Classification') {
    choices <- c('binary_logloss', 'average_precision', 'auc', 'map', 'binary_error', 'auc_mu')
    default <- 'binary_logloss'
  } else if(TT == 'MultiClass') {
    choices <- c('multiclass', 'multiclassova')
    default <- 'multiclass'
  }
  return(list(Choices = choices, Default = default))
}

#' @title Shiny.ML.H2O.DistributionOptions
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @export
Shiny.ML.H2O.DistributionOptions <- function(TT) {
  print(TT)
  if(TT == 'Regression') {
    choices <- c("AUTO", "gaussian", "poisson", "gamma", "tweedie", "huber", "laplace")
    default <- 'AUTO'
  } else if(TT == 'Binary Classification') {
    choices <- c("AUTO", "bernoulli", "quasibinomial")
    default <- c('AUTO')
  } else if(TT == 'MultiClass') {
    choices <- c('AUTO',"multinomial")
    default <- 'multinomial'
  }
  return(list(Choices = choices, Default = default))
}

#' @title Shiny.ML.H2O.DistributionOptions
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @export
Shiny.ML.H2O.GLM.DistributionOptions <- function(TT) {
  print(TT)
  if(TT == 'Regression') {
    choices <- c("AUTO", "gaussian", "poisson", "gamma", "tweedie", "negativebinomial", "fractionalbinomial")
    default <- 'AUTO'
  } else if(TT == 'Binary Classification') {
    choices <- c("AUTO", "binomial", "quasibinomial")
    default <- c('AUTO','binomial')
  } else if(TT == 'MultiClass') {
    choices <- c('AUTO',"multinomial","ordinal")
    default <- 'multinomial'
  }
  return(list(Choices = choices, Default = default))
}

#' @title Shiny.ML.H2O.LinkOptions
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param TT Target Type: 'regression', 'classification', 'multiclass', all lower case
#'
#' @export
Shiny.ML.H2O.LinkOptions <- function(TT) {
  if(TT == 'Regression') {
    choices <- c("family_default", "identity", "log", "inverse", "tweedie", "logit")
    default <- 'family_default'
  } else if(TT == 'Binary Classification') {
    choices <- c("family_default", 'identity', 'logit', 'log', 'inverse', 'tweedie')
    default <- "family_default"
  } else if(TT == 'MultiClass') {
    choices <- c("family_default", "ologit")
    default <- "family_default"
  }
  return(list(Choices = choices, Default = default))
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# ML Parameters                                                                              ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Shiny.ML.CatBoost_Params
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param l ArgsList created inside server.R
#' @param input input from shiny app
#' @param Debug Debug. TRUE or FALSE
#' @param TT E.g. CatBoost_TargetType. Can be 'Regression', 'Binary Classification', or 'MultiClass'. Case sensitive
#' @param CodeList App code list
#'
#' @export
Shiny.ML.CatBoost_Params <- function(l,input,Debug,TT,CodeList) {

  # CatBoost ML Algo Specific
  l[['task_type']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_task_type']]}, error=function(x) NULL), Type='character', Default='CPU')
  l[['NumGPUs']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_NumGPUs']]}, error=function(x) NULL), Type='numeric', Default=1)

  # CatBoost ML Parameters
  l[['Trees']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_Trees']]}, error=function(x) NULL), Type='numeric', Default=1000)
  l[['Depth']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_Depth']]}, error=function(x) NULL), Type='numeric', Default=8)
  l[['LearningRate']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_LearningRate']]}, error=function(x) NULL), Type='numeric', Default=NULL)
  l[['L2_Leaf_Reg']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_L2_Leaf_Reg']]}, error=function(x) NULL), Type='numeric', Default=NULL)
  l[['model_size_reg']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_model_size_reg']]}, error=function(x) NULL), Type='numeric', Default=0.50)
  l[['langevin']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_langevin']]}, error=function(x) NULL), Type='logical', Default=FALSE)
  l[['diffusion_temperature']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_diffusion_temperature']]}, error=function(x) NULL), Type='numeric', Default=10000)
  l[['RandomStrength']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_RandomStrength']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['BorderCount']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_BorderCount']]}, error=function(x) NULL), Type='numeric', Default=256)
  l[['RSM']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_RSM']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['BootStrapType']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_BootStrapType']]}, error=function(x) NULL), Type='character', Default='Bayesian')
  l[['GrowPolicy']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_GrowPolicy']]}, error=function(x) NULL), Type='character', Default='SymmetricTree')
  l[['feature_border_type']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_feature_border_type']]}, error=function(x) NULL), Type='character', Default='GreedyLogSum')
  l[['subsample']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_subsample']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['score_function']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_score_function']]}, error=function(x) NULL), Type='character', Default='Cosine')
  l[['min_data_in_leaf']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_min_data_in_leaf']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['sampling_unit']] <- 'Object'

  # Code Collection
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# CatBoost ML Parameters\n",
    "ArgsList[['Trees']] <- ", Quantico:::CEPP(l[['Trees']]), "\n",
    "ArgsList[['Depth']] <- ", Quantico:::CEPP(l[['Depth']]), "\n",
    "ArgsList[['LearningRate']] <- ", if(length(Quantico:::CEPP(l[['LearningRate']])) > 0L) Quantico:::CEPP(l[['LearningRate']]) else "NULL", "\n",
    "ArgsList[['L2_Leaf_Reg']] <- ", if(length(Quantico:::CEPP(l[['L2_Leaf_Reg']])) > 0L) Quantico:::CEPP(l[['L2_Leaf_Reg']]) else "NULL", "\n",
    "ArgsList[['model_size_reg']] <- ", Quantico:::CEPP(l[['model_size_reg']]), "\n",
    "ArgsList[['langevin']] <- ", Quantico:::CEPP(l[['langevin']]), "\n",
    "ArgsList[['diffusion_temperature']] <- ", Quantico:::CEPP(l[['diffusion_temperature']]), "\n",
    "ArgsList[['RandomStrength']] <- ", Quantico:::CEPP(l[['RandomStrength']]), "\n",
    "ArgsList[['BorderCount']] <- ", Quantico:::CEPP(l[['BorderCount']]), "\n",
    "ArgsList[['RSM']] <- ", Quantico:::CEPP(l[['RSM']]), "\n",
    "ArgsList[['BootStrapType']] <- ", Quantico:::CEP(l[['BootStrapType']]), "\n",
    "ArgsList[['GrowPolicy']] <- ", Quantico:::CEP(l[['GrowPolicy']]), "\n",
    "ArgsList[['feature_border_type']] <- ", Quantico:::CEP(l[['feature_border_type']]), "\n",
    "ArgsList[['subsample']] <- ", Quantico:::CEPP(l[['subsample']]), "\n",
    "ArgsList[['score_function']] <- ", Quantico:::CEP(l[['score_function']]), "\n",
    "ArgsList[['min_data_in_leaf']] <- ", Quantico:::CEPP(l[['min_data_in_leaf']]), "\n",
    "ArgsList[['sampling_unit']] <- ", Quantico:::CEP(l[['sampling_unit']]), "\n"))

  print(' ::  BuildModels 7  :: ')

  print(TT)
  if(TT == 'Regression') {
    if(Debug) print(' ::  BuildModels 8.1 :: ')

    # Store args
    l[['loss_function']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='RMSE')
    l[['loss_function_value']] <- 1.5
    l[['eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='RMSE')
    l[['eval_metric_value']] <- 1.5
    l[['grid_eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='mse')
    l[['MetricPeriods']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_MetricPeriods']]}, error=function(x) NULL), Type='numeric', Default=10)

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# CatBoost Evaluation Parameters\n",
      "ArgsList[['loss_function']] <- ", Quantico:::CEP(l[['loss_function']]), "\n",
      "ArgsList[['loss_function_value']] <- ", Quantico:::CEPP(l[['loss_function_value']]), "\n",
      "ArgsList[['eval_metric']] <- ", Quantico:::CEP(l[['eval_metric']]), "\n",
      "ArgsList[['eval_metric_value']] <- ", Quantico:::CEPP(l[['eval_metric_value']]), "\n",
      "ArgsList[['grid_eval_metric']] <- ", Quantico:::CEP(l[['grid_eval_metric']]), "\n",
      "ArgsList[['MetricPeriods']] <- ", Quantico:::CEPP(l[['MetricPeriods']]), "\n"))

  } else if(TT == 'Binary Classification') {
    if(Debug) print(' ::  BuildModels 8.2 :: ')

    # Store Args
    cw0 <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_ClassWeights0']]}, error=function(x) NULL), Type='numeric', Default=1)
    cw1 <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_ClassWeights1']]}, error=function(x) NULL), Type='numeric', Default=1)
    l[['LossFunction']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='Logloss')
    l[['EvalMetric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='AUC')
    l[['grid_eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='MCC')
    l[['ClassWeights']] <- c(cw0, cw1)
    l[['MetricPeriods']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_MetricPeriods']]}, error=function(x) NULL), Type='numeric', Default=10)

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# CatBoost Evaluation Parameters\n",
      "ArgsList[['LossFunction']] <- ", Quantico:::CEP(l[['LossFunction']]), "\n",
      "ArgsList[['EvalMetric']] <- ", Quantico:::CEP(l[['EvalMetric']]), "\n",
      "ArgsList[['grid_eval_metric']] <- ", Quantico:::CEP(l[['grid_eval_metric']]), "\n",
      "ArgsList[['ClassWeights']] <- ", Quantico:::ExpandText(l[['ClassWeights']]), "\n",
      "ArgsList[['MetricPeriods']] <- ", Quantico:::CEPP(l[['MetricPeriods']]), "\n"))

  } else if(TT == 'MultiClass') {
    if(Debug) print(' ::  BuildModels 8.3 :: ')

    # Store Args
    l[['loss_function']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='MultiClassOneVsAll')
    l[['eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='MultiClassOneVsAll')
    l[['grid_eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='microauc')
    l[['MetricPeriods']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_MetricPeriods']]}, error=function(x) NULL), Type='numeric', Default=10)

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# CatBoost Evaluation Parameters\n",
      "ArgsList[['loss_function']] <- ", Quantico:::CEP(l[['loss_function']]), "\n",
      "ArgsList[['eval_metric']] <- ", Quantico:::CEP(l[['eval_metric']]), "\n",
      "ArgsList[['grid_eval_metric']] <- ", Quantico:::CEP(l[['grid_eval_metric']]), "\n",
      "ArgsList[['MetricPeriods']] <- ", Quantico:::CEPP(l[['MetricPeriods']]), "\n"))
  }
  return(list(ArgsList = l,CodeList = CodeList))
}

#' @title Shiny.ML.XGBoost_Params
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param l ArgsList created inside server.R
#' @param input input from shiny app
#' @param Debug Debug. TRUE or FALSE
#' @param TT E.g. XGBoost_TargetType. Can be 'Regression', 'Binary Classification', or 'MultiClass'. Case sensitive
#' @param CodeList From App
#'
#' @export
Shiny.ML.XGBoost_Params <- function(l,input,Debug,TT,CodeList) {

  # XGBoost ML Parameters
  l[['Trees']]            <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_Trees']]}, error=function(x) NULL), Type='numeric', Default=1000)
  l[['max_depth']]        <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_max_depth']]}, error=function(x) NULL), Type='numeric', Default=8)
  l[['eta']]              <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_eta']]}, error=function(x) NULL), Type='numeric', Default=0.1)
  l[['min_child_weight']] <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_min_child_weight']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['subsample']]        <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_subsample']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['colsample_bytree']] <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_colsample_bytree']]}, error=function(x) NULL), Type='numeric', Default=1)

  # Code Collection
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# XGBoost ML Parameters\n",
    "ArgsList[['Trees']] <- ", Quantico:::CEPP(l[['Trees']]), "\n",
    "ArgsList[['max_depth']] <- ", Quantico:::CEPP(l[['max_depth']]), "\n",
    "ArgsList[['eta']] <- ", Quantico:::CEPP(l[['eta']]), "\n",
    "ArgsList[['min_child_weight']] <- ", Quantico:::CEPP(l[['min_child_weight']]), "\n",
    "ArgsList[['subsample']] <- ", Quantico:::CEPP(l[['subsample']]), "\n",
    "ArgsList[['colsample_bytree']] <- ", Quantico:::CEPP(l[['colsample_bytree']]), "\n"))

  if(Debug) print(' ::  BuildModels 7 :: ')
  if(Debug) print(TT)

  # XGBoost Eval Parameters
  if(TT == 'Regression') {
    if(Debug) print(' ::  BuildModels 7.1 :: ')

    # Store Args
    l[['LossFunction']]     <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='reg:squarederror')
    l[['eval_metric']]      <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='rmse')
    l[['grid_eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='r2')

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# XGBoost Evaluation Parameters\n",
      "ArgsList[['LossFunction']] <- ", Quantico:::CEP(l[['LossFunction']]), "\n",
      "ArgsList[['eval_metric']] <- ", Quantico:::CEP(l[['eval_metric']]), "\n",
      "ArgsList[['grid_eval_metric']] <- ", Quantico:::CEP(l[['grid_eval_metric']]), "\n"))

  } else if(TT == 'Binary Classification') {
    if(Debug) print(' ::  BuildModels 7.2 :: ')

    # Store Args
    l[['LossFunction']]     <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='binary:logistic')
    l[['eval_metric']]      <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='auc')
    l[['grid_eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='MCC')

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# XGBoost Evaluation Parameters\n",
      "ArgsList[['LossFunction']] <- ", Quantico:::CEP(l[['LossFunction']]), "\n",
      "ArgsList[['eval_metric']] <- ", Quantico:::CEP(l[['eval_metric']]), "\n",
      "ArgsList[['grid_eval_metric']] <- ", Quantico:::CEP(l[['grid_eval_metric']]), "\n"))

  } else {
    if(Debug) print(' ::  BuildModels 7.3 :: ')
    if(Debug) print(tryCatch({input[['XGBoost_LossFunction']]}, error=function(x) NULL))
    if(Debug) print(Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='multi:softprob'))

    # Store Args
    l[['LossFunction']]     <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_LossFunction']]}, error=function(x) NULL), Type='character', Default='multi:softprob')
    l[['eval_metric']]      <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_EvalMetric']]}, error=function(x) NULL), Type='character', Default='mlogloss')
    l[['grid_eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='microauc')

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# XGBoost Evaluation Parameters\n",
      "ArgsList[['LossFunction']] <- ", Quantico:::CEP(l[['LossFunction']]), "\n",
      "ArgsList[['eval_metric']] <- ", Quantico:::CEP(l[['eval_metric']]), "\n",
      "ArgsList[['grid_eval_metric']] <- ", Quantico:::CEP(l[['grid_eval_metric']]), "\n"))
  }
  return(list(ArgsList = l,CodeList = CodeList))
}

#' @title Shiny.ML.LightGBM_Params
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param l ArgsList created inside server.R
#' @param input input from shiny app
#' @param Debug Debug. TRUE or FALSE
#' @param TT E.g. XGBoost_TargetType. Can be 'Regression', 'Binary Classification', or 'MultiClass'. Case sensitive
#' @param CodeList From App
#'
#' @export
Shiny.ML.LightGBM_Params <- function(l,input,Debug,TT,CodeList) {

  print(' ::  BuildModels 7  :: ')

  # LightGBM ML Parameters
  l[['Trees']] <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_Trees']]}, error=function(x) NULL), Type='numeric', Default=1000)
  l[['max_depth']] <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_max_depth']]}, error=function(x) NULL), Type='numeric', Default=8)
  l[['eta']] <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_eta']]}, error=function(x) NULL), Type='numeric', Default=NULL)
  l[['min_data_in_leaf']] <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_min_data_in_leaf']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['num_leaves']] <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_num_leaves']]}, error=function(x) NULL), Type='numeric', Default=31)
  l[['bagging_fraction']] <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_bagging_fraction']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['feature_fraction']] <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_feature_fraction']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['feature_fraction_bynode']] <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_feature_fraction_bynode']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['lambda_l1']] <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_lambda_l1']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['lambda_l2']] <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_lambda_l2']]}, error=function(x) NULL), Type='numeric', Default=1)

  # Code Collection
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# LightGBM ML Parameters\n",
    "ArgsList[['Trees']] <- ", Quantico:::CEPP(l[['Trees']]), "\n",
    "ArgsList[['max_depth']] <- ", Quantico:::CEPP(l[['max_depth']]), "\n",
    "ArgsList[['eta']] <- ", Quantico:::CEPP(l[['eta']]), "\n",
    "ArgsList[['min_data_in_leaf']] <- ", Quantico:::CEPP(l[['min_data_in_leaf']]), "\n",
    "ArgsList[['num_leaves']] <- ", Quantico:::CEPP(l[['num_leaves']]), "\n",
    "ArgsList[['bagging_fraction']] <- ", Quantico:::CEPP(l[['bagging_fraction']]), "\n",
    "ArgsList[['feature_fraction']] <- ", Quantico:::CEPP(l[['feature_fraction']]), "\n",
    "ArgsList[['feature_fraction_bynode']] <- ", Quantico:::CEPP(l[['feature_fraction_bynode']]), "\n",
    "ArgsList[['lambda_l1']] <- ", Quantico:::CEPP(l[['lambda_l1']]), "\n",
    "ArgsList[['lambda_l2']] <- ", Quantico:::CEPP(l[['lambda_l2']]), "\n"))

  # LightGBM Eval Parameters
  if(TT == 'Regression') {
    if(Debug) print(' ::  BuildModels 8.1  :: ')

    # Store Args
    l[['objective']] <- 'regression'
    l[['metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_metric']]}, error=function(x) NULL), Type='character', Default='rmse')
    l[['grid_eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='mse')

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# LightGBM Evaluation Parameters\n",
      "ArgsList[['objective']] <- ", Quantico:::CEP(l[['objective']]), "\n",
      "ArgsList[['metric']] <- ", Quantico:::CEP(l[['metric']]), "\n",
      "ArgsList[['grid_eval_metric']] <- ", Quantico:::CEP(l[['grid_eval_metric']]), "\n"))

  } else if(TT == 'Binary Classification') {
    if(Debug) print(' ::  BuildModels 8.2  :: ')

    # Store Args
    l[['objective']] <- 'binary'
    l[['metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_metric']]}, error=function(x) NULL), Type='character', Default='binary_logloss')
    l[['grid_eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='MCC')

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# LightGBM Evaluation Parameters\n",
      "ArgsList[['objective']] <- ", Quantico:::CEP(l[['objective']]), "\n",
      "ArgsList[['metric']] <- ", Quantico:::CEP(l[['metric']]), "\n",
      "ArgsList[['grid_eval_metric']] <- ", Quantico:::CEP(l[['grid_eval_metric']]), "\n"))

  } else if(TT == 'MultiClass') {
    if(Debug) print(' ::  BuildModels 8.3  :: ')

    # Store Args
    l[['objective']] <- 'multiclass'
    l[['metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_metric']]}, error=function(x) NULL), Type='character', Default='multiclass')
    l[['grid_eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_grid_eval_metric']]}, error=function(x) NULL), Type='character', Default='Accuracy')

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# LightGBM Evaluation Parameters\n",
      "ArgsList[['objective']] <- ", Quantico:::CEP(l[['objective']]), "\n",
      "ArgsList[['metric']] <- ", Quantico:::CEP(l[['metric']]), "\n",
      "ArgsList[['grid_eval_metric']] <- ", Quantico:::CEP(l[['grid_eval_metric']]), "\n"))

  }
  return(list(ArgsList = l,CodeList = CodeList))
}

#' @title Shiny.ML.H2O_DRF_Params
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param l ArgsList created inside server.R
#' @param input input from shiny app
#' @param Debug Debug. TRUE or FALSE
#' @param TT E.g. XGBoost_TargetType. Can be 'Regression', 'Binary Classification', or 'MultiClass'. Case sensitive
#' @param CodeList From App
#'
#' @export
Shiny.ML.H2O_DRF_Params <- function(l,input,Debug,TT,CodeList) {

  print(' ::  BuildModels 7  :: ')

  # H2O_DRF ML Parameters
  l[['Trees']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_Trees']]}, error=function(x) NULL), Type='numeric', Default=50)
  l[['MTries']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_MTries']]}, error=function(x) NULL), Type='numeric', Default=-1)
  l[['MaxDepth']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_MaxDepth']]}, error=function(x) NULL), Type='numeric', Default=20)
  l[['SampleRate']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_SampleRate']]}, error=function(x) NULL), Type='numeric', Default=0.615)
  l[['ColSampleRatePerTree']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_ColSampleRatePerTree']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['MinRows']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_MinRows']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['NBinsCats']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_NBinsCats']]}, error=function(x) NULL), Type='numeric', Default=1024)
  l[['NBinsTopLevel']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_NBinsTopLevel']]}, error=function(x) NULL), Type='numeric', Default=1024)
  l[['HistogramType']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_HistogramType']]}, error=function(x) NULL), Type='character', Default='AUTO')
  l[['CategoricalEncoding']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_CategoricalEncoding']]}, error=function(x) NULL), Type='character', Default='AUTO')
  l[['StoppingRounds']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_StoppingRounds']]}, error=function(x) NULL), Type='numeric', Default=10)

  # Code Collection
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# H2O-DRF ML Parameters\n",
    "ArgsList[['Trees']] <- ", Quantico:::CEPP(l[['Trees']]), "\n",
    "ArgsList[['MaxDepth']] <- ", Quantico:::CEPP(l[['MaxDepth']]), "\n",
    "ArgsList[['SampleRate']] <- ", Quantico:::CEPP(l[['SampleRate']]), "\n",
    "ArgsList[['MTries']] <- ", Quantico:::CEPP(l[['MTries']]), "\n",
    "ArgsList[['ColSampleRatePerTree']] <- ", Quantico:::CEPP(l[['ColSampleRatePerTree']]), "\n",
    "ArgsList[['MinRows']] <- ", Quantico:::CEPP(l[['MinRows']]), "\n",
    "ArgsList[['NBinsCats']] <- ", Quantico:::CEPP(l[['NBinsCats']]), "\n",
    "ArgsList[['NBinsTopLevel']] <- ", Quantico:::CEPP(l[['NBinsTopLevel']]), "\n",
    "ArgsList[['HistogramType']] <- ", Quantico:::CEP(l[['HistogramType']]), "\n",
    "ArgsList[['CategoricalEncoding']] <- ", Quantico:::CEP(l[['CategoricalEncoding']]), "\n",
    "ArgsList[['StoppingRounds']] <- ", Quantico:::CEPP(l[['StoppingRounds']]), "\n"))

  # H2O_DRF Eval Parameters
  if(TT == 'Regression') {
    if(Debug) print(' ::  BuildModels 8.1  :: ')

    # Store Args
    l[['eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_eval_metric']]}, error=function(x) NULL), Type='character', Default='rmse')

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# H2O-DRF Evaluation Parameters\n",
      "ArgsList[['eval_metric']] <- ", Quantico:::CEP(l[['eval_metric']]), "\n"))

  } else if(TT == 'Binary Classification') {
    if(Debug) print(' ::  BuildModels 8.2  :: ')

    # Store Args
    l[['eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_eval_metric']]}, error=function(x) NULL), Type='character', Default='binary_logloss')

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# H2O-DRF Evaluation Parameters\n",
      "ArgsList[['eval_metric']] <- ", Quantico:::CEP(l[['eval_metric']]), "\n"))

  } else if(TT == 'MultiClass') {
    if(Debug) print(' ::  BuildModels 8.3  :: ')

    # Store Args
    l[['eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_eval_metric']]}, error=function(x) NULL), Type='character', Default='multiclass')

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# H2O-DRF Evaluation Parameters\n",
      "ArgsList[['eval_metric']] <- ", Quantico:::CEP(l[['eval_metric']]), "\n"))

  }
  return(list(ArgsList = l,CodeList = CodeList))
}

#' @title Shiny.ML.H2O_GBM_Params
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param l ArgsList created inside server.R
#' @param input input from shiny app
#' @param Debug Debug. TRUE or FALSE
#' @param TT E.g. XGBoost_TargetType. Can be 'Regression', 'Binary Classification', or 'MultiClass'. Case sensitive
#' @param CodeList From App
#'
#' @export
Shiny.ML.H2O_GBM_Params <- function(l,input,Debug,TT,CodeList) {

  print(' ::  BuildModels 7  :: ')

  # H2O_GBM ML Parameters
  l[['Trees']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_Trees']]}, error=function(x) NULL), Type='numeric', Default=1000)
  l[['MaxDepth']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_MaxDepth']]}, error=function(x) NULL), Type='numeric', Default=6)
  l[['SampleRate']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_SampleRate']]}, error=function(x) NULL), Type='numeric', Default=0.615)
  l[['ColSampleRatePerTree']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_ColSampleRatePerTree']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['MinRows']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_MinRows']]}, error=function(x) NULL), Type='numeric', Default=1)
  l[['NBinsCats']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_NBinsCats']]}, error=function(x) NULL), Type='numeric', Default=1024)
  l[['NBinsTopLevel']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_NBinsTopLevel']]}, error=function(x) NULL), Type='numeric', Default=1024)
  l[['HistogramType']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_HistogramType']]}, error=function(x) NULL), Type='character', Default='AUTO')
  l[['CategoricalEncoding']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_CategoricalEncoding']]}, error=function(x) NULL), Type='character', Default='AUTO')
  l[['StoppingRounds']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_StoppingRounds']]}, error=function(x) NULL), Type='numeric', Default=10)
  l[['LearnRate']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_LearnRate']]}, error=function(x) NULL), Type='numeric', Default=0.10)
  l[['LearnRateAnnealing']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_MinRows']]}, error=function(x) NULL), Type='numeric', Default=0.999)
  l[['Distribution']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_Distribution']]}, error=function(x) NULL), Type='character', Default='AUTO')

  # Code Collection
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# H2O-GBM ML Parameters\n",
    "ArgsList[['Trees']] <- ", Quantico:::CEPP(l[['Trees']]), "\n",
    "ArgsList[['MaxDepth']] <- ", Quantico:::CEPP(l[['MaxDepth']]), "\n",
    "ArgsList[['SampleRate']] <- ", Quantico:::CEPP(l[['SampleRate']]), "\n",
    "ArgsList[['ColSampleRatePerTree']] <- ", Quantico:::CEPP(l[['ColSampleRatePerTree']]), "\n",
    "ArgsList[['MinRows']] <- ", Quantico:::CEPP(l[['MinRows']]), "\n",
    "ArgsList[['NBinsCats']] <- ", Quantico:::CEPP(l[['NBinsCats']]), "\n",
    "ArgsList[['NBinsTopLevel']] <- ", Quantico:::CEPP(l[['NBinsTopLevel']]), "\n",
    "ArgsList[['HistogramType']] <- ", Quantico:::CEP(l[['HistogramType']]), "\n",
    "ArgsList[['CategoricalEncoding']] <- ", Quantico:::CEP(l[['CategoricalEncoding']]), "\n",
    "ArgsList[['StoppingRounds']] <- ", Quantico:::CEPP(l[['StoppingRounds']]), "\n",
    "ArgsList[['LearnRate']] <- ", Quantico:::CEPP(l[['LearnRate']]), "\n",
    "ArgsList[['LearnRateAnnealing']] <- ", Quantico:::CEPP(l[['LearnRateAnnealing']]), "\n",
    "ArgsList[['Distribution']] <- ", Quantico:::CEP(l[['Distribution']]), "\n"))

  # H2O_GBM Eval Parameters
  if(TT == 'Regression') {
    if(Debug) print(' ::  BuildModels 8.1  :: ')

    # Store Args
    l[['eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_eval_metric']]}, error=function(x) NULL), Type='character', Default='rmse')

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# H2O-GBM Evaluation Parameters\n",
      "ArgsList[['eval_metric']] <- ", Quantico:::CEP(l[['eval_metric']]), "\n"))

  } else if(TT == 'Binary Classification') {
    if(Debug) print(' ::  BuildModels 8.2  :: ')

    # Store Args
    l[['eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_eval_metric']]}, error=function(x) NULL), Type='character', Default='binary_logloss')

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# H2O-GBM Evaluation Parameters\n",
      "ArgsList[['eval_metric']] <- ", Quantico:::CEP(l[['eval_metric']]), "\n"))

  } else if(TT == 'MultiClass') {
    if(Debug) print(' ::  BuildModels 8.3  :: ')

    # Store Args
    l[['eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_eval_metric']]}, error=function(x) NULL), Type='character', Default='multiclass')

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# H2O-GBM Evaluation Parameters\n",
      "ArgsList[['eval_metric']] <- ", Quantico:::CEP(l[['eval_metric']]), "\n"))

  }
  return(list(ArgsList = l,CodeList = CodeList))
}

#' @title Shiny.ML.H2O_GLM_Params
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param l ArgsList created inside server.R
#' @param input input from shiny app
#' @param Debug Debug. TRUE or FALSE
#' @param TT E.g. XGBoost_TargetType. Can be 'Regression', 'Binary Classification', or 'MultiClass'. Case sensitive
#' @param CodeList From App
#'
#' @export
Shiny.ML.H2O_GLM_Params <- function(l,input,Debug,TT,CodeList) {

  print(' ::  BuildModels 7  :: ')

  # H2O_DRF ML Parameters
  l[['Distribution']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_Distribution']]}, error=function(x) NULL), Type='character', Default='AUTO')
  l[['Link']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_Link']]}, error=function(x) NULL), Type='character', Default='family_default')
  l[['Solver']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_Solver']]}, error=function(x) NULL), Type='character', Default='IRSLM')
  l[['Alpha']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_Alpha']]}, error=function(x) NULL), Type='numeric', Default=0.5)
  l[['LambdaSearch']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_LambdaSearch']]}, error=function(x) NULL), Type='logical', Default=FALSE)
  if(length(l[['LambdaSearch']]) == 0L) l[['Lambda']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_Lambda']]}, error=function(x) NULL), Type='character', Default=0)
  l[['Standardize']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_Standardize']]}, error=function(x) NULL), Type='logical', Default=TRUE)
  l[['RemoveCollinearColumns']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_RemoveCollinearColumns']]}, error=function(x) NULL), Type='logical', Default=TRUE)
  l[['NonNegativeCoefficients']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_NonNegativeCoefficients']]}, error=function(x) NULL), Type='logical', Default=FALSE)

  # Code Collection
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# H2O-GLM ML Parameters\n",
    "ArgsList[['Distribution']] <- ", Quantico:::CEP(l[['Distribution']]), "\n",
    "ArgsList[['Link']] <- ", Quantico:::CEP(l[['Link']]), "\n",
    "ArgsList[['Solver']] <- ", Quantico:::CEP(l[['Solver']]), "\n",
    "ArgsList[['Alpha']] <- ", Quantico:::CEP(l[['Alpha']]), "\n",
    if(length(l[['LambdaSearch']]) == 0L) "ArgsList[['Lambda']] <- ", Quantico:::CEPP(l[['Lambda']]), "\n",
    "ArgsList[['LambdaSearch']] <- ", Quantico:::CEPP(l[['LambdaSearch']]), "\n",
    "ArgsList[['Standardize']] <- ", Quantico:::CEPP(l[['Standardize']]), "\n",
    "ArgsList[['RemoveCollinearColumns']] <- ", Quantico:::CEPP(l[['RemoveCollinearColumns']]), "\n",
    "ArgsList[['NonNegativeCoefficients']] <- ", Quantico:::CEPP(l[['NonNegativeCoefficients']]), "\n"))

  # H2O_DRF Eval Parameters
  if(TT == 'Regression') {
    if(Debug) print(' ::  BuildModels 8.1  :: ')

    # Store Args
    l[['TweedieVariancePower']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_TweedieVariancePower']]}, error=function(x) NULL), Type='numeric', Default=1.2)
    l[['eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_eval_metric']]}, error=function(x) NULL), Type='character', Default='RMSE') # MSE, RMSE, MAE, RMSLE

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# H2O-GLM Evaluation Parameters\n",
      "ArgsList[['TweedieVariancePower']] <- ", Quantico:::CEPP(l[['TweedieVariancePower']]), "\n",
      "ArgsList[['eval_metric']] <- ", Quantico:::CEP(l[['eval_metric']]), "\n"))

  } else if(TT == 'Binary Classification') {
    if(Debug) print(' ::  BuildModels 8.2  :: ')

    # Store Args
    l[['eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_eval_metric']]}, error=function(x) NULL), Type='character', Default='auc') # 'auc' only

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# H2O-GLM Evaluation Parameters\n",
      "ArgsList[['eval_metric']] <- ", Quantico:::CEP(l[['eval_metric']]), "\n"))

  } else if(TT == 'MultiClass') {
    if(Debug) print(' ::  BuildModels 8.3  :: ')

    # Store Args
    l[['eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_eval_metric']]}, error=function(x) NULL), Type='character', Default='logloss') # 'logloss' only

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# H2O-GLM Evaluation Parameters\n",
      "ArgsList[['eval_metric']] <- ", Quantico:::CEP(l[['eval_metric']]), "\n"))

  }
  return(list(ArgsList = l,CodeList = CodeList))
}

#' @title Shiny.ML.H2O_HGLM_Params
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param l ArgsList created inside server.R
#' @param input input from shiny app
#' @param Debug Debug. TRUE or FALSE
#' @param TT E.g. XGBoost_TargetType. Can be 'Regression', 'Binary Classification', or 'MultiClass'. Case sensitive
#' @param CodeList From App
#'
#' @export
Shiny.ML.H2O_HGLM_Params <- function(l,input,Debug,TT,CodeList) {

  print(' ::  BuildModels 7  :: ')

  # H2O_DRF ML Parameters
  l[['RandomColNumbers']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_RandomColNumbers']]}, error=function(x) NULL), Type='character', Default=NULL)
  l[['Distribution']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_Distribution']]}, error=function(x) NULL), Type='character', Default='gaussian')
  l[['RandomDistribution']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_Distribution']]}, error=function(x) NULL), Type='character', Default='Gaussian')
  l[['Link']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_Link']]}, error=function(x) NULL), Type='character', Default='identity')
  l[['RandomLink']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_RandomLink']]}, error=function(x) NULL), Type='character', Default='identity')
  l[['Solver']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_Solver']]}, error=function(x) NULL), Type='character', Default='AUTO')
  l[['Alpha']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_Alpha']]}, error=function(x) NULL), Type='numeric', Default=0.5)
  l[['LambdaSearch']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_LambdaSearch']]}, error=function(x) NULL), Type='logical', Default=FALSE)
  if(length(l[['LambdaSearch']]) == 0L) l[['Lambda']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_Lambda']]}, error=function(x) NULL), Type='character', Default=0)
  l[['Standardize']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_Standardize']]}, error=function(x) NULL), Type='logical', Default=TRUE)
  l[['RemoveCollinearColumns']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_RemoveCollinearColumns']]}, error=function(x) NULL), Type='logical', Default=FALSE)
  l[['NonNegativeCoefficients']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_NonNegativeCoefficients']]}, error=function(x) NULL), Type='logical', Default=FALSE)

  # Code Collection
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# H2O-GLM ML Parameters\n",
    "ArgsList[['RandomColNumbers']] <- ", Quantico:::ExpandText(l[['RandomColNumbers']]), "\n",
    "ArgsList[['RandomDistribution']] <- ", Quantico:::CEP(l[['RandomDistribution']]), "\n",
    "ArgsList[['Distribution']] <- ", Quantico:::CEP(l[['Distribution']]), "\n",
    "ArgsList[['Link']] <- ", Quantico:::CEP(l[['Link']]), "\n",
    "ArgsList[['RandomLink']] <- ", Quantico:::CEP(l[['RandomLink']]), "\n",
    "ArgsList[['Solver']] <- ", Quantico:::CEP(l[['Solver']]), "\n",
    "ArgsList[['Alpha']] <- ", Quantico:::CEP(l[['Alpha']]), "\n",
    "ArgsList[['Lambda']] <- ", if(length(l[['LambdaSearch']]) == 0L) Quantico:::CEPP(l[['Lambda']]) else 'NULL', "\n",
    "ArgsList[['LambdaSearch']] <- ", Quantico:::CEPP(l[['LambdaSearch']]), "\n",
    "ArgsList[['Standardize']] <- ", Quantico:::CEPP(l[['Standardize']]), "\n",
    "ArgsList[['RemoveCollinearColumns']] <- ", Quantico:::CEPP(l[['RemoveCollinearColumns']]), "\n",
    "ArgsList[['NonNegativeCoefficients']] <- ", Quantico:::CEPP(l[['NonNegativeCoefficients']]), "\n"))

  # H2O_DRF Eval Parameters
  if(TT == 'Regression') {
    if(Debug) print(' ::  BuildModels 8.1  :: ')

    # Store Args
    l[['TweedieVariancePower']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_TweedieVariancePower']]}, error=function(x) NULL), Type='numeric', Default=1.2)
    l[['eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_eval_metric']]}, error=function(x) NULL), Type='character', Default='RMSE') # MSE, RMSE, MAE, RMSLE

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# H2O-GLM Evaluation Parameters\n",
      "ArgsList[['TweedieVariancePower']] <- ", Quantico:::CEPP(l[['TweedieVariancePower']]), "\n",
      "ArgsList[['eval_metric']] <- ", Quantico:::CEP(l[['eval_metric']]), "\n"))

  } else if(TT == 'Binary Classification') {
    if(Debug) print(' ::  BuildModels 8.2  :: ')

    # Store Args
    l[['eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_eval_metric']]}, error=function(x) NULL), Type='character', Default='auc') # 'auc' only

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# H2O-GLM Evaluation Parameters\n",
      "ArgsList[['eval_metric']] <- ", Quantico:::CEP(l[['eval_metric']]), "\n"))

  } else if(TT == 'MultiClass') {
    if(Debug) print(' ::  BuildModels 8.3  :: ')

    # Store Args
    l[['eval_metric']] <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_eval_metric']]}, error=function(x) NULL), Type='character', Default='logloss') # 'logloss' only

    # Code Collection
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# H2O-GLM Evaluation Parameters\n",
      "ArgsList[['eval_metric']] <- ", Quantico:::CEP(l[['eval_metric']]), "\n"))

  }
  return(list(ArgsList = l,CodeList = CodeList))
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# ML Training and Output                                                                     ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Shiny.ML.ModelID
#'
#' @description Generate a new ModelID if a user doesn't supply one
#'
#' @param x ModelID as returned from an input in shiny
#'
#' @examples
#' \dontrun{
#' x <- Shiny.ML.ModelID(LGBM_MetaData[['LightGBM_ModelID']][['SelectedDefault']][[length(LGBM_MetaData[['LightGBM_ModelID']][['SelectedDefault']])]])
#' }
#'
#' @noRd
Shiny.ML.ModelID <- function(x) {
  if(length(x) > 0 && substr(x = x, start = 1L, stop = 5L) == 'Model') {
    if(nchar(x) == 6L) {
      x <- paste0('Model', as.integer(substr(x, 6L, 6L)) + 1L)
    } else if(nchar(x) == 7L) {
      x <- paste0('Model', as.integer(substr(x, 6L, 7L)) + 1L)
    } else if(nchar(x) == 8L) {
      x <- paste0('Model', as.integer(substr(x, 6L, 8L)) + 1L)
    } else if(nchar(x) == 9L) {
      x <- paste0('Model', as.integer(substr(x, 6L, 9L)) + 1L)
    } else if(nchar(x) == 10L) {
      x <- paste0('Model', as.integer(substr(x, 6L, 10L)) + 1L)
    }
  } else {
    x <- 'Model1'
  }
  return(x)
}

#' @title Shiny.ML.StoreInputArgs
#'
#' @description Generate a new ModelID if a user doesn't supply one
#'
#' @param session From App
#' @param input From App
#' @param x ModelID as returned from an input in shiny
#' @param list From App
#' @param Params From App
#' @param Debug From App
#'
#' @examples
#' \dontrun{
#' x <- Shiny.ML.StoreInputArgs(session,input,Cat_DataParameters,c('CatBoost_Trees'),Debug)
#' }
#'
#' @noRd
Shiny.ML.StoreInputArgs <- function(session, input, list, Params, Debug) {

  for(i in Params) {
    if(Debug) {print(i); print(input[[i]])}
    list[[i]][['SelectedDefault']][[length(list[[i]][['SelectedDefault']]) + 1L]] <- tryCatch({input[[i]]}, error = function(x) NULL)
  }
  return(list)
}

#' @title Shiny.ML.Trainer
#'
#' @description ML Trainer
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param input shiny input
#' @param output shiny output
#' @param DataList DataList stores data in app
#' @param ArgsList ArgsList
#' @param CodeList CodeList from app
#' @param ModelOutputList from app
#' @param TT TargetType
#' @param ML_ExperimentTable Collection table
#' @param run Cross Validation Iteration Number in app
#' @param n Total Cross Validation Iterations in app
#' @param Debug Debug from app
#' @param Algo 'CatBoost', 'XGBoost', 'LightGBM', 'H2O_DRF', 'H2O_GBM'
#'
#' @return a list of columns names by data type
#'
#' @export
Shiny.ML.Trainer <- function(input,
                             output,
                             DataList,
                             ArgsList,
                             CodeList,
                             ModelOutputList,
                             TT,
                             ML_ExperimentTable,
                             run,
                             n,
                             Debug,
                             TabCount=5L,
                             Algo = 'CatBoost',
                             wd = NULL) {

  # Data, Target variable, Features
  temp_data <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_data')]]}, error=function(x) NULL), Type='character', Default=NULL)
  if(length(temp_data) != 0) ArgsList[['data']] <- data.table::copy(DataList[[temp_data]][['data']]) else ArgsList[['data']] <- NULL
  temp_validation <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_ValidationData')]]}, error=function(x) NULL), Type='character', Default=NULL)
  if(length(temp_validation) != 0) ArgsList[['ValidationData']] <- data.table::copy(DataList[[temp_validation]][['data']]) else ArgsList[['ValidationData']] <- NULL
  temp_test <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_TestData')]]}, error=function(x) NULL), Type='character', Default=NULL)
  if(length(temp_test) != 0) ArgsList[['TestData']] <- data.table::copy(DataList[[temp_test]][['data']]) else ArgsList[['TestData']] <- NULL

  if(Debug) print(' ::  BuildModels 3  :: ')
  ArgsList[['TargetColumnName']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_TargetColumnName')]]}, error=function(x) NULL), Type='character', Default=NULL)
  ArgsList[['FeatureColNames']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_FeatureColNames')]]}, error=function(x) NULL), Type='character', Default=NULL)
  if(Debug) print("class(ArgsList[['data']])[[1L]] %in% 'data.table' && length(ArgsList[['TargetColumnName']]) != 0L && length(ArgsList[['FeatureColNames']]) != 0L")

  # If run < n, combine data so that
  #   data is randomized and partitioned in the
  #   Auto__() function for Cross Validation Purposes
  RunReady <- FALSE
  check <- n > 1L
  if(Debug) {
    print(class(ArgsList[['data']])[[1L]] %in% 'data.table')
    print(length(ArgsList[['TargetColumnName']]) != 0L)
    print(length(ArgsList[['FeatureColNames']]) != 0L)
  }
  if(class(ArgsList[['data']])[[1L]] %in% 'data.table' && length(ArgsList[['TargetColumnName']]) != 0L && length(ArgsList[['FeatureColNames']]) != 0L) {
    RunReady <- TRUE
    if(check) {
      if(class(ArgsList[['ValidationData']])[[1L]] %in% 'data.table') {
        ArgsList[['data']] <- data.table::rbindlist(list(ArgsList[['data']], ArgsList[['ValidationData']]), use.names = TRUE, fill = TRUE)
        ArgsList[['ValidationData']] <- NULL
      }
      if(class(ArgsList[['TestData']])[[1L]] %in% 'data.table') {
        ArgsList[['data']] <- data.table::rbindlist(list(ArgsList[['data']], ArgsList[['TestData']]), use.names = TRUE, fill = TRUE)
        ArgsList[['TestData']] <- NULL
      }
    }
  }

  # Code Collection
  if(check && class(ArgsList[['ValidationData']])[[1L]] %in% 'data.table' && class(ArgsList[['TestData']])[[1L]] %in% 'data.table') {
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# ML Data\n",
      "if(!exists('ArgsList')) ArgsList <- list()\n",
      "ArgsList <- list()\n",
      "ArgsList[['data']] <- data.table::rbindlist(list(\n  ",
      "data.table::copy(DataList[[", Quantico:::CEP(temp_data), "]])\n  ",
      "data.table::copy(DataList[[", Quantico:::CEP(temp_validation), "]])\n  ",
      "data.table::copy(DataList[[", Quantico:::CEP(temp_test), "]])))\n"))
  } else if(check && class(ArgsList[['ValidationData']])[[1L]] %in% 'data.table' && !class(ArgsList[['TestData']])[[1L]] %in% 'data.table') {
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# ML Data\n",
      "if(!exists('ArgsList')) ArgsList <- list()\n",
      "ArgsList[['data']] <- data.table::rbindlist(list(\n  ",
      "data.table::copy(DataList[[", Quantico:::CEP(temp_data), "]])\n  ",
      "data.table::copy(DataList[[", Quantico:::CEP(temp_validation), "]])))\n"))
  } else if(check && !class(ArgsList[['ValidationData']])[[1L]] %in% 'data.table' && class(ArgsList[['TestData']])[[1L]] %in% 'data.table') {
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# ML Data\n",
      "if(!exists('ArgsList')) ArgsList <- list()\n",
      "ArgsList[['data']] <- data.table::rbindlist(list(\n  ",
      "data.table::copy(DataList[[", Quantico:::CEP(temp_data), "]])\n  ",
      "data.table::copy(DataList[[", Quantico:::CEP(temp_test), "]])))\n"))
  } else if(check && !class(ArgsList[['ValidationData']])[[1L]] %in% 'data.table' && !class(ArgsList[['TestData']])[[1L]] %in% 'data.table') {
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# ML Data\n",
      "if(!exists('ArgsList')) ArgsList <- list()\n",
      "ArgsList[['data']] <- data.table::copy(DataList[[", Quantico:::CEP(temp_data), "]])\n"))
  } else if(!check && class(ArgsList[['ValidationData']])[[1L]] %in% 'data.table' && class(ArgsList[['TestData']])[[1L]] %in% 'data.table') {
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# ML Data\n",
      "if(!exists('ArgsList')) ArgsList <- list()\n",
      "ArgsList[['data']] <- data.table::copy(DataList[[", Quantico:::CEP(temp_data), "]])\n  ",
      "ArgsList[['ValidationData']] <- data.table::copy(DataList[[", Quantico:::CEP(temp_validation), "]])\n  ",
      "ArgsList[['TestData']] <- data.table::copy(DataList[[", Quantico:::CEP(temp_test), "]])\n"))
  } else if(!check && class(ArgsList[['ValidationData']])[[1L]] %in% 'data.table' && !class(ArgsList[['TestData']])[[1L]] %in% 'data.table') {
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# ML Data\n",
      "if(!exists('ArgsList')) ArgsList <- list()\n",
      "ArgsList[['data']] <- data.table::copy(DataList[[", Quantico:::CEP(temp_data), "]])\n  ",
      "ArgsList[['ValidationData']] <- data.table::copy(DataList[[", Quantico:::CEP(temp_validation), "]])\n"))
  } else if(!check && !class(ArgsList[['ValidationData']])[[1L]] %in% 'data.table' && class(ArgsList[['TestData']])[[1L]] %in% 'data.table') {
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# ML Data\n",
      "if(!exists('ArgsList')) ArgsList <- list()\n",
      "ArgsList[['data']] <- data.table::copy(DataList[[", Quantico:::CEP(temp_data), "]])\n  ",
      "ArgsList[['TestData']] <- data.table::copy(DataList[[", Quantico:::CEP(temp_test), "]])\n"))
  } else if(!check && !class(ArgsList[['ValidationData']])[[1L]] %in% 'data.table' && !class(ArgsList[['TestData']])[[1L]] %in% 'data.table') {
    CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# ML Data\n",
      "if(!exists('ArgsList')) ArgsList <- list()\n",
      "ArgsList[['data']] <- data.table::copy(DataList[[", Quantico:::CEP(temp_data), "]])\n"))
  }

  # ML Build
  if(Debug) print(paste0(' ::  BuildModels 1  :: ', RunReady))
  if(!RunReady) {
    return(list(
      DataList = DataList,
      ArgsList = ArgsList,
      CodeList = CodeList
    ))

  } else {

    # Starting logic: 100 rows - 100 rows = 0 row, need 1 row
    if(Debug) print("ML_Experiment Table Start")
    if(Debug) print(ML_ExperimentTable)
    if(length(ML_ExperimentTable) == 0L) {
      if(TT == "Regression") {
        ML_ExperimentTable <- DataList[["ML_RegressionMetrics"]][["sample"]]
      } else if(TT == "Binary Classification") {
        ML_ExperimentTable <- DataList[["ML_ClassificationMetrics"]][["sample"]]
      } else if(TT == "MultiClass") {
        ML_ExperimentTable <- DataList[["ML_MultiClassMetrics"]][["sample"]]
      }
    }
    iter <- ML_ExperimentTable[, .N] - ML_ExperimentTable[`Model ID` == 'zzz', .N] + 1L

    # Target Type
    if(TT == 'Regression' && class(ArgsList[['data']][[ArgsList[['TargetColumnName']]]])[1L] %in% c('character','factor')) {
      TT <- 'MultiClass'
    } else if(TT == 'MultiClass' && class(ArgsList[['data']][[ArgsList[['TargetColumnName']]]])[1L] %in% c('numeric','integer')) {
      TT <- 'Regression'
    } else if(TT == 'Binary Classification' && class(ArgsList[['data']][[ArgsList[['TargetColumnName']]]])[1L] %in% c('character','factor')) {
      TT <- 'MultiClass'
    }
    if(Debug) print(paste0('TT = ', TT))

    # Notification of starting
    shiny::showNotification(paste0(Algo, 'Building has Begun!'))

    # MetaData Parameters
    ArgsList[['OutputSelection']] <- c('Importances', 'EvalMetrics', 'Score_TrainData')
    ArgsList[['TrainOnFull']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_TrainOnFull')]]}, error=function(x) NULL), Type='logical', Default=FALSE)
    ArgsList[['ModelID']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_ModelID')]]}, error=function(x) NULL), Type='character', Default='Model_1')
    ArgsList[['DebugMode']] <- Debug
    ArgsList[['model_path']] <- wd
    ArgsList[['metadata_path']] <- NULL
    ArgsList[['SaveModelObjects']] <- FALSE
    ArgsList[['ReturnModelObjects']] <- TRUE
    if(TT != 'MultiClass') ArgsList[['NumOfParDepPlots']] <- 0
    if(TT != 'MultiClass') ArgsList[['SaveInfoToPDF']] <- FALSE

    if(Debug) print(' ::  BuildModels 2  :: ')

    # Data Parameters

    # Data
    ArgsList[['WeightsColumn']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_WeightsColumnName')]]}, error=function(x) NULL), Type='character', Default=NULL)
    if(TT == 'Regression') {
      ArgsList[['TransformNumericColumns']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_TransformNumericColumns')]]}, error=function(x) NULL), Type='character', Default=NULL)
      ArgsList[['Methods']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_Methods')]]}, error=function(x) NULL), Type='character', Default=NULL)
    }

    if(Debug) print(' ::  BuildModels 5  :: ')

    # Grid Tuning Parameters
    if(!Algo %in% c('H2O_DRF','H2O_GBM','H2O_GLM','H2O_HGLM')) ArgsList[['PassInGrid']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_PassInGrid')]]}, error=function(x) NULL), Type='character', Default=NULL)
    ArgsList[['GridTune']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_GridTune')]]}, error=function(x) NULL), Type='logical', Default=FALSE)
    ArgsList[['MaxModelsInGrid']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_MaxModelsInGrid')]]}, error=function(x) NULL), Type='numeric', Default=30)
    if(!Algo %in% c('H2O_DRF','H2O_GBM','H2O_GLM','H2O_HGLM')) ArgsList[['MaxRunsWithoutNewWinner']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_MaxRunsWithoutNewWinner')]]}, error=function(x) NULL), Type='numeric', Default=15)
    if(!Algo %in% c('H2O_DRF','H2O_GBM','H2O_GLM','H2O_HGLM')) ArgsList[['MaxRunMinutes']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_MaxRunMinutes')]]}, error=function(x) NULL), Type='numeric', Default=30)
    if(!Algo %in% c('H2O_DRF','H2O_GBM','H2O_GLM','H2O_HGLM')) ArgsList[['BaselineComparison']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_BaselineComparison')]]}, error=function(x) NULL), Type='character', Default='default')

    # Code Collection
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# ML Set Metadata Parameters\n",
      "ArgsList[['TargetColumnName']] <- ", Quantico:::ExpandText(ArgsList[['TargetColumnName']]),"\n",
      "ArgsList[['FeatureColNames']] <- ", Quantico:::ExpandText(ArgsList[['FeatureColNames']]),"\n\n",
      "# Additional Metadata Parameters\n",
      "ArgsList[['OutputSelection']] <- c('Importances','EvalMetrics','Score_TrainData')\n",
      "ArgsList[['TrainOnFull']] <- ", Quantico:::CEPP(ArgsList[['TrainOnFull']]), "\n",
      "ArgsList[['ModelID']] <- ", Quantico:::CEP(ArgsList[['ModelID']]), "\n",
      "ArgsList[['DebugMode']] <- ", Quantico:::CEPP(Debug), "\n",
      "ArgsList[['model_path']] <- ", Quantico:::CEP(NULL), "\n",
      "ArgsList[['metadata_path']] <- ", Quantico:::CEP(NULL),"\n",
      "ArgsList[['SaveModelObjects']] <- FALSE\n",
      "ArgsList[['ReturnModelObjects']] <- TRUE\n",
      if(Algo %in% c('H2O_DRF','H2O_GBM','H2O_GLM','H2O_HGLM')) "ArgsList[['H2OStartUp']] <- TRUE\n",
      if(Algo %in% c('H2O_DRF','H2O_GBM','H2O_GLM','H2O_HGLM')) "ArgsList[['H2OShutdown']] <- TRUE\n",
      "ArgsList[['NumOfParDepPlots']] <- 1\n"))}, error = function(x) CodeList)

    # Auto Save for H2O because if not, the instance gets shut down and then you can't save the model object
    if(Algo %in% c("H2O_DRF","H2O_GBM","H2O_GLM")) {
      ArgsList[["ModelID"]] <- paste0("ML_", ArgsList[["ModelID"]])
      NewDirectory <- file.path(wd, ArgsList[["ModelID"]])
      dir.create(path = NewDirectory)
      ArgsList[["SaveModelObjects"]] <- TRUE
      ArgsList[["IfSaveModel"]] <- "mojo"
      for(adsfadsf in 1:10) print(ArgsList[["IfSaveModel"]])
      ArgsList[["model_path"]] <- NewDirectory
    }

    # Build Model
    if(Algo == 'CatBoost') {

      ArgsList[['PrimaryDateColumn']] <- NULL # NO LONGER USEFUL
      ArgsList[['EncodeMethod']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_EncodeMethod')]]}, error=function(x) NULL), Type='character', Default='credibility')

      #if(!Algo %in% c('H2O_DRF','H2O_GBM','H2O_GLM')) ArgsList[['IDcols']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_IDcols')]]}, error=function(x) NULL), Type='character', Default=NULL)
      gg <- c(ArgsList[['TargetColumnName']], ArgsList[['FeatureColNames']], ArgsList[['PrimaryDateColumn']], ArgsList[['WeightsColumnName']])
      gg <- Quantico:::CleanVector(gg)
      if(length(gg) > 0L) {
        ArgsList[['IDcols']] <- setdiff(names(ArgsList[['data']]), gg)
      }

      # CatBoost ML Args
      Output <- Quantico:::Shiny.ML.CatBoost_Params(ArgsList,input,Debug,TT,CodeList)
      ArgsList <- Output$ArgsList
      CodeList <- Output$CodeList

      # Build model
      if(TT == 'Regression') {
        if(length(ArgsList[['TargetColumnName']]) == 1L) {
          ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]] <- do.call(what = AutoQuant::AutoCatBoostRegression, args = ArgsList)
        } else {
          ArgsList[['eval_metric']] <- 'MultiRMSE'
          ArgsList[['loss_function']] <- 'MultiRMSE'
          ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]] <- do.call(what = AutoQuant::AutoCatBoostVectorRegression, args = ArgsList)
        }

        # Code collect
        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['PrimaryDateColumn']] <- ", Quantico:::CEP(ArgsList[['PrimaryDateColumn']]), "\n",
          "ArgsList[['EncodeMethod']] <- ", Quantico:::CEP(ArgsList[['EncodeMethod']]), "\n",
          "ArgsList[['IDcols']] <- ", tryCatch({Quantico:::ExpandText(ArgsList[['IDcols']])}, error = function(x) 'NULL'), "\n",
          "if(length(ArgsList[['TargetColumnName']]) == 1L) {\n  ",
          "ModelOutputList <- do.call(what = AutoQuant::AutoCatBoostRegression, args = ArgsList)\n",
          "} else {\n  ",
          "ArgsList[['eval_metric']] <- 'MultiRMSE'\n  ",
          "ArgsList[['loss_function']] <- 'MultiRMSE'\n",
          "}\n",
          "ModelOutputList <- do.call(what = AutoQuant::AutoCatBoostRegression, args = ArgsList)\n"))}, error = function(x) CodeList)

      } else if(TT == 'Binary Classification') {
        if(Debug) print(ArgsList)
        ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]] <- do.call(AutoQuant::AutoCatBoostClassifier, ArgsList)
        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['PrimaryDateColumn']] <- ArgsList[['PrimaryDateColumn']]\n",
          "ArgsList[['EncodeMethod']] <- ArgsList[['EncodeMethod']]\n",
          "ModelOutputList <- do.call(AutoQuant::AutoCatBoostClassifier, ArgsList)\n\n"))}, error = function(x) CodeList)

      } else if(TT == 'MultiClass') {
        ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]] <- do.call(AutoQuant::AutoCatBoostMultiClass, ArgsList)
        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['PrimaryDateColumn']] <- ArgsList[['PrimaryDateColumn']]\n",
          "ArgsList[['EncodeMethod']] <- ArgsList[['EncodeMethod']]\n",
          "ModelOutputList <- do.call(AutoQuant::AutoCatBoostMultiClass, ArgsList)\n\n"))}, error = function(x) CodeList)
      }

      # Store in DataList
      KeyName <- paste0(TT, "_", ArgsList[['ModelID']])
      Output <- Quantico:::Shiny.ML.ModelDataObjects(ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]], Debug, TT = 'catboost')
      DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_ScoringData')]][['data']] <- Output$ScoringDataCombined
      DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_ScoringData')]][['sample']] <- DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_ScoringData')]][['data']][1L:min(.N,1000L)]

      DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_Test_VI_Data')]][['data']] <- Output$VI_Train
      DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_Test_VI_Data')]][['sample']] <- DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_Test_VI_Data')]][['data']]

      DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_Train_VI_Data')]][['data']] <- Output$VI_Validation
      DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_Train_VI_Data')]][['sample']] <- DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_Train_VI_Data')]][['data']]

      DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_Validation_VI_Data')]][['data']] <- Output$VI_Test
      DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_Validation_VI_Data')]][['sample']] <- DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_Validation_VI_Data')]][['data']]

      DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_All_II_Data')]][['data']] <- Output$II_Train
      DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_All_II_Data')]][['sample']] <- DataList[[paste0('CatBoost_', ArgsList[['ModelID']], '_All_II_Data')]][['data']]

      rm(Output); gc()

    }

    if(Algo == 'XGBoost') {

      if(Debug) print(' ::  BuildModels 6  :: ')

      # XGBoost ML Args
      temp <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_TreeMethod']]}, error=function(x) NULL), Type='character', Default = 'hist')
      if(temp == 'GPU') ArgsList[['TreeMethod']] <- 'gpu_hist' else ArgsList[['TreeMethod']] <- 'hist'
      ArgsList[['NThreads']] <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_NThreads']]}, error=function(x) NULL), Type='numeric', Default=-1)
      ArgsList[['EncodingMethod']] <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_EncodeMethod']]}, error=function(x) NULL), Type='character', Default='credibility')
      for(asdfasdfasdf in 1:10) print(ArgsList[['EncodingMethod']])
      ArgsList[['Verbose']] <- 0L
      Output <- Quantico:::Shiny.ML.XGBoost_Params(ArgsList,input,Debug,TT,CodeList)
      for(asdfasdfasdf in 1:10) print(ArgsList[['EncodingMethod']])
      ArgsList <- Output$ArgsList
      CodeList <- Output$CodeList

      # Build model
      if(Debug) print(' ::  BuildModels 8 :: ')
      if(TT == 'Regression') {
        # print(' ::  BuildModels 8.1 :: ')
        # print(CodeList)
        ArgsList[['PrimaryDateColumn']] <- Quantico:::ReturnParam(xx=input[['XGBoost_PrimaryDateColumn']], Type='character', Default=NULL)
        ArgsList[['SaveInfoToPDF']] <- FALSE
        ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]] <- do.call(AutoQuant::AutoXGBoostRegression, ArgsList)
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['PrimaryDateColumn']] <- ", Quantico:::CEP(ArgsList[['PrimaryDateColumn']]), "\n",
          "ArgsList[['SaveInfoToPDF']] <- FALSE\n",
          "ModelOutputList <- do.call(AutoQuant::AutoXGBoostRegression, ArgsList)\n"))


      } else if(TT == 'Binary Classification') {
        if(Debug) print(' ::  BuildModels 8.2 :: ')
        ArgsList[['CostMatrixWeights']] <- c(0,1,1,0)
        ArgsList[['SaveInfoToPDF']] <- FALSE
        ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]] <- do.call(AutoQuant::AutoXGBoostClassifier, ArgsList)
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['SaveInfoToPDF']] <- FALSE\n",
          "ArgsList[['CostMatrixWeights']] <- c(0,1,1,0)\n",
          "ModelOutputList <- do.call(AutoQuant::AutoXGBoostClassifier, ArgsList)\n"))

      } else if(TT == 'MultiClass') {
        if(Debug) print(' ::  BuildModels 8.3 :: ')
        ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]] <- do.call(AutoQuant::AutoXGBoostMultiClass, ArgsList)
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ModelOutputList <- do.call(AutoQuant::AutoXGBoostMultiClass, ArgsList)\n"))
      }

      # Store in DataList
      if(Debug) print(' ::  BuildModels 10 :: ')
      Output <- Quantico:::Shiny.ML.ModelDataObjects(ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]], Debug, TT = 'xgboost')

      if(Debug) print(' ::  BuildModels 10.1 :: ')
      DataList[[paste0('ML_', ArgsList[['ModelID']], '_ScoringData')]][["data"]] <- Output$ScoringDataCombined
      DataList[[paste0('ML_', ArgsList[['ModelID']], '_ScoringData')]][["sample"]] <- DataList[[paste0('ML_', ArgsList[['ModelID']], '_ScoringData')]][["data"]][1L:min(.N,1000L)]

      if(Debug) print(' ::  BuildModels 10.2 :: ')

      if(Debug) print(' ::  BuildModels 10.3 :: ')
      DataList[[paste0('ML_', ArgsList[['ModelID']], '_Test_VI_Data')]][["data"]] <- Output$VI_Train
      DataList[[paste0('ML_', ArgsList[['ModelID']], '_Test_VI_Data')]][["sample"]] <- DataList[[paste0('ML_', ArgsList[['ModelID']], '_Test_VI_Data')]][["data"]]

      if(Debug) print(' ::  BuildModels 10.4 :: ')

      if(Debug) print(' ::  BuildModels 10.5 :: ')
      rm(Output); gc()

    }

    if(Algo == 'LightGBM') {

      if(Debug) print(' ::  BuildModels 6  :: ')

      # LightGBM ML Args
      Output <- Quantico:::Shiny.ML.LightGBM_Params(ArgsList,input,Debug,TT,CodeList)
      ArgsList <- Output$ArgsList
      CodeList <- Output$CodeList

      # Build model
      if(Debug) print(' ::  BuildModels 8 :: ')
      if(TT == 'Regression') {
        if(Debug) print(' ::  BuildModels 8.1 :: ')
        ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]] <- do.call(AutoQuant::AutoLightGBMRegression, ArgsList)
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ModelOutputList <- do.call(AutoQuant::AutoLightGBMRegression, ArgsList)\n"))

      } else if(TT == 'Binary Classification') {
        if(Debug) print(' ::  BuildModels 8.2 :: ')
        ArgsList[['SaveInfoToPDF']] <- FALSE
        cw0 <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_ClassWeights0']]}, error=function(x) NULL), Type='numeric', Default=1)
        cw1 <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_ClassWeights1']]}, error=function(x) NULL), Type='numeric', Default=1)
        ArgsList[['CostMatrixWeights']] <- c(0,1,1,0)
        ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]] <- do.call(AutoQuant::AutoLightGBMClassifier, ArgsList)
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['SaveInfoToPDF']] <- FALSE\n",
          "ArgsList[['CostMatrixWeights']] <- c(0,1,1,0)\n",
          "ModelOutputList <- do.call(AutoQuant::AutoLightGBMMultiClass, ArgsList)\n"))

      } else if(TT == 'MultiClass') {
        if(Debug) print(' ::  BuildModels 8.3 :: ')
        ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]] <- do.call(AutoQuant::AutoLightGBMMultiClass, ArgsList)
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ModelOutputList <- do.call(AutoQuant::AutoLightGBMMultiClass, ArgsList)\n"))
      }

      # Store in DataList
      if(Debug) print(' ::  BuildModels 10 :: ')
      Output <- Quantico:::Shiny.ML.ModelDataObjects(ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]], Debug, TT = 'lightgbm')
      DataList[[paste0('LightGBM_', ArgsList[['ModelID']], '_ScoringData')]][['data']] <- Output$ScoringDataCombined
      DataList[[paste0('LightGBM_', ArgsList[['ModelID']], '_ScoringData')]][['sample']] <- DataList[[paste0('LightGBM_', ArgsList[['ModelID']], '_ScoringData')]][['data']][1L:min(.N,1000L)]

      DataList[[paste0('LightGBM_', ArgsList[['ModelID']], '_Test_VI_Data')]][['data']] <- Output$VI_Train
      DataList[[paste0('LightGBM_', ArgsList[['ModelID']], '_Test_VI_Data')]][['sample']] <- DataList[[paste0('LightGBM_', ArgsList[['ModelID']], '_Test_VI_Data')]][['data']]

      rm(Output); gc()

    }

    if(Algo == 'H2O_DRF') {

      ArgsList[["MaxMem"]] <- paste0(Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_MaxMem']]}, error=function(x) NULL), Type='numeric', Default=4), "G")

      if(Debug) print(' ::  BuildModels 6  :: ')

      # H2O_DRF ML Args
      Output <- Quantico:::Shiny.ML.H2O_DRF_Params(ArgsList,input,Debug,TT,CodeList)
      ArgsList <- Output$ArgsList
      CodeList <- Output$CodeList
      ArgsList[['H2OStartUp']] <- TRUE
      ArgsList[['H2OShutdown']] <- TRUE

      # Build model
      if(Debug) print(' ::  BuildModels 8 :: ')
      if(TT == 'Regression') {
        if(Debug) print(' ::  BuildModels 8.1 :: ')
        if(!ArgsList$eval_metric %in% Shiny.ML.H2O_DRF.EvalMetricOptions(TT = 'Regression')) {
          ArgsList[['eval_metric']] <- 'RMSE'
          if(Debug) print('H2O.DRF eval_metric was switched to RMSE')
        }
        ModelOutputList[[ArgsList[['ModelID']]]] <- do.call(AutoQuant::AutoH2oDRFRegression, ArgsList)
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['eval_metric']] <- ", Quantico:::CEP(ArgsList$eval_metric), "\n",
          "ModelOutputList <- do.call(AutoQuant::AutoH2oDRFRegression, ArgsList)\n"))

      } else if(TT == 'Binary Classification') {
        if(Debug) print(' ::  BuildModels 8.2 :: ')
        ArgsList[['SaveInfoToPDF']] <- FALSE
        cw0 <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_ClassWeights0']]}, error=function(x) NULL), Type='numeric', Default=1)
        cw1 <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_ClassWeights1']]}, error=function(x) NULL), Type='numeric', Default=1)
        ArgsList[['CostMatrixWeights']] <- c(0,1,1,0)
        if(!ArgsList$eval_metric %in% Shiny.ML.H2O_DRF.EvalMetricOptions(TT = 'Binary Classification')) {
          ArgsList[['eval_metric']] <- 'logloss'
          if(Debug) print('H2O.DRF eval_metric was switched to RMSE')
        }
        ModelOutputList[[ArgsList[['ModelID']]]] <- do.call(AutoQuant::AutoH2oDRFClassifier, ArgsList)
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['SaveInfoToPDF']] <- FALSE\n",
          "cw0 <- ", Quantico:::CEPP(cw0), "\n",
          "cw1 <- ", Quantico:::CEPP(cw1), "\n",
          "ArgsList[['CostMatrixWeights']] <- c(0,1,1,0)\n",
          "ArgsList[['eval_metric']] <- ", Quantico:::CEP(ArgsList$eval_metric), "\n",
          "ModelOutputList <- do.call(AutoQuant::AutoH2oDRFClassifier, ArgsList)\n"))

      } else if(TT == 'MultiClass') {
        if(Debug) print(' ::  BuildModels 8.3 :: ')
        if(!ArgsList$eval_metric %in% Shiny.ML.H2O_DRF.EvalMetricOptions(TT = 'MultiClass')) {
          ArgsList[['eval_metric']] <- 'logloss'
          ArgsList[['NumOfParDepPlots']] <- NULL
          if(Debug) print('H2O.DRF eval_metric was switched to RMSE')
        }
        ModelOutputList[[ArgsList[['ModelID']]]] <- do.call(AutoQuant::AutoH2oDRFMultiClass, ArgsList)
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['eval_metric']] <- ", Quantico:::CEP(ArgsList$eval_metric), "\n",
          "ModelOutputList <- do.call(AutoQuant::AutoH2oDRFMultiClass, ArgsList)\n"))
      }

      # Remove Model: it gets saved during the run
      ModelOutputList[[ArgsList[['ModelID']]]]$Model <- NULL

      # Store in DataList
      if(Debug) print(' ::  BuildModels 10 :: ')
      Output <- Quantico:::Shiny.ML.ModelDataObjects(ModelOutputList[[ArgsList[['ModelID']]]], Debug, TT = 'h2o_drf')
      if(Debug) print(' ::  BuildModels 11 :: ')
      DataList[[paste0('H2O_DRF_', ArgsList[['ModelID']], '_ScoringData')]][['data']] <- Output$ScoringDataCombined
      DataList[[paste0('H2O_DRF_', ArgsList[['ModelID']], '_ScoringData')]][['sample']] <- DataList[[paste0('H2O_DRF_', ArgsList[['ModelID']], '_ScoringData')]][['data']][1L:min(.N, 1000L)]

      if(Debug) print(' ::  BuildModels 12 :: ')
      DataList[[paste0('H2O_DRF_', ArgsList[['ModelID']], '_Test_VI_Data')]][['data']] <- Output$VI_Train
      DataList[[paste0('H2O_DRF_', ArgsList[['ModelID']], '_Test_VI_Data')]][['sample']] <- DataList[[paste0('H2O_DRF_', ArgsList[['ModelID']], '_Test_VI_Data')]][['data']]

      if(Debug) print(' ::  BuildModels 13 :: ')
      rm(Output); gc()

    }

    if(Algo == 'H2O_GBM') {

      if(Debug) print(' ::  BuildModels 6  :: ')
      ArgsList[["MaxMem"]] <- paste0(Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_MaxMem']]}, error=function(x) NULL), Type='numeric', Default=4), "G")

      # H2O_GBM ML Args
      Output <- Quantico:::Shiny.ML.H2O_GBM_Params(ArgsList,input,Debug,TT,CodeList)
      ArgsList <- Output$ArgsList
      CodeList <- Output$CodeList

      ArgsList[['H2OStartUp']] <- TRUE
      ArgsList[['H2OShutdown']] <- TRUE

      # Build model
      if(Debug) print(' ::  BuildModels 8 :: ')
      if(TT == 'Regression') {
        if(Debug) print(' ::  BuildModels 8.1 :: ')
        if(!ArgsList$eval_metric %in% Shiny.ML.H2O_GBM.EvalMetricOptions(TT = 'Regression')) {
          ArgsList[['eval_metric']] <- 'RMSE'
          if(Debug) print('H2O_GBM eval_metric was switched to RMSE')
        }

        # Build
        if(Debug) print(ArgsList)
        ModelOutputList[[ArgsList[['ModelID']]]] <- do.call(AutoQuant::AutoH2oGBMRegression, ArgsList)
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['eval_metric']] <- ", Quantico:::CEP(ArgsList$eval_metric), "\n",
          "ModelOutputList <- do.call(AutoQuant::AutoH2oGBMRegression, ArgsList)\n"))

      } else if(TT == 'Binary Classification') {
        if(Debug) print(' ::  BuildModels 8.2 :: ')
        ArgsList[['SaveInfoToPDF']] <- FALSE
        cw0 <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_ClassWeights0']]}, error=function(x) NULL), Type='numeric', Default=1)
        cw1 <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_ClassWeights1']]}, error=function(x) NULL), Type='numeric', Default=1)
        ArgsList[['CostMatrixWeights']] <- c(0,1,1,0)
        if(!ArgsList$eval_metric %in% Shiny.ML.H2O_GBM.EvalMetricOptions(TT = 'Binary Classification')) {
          ArgsList[['eval_metric']] <- 'logloss'
          if(Debug) print('H2O_GBM eval_metric was switched to RMSE')
        }

        # Build
        ModelOutputList[[ArgsList[['ModelID']]]] <- do.call(AutoQuant::AutoH2oGBMClassifier, ArgsList)
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['SaveInfoToPDF']] <- FALSE\n",
          "cw0 <- ", Quantico:::CEP(cw0), "\n",
          "cw1 <- ", Quantico:::CEP(cw1), "\n",
          "ArgsList[['CostMatrixWeights']] <- c(0,1,1,0)\n",
          "ArgsList[['eval_metric']] <- ", Quantico:::CEP(ArgsList$eval_metric), "\n",
          "ModelOutputList <- do.call(AutoQuant::AutoH2oGBMClassifier, ArgsList)\n"))

      } else if(TT == 'MultiClass') {

        if(Debug) print(' ::  BuildModels 8.3 :: ')
        ArgsList[['eval_metric']] <- 'auc'
        ArgsList[['Distribution']] <- 'multinomial'
        ArgsList[['NumOfParDepPlots']] <- NULL
        if(Debug) print('H2O_GBM eval_metric was switched to RMSE')

        # Build
        ModelOutputList[[ArgsList[['ModelID']]]] <- do.call(AutoQuant::AutoH2oGBMMultiClass, ArgsList)
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['eval_metric']] <- ", Quantico:::CEP(ArgsList$eval_metric), "\n",
          "ModelOutputList <- do.call(AutoQuant::AutoH2oGBMClassifier, ArgsList)\n"))
      }

      # Remove Model: it gets saved during the run
      ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]]$Model <- NULL

      # Store in DataList
      if(Debug) print(' ::  BuildModels 10 :: ')
      Output <- Quantico:::Shiny.ML.ModelDataObjects(ModelOutputList[[ArgsList[['ModelID']]]], Debug, TT = 'h2o_gbm')
      if(Debug) print(' ::  BuildModels 11 :: ')
      DataList[[paste0('H2O_GBM_', ArgsList[['ModelID']], '_ScoringData')]][['data']] <- Output$ScoringDataCombined
      DataList[[paste0('H2O_GBM_', ArgsList[['ModelID']], '_ScoringData')]][['sample']] <- DataList[[paste0('H2O_GBM_', ArgsList[['ModelID']], '_ScoringData')]][['data']][1L:min(.N,1000L)]

      if(Debug) print(' ::  BuildModels 12 :: ')
      DataList[[paste0('H2O_GBM_', ArgsList[['ModelID']], '_Test_VI_Data')]][['data']] <- Output$VI_Train
      DataList[[paste0('H2O_GBM_', ArgsList[['ModelID']], '_Test_VI_Data')]][['sample']] <- DataList[[paste0('H2O_GBM_', ArgsList[['ModelID']], '_Test_VI_Data')]][['data']]

      if(Debug) print(' ::  BuildModels 13 :: ')
      rm(Output); gc()
    }

    if(Algo == 'H2O_GLM') {

      ArgsList[["MaxMem"]] <- paste0(Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_MaxMem']]}, error=function(x) NULL), Type='numeric', Default=4), "G")

      if(Debug) print(' ::  BuildModels 6  :: ')

      # H2O_GLM ML Args
      Output <- Quantico:::Shiny.ML.H2O_GLM_Params(ArgsList,input,Debug,TT,CodeList)
      ArgsList <- Output$ArgsList
      CodeList <- Output$CodeList

      ArgsList[['H2OStartUp']] <- TRUE
      ArgsList[['H2OShutdown']] <- TRUE

      # Build model
      if(Debug) print(' ::  BuildModels 8 :: ')
      if(TT == 'Regression') {
        if(Debug) print(' ::  BuildModels 8.1 :: ')

        # Ensure correct arg for eval_metric
        if(!ArgsList$eval_metric %in% Shiny.ML.H2O_GLM.EvalMetricOptions(TT = 'Regression')) {
          ArgsList[['eval_metric']] <- 'RMSE'
          if(Debug) print('H2O_GLM eval_metric was switched to RMSE')
        }

        # Build
        if(Debug) print(CodeList)
        ModelOutputList[[ArgsList[['ModelID']]]] <- do.call(AutoQuant::AutoH2oGLMRegression, ArgsList)
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['eval_metric']] <- ", Quantico:::CEP(ArgsList$eval_metric), "\n",
          "ModelOutputList <- do.call(AutoQuant::AutoH2oGLMRegression, ArgsList)\n"))

      } else if(TT == 'Binary Classification') {
        if(Debug) print(' ::  BuildModels 8.2 :: ')
        ArgsList[['SaveInfoToPDF']] <- FALSE
        cw0 <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_ClassWeights0']]}, error=function(x) NULL), Type='numeric', Default=1)
        cw1 <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_ClassWeights1']]}, error=function(x) NULL), Type='numeric', Default=1)
        ArgsList[['CostMatrixWeights']] <- c(0,1,1,0)
        ArgsList[['eval_metric']] <- 'auc'

        # Build
        ModelOutputList[[ArgsList[['ModelID']]]] <- do.call(AutoQuant::AutoH2oGLMClassifier, ArgsList)
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['SaveInfoToPDF']] <- FALSE\n",
          "cw0 <- ", Quantico:::CEP(cw0), "\n",
          "cw1 <- ", Quantico:::CEP(cw1), "\n",
          "ArgsList[['CostMatrixWeights']] <- c(0,1,1,0)\n",
          "ArgsList[['eval_metric']] <- ", Quantico:::CEP(ArgsList$eval_metric), "\n",
          "ModelOutputList <- do.call(AutoQuant::AutoH2oGLMClassifier, ArgsList)\n"))

      } else if(TT == 'MultiClass') {
        if(Debug) print(' ::  BuildModels 8.3 :: ')
        if(!ArgsList$eval_metric %in% Shiny.ML.H2O_GLM.EvalMetricOptions(TT = 'MultiClass')) {
          ArgsList[['eval_metric']] <- 'logloss'
          ArgsList[['Distribution']] <- "multinomial"
          ArgsList[['Link']] <- "family_default"
          ArgsList[['NumOfParDepPlots']] <- NULL
          if(Debug) print('H2O_GLM eval_metric was switched to RMSE')
        }

        # Build
        ModelOutputList[[ArgsList[['ModelID']]]] <- do.call(AutoQuant::AutoH2oGLMMultiClass, ArgsList)
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['eval_metric']] <- ", Quantico:::CEP(ArgsList$eval_metric), "\n",
          "ModelOutputList <- do.call(AutoQuant::AutoH2oGLMClassifier, ArgsList)\n"))
      }

      # Remove Model: it gets saved during the run
      ModelOutputList[[ArgsList[['ModelID']]]]$Model <- NULL

      # Store in DataList
      if(Debug) print(' ::  BuildModels 10 :: ')

      Output <- Quantico:::Shiny.ML.ModelDataObjects(ModelOutputList[[ArgsList[['ModelID']]]], Debug, TT = 'H2O_GLM')

      if(Debug) print(' ::  BuildModels 11 :: ')

      DataList[[paste0('H2O_GLM_', ArgsList[['ModelID']], '_ScoringData')]][['data']] <- Output$ScoringDataCombined
      DataList[[paste0('H2O_GLM_', ArgsList[['ModelID']], '_ScoringData')]][['sample']] <- DataList[[paste0('H2O_GLM_', ArgsList[['ModelID']], '_ScoringData')]][['data']][1L:min(.N,1000L)]

      if(Debug) print(' ::  BuildModels 12 :: ')

      DataList[[paste0('H2O_GLM_', ArgsList[['ModelID']], '_Test_VI_Data')]][['data']] <- Output$VI_Train
      DataList[[paste0('H2O_GLM_', ArgsList[['ModelID']], '_Test_VI_Data')]][['sample']] <- DataList[[paste0('H2O_GLM_', ArgsList[['ModelID']], '_Test_VI_Data')]][['data']]

      if(Debug) print(' ::  BuildModels 13 :: ')

      DataList[[paste0('H2O_GLM_', ArgsList[['ModelID']], '_Coeff_Table')]][['data']] <- Output$Coeff_Table
      DataList[[paste0('H2O_GLM_', ArgsList[['ModelID']], '_Coeff_Table')]][['sample']] <- DataList[[paste0('H2O_GLM_', ArgsList[['ModelID']], '_Coeff_Table')]][['data']]

      rm(Output); gc()
    }

    if(Algo == 'H2O_HGLM') {

      ArgsList[["MaxMem"]] <- paste0(Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_MaxMem']]}, error=function(x) NULL), Type='numeric', Default=4), "G")

      if(Debug) print(' ::  BuildModels 6  :: ')

      # H2O_HGLM ML Args
      Output <- Quantico:::Shiny.ML.H2O_HGLM_Params(ArgsList,input,Debug,TT,CodeList)
      ArgsList <- Output$ArgsList
      CodeList <- Output$CodeList

      # print(which(names(ArgsList[["data"]]) %in% names(ArgsList[["data"]])[ArgsList[["RandomColNumbers"]]]))
      ArgsList[["RandomColNumbers"]] <- which(names(ArgsList[["data"]]) %in% ArgsList[["RandomColNumbers"]])

      ArgsList[['H2OStartUp']] <- TRUE
      ArgsList[['H2OShutdown']] <- TRUE

      # Build model
      if(Debug) print(' ::  BuildModels 8 :: ')
      if(TT == 'Regression') {
        if(Debug) print(' ::  BuildModels 8.1 :: ')

        # Ensure correct arg for eval_metric
        if(!ArgsList$eval_metric %in% Shiny.ML.H2O_GLM.EvalMetricOptions(TT = 'Regression')) {
          ArgsList[['eval_metric']] <- 'RMSE'
          if(Debug) print('H2O_HGLM eval_metric was switched to RMSE')
        }

        # Build
        ModelOutputList[[paste0('SM_', ArgsList[['ModelID']])]] <- do.call(AutoQuant::AutoH2oGLMRegression, ArgsList)
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['eval_metric']] <- ", Quantico:::CEP(ArgsList$eval_metric), "\n",
          "ModelOutputList <- do.call(AutoQuant::AutoH2oGLMRegression, ArgsList)\n"))

      } else if(TT == 'Binary Classification') {
        if(Debug) print(' ::  BuildModels 8.2 :: ')
        ArgsList[['SaveInfoToPDF']] <- FALSE
        cw0 <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_ClassWeights0']]}, error=function(x) NULL), Type='numeric', Default=1)
        cw1 <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_ClassWeights1']]}, error=function(x) NULL), Type='numeric', Default=1)
        ArgsList[['CostMatrixWeights']] <- c(0,1,1,0)
        ArgsList[['eval_metric']] <- 'auc'

        # Build
        ModelOutputList[[paste0('SM_', ArgsList[['ModelID']])]] <- do.call(AutoQuant::AutoH2oGLMClassifier, ArgsList)
        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['SaveInfoToPDF']] <- FALSE\n",
          "cw0 <- ", Quantico:::CEP(cw0), "\n",
          "cw1 <- ", Quantico:::CEP(cw1), "\n",
          "ArgsList[['CostMatrixWeights']] <- c(0,1,1,0)\n",
          "ArgsList[['eval_metric']] <- ", Quantico:::CEP(ArgsList$eval_metric), "\n",
          "ModelOutputList <- do.call(AutoQuant::AutoH2oGLMClassifier, ArgsList)\n"))}, error = function(x) CodeList)

      } else if(TT == 'MultiClass') {
        if(Debug) print(' ::  BuildModels 8.3 :: ')
        if(!ArgsList$eval_metric %in% Shiny.ML.H2O_GLM.EvalMetricOptions(TT = 'MultiClass')) {
          ArgsList[['eval_metric']] <- 'logloss'
          ArgsList[['NumOfParDepPlots']] <- NULL
          if(Debug) print('H2O_HGLM eval_metric was switched to RMSE')
        }

        # Build
        ModelOutputList[[paste0('SM_', ArgsList[['ModelID']])]] <- do.call(AutoQuant::AutoH2oGLMMultiClass, ArgsList)
        CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ML Build Model\n",
          "ArgsList[['eval_metric']] <- ", Quantico:::CEP(ArgsList$eval_metric), "\n",
          "ModelOutputList <- do.call(AutoQuant::AutoH2oGLMClassifier, ArgsList)\n"))
      }

      # Store in DataList
      if(Debug) print(' ::  BuildModels 10 :: ')

      Output <- Quantico:::Shiny.ML.ModelDataObjects(ModelOutputList[[paste0('SM_', ArgsList[['ModelID']])]], Debug, TT = 'h2o_hglm')

      if(Debug) print(' ::  BuildModels 11 :: ')

      DataList[[paste0('H2O_HGLM_', ArgsList[['ModelID']], '_ScoringData')]][["data"]] <- Output$ScoringDataCombined
      DataList[[paste0('H2O_HGLM_', ArgsList[['ModelID']], '_ScoringData')]][["sample"]] <- DataList[[paste0('H2O_HGLM_', ArgsList[['ModelID']], '_ScoringData')]][["data"]][1L:min(.N,1000L)]

      if(Debug) print(' ::  BuildModels 12 :: ')

      DataList[[paste0('H2O_HGLM_', ArgsList[['ModelID']], '_Test_VI_Data')]][["data"]] <- Output$VI_Train
      DataList[[paste0('H2O_HGLM_', ArgsList[['ModelID']], '_Test_VI_Data')]][["sample"]] <- DataList[[paste0('H2O_HGLM_', ArgsList[['ModelID']], '_Test_VI_Data')]][["data"]]

      if(Debug) print(' ::  BuildModels 13 :: ')

      DataList[[paste0('H2O_HGLM_', ArgsList[['ModelID']], '_Coeff_Table')]][["data"]] <- Output$Coeff_Table
      DataList[[paste0('H2O_HGLM_', ArgsList[['ModelID']], '_Coeff_Table')]][["sample"]] <- DataList[[paste0('H2O_HGLM_', ArgsList[['ModelID']], '_Coeff_Table')]][["data"]]

      if(Debug) print(' ::  BuildModels 14 :: ')

      DataList[[paste0('H2O_HGLM_', ArgsList[['ModelID']], 'Random_Coeff_Table')]][["data"]] <- Output$Random_Coeff_Table
      DataList[[paste0('H2O_HGLM_', ArgsList[['ModelID']], 'Random_Coeff_Table')]][["sample"]] <- DataList[[paste0('H2O_HGLM_', ArgsList[['ModelID']], 'Random_Coeff_Table')]][["data"]]

      rm(Output); gc()
    }

  }

  # Update Table
  if(Debug) {
    if(Debug) print(ML_ExperimentTable)
    if(Debug) print(iter)
    if(Debug) print(TT)
    if(Debug) print(Algo)
  }
  if(TT == "Regression") {
    if(Algo %in% c("H2O_DRF","H2O_GBM","H2O_GLM")) {
      DataList[["ML_RegressionMetrics"]][["data"]] <- Quantico:::Shiny.ML.Evaluation.Table(ArgsList,ML_ExperimentTable,iter,ModelOutputList[[ArgsList[['ModelID']]]],TT,Algo,Debug)
      DataList[["ML_RegressionMetrics"]][["sample"]] <- Quantico:::Shiny.ML.Evaluation.Table(ArgsList,ML_ExperimentTable,iter,ModelOutputList[[ArgsList[['ModelID']]]],TT,Algo,Debug)
      ModelOutputList[[ArgsList[['ModelID']]]][["ML_RegressionMetrics"]] <- DataList[["ML_RegressionMetrics"]][["data"]]
    } else {
      DataList[["ML_RegressionMetrics"]][["data"]] <- Quantico:::Shiny.ML.Evaluation.Table(ArgsList,ML_ExperimentTable,iter,ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]],TT,Algo,Debug)
      DataList[["ML_RegressionMetrics"]][["sample"]] <- Quantico:::Shiny.ML.Evaluation.Table(ArgsList,ML_ExperimentTable,iter,ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]],TT,Algo,Debug)
      ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]][["ML_RegressionMetrics"]] <- DataList[["ML_RegressionMetrics"]][["data"]]
    }
  } else if(TT == "Binary Classification") {
    if(Algo %in% c("H2O_DRF","H2O_GBM","H2O_GLM")) {
      DataList[["ML_ClassificationMetrics"]][["data"]] <- Quantico:::Shiny.ML.Evaluation.Table(ArgsList,ML_ExperimentTable,iter,ModelOutputList[[ArgsList[['ModelID']]]],TT,Algo,Debug)
      DataList[["ML_ClassificationMetrics"]][["sample"]] <- Quantico:::Shiny.ML.Evaluation.Table(ArgsList,ML_ExperimentTable,iter,ModelOutputList[[ArgsList[['ModelID']]]],TT,Algo,Debug)
      ModelOutputList[[ArgsList[['ModelID']]]][["ML_ClassificationMetrics"]] <- DataList[["ML_ClassificationMetrics"]][["data"]]
    } else {
      DataList[["ML_ClassificationMetrics"]][["data"]] <- Quantico:::Shiny.ML.Evaluation.Table(ArgsList,ML_ExperimentTable,iter,ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]],TT,Algo,Debug)
      DataList[["ML_ClassificationMetrics"]][["sample"]] <- Quantico:::Shiny.ML.Evaluation.Table(ArgsList,ML_ExperimentTable,iter,ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]],TT,Algo,Debug)
      ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]][["ML_ClassificationMetrics"]] <- DataList[["ML_ClassificationMetrics"]][["data"]]
    }
  } else if(TT == "MultiClass") {
    if(Algo %in% c("H2O_DRF","H2O_GBM","H2O_GLM")) {
      DataList[["ML_MultiClassMetrics"]][["data"]] <- Quantico:::Shiny.ML.Evaluation.Table(ArgsList,ML_ExperimentTable,iter,ModelOutputList[[ArgsList[['ModelID']]]],TT,Algo,Debug)
      DataList[["ML_MultiClassMetrics"]][["sample"]] <- Quantico:::Shiny.ML.Evaluation.Table(ArgsList,ML_ExperimentTable,iter,ModelOutputList[[ArgsList[['ModelID']]]],TT,Algo,Debug)
      ModelOutputList[[ArgsList[['ModelID']]]][["ML_MultiClassMetrics"]] <- DataList[["ML_MultiClassMetrics"]][["data"]]
    } else {
      DataList[["ML_MultiClassMetrics"]][["data"]] <- Quantico:::Shiny.ML.Evaluation.Table(ArgsList,ML_ExperimentTable,iter,ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]],TT,Algo,Debug)
      DataList[["ML_MultiClassMetrics"]][["sample"]] <- Quantico:::Shiny.ML.Evaluation.Table(ArgsList,ML_ExperimentTable,iter,ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]],TT,Algo,Debug)
      ModelOutputList[[paste0('ML_', ArgsList[['ModelID']])]][["ML_MultiClassMetrics"]] <- DataList[["ML_MultiClassMetrics"]][["data"]]
    }
  }

  # Return output
  for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
  for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
  return(list(
    DataList = DataList,
    ArgsList = ArgsList,
    CodeList = CodeList,
    ModelOutputList = ModelOutputList
  ))
}

#' @title Shiny.ML.Evaluation.Table
#'
#' @description Output table for evaluation metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param ArgsList from app
#' @param ML_ExperimentTable data.table
#' @param iter Row number to have updated
#' @param ModelOutputList List with model results like AutoQuant
#' @param TT Target Type. "Regression", "Binary Classification", "MultiClass"
#' @param Algo "CatBoost", "XGBoost", "LightGBM", "H2O_DRF","H2O_GBM","H2O_RuleFit","H2O_GLM","H2O_CoxPH"
#' @param Debug From App
#'
#' @export
Shiny.ML.Evaluation.Table <- function(ArgsList,
                                      ML_ExperimentTable,
                                      iter,
                                      ModelOutputList,
                                      TT,
                                      Algo,
                                      Debug) {

  # Update ML_ExperimentTable
  if(Debug) print(' ::  BuildModels 15 :: ')
  data.table::set(ML_ExperimentTable, i = iter, j = 'Date', value = Sys.time())
  data.table::set(ML_ExperimentTable, i = iter, j = 'Model ID', value = if(length(ModelOutputList$ArgsList$ModelID) > 0L) ModelOutputList$ArgsList$ModelID else NA_character_)
  data.table::set(ML_ExperimentTable, i = iter, j = 'Algo', value = Algo)
  data.table::set(ML_ExperimentTable, i = iter, j = 'Grid Tune', value = ArgsList[['GridTune']])

  if(Debug) print(' ::  BuildModels 21 :: ')
  print(ModelOutputList$EvaluationMetrics)
  if(TT == "Regression") {
    if(length(ModelOutputList$EvaluationMetrics$TestData[Metric == 'R2', MetricValue]) > 0L) data.table::set(ML_ExperimentTable, i = iter, j = 'Test r-sq', value =  tryCatch({if(TT == 'Regression') round(ModelOutputList$EvaluationMetrics$TestData[Metric == 'R2', MetricValue], 4L) else NA_real_}, error = function(x) NA_real_))
    if(length(ModelOutputList$EvaluationMetrics$TrainData[Metric == 'R2', MetricValue]) > 0L) data.table::set(ML_ExperimentTable, i = iter, j = 'Train r-sq', value = tryCatch({if(TT == 'Regression') round(ModelOutputList$EvaluationMetrics$TrainData[Metric == 'R2', MetricValue], 4L) else NA_real_}, error = function(x) NA_real_))
    if(length(ModelOutputList$EvaluationMetrics$TestData[Metric == 'RMSE', MetricValue]) > 0L) data.table::set(ML_ExperimentTable, i = iter, j = 'Test RMSE',  value = tryCatch({if(TT == 'Regression') round(ModelOutputList$EvaluationMetrics$TestData[Metric == 'RMSE', MetricValue], 4L) else NA_real_}, error = function(x) NA_real_))
    if(length(ModelOutputList$EvaluationMetrics$TrainData[Metric == 'RMSE', MetricValue]) > 0L) data.table::set(ML_ExperimentTable, i = iter, j = 'Train RMSE', value = tryCatch({if(TT == 'Regression') round(ModelOutputList$EvaluationMetrics$TrainData[Metric == 'RMSE', MetricValue], 4L) else NA_real_}, error = function(x) NA_real_))
    if(length(ModelOutputList$EvaluationMetrics$TestData[Metric == 'MAE', MetricValue]) > 0L) data.table::set(ML_ExperimentTable, i = iter, j = 'Test MAE',   value = tryCatch({if(TT == 'Regression') round(ModelOutputList$EvaluationMetrics$TestData[Metric == 'MAE', MetricValue], 4L) else NA_real_}, error = function(x) NA_real_))
    if(length(ModelOutputList$EvaluationMetrics$TrainData[Metric == 'MAE', MetricValue]) > 0L) data.table::set(ML_ExperimentTable, i = iter, j = 'Train MAE',  value = tryCatch({if(TT == 'Regression') round(ModelOutputList$EvaluationMetrics$TrainData[Metric == 'MAE', MetricValue], 4L) else NA_real_}, error = function(x) NA_real_))
    if(length(ModelOutputList$EvaluationMetrics$TestData[Metric == 'MAPE', MetricValue]) > 0L) data.table::set(ML_ExperimentTable, i = iter, j = 'Test MAPE',  value = tryCatch({if(TT == 'Regression') round(ModelOutputList$EvaluationMetrics$TestData[Metric == 'MAPE', MetricValue], 4L) else NA_real_}, error = function(x) NA_real_))
    if(length(ModelOutputList$EvaluationMetrics$TrainData[Metric == 'MAPE', MetricValue]) > 0L) data.table::set(ML_ExperimentTable, i = iter, j = 'Train MAPE', value = tryCatch({if(TT == 'Regression') round(ModelOutputList$EvaluationMetrics$TrainData[Metric == 'MAPE', MetricValue], 4L) else NA_real_}, error = function(x) NA_real_))
  } else if(TT == "Binary Classification") {
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test Accuracy',         value = tryCatch({if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TestData[order(-Accuracy)][1L, Accuracy], 4L) else NA_real_}, error = function(x) NA_real_))
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test Accuracy Thresh',  value = tryCatch({if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TestData[order(-Accuracy)][1L, Threshold], 4L) else NA_real_}, error = function(x) NA_real_))
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train Accuracy',        value = tryCatch({if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TrainData[order(-Accuracy)][1L, Accuracy], 4L) else NA_real_}, error = function(x) NA_real_))
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train Accuracy Thresh', value = tryCatch({if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TrainData[order(-Accuracy)][1L, Threshold], 4L) else NA_real_}, error = function(x) NA_real_))
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test MCC',              value = tryCatch({if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TestData[order(-MCC)][1L, MCC], 4L) else NA_real_}, error = function(x) NA_real_))
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test Accuracy Thresh',  value = tryCatch({if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TestData[order(-MCC)][1L, Threshold], 4L) else NA_real_}, error = function(x) NA_real_))
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train MCC',             value = tryCatch({if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TrainData[order(-MCC)][1L, MCC], 4L) else NA_real_}, error = function(x) NA_real_))
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train Accuracy Thresh', value = tryCatch({if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TrainData[order(-MCC)][1L, Threshold], 4L) else NA_real_}, error = function(x) NA_real_))
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test Utility',          value = tryCatch({if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TestData[order(-Utility)][1L, Utility], 4L) else NA_real_}, error = function(x) NA_real_))
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test Utility Thresh',   value = tryCatch({if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TestData[order(-Utility)][1L, Threshold], 4L) else NA_real_}, error = function(x) NA_real_))
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train Utility',         value = tryCatch({if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TrainData[order(-Utility)][1L, Utility], 4L) else NA_real_}, error = function(x) NA_real_))
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train Utility Thresh',  value = tryCatch({if(TT == 'Binary Classification') round(ModelOutputList$EvaluationMetrics$TrainData[order(-Utility)][1L, Threshold], 4L) else NA_real_}, error = function(x) NA_real_))
  } else if(TT == "MultiClass") {
    if(!Algo %in% c('H2O_DRF','H2O_GBM','H2O_RuleFit','H2O_GLM','H2O_CoxPH')) data.table::set(ML_ExperimentTable, i = iter, j = 'Test MCC',       value = if(TT == 'MultiClass') round(ModelOutputList$MultinomialMetrics$TestData[Metric == 'MCC'][1L, MetricValue], 4L) else NA_real_)
    if(!Algo %in% c('H2O_DRF','H2O_GBM','H2O_RuleFit','H2O_GLM','H2O_CoxPH')) data.table::set(ML_ExperimentTable, i = iter, j = 'Train MCC',      value = if(TT == 'MultiClass') round(ModelOutputList$MultinomialMetrics$TrainData[Metric == 'MCC'][1L, MetricValue], 4L) else NA_real_)
    if(Debug) print(ModelOutputList$MultinomialMetrics)
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test Accuracy',  value = tryCatch({if(TT == 'MultiClass') round(ModelOutputList$MultinomialMetrics$TestData[Metric == 'Accuracy'][1L, MetricValue], 4L) else NA_real_}, error = function(x) NA_real_))
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train Accuracy', value = tryCatch({if(TT == 'MultiClass') round(ModelOutputList$MultinomialMetrics$TrainData[Metric == 'Accuracy'][1L, MetricValue], 4L) else NA_real_}, error = function(x) NA_real_))
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test MicroAUC',  value = tryCatch({if(TT == 'MultiClass') round(ModelOutputList$MultinomialMetrics$TestData[Metric == 'MicroAUC'][1L, MetricValue], 4L) else NA_real_}, error = function(x) NA_real_))
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train MicroAUC', value = tryCatch({if(TT == 'MultiClass') round(ModelOutputList$MultinomialMetrics$TrainData[Metric == 'MicroAUC'][1L, MetricValue], 4L) else NA_real_}, error = function(x) NA_real_))
    data.table::set(ML_ExperimentTable, i = iter, j = 'Test LogLoss',   value = tryCatch({if(TT == 'MultiClass') round(ModelOutputList$MultinomialMetrics$TestData[Metric == 'LogLoss'][1L, MetricValue], 4L) else NA_real_}, error = function(x) NA_real_))
    data.table::set(ML_ExperimentTable, i = iter, j = 'Train LogLoss',  value = tryCatch({if(TT == 'MultiClass') round(ModelOutputList$MultinomialMetrics$TrainData[Metric == 'LogLoss'][1L, MetricValue], 4L) else NA_real_}, error = function(x) NA_real_))
  }

  if(Debug) print(' ::  BuildModels 42 :: ')
  return(ML_ExperimentTable)
}

#' @title Shiny.ML.ModelDataObjects
#'
#' @description Eval Metrics
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param ModelOutput Output from AutoQuant:: supervised learning functions
#' @param TT asdf
#'
#' @export
Shiny.ML.ModelDataObjects <- function(ModelOutput, Debug, TT = 'catboost') {
  print("Shiny.ML.ModelDataObjects 1")

  # Extract scored data
  if(!is.null(ModelOutput$TrainData) && !is.null(ModelOutput$TestData)) {
    if(Debug) print("Shiny.ML.ModelDataObjects 2")
    temp1 <- data.table::rbindlist(list(
      'TRAIN' = ModelOutput$TrainData,
      'TEST' = ModelOutput$TestData
    ), use.names = TRUE, fill = TRUE, idcol = 'DataSet')
  } else if(is.null(ModelOutput$TrainData) && !is.null(ModelOutput$TestData)) {
    if(Debug) print("Shiny.ML.ModelDataObjects 3")
    temp1 <- ModelOutput$TestData
  } else if(!is.null(ModelOutput$TrainData) && is.null(ModelOutput$TestData)) {
    if(Debug) print("Shiny.ML.ModelDataObjects 4")
    temp1 <- ModelOutput$TrainData
  } else {
    if(Debug) print("Shiny.ML.ModelDataObjects 5")
    temp1 <- NULL
  }

  # Extract Importances and coeff tables
  if(tolower(TT) == 'catboost') {
    if(Debug) print("Shiny.ML.ModelDataObjects 6")
    oList <- list()
    oList[["ScoringDataCombined"]] <- temp1
    oList[["VI_Train"]] <- ModelOutput$VariableImportance$Train_Importance
    oList[["VI_Validation"]] <- ModelOutput$VariableImportance$Validation_Importance
    oList[["VI_Test"]] <- ModelOutput$VariableImportance$Test_Importance
    oList[["II_Train"]] <- ModelOutput$InteractionImportance$Train_Interaction
    if(Debug) print(names(oList))
    return(oList)

  } else if(tolower(TT) %in% c('xgboost','lightgbm','h2o_drf','h2o_gbm','h2o_rulefit','h2o_glm','h2o_hglm','h2o_coxph')) {
    if(Debug) print("Shiny.ML.ModelDataObjects 7")
    if(tolower(TT) %in% 'h2o_hglm') print(data.table::as.data.table(ModelOutput$Model@model$random_coefficients_table))
    oList <- list()
    oList[["ScoringDataCombined"]] <- temp1
    oList[["VI_Train"]] <- ModelOutput$VariableImportance; print(ModelOutput$VariableImportance)
    oList[["Coeff_Table"]] <- if(tolower(TT) %in% c('h2o_glm','h2o_hglm')) data.table::as.data.table(ModelOutput$Model@model$coefficients_table) else NULL
    oList[["Random_Coeff_Table"]] <- if(tolower(TT) %in% 'h2o_hglm') data.table::as.data.table(ModelOutput$Model@model$random_coefficients_table) else NULL
    if(Debug) print(names(oList))
    return(oList)
  }
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# ML Reporting                                                                               ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Shiny.ML.ReportOutput
#'
#' @description Shiny ML Report
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param input shiny input
#' @param output shiny output
#' @param DataList DataList stores data in app
#' @param ArgsList ArgsList
#' @param CodeList CodeList from app
#' @param Debug DebugFC from app
#'
#' @return a list of columns names by data type
#'
#' @export
Shiny.ML.ReportOutput <- function(input,
                                  output,
                                  DataList,
                                  CodeList,
                                  Page,
                                  Debug = FALSE,
                                  MOL = NULL,
                                  ModelID = NULL,
                                  Theme = "dark",
                                  FontColor = NULL) {

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Create collection lists                                                   ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(Debug) print("ML Reports 1")

  # temp_model_rdata$ArgsList$TargetColumnName
  TargetColumnName <- MOL[[ModelID]][["ArgsList"]][["TargetColumnName"]]

  # temp_model_rdata$ArgsList$
  PredictionColumnName <- MOL[[ModelID]][["ArgsList"]][["PredictionColumnName"]]

  # temp_model_rdata$ArgsList$FeatureColNames
  FeatureColumnNames <- MOL[[ModelID]][["ArgsList"]][["FeatureColNames"]]

  # NEED
  TargetType <- MOL[[ModelID]][["ArgsList"]][["TargetType"]]

  # Group Var
  GroupVariableInclude <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0('GroupVariableInclude',Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
  if(Debug) print(GroupVariableInclude)

  # NEED
  Algo <- MOL[[ModelID]][["ArgsList"]][["Algo"]]

  # Data sets
  SampleSize <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0('SampleSize',Page)]]}, error = function(x) NULL), Type = "numeric", Default = NULL)
  TrainDataInclude <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0('TrainDataInclude',Page)]]}, error = function(x) NULL), Type = "logical", Default = NULL)
  if(Debug) print("ML Reports 2")
  if(TrainDataInclude) {
    TrainData <- MOL[[ModelID]][["TrainData"]]
    TrainData <- TrainData[order(runif(.N))][seq_len(min(.N, eval(SampleSize)))]
  } else {
    TrainData <- NULL
  }

  if(Debug) print("ML Reports 2.1")
  TestData <- MOL[[ModelID]][["TestData"]]
  # print(TestData)
  N <- TestData[,.N]
  # print(N)
  NN <- min(N, SampleSize)
  # print(NN)
  TestData <- TestData[order(runif(.N))][seq_len(NN)]
  # print(TestData)
  if(Debug) print("ML Reports 2.2")

  # Checks
  if(length(ModelID) == 0L) return(NULL)
  if(length(Algo) == 0L || Algo %in% c("H2OHGLM","CausalMediation")) return(NULL)
  if(!exists("DataList")) return(NULL)
  if(!exists("CodeList")) return(NULL)
  OutputList <- list()

  if(Debug) print("ML Reports 3")

  # Args
  MLOutputSelection <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0('MLOutputSelection',Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
  PDPVariables <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0('PDPVariables',Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
  PlotWidtha <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0('PlotWidtha',Page)]]}, error = function(x) NULL), Type = "numeric", Default = NULL)
  PlotWidtha <- paste0(PlotWidtha, "px")
  PlotHeighta <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0('PlotHeighta',Page)]]}, error = function(x) NULL), Type = "numeric", Default = NULL)
  PlotHeighta <- paste0(PlotHeighta, "px")

  if(Debug) print("ML Reports 4")

  # All Metrics Table
  if(TargetType == "Regression") {
    MetricsTable <- DataList[["ML_RegressionMetrics"]][["data"]]
    print(MetricsTable)
    if(!data.table::is.data.table(MetricsTable)) {
      print("ML Reports 4.1")
      print(names(MOL[[ModelID]]))
      MetricsTable <- MOL[[ModelID]][["ML_RegressionMetrics"]]
      print(MetricsTable)
    }
  } else if(TargetType == "Binary Classification") {
    MetricsTable <- DataList[["ML_ClassificationMetrics"]][["data"]]
    if(!data.table::is.data.table(MetricsTable)) {
      MetricsTable <- MOL[[ModelID]][["ML_ClassificationMetrics"]]
    }
  } else if(TargetType == "MultiClass") {
    MetricsTable <- DataList[["ML_MultiClassMetrics"]][["data"]]
    if(!data.table::is.data.table(MetricsTable)) {
      MetricsTable <- MOL[[ModelID]][["ML_MultiClassMetrics"]]
    }
  }

  # Remove empty rows
  if(Debug) print(MetricsTable)
  MetricsTable <- MetricsTable[`Model ID` != "zzz"]

  if(Debug) print("ML Reports 5")

  # Variable Importance
  if(tolower(Algo) == "catboost") {
    Test_Importance <- MOL[[ModelID]][["VariableImportance"]][["Test_Importance"]]
    Validation_Importance <- MOL[[ModelID]][["VariableImportance"]][["Validation_Importance"]]
    Train_Importance <- MOL[[ModelID]][["VariableImportance"]][["Train_Importance"]]
  } else {
    Test_Importance <- tryCatch({MOL[[ModelID]][["VariableImportance"]]}, error = function(x) NULL)
  }

  if(Debug) print("ML Reports 6")

  # Interaction Importances
  if(tolower(Algo) == 'catboost' && TargetType != "MultiClass") {

    Test_Interaction <- MOL[[ModelID]][["InteractionImportance"]][["Test_Interaction"]]
    Validation_Interaction <- NULL
    Train_Interaction <- NULL

    # Update Colnames
    if(!is.null(Test_Interaction)) data.table::setnames(Test_Interaction, old = 'score', new = 'Test_Importance', skip_absent = TRUE)
    if(!is.null(Validation_Interaction)) data.table::setnames(Validation_Interaction, old = 'score', new = 'Validation_Importance', skip_absent = TRUE)
    if(!is.null(Train_Interaction)) data.table::setnames(Train_Interaction, old = 'score', new = 'Train_Importance', skip_absent = TRUE)

    # CatBoost only
    if(is.null(Test_Interaction) && is.null(Validation_Interaction) && is.null(Train_Interaction)) {
      All_Interaction <- NULL
    } else if(!is.null(Test_Interaction) && !is.null(Validation_Interaction) && !is.null(Train_Interaction)) {
      All_Interaction <- merge(Test_Interaction, Validation_Interaction, by = c('Features1','Features2'), all = TRUE)
      All_Interaction <- merge(All_Interaction, Train_Interaction, by = c('Features1','Features2'), all = TRUE)
      data.table::setorderv(x = All_Interaction, cols = names(All_Interaction)[3L], order = -1)
    } else if(!is.null(Test_Interaction) && !is.null(Validation_Interaction) && is.null(Train_Interaction)) {
      All_Interaction <- merge(Test_Interaction, Validation_Interaction, by = c('Features1','Features2'), all = TRUE)
      data.table::setorderv(x = All_Interaction, cols = names(All_Interaction)[3L], order = -1)
    } else if(!is.null(Test_Interaction) && is.null(Validation_Interaction) && !is.null(Train_Interaction)) {
      All_Interaction <- merge(Test_Interaction, Train_Interaction, by = c('Features1','Features2'), all = TRUE)
      data.table::setorderv(x = All_Interaction, cols = names(All_Interaction)[3L], order = -1)
    } else if(is.null(Test_Interaction) && !is.null(Validation_Interaction) && !is.null(Train_Interaction)) {
      All_Interaction <- merge(Validation_Interaction, Train_Interaction, by = c('Features1','Features2'), all = TRUE)
      data.table::setorderv(x = All_Interaction, cols = names(All_Interaction)[3L], order = -1)
    } else if(is.null(Test_Interaction) && is.null(Validation_Interaction) && !is.null(Train_Interaction)) {
      All_Interaction <- Train_Interaction
    } else if(is.null(Test_Interaction) && !is.null(Validation_Interaction) && is.null(Train_Interaction)) {
      All_Interaction <- Validation_Interaction
    } else if(!is.null(Test_Interaction) && is.null(Validation_Interaction) && is.null(Train_Interaction)) {
      All_Interaction <- Test_Interaction
    } else {
      All_Interaction <- NULL
    }
  } else {
    All_Interaction <- NULL
  }

  if(Debug) print("ML Reports 7")

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Regression Outputs                                                        ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(TargetType == "Regression") {

    # Evaluation Metrics ----
    if("Model Comparison Metrics" %in% MLOutputSelection) {
      OutputList[["Regression Evaluation Metrics"]] <- reactable::reactable(
        data = MetricsTable,
        compact = TRUE,
        defaultPageSize = 10,
        wrap = TRUE,
        filterable = TRUE,
        fullWidth = TRUE,
        highlight = TRUE,
        pagination = TRUE,
        resizable = TRUE,
        searchable = TRUE,
        selection = "multiple",
        showPagination = TRUE,
        showSortable = TRUE,
        showSortIcon = TRUE,
        sortable = TRUE,
        striped = TRUE,
        theme = reactable::reactableTheme(
          color = FontColor$flv,
          backgroundColor = "#4f4f4f26",
          borderColor = "#dfe2e5",
          stripedColor = "#4f4f4f8f",
          highlightColor = "#8989898f",
          cellPadding = "8px 12px",
          style = list(
            fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
          ),
          searchInputStyle = list(width = "100%")
        )
      )

      ## Model_VarImportanceTable ----
      if(tolower(Algo) == 'catboost') {

        # Update Colnames
        if(!is.null(Test_Importance)) data.table::setnames(Test_Importance, old = 'Importance', new = 'Test_Importance', skip_absent = TRUE)
        if(!is.null(Validation_Importance)) data.table::setnames(Validation_Importance, old = 'Importance', new = 'Validation_Importance', skip_absent = TRUE)
        if(!is.null(Train_Importance)) data.table::setnames(Train_Importance, old = 'Importance', new = 'Train_Importance', skip_absent = TRUE)

        # CatBoost only
        if(is.null(Test_Importance) && is.null(Validation_Importance) && is.null(Train_Importance)) {
          All_Importance <- NULL
        } else if(!is.null(Test_Importance) && !is.null(Validation_Importance) && !is.null(Train_Importance)) {
          All_Importance <- merge(Test_Importance, Validation_Importance, by = 'Variable', all = TRUE)
          All_Importance <- merge(All_Importance, Train_Importance, by = 'Variable', all = TRUE)
        } else if(!is.null(Test_Importance) && !is.null(Validation_Importance) && is.null(Train_Importance)) {
          All_Importance <- merge(Test_Importance, Validation_Importance, by = 'Variable', all = TRUE)
        } else if(!is.null(Test_Importance) && is.null(Validation_Importance) && !is.null(Train_Importance)) {
          All_Importance <- merge(Test_Importance, Train_Importance, by = 'Variable', all = TRUE)
        } else if(is.null(Test_Importance) && !is.null(Validation_Importance) && !is.null(Train_Importance)) {
          All_Importance <- merge(Validation_Importance, Train_Importance, by = 'Variable', all = TRUE)
        } else if(is.null(Test_Importance) && is.null(Validation_Importance) && !is.null(Train_Importance)) {
          All_Importance <- Train_Importance
        } else if(is.null(Test_Importance) && !is.null(Validation_Importance) && is.null(Train_Importance)) {
          All_Importance <- Validation_Importance
        } else if(!is.null(Test_Importance) && is.null(Validation_Importance) && is.null(Train_Importance)) {
          All_Importance <- Test_Importance
        } else {
          All_Importance <- NULL
        }

      } else {
        All_Importance <- Test_Importance
      }

      # Variable Importance
      if(data.table::is.data.table(All_Importance)) {
        data.table::setorderv(x = All_Importance, cols = names(All_Importance)[2L], order = -1, na.last = TRUE)
        OutputList[["Variable Importance"]] <- reactable::reactable(
          data = All_Importance,
          compact = TRUE,
          defaultPageSize = 10,
          wrap = TRUE,
          filterable = TRUE,
          fullWidth = TRUE,
          highlight = TRUE,
          pagination = TRUE,
          resizable = TRUE,
          searchable = TRUE,
          selection = "multiple",
          showPagination = TRUE,
          showSortable = TRUE,
          showSortIcon = TRUE,
          sortable = TRUE,
          striped = TRUE,
          theme = reactable::reactableTheme(
            color = FontColor$flv,
            backgroundColor = "#4f4f4f26",
            borderColor = "#dfe2e5",
            stripedColor = "#4f4f4f8f",
            highlightColor = "#8989898f",
            cellPadding = "8px 12px",
            style = list(
              fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
            ),
            searchInputStyle = list(width = "100%")
          )
        )
      }

      # Interaction Importance Table
      if(data.table::is.data.table(All_Interaction)) {
        OutputList[["Interaction Importance"]] <- reactable::reactable(
          data = All_Interaction,
          compact = TRUE,
          defaultPageSize = 10,
          wrap = TRUE,
          filterable = TRUE,
          fullWidth = TRUE,
          highlight = TRUE,
          pagination = TRUE,
          resizable = TRUE,
          searchable = TRUE,
          selection = "multiple",
          showPagination = TRUE,
          showSortable = TRUE,
          showSortIcon = TRUE,
          sortable = TRUE,
          striped = TRUE,
          theme = reactable::reactableTheme(
            color = FontColor$flv,
            backgroundColor = "#4f4f4f26",
            borderColor = "#dfe2e5",
            stripedColor = "#4f4f4f8f",
            highlightColor = "#8989898f",
            cellPadding = "8px 12px",
            style = list(
              fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
            ),
            searchInputStyle = list(width = "100%")
          )
        )
      }
    }

    # Evaluation Plots ----
    if("Evaluation Plots" %in% MLOutputSelection) {

      if(Debug) print("ML Reports 7.1")

      OutputList[["TestData Residual Histogram"]] <- AutoPlots::Plot.Residuals.Histogram(
        dt = TestData,
        AggMethod = "mean",
        SampleSize = 30000,
        XVar = 'Predict',
        YVar = TargetColumnName,
        GroupVar = GroupVariableInclude,
        YVarTrans = "Identity",
        XVarTrans = "Identity",
        FacetRows = 1,
        FacetCols = 1,
        FacetLevels = NULL,
        NumberBins = 20,
        Height = PlotHeighta,
        Width = PlotWidtha,
        Title = "Residuals Histogram",
        ShowLabels = FALSE,
        Title.YAxis = TargetColumnName,
        Title.XAxis = 'Predict',
        EchartsTheme = Theme,
        TimeLine = FALSE,
        X_Scroll = TRUE,
        Y_Scroll = FALSE,
        TextColor = "white",
        title.fontSize = 22,
        title.fontWeight = "bold",
        title.textShadowColor = "#63aeff",
        title.textShadowBlur = 3,
        title.textShadowOffsetY = 1,
        title.textShadowOffsetX = -1,
        xaxis.fontSize = 14,
        yaxis.fontSize = 14,
        Debug = Debug)

      ### Train
      if(length(TrainData) > 0L) {

        if(Debug) print("ML Reports 7.2")

        OutputList[["TrainData Residual Histogram"]] <- AutoPlots::Plot.Residuals.Histogram(
          dt = TrainData,
          AggMethod = "mean",
          SampleSize = 30000,
          XVar = 'Predict',
          YVar = TargetColumnName,
          GroupVar = GroupVariableInclude,
          YVarTrans = "Identity",
          XVarTrans = "Identity",
          FacetRows = 1,
          FacetCols = 1,
          FacetLevels = NULL,
          NumberBins = 20,
          Height = PlotHeighta,
          Width = PlotWidtha,
          Title = "Residuals Histogram",
          ShowLabels = FALSE,
          Title.YAxis = TargetColumnName,
          Title.XAxis = 'Predict',
          EchartsTheme = Theme,
          TimeLine = FALSE,
          X_Scroll = TRUE,
          Y_Scroll = TRUE,
          TextColor = "white",
          title.fontSize = 22,
          title.fontWeight = "bold",
          title.textShadowColor = "#63aeff",
          title.textShadowBlur = 3,
          title.textShadowOffsetY = 1,
          title.textShadowOffsetX = -1,
          xaxis.fontSize = 14,
          yaxis.fontSize = 14,
          Debug = Debug)
      }

      ## EvaluationPlots_CalibrationPlot ----

      ### Test

      if(Debug) print("ML Reports 7.3")

      OutputList[["TestData Calibration Plot"]] <- AutoPlots::Plot.Calibration.Line(
        dt = TestData,
        AggMethod = "mean",
        XVar = 'Predict',
        YVar = TargetColumnName,
        GroupVar = GroupVariableInclude,
        YVarTrans = "Identity",
        XVarTrans = "Identity",
        FacetRows = 1,
        FacetCols = 1,
        FacetLevels = NULL,
        NumberBins = 20,
        Height = PlotHeighta,
        Width = PlotWidtha,
        Title = "Calibration Line Plot",
        ShowLabels = FALSE,
        Title.YAxis = TargetColumnName,
        Title.XAxis = "Predict",
        EchartsTheme = Theme,
        TimeLine = FALSE,
        X_Scroll = TRUE,
        Y_Scroll = TRUE,
        TextColor = "white",
        Debug = Debug)

      ### Train
      if(!is.null(TrainData) && TrainData[,.N] > 100L) {

        if(Debug) print("ML Reports 7.4")

        OutputList[["TrainData Calibration Plot"]] <- AutoPlots::Plot.Calibration.Line(
          dt = TrainData,
          AggMethod = "mean",
          XVar = 'Predict',
          YVar = TargetColumnName,
          GroupVar = GroupVariableInclude,
          YVarTrans = "Identity",
          XVarTrans = "Identity",
          FacetRows = 1,
          FacetCols = 1,
          FacetLevels = NULL,
          NumberBins = 20,
          Height = PlotHeighta,
          Width = PlotWidtha,
          Title = "Calibration Line Plot",
          ShowLabels = FALSE,
          Title.YAxis = TargetColumnName,
          Title.XAxis = "Predict",
          EchartsTheme = Theme,
          TimeLine = FALSE,
          X_Scroll = TRUE,
          Y_Scroll = TRUE,
          TextColor = "white",
          Debug = Debug)
      }

      ## EvaluationPlots_CalibrationBoxPlot ----

      ### Test

      if(Debug) print("ML Reports 7.5")

      OutputList[["TestData Calibration Box Plot"]] <- AutoPlots::Plot.Calibration.Box(
        dt = TestData,
        SampleSize = 30000L,
        AggMethod = "mean",
        XVar = 'Predict',
        YVar = TargetColumnName,
        GroupVar = NULL, #GroupVariableInclude,
        YVarTrans = "Identity",
        XVarTrans = "Identity",
        FacetRows = 1,
        FacetCols = 1,
        FacetLevels = NULL,
        NumberBins = 20,
        Height = PlotHeighta,
        Width = PlotWidtha,
        Title = "Calibration Plot",
        ShowLabels = FALSE,
        Title.YAxis = TargetColumnName,
        Title.XAxis = "Predict",
        EchartsTheme = Theme,
        TimeLine = FALSE,
        X_Scroll = TRUE,
        Y_Scroll = TRUE,
        TextColor = "white",
        Debug = Debug)

      ### Train
      if(!is.null(TrainData) && TrainData[,.N] > 100L) {

        if(Debug) print("ML Reports 7.6")

        OutputList[["TrainData Calibration Box Plot"]] <- AutoPlots::Plot.Calibration.Box(
          dt = TrainData,
          SampleSize = 30000L,
          AggMethod = "mean",
          XVar = 'Predict',
          YVar = TargetColumnName,
          GroupVar = NULL, #GroupVariableInclude,
          YVarTrans = "Identity",
          XVarTrans = "Identity",
          FacetRows = 1,
          FacetCols = 1,
          FacetLevels = NULL,
          NumberBins = 20,
          Height = PlotHeighta,
          Width = PlotWidtha,
          Title = "Calibration Plot",
          ShowLabels = FALSE,
          Title.YAxis = TargetColumnName,
          Title.XAxis = g,
          EchartsTheme = Theme,
          TimeLine = FALSE,
          X_Scroll = TRUE,
          Y_Scroll = TRUE,
          TextColor = "white",
          Debug = Debug)
      }

      ## EvaluationPlots_ResidualsScatterPlot (depends on ) ----

      if(Debug) print("ML Reports 7.8")

      # OutputList[["TestData Residuals Scatter Plot"]] <- AutoPlots::Plot.Residuals.Scatter(
      #   dt = TestData,
      #   AggMethod = "mean",
      #   SampleSize = 30000,
      #   XVar = "Predict",
      #   YVar = TargetColumnName,
      #   GroupVar = GroupVariableInclude,
      #   YVarTrans = "Identity",
      #   XVarTrans = "Identity",
      #   FacetRows = 1,
      #   FacetCols = 1,
      #   FacetLevels = NULL,
      #   Height = PlotHeighta,
      #   Width = PlotWidtha,
      #   Title = "Calibration Plot",
      #   ShowLabels = FALSE,
      #   Title.YAxis = TargetColumnName,
      #   Title.XAxis = g,
      #   EchartsTheme = Theme,
      #   TimeLine = FALSE,
      #   X_Scroll = TRUE,
      #   Y_Scroll = TRUE,
      #   TextColor = "white",
      #   Debug = Debug)

      # if(length(TrainData) > 0L) {
      #
      #   if(Debug) print("ML Reports 7.9")
      #
      #   OutputList[["TrainData Residuals Scatter Plot"]] <- AutoPlots::Plot.Residuals.Scatter(
      #     dt = TrainData,
      #     AggMethod = "mean",
      #     SampleSize = 30000,
      #     XVar = "Predict",
      #     YVar = TargetColumnName,
      #     GroupVar = GroupVariableInclude,
      #     YVarTrans = "Identity",
      #     XVarTrans = "Identity",
      #     FacetRows = 1,
      #     FacetCols = 1,
      #     FacetLevels = NULL,
      #     Height = PlotHeighta,
      #     Width = PlotWidtha,
      #     Title = "Residuals ScatterPlot",
      #     ShowLabels = FALSE,
      #     Title.YAxis = TargetColumnName,
      #     Title.XAxis = "Predict",
      #     EchartsTheme = Theme,
      #     TimeLine = FALSE,
      #     X_Scroll = TRUE,
      #     Y_Scroll = TRUE,
      #     TextColor = "white",
      #     Debug = Debug)
      # }
    }

    # Model Interpretation ----
    if("Partial Dependence Plots" %in% MLOutputSelection) {

      # Add Plots
      if(!is.null(TestData) && !is.null(PDPVariables)) {
        for(g in PDPVariables) {
          if(is.numeric(TestData[[g]])) {
            OutputList[[paste0('TestData Partial Dependence Line Plot: ', g)]] <- AutoPlots::Plot.PartialDependence.Line(
              dt = TestData,
              XVar = g,
              YVar = TargetColumnName,
              ZVar = 'Predict',
              YVarTrans = "Identity",
              XVarTrans = "Identity",
              ZVarTrans = "Identity",
              FacetRows = 1,
              FacetCols = 1,
              FacetLevels = NULL,
              GroupVar = GroupVariableInclude,
              NumberBins = 20,
              AggMethod = "mean",
              Height = PlotHeighta,
              Width = PlotWidtha,
              Title = "Partial Dependence Line",
              ShowLabels = FALSE,
              Title.YAxis = TargetColumnName,
              Title.XAxis = g,
              EchartsTheme = Theme,
              TimeLine = FALSE,
              X_Scroll = TRUE,
              Y_Scroll = TRUE,
              TextColor = "white",
              Debug = Debug)
          }
        }
      }

      # Add Plots
      if(!is.null(TrainData) && TrainData[,.N] > 100L && !is.null(PDPVariables)) {
        for(g in PDPVariables) {
          if(is.numeric(TrainData[[g]])) {
            OutputList[[paste0('TrainData Partial Dependence Line Plot: ', g)]] <- AutoPlots::Plot.PartialDependence.Line(
              dt = TrainData,
              XVar = g,
              YVar = TargetColumnName,
              ZVar = 'Predict',
              YVarTrans = "Identity",
              XVarTrans = "Identity",
              ZVarTrans = "Identity",
              FacetRows = 1,
              FacetCols = 1,
              FacetLevels = NULL,
              GroupVar = GroupVariableInclude,
              NumberBins = 20,
              AggMethod = "mean",
              Height = PlotHeighta,
              Width = PlotWidtha,
              Title = "Partial Dependence Line",
              ShowLabels = FALSE,
              Title.YAxis = TargetColumnName,
              Title.XAxis = g,
              EchartsTheme = Theme,
              TimeLine = FALSE,
              X_Scroll = TRUE,
              Y_Scroll = TRUE,
              TextColor = "white",
              Debug = Debug)
          }
        }
      }

      # Add Plots
      if(!is.null(TestData) && !is.null(PDPVariables)) {
        for(g in PDPVariables) {
          if(is.numeric(TestData[[g]])) {
            if(Debug) print(paste0("g = ", g))
            OutputList[[paste0('TestData Partial Dependence Box Plot: ', g)]] <- AutoPlots::Plot.PartialDependence.Box(
              dt = TestData,
              XVar = g,
              YVar = TargetColumnName,
              ZVar = 'Predict',
              YVarTrans = "Identity",
              XVarTrans = "Identity",
              ZVarTrans = "Identity",
              FacetRows = 1,
              FacetCols = 1,
              FacetLevels = NULL,
              GroupVar = GroupVariableInclude,
              NumberBins = 20,
              AggMethod = "mean",
              Height = PlotHeighta,
              Width = PlotWidtha,
              Title = "Partial Dependence Line",
              ShowLabels = FALSE,
              Title.YAxis = TargetColumnName,
              Title.XAxis = g,
              EchartsTheme = Theme,
              TimeLine = FALSE,
              X_Scroll = TRUE,
              Y_Scroll = TRUE,
              TextColor = "white",
              Debug = Debug)
          }
        }
      }

      # Add Plots
      if(!is.null(TrainData) && TrainData[,.N] > 100L && !is.null(PDPVariables)) {
        for(g in PDPVariables) {
          if(is.numeric(TrainData[[g]])) {
            OutputList[[paste0('TrainData Partial Dependence Box Plot: ', g)]] <- AutoPlots::Plot.PartialDependence.Box(
              dt = TrainData,
              XVar = g,
              YVar = TargetColumnName,
              ZVar = 'Predict',
              YVarTrans = "Identity",
              XVarTrans = "Identity",
              ZVarTrans = "Identity",
              FacetRows = 1,
              FacetCols = 1,
              FacetLevels = NULL,
              GroupVar = GroupVariableInclude,
              NumberBins = 20,
              AggMethod = "mean",
              Height = PlotHeighta,
              Width = PlotWidtha,
              Title = "Partial Dependence Line",
              ShowLabels = FALSE,
              Title.YAxis = TargetColumnName,
              Title.XAxis = g,
              EchartsTheme = Theme,
              TimeLine = FALSE,
              X_Scroll = TRUE,
              Y_Scroll = TRUE,
              TextColor = "white",
              Debug = Debug)
          }
        }
      }

      # Add Plots
      if(!is.null(TestData) && !is.null(PDPVariables)) {
        for(g in PDPVariables) {
          if(!is.numeric(TestData[[g]])) {
            OutputList[[paste0('TestData Partial Dependence Heatmap: ', g)]] <- AutoPlots::Plot.PartialDependence.HeatMap(
              dt = TestData,
              XVar = g,
              YVar = TargetColumnName,
              ZVar = 'Predict',
              YVarTrans = "Identity",
              XVarTrans = "Identity",
              ZVarTrans = "Identity",
              FacetRows = 1,
              FacetCols = 1,
              FacetLevels = NULL,
              GroupVar = GroupVariableInclude,
              NumberBins = 20,
              AggMethod = "mean",
              Height = PlotHeighta,
              Width = PlotWidtha,
              Title = "Partial Dependence Line",
              ShowLabels = FALSE,
              Title.YAxis = TargetColumnName,
              Title.XAxis = g,
              EchartsTheme = Theme,
              TimeLine = FALSE,
              X_Scroll = TRUE,
              Y_Scroll = TRUE,
              TextColor = "white",
              Debug = Debug)
          }
        }
      }

      # Add Plots
      if(!is.null(TrainData) && TrainData[,.N] > 100L && !is.null(PDPVariables)) {
        for(g in PDPVariables) {
          if(!is.numeric(TrainData[[g]])) {
            OutputList[[paste0('TrainData Partial Dependence Heatmap: ', g)]] <- AutoPlots::Plot.PartialDependence.HeatMap(
              dt = TrainData,
              XVar = g,
              YVar = TargetColumnName,
              ZVar = 'Predict',
              YVarTrans = "Identity",
              XVarTrans = "Identity",
              ZVarTrans = "Identity",
              FacetRows = 1,
              FacetCols = 1,
              FacetLevels = NULL,
              GroupVar = GroupVariableInclude,
              NumberBins = 20,
              AggMethod = "mean",
              Height = PlotHeighta,
              Width = PlotWidtha,
              Title = "Partial Dependence Line",
              ShowLabels = FALSE,
              Title.YAxis = TargetColumnName,
              Title.XAxis = g,
              EchartsTheme = Theme,
              TimeLine = FALSE,
              X_Scroll = TRUE,
              Y_Scroll = TRUE,
              TextColor = "white",
              Debug = Debug)
          }
        }
      }

    }
  }

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Binary Classification Outputs                                             ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(TargetType == "Binary Classification") {

    # Convert from Factor to Numeric
    if(Algo %in% c("H2OGBM", "H2ODRF", "H2OGLM", "H2OHGLM")) {
      TestData[, eval(TargetColumnName) := as.numeric(as.character(get(TargetColumnName)))]
      if(data.table::is.data.table(TrainData)) {
        TrainData[, eval(TargetColumnName) := as.numeric(as.character(get(TargetColumnName)))]
      }
    }

    # Evaluation Metrics ----
    if("Model Comparison Metrics" %in% MLOutputSelection) {
      OutputList[["Classification Evaluation Metrics"]] <- reactable::reactable(
        data = MetricsTable,
        compact = TRUE,
        defaultPageSize = 10,
        wrap = TRUE,
        filterable = TRUE,
        fullWidth = TRUE,
        highlight = TRUE,
        pagination = TRUE,
        resizable = TRUE,
        searchable = TRUE,
        selection = "multiple",
        showPagination = TRUE,
        showSortable = TRUE,
        showSortIcon = TRUE,
        sortable = TRUE,
        striped = TRUE,
        theme = reactable::reactableTheme(
          color = FontColor$flv,
          backgroundColor = "#4f4f4f26",
          borderColor = "#dfe2e5",
          stripedColor = "#4f4f4f8f",
          highlightColor = "#8989898f",
          cellPadding = "8px 12px",
          style = list(
            fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
          ),
          searchInputStyle = list(width = "100%")
        )
      )

      ## Model_VarImportanceTable ----
      if(tolower(Algo) == 'catboost') {

        # Update Colnames
        if(!is.null(Test_Importance)) data.table::setnames(Test_Importance, old = 'Importance', new = 'Test_Importance', skip_absent = TRUE)
        if(!is.null(Validation_Importance)) data.table::setnames(Validation_Importance, old = 'Importance', new = 'Validation_Importance', skip_absent = TRUE)
        if(!is.null(Train_Importance)) data.table::setnames(Train_Importance, old = 'Importance', new = 'Train_Importance', skip_absent = TRUE)

        # CatBoost only
        if(is.null(Test_Importance) && is.null(Validation_Importance) && is.null(Train_Importance)) {
          All_Importance <- NULL
        } else if(!is.null(Test_Importance) && !is.null(Validation_Importance) && !is.null(Train_Importance)) {
          All_Importance <- merge(Test_Importance, Validation_Importance, by = 'Variable', all = TRUE)
          All_Importance <- merge(All_Importance, Train_Importance, by = 'Variable', all = TRUE)
        } else if(!is.null(Test_Importance) && !is.null(Validation_Importance) && is.null(Train_Importance)) {
          All_Importance <- merge(Test_Importance, Validation_Importance, by = 'Variable', all = TRUE)
        } else if(!is.null(Test_Importance) && is.null(Validation_Importance) && !is.null(Train_Importance)) {
          All_Importance <- merge(Test_Importance, Train_Importance, by = 'Variable', all = TRUE)
        } else if(is.null(Test_Importance) && !is.null(Validation_Importance) && !is.null(Train_Importance)) {
          All_Importance <- merge(Validation_Importance, Train_Importance, by = 'Variable', all = TRUE)
        } else if(is.null(Test_Importance) && is.null(Validation_Importance) && !is.null(Train_Importance)) {
          All_Importance <- Train_Importance
        } else if(is.null(Test_Importance) && !is.null(Validation_Importance) && is.null(Train_Importance)) {
          All_Importance <- Validation_Importance
        } else if(!is.null(Test_Importance) && is.null(Validation_Importance) && is.null(Train_Importance)) {
          All_Importance <- Test_Importance
        } else {
          All_Importance <- NULL
        }

      } else {
        All_Importance <- Test_Importance
      }

      # Variable Importance
      if(data.table::is.data.table(All_Importance)) {
        data.table::setorderv(x = All_Importance, cols = names(All_Importance)[2L], order = -1, na.last = TRUE)
        OutputList[["Variable Importance"]] <- reactable::reactable(
          data = All_Importance,
          compact = TRUE,
          defaultPageSize = 10,
          wrap = TRUE,
          filterable = TRUE,
          fullWidth = TRUE,
          highlight = TRUE,
          pagination = TRUE,
          resizable = TRUE,
          searchable = TRUE,
          selection = "multiple",
          showPagination = TRUE,
          showSortable = TRUE,
          showSortIcon = TRUE,
          sortable = TRUE,
          striped = TRUE,
          theme = reactable::reactableTheme(
            color = FontColor$flv,
            backgroundColor = "#4f4f4f26",
            borderColor = "#dfe2e5",
            stripedColor = "#4f4f4f8f",
            highlightColor = "#8989898f",
            cellPadding = "8px 12px",
            style = list(
              fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
            ),
            searchInputStyle = list(width = "100%")
          )
        )
      }

      # Interaction Importance Table
      if(data.table::is.data.table(All_Interaction)) {
        OutputList[["Interaction Importance"]] <- reactable::reactable(
          data = All_Interaction,
          compact = TRUE,
          defaultPageSize = 10,
          wrap = TRUE,
          filterable = TRUE,
          fullWidth = TRUE,
          highlight = TRUE,
          pagination = TRUE,
          resizable = TRUE,
          searchable = TRUE,
          selection = "multiple",
          showPagination = TRUE,
          showSortable = TRUE,
          showSortIcon = TRUE,
          sortable = TRUE,
          striped = TRUE,
          theme = reactable::reactableTheme(
            color = FontColor$flv,
            backgroundColor = "#4f4f4f26",
            borderColor = "#dfe2e5",
            stripedColor = "#4f4f4f8f",
            highlightColor = "#8989898f",
            cellPadding = "8px 12px",
            style = list(
              fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
            ),
            searchInputStyle = list(width = "100%")
          )
        )
      }
    }

    if(Debug) print("ML Reports 9")

    # Evaluation Plots ----
    if("Evaluation Plots" %in% MLOutputSelection) {

      if(Debug) print("ML Reports 9.1")

      ### Test
      OutputList[["TestData Calibration Plot"]] <- AutoPlots::Plot.Calibration.Line(
        dt = TestData,
        AggMethod = "mean",
        XVar = 'p1',
        YVar = TargetColumnName,
        GroupVar = GroupVariableInclude,
        YVarTrans = "Identity",
        XVarTrans = "Identity",
        FacetRows = 1,
        FacetCols = 1,
        FacetLevels = NULL,
        NumberBins = 20,
        Height = PlotHeighta,
        Width = PlotWidtha,
        Title = "Calibration Line Plot",
        ShowLabels = FALSE,
        Title.YAxis = TargetColumnName,
        Title.XAxis = "Predict",
        EchartsTheme = Theme,
        TimeLine = FALSE,
        X_Scroll = TRUE,
        Y_Scroll = TRUE,
        TextColor = "white",
        Debug = Debug)

      if(Debug) print("ML Reports 9.2")

      ### Train
      if(!is.null(TrainData) && TrainData[,.N] > 100L) {
        OutputList[["TrainData Calibration Plot"]] <- AutoPlots::Plot.Calibration.Line(
          dt = TrainData,
          AggMethod = "mean",
          XVar = 'p1',
          YVar = TargetColumnName,
          GroupVar = GroupVariableInclude,
          YVarTrans = "Identity",
          XVarTrans = "Identity",
          FacetRows = 1,
          FacetCols = 1,
          FacetLevels = NULL,
          NumberBins = 20,
          Height = PlotHeighta,
          Width = PlotWidtha,
          Title = "Calibration Line Plot",
          ShowLabels = FALSE,
          Title.YAxis = TargetColumnName,
          Title.XAxis = "p1",
          EchartsTheme = Theme,
          TimeLine = FALSE,
          X_Scroll = TRUE,
          Y_Scroll = TRUE,
          TextColor = "white",
          Debug = Debug)
      }

      if(Debug) print("ML Reports 9.3")

      ## ROC
      OutputList[["TestData ROC Plot"]] <- AutoPlots::Plot.ROC(
        dt = TestData,
        AggMethod = "mean",
        SampleSize = 30000,
        XVar = "p1",
        YVar = TargetColumnName,
        GroupVar = GroupVariableInclude,
        YVarTrans = "Identity",
        XVarTrans = "Identity",
        FacetRows = 1,
        FacetCols = 1,
        FacetLevels = NULL,
        Height = PlotHeighta,
        Width = PlotWidtha,
        Title = "ROC Plot",
        ShowLabels = FALSE,
        Title.YAxis = "True Positive Rate",
        Title.XAxis = "1 - False Positive Rate",
        EchartsTheme = Theme,
        TimeLine = FALSE,
        X_Scroll = TRUE,
        Y_Scroll = TRUE,
        TextColor = "white",
        Debug = Debug)

      if(Debug) print("ML Reports 9.4")

      if(length(TrainData) > 0L) {
        OutputList[["TrainData ROC Plot"]] <- AutoPlots::Plot.ROC(
          dt = TrainData,
          AggMethod = "mean",
          SampleSize = 30000,
          XVar = "p1",
          YVar = TargetColumnName,
          GroupVar = GroupVariableInclude,
          YVarTrans = "Identity",
          XVarTrans = "Identity",
          FacetRows = 1,
          FacetCols = 1,
          FacetLevels = NULL,
          Height = PlotHeighta,
          Width = PlotWidtha,
          Title = "ROC Plot",
          ShowLabels = FALSE,
          Title.YAxis = "True Positive Rate",
          Title.XAxis = "1 - False Positive Rate",
          EchartsTheme = Theme,
          TimeLine = FALSE,
          X_Scroll = TRUE,
          Y_Scroll = TRUE,
          TextColor = "white",
          Debug = Debug)
      }

      if(Debug) print("ML Reports 9.5")

      ## Lift
      if(Debug) print(data.table::is.data.table(TestData))
      if(Debug) print(FALSE)
      if(Debug) print("p1")
      if(Debug) print(TargetColumnName)
      if(Debug) print(GroupVariableInclude)
      if(Debug) print("Identity")
      if(Debug) print("Identity")
      if(Debug) print(1)
      if(Debug) print(1)
      if(Debug) print(NULL)
      if(Debug) print(PlotHeighta)
      if(Debug) print(PlotWidtha)
      if(Debug) print("Lift Plot")
      if(Debug) print(FALSE)
      if(Debug) print("Lift")
      if(Debug) print("% Positive Classified")
      if(Debug) print(Theme)
      if(Debug) print(FALSE)
      if(Debug) print(TRUE)
      if(Debug) print(TRUE)
      if(Debug) print("white")
      if(Debug) print(FALSE)

      OutputList[["TestData Lift Plot"]] <- AutoPlots::Plot.Lift(
        dt = TestData,
        PreAgg = FALSE,
        XVar = "p1",
        YVar = TargetColumnName,
        ZVar = NULL,
        GroupVar = GroupVariableInclude,
        YVarTrans = "Identity",
        XVarTrans = "Identity",
        ZVarTrans = "Identity",
        FacetRows = 1,
        FacetCols = 1,
        FacetLevels = NULL,
        NumberBins = 20,
        Height = PlotHeighta,
        Width = PlotWidtha,
        Title = "Lift Plot",
        ShowLabels = FALSE,
        Title.YAxis = "Lift",
        Title.XAxis = "Positive Predicted",
        EchartsTheme = Theme,
        TimeLine = FALSE,
        X_Scroll = TRUE,
        Y_Scroll = TRUE,
        TextColor = "white",
        Debug = Debug)

      if(Debug) print("ML Reports 9.6")

      if(length(TrainData) > 0L) {
        OutputList[["TrainData Lift Plot"]] <- AutoPlots::Plot.Lift(
          dt = TrainData,
          PreAgg = TRUE,
          XVar = "p1",
          YVar = TargetColumnName,
          ZVar = NULL,
          GroupVar = GroupVariableInclude,
          YVarTrans = "Identity",
          XVarTrans = "Identity",
          FacetRows = 1,
          FacetCols = 1,
          FacetLevels = NULL,
          Height = PlotHeighta,
          Width = PlotWidtha,
          Title = "Lift Plot",
          ShowLabels = FALSE,
          Title.YAxis = "Lift",
          Title.XAxis = "Positive Predicted",
          EchartsTheme = Theme,
          TimeLine = FALSE,
          X_Scroll = TRUE,
          Y_Scroll = TRUE,
          TextColor = "white",
          Debug = Debug)
      }

      if(Debug) print("ML Reports 9.7")

      ## Gains
      OutputList[["TestData Gains Plot"]] <- AutoPlots::Plot.Gains(
        dt = TestData,
        PreAgg = FALSE,
        XVar = "p1",
        YVar = TargetColumnName,
        GroupVar = GroupVariableInclude,
        YVarTrans = "Identity",
        XVarTrans = "Identity",
        FacetRows = 1,
        FacetCols = 1,
        FacetLevels = NULL,
        Height = PlotHeighta,
        Width = PlotWidtha,
        Title = "Gains Plot",
        ShowLabels = FALSE,
        Title.YAxis = TargetColumnName,
        Title.XAxis = g,
        EchartsTheme = Theme,
        TimeLine = FALSE,
        X_Scroll = TRUE,
        Y_Scroll = TRUE,
        TextColor = "white",
        Debug = Debug)

      if(Debug) print("ML Reports 9.8")

      if(length(TrainData) > 0L) {
        OutputList[["TrainData Gains Plot"]] <- AutoPlots::Plot.Gains(
          dt = TrainData,
          PreAgg = TRUE,
          XVar = "p1",
          YVar = TargetColumnName,
          ZVar = NULL,
          GroupVar = GroupVariableInclude,
          YVarTrans = "Identity",
          XVarTrans = "Identity",
          FacetRows = 1,
          FacetCols = 1,
          FacetLevels = NULL,
          Height = PlotHeighta,
          Width = PlotWidtha,
          Title = "Gains Plot",
          ShowLabels = FALSE,
          Title.YAxis = TargetColumnName,
          Title.XAxis = "p1",
          EchartsTheme = Theme,
          TimeLine = FALSE,
          X_Scroll = TRUE,
          Y_Scroll = TRUE,
          TextColor = "white",
          Debug = Debug)
      }
    }

    if(Debug) print("ML Reports 10")

    # Model Interpretation ----
    if("Partial Dependence Plots" %in% MLOutputSelection) {

      # Add Plots
      if(!is.null(TestData) && !is.null(PDPVariables)) {
        for(g in PDPVariables) {
          if(is.numeric(TestData[[g]])) {
            OutputList[[paste0('TestData Partial Dependence Line Plot: ', g)]] <- AutoPlots::Plot.PartialDependence.Line(
              dt = TestData,
              XVar = g,
              YVar = TargetColumnName,
              ZVar = 'p1',
              YVarTrans = "Identity",
              XVarTrans = "Identity",
              ZVarTrans = "Identity",
              FacetRows = 1,
              FacetCols = 1,
              FacetLevels = NULL,
              GroupVar = GroupVariableInclude,
              NumberBins = 20,
              AggMethod = "mean",
              Height = PlotHeighta,
              Width = PlotWidtha,
              Title = "Partial Dependence Line",
              ShowLabels = FALSE,
              Title.YAxis = TargetColumnName,
              Title.XAxis = g,
              EchartsTheme = Theme,
              TimeLine = FALSE,
              X_Scroll = TRUE,
              Y_Scroll = TRUE,
              TextColor = "white",
              Debug = Debug)
          }
        }
      }

      # Add Plots
      if(!is.null(TrainData) && TrainData[,.N] > 100L && !is.null(PDPVariables)) {
        for(g in PDPVariables) {
          if(is.numeric(TrainData[[g]])) {
            OutputList[[paste0('TrainData Partial Dependence Line Plot: ', g)]] <- AutoPlots::Plot.PartialDependence.Line(
              dt = TrainData,
              XVar = g,
              YVar = TargetColumnName,
              ZVar = 'p1',
              YVarTrans = "Identity",
              XVarTrans = "Identity",
              ZVarTrans = "Identity",
              FacetRows = 1,
              FacetCols = 1,
              FacetLevels = NULL,
              GroupVar = GroupVariableInclude,
              NumberBins = 20,
              AggMethod = "mean",
              Height = PlotHeighta,
              Width = PlotWidtha,
              Title = "Partial Dependence Line",
              ShowLabels = FALSE,
              Title.YAxis = TargetColumnName,
              Title.XAxis = g,
              EchartsTheme = Theme,
              TimeLine = FALSE,
              X_Scroll = TRUE,
              Y_Scroll = TRUE,
              TextColor = "white",
              Debug = Debug)
          }
        }
      }

      # Add Plots
      if(!is.null(TestData) && !is.null(PDPVariables)) {
        for(g in PDPVariables) {
          if(!is.numeric(TestData[[g]])) {
            OutputList[[paste0('TestData Partial Dependence Heatmap: ', g)]] <- AutoPlots::Plot.PartialDependence.HeatMap(
              dt = TestData,
              XVar = g,
              YVar = TargetColumnName,
              ZVar = 'p1',
              YVarTrans = "Identity",
              XVarTrans = "Identity",
              ZVarTrans = "Identity",
              FacetRows = 1,
              FacetCols = 1,
              FacetLevels = NULL,
              GroupVar = GroupVariableInclude,
              NumberBins = 20,
              AggMethod = "mean",
              Height = PlotHeighta,
              Width = PlotWidtha,
              Title = "Partial Dependence Line",
              ShowLabels = FALSE,
              Title.YAxis = TargetColumnName,
              Title.XAxis = g,
              EchartsTheme = Theme,
              TimeLine = FALSE,
              X_Scroll = TRUE,
              Y_Scroll = TRUE,
              TextColor = "white",
              Debug = Debug)
          }
        }
      }

      # Add Plots
      if(!is.null(TrainData) && TrainData[,.N] > 100L && !is.null(PDPVariables)) {
        for(g in PDPVariables) {
          if(!is.numeric(TrainData[[g]])) {
            OutputList[[paste0('TrainData Partial Dependence Heatmap: ', g)]] <- AutoPlots::Plot.PartialDependence.HeatMap(
              dt = TrainData,
              XVar = g,
              YVar = TargetColumnName,
              ZVar = 'p1',
              YVarTrans = "Identity",
              XVarTrans = "Identity",
              ZVarTrans = "Identity",
              FacetRows = 1,
              FacetCols = 1,
              FacetLevels = NULL,
              GroupVar = GroupVariableInclude,
              NumberBins = 20,
              AggMethod = "mean",
              Height = PlotHeighta,
              Width = PlotWidtha,
              Title = "Partial Dependence Line",
              ShowLabels = FALSE,
              Title.YAxis = TargetColumnName,
              Title.XAxis = g,
              EchartsTheme = Theme,
              TimeLine = FALSE,
              X_Scroll = TRUE,
              Y_Scroll = TRUE,
              TextColor = "white",
              Debug = Debug)
          }
        }
      }

    }
  }

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # MultiClass Outputs                                                        ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(TargetType == "MultiClass") {

    # Evaluation Metrics ----
    if("Model Comparison Metrics" %in% MLOutputSelection) {
      OutputList[["MultiClass Evaluation Metrics"]] <- reactable::reactable(
        data = MetricsTable,
        compact = TRUE,
        defaultPageSize = 10,
        wrap = TRUE,
        filterable = TRUE,
        fullWidth = TRUE,
        highlight = TRUE,
        pagination = TRUE,
        resizable = TRUE,
        searchable = TRUE,
        selection = "multiple",
        showPagination = TRUE,
        showSortable = TRUE,
        showSortIcon = TRUE,
        sortable = TRUE,
        striped = TRUE,
        theme = reactable::reactableTheme(
          color = FontColor$flv,
          backgroundColor = "#4f4f4f26",
          borderColor = "#dfe2e5",
          stripedColor = "#4f4f4f8f",
          highlightColor = "#8989898f",
          cellPadding = "8px 12px",
          style = list(
            fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
          ),
          searchInputStyle = list(width = "100%")
        )
      )

      ## Model_VarImportanceTable ----
      if(tolower(Algo) == 'catboost') {

        # Update Colnames
        if(!is.null(Test_Importance)) data.table::setnames(Test_Importance, old = 'Importance', new = 'Test_Importance', skip_absent = TRUE)
        if(!is.null(Validation_Importance)) data.table::setnames(Validation_Importance, old = 'Importance', new = 'Validation_Importance', skip_absent = TRUE)
        if(!is.null(Train_Importance)) data.table::setnames(Train_Importance, old = 'Importance', new = 'Train_Importance', skip_absent = TRUE)

        # CatBoost only
        if(is.null(Test_Importance) && is.null(Validation_Importance) && is.null(Train_Importance)) {
          All_Importance <- NULL
        } else if(!is.null(Test_Importance) && !is.null(Validation_Importance) && !is.null(Train_Importance)) {
          All_Importance <- merge(Test_Importance, Validation_Importance, by = 'Variable', all = TRUE)
          All_Importance <- merge(All_Importance, Train_Importance, by = 'Variable', all = TRUE)
        } else if(!is.null(Test_Importance) && !is.null(Validation_Importance) && is.null(Train_Importance)) {
          All_Importance <- merge(Test_Importance, Validation_Importance, by = 'Variable', all = TRUE)
        } else if(!is.null(Test_Importance) && is.null(Validation_Importance) && !is.null(Train_Importance)) {
          All_Importance <- merge(Test_Importance, Train_Importance, by = 'Variable', all = TRUE)
        } else if(is.null(Test_Importance) && !is.null(Validation_Importance) && !is.null(Train_Importance)) {
          All_Importance <- merge(Validation_Importance, Train_Importance, by = 'Variable', all = TRUE)
        } else if(is.null(Test_Importance) && is.null(Validation_Importance) && !is.null(Train_Importance)) {
          All_Importance <- Train_Importance
        } else if(is.null(Test_Importance) && !is.null(Validation_Importance) && is.null(Train_Importance)) {
          All_Importance <- Validation_Importance
        } else if(!is.null(Test_Importance) && is.null(Validation_Importance) && is.null(Train_Importance)) {
          All_Importance <- Test_Importance
        } else {
          All_Importance <- NULL
        }

      } else {
        All_Importance <- Test_Importance
      }

      # Variable Importance
      if(data.table::is.data.table(All_Importance)) {
        data.table::setorderv(x = All_Importance, cols = names(All_Importance)[2L], order = -1, na.last = TRUE)
        OutputList[["Variable Importance"]] <- reactable::reactable(
          data = All_Importance,
          compact = TRUE,
          defaultPageSize = 10,
          wrap = TRUE,
          filterable = TRUE,
          fullWidth = TRUE,
          highlight = TRUE,
          pagination = TRUE,
          resizable = TRUE,
          searchable = TRUE,
          selection = "multiple",
          showPagination = TRUE,
          showSortable = TRUE,
          showSortIcon = TRUE,
          sortable = TRUE,
          striped = TRUE,
          theme = reactable::reactableTheme(
            color = FontColor$flv,
            backgroundColor = "#4f4f4f26",
            borderColor = "#dfe2e5",
            stripedColor = "#4f4f4f8f",
            highlightColor = "#8989898f",
            cellPadding = "8px 12px",
            style = list(
              fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
            ),
            searchInputStyle = list(width = "100%")
          )
        )
      }

      # Interaction Importance Table
      if(data.table::is.data.table(All_Interaction)) {
        OutputList[["Interaction Importance"]] <- reactable::reactable(
          data = All_Interaction,
          compact = TRUE,
          defaultPageSize = 10,
          wrap = TRUE,
          filterable = TRUE,
          fullWidth = TRUE,
          highlight = TRUE,
          pagination = TRUE,
          resizable = TRUE,
          searchable = TRUE,
          selection = "multiple",
          showPagination = TRUE,
          showSortable = TRUE,
          showSortIcon = TRUE,
          sortable = TRUE,
          striped = TRUE,
          theme = reactable::reactableTheme(
            color = FontColor$flv,
            backgroundColor = "#4f4f4f26",
            borderColor = "#dfe2e5",
            stripedColor = "#4f4f4f8f",
            highlightColor = "#8989898f",
            cellPadding = "8px 12px",
            style = list(
              fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"
            ),
            searchInputStyle = list(width = "100%")
          )
        )
      }
    }

    if(Debug) print("ML Reports 9")

    # Evaluation Plots ----
    if("Evaluation Plots" %in% MLOutputSelection) {

      if(Debug) print("ML Reports 9.1")

      ### Test
      OutputList[["TestData Calibration Plot"]] <- AutoPlots::Plot.Calibration.Line(
        dt = TestData,
        AggMethod = "mean",
        XVar = 'Predict',
        YVar = TargetColumnName,
        GroupVar = GroupVariableInclude,
        YVarTrans = "Identity",
        XVarTrans = "Identity",
        FacetRows = 1,
        FacetCols = 1,
        FacetLevels = NULL,
        NumberBins = 20,
        Height = PlotHeighta,
        Width = PlotWidtha,
        Title = "Calibration Line Plot",
        ShowLabels = FALSE,
        Title.YAxis = TargetColumnName,
        Title.XAxis = "Predict",
        EchartsTheme = Theme,
        TimeLine = FALSE,
        X_Scroll = TRUE,
        Y_Scroll = TRUE,
        TextColor = "white",
        Debug = Debug)

      if(Debug) print("ML Reports 9.2")

      ### Train
      if(!is.null(TrainData) && TrainData[,.N] > 100L) {
        OutputList[["TrainData Calibration Plot"]] <- AutoPlots::Plot.Calibration.Line(
          dt = TrainData,
          AggMethod = "mean",
          XVar = 'Predict',
          YVar = TargetColumnName,
          GroupVar = GroupVariableInclude,
          YVarTrans = "Identity",
          XVarTrans = "Identity",
          FacetRows = 1,
          FacetCols = 1,
          FacetLevels = NULL,
          NumberBins = 20,
          Height = PlotHeighta,
          Width = PlotWidtha,
          Title = "Calibration Line Plot",
          ShowLabels = FALSE,
          Title.YAxis = TargetColumnName,
          Title.XAxis = "Predict",
          EchartsTheme = Theme,
          TimeLine = FALSE,
          X_Scroll = TRUE,
          Y_Scroll = TRUE,
          TextColor = "white",
          Debug = Debug)
      }

      if(Debug) print("ML Reports 9.3")

      ## ROC
      OutputList[["TestData ROC Plot"]] <- AutoPlots::Plot.ROC(
        dt = TestData,
        AggMethod = "mean",
        SampleSize = 30000,
        XVar = "Predict",
        YVar = TargetColumnName,
        GroupVar = GroupVariableInclude,
        YVarTrans = "Identity",
        XVarTrans = "Identity",
        FacetRows = 1,
        FacetCols = 1,
        FacetLevels = NULL,
        Height = PlotHeighta,
        Width = PlotWidtha,
        Title = "ROC Plot",
        ShowLabels = FALSE,
        Title.YAxis = "True Positive Rate",
        Title.XAxis = "1 - False Positive Rate",
        EchartsTheme = Theme,
        TimeLine = FALSE,
        X_Scroll = TRUE,
        Y_Scroll = TRUE,
        TextColor = "white",
        Debug = Debug)

      if(Debug) print("ML Reports 9.4")

      if(length(TrainData) > 0L) {
        OutputList[["TrainData ROC Plot"]] <- AutoPlots::Plot.ROC(
          dt = TrainData,
          AggMethod = "mean",
          SampleSize = 30000,
          XVar = "Predict",
          YVar = TargetColumnName,
          GroupVar = GroupVariableInclude,
          YVarTrans = "Identity",
          XVarTrans = "Identity",
          FacetRows = 1,
          FacetCols = 1,
          FacetLevels = NULL,
          Height = PlotHeighta,
          Width = PlotWidtha,
          Title = "ROC Plot",
          ShowLabels = FALSE,
          Title.YAxis = "True Positive Rate",
          Title.XAxis = "1 - False Positive Rate",
          EchartsTheme = Theme,
          TimeLine = FALSE,
          X_Scroll = TRUE,
          Y_Scroll = TRUE,
          TextColor = "white",
          Debug = Debug)
      }

      if(Debug) print("ML Reports 9.5")

      ## Lift
      OutputList[["TestData Lift Plot"]] <- AutoPlots::Plot.Lift(
        dt = TestData,
        PreAgg = FALSE,
        XVar = "Predict",
        YVar = TargetColumnName,
        GroupVar = GroupVariableInclude,
        YVarTrans = "Identity",
        XVarTrans = "Identity",
        FacetRows = 1,
        FacetCols = 1,
        FacetLevels = NULL,
        Height = PlotHeighta,
        Width = PlotWidtha,
        Title = "Lift Plot",
        ShowLabels = FALSE,
        Title.YAxis = "Lift",
        Title.XAxis = "% Positive Classified",
        EchartsTheme = Theme,
        TimeLine = FALSE,
        X_Scroll = TRUE,
        Y_Scroll = TRUE,
        TextColor = "white",
        Debug = Debug)

      if(Debug) print("ML Reports 9.6")

      if(length(TrainData) > 0L) {
        OutputList[["TrainData Lift Plot"]] <- AutoPlots::Plot.Lift(
          dt = TrainData,
          PreAgg = TRUE,
          XVar = "Predict",
          YVar = TargetColumnName,
          ZVar = NULL,
          GroupVar = GroupVariableInclude,
          YVarTrans = "Identity",
          XVarTrans = "Identity",
          FacetRows = 1,
          FacetCols = 1,
          FacetLevels = NULL,
          Height = PlotHeighta,
          Width = PlotWidtha,
          Title = "Lift Plot",
          ShowLabels = FALSE,
          Title.YAxis = "Lift",
          Title.XAxis = "% Positive Classified",
          EchartsTheme = Theme,
          TimeLine = FALSE,
          X_Scroll = TRUE,
          Y_Scroll = TRUE,
          TextColor = "white",
          Debug = Debug)
      }

      if(Debug) print("ML Reports 9.7")

      ## Gains
      OutputList[["TestData Gains Plot"]] <- AutoPlots::Plot.Gains(
        dt = TestData,
        PreAgg = FALSE,
        XVar = "Predict",
        YVar = TargetColumnName,
        GroupVar = GroupVariableInclude,
        YVarTrans = "Identity",
        XVarTrans = "Identity",
        FacetRows = 1,
        FacetCols = 1,
        FacetLevels = NULL,
        Height = PlotHeighta,
        Width = PlotWidtha,
        Title = "Gains Plot",
        ShowLabels = FALSE,
        Title.YAxis = TargetColumnName,
        Title.XAxis = g,
        EchartsTheme = Theme,
        TimeLine = FALSE,
        X_Scroll = TRUE,
        Y_Scroll = TRUE,
        TextColor = "white",
        Debug = Debug)

      if(Debug) print("ML Reports 9.8")

      if(length(TrainData) > 0L) {
        OutputList[["TrainData Gains Plot"]] <- AutoPlots::Plot.Gains(
          dt = TrainData,
          PreAgg = TRUE,
          XVar = "Predict",
          YVar = TargetColumnName,
          ZVar = NULL,
          GroupVar = GroupVariableInclude,
          YVarTrans = "Identity",
          XVarTrans = "Identity",
          FacetRows = 1,
          FacetCols = 1,
          FacetLevels = NULL,
          Height = PlotHeighta,
          Width = PlotWidtha,
          Title = "Gains Plot",
          ShowLabels = FALSE,
          Title.YAxis = TargetColumnName,
          Title.XAxis = "Predict",
          EchartsTheme = Theme,
          TimeLine = FALSE,
          X_Scroll = TRUE,
          Y_Scroll = TRUE,
          TextColor = "white",
          Debug = Debug)
      }
    }

    if(Debug) print("ML Reports 10")

    # Model Interpretation ----
    if("Partial Dependence Plots" %in% MLOutputSelection) {

      if(Debug) print("ML Reports 10.1")

      # Add Plots
      if(!is.null(TestData) && !is.null(PDPVariables)) {
        if(Debug) print("ML Reports 10.2")
        for(g in PDPVariables) {
          if(Debug) print("ML Reports 10.22")
          if(is.numeric(TestData[[g]])) {
            OutputList[[paste0('TestData Partial Dependence Line Plot: ', g)]] <- AutoPlots::Plot.PartialDependence.Line(
              dt = TestData,
              XVar = g,
              YVar = TargetColumnName,
              ZVar = 'Predict',
              YVarTrans = "Identity",
              XVarTrans = "Identity",
              ZVarTrans = "Identity",
              FacetRows = 1,
              FacetCols = 1,
              FacetLevels = NULL,
              GroupVar = GroupVariableInclude,
              NumberBins = 20,
              AggMethod = "mean",
              Height = PlotHeighta,
              Width = PlotWidtha,
              Title = "Partial Dependence Line",
              ShowLabels = FALSE,
              Title.YAxis = TargetColumnName,
              Title.XAxis = g,
              EchartsTheme = Theme,
              TimeLine = FALSE,
              X_Scroll = TRUE,
              Y_Scroll = TRUE,
              TextColor = "white",
              Debug = Debug)
          }
        }
      }

      # Add Plots
      if(!is.null(TrainData) && TrainData[,.N] > 100L && !is.null(PDPVariables)) {
        if(Debug) print("ML Reports 10.3")
        for(g in PDPVariables) {
          if(Debug) print("ML Reports 10.33")
          if(is.numeric(TrainData[[g]])) {
            OutputList[[paste0('TrainData Partial Dependence Line Plot: ', g)]] <- AutoPlots::Plot.PartialDependence.Line(
              dt = TrainData,
              XVar = g,
              YVar = TargetColumnName,
              ZVar = 'Predict',
              YVarTrans = "Identity",
              XVarTrans = "Identity",
              ZVarTrans = "Identity",
              FacetRows = 1,
              FacetCols = 1,
              FacetLevels = NULL,
              GroupVar = GroupVariableInclude,
              NumberBins = 20,
              AggMethod = "mean",
              Height = PlotHeighta,
              Width = PlotWidtha,
              Title = "Partial Dependence Line",
              ShowLabels = FALSE,
              Title.YAxis = TargetColumnName,
              Title.XAxis = g,
              EchartsTheme = Theme,
              TimeLine = FALSE,
              X_Scroll = TRUE,
              Y_Scroll = TRUE,
              TextColor = "white",
              Debug = Debug)
          }
        }
      }

      # Add Plots
      if(!is.null(TestData) && !is.null(PDPVariables)) {
        if(Debug) print("ML Reports 10.4")
        for(g in PDPVariables) {
          if(Debug) print("ML Reports 10.44")
          if(!is.numeric(TestData[[g]])) {
            OutputList[[paste0('TestData Partial Dependence Heatmap: ', g)]] <- AutoPlots::Plot.PartialDependence.HeatMap(
              dt = TestData,
              XVar = g,
              YVar = TargetColumnName,
              ZVar = 'Predict',
              YVarTrans = "Identity",
              XVarTrans = "Identity",
              ZVarTrans = "Identity",
              FacetRows = 1,
              FacetCols = 1,
              FacetLevels = NULL,
              GroupVar = GroupVariableInclude,
              NumberBins = 20,
              AggMethod = "mean",
              Height = PlotHeighta,
              Width = PlotWidtha,
              Title = "Partial Dependence Line",
              ShowLabels = FALSE,
              Title.YAxis = TargetColumnName,
              Title.XAxis = g,
              EchartsTheme = Theme,
              TimeLine = FALSE,
              X_Scroll = TRUE,
              Y_Scroll = TRUE,
              TextColor = "white",
              Debug = Debug)
          }
        }
      }

      # Add Plots
      if(!is.null(TrainData) && TrainData[,.N] > 100L && !is.null(PDPVariables)) {
        if(Debug) print("ML Reports 10.5")
        for(g in PDPVariables) {
          if(Debug) print("ML Reports 10.55")
          if(!is.numeric(TrainData[[g]])) {
            OutputList[[paste0('TrainData Partial Dependence Heatmap: ', g)]] <- AutoPlots::Plot.PartialDependence.HeatMap(
              dt = TrainData,
              XVar = g,
              YVar = TargetColumnName,
              ZVar = 'Predict',
              YVarTrans = "Identity",
              XVarTrans = "Identity",
              ZVarTrans = "Identity",
              FacetRows = 1,
              FacetCols = 1,
              FacetLevels = NULL,
              GroupVar = GroupVariableInclude,
              NumberBins = 20,
              AggMethod = "mean",
              Height = PlotHeighta,
              Width = PlotWidtha,
              Title = "Partial Dependence Line",
              ShowLabels = FALSE,
              Title.YAxis = TargetColumnName,
              Title.XAxis = g,
              EchartsTheme = Theme,
              TimeLine = FALSE,
              X_Scroll = TRUE,
              Y_Scroll = TRUE,
              TextColor = "white",
              Debug = Debug)
          }
        }
      }

    }
  }

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Return                                                                    ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  returnList <- list()
  returnList[["OutputList"]] <- OutputList
  returnList[["DataList"]] <- DataList
  returnList[["CodeList"]] <- CodeList
  return(returnList)

  # ----

}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# ML Scoring                                                                                 ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
#' @title Shiny.ML.Trainer
#'
#' @description ML Trainer
#'
#' @author Adrian Antico
#' @family ML
#'
#' @param input shiny input
#' @param output shiny output
#' @param DataList DataList stores data in app
#' @param ArgsList ArgsList
#' @param CodeList CodeList from app
#' @param ModelOutputList from app
#' @param TT TargetType
#' @param ML_ExperimentTable Collection table
#' @param run Cross Validation Iteration Number in app
#' @param n Total Cross Validation Iterations in app
#' @param Debug Debug from app
#' @param Algo 'CatBoost', 'XGBoost', 'LightGBM', 'H2O_DRF', 'H2O_GBM'
#'
#' @return a list of columns names by data type
#'
#' @export
Shiny.ML.Scoring <- function(input,
                             output,
                             DataList = NULL,
                             CodeList = NULL,
                             MOL = NULL,
                             Debug = FALSE) {

  if(Debug) print("Shiny.ML.Scoring 1")

  # Data & Model
  ModelName <- Quantico::ReturnParam(xx = tryCatch({input$ScoreML_Model}, error = function(x) NULL), Type = "character", Default = NULL)
  if(Debug) {
    print("Shiny.ML.Scoring 1.1")
    print(ModelName)
  }
  DataName <- Quantico::ReturnParam(xx = tryCatch({input$ScoreML_Data}, error = function(x) NULL), Type = "character", Default = NULL)
  if(Debug) {
    print("Shiny.ML.Scoring 1.2")
    print(DataName)
  }
  ReturnFeats <- Quantico::ReturnParam(xx = tryCatch({input$ScoreML_ReturnFeatures}, error = function(x) NULL), Type = "logical", Default = FALSE)
  if(Debug) {
    print("Shiny.ML.Scoring 1.3")
    print(ReturnFeats)
  }
  ReturnShap <- Quantico::ReturnParam(xx = tryCatch({input$ScoreML_ReturnShapVals}, error = function(x) NULL), Type = "logical", Default = FALSE)
  if(Debug) {
    print(ReturnShap)
  }

  # Score Models
  if(length(ModelName) > 0L && length(DataName) > 0L) {

    if(Debug) print("Shiny.ML.Scoring 3")

    # Args Collection
    SData <- data.table::copy(DataList[[DataName]][['data']])
    if(Debug) print(head(SData))
    ArgsList <- MOL[[ModelName]][["ArgsList"]]
    Algo <- ArgsList[["Algo"]]
    TT <- ArgsList[["TargetType"]]
    TransObj <- MOL[[ModelName]]$TransformationResults
    if(length(TransObj) == 0L) TranNum <- FALSE else TranNum <- TRUE
    TarColName <- MOL[[ModelName]]$ArgsList$TargetColumnName
    FeatColNames <- MOL[[ModelName]]$ArgsList$FeatureColNames
    idcols <- names(SData)[!which(names(SData) %in% FeatColNames)]
    FactLevelList <- MOL[[ModelName]]$FactorLevelsList
    ModObj <- MOL[[ModelName]]$Model
    MCTargetLevels <- MOL[[ModelName]]$ArgsList$TargetLevels

    if(Debug) {
      print("Shiny.ML.Scoring 4")
      print(ModObj)
    }
    if(TT == "Binary Classification") TT <- "Classification"

    # Score Data
    if(tolower(Algo) == "catboost") {

      # Score Data
      DataList[[paste0(ModelName, "_ScoredData")]][['data']] <- AutoQuant::AutoCatBoostScoring(
        MultiClassTargetLevels = MCTargetLevels,
        TargetType = TT,
        ScoringData = data.table::copy(SData),
        FeatureColumnNames = FeatColNames,
        FactorLevelsList = FactLevelList,
        IDcols = idcols,
        OneHot = FALSE,
        ReturnShapValues = ReturnShap,
        ModelObject = ModObj,
        ModelPath = NULL, # only needed for loading from file
        ModelID = NULL, # only needed for loading from file
        ReturnFeatures = ReturnFeats,
        TransformNumeric = TranNum,
        BackTransNumeric = if(TranNum) TRUE else FALSE,
        TargetColumnName = TarColName,
        TransformationObject = TransObj,
        TransID = NULL, # only needed for loading from file
        TransPath = NULL, # only needed for loading from file
        MDP_Impute = FALSE,
        MDP_CharToFactor = FALSE,
        MDP_RemoveDates = FALSE,
        MDP_MissFactor = '0',
        MDP_MissNum = -1,
        RemoveModel = FALSE,
        Debug = Debug)

      # Code Print
      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# CatBoost ML Scoring\n",
        "DataList[[", Quantico:::CEP(paste0(ModelName, "_ScoredData")), "]][['data']] <- AutoQuant::AutoCatBoostScoring(\n  ",
        "MultiClassTargetLevels = ", Quantico:::ExpandText(MCTargetLevels), ",\n  ",
        "TargetType = ", Quantico:::CEP(TT), ",\n  ",
        "ScoringData = data.table::copy(DataList[[data.table::copy(DataList[[", Quantico:::CEP(DataName), "]][['data']])), ]])\n  ",
        "FeatureColumnNames = ", Quantico:::ExpandText(FeatColNames), ",\n  ",
        "FactorLevelsList = ModelOutputList[[", Quantico:::CEP(ModelName), "]]$FactorLevelsList,\n  ",
        "IDcols = ", Quantico:::ExpandText(idcols), ",\n  ",
        "ReturnShapValues = ", Quantico:::CEPP(ReturnShap), ",\n  ",
        "ModelObject = ModelOutputList[[", Quantico:::CEP(ModelName), "]]$Model,\n  ",
        "ReturnFeatures = ", Quantico:::CEPP(ReturnFeats), ",\n  ",
        "TransformNumeric = ", Quantico:::CEPP(TranNum), ",\n  ",
        "BackTransNumeric = ", Quantico:::CEPP(if(TranNum) TRUE else FALSE), ",\n  ",
        "TargetColumnName = ", Quantico:::CEP(TarColName), ",\n  ",
        "TransformationObject = ModelOutputList[[", Quantico:::CEP(ModelName), "]]$TransformationResults,\n  ",
        "MDP_Impute = FALSE,\n  ",
        "MDP_CharToFactor = FALSE,\n  ",
        "MDP_RemoveDates = FALSE,\n  ",
        "MDP_MissFactor = '0',\n  ",
        "MDP_MissNum = -1,\n  ",
        "RemoveModel = FALSE)\n"))}, error = function(x) CodeList)

    } else if(tolower(Algo) == "xgboost") {

      # Score Data
      if(Debug) for(asdfasfdasdfasfdasfasdf in 1:10) print(MCTargetLevels)
      DataList[[paste0(ModelName, "_ScoredData")]][['data']] <- AutoQuant::AutoXGBoostScoring(
        EncodingMethod = FactLevelList$EncodingMethod,
        TargetLevels = MCTargetLevels,
        TargetType = TT,
        ScoringData = data.table::copy(SData),
        FeatureColumnNames = FeatColNames,
        FactorLevelsList = FactLevelList,
        IDcols = idcols,
        OneHot = FALSE,
        ReturnShapValues = ReturnShap,
        ModelObject = ModObj,
        ModelPath = NULL, # only needed for loading from file
        ModelID = NULL, # only needed for loading from file
        ReturnFeatures = ReturnFeats,
        TransformNumeric = TranNum,
        BackTransNumeric = if(TranNum) TRUE else FALSE,
        TargetColumnName = TarColName,
        TransformationObject = TransObj,
        TransID = NULL, # only needed for loading from file
        TransPath = NULL, # only needed for loading from file
        MDP_Impute = FALSE,
        MDP_CharToFactor = FALSE,
        MDP_RemoveDates = FALSE,
        MDP_MissFactor = '0',
        MDP_MissNum = -1,
        Debug = Debug)

      # Code Print
      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# XGBoost ML Scoring\n",
        "DataList[[", Quantico:::CEP(paste0(ModelName, "_ScoredData")), "]][['data']] <- AutoQuant::AutoCatBoostScoring(\n  ",
        "EncodingMethod = ModelOutputList[[", Quantico:::CEP(ModelName), "]]$FactorLevelsList$EncodingMethod,\n  ",
        "TargetLevels = ", Quantico:::ExpandText(MCTargetLevels), ",\n  ",
        "TargetType = ", Quantico:::CEP(TT), ",\n  ",
        "ScoringData = data.table::copy(DataList[[data.table::copy(DataList[[", Quantico:::CEP(DataName), "]][['data']])), ]])\n  ",
        "FeatureColumnNames = ", Quantico:::ExpandText(FeatColNames), ",\n  ",
        "FactorLevelsList = ModelOutputList[[", Quantico:::CEP(ModelName), "]]$FactorLevelsList,\n  ",
        "IDcols = ", Quantico:::ExpandText(idcols), ",\n  ",
        "ReturnShapValues = ", Quantico:::CEPP(ReturnShap), ",\n  ",
        "ModelObject = ModelOutputList[[", Quantico:::CEP(ModelName), "]]$Model,\n  ",
        "ReturnFeatures = ", Quantico:::CEPP(ReturnFeats), ",\n  ",
        "TransformNumeric = ", Quantico:::CEPP(TranNum), ",\n  ",
        "BackTransNumeric = ", Quantico:::CEPP(if(TranNum) TRUE else FALSE), ",\n  ",
        "TargetColumnName = ", Quantico:::CEP(TarColName), ",\n  ",
        "TransformationObject = ModelOutputList[[", Quantico:::CEP(ModelName), "]]$TransformationResults,\n  ",
        "MDP_Impute = FALSE,\n  ",
        "MDP_CharToFactor = FALSE,\n  ",
        "MDP_RemoveDates = FALSE,\n  ",
        "MDP_MissFactor = '0',\n  ",
        "MDP_MissNum = -1,\n  ",
        "RemoveModel = FALSE)\n"))}, error = function(x) CodeList)

    } else if(tolower(Algo) == "lightgbm") {

      # Score Data
      DataList[[paste0(ModelName, "_ScoredData")]][['data']] <- AutoQuant::AutoLightGBMScoring(
        EncodingMethod = FactLevelList$EncodingMethod,
        TargetLevels = MCTargetLevels,
        TargetType = TT,
        ScoringData = data.table::copy(SData),
        FeatureColumnNames = FeatColNames,
        FactorLevelsList = FactLevelList,
        IDcols = idcols,
        OneHot = FALSE,
        ReturnShapValues = ReturnShap,
        ModelObject = ModObj,
        ModelPath = NULL, # only needed for loading from file
        ModelID = NULL, # only needed for loading from file
        ReturnFeatures = ReturnFeats,
        TransformNumeric = TranNum,
        BackTransNumeric = if(TranNum) TRUE else FALSE,
        TargetColumnName = TarColName,
        TransformationObject = TransObj,
        TransID = NULL, # only needed for loading from file
        TransPath = NULL, # only needed for loading from file
        MDP_Impute = FALSE,
        MDP_CharToFactor = FALSE,
        MDP_RemoveDates = FALSE,
        MDP_MissFactor = '0',
        MDP_MissNum = -1,
        Debug = Debug)

      # Code Print
      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# LightGBM ML Scoring\n",
        "DataList[[", Quantico:::CEP(paste0(ModelName, "_ScoredData")), "]][['data']] <- AutoQuant::AutoCatBoostScoring(\n  ",
        "EncodingMethod = ModelOutputList[[", Quantico:::CEP(ModelName), "]]$FactorLevelsList$EncodingMethod,\n  ",
        "TargetLevels = ", Quantico:::ExpandText(MCTargetLevels), ",\n  ",
        "TargetType = ", Quantico:::CEP(TT), ",\n  ",
        "ScoringData = data.table::copy(DataList[[data.table::copy(DataList[[", Quantico:::CEP(DataName), "]][['data']])), ]])\n  ",
        "FeatureColumnNames = ", Quantico:::ExpandText(FeatColNames), ",\n  ",
        "FactorLevelsList = ModelOutputList[[", Quantico:::CEP(ModelName), "]]$FactorLevelsList,\n  ",
        "IDcols = ", Quantico:::ExpandText(idcols), ",\n  ",
        "ReturnShapValues = ", Quantico:::CEPP(ReturnShap), ",\n  ",
        "ModelObject = ModelOutputList[[", Quantico:::CEP(ModelName), "]]$Model,\n  ",
        "ReturnFeatures = ", Quantico:::CEPP(ReturnFeats), ",\n  ",
        "TransformNumeric = ", Quantico:::CEPP(TranNum), ",\n  ",
        "BackTransNumeric = ", Quantico:::CEPP(if(TranNum) TRUE else FALSE), ",\n  ",
        "TargetColumnName = ", Quantico:::CEP(TarColName), ",\n  ",
        "TransformationObject = ModelOutputList[[", Quantico:::CEP(ModelName), "]]$TransformationResults,\n  ",
        "MDP_Impute = FALSE,\n  ",
        "MDP_CharToFactor = FALSE,\n  ",
        "MDP_RemoveDates = FALSE,\n  ",
        "MDP_MissFactor = '0',\n  ",
        "MDP_MissNum = -1,\n  ",
        "RemoveModel = FALSE)\n"))}, error = function(x) CodeList)

    } else if(tolower(Algo) %in% c("h2odrf","h2ogbm","h2oglm")) {

      # Score Data
      if(Debug) {
        print("# Score Data")
        print(ModelName)
      }
      ModelName <- gsub(pattern = ".rds", replacement = "", ModelName)
      DataList[[paste0(ModelName, "_ScoredData")]][['data']] <- AutoH2OMLScoring(
        ScoringData = data.table::copy(SData),
        ModelObject = ModObj,
        ModelType = "mojo",
        H2OShutdown = TRUE,
        H2OStartUp = TRUE,
        MaxMem = {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),"G")},
        NThreads = max(1, parallel::detectCores()-2),
        JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',
        ModelPath = ArgsList[["model_path"]],
        ModelID = ModelName,
        ReturnFeatures = ReturnFeats,
        TransformNumeric = TranNum,
        BackTransNumeric = if(TranNum) TRUE else FALSE,
        TargetColumnName = TarColName,
        TransformationObject = TransObj,
        TransID = NULL, # only needed for loading from file
        TransPath = NULL, # only needed for loading from file
        MDP_Impute = TRUE,
        MDP_CharToFactor = TRUE,
        MDP_RemoveDates = TRUE,
        MDP_MissFactor = "0",
        MDP_MissNum = -1,
        Debug = Debug)

      # Code Print
      mem <- {gc();paste0(as.character(floor(as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo", intern=TRUE)) / 1000000)),'G')}
      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# H2O ML Scoring\n",
        "DataList[[", Quantico:::CEP(paste0(ModelName, "_ScoredData")), "]][['data']] <- AutoQuant::AutoCatBoostScoring(\n  ",
        "H2OShutdown = TRUE,\n  ",
        "H2OStartUp = TRUE,\n  ",
        "MaxMem = ", Quantico:::CEP(mem),"\n  ",
        "NThreads = ", Quantico:::CEPP(max(1, parallel::detectCores()-2)), ",\n  ",
        "JavaOptions = '-Xmx1g -XX:ReservedCodeCacheSize=256m',\n  ",
        "ScoringData = data.table::copy(DataList[[data.table::copy(DataList[[", Quantico:::CEP(DataName), "]][['data']])), ]])\n  ",
        "ModelObject = ModelOutputList[[", Quantico:::CEP(ModelName), "]]$Model,\n  ",
        "ReturnFeatures = ", Quantico:::CEPP(ReturnFeats), ",\n  ",
        "TransformNumeric = ", Quantico:::CEPP(TranNum), ",\n  ",
        "BackTransNumeric = ", Quantico:::CEPP(if(TranNum) TRUE else FALSE), ",\n  ",
        "TargetColumnName = ", Quantico:::CEP(TarColName), ",\n  ",
        "TransformationObject = ModelOutputList[[", Quantico:::CEP(ModelName), "]]$TransformationResults,\n  ",
        "MDP_Impute = FALSE,\n  ",
        "MDP_CharToFactor = FALSE,\n  ",
        "MDP_RemoveDates = FALSE,\n  ",
        "MDP_MissFactor = '0',\n  ",
        "MDP_MissNum = -1,\n  ",
        "RemoveModel = FALSE)\n"))}, error = function(x) CodeList)
    }

    # Return
    if(Debug) print("Shiny.ML.Scoring 5")
    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))
  }
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Mixed Effects                                                                              ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title EffectsInterval
#'
#' @description Random Effects with confidence endpoints
#'
#' @author Adrian Antico
#' @family Mixed Effects
#'
#' @param Model lmer and brms models
#'
#' @export
EffectsInterval <- function(Model) {
  data.table::setDT(mixedup::extract_random_effects(Model))
}

#' @title EffectsInterval
#'
#' @description Random Effects Prediction Intervals: random effect, upper and lower 95th Percentile. For various Model predictions, possibly with new data
#'
#' @author Adrian Antico
#' @family Mixed Effects
#'
#' @param Model Output from AutoQuant:: supervised learning functions
#'
#' @export
PredictionIntervals <- function(Model) {
  data.table::setDT(merTools::predictInterval(Model)) # for various Model predictions, possibly with new data
}

#' @title REsim
#'
#' @description Mixture modeling random effects simulation data generator. Rebuild of merTools::REsim. 8x speedup.
#'
#' @author Adrian Antico
#' @family Mixed Effects
#'
#' @param Model Model <- lme4::lmer(gpa ~ occasion + (1 | student), data = gpa)
#' @param n.sims = 200
#' @param oddsRatio = FALSE
#' @param seed = NULL
#'
#' @examples
#' \dontrun{
#'
#' # Load Data
#' # https://m-clark.github.io/mixed-models-with-R/random_intercepts.html#fnref10
#' gpa <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/gpa.csv")
#' Model = lme4::lmer(gpa ~ occasion + (1 | student), data = gpa)
#' summary(Model)
#'
#' # Code
#' # paste0(as.character(Model@call), collapse = " ")
#' head(lme4::ranef(Model),5)
#' mixedup::extract_random_effects(Model)
#'
#' microbenchmark::microbenchmark(
#'   times = 10,
#'   merTools::REsim(Model),
#'   Quantico::REsim(Model))
#' }
#'
#' @export
REsim <- function(Model,
                  n.sims = 200,
                  oddsRatio = FALSE,
                  seed = NULL) {

  if(!is.null(seed)) set.seed(seed) else if (!exists(".Random.seed", envir = .GlobalEnv)) runif(1)
  mysim <- arm::sim(Model, n.sims = n.sims)
  reDims <- length(mysim@ranef)
  tmp.out <- vector("list", reDims)
  names(tmp.out) <- names(mysim@ranef)
  # i = 1
  for(i in seq_len(reDims)) {
    zed <- data.table::setDT(as.data.frame(mysim@ranef[[i]]))
    zed[, ID := seq_len(.N)]
    zed <- data.table::melt.data.table(data = zed, id.vars = 'ID', measure.vars = names(zed)[seq_len(ncol(zed)-1)], variable.name = 'RandomEffect', value.name = 'Intercept')
    zed <- zed[, list(mean = mean(Intercept, na.rm = TRUE), median = median(Intercept, na.rm = TRUE), sd = sd(Intercept, na.rm = TRUE)), by = 'RandomEffect'][, RandomEffect := NULL]
    zed[, groupID := rep(dimnames(mysim@ranef[[i]])[[2L]], length(dimnames(mysim@ranef[[i]])[[3L]]))]
    zed[, term := rep(dimnames(mysim@ranef[[i]])[[3L]], each = length(dimnames(mysim@ranef[[i]])[[2L]]))]
    tmp.out[[i]] <- zed
    tmp.out[[i]][, ":=" (groupFctr = names(tmp.out)[i], groupID = as.character(groupID), term = as.character(term))]
  }
  dat <- data.table::rbindlist(tmp.out)
  dat <- dat[, .SD, .SDcols = c("groupFctr", "groupID", "term", "mean", "median", "sd")]
  if(oddsRatio == TRUE) {
    return(dat[, ":=" (median = exp(median), mean = exp(mean), sd = NA)])
  } else {
    return(dat)
  }
}

#' @title MEOW
#'
#' @description Mixed Effects Offset Weighting
#'
#' @author Adrian Antico
#' @family Mixed Effects
#'
#' @param data = NULL
#' @param TargetVariable = NULL
#' @param TargetType = 'regression', 'classification', 'multiclass'
#' @param RandomEffects = NULL
#' @param FixedEffects = NULL
#' @param Nest Specify the outer and inner variable numbers. Function checks for successive numbers (1,2), (2,3), ... (N-1,N).  c(1,2)
#' @param CollapseEPV CollapseEPV == TRUE gives you a constant EPV, otherwise it remains varied
#' @param KeepSubStats = FALSE
#' @param Lmer = FALSE,
#' @param LmerType = 'add',
#' @param PolyN NULL. Add a numeric value to use poly(FixedEffect, PolyN) in the mixed effects adjustment for fixed effects. Only allowed when length(FixedEffects) == 1
#' @param Debug = FALSE
#'
#' @examples
#' \dontrun{
#' # Pull data and set vars to factors
#' data <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/gpa.csv"); data[, student := as.factor(student)];data[, semester := as.factor(semester)]
#' data[, year := as.factor(year)]
#' data[, student := as.factor(student)]
#' data[, semester := as.factor(semester)]
#' data[, Mean := mean(gpa), by = c('sex','semester','year','student')]
#'
#' # Run function
#' TestModel <- AutoQuant::AutoCatBoostRegression(
#'
#'   # GPU or CPU and the number of available GPUs
#'   task_type = 'GPU', NumGPUs = 1,
#'   OutputSelection = c('Importances', 'EvalMetrics', 'Score_TrainData'),
#'   ReturnModelObjects = TRUE,
#'
#'   # Data args
#'   data = data, model_path = getwd(),
#'   TargetColumnName = 'gpa',
#'   FeatureColNames = c('sex','semester','year'),
#'   IDcols = setdiff(names(data), c('sex','semester','year','gpa')),
#'
#'   # ML args
#'   Trees = 250, RandomStrength = 1, RSM = 1, GrowPolicy = 'SymmetricTree',
#'   Depth = 9,
#'   L2_Leaf_Reg = 10,
#'   model_size_reg = 2)
#'
#' Output <- Quantico::MEOW(
#'   data = TestModel$TrainData,
#'   TargetType = 'regression',
#'   TargetVariable = TestModel$ArgsList$TargetColumnName,
#'   RandomEffects = 'student', # c('sex','semester','year','student'),
#'   FixedEffects = 'Predict',
#'   Nest = NULL,
#'   CollapseEPV = TRUE,
#'   KeepSubStats = FALSE,
#'   Lmer = FALSE,
#'   LmerType = 'add',
#'   PolyN = NULL,
#'   Debug = FALSE)
#'
#' Output <- Quantico::MEOW(
#'   data = Output$data,
#'   TargetType = 'regression',
#'   TargetVariable = TestModel$ArgsList$TargetColumnName,
#'   RandomEffects = 'student', # c('sex','semester','year','student'),
#'   FixedEffects = 'Predict',
#'   Nest = NULL,
#'   CollapseEPV = TRUE,
#'   KeepSubStats = FALSE,
#'   Lmer = TRUE,
#'   LmerType = 'add',
#'   PolyN = NULL,
#'   Debug = FALSE)
#'
#' }
#'
#' @export
MEOW <- function(data = NULL,
                 TargetType = 'regression',
                 TargetVariable = NULL,
                 RandomEffects = NULL,
                 FixedEffects = NULL,
                 Lmer = FALSE,
                 LmerType = 'add',
                 PolyN = NULL,
                 Nest = NULL,
                 CollapseEPV = TRUE,
                 KeepSubStats = FALSE,
                 Debug = FALSE) {

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Rename variables to reduce run times. Switch back at end
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  library(collapse); library(magrittr); dataNames <- data.table::copy(names(data))
  if(length(RandomEffects) > 1L) {
    old <- c(TargetVariable, RandomEffects); new <- c('TargetVar', paste0('RE', seq_along(RandomEffects)))
    data.table::setnames(data, old, new)
    GroupVarsNew <- RandomEffects
    RandomEffectsOld <- RandomEffects
    RandomEffects <- paste0("RE", seq_along(RandomEffects))
    old <- c('TargetVar', paste0("RE", seq_along(RandomEffects)), paste0('RE', seq_along(RandomEffects), "_MixedEffects"))
    new <- c(TargetVariable, GroupVarsNew, paste0(GroupVarsNew, "_MixedEffects"))
  } else {
    old <- c(TargetVariable, RandomEffects); new <- c('TargetVar', paste0('RE', seq_along(RandomEffects)))
    data.table::setnames(data, old, new)
    GroupVarsNew <- RandomEffects
    RandomEffectsOld <- RandomEffects
    RandomEffects <- paste0("RE", seq_along(RandomEffects))
    old <- c('TargetVar', paste0("RE", seq_along(RandomEffects)), paste0('RE', seq_along(RandomEffects), "_MixedEffects"))
    new <- c(TargetVariable, GroupVarsNew, paste0(GroupVarsNew, "_MixedEffects"))
  }

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Nested Random Effects
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  # Build Nested Random Effects
  for(i in seq_along(RandomEffects)) {

    # i = 1    i = 2
    # 1. Prior Stats; # 2. Posterior Stats; # 3. Merge prior to data (data holds posterior stats)
    if(i == 1L) {

      # Prior Stats ----
      data.table::setkey(x = data, cols = RE1)
      if(tolower(TargetType) == 'classification') {
        DT <- list()
        DT[['RE1']] <- collapse::funique(data$RE1)
        DT[['RE1_Mean']] <- (data %>% gby(RE1) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE1_N']] <- (data %>% gby(RE1) %>% gv('TargetVar') %>% fnobs)[[2L]]
        GroupMean <- data.table::setkey(data.table::setDT(DT), RE1)
        rm(DT); gc()
        GroupMean[, RE1_EPV := RE1_Mean * (1 - RE1_Mean) / RE1_N]
      } else if(tolower(TargetType) == 'regression') {
        DT <- list()
        DT[['RE1']] <- collapse::funique(data$RE1)
        DT[['RE1_Mean']] <- (data %>% gby(RE1) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE1_N']] <- (data %>% gby(RE1) %>% gv('TargetVar') %>% fnobs)[[2L]]
        DT[['RE1_EPV']] <- (data %>% gby(RE1) %>% gv('TargetVar') %>% fvar)[[2L]]
        GroupMean <- data.table::setkey(data.table::setDT(DT), RE1)
        rm(DT); gc()
      } else if(tolower(TargetType) == 'multiclass') {
        GroupMean <- data[, list(RE1_N = .N), by = c('TargetVar', "RE1")]
        GroupMean[, GrandSum := sum(RE1_N)]
        GroupMean[, TargetSum := sum(RE1_N), by = 'TargetVar']
        GroupMean[, TargetMean := TargetSum / GrandSum]
        GroupMean[, TargetGroupMean := N / TargetSum]
        GroupMean[, TargetVariance := TargetMean * (1 - TargetMean) / TargetSum]
        GroupMean[, TargetGroupVariance := TargetGroupMean * (1 - TargetGroupMean) / RE1_N]
        GroupMean[, Z := TargetGroupVariance / (TargetGroupVariance + TargetVariance)]
        GroupMean[, RE1_MixedEffects := (1 - Z) * TargetGroupMean + Z * TargetMean]
        GroupMean[, (setdiff(names(GroupMean), c("RE1_MixedEffects", 'TargetVar', 'RE1'))) := NULL]
        GroupMean <- data.table::dcast.data.table(data = GroupMean, formula = RE1 ~ get(TargetVariable), fun.aggregate = sum, value.var = "RE1_MixedEffects", fill = 0)
        data.table::setnames(x = GroupMean, names(GroupMean), c('RE1', paste0("RE1_MixedEffects_TargetLevel_", names(GroupMean)[-1L])))
        data.table::setkeyv(GroupMean, cols = 'RE1')
      }

      # Merge prior ----
      x <- data %>% gv('TargetVar') %>% fmean # 4x faster than GroupMean[, GrandMean := mean(data$TargetVar, na.rm = TRUE)]
      GroupMean[, RE1_GrandMean := x]; rm(x)

      if(CollapseEPV) {
        GroupMean[, RE1_EPV := collapse::fmean(RE1_EPV, na.rm = TRUE)]
      }

      # Posterior
      GroupMean[, RE1_VHM := collapse::fsum((RE1_Mean - RE1_GrandMean) ^ 2) / (.N-1) - RE1_EPV / RE1_N]
      data.table::set(GroupMean, i = which(GroupMean[['RE1_VHM']] < 0), j = 'RE1_VHM', value = 0)
      GroupMean[, RE1_K := RE1_EPV / (RE1_VHM + 1)]
      GroupMean[, RE1_Z := RE1_N / (RE1_N + RE1_K + 1)]
      GroupMean[, paste0(RandomEffects[i], "_MixedEffects") := RE1_Z * RE1_Mean + (1 - RE1_Z) * RE1_GrandMean]
      if(KeepSubStats && tolower(TargetType) != 'multiclass') {
        nam <- names(GroupMean)[-1L]
        data[GroupMean, paste0(nam) := mget(paste0("i.", nam))]
      } else if(tolower(TargetType) %in% c('regression','classification')) {
        data[GroupMean, RE1_MixedEffects := i.RE1_MixedEffects]
      } else {
        data[GroupMean, eval(names(GroupMean)[!names(GroupMean) %chin% 'RE1']) := mget(paste0("i.", names(GroupMean)[!names(GroupMean) %chin% 'RE1']))]
      }

    } else if(i == 2L) {

      # Prior Stats ----
      GrandMean <- data %>% gby(RE2) %>% gv('RE1_MixedEffects') %>% fmean
      data.table::setkeyv(GrandMean, 'RE2')
      data.table::setkeyv(x = data, cols = 'RE2')
      data.table::setnames(GrandMean, 'RE1_MixedEffects', 'RE2_GrandMean')

      # Posterior Stats ----
      DT <- list()
      DT[['RE2']] <- funique(data[['RE2']])
      if(all(c(1,2) %in% Nest)) {
        DT[['RE2_Mean']] <- ((data %>% gby(RE1,RE2) %>% gv('TargetVar') %>% fmean) %>% gby(RE2) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE2_N']] <- ((data %>% gby(RE1,RE2) %>% gv('TargetVar') %>% fnobs) %>% gby(RE2) %>% gv('TargetVar') %>% fnobs)[[2L]]
        if(TargetType == 'classification') {
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE2)
          rm(DT); gc()
          GroupMean[, RE2_EPV := RE2_Mean * (1 - RE2_Mean) / (RE2_N + 0.01)]
        } else {
          DT[['RE2_EPV']] <- ((data %>% gby(RE1,RE2) %>% gv('TargetVar') %>% fmean) %>% gby(RE2) %>% gv('TargetVar') %>% fvar)[[2L]]
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE2)
          rm(DT); gc()
        }

      } else {

        DT[['RE2_Mean']] <- (data %>% gby(RE2) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE2_N']] <- (data %>% gby(RE2) %>% gv('TargetVar') %>% fnobs)[[2L]]
        if(TargetType == 'classification') {
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE2)
          rm(DT); gc()
          GroupMean[, RE2_EPV := RE2_Mean * (1 - RE2_Mean) / (RE2_N + 1)]
        } else {
          DT[['RE2_EPV']] <- (data %>% gby(RE2) %>% gv('TargetVar') %>% fvar)[[2L]]
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE2)
          rm(DT); gc()
        }
      }

      # Merge Prior to data ----
      GroupMean[GrandMean, RE2_GrandMean := i.RE2_GrandMean]

      if(CollapseEPV) {
        GroupMean[, RE2_EPV := collapse::fmean(RE2_EPV, na.rm = TRUE)]
      }

      GroupMean[, RE2_VHM := collapse::fsum((RE2_Mean - RE2_GrandMean) ^ 2) / (.N-1) - RE2_EPV / (RE2_N + 1)]
      data.table::set(GroupMean, i = which(GroupMean[['RE2_VHM']] < 0), j = 'RE2_VHM', value = 0)
      GroupMean[, RE2_K := RE2_EPV / RE2_VHM]
      GroupMean[, RE2_Z := RE2_N / (RE2_N + RE2_K + 1)]
      GroupMean[, paste0(RandomEffects[i], "_MixedEffects") := RE2_Z * RE2_Mean + (1 - RE2_Z) * RE2_GrandMean]
      if(KeepSubStats) {
        nam <- names(GroupMean)[-1L]
        data[GroupMean, paste0(nam) := mget(paste0("i.", nam))]
      } else {
        data[GroupMean, RE2_MixedEffects := i.RE2_MixedEffects]
      }

    } else if(i == 3L) {

      # Prior Stats ----
      GrandMean <- data %>% gby(RE3) %>% gv('RE2_MixedEffects') %>% fmean
      data.table::setkey(GrandMean, RE3)
      data.table::setnames(GrandMean, 'RE2_MixedEffects', 'RE3_GrandMean')
      data.table::setkey(x = data, cols = RE3)

      # Posterior Stats
      DT <- list()
      DT[['RE3']] <- unique(data$RE3)
      if(all(c(2,3) %in% Nest)) {
        DT[['RE3_Mean']] <- ((data %>% gby(RE2,RE3) %>% gv('TargetVar') %>% fmean) %>% gby(RE3) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE3_N']] <- ((data %>% gby(RE2,RE3) %>% gv('TargetVar') %>% fnobs) %>% gby(RE3) %>% gv('TargetVar') %>% fnobs)[[2L]]
        if(TargetType == 'classification') {
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE3)
          rm(DT); gc()
          GroupMean[, RE3_EPV := RE3_Mean * (1 - RE3_Mean) / RE3_N]
        } else {
          DT[['RE3_EPV']] <- ((data %>% gby(RE2,RE3) %>% gv('TargetVar') %>% fmean) %>% gby(RE3) %>% gv('TargetVar') %>% fvar)[[2L]]
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE3)
          rm(DT); gc()
        }
      } else {
        DT[['RE3_Mean']] <- (data %>% gby(RE3) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE3_N']] <- (data %>% gby(RE3) %>% gv('TargetVar') %>% fnobs)[[2L]]
        if(TargetType == 'classification') {
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE3)
          rm(DT); gc()
          GroupMean[, RE3_EPV := RE3_Mean * (1 - RE3_Mean) / RE3_N]
        } else {
          DT[['RE3_EPV']] <- (data %>% gby(RE3) %>% gv('TargetVar') %>% fvar)[[2L]]
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE3)
          rm(DT); gc()
        }
      }

      # Merge Prior to data ----
      GroupMean[GrandMean, RE3_GrandMean := i.RE3_GrandMean]

      if(CollapseEPV) {
        GroupMean[, RE3_EPV := collapse::fmean(RE3_EPV, na.rm = TRUE)]
      }

      # Set to zero if negative
      GroupMean[, RE3_VHM := collapse::fsum((RE3_Mean - RE3_GrandMean) ^ 2) / (.N-1) - RE3_EPV / RE3_N]
      data.table::set(GroupMean, i = which(GroupMean[['RE3_VHM']] < 0), j = 'RE3_VHM', value = 0)
      GroupMean[, RE3_K := RE3_EPV / RE3_VHM]
      GroupMean[, RE3_Z := RE3_N / (RE3_N + RE3_K)]
      GroupMean[, paste0(RandomEffects[i], "_MixedEffects") := RE3_Z * RE3_Mean + (1 - RE3_Z) * RE3_GrandMean]
      if(KeepSubStats) {
        nam <- names(GroupMean)[-1L]
        data[GroupMean, paste0(nam) := mget(paste0("i.", nam))]
      } else {
        data[GroupMean, RE3_MixedEffects := i.RE3_MixedEffects]
      }

    } else if(i == 4L) {

      # Prior Stats ----
      GrandMean <- data %>% gby(RE4) %>% gv('RE3_MixedEffects') %>% fmean
      data.table::setkey(GrandMean, RE4)
      data.table::setnames(GrandMean, 'RE3_MixedEffects', 'RE4_GrandMean')
      data.table::setkey(x = data, cols = RE4)

      # Posterior Stats
      DT <- list()
      DT[['RE4']] <- unique(data$RE4)

      if(all(c(3,4) %in% Nest)) {
        DT[['RE4_Mean']] <- ((data %>% gby(RE3,RE4) %>% gv('TargetVar') %>% fmean) %>% gby(RE4) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE4_N']] <- ((data %>% gby(RE3,RE4) %>% gv('TargetVar') %>% fnobs) %>% gby(RE4) %>% gv('TargetVar') %>% fnobs)[[2L]]
        if(TargetType == 'classification') {
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE4)
          rm(DT); gc()
          GroupMean[, RE4_EPV := RE4_Mean * (1 - RE4_Mean) / RE4_N]
        } else {
          DT[['RE4_EPV']] <- ((data %>% gby(RE3,RE4) %>% gv('TargetVar') %>% fmean) %>% gby(RE4) %>% gv('TargetVar') %>% fvar)[[2L]]
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE4)
          rm(DT); gc()
        }

      } else {

        DT[['RE4_Mean']] <- (data %>% gby(RE4) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE4_N']] <- (data %>% gby(RE4) %>% gv('TargetVar') %>% fnobs)[[2L]]
        if(TargetType == 'classification') {
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE4)
          rm(DT); gc()
          GroupMean[, RE4_EPV := RE4_Mean * (1 - RE4_Mean) / RE4_N]
        } else {
          DT[['RE4_EPV']] <- (data %>% gby(RE4) %>% gv('TargetVar') %>% fvar)[[2L]]
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE4)
          rm(DT); gc()
        }
      }

      # Merge Prior to data ----
      GroupMean[GrandMean, RE4_GrandMean := i.RE4_GrandMean]

      if(CollapseEPV) {
        GroupMean[, RE4_EPV := collapse::fmean(RE4_EPV, na.rm = TRUE)]
      }

      # Set to zero if negative
      GroupMean[, RE4_VHM := collapse::fsum((RE4_Mean - RE4_GrandMean) ^ 2) / (.N-1) - RE4_EPV / RE4_N]
      data.table::set(GroupMean, i = which(GroupMean[['RE4_VHM']] < 0), j = 'RE4_VHM', value = 0)
      GroupMean[, RE4_K := RE4_EPV / RE4_VHM]
      GroupMean[, RE4_Z := RE4_N / (RE4_N + RE4_K)]
      GroupMean[, paste0(RandomEffects[i], "_MixedEffects") := RE4_Z * RE4_Mean + (1 - RE4_Z) * RE4_GrandMean]
      if(KeepSubStats) {
        nam <- names(GroupMean)[-1L]
        data[GroupMean, paste0(nam) := mget(paste0("i.", nam))]
      } else {
        data[GroupMean, RE4_MixedEffects := i.RE4_MixedEffects]
      }

    } else if(i == 5L) {

      # Prior Stats ----
      GrandMean <- data %>% gby(RE5) %>% gv('RE4_MixedEffects') %>% fmean
      data.table::setkey(GrandMean, RE5)
      data.table::setnames(GrandMean, 'RE4_MixedEffects', 'RE5_GrandMean')
      data.table::setkey(x = data, cols = RE5)

      # Posterior Stats
      DT <- list()
      DT[['RE5']] <- unique(data$RE3)

      if(all(c(4,5) %in% Nest)) {
        DT[['RE5_Mean']] <- ((data %>% gby(RE4,RE5) %>% gv('TargetVar') %>% fmean) %>% gby(RE5) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE5_N']] <- ((data %>% gby(RE4,RE5) %>% gv('TargetVar') %>% fnobs) %>% gby(RE5) %>% gv('TargetVar') %>% fnobs)[[2L]]
        if(TargetType == 'classification') {
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE5)
          rm(DT); gc()
          GroupMean[, RE5_EPV := RE5_Mean * (1 - RE5_Mean) / RE5_N]
        } else {
          DT[['RE5_EPV']] <- ((data %>% gby(RE4,RE5) %>% gv('TargetVar') %>% fmean) %>% gby(RE5) %>% gv('TargetVar') %>% fvar)[[2L]]
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE5)
          rm(DT); gc()
        }
      } else {
        DT[['RE5_Mean']] <- (data %>% gby(RE5) %>% gv('TargetVar') %>% fmean)[[2L]]
        DT[['RE5_N']] <- (data %>% gby(RE5) %>% gv('TargetVar') %>% fnobs)[[2L]]
        if(TargetType == 'classification') {
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE5)
          rm(DT); gc()
          GroupMean[, RE5_EPV := RE5_Mean * (1 - RE5_Mean) / RE5_N]
        } else {
          DT[['RE5_EPV']] <- (data %>% gby(RE5) %>% gv('TargetVar') %>% fvar)[[2L]]
          GroupMean <- data.table::setkey(data.table::setDT(DT), RE5)
          rm(DT); gc()
        }
      }

      # Merge Prior to data ----
      GroupMean[GrandMean, RE5_GrandMean := i.RE5_GrandMean]

      if(CollapseEPV) {
        GroupMean[, RE5_EPV := collapse::fmean(RE5_EPV, na.rm = TRUE)]
      }

      # Set to zero if negative
      GroupMean[, RE5_VHM := collapse::fsum((RE5_Mean - RE5_GrandMean) ^ 2) / (.N-1) - RE5_EPV / RE5_N]
      data.table::set(GroupMean, i = which(GroupMean[['RE5_VHM']] < 0), j = 'RE2_VHM', value = 0)
      GroupMean[, RE5_K := RE5_EPV / RE5_VHM]
      GroupMean[, RE5_Z := RE5_N / (RE5_N + RE5_K)]
      GroupMean[, paste0(RandomEffects[i], "_MixedEffects") := RE5_Z * RE5_Mean + (1 - RE5_Z) * RE5_GrandMean]
      if(KeepSubStats) {
        nam <- names(GroupMean)[-1L]
        data[GroupMean, paste0(nam) := mget(paste0("i.", nam))]
      } else {
        data[GroupMean, RE5_MixedEffects := i.RE5_MixedEffects]
      }
    }
  }

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Revert back to original names
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  data.table::setnames(data, 'TargetVar', TargetVariable)
  # i = 1  i = 2
  for(i in seq_along(GroupVarsNew)) {
    data.table::setnames(
      data,
      names(data)[which(names(data) %like% paste0('RE', i))],
      gsub(pattern = paste0('RE', i), replacement = GroupVarsNew[i], x = names(data)[which(names(data) %like% paste0('RE', i))]))
  }

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Fixed Effects Time
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  if(length(FixedEffects) > 0L && Lmer) {
    Output <- Quantico::LMER(data, TargetVariable = TargetVariable, FixedEffects = FixedEffects, RandomEffects = RandomEffectsOld, Type = LmerType, Poly = PolyN, Family = if(TargetType == 'regression') 'gaussian' else 'binomial')
    return(Output)
  } else if(length(FixedEffects) > 0L && !Lmer) {
    if(length(PolyN) == 1L) {
      data[, paste0(TargetVariable, "_MMPredict") := lm(as.formula(paste0(TargetVariable, " ~ poly(", FixedEffects, ") + ", paste0("offset(", RandomEffectsOld[length(RandomEffectsOld)], "_MixedEffects)"))), data = data)$fitted.values]
    } else {
      data[, paste0(TargetVariable, "_MMPredict") := lm(as.formula(paste0(TargetVariable, " ~ ", paste0(FixedEffects, collapse =  " + "), " + ", paste0("offset(", RandomEffectsOld[length(RandomEffectsOld)], "_MixedEffects)"), collapse = " ")), data = data)$fitted.values]
    }
    return(list(
      data = data,
      DensityPlot = function(MeasureVariables) Quantico::Plot.Density(data = data, GroupVariables = RandomEffectsOld, MeasureVars = MeasureVariables)
    ))
  } else {
    FactorLevelsList <- data[, mean(get(paste0(RandomEffectsOld[length(RandomEffectsOld)], "_MixedEffects"))), by = c(RandomEffectsOld)]
    data.table::setnames(FactorLevelsList, 'V1', paste0(RandomEffectsOld[length(RandomEffectsOld)], "_MixedEffects"))
    return(list(data = data, ComponentList = FactorLevelsList))
  }
}

#' @title FastLM
#'
#' @description LM with coefficients greater than or equal to zero if Constrained is TRUE. Otherwise, a fast lm coefficient set is returned from armadillo
#'
#' @author Adrian Antico
#' @family Mixed Effects
#'
#' @param data data.table
#' @param yvar target variable name
#' @param xvar x variable names
#'
#' @export
FastLM <- function(data,
                   yvar = NULL,
                   xvar = NULL,
                   Constrained = TRUE) {
  if(Constrained) {
    solution <- matrix(as.matrix(x = data[, .SD, .SDcols = c(xvar)]), nrow = 1200L, ncol = length(xvar))
    Y <- matrix(data[[yvar]])
    Rinv <- solve(chol(t(solution) %*% solution));
    C <- cbind(rep(1,length(xvar)), diag(length(xvar)))
    b <- c(1,rep(0,length(xvar)))
    d <- t(Y) %*% solution
    return(quadprog::solve.QP(Dmat = Rinv, factorized = TRUE, dvec = d, Amat = C, bvec = b, meq = 1))
  } else {
    return(unlist(RcppArmadillo::fastLmPure(y = data[[yvar]], X = Matrix::as.matrix(data[, .SD, .SDcols = c(xvar)], ncol = length(ncol(data)))))[seq_along(xvar)])
  }
}

#' @title LMER
#'
#' @description LM with coefficients greater than or equal to zero if Constrained is TRUE. Otherwise, a fast lm coefficient set is returned from armadillo
#'
#' @author Adrian Antico
#' @family Mixed Effects
#'
#' @param data data.table
#' @param TargetVariable target variable name
#' @param RandomEffects NULL. Must include at least one variable
#' @param FixedEffects NULL. Optional
#' @param Poly NULL. If set to a numeric value, then polynomial terms will be used for the Fixed Effects. Used for when the FixedEffect is a Prediction output and a single value.
#' @param Family = 'gaussian' or 'binomial'
#'
#' @details
#' Up to 5 random effects variables can be included.
#'
#' There are
#'
#' @examples
#' FixedEffects <- 'Predict'; RandomEffects <- 'Factor_1', TargetVariable <- 'TargetVar'
#'
#' @export
LMER <- function(data,
                 TargetVariable = NULL,
                 FixedEffects = NULL,
                 RandomEffects = NULL,
                 Type = 'add',
                 Poly = NULL,
                 Family = 'gaussian') {

  if(length(RandomEffects) == 1L) {

    # y = (1 | var1)
    if(length(Poly) > 0L && length(FixedEffects) == 1L) {
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ poly(", FixedEffects, ", ", Poly, ") + (1 | ", RandomEffects[1L], ")")))
    } else {
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", FixedEffects, " + (1 | ", RandomEffects[1L], ")")))
    }

    if(Family == 'gaussian') {
      model_lmer <- lme4::lmer(formula = x, data = data)
    } else {
      model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
    }

    data[, paste0(RandomEffects[1L], "_Lmer") := predict(model_lmer)]

  } else if(length(RandomEffects) == 2L) {

    if(Type == 'add') {

      #     y =  (1 | var1) + (1 | var2)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 | ", RandomEffects[1L],") + (1 | ", RandomEffects[2L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[2L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'add_nest') {

      #     y =  (1 | var1) + (var1 | var2)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 | ", RandomEffects[1L],") + (", RandomEffects[1], " | ", RandomEffects[2L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[2L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'int_nest') {

      #     y =  (1 + var1 | var2)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 + ", RandomEffects[1], " | ", RandomEffects[2L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[2L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'nest') {

      #     y =  (var1 | var2)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(", RandomEffects[1], " | ", RandomEffects[2L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[2L], "_Lmer") := predict(model_lmer)]

    }

  } else if(length(RandomEffects) == 3L) {

    if(Type == 'add') {

      #     y =  (1 | var1) + (1 | var2) + (1 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 | ", RandomEffects[1L],") + (1 | ", RandomEffects[2L],") + (1 | ", RandomEffects[3L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[3L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'add_nest') {

      #     y =  (1 | var1) + (var1 | var2) + (var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 | ", RandomEffects[1L],") + (", RandomEffects[1], " | ", RandomEffects[2L],") + (", RandomEffects[2L], " | ", RandomEffects[3L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[3L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'int_nest') {

      #     y =  (1 + var1 | var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 + ", RandomEffects[1], " | ", RandomEffects[2L], " | ", RandomEffects[3L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[3L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'nest') {

      #     y =  (var1 | var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(", RandomEffects[1], " | ", RandomEffects[2L], " | ", RandomEffects[3L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[3L], "_Lmer") := predict(model_lmer)]
    }

  } else if(length(RandomEffects) == 4L) {

    if(Type == 'add') {

      #     y =  (1 | var1) + (1 | var2) + (1 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 | ", RandomEffects[1L],") + (1 | ", RandomEffects[2L],") + (1 | ", RandomEffects[3L],") + (1 | ", RandomEffects[4L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[4L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'add_nest') {

      #     y =  (1 | var1) + (var1 | var2) + (var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 | ", RandomEffects[1L],") + (", RandomEffects[1], " | ", RandomEffects[2L],") + (", RandomEffects[2L], " | ", RandomEffects[3L],") + (", RandomEffects[3L]," | ", RandomEffects[4L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[4L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'int_nest') {

      #     y =  (1 + var1 | var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 + ", RandomEffects[1], " | ", RandomEffects[2L], " | ", RandomEffects[3L], " | ", RandomEffects[4L], ")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[4L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'nest') {

      #     y =  (var1 | var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(", RandomEffects[1], " | ", RandomEffects[2L], " | ", RandomEffects[3L], " | ", RandomEffects[4L], ")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[4L], "_Lmer") := predict(model_lmer)]
    }

  } else if(length(RandomEffects) == 5L) {

    if(Type == 'add') {

      #     y =  (1 | var1) + (1 | var2) + (1 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 | ", RandomEffects[1L],") + (1 | ", RandomEffects[2L],") + (1 | ", RandomEffects[3L],") + (1 | ", RandomEffects[4L],") + (1 | ", RandomEffects[5L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[5L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'add_nest') {

      #     y =  (1 | var1) + (var1 | var2) + (var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 | ", RandomEffects[1L],") + (", RandomEffects[1], " | ", RandomEffects[2L],") + (", RandomEffects[2L], " | ", RandomEffects[3L],") + (", RandomEffects[3L]," | ", RandomEffects[4L],") + (", RandomEffects[4L]," | ", RandomEffects[5L],")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[5L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'int_nest') {

      #     y =  (1 + var1 | var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(1 + ", RandomEffects[1], " | ", RandomEffects[2L], " | ", RandomEffects[3L], " | ", RandomEffects[4L], " | ", RandomEffects[5L], ")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[5L], "_Lmer") := predict(model_lmer)]

    } else if(Type == 'nest') {

      #     y =  (var1 | var2 | var3)
      x <- suppressWarnings(as.formula(paste0("formula = ", TargetVariable, " ~ ", paste0(FixedEffects, collapse = " + "), "(", RandomEffects[1], " | ", RandomEffects[2L], " | ", RandomEffects[3L], " | ", RandomEffects[4L], " | ", RandomEffects[5L], ")")))
      if(Family == 'gaussian') {
        model_lmer <- lme4::lmer(formula = x, data = data)
      } else {
        model_lmer <- lme4::glmer(formula = x, data = data, family = Family)
      }
      data[, paste0(RandomEffects[5L], "_Lmer") := predict(model_lmer)]
    }

  }

  # Return data and model
  return(list(
    data = data,
    Model = model_lmer,
    EffectsInterval = function() EffectsInterval(Model = model_lmer)[],
    PredictionInterval = function() suppressWarnings(Quantico::PredictionIntervals(Model = model_lmer))[],
    DensityPlot = function(MeasureVariables) Quantico::Plot.Density(data, RandomEffects, MeasureVars = MeasureVariables)))
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Panel Forecasting                                                                          ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title FC.XREGS
#'
#' @description Management of xregs either generated, user supplied, or modified
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param dt data a data.table
#' @param tv TargetColumnName
#' @param dv DateColumnName
#' @param fcp PC_Periods
#' @param tu TimeUnit
#' @param gv GroupVariables
#' @param xrs XREGS a data.table
#' @param em EncodingMethod
#' @param wd working directory
#' @param db DebugMode
#'
#' @return a list of columns names by data type
#'
#' @export
FC.XREGS <- function(dt,
                     tv,
                     dv,
                     fcp,
                     tu,
                     gv = NULL,
                     xrs = NULL,
                     em = NULL,
                     wd = NULL,
                     db = FALSE) {

  # Has Group Variables > 1
  if(length(gv) > 1L) {

    # Create xrs is length == 0
    # otherwise, ensure it has same exact attributes as the created version
    if(length(xrs) == 0L) {

      # QA values
      # dt = data
      # tv = TargetColumnName
      # dv = DateColumnName
      # fcp = FC_Periods
      # tu = TimeUnit[1L]
      # gv = GroupVariables
      # xrs = XREGS
      # em = 'MEOW'
      # wd = "C:/Users/Bizon/Documents"
      # db = FALSE

      ## create xrs
      xrs_future <- AutoQuant:::FutureTimePeriods(UpdateData. = dt, TimeUnit. = tu, DateColumnName. = dv, FC_Periods = fcp, GroupVariables. = gv, SkipPeriods = NULL)
      xrs_future[, eval(tv) := NA_real_]

      # Iterate through group var combinations (minus the first one: the full interaction)

      # 2 Group Variables
      # 0. Default: all 2 interacted or nested
      # 1. 2
      if(length(gv) == 2L) {
        combos <- list(
          runs = 1L,
          vars1 = gv[length(gv)])
      }

      # 3 Group Variables
      # 0. Default: all 3 interacted or nested
      # 1. 1 & 3
      # 2. 2 & 3
      # 3. 3
      if(length(gv) == 3L) {
        combos <- list(
          runs = 3L,
          vars1 = gv[c(1L,3L)],
          vars2 = gv[2L:3L],
          vars3 = gv[length(gv)])
      }

      # 4 Group Variables
      # 0. Default: all 4 interacted or nested
      # 1. 1 & 3 & 4
      # 2. 1 & 4
      # 3. 2 & 3 & 4
      # 4. 2 & 4
      # 5. 3 & 4
      # 6. 4
      if(length(gv) == 4L) {
        combos <- list(
          runs = 6L,
          vars1 = gv[c(1L,3L:4L)],
          vars2 = gv[c(1L,4L)],
          vars3 = gv[2L:4L],
          vars4 = gv[c(2L,4L)],
          vars5 = gv[3L:4L],
          vars6 = gv[length(gv)])
      }

      # 5 Group Variables
      # 0. Default: all 5 interacted or nested
      # 1.  1 & 3 & 4 & 5
      # 2.  1 & 4 & 5
      # 3.  1 & 5
      # 4.  2 & 3 & 4 & 5
      # 5.  2 & 4 & 5
      # 6.  2 & 5
      # 7.  3 & 4 & 5
      # 8.  3 & 5
      # 9.  4 & 5
      # 10. 5
      if(length(gv) == 5L) {
        combos <- list(
          runs = 10L, # gv[1L:5L] = default CARMA GroupVar
          vars1  = gv[c(1L,3L:5L)],
          vars2  = gv[c(1L,4L:5L)],
          vars3  = gv[c(1L,5L)],
          vars4  = gv[2L:5L],
          vars5  = gv[c(2L,4L:5L)],
          vars6  = gv[c(2L,5L)],
          vars7  = gv[3L:5L],
          vars8  = gv[c(3L,5L)],
          vars8  = gv[4L:5L],
          vars10 = gv[length(gv)])
      }

      # Build meows
      # comb = 1; comb = 2; comb = 3
      iter <- 1L
      for(comb in seq_len(combos$runs)) {

        ## categorical encoding:
        ### Note: full interaction of them is managed already by the carma function
        ###       thus, we only need to capture the remaining combinations
        Output <- Rodeo:::EncodeCharacterVariables(
          EncodeMethod = em,
          TrainData = dt,
          Debug = db,
          ValidationData = xrs_future,
          TargetVariableName = tv,
          CategoricalVariableNames = combos[[comb+1]],
          MetaDataPath = wd,
          RunMode = 'train', ModelType = 'regression', TestData = NULL,
          KeepCategoricalVariables = TRUE, ReturnMetaData = FALSE,
          MetaDataList = NULL, ImputeMissingValue = FALSE)

        # QA values
        # RunMode = 'train'
        # ModelType = 'regression'
        # TrainData = dt
        # ValidationData = xrs_future
        # TestData = NULL
        # TargetVariableName = tv
        # CategoricalVariableNames = combos[[comb+1]]
        # EncodeMethod = em
        # KeepCategoricalVariables = TRUE
        # ReturnMetaData = FALSE
        # MetaDataPath = wd
        # MetaDataList = NULL
        # ImputeMissingValue = FALSE
        # DebugFC = db
        # if(RunMode != 'train') Score <- TRUE else Score <- FALSE

        # Collect output
        if(comb == 1L) {

          dt <- Output$TrainData
          Final <- data.table::rbindlist(list(dt, Output$ValidationData))
          nam <- names(Final)[ncol(Final)]
          data.table::setnames(dt, old = nam, new = paste0(paste0(combos[[comb+1L]], collapse = '_'), "_MixedEffects"))
          data.table::setnames(Final, old = nam, new = paste0(paste0(combos[[comb+1L]], collapse = '_'), "_MixedEffects"))

          iter <- iter + 1L

        } else if(comb != combos$runs) {

          kk <- paste0('vars',comb)

          dt <- Output$TrainData
          temp <- data.table::rbindlist(list(dt, Output$ValidationData))
          temp <- unique(temp[, .SD, .SDcols = c(combos[[comb+1L]], names(temp)[ncol(temp)])])
          data.table::setkeyv(x = temp, cols = c(combos[[kk]]))
          data.table::setkeyv(x = Final, cols = c(combos[[kk]]))
          Final[temp, paste0(names(temp)[ncol(temp)]) := get(paste0("i.", names(temp)[ncol(temp)]))]#, allow.cartesian = TRUE]

          nam <- names(Final)[ncol(Final)]
          data.table::setnames(dt, old = nam, new = paste0(paste0(combos[[comb+1L]], collapse = '_'), "_MixedEffects"))
          data.table::setnames(Final, old = nam, new = paste0(paste0(combos[[comb+1L]], collapse = '_'), "_MixedEffects"))

          iter <- iter + 1L

        } else {

          kk <- paste0('vars',comb)

          dt <- Output$TrainData
          temp <- data.table::rbindlist(list(dt, Output$ValidationData))
          temp <- unique(temp[, .SD, .SDcols = c(combos[[comb+1L]], names(temp)[ncol(temp)])])
          data.table::setkeyv(x = temp, cols = c(combos[[kk]]))
          data.table::setkeyv(x = Final, cols = c(combos[[kk]]))
          Final[temp, paste0(names(temp)[ncol(temp)]) := get(paste0("i.", names(temp)[ncol(temp)]))]#, allow.cartesian = TRUE]

        }

      }

      # Add GroupVar
      if(length(gv) > 0L) if(!'GroupVar' %chin% names(F)) Final[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(gv)]

      # Remove Group Variables from XREGS
      if(any(gv %in% names(Final))) {
        for(i in gv) if(i %in% names(Final)) data.table::set(Final, j = i, value = NULL)
      }

      # Remove Target Variable from XREGS
      if(tv %in% names(Final)) data.table::set(Final, j = tv, value = NULL)

      # Return
      return(list(data = dt, XREGS = Final))

    } else {

      # Add GroupVar
      if(length(gv) > 0L) if(!'GroupVar' %chin% names(xrs)) xrs[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(gv)]

      # Remove Group Variables from XREGS
      if(length(gv) > 0L && any(gv %in% names(xrs))) {
        for(i in gv) if(i %in% names(xrs)) data.table::set(xrs, j = i, value = NULL)
      }

      # Remove Target Variable from XREGS
      if(tv %in% names(xrs)) data.table::set(xrs, j = tv, value = NULL)

      # Simply return artifacts back to user
      return(list(data = dt, XREGS = xrs))
    }

  } else {

    if(length(xrs) > 0L) {

      # Add GroupVar
      if(length(gv) > 0L) if(!'GroupVar' %chin% names(xrs)) xrs[, GroupVar := do.call(paste, c(.SD, sep = ' ')), .SDcols = c(gv)]

      # Remove Group Variables from XREGS
      if(length(gv) > 0L && any(gv %in% names(xrs))) {
        for(i in gv) if(i %in% names(xrs)) data.table::set(xrs, j = i, value = NULL)
      }

      # Remove Target Variable from XREGS
      if(tv %in% names(xrs)) data.table::set(xrs, j = tv, value = NULL)

      # Return
      return(list(data = dt, XREGS = xrs))

    } else {

      # Return
      return(list(data = dt, XREGS = xrs))
    }
  }
}

#' @title FC.TimeUnit
#'
#' @description Forecasting TimeUnit conversion
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param ArgsList List
#'
#' @export
FC.TimeUnit <- function(ArgsList) {
  if(ArgsList[['TimeUnit']] %in% c("1-Minute","5-Minutes","10-Minutes","15-Minutes","30-Minutes","Hourly")) {
    if(ArgsList[['TimeUnit']] %in% "1-Minute") {
      ArgsList[['TimeUnit']] <- '1min'
    }
    if(ArgsList[['TimeUnit']] %in% "5-Minutes") {
      ArgsList[['TimeUnit']] <- '5min'
    }
    if(ArgsList[['TimeUnit']] %in% "10-Minutes") {
      ArgsList[['TimeUnit']] <- '10min'
    }
    if(ArgsList[['TimeUnit']] %in% "15-Minutes") {
      ArgsList[['TimeUnit']] <- '15min'
    }
    if(ArgsList[['TimeUnit']] %in% "30-Minutes") {
      ArgsList[['TimeUnit']] <- '30min'
    }
    if(ArgsList[['TimeUnit']] %in% "Hourly") {
      ArgsList[['TimeUnit']] <- 'hour'
    }
  } else {
    if(ArgsList[['TimeUnit']] %in% "Daily") {
      ArgsList[['TimeUnit']] <- 'day'
    }
    if(ArgsList[['TimeUnit']] %in% "Weekly") {
      ArgsList[['TimeUnit']] <- 'week'
    }
    if(ArgsList[['TimeUnit']] %in% "Monthly") {
      ArgsList[['TimeUnit']] <- 'month'
    }
    if(ArgsList[['TimeUnit']] %in% "Quarterly") {
      ArgsList[['TimeUnit']] <- 'quarter'
    }
    if(ArgsList[['TimeUnit']] %in% "Yearly") {
      ArgsList[['TimeUnit']] <- 'year'
    }
  }
  return(ArgsList)
}

#' @title FC.Backtest.CalendarGroups
#'
#' @description Forecasting TimeUnit expansion for backtesting eval rollups
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param ArgsList List
#'
#' @export
FC.Backtest.CalendarGroups <- function(ArgsList) {

  # 1min
  if(ArgsList$TimeUnit[[1L]] %in% c('1min')) {
    if(ArgsList$FC_Periods >= 360 * 24 * 60) {
      x <- c('hour','wday','wom','month','quarter','year')
    } else if(ArgsList$FC_Periods >= 90 * 24 * 60) {
      x <- c('hour','wday','wom','month','quarter')
    } else if(ArgsList$FC_Periods >= 60 * 24 * 60) {
      x <- c('hour','wday','wom','month')
    } else if(ArgsList$FC_Periods >= 30 * 24 * 60) {
      x <- c('hour','wday','wom')
    } else if(ArgsList$FC_Periods >= 7 * 24 * 60) {
      x <- c('hour','wday')
    } else {
      x <- c('hour')
    }
    return(x)
  }

  # 5min
  if(ArgsList$TimeUnit[[1L]] %in% c('5min')) {
    if(ArgsList$FC_Periods >= 360 * 24 * 12) {
      x <- c('hour','wday','wom','month','quarter','year')
    } else if(ArgsList$FC_Periods >= 90 * 24 * 12) {
      x <- c('hour','wday','wom','month','quarter')
    } else if(ArgsList$FC_Periods >= 30 * 24 * 12) {
      x <- c('hour','wday','wom','month')
    } else if(ArgsList$FC_Periods >= 7 * 24 * 12) {
      x <- c('hour','wday','wom')
    } else if(ArgsList$FC_Periods >= 1 * 24 * 12) {
      x <- c('hour','wday')
    } else {
      x <- c('hour')
    }
    return(x)
  }

  # 10min
  if(ArgsList$TimeUnit[[1L]] %in% c('10min')) {
    if(ArgsList$FC_Periods >= 360 * 24 * 6) {
      x <- c('hour','wday','wom','month','quarter','year')
    } else if(ArgsList$FC_Periods >= 90 * 24 * 6) {
      x <- c('hour','wday','wom','month','quarter')
    } else if(ArgsList$FC_Periods >= 30 * 24 * 6) {
      x <- c('hour','wday','wom','month')
    } else if(ArgsList$FC_Periods >= 7 * 24 * 6) {
      x <- c('hour','wday','wom')
    } else if(ArgsList$FC_Periods >= 1 * 24 * 6) {
      x <- c('hour','wday')
    } else {
      x <- c('hour')
    }
    return(x)
  }

  # 15min
  if(ArgsList$TimeUnit[[1L]] %in% c('15min')) {
    if(ArgsList$FC_Periods >= 360 * 24 * 4) {
      x <- c('hour','wday','wom','month','quarter','year')
    } else if(ArgsList$FC_Periods >= 90 * 24 * 4) {
      x <- c('hour','wday','wom','month','quarter')
    } else if(ArgsList$FC_Periods >= 30 * 24 * 4) {
      x <- c('hour','wday','wom','month')
    } else if(ArgsList$FC_Periods >= 7 * 24 * 4) {
      x <- c('hour','wday','wom')
    } else if(ArgsList$FC_Periods >= 1 * 24 * 4) {
      x <- c('hour','wday')
    } else {
      x <- c('hour')
    }
    return(x)
  }

  # 30min
  if(ArgsList$TimeUnit[[1L]] %in% c('30min')) {
    if(ArgsList$FC_Periods >= 360 * 24 * 2) {
      x <- c('hour','wday','wom','month','quarter','year')
    } else if(ArgsList$FC_Periods >= 90 * 24 * 2) {
      x <- c('hour','wday','wom','month','quarter')
    } else if(ArgsList$FC_Periods >= 30 * 24 * 2) {
      x <- c('hour','wday','wom','month')
    } else if(ArgsList$FC_Periods >= 7 * 24 * 2) {
      x <- c('hour','wday','wom')
    } else if(ArgsList$FC_Periods >= 1 * 24 * 2) {
      x <- c('hour','wday')
    } else {
      x <- c('hour')
    }
    return(x)
  }

  # 45min
  if(ArgsList$TimeUnit[[1L]] %in% c('45min')) {
    if(ArgsList$FC_Periods >= 360 * 24 * 4 / 3) {
      x <- c('hour','wday','wom','month','quarter','year')
    } else if(ArgsList$FC_Periods >= 90 * 24 * 4 / 3) {
      x <- c('hour','wday','wom','month','quarter')
    } else if(ArgsList$FC_Periods >= 30 * 24 * 4 / 3) {
      x <- c('hour','wday','wom','month')
    } else if(ArgsList$FC_Periods >= 7 * 24 * 4 / 3) {
      x <- c('hour','wday','wom')
    } else if(ArgsList$FC_Periods >= 1 * 24 * 4 / 3) {
      x <- c('hour','wday')
    } else {
      x <- c('hour')
    }
    return(x)
  }

  # hour
  if(ArgsList$TimeUnit[[1L]] %in% c('hour')) {
    if(ArgsList$FC_Periods >= 360 * 24) {
      x <- c('wday','wom','month','quarter','year')
    } else if(ArgsList$FC_Periods >= 90 * 24) {
      x <- c('wday','wom','month','quarter')
    } else if(ArgsList$FC_Periods >= 30 * 24) {
      x <- c('wday','wom','month')
    } else if(ArgsList$FC_Periods >= 7 * 24) {
      x <- c('wday','wom')
    } else {
      x <- 'wday'
    }
    return(x)
  }

  # day
  if(ArgsList$TimeUnit[[1L]] %in% c('day')) {
    if(ArgsList$FC_Periods >= 360) {
      x <- c('wday','wom','month','quarter','year')
    } else if(ArgsList$FC_Periods >= 90) {
      x <- c('wday','wom','month','quarter')
    } else if(ArgsList$FC_Periods >= 30) {
      x <- c('wday','wom','month')
    } else if(ArgsList$FC_Periods >= 7) {
      x <- c('wday','wom')
    } else {
      x <- 'wday'
    }
    return(x)
  }

  # week
  if(ArgsList$TimeUnit[[1L]] %in% c('week')) {
    if(ArgsList$FC_Periods >= 52) {
      x <- c('wom','month','quarter','year')
    } else if(ArgsList$FC_Periods >= 12) {
      x <- c('wom','month','quarter')
    } else if(ArgsList$FC_Periods >= 4) {
      x <- c('wom','month')
    } else {
      x <- 'wom'
    }
    return(x)
  }

  # month
  if(ArgsList$TimeUnit[[1L]] %in% c('month')) {
    if(ArgsList$FC_Periods >= 12) {
      x <- c('month','quarter','year')
    } else if(ArgsList$FC_Periods >= 3) {
      x <- c('month','quarter')
    } else {
      x <- 'month'
    }
    return(x)
  }

  # quarter
  if(ArgsList$TimeUnit[[1L]] %in% c('quarter')) {
    if(ArgsList$FC_Periods >= 4) {
      x <- c('quarter','year')
    } else {
      x <- c('quarter')
    }
  }

  # year
  x <- 'year'
  return(x)
}

#' @title FC.Backtest.DateGroups
#'
#' @description Forecasting TimeUnit expansion for backtesting eval rollups
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param ArgsList List
#'
#' @export
FC.Backtest.DateGroups <- function(ArgsList) {

  # 1min
  if(ArgsList$TimeUnit[[1L]] %in% c('1min')) {
    if(ArgsList$FC_Periods >= 360 * 24 * 60) {
      x <- c('day','week','month','quarter','year')
    } else if(ArgsList$FC_Periods >= 90 * 24 * 60) {
      x <- c('day','week','month','quarter')
    } else if(ArgsList$FC_Periods >= 30 * 24 * 60) {
      x <- c('day','week','month')
    } else if(ArgsList$FC_Periods >= 7 * 24 * 60) {
      x <- c('day','week')
    } else if(ArgsList$FC_Periods >= 1 * 24 * 60) {
      x <- c('day')
    } else {
      x <- NULL
    }
    return(x)
  }

  # 5min
  if(ArgsList$TimeUnit[[1L]] %in% c('5min')) {
    if(ArgsList$FC_Periods >= 360 * 24 * 12) {
      x <- c('day','week','month','quarter','year')
    } else if(ArgsList$FC_Periods >= 90 * 24 * 12) {
      x <- c('day','week','month','quarter')
    } else if(ArgsList$FC_Periods >= 30 * 24 * 12) {
      x <- c('day','week','month')
    } else if(ArgsList$FC_Periods >= 7 * 24 * 12) {
      x <- c('day','week')
    } else if(ArgsList$FC_Periods >= 1 * 24 * 12) {
      x <- c('day')
    } else {
      x <- NULL
    }
    return(x)
  }

  # 10min
  if(ArgsList$TimeUnit[[1L]] %in% c('10min')) {
    if(ArgsList$FC_Periods >= 360 * 24 * 6) {
      x <- c('day','week','month','quarter','year')
    } else if(ArgsList$FC_Periods >= 90 * 24 * 6) {
      x <- c('day','week','month','quarter')
    } else if(ArgsList$FC_Periods >= 30 * 24 * 6) {
      x <- c('day','week','month')
    } else if(ArgsList$FC_Periods >= 7 * 24 * 6) {
      x <- c('day','week')
    } else if(ArgsList$FC_Periods >= 1 * 24 * 6) {
      x <- c('day')
    } else {
      x <- NULL
    }
    return(x)
  }

  # 15min
  if(ArgsList$TimeUnit[[1L]] %in% c('15min')) {
    if(ArgsList$FC_Periods >= 360 * 24 * 4) {
      x <- c('day','week','month','quarter','year')
    } else if(ArgsList$FC_Periods >= 90 * 24 * 4) {
      x <- c('day','week','month','quarter')
    } else if(ArgsList$FC_Periods >= 30 * 24 * 4) {
      x <- c('day','week','month')
    } else if(ArgsList$FC_Periods >= 7 * 24 * 4) {
      x <- c('day','week')
    } else if(ArgsList$FC_Periods >= 1 * 24 * 4) {
      x <- c('day')
    } else {
      x <- NULL
    }
    return(x)
  }

  # 30min
  if(ArgsList$TimeUnit[[1L]] %in% c('30min')) {
    if(ArgsList$FC_Periods >= 360 * 24 * 2) {
      x <- c('day','week','month','quarter','year')
    } else if(ArgsList$FC_Periods >= 90 * 24 * 2) {
      x <- c('day','week','month','quarter')
    } else if(ArgsList$FC_Periods >= 30 * 24 * 2) {
      x <- c('day','week','month')
    } else if(ArgsList$FC_Periods >= 7 * 24 * 2) {
      x <- c('day','week')
    } else if(ArgsList$FC_Periods >= 1 * 24 * 2) {
      x <- c('day')
    } else {
      x <- NULL
    }
    return(x)
  }

  # 45min
  if(ArgsList$TimeUnit[[1L]] %in% c('45min')) {
    if(ArgsList$FC_Periods >= 360 * 24 * 4 / 3) {
      x <- c('day','week','month','quarter','year')
    } else if(ArgsList$FC_Periods >= 90 * 24 * 4 / 3) {
      x <- c('day','week','month','quarter')
    } else if(ArgsList$FC_Periods >= 30 * 24 * 4 / 3) {
      x <- c('day','week','month')
    } else if(ArgsList$FC_Periods >= 7 * 24 * 4 / 3) {
      x <- c('day','week')
    } else if(ArgsList$FC_Periods >= 24 * 4 / 3) {
      x <- c('day')
    } else {
      x <- NULL
    }
    return(x)
  }

  # hour
  if(ArgsList$TimeUnit[[1L]] %in% c('hour')) {
    if(ArgsList$FC_Periods >= 360 * 24) {
      x <- c('day','week','month','quarter','year')
    } else if(ArgsList$FC_Periods >= 90 * 24) {
      x <- c('day','week','month','quarter')
    } else if(ArgsList$FC_Periods >= 30 * 24) {
      x <- c('day','week','month')
    } else if(ArgsList$FC_Periods >= 7 * 24) {
      x <- c('day','week')
    } else if(ArgsList$FC_Periods >= 1 * 24) {
      x <- c('day')
    } else {
      x <- NULL
    }
    return(x)
  }

  # day
  if(ArgsList$TimeUnit[[1L]] %in% c('day')) {
    if(ArgsList$FC_Periods >= 360) {
      x <- c('week','month','quarter','year')
    } else if(ArgsList$FC_Periods >= 90) {
      x <- c('week','month','quarter')
    } else if(ArgsList$FC_Periods >= 30) {
      x <- c('week','month')
    } else if(ArgsList$FC_Periods >= 7) {
      x <- c('week')
    } else {
      x <- NULL
    }
    return(x)
  }

  # week
  if(ArgsList$TimeUnit[[1L]] %in% c('week')) {
    if(ArgsList$FC_Periods > 52) {
      x <- c('month','quarter','year')
    } else if(ArgsList$FC_Periods >= 12) {
      x <- c('month','quarter')
    } else if(ArgsList$FC_Periods >= 4) {
      x <- c('month')
    } else {
      x <- NULL
    }
    return(x)
  }

  # month
  if(ArgsList$TimeUnit[[1L]] %in% c('month')) {
    if(ArgsList$FC_Periods >= 12) {
      x <- c('quarter','year')
    } else if(ArgsList$FC_Periods >= 3) {
      x <- c('quarter')
    } else {
      x <- NULL
    }
    return(x)
  }

  # quarter
  if(ArgsList$TimeUnit[[1L]] %in% c('quarter')) {
    if(ArgsList$FC_Periods >= 4) {
      x <- c('year')
    } else {
      x <- NULL
    }
  }

  # year
  x <- 'year'
  return(x)
}

#' @title FC.DateCast
#'
#' @description Forecasting TimeUnit conversion
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param ArgsList List
#' @param VD ValidationData
#'
#' @export
FC.DateCast <- function(ArgsList, DebugFC, VD = NULL) {
  print('Check data DataColumnName type')

  if(missing(ArgsList)) {print('FC.DateCast() returning from ArgsList missing'); return(list(ArgsList = NULL, VD = VD))}
  if(length(ArgsList[['data']]) == 0L) {print('FC.DateCast() returning from ArgsList$data missing'); return(list(ArgsList = NULL, VD = VD))}
  x <- tryCatch({!class(ArgsList[['data']][[ArgsList[['DateColumnName']]]])[[1L]] %in% c('Date','posix')}, error = function(x) FALSE)
  if(x) {
    if(DebugFC) print('Data DateColumnName needs to be updated')
    if(ArgsList[['TimeUnit']] %in% c("1min","5min","10min","15min","30min","hour")) {
      if(DebugFC) print('Update A')
      ArgsList[['data']][, eval(ArgsList[['DateColumnName']]) := as.POSIXct(get(ArgsList[['DateColumnName']]))]
    } else {
      if(DebugFC) print('Update B')
      ArgsList[['data']][, eval(ArgsList[['DateColumnName']]) := as.Date(get(ArgsList[['DateColumnName']]))]
    }
  }
  print('Check if VD is NULL or a data.table')
  if(length(VD) > 0L && data.table::is.data.table(VD)) {
    if(DebugFC) print('ValidationData DateColumnName needs to be updated')
    if(!class(VD[[ArgsList[['DateColumnName']]]])[[1L]] %in% c('Date','posix')) {
      if(ArgsList[['TimeUnit']] %in% c("1min","5min","10min","15min","30min","hour")) {
        if(DebugFC) print('Update A')
        VD[, eval(ArgsList[['DateColumnName']]) := as.POSIXct(get(ArgsList[['DateColumnName']]))]
      } else {
        if(DebugFC) print('Update B')
        VD[, eval(ArgsList[['DateColumnName']]) := as.Date(get(ArgsList[['DateColumnName']]))]
      }
    }
  }
  return(list(ArgsList = ArgsList, VD = VD))
}

#' @title FC.DateCast
#'
#' @description Forecasting TimeUnit conversion
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param ArgsList List
#' @param VD ValidationData
#'
#' @export
FC.FCPeriods <- function(ArgsList, VD = NULL) {
  if(data.table::is.data.table(VD) && data.table::is.data.table(ArgsList$data))  {
    TrainMaxDate <- max(ArgsList[['data']][[ArgsList[['DateColumnName']]]], na.rm = TRUE)
    ValidationMaxDate <- max(VD[[ArgsList[['DateColumnName']]]], na.rm = TRUE)
    ArgsList[['FC_Periods']] <- max(0L, min(ArgsList[['FC_Periods']], as.numeric(difftime(ValidationMaxDate, TrainMaxDate))))
    return(ArgsList)
  } else {
    return(ArgsList)
  }
}

#' @title Shiny.FC.Panel.Metrics.Raw
#'
#' @description Forecasting create error metrics columns
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param DataList From App
#' @param ArgsList From App
#' @param ModelID From App
#' @param GroupVariables From App
#' @param Metrics 'AvgError', 'AccumError', 'RMSE', 'MAE', 'MAPE', 'SMAPE'
#' @param DebugFC = FALSE
#'
#' @export
Shiny.FC.Panel.Metrics.Raw <- function(DataList,
                                       ArgsList,
                                       ModelID,
                                       GroupVariables = NULL,
                                       Metrics = c('AvgError','AccumError','RMSE','MAE','MAPE','SMAPE'),
                                       CrossEval = FALSE,
                                       DebugFC = FALSE) {

  # Helpers Variables
  if(DebugFC) print('Shiny.FC.Panel.Metrics.Raw 1')
  if(CrossEval) {
    if(DebugFC) print("CrossEval == TRUE")
    x <- paste0(ModelID, '_CE_Raw')
    xx <- paste0(ModelID, '_CE_Rollup')
  } else {
    if(DebugFC) print("CrossEval == FALSE")
    x <- paste0(ModelID, '_BT_Raw')
    xx <- paste0(ModelID, '_BT_Rollup')
  }

  dns <- DataList[[x]][['colnames']]
  if(DebugFC) {print("dns == "); print(dns)}
  gvl <- length(GroupVariables)
  if(DebugFC) {print("gvl == "); print(gvl)}
  gvm <- ArgsList$GroupVariables
  if(DebugFC) {print("gvm == "); print(gvm)}

  # Add Calendar Variables for addition breakouts
  if(DebugFC) print('Shiny.FC.Panel.Metrics.Raw: FC.Backtest.CalendarGroups()')
  cgs <- Quantico:::FC.Backtest.CalendarGroups(ArgsList)
  if(DebugFC) {print("cgs == "); print(cgs)}
  DateVarAggs <- Quantico:::FC.Backtest.DateGroups(ArgsList)
  if(DebugFC) {print("DateVarAggs == "); print(DateVarAggs)}
  if(length(DateVarAggs) > 0L) for(j in DateVarAggs) DataList[[x]][['data']][, paste0(ArgsList$DateColumnName, "_", j) := lubridate::floor_date(get(ArgsList$DateColumnName), unit = j)]

  # Calendar Vars
  if(!any(paste0(ArgsList$DateColumnName, c('_wday','_mdy','_yday','_wom','_week','_isoweek','_month','_quarter','_year')) %in% DataList[[x]][['colnames']])) {
    if(DebugFC) print('Shiny.FC.Panel.Metrics.Raw: CalendarVariables()')
    if(DebugFC) print(x)
    if(DebugFC) print(names(DataList))
    if(DebugFC) print(DataList[[x]][['data']])
    if(DebugFC) print(ArgsList$DateColumnName)
    if(DebugFC) print(cgs)
    DataList[[x]][['data']] <- Rodeo::CreateCalendarVariables(data = DataList[[x]][['data']], DateCols = ArgsList$DateColumnName, TimeUnits = cgs); if(DebugFC) print('Shiny.FC.Panel.Metrics.Raw 6')
  }

  # Sort to ensure AccumError is correct
  if(DebugFC) {print('Shiny.FC.Panel.Metrics.Raw Sort data.table'); print(c(ArgsList$GroupVariables, ArgsList$DateColumnName))}
  data.table::setorderv(x = DataList[[x]][['data']], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName)); if(DebugFC) print('Shiny.FC.Panel.Metrics.Raw 3')

  # Add a FCPeriod column to indentify the period in the forecast horizon. zeros filled for all training records
  if(DebugFC) print('Shiny.FC.Panel.Metrics.Raw Rbindlist Train and FC')
  DataList[[x]][['data']] <- data.table::rbindlist(list(
    DataList[[x]][['data']][DataSet == 'Train'][, FCPeriod := 0, by = c(ArgsList$GroupVariables)],
    DataList[[x]][['data']][DataSet == 'Evaluation'][, FCPeriod := seq_len(.N), by = c(ArgsList$GroupVariables)]
  ))

  # Add metrics
  if(DebugFC) print('Shiny.FC.Panel.Metrics.Raw Rbindlist Create Metrics')
  if(DebugFC) {print("DataList[[x]][['data']]"); print(DataList[[x]][['data']])}
  DataList[[x]][['data']][, AvgError := get(ArgsList[['TargetColumnName']]) - Predictions]
  DataList[[x]][['data']][, AccumError := cumsum(AvgError), by = c(ArgsList$GroupVariables)]
  DataList[[x]][['data']][, RMSE := AvgError ^ 2]
  DataList[[x]][['data']][, MAE := abs(AvgError)]
  DataList[[x]][['data']][, MAPE := MAE / (get(ArgsList$TargetColumnName) + 0.01) - 1]
  DataList[[x]][['data']][, SMAPE := 2 * MAE / (abs(get(ArgsList$TargetColumnName)) + abs(Predictions))]
  DataList[[x]][['data']] <- DataList[[x]][['data']][, paste0(Metrics) := lapply(.SD, round, 4L), .SDcols = c(Metrics)]
  DataList[[x]][['data']][, ModelID := eval(ModelID)]
  data.table::setcolorder(x = DataList[[x]][['data']], neworder = c(ncol(DataList[[x]][['data']]), 1L:(ncol(DataList[[x]][['data']])-1L)))
  DataList <- Quantico:::DM.DataListUpdate(DataList, x)

  # Return
  if(DebugFC) print('Shiny.FC.Panel.Metrics.Raw Finsihed up with Metrics.Raw')
  return(DataList)
}

#' @title Shiny.FC.Panel.Metrics.Agg
#'
#' @description Aggregate Raw Metrics, update DataList
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param DataList From App
#' @param ArgsList From App
#' @param ModelID From App
#' @param Rollup = FALSE. Set to TRUE to get an agg set of eval metrics
#' @param GroupVariables From App
#' @param Metrics From App
#' @param CrossEval = FALSE. Set to TRUE for CrossEval agg
#' @param DebugFC = FALSE
#'
#' @export
Shiny.FC.Panel.Metrics.Agg <- function(DataList,
                                       ArgsList,
                                       ModelID,
                                       GroupVariables = NULL,
                                       CrossEval = FALSE,
                                       Metrics = c('AvgError','AccumError','RMSE','MAE','MAPE','SMAPE'),
                                       DebugFC = FALSE) {

  # Helpers
  if(CrossEval) {
    x <- paste0(ModelID, '_CE_Raw')
    xx <- paste0(ModelID, '_CE_Rollup')
  } else {
    x <- paste0(ModelID, '_BT_Raw')
    xx <- paste0(ModelID, '_BT_Rollup')
  }

  vars <- c(ArgsList[['TargetColumnName']], 'Predictions', Metrics)

  # Rollup Metrics
  if(length(GroupVariables) == 0L) {
    aggD <- DataList[[x]][['data']][DataSet == 'Evaluation'][, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(vars)]
  } else {
    aggD <- DataList[[x]][['data']][DataSet == 'Evaluation'][, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(vars), by = c(GroupVariables)]
  }

  # Final Modifications to metrics
  aggD[, eval(ArgsList[['TargetColumnName']]) := round(get(ArgsList[['TargetColumnName']]), 4)]
  aggD[, Predictions := round(Predictions, 4)]
  aggD[, RMSE := round(sqrt(RMSE), 2)]
  aggD[, MAPE := paste0(round(MAPE, 4)*100, "%")]
  aggD[, SMAPE := paste0(round(SMAPE, 4)*100, "%")]
  aggD[, AvgError := round(AvgError, 2)]
  aggD[, AccumError := round(AccumError, 2)]
  aggD[, MAE := round(MAE, 2)]

  if(DebugFC) print(rep("$$$", 5))
  if(DebugFC) print(aggD)

  # Finalize
  if(!data.table::is.data.table(DataList[[xx]][['data']])) {
    if(DebugFC) print("Store Agg Data in DataList 1.a")
    if(DebugFC) print(data.table::is.data.table(aggD))
    DataList[[xx]][['data']] <- data.table::copy(aggD)
  } else {
    if(DebugFC) print("Store Agg Data in DataList 1.b")
    if(DebugFC) print(data.table::is.data.table(DataList[[xx]][['data']]))
    if(DebugFC) print(data.table::is.data.table(aggD))
    DataList[[xx]][['data']] <- data.table::rbindlist(list(
      DataList[[xx]][['data']],
      data.table::copy(aggD)), use.names = TRUE, fill = TRUE)
  }

  if(DebugFC) print(rep("s", 10))
  if(DebugFC) print(data.table::is.data.table(DataList[[xx]][['data']]))

  mid <- ModelID # data.table doesn't work well will Name := eval(Name) when Name is a colname and Name is a value to be used to fill. Doesn't work
  DataList[[xx]][['data']][, ModelID := eval(mid)]
  if(names(DataList[[xx]][['data']])[ncol(DataList[[xx]][['data']])] == 'ModelID') data.table::setcolorder(x = DataList[[xx]][['data']], neworder = c(ncol(DataList[[xx]][['data']]), 1L:(ncol(DataList[[xx]][['data']])-1L)))
  DataList <- Quantico:::DM.DataListUpdate(DataList, xx)

  if(DebugFC) print(rep("t", 10))
  if(DebugFC) print(data.table::is.data.table(DataList[[xx]][['data']]))

  # Return
  return(DataList)
}

#' @title Shiny.FC.Panel.Metric.Rollup
#'
#' @description Rollup of error metrics
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param ModelID From App
#' @param ArgsList From App
#' @param DataList From App
#' @param CodeList From App
#' @param DebugFC From App
#' @param CE FALSE
#'
#' @export
Shiny.FC.Panel.Metric.Rollup <- function(ModelID,ArgsList,DataList,CodeList,DebugFC, CE = FALSE) {

  # Raw Eval Data Creation: No agg and Date Columns still exists
  if(DebugFC) print("Shiny.FC.Panel.Metric.Rollup Raw Eval")
  DataList <- Quantico::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, DebugFC = DebugFC, CrossEval = CE)
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Error Metrics Raw: Raw data granularity and DateColumnName still exist\n",
    "DataList <- Quantico::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = ArgsList$GroupVariables, DebugFC = DebugFC)\n"))

  # Agg Eval Total: everything in between can be created by user
  if(DebugFC) print("Shiny.FC.Panel.Metric.Rollup Agg Eval")
  DataList <- Quantico::Shiny.FC.Panel.Metrics.Agg(DataList,ArgsList,ModelID,GroupVariables = NULL, CrossEval = CE, DebugFC = DebugFC)
  CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Error Metrics: Total Full Period\n",
    "DataList <- Quantico::Shiny.FC.Panel.Metrics.Raw(DataList,ArgsList,ModelID,GroupVariables = NULL)\n"))

  if(DebugFC) print("Shiny.FC.Panel.Metric.Rollup Return")
  return(list(
    DataList = DataList,
    CodeList = CodeList
  ))
}

#' @title Shiny.FC.Panel.Train
#'
#' @description Step 1 in the forecasting process, train the model
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param ArgsList List
#' @param CodeList From app
#' @param DataList From app
#' @param ModelID From Panel Forecasting Common Args Section of Shiny.FC.Trainer
#' @param Algo 'catboost', 'xgboost', 'lightgbm'
#' @param VD ValidationData
#' @param DebugFC From app
#'
#' @export
Shiny.FC.Panel.Train <- function(ArgsList, CodeList, DataList, ModelID, Algo = 'catboost', VD = NULL, DebugFC = FALSE) {

  if(missing(ModelID) || length(ModelID) == 0L || is.na(ModelID)) {
    ModelID <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_ModelID')]]}, error=function(x) NULL), Type='character', Default='FC001')
    ArgsList$ModelID <- ModelID
  }

  ReportOutput <- list()

  # Combine TrainData and VD: train and not forecast, so combine and do a regular train / validate / test model build / analysis
  if(DebugFC) print("Shiny.FC.Panel.Train 1")
  if(length(VD) > 0L) {
    if(DebugFC) print("Shiny.FC.Panel.Train 1.a")
    ArgsList[['data']] <- unique(data.table::rbindlist(list(ArgsList[['data']], VD), use.names = TRUE, fill = TRUE))
    VD <- NULL
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Combine data for ML training only\n",
      "ArgsList[['data']] <- data.table::rbindlist(list(\n  ",
      "ArgsList[['data']],\n  ",
      "VD = ValidationData),\n  ",
      "use.names = TRUE, fill = TRUE)\n"))}, error = function(x) NULL)
  }

  # Build model
  if(DebugFC) print("Shiny.FC.Panel.Train 2")
  dtc <- data.table::copy(ArgsList[['data']])
  ArgsList$TrainOnFull <- FALSE
  if(DebugFC) print("Shiny.FC.Panel.Train 3")

  # saveRDS(object = ArgsList, file = "C:/Users/Bizon/Documents/GitHub/CatBoostFC_ArgsList.rds")

  if(tolower(Algo) == 'catboost') {
    Output <- do.call(what = AutoQuant::AutoCatBoostCARMA, args = ArgsList)
  } else if(tolower(Algo) == 'xgboost') {
    Output <- do.call(what = AutoQuant::AutoXGBoostCARMA, args = ArgsList)
  } else if(tolower(Algo) == 'lightgbm') {
    Output <- do.call(what = AutoQuant::AutoLightGBMCARMA, args = ArgsList)
  }

  # Store ML Output Object
  if(DebugFC) print("Shiny.FC.Panel.Train 4")
  ArgsList <- Output$ArgsList

  # Do not want data persisting from here. From here, you can
  #   either go to Backtest, Retrain, Forecast and for each
  #   of those, you must supply your own data
  ArgsList$data <- NULL
  if(DebugFC) print("Shiny.FC.Panel.Train 6")
  CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Train ML Model\n",
    "dtc <- data.table::copy(ArgsList[['data']])\n",
    "Output <- do.call(what = AutoQuant::AutoCatBoostCARMA, args = ArgsList)\n",
    "ArgsList <- Output$ArgsList\n",
    "ArgsList[[", Quantico:::CEP(ModelID), "_Meta)]] <- Output$TestModel\n",
    "if(!(length(ArgsList$data) > 0 && data.table::is.data.table(ArgsList$data))) {\n  ",
    "if(length(VD) > 0L) {\n    ",
    "ArgsList[['data']] <- unique(data.table::rbindlist(list(dtc, VD), use.names = TRUE, fill = TRUE))\n  ",
    "}\n",
    "}\n"))}, error = function(x) NULL)

  ArgsList[[paste0(ModelID, "_Meta")]] <- Output$TestModel
  if(DebugFC) print("Shiny.FC.Panel.Train 7")

  # ML Data: just like in the ML function shiny.ML.Trainer()
  Output <- Quantico:::Shiny.ML.ModelDataObjects(ArgsList[[paste0(ModelID, "_Meta")]], DebugFC, TT = 'catboost')

  if(DebugFC) print("Shiny.FC.Panel.Train 8")
  if(length(Output$ScoringDataCombined) > 0L) {
    if(DebugFC) print("Shiny.FC.Panel.Train 8.a")
    DataList[[paste0('CatBoostFC_', ModelID, '_ScoringData')]][['data']] <- Output$ScoringDataCombined
    if(DebugFC) print("Shiny.FC.Panel.Train 8.b")
    DataList <- tryCatch({Quantico:::DM.DataListUpdate(DataList, paste0('CatBoostFC_', ModelID, '_ScoringData'))}, error = function(x) {print(names(DataList)); return(DataList)})
  }

  if(DebugFC) print("Shiny.FC.Panel.Train 9")
  if(length(Output$VI_Train) > 0L) {
    if(DebugFC) print("Shiny.FC.Panel.Train 9.a")
    DataList[[paste0('CatBoostFC_', ModelID, '_Test_VI_Data')]][['data']] <- Output$VI_Train
    if(DebugFC) print("Shiny.FC.Panel.Train 9.b")
    DataList <- Quantico:::DM.DataListUpdate(DataList, paste0('CatBoostFC_', ModelID, '_Test_VI_Data'), Sample = FALSE)
  }

  if(DebugFC) print("Shiny.FC.Panel.Train 10")
  if(length(Output$VI_Validation) > 0L) {
    if(DebugFC) print("Shiny.FC.Panel.Train 10.a")
    DataList[[paste0('CatBoostFC_', ModelID, '_Train_VI_Data')]][['data']] <- Output$VI_Validation
    if(DebugFC) print("Shiny.FC.Panel.Train 10.b")
    DataList <- Quantico:::DM.DataListUpdate(DataList, paste0('CatBoostFC_', ModelID, '_Train_VI_Data'), Sample = FALSE)
  }

  if(DebugFC) print("Shiny.FC.Panel.Train 11")
  if(length(Output$VI_Test) > 0L) {
    if(DebugFC) print("Shiny.FC.Panel.Train 11.a")
    DataList[[paste0('CatBoostFC_', ModelID, '_Validation_VI_Data')]][['data']] <- Output$VI_Test
    if(DebugFC) print("Shiny.FC.Panel.Train 11.b")
    DataList <- Quantico:::DM.DataListUpdate(DataList, paste0('CatBoostFC_', ModelID, '_Validation_VI_Data'), Sample = FALSE)
  }

  if(DebugFC) print("Shiny.FC.Panel.Train 12")
  if(length(Output$II_Train) > 0L) {
    if(DebugFC) print("Shiny.FC.Panel.Train 12.a")
    DataList[[paste0('CatBoostFC_', ModelID, '_All_II_Data')]][['data']] <- Output$II_Train
    if(DebugFC) print("Shiny.FC.Panel.Train 12.b")
    DataList <- Quantico:::DM.DataListUpdate(DataList, paste0('CatBoostFC_', ModelID, '_All_II_Data'), Sample = FALSE)
  }

  if(DebugFC) print("Shiny.FC.Panel.Train return")
  return(list(
    DataList = DataList,
    CodeList = CodeList,
    ArgsList = ArgsList,
    ValidationData = VD,
    ReportObjects = Output
  ))
}

#' @title Shiny.FC.Panel.Retrain
#'
#' @description Step 1 in the forecasting process, train the model
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param NewDataName ArgsList name
#' @param ArgsList List
#' @param CodeList From app
#' @param DataList From app
#' @param Algo 'catboost', 'xgboost', 'lightgbm'
#' @param DebugFC From app
#'
#' @export
Shiny.FC.Panel.Retrain <- function(NewDataName, ArgsList, CodeList, DataList, Algo = 'catboost', DebugFC = FALSE) {

  if(DebugFC) {print("Shiny.FC.Panel.Retrain 1"); print(paste0('length of ArgsList = ', length(ArgsList)))}
  if(length(ArgsList) > 5) print(names(ArgsList)) else return(NULL)

  # Model ID
  if(missing(ModelID) || length(ModelID) == 0L || is.na(ModelID)) {
    ModelID <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_ModelID')]]}, error=function(x) NULL), Type='character', Default='FC001')
    ArgsList$ModelID <- ModelID
  }

  # Define Data and Remove data from ArgsList so we're not passing data to carma function twice
  data <- DataList[[NewDataName]][['data']]
  ArgsList$Model <- NULL

  # Build model
  if(DebugFC) print("Shiny.FC.Panel.Retrain 2")
  print(length(ArgsList))
  if(length(ArgsList) > 0L) print(tryCatch({names(ArgsList)}, error = function(x) NULL))

  ArgsList$TrainOnFull <- FALSE
  if(tolower(Algo) == 'catboost') {
    Output <- AutoQuant::AutoCatBoostCARMA(data = data, TrainOnFull = FALSE, SaveModel = TRUE, ArgsList = ArgsList)
  } else if(tolower(Algo) == 'xgboost') {
    Output <- AutoQuant::AutoXGBoostCARMA(data = data, TrainOnFull = FALSE, SaveModel = TRUE, ArgsList = ArgsList)
  } else if(tolower(Algo) == 'lightgbm') {
    Output <- AutoQuant::AutoLightGBMCARMA(data = data, TrainOnFull = FALSE, SaveModel = TRUE, ArgsList = ArgsList)
  }

  # Code Collection
  if(DebugFC) print("Shiny.FC.Panel.Retrain 3")
  CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Retrain Model\n",
    "Output <- AutoQuant::AutoCatBoostCARMA(\n  ",
    "data = data,\n  ",
    "TrainOnFull = FALSE,\n  ",
    "SaveModel = TRUE,\n  ",
    "ArgsList = ArgsList)\n"))}, error = function(x) CodeList)

  # ML Data: just like in the ML function shiny.ML.Trainer()
  if(DebugFC) {print("Shiny.FC.Panel.Retrain 4"); if(length(ArgsList) > 0L) print(names(ArgsList))}

  x <- Output$ModelInformation
  Output <- Quantico:::Shiny.ML.ModelDataObjects(x, Debug, TT = 'catboost')
  DataList[[paste0('CatBoostFC_', ModelID, '_ScoringData')]][['data']] <- Output$ScoringDataCombined
  DataList <- Quantico:::DM.DataListUpdate(DataList, paste0(Algo, 'FC_', ModelID, '_ScoringData'), Sample = FALSE)

  if(DebugFC) print("Shiny.FC.Panel.Retrain 5")
  DataList[[paste0('CatBoostFC_', ModelID, '_Test_VI_Data')]][['data']] <- Output$VI_Train
  DataList <- Quantico:::DM.DataListUpdate(DataList, paste0(Algo, 'FC_', ModelID, '_Test_VI_Data'), Sample = FALSE)

  if(DebugFC) print("Shiny.FC.Panel.Retrain 6")
  DataList[[paste0('CatBoostFC_', ModelID, '_Train_VI_Data')]][['data']] <- Output$VI_Validation
  DataList <- Quantico:::DM.DataListUpdate(DataList, paste0(Algo, 'FC_', ModelID, '_Train_VI_Data'), Sample = FALSE)

  if(DebugFC) print("Shiny.FC.Panel.Retrain 7")
  DataList[[paste0('CatBoostFC_', ModelID, '_Validation_VI_Data')]][['data']] <- Output$VI_Test
  DataList <- Quantico:::DM.DataListUpdate(DataList, paste0(Algo, 'FC_', ModelID, '_Validation_VI_Data'), Sample = FALSE)

  if(DebugFC) print("Shiny.FC.Panel.Retrain 8")
  DataList[[paste0('CatBoostFC_', ModelID, '_All_II_Data')]][['data']] <- Output$II_Train
  DataList <- Quantico:::DM.DataListUpdate(DataList, paste0(Algo, 'FC_', ModelID, '_All_II_Data'), Sample = FALSE)

  # Return
  if(DebugFC) print("Shiny.FC.Panel.Retrain Return")
  return(list(
    DataList = DataList,
    CodeList = CodeList,
    ArgsList = ArgsList
  ))
}

#' @title Shiny.FC.Panel.Forecast
#'
#' @description Step 1 in the forecasting process, train the model
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param NewDataName ArgsList name
#' @param ArgsList List
#' @param CodeList From app
#' @param DataList From app
#' @param ModelID From Panel Forecasting Common Args Section of Shiny.FC.Trainer
#' @param Algo 'catboost', 'xgboost', 'lightgbm'
#' @param VD ValidationData
#' @param DebugFC From app
#'
#' @export
Shiny.FC.Panel.Forecast <- function(NewDataName, ArgsList, CodeList, DataList, ModelID, Algo = 'catboost', VD = NULL, DebugFC = FALSE) {

  if(DebugFC) print("Shiny.FC.Panel.Forecast 1")

  # Define Data and Remove data from ArgsList so we're not passing data to carma function twice
  if(length(ArgsList) > 5) {
    ArgsList$data <- NULL

    if(length(ArgsList$Model) == 0L) sm <- TRUE else sm <- FALSE

    # Build model
    if(DebugFC) {print("Shiny.FC.Panel.Forecast 2"); print(ArgsList$Model)}
    if(tolower(Algo) == 'catboost') {
      Output <- AutoQuant::AutoCatBoostCARMA(data = DataList[[NewDataName]][['data']], FC_Periods = ArgsList$FC_Periods, TrainOnFull = TRUE, SaveModel = sm, ArgsList = ArgsList)
    } else if(tolower(Algo) == 'xgboost') {
      Output <- AutoQuant::AutoXGBoostCARMA(data = DataList[[NewDataName]][['data']], FC_Periods = ArgsList$FC_Periods, TrainOnFull = TRUE, SaveModel = sm, ArgsList = ArgsList)
    } else if(tolower(Algo) == 'lightgbm') {
      Output <- AutoQuant::AutoLightGBMCARMA(data = DataList[[NewDataName]][['data']], FC_Periods = ArgsList$FC_Periods, TrainOnFull = TRUE, SaveModel = sm, ArgsList = ArgsList)
    }

    # Code Collection
    if(DebugFC) print("Shiny.FC.Panel.Forecast 3")
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Generate Forecast\n",
      "Output <- AutoQuant::AutoCatBoostCARMA(\n  ",
      "data = data,\n  ",
      "FC_Periods = ArgsList$FC_Periods,\n  ",
      "TrainOnFull = TRUE,\n  ",
      "SaveModel = FALSE,\n  ",
      "ArgsList = ArgsList)\n"))}, error = function(x) CodeList)

    # ML Data: just like in the ML function shiny.ML.Trainer()
    if(DebugFC) print("Shiny.FC.Panel.Forecast 4")
    DataList[[paste0('FC_', ModelID)]][['data']] <- Output$Forecast
    if(DebugFC) print("Shiny.FC.Panel.Forecast 5")
    DataList <- Quantico:::DM.DataListUpdate(DataList, paste0('FC_', ModelID))

    # Return
    if(DebugFC) print("Shiny.FC.Panel.Forecast Return")
    return(list(
      DataList = DataList,
      CodeList = CodeList
    ))

  } else {
    return(NULL)
  }
}

#' @title Shiny.FC.Panel.Backtest
#'
#' @description Step 2 in the forecasting process, backtest the model
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param ArgsList List
#' @param CodeList From app
#' @param DataList From app
#' @param ModelID From Panel Forecasting Common Args Section of Shiny.FC.Trainer
#' @param Algo 'catboost', 'lightgbm', 'xgboost'
#' @param VD ValidationData
#' @param CrossEval FALSE. Set to TRUE for CrossEval run mode
#' @param DebugFC From app
#'
#' @export
Shiny.FC.Panel.Backtest <- function(ArgsList,
                                    CodeList,
                                    DataList,
                                    ModelID,
                                    Algo = 'catboost',
                                    VD = NULL,
                                    CrossEval = FALSE,
                                    DebugFC = FALSE) {

  # Check if VD is NULL
  if(DebugFC) print("Shiny.FC.Panel.Backtest 0 Start")
  if(!data.table::is.data.table(VD)) {print('VD is not a data.table'); return(list(DataList = DataList, CodeList = CodeList, ArgsList = ArgsList))}

  # Check if ModelID is NULL
  if(DebugFC) print("Shiny.FC.Panel.Backtest 1")
  if(missing(ModelID) || length(ModelID) == 0L || is.na(ModelID)) {
    ModelID <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_ModelID')]]}, error=function(x) NULL), Type='character', Default='FC001')
    ArgsList$ModelID <- ModelID
  }

  # @@@@@@@@@@@@@@@@@
  #    TRAIN + FC
  # @@@@@@@@@@@@@@@@@

  # Reconcile FC_Periods: enforce --> min(requested FC_Periods, available FC_Periods)
  if(DebugFC) print("Shiny.FC.Panel.Backtest 2")
  ArgsList <- Quantico::FC.FCPeriods(ArgsList, VD = VD)
  CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Reconcile FC_Periods and set to TrainOnFull == TRUE\n",
    "ArgsList <- Quantico::FC.FCPeriods(ArgsList, VD = ValidationData)\n"))}, error = function(x) CodeList)

  # Build FC
  if(DebugFC) print("Shiny.FC.Panel.Backtest 3")
  ArgsList$TrainOnFull <- TRUE
  print(ArgsList)
  if(tolower(Algo) == 'catboost') {
    Output <- do.call(what = AutoQuant::AutoCatBoostCARMA, args = ArgsList)
  } else if(tolower(Algo) == 'xgboost') {
    Output <- do.call(what = AutoQuant::AutoXGBoostCARMA, args = ArgsList)
  } else if(tolower(Algo) == 'lightgbm') {
    Output <- do.call(what = AutoQuant::AutoLightGBMCARMA, args = ArgsList)
  }

  if(DebugFC) print("Shiny.FC.Panel.Backtest 4")

  # Code collect
  CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Generate Forecast for Backtest\n",
    "Output <- do.call(what = AutoQuant::AutoCatBoostCARMA, args = ArgsList)\n"))}, error = function(x) CodeList)

  # @@@@@@@@@@@@@@@@@
  #     EVALUATE
  # @@@@@@@@@@@@@@@@@

  # WORKS ON STEPPING THROUGH. DataList has data, but not when running function?
  if(DebugFC) print("Shiny.FC.Panel.Backtest 5")
  if(CrossEval) {
    dlname <- '_CE_Raw'
    dlrname <- '_CE_Rollup'
  } else {
    dlname <- '_BT_Raw'
    dlrname <- '_BT_Rollup'
  }

  # Store Output Data
  if(DebugFC) print("Shiny.FC.Panel.Backtest 6")
  DataList[[paste0(ModelID, dlname)]][['data']] <- Output$Forecast
  ArgsList$TVT <- Output$ArgsList$TVT
  DataList[[paste0(ModelID, dlname)]][['data']][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]

  # Merge Validation Data to FC data
  if(DebugFC) print("Shiny.FC.Panel.Backtest 7")
  data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]][['data']], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
  data.table::setkeyv(x = VD, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))
  DataList[[paste0(ModelID, dlname)]][['data']][VD, eval(ArgsList$TargetColumnName) := get(paste0("i.", ArgsList$TargetColumnName))]
  DataList[[paste0(ModelID, dlname)]][['data']] <- DataList[[paste0(ModelID, dlname)]][['data']][!is.na(get(ArgsList$TargetColumnName))]
  CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Store Output Data\n",
    "if(!exists('CrossEval')) CrossEval <- FALSE\n",
    "if(CrossEval) dlname <- '_CE_Raw' else dlname <- '_BT_Raw'\n",
    "DataList[[paste0(ModelID, dlname)]] <- Output$Forecast\n",
    "DataList[[paste0(ModelID, dlname)]][, DataSet := data.table::fifelse(is.na(get(ArgsList$TargetColumnName)), 'Evaluation','Train')]\n",
    "data.table::setkeyv(x = DataList[[paste0(ModelID, dlname)]], cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))\n",
    "data.table::setkeyv(x = ValidationData, cols = c(ArgsList$GroupVariables, ArgsList$DateColumnName))\n",
    "DataList[[paste0(ModelID, dlname)]][ValidationData, eval(ArgsList$TargetColumnName) := get(paste0('i.', ArgsList$TargetColumnName))]\n",
    "DataList[[paste0(ModelID, dlname)]] <- DataList[[paste0(ModelID, dlname)]][!is.na(get(ArgsList$TargetColumnName))]\n"))}, error = function(x) CodeList)

  # Rollup Metrics
  # Output <- Quantico:::Shiny.FC.Panel.Metrics.Raw(ArgsList,DataList,CodeList,DebugFC)
  # DataList <- Output$DataList; CodeList <- Output$CodeList
  if(DebugFC) print("Shiny.FC.Panel.Backtest 8")
  Output <- Quantico:::Shiny.FC.Panel.Metric.Rollup(ModelID,ArgsList,DataList,CodeList,DebugFC, CE = if(dlname == '_CE_Raw') TRUE else FALSE)
  DataList <- Output$DataList; CodeList <- Output$CodeList

  # Subset + Reorder Columns
  if(DebugFC) print("Shiny.FC.Panel.Backtest 9")
  if(length(DataList[[paste0(ModelID, dlrname)]]) > 0L) {
    DataList <- tryCatch({Quantico:::DM.DataListUpdate(DataList, paste0(ModelID, dlrname))}, error = function(x) DataList)
    DataList[[paste0(ModelID, dlrname)]][['sample']] <- DataList[[paste0(ModelID, dlrname)]][['data']]
  }

  # Return
  if(DebugFC) print('Shiny.FC.Panel.Backtest 10 Done')
  print(paste0("TYT has length: ", length(ArgsList$TVT)))
  DataList[[paste0(ModelID, dlname)]][['sample']] <- DataList[[paste0(ModelID, dlname)]][['data']]
  return(list(
    DataList = DataList,
    CodeList = CodeList,
    ArgsList = ArgsList,
    dlname = dlname
  ))
}

#' @title BasicLoop.SingleTest
#'
#' @description Test out various combinations of structure parameters to narrow the search space
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param LM LoopMetrics From Backtest.SingelLoop
#' @param Plan From Backtest.SingelLoop
#' @param ArgsList List
#' @param CodeList From app
#' @param DataList From app
#' @param ModelID From Panel Forecasting Common Args Section of Shiny.FC.Trainer
#' @param VD ValidationData
#' @param Test = Test
#' @param Lift = 0. If MAE with args on is greater than MAE with args off by at least MAE_MinLift then the feature will be kept on
#' @param LiftMetric = 'MAE' or 'RMSE'
#' @param Algo = 'catboost', 'xgboost', 'lightgbm'
#' @param DebugFC From app
#'
#' @keywords internal
BasicLoop.SingleTest <- function(LM, Plan, ArgsList, DataList, CodeList, ModelID, VD, Test, Lift = 0, LiftMetric = 'MAE', Algo = 'catboost', DebugFC = FALSE) {

  if(DebugFC) print(paste0('BasicLoop.SingleTest 1: test = ', Test))

  if(!data.table::is.data.table(VD)) {print('BasicLoop.SingleTest(): VD is NOT a data.table'); return(list(LoopMetrics = LM, ArgsList = ArgsList))}

  if(missing(ModelID) || length(ModelID) == 0L || is.na(ModelID)) {
    ModelID <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_ModelID')]]}, error=function(x) NULL), Type='character', Default='FC001')
    ArgsList$ModelID <- ModelID
  }

  # Ensure no model is found
  ArgsList[['Model']] <- NULL

  RunCheck <- LM[`RMSE On` > -5, .N]
  if(RunCheck > 0L) RunCheck <- 1L else RunCheck <- 2L

  # Build
  if(DebugFC) print(paste0('BasicLoop.SingleTest 2'))

  for(trial in seq_len(RunCheck)) {
    if(DebugFC) print(paste0('trial number: ', trial))

    ArgsList$DebugMode <- TRUE

    if(trial == 1L) ArgsList[[Test]] <- Plan[[Test]]$ArgsOn else ArgsList[[Test]] <- Plan[[Test]]$ArgsOff
    Output <- Quantico::Shiny.FC.Panel.Backtest(ArgsList, CodeList, DataList, ModelID, VD = VD, DebugFC = DebugFC, Algo = Algo)
    DataList <- Output$DataList; CodeList <- Output$CodeList; ArgsList <- Output$ArgsList; dlname <- Output$dlname
    totals <- DataList[[paste0(ModelID, dlname)]][['data']]
    if(trial == 1L) update_row <- LM[`RMSE On` > -5][, .N] + 1L
    if(trial == 1L) {
      data.table::set(x = LM, i = eval(update_row), j = 'AvgError On', value = totals[.N, AvgError])
      data.table::set(x = LM, i = eval(update_row), j = 'RMSE On', value = totals[.N, RMSE])
      data.table::set(x = LM, i = eval(update_row), j = 'MAE On', value = totals[.N, MAE])
      data.table::set(x = LM, i = eval(update_row), j = 'MAPE On', value = totals[.N, MAPE])
      data.table::set(x = LM, i = eval(update_row), j = 'SMAPE On', value = totals[.N, SMAPE])
      if(RunCheck == 1L) {
        rowval <- LM[`RMSE Off` > -5, .N]
        data.table::set(x = LM, i = eval(update_row), j = 'AvgError Off', value = min(LM[eval(rowval), abs(`AvgError On`)], LM[eval(rowval), abs(`AvgError Off`)]))
        data.table::set(x = LM, i = eval(update_row), j = 'RMSE Off', value = min(LM[eval(rowval), `RMSE On`], LM[eval(rowval), `RMSE Off`]))
        data.table::set(x = LM, i = eval(update_row), j = 'MAE Off', value = min(LM[eval(rowval), `MAE On`], LM[eval(rowval), `MAE Off`]))
        data.table::set(x = LM, i = eval(update_row), j = 'MAPE Off', value = min(LM[eval(rowval), `MAPE On`], LM[eval(rowval), `MAPE Off`]))
        data.table::set(x = LM, i = eval(update_row), j = 'SMAPE Off', value = min(LM[eval(rowval), `SMAPE On`], LM[eval(rowval), `SMAPE Off`]))
      }
    } else {
      data.table::set(x = LM, i = eval(update_row), j = 'AvgError Off', value = totals[.N, AvgError])
      data.table::set(x = LM, i = eval(update_row), j = 'RMSE Off', value = totals[.N, RMSE])
      data.table::set(x = LM, i = eval(update_row), j = 'MAE Off', value = totals[.N, MAE])
      data.table::set(x = LM, i = eval(update_row), j = 'MAPE Off', value = totals[.N, MAPE])
      data.table::set(x = LM, i = eval(update_row), j = 'SMAPE Off', value = totals[.N, SMAPE])
    }
  }

  # Make permanent for remainder of Loop & Update LM
  act_lift <- LM[eval(update_row), `MAE On`] - LM[eval(update_row), `MAE Off`]
  if(act_lift < Lift) {
    ArgsList[[Test]] <- Plan[[Test]]$ArgsOn
  } else {
    ArgsList[[Test]] <- Plan[[Test]]$ArgsOff
  }

  return(list(
    LoopMetrics = LM,
    ArgsList = ArgsList
  ))
}

#' @title Shiny.FC.Panel.Backest.FeatureEval
#'
#' @description Test out various combinations of structure parameters to narrow the search space
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param LoopMetrics From function above
#' @param ArgsList List
#' @param CodeList From app
#' @param DataList From app
#' @param ModelID From Panel Forecasting Common Args Section of Shiny.FC.Trainer
#' @param Rollup = FALSE. If TRUE, a rollup is made from the _Raw data
#' @param Algo = 'catboost', 'xgboost', 'lightgbm'
#' @param VD ValidationData
#' @param DebugFC From app
#'
#' @export
Shiny.FC.Panel.Backest.FeatureEval <- function(LoopMetrics, ArgsList, CodeList, DataList, ModelID, Algo = 'catboost', VD, DebugFC = FALSE) {

  # Function requires VD to be available
  if(!data.table::is.data.table(VD)) {print('VD IS NOT A data.table !!!!! shutting it down'); return(list(DataList = DataList,ArgsList = ArgsList,CodeList = CodeList))}

  # Create ModelID if not defined
  if(missing(ModelID) || length(ModelID) == 0L || is.na(ModelID)) {
    ModelID <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_ModelID')]]}, error=function(x) NULL), Type='character', Default='FC001')
  }

  # Add to ArgsList if not there already
  if(length(ArgsList$ModelID) == 0L) ArgsList$ModelID <- ModelID

  # Switch args to do force CARMA to behave properly
  # TrainOnFull = TRUE --> forces function to generate a forecast, which we need to validation
  # SaveModel = FALSE --> we don't need to be saving these models, we just want to evaluate them
  if(DebugFC) print('Shiny.FC.Panel.Backest.FeatureEval 1')
  ArgsList$TrainOnFull <- TRUE
  ArgsList$Model <- NULL
  ArgsList$SaveModel <- FALSE
  if(DebugFC) print(paste0("ModelID == ", ModelID))

  # Feature Engineering Test
  Plan <- list()
  Plan[['Methods']] <- list(Method = 'Methods', ArgsOn = c('LogPlus1'), ArgsOff = "Identity")
  Plan[['Difference']] <- list(Method = 'Difference', ArgsOn = c(TRUE))
  Plan[['EncodingMethod']] <- list(Method = 'EncodingMethod', ArgsOn = 'credibility', ArgsOff = 'target_encoding')
  Plan[['CalendarVariables']] <- list(Method = 'CalendarVariables', ArgsOn = c(ArgsList$CalendarVariables))
  Plan[['HolidayVariable']] <- list(Method = 'HolidayVariable', ArgsOn = if(length(ArgsList$HolidayVariable) == 0L) c('USPublicHolidays','EasterGroup','ChristmasGroup','OtherEcclesticalFeasts') else ArgsList$HolidayVariable)
  Plan[['TimeWeights']] <- list(Method = 'TimeWeights', ArgsOn = if(ArgsList$TimeWeights == 1) 0.9995 else ArgsList$TimeWeights, ArgsOff = 1)
  Plan[['AnomalyDetection']] <- list(Method = 'AnomalyDetection', ArgsOn = list('tstat_high' = 6.25, 'tstat_low' = -6.25))
  Plan[['TimeTrendVariable']] <- list(Method = 'TimeTrendVariable', ArgsOn = c(TRUE))
  Plan[['Lags']] <- list(Method = 'Lags', ArgsOn = 1, ArgsOff = 0)

  # Reset args
  ArgsList$TargetTransformation <- FALSE
  ArgsList$Difference <- FALSE
  ArgsList$EncodingMethod <- 'target_encoding'
  ArgsList$ZeroPadSeries <- 'maxmax'
  ArgsList$CalendarVariables <- NULL
  ArgsList$HolidayVariable <- NULL
  ArgsList$TimeWeights <- 1
  ArgsList$AnomalyDetection <- NULL
  ArgsList$TimeTrendVariable <- FALSE
  ArgsList$Lags <- 0
  ArgsList$MA_Periods <- 0

  # Run BasicLoop.SingleTest for each of the feature engineering options you want tested
  if(DebugFC) {print('Shiny.FC.Panel.Backest.FeatureEval 2: Begin Loop'); print(paste0('VD Validation is data.table = ', data.table::is.data.table(VD)))}
  for(svs in seq_along(names(Plan))) {
    shiny::showNotification(paste("Begin running: ", names(Plan)[svs], " Test.."), duration = 300)
    if(DebugFC) {print('Run: BasicLoop.SingleTest()'); print(paste0('VD is data.table? :: ', data.table::is.data.table(VD)))}
    Output <- Quantico:::BasicLoop.SingleTest(LoopMetrics, Plan, ArgsList, DataList, CodeList, ModelID, VD = VD, DebugFC = DebugFC, Test = names(Plan)[svs], Algo = Algo)
    ArgsList <- Output$ArgsList; LoopMetrics <- Output$LoopMetrics
  }

  # Update DataList
  LoopMetrics[, RunNumber := seq_len(.N)]
  DataList <- Quantico:::DM.DataListUpdate(dl = DataList, dn = paste0(ModelID, "_FeatureEngineeringTest"), Sample = TRUE)

  # Code Collection
  CodeList <- tryCatch({Quantico::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    # Collection table
    "LoopMetrics <- data.table::data.table(\n  ",
    "'Feature Engineering' = c(\n    ",
    "'LogPlus1 vs None',\n    ",
    "'Series Differencing vs None',\n    ",
    "'Credibility vs Target Encoding'\n    ",
    "'Calendar Variables vs None'\n    ",
    "'Holiday Variables vs None'\n    ",
    "'Time Weights vs None'\n    ",
    "'Anomaly Detection vs None'\n    ",
    "'Time Trend Variable vs None'\n    ",
    "'Lag 1 vs None'\n  ",
    "),\n  ",
    "'AvgError On' = -5,\n  ",
    "'AvgError Off' = -5,\n  ",
    "'RMSE On' = -5,\n  ",
    "'RMSE Off' = -5,\n  ",
    "'MAE On' = -5,\n  ",
    "'MAE Off' = -5,\n  ",
    "'MAPE On' = -5,\n  ",
    "'MAPE Off' = -5,\n  ",
    "'SMAPE On' = -5,\n  ",
    "'SMAPE Off' = -5)\n  ",
    "\n\n",
    "# Encoding and Time Series fill\n",
    "Plan <- list()\n",
    "Plan[['Methods']] <- list(Method = 'Methods', ArgsOn = c('LogPlus1'), ArgsOff = 'Identity')\n",
    "Plan[['Difference']] <- list(Method = 'Difference', ArgsOn = c(TRUE))\n",
    "Plan[['EncodingMethod']] <- list(Method = 'EncodingMethod', ArgsOn = 'credibility', ArgsOff = 'target_encoding')\n",
    "Plan[['CalendarVariables']] <- list(Method = 'CalendarVariables', ArgsOn = c(ArgsList$CalendarVariables))\n",
    "Plan[['HolidayVariable']] <- list(Method = 'HolidayVariable', ArgsOn = ArgsList$HolidayVariable)\n",
    "Plan[['TimeWeights']] <- list(Method = 'TimeWeights', ArgsOn = c(1,0.9995))\n",
    "Plan[['AnomalyDetection']] <- list(Method = 'AnomalyDetection', ArgsOn = list('tstat_high' = 7, 'tstat_low' = -7))\n",
    "Plan[['TimeTrendVariable']] <- list(Method = 'TimeTrendVariable', ArgsOn = c(TRUE))\n",
    "Plan[['Lags']] <- list(Method = 'Lags', ArgsOn = 1)\n\n",
    "# Reset args\n",
    "ArgsList$TargetTransformation <- FALSE\n",
    "ArgsList$Difference <- FALSE\n",
    "ArgsList$EncodingMethod <- 'target_encoding'\n",
    "ArgsList$CalendarVariables <- NULL\n",
    "ArgsList$HolidayVariable <- NULL\n",
    "ArgsList$TimeWeights <- 1\n",
    "ArgsList$AnomalyDetection <- NULL\n",
    "ArgsList$TimeTrendVariable <- FALSE\n",
    "ArgsList$Lags <- 0\n",
    "ArgsList$MA_Periods <- 0\n\n",
    "# Loop through builds\n",
    "if(DebugFC) print('Shiny.FC.Panel.Backest.FeatureEval 2: Begin Loop')\n",
    "CodeList <- list()\n",
    "DebugFC <- FALSE\n",
    "for(svs in seq_along(names(Plan))) {\n  ",
    "Output <- Quantico:::BasicLoop.SingleTest(LoopMetrics, Plan, ArgsList, DataList, CodeList, ModelID, ValidationData, DebugFC, Test = names(Plan)[svs], Algo = ", Quantico:::CEP(Algo), ")\n  ",
    "ArgsList <- Output$ArgsList; LoopMetrics <- Output$LoopMetrics\n  ",
    "}\n\n",
    "# Store LoopMetrics in DataList \n",
    "mcn <- paste0(ModelID, '_FeatureEngineeringTest')\n",
    "DataList[[mcn]][['data']] <- LoopMetrics\n",
    "DataList <- Quantico:::DM.DataListUpdate(dl = DataList, dn = mcn, Sample = FALSE)\n"))}, error = function(x) CodeList)

  if(DebugFC) print('Shiny.FC.Panel.Backest.FeatureEval finished')
  if(DebugFC) print(LoopMetrics)
  return(list(
    DataList = DataList,
    ArgsList = ArgsList,
    CodeList = CodeList,
    LoopMetrics = LoopMetrics
  ))
}

#' @title Shiny.FC.Panel.Backest.RollingEval
#'
#' @description Goal of this function is to get a better understanding of the cost tradeoffs for different model configurations
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param ArgsList List
#' @param CodeList From app
#' @param DataList From app
#' @param ModelID From Panel Forecasting Common Args Section of Shiny.FC.Trainer
#' @param VD ValidationData
#' @param mu Model updating
#' @param vu Values updating
#' @param vuf Value update frequency = every nth FC_Period. E.g., update data daily vs weekly
#' @param muf Model update frequency = every nth FC_Period
#' @param Algo = 'catboost', 'xgboost', 'lightgbm'
#' @param DebugFC From app
#'
#' @export
Shiny.FC.Panel.Backest.RollingEval <- function(ArgsList, DataList, CodeList, ModelID, VD = NULL, mu = TRUE, vu = TRUE, vuf = 1, muf = 1, Algo = 'catboost', DebugFC = FALSE)  {

  # Determine if I should remove the _BT_Raw and _BT_Rollup data sets
  #   If they already existed before this process begins, then keep them
  #   Otherwise, remove them because they are generated
  if(data.table::is.data.table(DataList[[paste0(ModelID, '_CE_Raw')]])) {
    RemoveData <- FALSE
  } else {
    RemoveData <- TRUE
  }
  if(DebugFC) print('Shiny.FC.Panel.Backest.RollingEval 1')
  if(!data.table::is.data.table(VD)) {
    if(DebugFC) print('Shiny.FC.Panel.Backest.RollingEval(): VD is NOT a data.table')
    return(list(
      DataList = DataList,
      ArgsList = ArgsList,
      CodeList = CodeList
    ))
  }
  if(missing(ModelID) || length(ModelID) == 0L || is.na(ModelID)) {
    ModelID <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_ModelID')]]}, error=function(x) NULL), Type='character', Default='FC001')
    ArgsList$ModelID <- ModelID
  }

  # Setup loop
  Iter <- seq(1L, ArgsList$FC_Periods, 1)
  ArgsList$FC_Periods <- 1

  # Run Cross Eval Procedure
  # run = 1L  run = 2L
  ArgsList$DebugMode <- FALSE
  for(run in Iter) {
    if(run > 1L) ArgsList$data <- VD[get(ArgsList$DateColumnName) <= DataList[[paste0(ModelID, '_CE_Raw')]][['data']][, max(get(ArgsList$DateColumnName), na.rm = TRUE)]]
    if(mu) ArgsList$Model <- NULL
    Output <- Quantico::Shiny.FC.Panel.Backtest(ArgsList, CodeList, DataList, ModelID, VD = VD, DebugFC = DebugFC, CrossEval = TRUE, Algo = Algo)
    DataList <- Output$DataList; CodeList <- Output$CodeList; ArgsList <- Output$ArgsList
    ArgsList$FC_Periods <- 1
    if(run == 1L) {
      FinalTable <- data.table::copy(DataList[[paste0(ModelID, '_CE_Raw')]][['data']])
      if(DebugFC) print(paste0('Run Number: ', run))
    } else {
      if(DebugFC) print(paste0('Run Number: ', run))
      maxD <- FinalTable[, max(get(ArgsList[['DateColumnName']]), na.rm = TRUE)]
      FinalTable <- data.table::rbindlist(list(
        FinalTable,
        DataList[[paste0(ModelID, '_CE_Raw')]][['data']][get(ArgsList[['DateColumnName']]) > eval(maxD)]),
        use.names = TRUE, fill = TRUE)
    }
  }

  # _CE_Rollup
  rm(Output)
  DataList[[paste0(ModelID, '_CE_Raw')]][['data']] <- FinalTable
  DataList <- Quantico::Shiny.FC.Panel.Metrics.Agg(DataList, ArgsList, ModelID, GroupVariables = NULL, CrossEval = TRUE, DebugFC = DebugFC)

  # Return
  return(list(
    DataList = DataList,
    ArgsList = ArgsList,
    CodeList = CodeList
  ))
}

#' @title Shiny.FC.CARMA
#'
#' @description Shiny forecasting function
#'
#' @details # There are 5 run modes for this function
#'
#' -'Train New Model': prepare for backtesting / forecasting; inspect ML performance
#'
#' -'Retrain Existing Model': Load model, new data, but don't need forecast yet
#'
#' -'Backtest': evaluate model in variety of ways
#'
#' -'Forecast': generate a forecast from a trained model; either loaded or created in session
#'
#' -'Forecast+Retrain': generate an updated model and use that model to generate a forecast
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param input shiny input
#' @param output shiny output
#' @param DataList DataList stores data in app
#' @param ArgsList ArgsList
#' @param CodeList CodeList from app
#' @param DebugFC DebugFC from app
#' @param Algo 'CatBoost', 'XGBoost', 'LightGBM', 'H2O_DRF', 'H2O_GBM'
#'
#' @return a list of columns names by data type
#'
#' @export
Shiny.FC.CARMA <- function(input,
                           output,
                           DataList,
                           ArgsList,
                           CodeList,
                           DebugFC,
                           TabCount = 5L,
                           Algo = 'CatBoost',
                           wd = NULL) {

  # Default
  if(length(wd) > 0L) {
    if(DebugFC) print(wd)
    ArgsList[['SaveDataPath']] <- wd
  } else {
    if(DebugFC) print('wd is NULL')
  }

  # There are 5 run modes for this function
  # 'Train New Model': prepare for backtesting / forecasting; inspect ML performance
  # 'Retrain Existing Model': Load model, new data, but don't need forecast yet
  # 'Backtest': evaluate model in variety of ways
  # 'Forecast': generate a forecast from a trained model; either loaded or created in session
  # 'Forecast+Retrain': generate an updated model and use that model to generate a forecast

  # Prepare for any RunType
  temp_data <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_TrainData')]]}, error=function(x) NULL), Type='character', Default=NULL)
  xregs <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_XREGS')]]}, error=function(x) NULL), Type='character', Default=NULL)
  ArgsList[["XREGS"]] <- xregs
  RunMode <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_RunMode')]]}, error=function(x) NULL), Type='character', Default='Train New Model')
  ModelID <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'ModelID')]]}, error=function(x) NULL), Type='character', Default="FC001")

  if(DebugFC) print('Shiny.FC.CARMA 1')

  if(ModelID == "") ModelID <- 'FC001'
  ArgsList[['ModelID']] <- ModelID
  ArgsList[['FC_Periods']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_FCPeriods')]]}, error=function(x) NULL), Type='numeric', Default=5)
  ArgsList[['DateColumnName']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_DateColumnName')]]}, error=function(x) NULL), Type='character', Default=NULL)
  ArgsList[['TimeUnit']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_TimeUnit')]]}, error=function(x) NULL), Type='character', Default=NULL)

  if(DebugFC) print('Shiny.FC.CARMA 2')

  if(DebugFC) print(rep(RunMode, 10))

  # *************************************
  # Forecast
  # *************************************
  if(RunMode == 'Forecast' || RunMode == 'Forecast+Retrain') {

    if(RunMode == 'Forecast+Retrain') {
      ArgsList$Model <- NULL
    }

    # Build FC
    if(DebugFC) print(rep('Forecast', 10))
    if(length(temp_data) > 0L) {
      Output <- Quantico::Shiny.FC.Panel.Forecast(temp_data, ArgsList, CodeList, DataList, ModelID, VD = NULL, DebugFC = DebugFC, Algo = Algo)
      DataList <- Output$DataList; CodeList <- Output$CodeList; rm(Output)
      for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
      for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    } else {
      if(DebugFC) print('Forecast: NewData is not Defined')
    }

    # Return results
    return(list(
      DataList = DataList,
      CodeList = CodeList,
      ArgsList = ArgsList,
      RunMode = RunMode,
      ModelID = ModelID
    ))
  }

  # *************************************
  # Retrain
  # *************************************
  if(RunMode == 'Retrain Existing Model') {

    # Build FC
    if(DebugFC) print(rep('Retrain', 10))
    if(length(temp_data) > 0L) {
      Output <- Quantico::Shiny.FC.Panel.Retrain(temp_data, ArgsList, CodeList, DataList, DebugFC = DebugFC, Algo = Algo)
      ArgsList <- Output$ArgsList; DataList <- Output$DataList; CodeList <- Output$CodeList; rm(Output)
      for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
      for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    } else {
      if(DebugFC) print('NewData is not Defined')
    }

    # Return results
    return(list(
      DataList = DataList,
      CodeList = CodeList,
      ArgsList = ArgsList,
      RunMode = RunMode,
      ModelID = ModelID
    ))
  }

  # ArgsList
  if(length(temp_data) > 0L) ArgsList[['data']] <- data.table::copy(DataList[[temp_data]][['data']]) else ArgsList[['data']] <- NULL

  if(DebugFC) print('Shiny.FC.CARMA 3')

  temp_validation <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_ValidationData')]]}, error=function(x) NULL), Type='character', Default=NULL)

  if(DebugFC) print('Shiny.FC.CARMA 4')

  if(length(temp_validation) > 0L && temp_validation != temp_data) {
    ValidationData <- data.table::copy(DataList[[temp_validation]][['data']])
  } else {
    ValidationData <- NULL
  }

  if(DebugFC) print('Shiny.FC.CARMA 5')

  # If no validation data was selected and some sort of backtesting procedure is requested
  if(length(temp_validation) == 0L && RunMode %in% c('Backtest', 'Feature Engineering Test', 'Backtest Cross Eval')) {

    if(DebugFC) print('Shiny.FC.CARMA 6')

    # Ensure date or posix class for date variable
    if(ArgsList[['TimeUnit']] %in% c("Daily", "Weekly", "Monthly", 'Quarterly', 'Yearly')) {
      ArgsList[['data']][, eval(ArgsList[['DateColumnName']]) := as.Date(get(ArgsList[['DateColumnName']]))]
      if(data.table::is.data.table(ValidationData)) {
        ValidationData[, eval(ArgsList[['DateColumnName']]) := as.Date(get(ArgsList[['DateColumnName']]))]
      }
    } else {
      ArgsList[['data']][, eval(ArgsList[['DateColumnName']]) := as.POSIXct(get(ArgsList[['DateColumnName']]))]
      if(data.table::is.data.table(ValidationData)) {
        ValidationData[, eval(ArgsList[['DateColumnName']]) := as.POSIXct(get(ArgsList[['DateColumnName']]))]
      }
    }

    if(DebugFC) print('Shiny.FC.CARMA 7')

    library(lubridate)
    if(ArgsList[['TimeUnit']] == "1-Minute") {
      ValidationData <- data.table::copy(ArgsList[['data']])
      ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::minutes(eval(ArgsList[['FC_Periods']]))]
    } else if(ArgsList[['TimeUnit']] == "5-Minutes") {
      ValidationData <- data.table::copy(ArgsList[['data']])
      ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::minutes(eval(ArgsList[['FC_Periods']]) * 5)]
    } else if(ArgsList[['TimeUnit']] == "10-Minutes") {
      ValidationData <- data.table::copy(ArgsList[['data']])
      ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::minutes(eval(ArgsList[['FC_Periods']]) * 10)]
    } else if(ArgsList[['TimeUnit']] == "15-Minutes") {
      ValidationData <- data.table::copy(ArgsList[['data']])
      ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::minutes(eval(ArgsList[['FC_Periods']]) * 15)]
    } else if(ArgsList[['TimeUnit']] == "30-Minutes") {
      ValidationData <- data.table::copy(ArgsList[['data']])
      ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::minutes(eval(ArgsList[['FC_Periods']]) * 30)]
    } else if(ArgsList[['TimeUnit']] == "Hourly") {
      ValidationData <- data.table::copy(ArgsList[['data']])
      ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::hours(eval(ArgsList[['FC_Periods']]))]
    } else if(ArgsList[['TimeUnit']] == "Daily") {
      ValidationData <- data.table::copy(ArgsList[['data']])
      ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::days(eval(ArgsList[['FC_Periods']]))]
    } else if(ArgsList[['TimeUnit']] == "Weekly") {
      ValidationData <- data.table::copy(ArgsList[['data']])
      ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::weeks(eval(ArgsList[['FC_Periods']]))]
    } else if(ArgsList[['TimeUnit']] == "Monthly") {
      ValidationData <- data.table::copy(ArgsList[['data']])
      ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) %m-% lubridate::months(eval(ArgsList[['FC_Periods']]))]
    } else if(ArgsList[['TimeUnit']] == "Quarterly") {
      ValidationData <- data.table::copy(ArgsList[['data']])
      ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) %m-% lubridate::months(eval(ArgsList[['FC_Periods']]) * 3)]
    } else if(ArgsList[['TimeUnit']] == "Yearly") {
      ValidationData <- data.table::copy(ArgsList[['data']])
      ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::years(eval(ArgsList[['FC_Periods']]))]
    }
  }

  if(DebugFC) print('Shiny.FC.CARMA 8')

  # *************************************
  # Backtest No Retrain
  # *************************************
  if(RunMode == 'Backtest' && data.table::is.data.table(ValidationData) && length(ArgsList[['Model']]) > 0L) {

    ValidationDataCheck <- TRUE

    if(DebugFC) print('Shiny.FC.CARMA 8.a')

    # Backtest CatBoost for Forecasting Purposes
    if(DebugFC) {print('Backtest 1st version'); print(ArgsList[['Model']])}
    Output <- Quantico::Shiny.FC.Panel.Backtest(ArgsList, CodeList, DataList, ModelID, VD = ValidationData, DebugFC = DebugFC, Algo = Algo)
    DataList <- Output$DataList; CodeList <- Output$CodeList; ArgsList <- Output$ArgsList
    for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
    for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)

    # Return results
    if(DebugFC) {print('Return from Shiny.FC.CARMA')}
    return(list(
      DataList = DataList,
      CodeList = CodeList,
      ArgsList = ArgsList,
      RunMode = RunMode,
      ModelID = ModelID
    ))
  } else {
    ValidationDataCheck <- FALSE
  }

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Common Args                           ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Key Variables

  if(DebugFC) print('Shiny.FC.CARMA 9')

  ArgsList[['TargetColumnName']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_TargetColumnName')]]}, error=function(x) NULL), Type='character', Default=NULL)
  ArgsList[['GroupVariables']]   <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_GroupVariables')]]}, error=function(x) NULL), Type='character', Default=NULL)

  # FC Time Periods Update

  if(DebugFC) print('Shiny.FC.CARMA 10')

  if(length(ArgsList[['TargetColumnName']]) == 0L || length(ArgsList[['DateColumnName']]) == 0L) {
    return(list(
      DataList = DataList,
      CodeList = CodeList,
      ArgsList = ArgsList,
      RunMode = RunMode,
      ModelID = ModelID
    ))
  }

  if(DebugFC) print('Shiny.FC.CARMA 11')
  ArgsList <- Quantico::FC.FCPeriods(ArgsList = ArgsList, VD = ValidationData) # Ensure FC_Periods is proper given ValidationData

  # Time Unit
  if(DebugFC) print('Shiny.FC.CARMA 12')
  if(length(temp_data) == 0L || length(ArgsList) == 0L || length(ArgsList[['TimeUnit']]) == 0L) {
    return(list(
      DataList = DataList,
      CodeList = CodeList,
      ArgsList = ArgsList,
      RunMode = RunMode,
      ModelID = ModelID
    ))
  }

  if(DebugFC) print('Shiny.FC.CARMA 13')
  ArgsList <- Quantico::FC.TimeUnit(ArgsList) # Convert UI input selected value to value needed by AutoCatBoostCARMA()
  ArgsList[['TimeGroups']] <- ArgsList[['TimeUnit']]
  if(tolower(Algo) == 'catboost') ArgsList[['TimeWeights']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_TimeWeights')]]}, error=function(x) NULL), Type='numeric', Default=1)
  ArgsList[['Methods']]     <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_Methods')]]}, error=function(x) NULL), Type='character', Default=NULL)
  ArgsList[['Difference']]  <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_Difference')]]}, error=function(x) NULL), Type='logical', Default=FALSE)
  if(length(Quantico:::CEP(ArgsList[['Methods']])) > 0L && tolower(Quantico:::CEP(ArgsList[['Methods']])) %in% c('','identity')) ArgsList[['TargetTransformation']] <- FALSE else ArgsList[['TargetTransformation']] <- TRUE

  # Code collection
  CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Forecasting: Data Args\n",
    "ModelID <- ", Quantico:::CEP(ModelID), "\n",
    "if(!exists('ArgsList')) ArgsList <- list()\n",
    "ArgsList[['data']] <- data.table::copy(DataList[[", Quantico:::CEP(temp_data), "]])\n",
    if(length(temp_validation) > 0L && temp_validation != temp_data) paste0("ValidationData <- data.table::copy(DataList[[", Quantico:::CEP(temp_validation), "]])\n") else paste0("ValidationData <- NULL\n"), "\n",
    "ArgsList[['TargetColumnName']] <- ", Quantico:::CEP(ArgsList[['TargetColumnName']]), "\n",
    "ArgsList[['DateColumnName']] <- ", Quantico:::CEP(ArgsList[['DateColumnName']]), "\n",
    "ArgsList[['GroupVariables']] <- ", Quantico:::ExpandText(ArgsList[['GroupVariables']]), "\n",
    "ArgsList[['TimeUnit']] <- ", Quantico:::CEP(ArgsList[['TimeUnit']]), "\n",
    "ArgsList[['TimeGroups']] <- ArgsList[['TimeUnit']]\n",
    "ArgsList[['FC_Periods']] <- ", Quantico:::CEPP(ArgsList[['FC_Periods']]), "\n",
    "ArgsList[['TimeWeights']] <- ", Quantico:::CEPP(ArgsList[['TimeWeights']]), "\n",
    "ArgsList[['TargetTransformation']] <- ", Quantico:::CEPP(ArgsList[['TargetTransformation']]), "\n",
    "ArgsList[['Methods']] <- ", Quantico:::CEP(ArgsList[['Methods']]), "\n",
    "ArgsList[['Difference']] <- ", Quantico:::CEPP(ArgsList[['Difference']]), "\n"))}, error = function(x) CodeList)

  # If no validation data was selected and some sort of backtesting procedure is requested
  if(ValidationDataCheck && RunMode %in% c('Backtest', 'Feature Engineering Test', 'Backtest Cross Eval')) {
    if(ArgsList[['TimeUnit']] == "1-Minute") {
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# FC Create Validation Data\n",
        "if(!exists('ArgsList') ArgsList <- list()\n",
        "ValidationData <- data.table::copy(ArgsList[['data']])\n",
        "ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::minutes(eval(ArgsList[['FC_Periods']]))]\n"))
    } else if(ArgsList[['TimeUnit']] == "5-Minutes") {
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# FC Create Validation Data\n",
        "if(!exists('ArgsList') ArgsList <- list()\n",
        "ValidationData <- data.table::copy(ArgsList[['data']])\n",
        "ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::minutes(eval(ArgsList[['FC_Periods']]) * 5)]\n"))
    } else if(ArgsList[['TimeUnit']] == "10-Minutes") {
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# FC Create Validation Data\n",
        "if(!exists('ArgsList') ArgsList <- list()\n",
        "ValidationData <- data.table::copy(ArgsList[['data']])\n",
        "ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::minutes(eval(ArgsList[['FC_Periods']]) * 10)]\n"))
    } else if(ArgsList[['TimeUnit']] == "15-Minutes") {
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# FC Create Validation Data\n",
        "if(!exists('ArgsList') ArgsList <- list()\n",
        "ValidationData <- data.table::copy(ArgsList[['data']])\n",
        "ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::minutes(eval(ArgsList[['FC_Periods']]) * 15)]\n"))
    } else if(ArgsList[['TimeUnit']] == "30-Minutes") {
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# FC Create Validation Data\n",
        "if(!exists('ArgsList') ArgsList <- list()\n",
        "ValidationData <- data.table::copy(ArgsList[['data']])\n",
        "ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::minutes(eval(ArgsList[['FC_Periods']]) * 30)]\n"))
    } else if(ArgsList[['TimeUnit']] == "Hourly") {
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# FC Create Validation Data\n",
        "if(!exists('ArgsList') ArgsList <- list()\n",
        "ValidationData <- data.table::copy(ArgsList[['data']])\n",
        "ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::hours(eval(ArgsList[['FC_Periods']]))]\n"))
    } else if(ArgsList[['TimeUnit']] == "Daily") {
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# FC Create Validation Data\n",
        "if(!exists('ArgsList') ArgsList <- list()\n",
        "ValidationData <- data.table::copy(ArgsList[['data']])\n",
        "ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::days(eval(ArgsList[['FC_Periods']]))]\n"))
    } else if(ArgsList[['TimeUnit']] == "Weekly") {
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# FC Create Validation Data\n",
        "if(!exists('ArgsList') ArgsList <- list()\n",
        "ValidationData <- data.table::copy(ArgsList[['data']])\n",
        "ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::weeks(eval(ArgsList[['FC_Periods']]))]\n"))
    } else if(ArgsList[['TimeUnit']] == "Monthly") {
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# FC Create Validation Data\n",
        "if(!exists('ArgsList') ArgsList <- list()\n",
        "ValidationData <- data.table::copy(ArgsList[['data']])\n",
        "ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) %m-% lubridate::months(eval(ArgsList[['FC_Periods']]))]\n"))
    } else if(ArgsList[['TimeUnit']] == "Quarterly") {
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# FC Create Validation Data\n",
        "if(!exists('ArgsList') ArgsList <- list()\n",
        "ValidationData <- data.table::copy(ArgsList[['data']])\n",
        "ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) %m-% lubridate::months(eval(ArgsList[['FC_Periods']]) * 3)]\n"))
    } else if(ArgsList[['TimeUnit']] == "Yearly") {
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# FC Create Validation Data\n",
        "if(!exists('ArgsList') ArgsList <- list()\n",
        "ValidationData <- data.table::copy(ArgsList[['data']])\n",
        "ArgsList[['data']] <- ArgsList[['data']][get(ArgsList[['DateColumnName']]) < max(get(ArgsList[['DateColumnName']])) - lubridate::years(eval(ArgsList[['FC_Periods']]))]\n"))
    }
  }

  # Production Args
  if(DebugFC) print('Shiny.FC.CARMA 14')
  if(tolower(Algo) == 'catboost') {
    ArgsList[['TaskType']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_TaskType')]]}, error=function(x) NULL), Type='character', Default='CPU')
    ArgsList[['NumGPU']]   <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_NumGPU')]]}, error=function(x) NULL), Type='numeric', Default=1)
  } else if(tolower(Algo) == 'xgboost') {
    bb <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_TaskType')]]}, error=function(x) NULL), Type='character', Default='CPU')
    if(tolower(bb) !='gpu') ArgsList[['TreeMethod']] <- 'hist' else ArgsList[['TreeMethod']] <- 'gpu_hist'
  }

  if(DebugFC) print('Shiny.FC.CARMA 15')
  ArgsList[['RoundPreds']]      <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_RoundPreds')]]}, error=function(x) NULL), Type='logical', Default=FALSE)
  ArgsList[['NonNegativePred']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_NonNegativePred')]]}, error=function(x) NULL), Type='logical', Default=FALSE)
  ArgsList[['SaveModel']]       <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_SaveModel')]]}, error=function(x) NULL), Type='logical', Default=FALSE)
  ArgsList[['ZeroPadSeries']]   <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_ZeroPadSeries')]]}, error=function(x) NULL), Type='character', Default='dynamic:target_encode')
  ArgsList[['DebugMode']] <- DebugFC

  # Code collection
  CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Forecasting: Production Args\n",
    "ArgsList[['TaskType']] <- ", Quantico:::CEP(ArgsList[['TaskType']]), "\n",
    "ArgsList[['NumGPU']] <- ", Quantico:::CEPP(ArgsList[['NumGPU']]), "\n",
    "ArgsList[['RoundPreds']] <- ", Quantico:::CEPP(ArgsList[['RoundPreds']]), "\n",
    "ArgsList[['NonNegativePred']] <- ", Quantico:::CEPP(ArgsList[['NonNegativePred']]), "\n",
    "ArgsList[['SaveModel']] <- ", Quantico:::CEPP(ArgsList[['SaveModel']]), "\n",
    "ArgsList[['DebugMode']] <- ", Quantico:::CEPP(ArgsList[['DebugMode']]), "\n",
    "ArgsList[['ZeroPadSeries']] <- ", Quantico:::CEP(ArgsList[['ZeroPadSeries']]), "\n"))}, error = function(x) CodeList)

  if(DebugFC) print('Shiny.FC.CARMA 16')
  ArgsList[['CalendarVariables']]  <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_CalendarVariables')]]}, error=function(x) NULL), Type='character', Default=NULL)
  ArgsList[['HolidayVariable']]    <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_HolidayVariables')]]}, error=function(x) NULL), Type='character', Default=NULL)
  ArgsList[['HolidayLookback']]    <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_Lookback')]]}, error=function(x) NULL), Type='numeric', Default=1)
  ArgsList[['Quantiles_Selected']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_Quantiles_Selected')]]}, error=function(x) NULL), Type='character', Default=NULL)
  ArgsList[['Lags']]               <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_Lags')]]}, error=function(x) NULL), Type='numeric', Default=NULL)
  ArgsList[['MA_Periods']]         <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_MovingAverages')]]}, error=function(x) NULL), Type='numeric', Default=NULL)
  ArgsList[['SD_Periods']]         <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_MovingSD')]]}, error=function(x) NULL), Type='numeric', Default=NULL)
  ArgsList[['Kurt_Periods']]       <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_MovingKurt')]]}, error=function(x) NULL), Type='numeric', Default=NULL)
  ArgsList[['Skew_Periods']]       <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_MovingSkew')]]}, error=function(x) NULL), Type='numeric', Default=NULL)
  ArgsList[['Quantile_Periods']]   <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_MovingQuantiles')]]}, error=function(x) NULL), Type='numeric', Default=NULL)
  ArgsList[['DataTruncate']]       <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_DataTruncate')]]}, error=function(x) NULL), Type='logical', Default=FALSE)
  ArgsList[['TimeTrendVariable']]  <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_TimeTrend')]]}, error=function(x) NULL), Type='logical', Default=FALSE)
  ArgsList[['EncodingMethod']]     <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_EncodingMethod')]]}, error=function(x) NULL), Type='character', Default='meow')
  AnomalyDetection_LowThreshold    <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_AnomalyDetection_LowThreshold')]]}, error=function(x) NULL), Type='numeric', Default=NULL)
  AnomalyDetection_HighThreshold   <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_AnomalyDetection_HighThreshold')]]}, error=function(x) NULL), Type='numeric', Default=NULL)

  if(DebugFC) print('Shiny.FC.CARMA 17')
  if(length(AnomalyDetection_HighThreshold) > 0L && length(AnomalyDetection_LowThreshold) > 0L) {
    ArgsList[['AnomalyDetection']] <- list('tstat_high' = AnomalyDetection_HighThreshold, 'tstat_low' = AnomalyDetection_LowThreshold)
  }

  # Code collection
  CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Forecasting: Feature Engineering Args\n",
    "ArgsList[['CalendarVariables']] <- ", Quantico:::ExpandText(ArgsList[['CalendarVariables']]), "\n",
    "ArgsList[['HolidayVariable']] <- ", Quantico:::ExpandText(ArgsList[['HolidayVariables']]), "\n",
    "ArgsList[['HolidayLookback']] <- ", Quantico:::CEPP(ArgsList[['Lookback']]), "\n",
    "ArgsList[['Quantiles_Selected']] <- ", Quantico:::ExpandText(ArgsList[['Quantiles_Selected']]), "\n",

    "ArgsList[['Lags']] <- ", Quantico:::ExpandText(ArgsList[['Lags']]), "\n",
    "ArgsList[['MA_Periods']] <- ", Quantico:::ExpandText(ArgsList[['MovingAverages']]), "\n",
    "ArgsList[['SD_Periods']] <- ", Quantico:::ExpandText(ArgsList[['MovingSD']]), "\n",
    "ArgsList[['Kurt_Periods']] <- ", Quantico:::ExpandText(ArgsList[['MovingKurt']]), "\n",
    "ArgsList[['Skew_Periods']] <- ", Quantico:::ExpandText(ArgsList[['MovingSkew']]), "\n",
    "ArgsList[['Quantile_Periods']] <- ", Quantico:::ExpandText(ArgsList[['MovingQuantiles']]), "\n",

    "ArgsList[['DataTruncate']] <- ", Quantico:::CEPP(ArgsList[['DataTruncate']]), "\n",
    "ArgsList[['TimeTrend']] <- ", Quantico:::CEPP(ArgsList[['TimeTrend']]), "\n",

    "AnomalyDetection_LowThreshold <- ", Quantico:::CEPP(AnomalyDetection_LowThreshold), "\n",
    "AnomalyDetection_HighThreshold <- ", Quantico:::CEPP(AnomalyDetection_LowThreshold), "\n",
    "if(length(AnomalyDetection_HighThreshold) > 0L && length(AnomalyDetection_LowThreshold) > 0L) {\n  ",
    "ArgsList[['AnomalyDetection']] <- list('tstat_high' = ", Quantico:::CEPP(AnomalyDetection_HighThreshold), ", 'tstat_low' = ", Quantico:::CEPP(AnomalyDetection_LowThreshold), ")\n",
    "}\n"))}, error = function(x) CodeList)

  # @@@@@@@@@@@@@@@@@@@@@@@@
  # ML Specific Algo Args
  # @@@@@@@@@@@@@@@@@@@@@@@@

  # CatBoost Args
  if(DebugFC) print('Shiny.FC.CARMA 18')
  if(tolower(Algo) == 'catboost') {
    ArgsList[['EvalMetric']]           <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_EvalMetric']]}, error = function(x) NULL), Type = 'character', Default = 'RMSE')
    ArgsList[['LossFunction']]         <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_LossFunction']]}, error = function(x) NULL), Type = 'character', Default = 'RMSE')
    ArgsList[['NTrees']]               <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_NTrees']]}, error=function(x) NULL), Type='numeric', Default=500)
    ArgsList[['Depth']]                <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_Depth']]}, error=function(x) NULL), Type='numeric', Default=6)
    ArgsList[['EvalMetricValue']]      <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_EvalMetricValue']]}, error = function(x) NULL), Type='numeric', Default = 1.2)
    ArgsList[['LossFunctionValue']]    <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_LossFunctionValue']]}, error = function(x) NULL), Type='numeric', Default = 1.2)
    ArgsList[['Langevin']]             <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_Langevin']]}, error=function(x) NULL), Type='logical', Default=FALSE)
    ArgsList[['DiffusionTemperature']] <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_DiffusionTemperature']]}, error=function(x) NULL), Type='numeric', Default=10000)
    ArgsList[['L2_Leaf_Reg']]          <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_L2_Leaf_Reg']]}, error=function(x) NULL), Type='numeric', Default=NULL)
    ArgsList[['ModelSizeReg']]         <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_ModelSizeReg']]}, error=function(x) NULL), Type='numeric', Default=0.50)
    ArgsList[['MinDataInLeaf']]        <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_MinDataInLeaf']]}, error=function(x) NULL), Type='numeric', Default=2)
    ArgsList[['LearningRate']]         <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_LearningRate']]}, error=function(x) NULL), Type='numeric', Default=NULL)
    ArgsList[['BootStrapType']]        <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_BootStrapType']]}, error=function(x) NULL), Type='character', Default='Bayesian')
    ArgsList[['GrowPolicy']]           <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_GrowPolicy']]}, error=function(x) NULL), Type='character', Default='SymmetricTree')
    ArgsList[['RandomStrength']]       <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_RandomStrength']]}, error=function(x) NULL), Type='numeric', Default=0.90)
    ArgsList[['RSM']]                  <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_RSM']]}, error=function(x) NULL), Type='numeric', Default=0.90)
    ArgsList[['SubSample']]            <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_SubSample']]}, error=function(x) NULL), Type='numeric', Default=0.80)
    ArgsList[['BorderCount']]          <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_BorderCount']]}, error=function(x) NULL), Type='numeric', Default=128)
    ArgsList[['FeatureBorderType']]    <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_FeatureBorderType']]}, error=function(x) NULL), Type='character', Default='GreedyLogSum')
    ArgsList[['ScoreFunction']]        <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoostCARMA_ScoreFunction']]}, error=function(x) NULL), Type='character', Default='Cosine')
  }

  # XGBoost Args
  if(DebugFC) print('Shiny.FC.CARMA 19')
  if(tolower(Algo) == 'xgboost') {
    ArgsList[['EvalMetric']]      <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoostCARMA_EvalMetric']]}, error = function(x) NULL), Type = 'character', Default = 'RMSE')
    ArgsList[['LossFunction']]    <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoostCARMA_LossFunction']]}, error = function(x) NULL), Type = 'character', Default = 'reg:squarederror')
    ArgsList[['NTrees']]          <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoostCARMA_NTrees']]}, error=function(x) NULL), Type='numeric', Default=500)
    ArgsList[['LearningRate']]    <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoostCARMA_LearningRate']]}, error=function(x) NULL), Type='numeric', Default=0.20)
    ArgsList[['MaxDepth']]        <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoostCARMA_MaxDepth']]}, error=function(x) NULL), Type='numeric', Default=6)
    ArgsList[['MinChildWeight']]  <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoostCARMA_MinChildWeight']]}, error=function(x) NULL), Type='numeric', Default=1)
    ArgsList[['SubSample']]       <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoostCARMA_SubSample']]}, error=function(x) NULL), Type='numeric', Default=0.85)
    ArgsList[['ColSampleByTree']] <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoostCARMA_ColSampleByTree']]}, error=function(x) NULL), Type='numeric', Default=1.0)
    ArgsList[['alpha']]           <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoostCARMA_alpha']]}, error=function(x) NULL), Type='numeric', Default=4)
    ArgsList[['lambda']]          <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoostCARMA_lambda']]}, error=function(x) NULL), Type='numeric', Default=4)
  }

  # LightGBM Args
  if(DebugFC) print('Shiny.FC.CARMA 20')
  if(tolower(Algo) == 'lightgbm') {
    ArgsList[['EvalMetric']]              <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBMCARMA_EvalMetric']]}, error = function(x) NULL), Type = 'character', Default = 'RMSE')
    ArgsList[['LossFunction']]            <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBMCARMA_LossFunction']]}, error = function(x) NULL), Type = 'character', Default = 'reg:squarederror')
    ArgsList[['Trees']]                   <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBMCARMA_Trees']]}, error=function(x) NULL), Type='numeric', Default=500)
    ArgsList[['Max_Depth']]               <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBMCARMA_Max_Depth']]}, error=function(x) NULL), Type='numeric', Default=6)
    ArgsList[['ETA']]                     <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBMCARMA_ETA']]}, error=function(x) NULL), Type='numeric', Default=0.20)
    ArgsList[['Min_Data_In_Leaf']]        <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBMCARMA_Min_Data_In_Leaf']]}, error=function(x) NULL), Type='numeric', Default=1)
    ArgsList[['Num_Leaves']]              <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBMCARMA_Num_Leaves']]}, error=function(x) NULL), Type='numeric', Default=31)
    ArgsList[['Bagging_Fraction']]        <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBMCARMA_Bagging_Fraction']]}, error=function(x) NULL), Type='numeric', Default=1)
    ArgsList[['Feature_Fraction']]        <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBMCARMA_Feature_Fraction']]}, error=function(x) NULL), Type='numeric', Default=1)
    ArgsList[['Feature_Fraction_Bynode']] <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBMCARMA_Feature_Fraction_Bynode']]}, error=function(x) NULL), Type='numeric', Default=1)
    ArgsList[['Lambda_L1']]               <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBMCARMA_Lambda_L1']]}, error=function(x) NULL), Type='numeric', Default=4)
    ArgsList[['Lambda_L2']]               <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBMCARMA_Lambda_L2']]}, error=function(x) NULL), Type='numeric', Default=4)
  }

  # Code collection
  CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Forecasting: Feature Enginnering Args\n",
    "ArgsList[['NTrees']] <- ", Quantico:::CEP(ArgsList[['NTrees']]), "\n",
    "ArgsList[['Depth']] <- ", Quantico:::CEP(ArgsList[['Depth']]), "\n",
    "ArgsList[['Langevin']] <- ", Quantico:::CEPP(ArgsList[['Langevin']]), "\n",
    "ArgsList[['DiffusionTemperature']] <- ", Quantico:::CEPP(ArgsList[['DiffusionTemperature']]), "\n",
    "ArgsList[['L2_Leaf_Reg']] <- ", Quantico:::CEP(ArgsList[['L2_Leaf_Reg']]), "\n",
    "ArgsList[['ModelSizeReg']] <- ", Quantico:::CEP(ArgsList[['ModelSizeReg']]), "\n",
    "ArgsList[['MinDataInLeaf']] <- ", Quantico:::CEPP(ArgsList[['MinDataInLeaf']]), "\n",
    "ArgsList[['LearningRate']] <- ", Quantico:::CEPP(ArgsList[['LearningRate']]), "\n",
    "ArgsList[['BootStrapType']] <- ", Quantico:::CEP(ArgsList[['BootStrapType']]), "\n",
    "ArgsList[['GrowPolicy']] <- ", Quantico:::CEP(ArgsList[['GrowPolicy']]), "\n",
    "ArgsList[['RandomStrength']] <- ", Quantico:::CEPP(ArgsList[['RandomStrength']]), "\n",
    "ArgsList[['RSM']] <- ", Quantico:::CEP(ArgsList[['RSM']]), "\n",
    "ArgsList[['SubSample']] <- ", Quantico:::CEP(ArgsList[['SubSample']]), "\n",
    "ArgsList[['BorderCount']] <- ", Quantico:::CEP(ArgsList[['BorderCount']]), "\n",
    "ArgsList[['FeatureBorderType']] <- ", Quantico:::CEP(ArgsList[['FeatureBorderType']]), "\n",
    "ArgsList[['ScoreFunction']] <- ", Quantico:::CEP(ArgsList[['ScoreFunction']]), "\n"))}, error = function(x) CodeList)

  # Ensure data column is of correct type
  if(DebugFC) print('Shiny.FC.CARMA 21')
  Output <- Quantico::FC.DateCast(ArgsList, DebugFC, VD = ValidationData)
  ArgsList <- Output$ArgsList; ValidationData <- Output$VD; rm(Output); gc()

  # Code collection
  CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Ensure date column is correct date type\n",
    "DebugFC <- FALSE\n",
    "Output <- Quantico::FC.DateCast(ArgsList, DebugFC, VD = ValidationData)\n",
    "ArgsList <- Output$ArgsList; ValidationData <- Output$VD; rm(Output); gc()\n"))}, error = function(x) CodeList)

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Forecasting Procedures                ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(DebugFC) print('Shiny.FC.CARMA 21')

  # Conditions to proceed
  Cond1 <- data.table::is.data.table(ArgsList[['data']])
  Cond2 <- length(ArgsList[['TargetColumnName']]) > 0L
  Cond3 <- length(ArgsList[['DateColumnName']]) > 0L

  # Run Readiness Check
  if(Cond1 && Cond2 && Cond3) {

    if(DebugFC) print('Shiny.FC.CARMA 22')

    # *************************************
    # Train Model
    # *************************************
    if(RunMode == 'Train New Model') {

      if(DebugFC) print('Shiny.FC.CARMA 22.a')

      # Train CatBoost for Forecasting Purposes
      Output <- Quantico::Shiny.FC.Panel.Train(ArgsList, CodeList, DataList, ModelID, VD = ValidationData, DebugFC = DebugFC, Algo = Algo)
      DataList <- Output$DataList; CodeList <- Output$CodeList; ArgsList <- Output$ArgsList; ReportOutput <- Output$ReportObjects
      for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
      for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)

      # Return results
      if(DebugFC) print('return from Shiny.FC.CARMA')
      return(list(
        DataList = DataList,
        CodeList = CodeList,
        ArgsList = ArgsList,
        RunMode = RunMode,
        ModelID = ModelID,
        ReportOutput = ReportOutput
      ))
    }

    # *************************************
    # Train + Backtest
    # *************************************
    if(RunMode == 'Backtest' && data.table::is.data.table(ValidationData)) {

      if(DebugFC) print('Shiny.FC.CARMA 22.b')

      # Backtest CatBoost for Forecasting Purposes
      Output <- Quantico::Shiny.FC.Panel.Backtest(ArgsList, CodeList, DataList, ModelID, VD = ValidationData, DebugFC = DebugFC, Algo = Algo)
      DataList <- Output$DataList; CodeList <- Output$CodeList; ArgsList <- Output$ArgsList
      for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
      for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)

      # Return results
      if(DebugFC) print('Return from Shiny.FC.CARMA')
      return(list(
        DataList = Output$DataList,
        CodeList = Output$CodeList,
        ArgsList = Output$ArgsList,
        RunMode = RunMode,
        ModelID = ModelID
      ))
    }

    # *************************************
    # Feature Engineering Test
    # *************************************
    if(RunMode == 'Feature Engineering Test' && data.table::is.data.table(ValidationData)) {

      if(DebugFC) print('Shiny.FC.CARMA 22.c')

      # Collection table
      LoopMetrics <- data.table::data.table(
        'Feature Engineering' = c(
          "LogPlus1 vs None",
          "Series Differencing vs None",
          "Credibility vs Target Encoding",
          "Calendar Variables vs None",
          "Holiday Variables vs None",
          "Time Weights vs None",
          "Anomaly Detection vs None",
          "Time Trend Variable vs None",
          "Lag 1 vs None"
        ),
        'AvgError On' = -5,
        'AvgError Off' = -5,
        'RMSE On' = -5,
        'RMSE Off' = -5,
        'MAE On' = -5,
        'MAE Off' = -5,
        'MAPE On' = -5,
        'MAPE Off' = -5,
        'SMAPE On' = -5,
        'SMAPE Off' = -5)

      # Backtest CatBoost for Forecasting Purposes
      if(DebugFC) {print('Run: Shiny.FC.Panel.Backest.FeatureEval()'); print(paste0('Validation is data.table = ', data.table::is.data.table(ValidationData)))}
      Output <- Quantico::Shiny.FC.Panel.Backest.FeatureEval(LoopMetrics, ArgsList, CodeList, DataList, ModelID, VD = ValidationData, DebugFC = DebugFC)
      DataList <- Output$DataList; CodeList <- Output$CodeList; ArgsList <- Output$ArgsList; LoopMetrics <- Output$LoopMetrics
      ColToCheck <- 'MAE On'

      if(DebugFC) print('Shiny.FC.Panel.Backest.FeatureEval post step 1')

      DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']] <- LoopMetrics[get(ColToCheck) > -5L]

      if(DebugFC) print('Shiny.FC.Panel.Backest.FeatureEval post step 2')

      DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']][, TestOutcome := data.table::fifelse(`MAE On` < `MAE Off`, 'Success','Failure')]
      DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']][, RunNumber := NULL]
      DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']][, Methods := NULL]
      data.table::setcolorder(
        DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']],
        c(1L,
          ncol(DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']]),
          2L:(ncol(DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']])-1L)))

      if(DebugFC) print('Shiny.FC.Panel.Backest.FeatureEval post step 4')
      #numcols <- ncol(DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']])
      #data.table::setcolorder(x = DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']], neworder = c(numcols-1L, numcols, 2L:(numcols-2L)))
      #if(DebugFC) print('Shiny.FC.Panel.Backest.FeatureEval post step 5')

      if(DebugFC) print(DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']])

      # DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']] <- DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']][
      #   , .SD, .SDcols = c(names(DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']])[c(1L,2L, (numcols-5L):numcols)])
      # ]

      # tryCatch({data.table::set(DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']], j = "Methods", value = NULL)}, error = function(x) NULL)

      if(DebugFC) print('Shiny.FC.Panel.Backest.FeatureEval post step 6')
      DataList <- Quantico:::DM.DataListUpdate(dl = DataList, dn = paste0(ModelID, "_FeatureEngineeringTest"), Sample = TRUE)
      for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
      for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)

      # Return results
      if(DebugFC) print('Return from Shiny.FC.CARMA')
      return(list(
        DataList = DataList,
        CodeList = CodeList,
        ArgsList = ArgsList,
        RunMode = RunMode,
        ModelID = ModelID
      ))
    }

    # *************************************
    # Backtest Cross Evaluation
    # *************************************
    if(RunMode == 'Backtest Cross Eval' && data.table::is.data.table(ValidationData)) {

      if(DebugFC) print('Shiny.FC.CARMA 22.d')

      # Get the Model Update Frequency # Get the Values Update Frequency
      muu <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_CrossEvalModelUpdate')]]}, error=function(x) NULL), Type='logical', Default=TRUE)
      vuu <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_CrossEvalValuesUpdate')]]}, error=function(x) NULL), Type='logical', Default=TRUE)

      muff <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_CrossEvalModelUpdateFreq')]]}, error=function(x) NULL), Type='numeric', Default=1)
      vuff <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_CrossEvalValuesUpdateFreq')]]}, error=function(x) NULL), Type='numeric', Default=1)

      # Run Cross Eval
      if(DebugFC) print('Run Quantico::Shiny.FC.Panel.Backest.RollingEval()')
      Output <- Quantico::Shiny.FC.Panel.Backest.RollingEval(ArgsList, DataList, CodeList, ModelID, VD = ValidationData, mu = muu, vu = vuu, vuf = vuff, muf = muff, Algo = Algo, DebugFC = DebugFC)
      DataList <- Output$DataList; CodeList <- Output$CodeList; ArgsList <- Output$ArgsList
      for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
      for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)

      # Return results
      if(DebugFC) print('Return from Shiny.FC.CARMA')
      return(list(
        DataList = DataList,
        CodeList = CodeList,
        ArgsList = ArgsList,
        RunMode = RunMode,
        ModelID = ModelID
      ))
    }

  } else {
    if(DebugFC) print('Conditions'); print(Cond1); print(Cond2); print(Cond3); print(rep(' Model Failed  ', 4))
  }
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# SS Forecasting                                                                             ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Shiny.FC.CARMA
#'
#' @description Shiny forecasting function
#'
#' @details # There are 5 run modes for this function
#'
#' -'Train New Model': prepare for backtesting / forecasting; inspect ML performance
#'
#' -'Retrain Existing Model': Load model, new data, but don't need forecast yet
#'
#' -'Backtest': evaluate model in variety of ways
#'
#' -'Forecast': generate a forecast from a trained model; either loaded or created in session
#'
#' -'Forecast+Retrain': generate an updated model and use that model to generate a forecast
#'
#' @author Adrian Antico
#' @family FC
#'
#' @param input shiny input
#' @param output shiny output
#' @param DataList DataList stores data in app
#' @param ArgsList ArgsList
#' @param CodeList CodeList from app
#' @param DebugFC DebugFC from app
#' @param Algo 'SARIMA', 'NNET', 'TBATS', 'ETS', 'ARFIMA'
#'
#' @return a list of columns names by data type
#'
#' @export
Shiny.FC.SS <- function(input,
                        output,
                        DataList,
                        ArgsList,
                        CodeList,
                        DebugFC,
                        TabCount=5L,
                        Algo = NULL,
                        wd = NULL) {

  # Default
  if(length(wd) > 0L) {
    if(DebugFC) print(wd)
    ArgsList[['SaveDataPath']] <- wd
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Directory for storing objects\n",
      "ArgsList[['SaveDataPath']] <- ", Quantico:::CEP(wd), "\n"))
    }, error = function(x) CodeList)
  } else {
    if(DebugFC) print('wd is NULL')
  }

  # There are 2 Run Modes for this function
  #   'Grid Tune': generate an output data.table of metrics and parameters for user evaluation
  #                and then used as ArgsList to generate Forecasts
  #   'Forecast': generate a forecast based on FC Periods, ArgsList from Grid Tune, and data

  # **************************************** ----
  # Args Management                          ----
  # **************************************** ----

  # ModelID
  #
  # Note: ArgsList is forced to NULL as an Argument to this function when Grid Tune is on
  if(length(ArgsList) > 0L) {
    if(DebugFC) print("ArgsList > 0")
    ModelID <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_ArgsList')]]}, error=function(x) NULL), Type='character', Default=NULL); if(DebugFC) print('here 3.2')
    if(DebugFC) print('here 3.2')
    if(length(ModelID) == 0L) ModelID <- "SS001"
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Define ModelID\n",
      "ModelID <- ", Quantico:::CEP(ModelID), "\n",
      "if(length(ModelID) == 0L) ModelID <- 'SS001'\n"
    ))
    }, error = function(x) CodeList)
  } else {
    if(DebugFC) print("ArgsList == 0")
    ModelID <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_ModelID')]]}, error=function(x) NULL), Type='character', Default=NULL); if(DebugFC) print('here 3.2')
    if(ModelID == "") ModelID <- "SS001"
    ArgsList[['ModelID']] <- ModelID

    # ModelID code block
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Define ModelID\n",
      "ModelID <- ", Quantico:::CEP(ModelID), "\n",
      "if(length(ModelID) == 0L) ModelID <- 'SS001'\n",
      "ArgsList[['ModelID']] <- ", Quantico:::CEP(ModelID), "\n"))
    }, error = function(x) {
      if(DebugFC) print("ModelID code block: failed")
      if(DebugFC) print(paste0("Quantico:::CEP(ModelID) = ", Quantico:::CEP(ModelID)))
      CodeList
    })
  }

  if(DebugFC) {
    if(DebugFC) print('here 4.0')
    if(DebugFC) print(ModelID)
  }

  # data, RunMode
  #
  # Note: data comes from user provided data for both Grid Tuning and Forecasting
  ArgsList[['data']] <- data.table::copy(DataList[[Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_Data')]]}, error=function(x) NULL), Type='character', Default=NULL)]][['data']])
  ArgsList[['RunMode']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_RunMode")]]}, error = NULL), Type = "character", Default = "Forecast")
  CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
    "\n",
    "# Grab data from DataList and store in ArgsList under parameter name 'data'\n",
    "# Store RunMode in ArgsList\n",
    "ArgsList[['data']] <- DataList[[", Quantico:::CEP(Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, '_Data')]]}, error=function(x) NULL), Type='character', Default=NULL)), "]]\n",
    "ArgsList[['RunMode']] <- ", Quantico:::CEP(ArgsList[['RunMode']]), "\n",
    "\n"))
  }, error = function(x) CodeList)


  # Grid Tune or Forecast:
  #   Grid tune pulls from inputs
  #   Forecast pulls from ArgsList and a few inputs
  #     The few inputs are data, modelID

  # ----

  # **************************************** ----
  # Grid Tune                                ----
  # **************************************** ----
  if(DebugFC)  print('here 5.0')
  if(ArgsList[['RunMode']] == "Grid Tune") {

    ArgsList[['ModelID']] <- ModelID
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# GRID TUNING ::::::::::: \n\n",
      "# Copy data from DataList and store in ArgsList under parameter name 'data'\n",
      "# Store ModelID in ArgsList\n",
      "if(!exists('ArgsList')) ArgsList <- list()\n",
      "ArgsList[['ModelID']] <- ", Quantico:::CEP(ModelID),
      "\n"))
    }, error = function(x) CodeList)

    if(DebugFC)  print('here 6.0')

    # Logic to proceed
    if(ArgsList[['RunMode']] == "Grid Tune" && data.table::is.data.table(ArgsList[['data']]) && ArgsList[['data']][,.N] > 0) {
      ValidRun <- TRUE
    } else {
      ValidRun <- FALSE
    }
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Check to see if run will be valid\n",
      "if(ArgsList[['RunMode']] == 'Grid Tune' && data.table::is.data.table(ArgsList[['data']]) && ArgsList[['data']][,.N] > 0) {\n  ",
      "ValidRun <- TRUE\n",
      "} else {\n  ",
      "ValidRun <- FALSE\n",
      "}\n"))
    }, error = function(x) CodeList)

    if(DebugFC)  print('here 7.0')

    if(!ValidRun) return(NULL)

    # ********************************* ----
    # Common Args                       ----
    # ********************************* ----

    if(DebugFC)  print('here 8.0')

    ArgsList[['ArgsList']] <- NULL #Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_ArgsList")]]}, error = NULL), Type = "character", Default = NULL)
    ArgsList[['TargetColumnName']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_TargetColumnName")]]}, error = NULL), Type = "character", Default = NULL)
    ArgsList[['DateColumnName']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_DateColumnName")]]}, error = NULL), Type = "character", Default = NULL)
    ArgsList[['TimeUnit']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_TimeUnit")]]}, error = NULL), Type = "character", Default = NULL)
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Store ArgsList, TargetColumnName, and DateColumnName into ArgsList\n",
      "ArgsList[['ArgsList']] <- NULL\n",
      "ArgsList[['TargetColumnName']] <- ", Quantico:::CEP(ArgsList[['TargetColumnName']]), "\n",
      "ArgsList[['DateColumnName']] <- ", Quantico:::CEP(ArgsList[['DateColumnName']]), "\n",
      "ArgsList[['TimeUnit']] <- ", Quantico:::CEP(ArgsList[['TimeUnit']], "\n")))
    }, error = function(x) CodeList)

    # Ensure DateVariable is a Date Type
    check <- class(ArgsList[['data']][[ArgsList[['DateColumnName']]]])[1L]
    if(check %in% c("numeric", "integer", "character", "factor", "logical")) {
      if(!(tolower(ArgsList[['TimeUnit']]) %chin% c('1min','5min','10min','15min','30min','hour'))) {
        x <- ArgsList[['data']][1L, get(ArgsList[['DateColumnName']])]
        x1 <- lubridate::guess_formats(x, orders = c('mdY', 'BdY', 'Bdy', 'bdY', 'bdy', 'mdy', 'dby', 'Ymd', 'Ydm','dmy'))
        ArgsList[['data']][, eval(ArgsList[['DateColumnName']]) := as.Date(get(ArgsList[['DateColumnName']]), tryFormats = x1)]
      } else {
        ArgsList[['data']][, eval(ArgsList[['DateColumnName']]) := as.POSIXct(get(ArgsList[['DateColumnName']]))]
      }
    }

    if(DebugFC) print('here 8.1')
    if(length(ArgsList[['TargetColumnName']]) == 0) {
      return(NULL)
    } else if(!ArgsList[['TargetColumnName']] %in% names(ArgsList[['data']])) {
      return(NULL)
    }
    if(length(ArgsList[['DateColumnName']]) == 0) {
      return(NULL)
    } else if(!ArgsList[['DateColumnName']] %in% names(ArgsList[['data']])) {
      return(NULL)
    }

    # Target -> Date code blocks
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Target must be defined, otherwise stop procedure\n",
      "if(length(ArgsList[['TargetColumnName']]) == 0) {\n  ",
      "stop('TargetColumnName cannot be NULL')\n",
      "} else if(!ArgsList[['TargetColumnName']] %in% names(ArgsList[['data']])) {\n  ",
      "stop('TargetColumnName must be in data')\n",
      "}\n"))
    }, error = function(x) CodeList)
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Date must be defined, otherwise stop procedure\n",
      "if(length(ArgsList[['DateColumnName']]) == 0) {\n  ",
      "stop('DateColumnName cannot be NULL')\n",
      "} else if(!ArgsList[['DateColumnName']] %in% names(ArgsList[['data']])) {\n  ",
      "stop('DateColumnName must be in data')\n",
      "}\n"))
    }, error = function(x) CodeList)

    if(DebugFC) print('here 8.2')
    ArgsList[['FCPeriods']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_FCPeriods")]]}, error = NULL), Type = "numeric", Default = 5)
    ArgsList[['EvaluationMetric']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_EvaluationMetric")]]}, error = NULL), Type = "character", Default = "MSE")
    ArgsList[['TrainWeighting']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_TrainWeighting")]]}, error = NULL), Type = "numeric", Default = 0.50)
    ArgsList[['DebugMode']] <- DebugFC
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Define additional args\n",
      "ArgsList[['FCPeriods']] <- ", Quantico:::CEP(ArgsList[['FCPeriods']]), "\n",
      "ArgsList[['EvaluationMetric']] <- ", Quantico:::CEP(ArgsList[['EvaluationMetric']]), "\n",
      "ArgsList[['TrainWeighting']] <- ", Quantico:::CEP(ArgsList[['TrainWeighting']]), "\n",
      "ArgsList[['DebugMode']] <- ", Quantico:::CEPP(ArgsList[['DebugMode']]), "\n"))
    }, error = function(x) CodeList)

    # ********************************* ----
    # Common data operations            ----
    # ********************************* ----

    # Ensure only the required columns are used
    data <- data.table::copy(ArgsList[['data']][, .SD, .SDcols = c(ArgsList[['DateColumnName']], ArgsList[['TargetColumnName']])])
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Define additional args\n",
      "data <- data.table::copy(ArgsList[['data']][, .SD, .SDcols = c(ArgsList[['DateColumnName']], ArgsList[['TargetColumnName']])])\n"))
    }, error = function(x) CodeList)

    if(DebugFC) {
      if(DebugFC) print('here 10.51')
      if(DebugFC) print(ArgsList[['TargetColumnName']])
    }

    # Aggregate data to ensure single timestamp per record
    data <- data[, mean(get(ArgsList[['TargetColumnName']]), na.rm = TRUE), by = eval(ArgsList[['DateColumnName']])]
    data.table::setnames(data, c(names(data)[1L], names(data)[2L]), c(ArgsList[['DateColumnName']], ArgsList[['TargetColumnName']]))
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Aggregate data to ensure single timestamp per record\n",
      "data <- data[, mean(get(ArgsList[['TargetColumnName']]), na.rm = TRUE), by = eval(ArgsList[['DateColumnName']])]\n",
      "data.table::setnames(data, c(names(data)[1L], names(data)[2L]), c(ArgsList[['DateColumnName']], ArgsList[['TargetColumnName']]))\n"))
    }, error = function(x) CodeList)

    if(class(data[[ArgsList[['DateColumnName']]]])[1L] %in% "IDate") {
      data[, eval(ArgsList[['DateColumnName']]) := as.Date(get(ArgsList[['DateColumnName']]))]
    }
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Convert date column to type date if of type IDate\n",
      "if(class(data[[ArgsList[['DateColumnName']]]])[1L] %in% 'IDate') {\n  ",
      "data[, eval(ArgsList[['DateColumnName']]) := as.Date(get(ArgsList[['DateColumnName']]))]\n",
      "}\n"))
    }, error = function(x) CodeList)

    # ********************************* ----
    # ALGO SPECIFIC ARGS                ----
    # ********************************* ----

    if(DebugFC)  print('here 9.0')

    # SARIMA Features
    if(Algo == "SARIMA") {
      ArgsList[['MaxLags']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxLags")]]}, error = NULL), Type = "numeric", Default = 0)
      ArgsList[['MaxSeasonalLags']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxSeasonalLags")]]}, error = NULL), Type = "numeric", Default = 0)
      ArgsList[['MaxMovingAverages']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxMovingAverages")]]}, error = NULL), Type = "numeric", Default = 0)
      ArgsList[['MaxSeasonalMovingAverages']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxSeasonalMovingAverages")]]}, error = NULL), Type = "numeric", Default = 0)
      ArgsList[['MaxFourierPairs']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxFourierPairs")]]}, error = NULL), Type = "numeric", Default = 0)
      ArgsList[['MaxConsecutiveFail']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxConsecutiveFail")]]}, error = NULL), Type = "numeric", Default = 20)
      ArgsList[['MaxNumberModel']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxNumberModel")]]}, error = NULL), Type = "numeric", Default = 30)
      ArgsList[['MaxRunTimeMinute']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxRunTimeMinute")]]}, error = NULL), Type = "numeric", Default = 10)
      #CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      CodeList <- Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# SARIMA Args\n",
        "ArgsList[['MaxLags']] <- ", Quantico:::CEPP(ArgsList[['MaxLags']]), "\n",
        "ArgsList[['MaxSeasonalLags']] <- ", Quantico:::CEPP(ArgsList[['MaxSeasonalLags']]), "\n",
        "ArgsList[['MaxMovingAverages']] <- ", Quantico:::CEPP(ArgsList[['MaxMovingAverages']]), "\n",
        "ArgsList[['MaxSeasonalMovingAverages']] <- ", Quantico:::CEPP(ArgsList[['MaxSeasonalMovingAverages']]), "\n",
        "ArgsList[['MaxFourierPairs']] <- ", Quantico:::CEPP(ArgsList[['MaxFourierPairs']]), "\n",
        "ArgsList[['MaxConsecutiveFail']] <- ", Quantico:::CEPP(ArgsList[['MaxConsecutiveFail']]), "\n",
        "ArgsList[['MaxNumberModel']] <- ", Quantico:::CEPP(ArgsList[['MaxNumberModel']]), "\n",
        "ArgsList[['MaxRunTimeMinute']] <- ", Quantico:::CEPP(ArgsList[['MaxRunTimeMinute']]), "\n"))
      #}, error = function(x) CodeList)
    }

    # NNET Features
    if(Algo == "NNET") {
      ArgsList[['MaxLags']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxLags")]]}, error = NULL), Type = "numeric", Default = 0)
      ArgsList[['MaxSeasonalLags']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxSeasonalLags")]]}, error = NULL), Type = "numeric", Default = 0)
      ArgsList[['MaxFourierPairs']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxFourierPairs")]]}, error = NULL), Type = "numeric", Default = 0)
      ArgsList[['MaxConsecutiveFail']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxConsecutiveFail")]]}, error = NULL), Type = "numeric", Default = 20)
      ArgsList[['MaxNumberModel']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxNumberModel")]]}, error = NULL), Type = "numeric", Default = 30)
      ArgsList[['MaxRunTimeMinute']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxRunTimeMinute")]]}, error = NULL), Type = "numeric", Default = 10)
      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# NNET Args\n",
        "ArgsList[['MaxLags']] <- ", Quantico:::CEPP(ArgsList[['MaxLags']]), "\n",
        "ArgsList[['MaxSeasonalLags']] <- ", Quantico:::CEPP(ArgsList[['MaxSeasonalLags']]), "\n",
        "ArgsList[['MaxFourierPairs']] <- ", Quantico:::CEPP(ArgsList[['MaxFourierPairs']]), "\n",
        "ArgsList[['MaxConsecutiveFail']] <- ", Quantico:::CEPP(ArgsList[['MaxConsecutiveFail']]), "\n",
        "ArgsList[['MaxNumberModel']] <- ", Quantico:::CEPP(ArgsList[['MaxNumberModel']]), "\n",
        "ArgsList[['MaxRunTimeMinute']] <- ", Quantico:::CEPP(ArgsList[['MaxRunTimeMinute']]), "\n"))
      }, error = function(x) CodeList)
    }

    # TBATS Features
    if(Algo == "TBATS") {
      ArgsList[['MaxLags']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxLags")]]}, error = NULL), Type = "numeric", Default = 0)
      ArgsList[['MaxMovingAverages']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxMovingAverages")]]}, error = NULL), Type = "numeric", Default = 0)
      ArgsList[['MaxSeasonalPeriods']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxSeasonalPeriods")]]}, error = NULL), Type = "numeric", Default = 0)
      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# TBATS Args\n",
        "ArgsList[['MaxLags']] <- ", Quantico:::CEPP(ArgsList[['MaxLags']]), "\n",
        "ArgsList[['MaxMovingAverages']] <- ", Quantico:::CEPP(ArgsList[['MaxMovingAverages']]), "\n",
        "ArgsList[['MaxSeasonalPeriods']] <- ", Quantico:::CEPP(ArgsList[['MaxSeasonalPeriods']]), "\n"))
      }, error = function(x) CodeList)
    }

    # ETS Features
    if(Algo == "ETS") {
      if(DebugFC) print("ETS currently doesn't expose inner params")
    }

    # ARFIMA Features
    if(Algo == "ARFIMA") {
      if(DebugFC)  print('here 9.50')
      ArgsList[['MaxLags']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxLags")]]}, error = NULL), Type = "numeric", Default = 0)
      ArgsList[['MaxMovingAverages']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_MaxMovingAverages")]]}, error = NULL), Type = "numeric", Default = 0)
      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# ARFIMA Args\n",
        "ArgsList[['MaxLags']] <- ", Quantico:::CEPP(ArgsList[['MaxLags']]), "\n",
        "ArgsList[['MaxMovingAverages']] <- ", Quantico:::CEPP(ArgsList[['MaxMovingAverages']]), "\n"))
      }, error = function(x) CodeList)
    }

    # ********************************* ----
    # ALGO SPECIFIC TUNING              ----
    # ********************************* ----

    # TBATS
    if(Algo == "TBATS") {

      # 1. Create time series artifacts----
      if(DebugFC) {
        if(DebugFC) print('here 10.56')
        if(DebugFC) print(names(data))
        if(DebugFC) print(str(data))
        if(DebugFC) print(ArgsList[['TargetColumnName']])
        if(DebugFC) print(ArgsList[['DateColumnName']])
        if(DebugFC) print(ArgsList[['MaxLags']])
        if(DebugFC) print(ArgsList[['MaxMovingAverages']])
        if(DebugFC) print(ArgsList[['MaxSeasonalPeriods']])
        if(DebugFC) print(ArgsList[['TimeUnit']])
        if(DebugFC) print(ArgsList[['FCPeriods']])
        if(DebugFC) print('here 10.561')
      }

      # Data Prepare
      # Experimental Grid Features from Backend Functions
      #   "DataSetName", "BoxCox", "IncludeDrift", "SeasonalDifferences", "SeasonalMovingAverages",
      #   "SeasonalLags", "MaxFourierTerms", "Differences", "MovingAverages	Lags"
      # Experimental Grid Features from App
      #   TODO
      TBATS_Artifacts_Build <- tryCatch({
        AutoQuant:::TimeSeriesDataPrepare(
          data = data,
          TargetName = ArgsList[['TargetColumnName']],
          DateName = ArgsList[['DateColumnName']],
          Lags = ArgsList[['MaxLags']],
          SeasonalLags = 0,
          MovingAverages = ArgsList[['MaxMovingAverages']],
          SeasonalMovingAverages = ArgsList[['MaxSeasonalPeriods']],
          TimeUnit = ArgsList[['TimeUnit']],
          FCPeriods = 1,
          HoldOutPeriods = ArgsList[['FCPeriods']],
          TSClean = TRUE,
          ModelFreq = TRUE,
          FinalBuild = FALSE)
      }, error = function(x) {
        if(DebugFC) print("AutoQuant:::TimeSeriesDataPrepare failed")
        NULL
      })
      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# TBATS Args\n",
        "TBATS_Artifacts_Build <- tryCatch({\n  ",
        "AutoQuant:::TimeSeriesDataPrepare(\n    ",
        "data = data,\n    ",
        "TargetName = ", Quantico:::CEP(ArgsList[['TargetColumnName']]),",\n    ",
        "DateName = ", Quantico:::CEP(ArgsList[['DatetColumnName']]),",\n    ",
        "Lags = ", Quantico:::CEPP(ArgsList[['MaxLags']]),",\n    ",
        "SeasonalLags = ", 0,"\n    ",
        "MovingAverages = ", Quantico:::CEPP(ArgsList[['MaxMovingAverages']]),",\n    ",
        "SeasonalMovingAverages = ", 0,",\n    ",
        "TimeUnit = ", Quantico:::CEP(ArgsList[['TimeUnit']]),",\n    ",
        "FCPeriods = ", 1,",\n    ",
        "HoldOutPeriods = ", Quantico:::CEPP(ArgsList[['FCPeriods']]),",\n    ",
        "TSClean = TRUE,\n    ",
        "ModelFreq = TRUE,\n    ",
        "FinalBuilt = FALSE)\n",
        "}, error = function(x) {\n  ",
        "print('AutoQuant:::TimeSeriesDataPrepare failed')\n  ",
        "NULL\n",
        "})\n"))
      }, error = function(x) {
        if(DebugFC) print("TimeSeriesDataPrepare code collect failure")
        if(DebugFC) print(Quantico:::CEPP(ArgsList[['TargetColumnName']]))
        if(DebugFC) print(Quantico:::CEPP(ArgsList[['DatetColumnName']]))
        if(DebugFC) print(Quantico:::CEPP(ArgsList[['MaxLags']]))
        if(DebugFC) print(Quantico:::CEPP(ArgsList[['MaxMovingAverages']]))
        if(DebugFC) print(Quantico:::CEPP(ArgsList[['TimeUnit']]))
        if(DebugFC) print(Quantico:::CEPP(ArgsList[['FCPeriods']]))
        CodeList
      } )

      # Create tons of models for evaluation
      if(length(TBATS_Artifacts_Build) > 0L) {

        if(DebugFC)  print('here 10.57')

        # 2. Find Best TBATS Models----
        cores <- parallel::detectCores()
        if(cores >= 4L) {
          NumberCores <- 4L
        } else {
          NumberCores <- cores
        }
        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Parallel Runs\n",
          "cores <- parallel::detectCores()\n",
          "if(cores >= 4L) {\n  ",
          "NumberCores <- 4L\n",
          "} else {\n  ",
          "NumberCores <- cores\n",
          "}\n"))
        }, error = function(x) CodeList)

        ExperimentGrid <- tryCatch({
          AutoQuant:::ParallelAutoTBATS(
            MetricSelection = ArgsList[['EvaluationMetric']],
            Output = TBATS_Artifacts_Build,
            NumCores = NumberCores,
            TrainValidateShare = ArgsList[['TrainWeighting']])
        }, error = function(x) {
          if(DebugFC) print("AutoQuant:::ParallelAutoETS failed")
          NULL
        })
        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# TBATS Build\n",
          "ExperimentGrid <- tryCatch({\n  ",
          "AutoQuant:::ParallelAutoTBATS(\n    ",
          "MetricSelection = ArgsList[['EvaluationMetric']],\n    ",
          "Output = TBATS_Artifacts_Build,\n    ",
          "NumCores = NumberCores,\n    ",
          "TrainValidateShare = ArgsList[['TrainWeighting']])\n",
          "}, error = function(x) {\n  ",
          "print('AutoQuant:::ParallelAutoTBATS failed')\n  ",
          "NULL\n",
          "})\n"))
        }, error = function(x) CodeList)
      } else {
        if(DebugFC) print("*_Artifacts_Build is NULL")
      }
    }

    # SARIMA
    if(Algo == "SARIMA") {

      # 1. Create time series artifacts----
      if(DebugFC) {
        if(DebugFC) print('here 10.56')
        if(DebugFC) print(names(data))
        if(DebugFC) print(str(data))
        if(DebugFC) print(ArgsList[['TargetColumnName']])
        if(DebugFC) print(ArgsList[['DateColumnName']])
        if(DebugFC) print(ArgsList[['MaxLags']])
        if(DebugFC) print(ArgsList[['MaxMovingAverages']])
        if(DebugFC) print(ArgsList[['MaxMovingAverages']])
        if(DebugFC) print(ArgsList[['MaxSeasonalMovingAverages']])
        if(DebugFC) print(ArgsList[['MaxFourierPairs']])
        if(DebugFC) print(ArgsList[['TimeUnit']])
        if(DebugFC) print(ArgsList[['FCPeriods']])
        if(DebugFC) print('here 10.561')
      }

      # Data Prepare
      # Experimental Grid Features from Backend Functions
      #   "DataSetName", "BoxCox", "IncludeDrift", "SeasonalDifferences", "SeasonalMovingAverages",
      #   "SeasonalLags", "MaxFourierTerms", "Differences", "MovingAverages	Lags"
      # Experimental Grid Features from App
      #   TODO
      SARIMA_Artifacts_Build <- tryCatch({
        AutoQuant:::TimeSeriesDataPrepare(
          data = data,
          TargetName = ArgsList[['TargetColumnName']],
          DateName = ArgsList[['DateColumnName']],
          Lags = ArgsList[['MaxLags']],
          SeasonalLags = ArgsList[['MaxSeasonalLags']],
          MovingAverages = ArgsList[['MaxMovingAverages']],
          SeasonalMovingAverages = ArgsList[['MaxSeasonalMovingAverages']],
          TimeUnit = ArgsList[['TimeUnit']],
          FCPeriods = 1,
          HoldOutPeriods = ArgsList[['FCPeriods']],
          TSClean = TRUE,
          ModelFreq = TRUE,
          FinalBuild = FALSE)
      }, error = function(x) {
        if(DebugFC) print("AutoQuant:::TimeSeriesDataPrepare failed")
        NULL
      })
      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# SARIMA Args\n",
        "SARIMA_Artifacts_Build <- tryCatch({\n  ",
        "AutoQuant:::TimeSeriesDataPrepare(\n    ",
        "data = data,\n    ",
        "TargetName = ", Quantico:::CEP(ArgsList[['TargetColumnName']]),",\n    ",
        "DateName = ", Quantico:::CEP(ArgsList[['DatetColumnName']]),",\n    ",
        "Lags = ", Quantico:::CEPP(ArgsList[['MaxLags']]),",\n    ",
        "SeasonalLags = ", Quantico:::CEPP(ArgsList[['MaxSeasonalLags']]),",\n    ",
        "MovingAverages = ", Quantico:::CEPP(ArgsList[['MaxMovingAverages']]),",\n    ",
        "SeasonalMovingAverages = ", Quantico:::CEPP(ArgsList[['MaxSeasonalMovingAverages']]),",\n    ",
        "TimeUnit = ", Quantico:::CEP(ArgsList[['TimeUnit']]),",\n    ",
        "FCPeriods = ", 1,",\n    ",
        "HoldOutPeriods = ", Quantico:::CEPP(ArgsList[['FCPeriods']]),",\n    ",
        "TSClean = TRUE,\n    ",
        "ModelFreq = TRUE,\n    ",
        "FinalBuilt = FALSE)\n",
        "}, error = function(x) {\n  ",
        "print('AutoQuant:::TimeSeriesDataPrepare failed')\n  ",
        "NULL\n",
        "})\n"))
      }, error = function(x) CodeList)

      # Create tons of models for evaluation
      if(length(SARIMA_Artifacts_Build) > 0L) {

        if(DebugFC)  print('here 10.57')

        # 2. Find Best SARIMA Models----
        cores <- parallel::detectCores()
        if(cores >= 4L) {
          NumberCores <- 4L
        } else {
          NumberCores <- cores
        }
        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Parallel Runs\n",
          "cores <- parallel::detectCores()\n",
          "if(cores >= 4L) {\n  ",
          "NumberCores <- 4L\n",
          "} else {\n  ",
          "NumberCores <- cores\n",
          "}\n"))
        }, error = function(x) CodeList)

        ExperimentGrid <- tryCatch({
          AutoQuant:::ParallelAutoARIMA(
            MetricSelection = ArgsList[['EvaluationMetric']],
            Output = SARIMA_Artifacts_Build,
            MaxFourierTerms = ArgsList[['MaxFourierPairs']],
            MaxNumberModels = ArgsList[['MaxNumberModel']],
            MaxRunMinutes = ArgsList[['MaxRunTimeMinute']],
            MaxRunsWithoutNewWinner = ArgsList[['MaxConsecutiveFail']],
            NumCores = NumberCores,
            TrainValidateShare = ArgsList[['TrainWeighting']])
        }, error = function(x) {
          if(DebugFC) print("AutoQuant:::ParallelAutoARIMA failed")
          NULL
        })
        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# SARIMA Build\n",
          "ExperimentGrid <- tryCatch({\n  ",
          "AutoQuant:::ParallelAutoARIMA(\n    ",
          "MetricSelection = ArgsList[['EvaluationMetric']],\n    ",
          "Output = SARIMA_Artifacts_Build,\n    ",
          "MaxFourierTerms = ", Quantico:::CEPP(ArgsList[['MaxFourierPairs']]), ",\n    ",
          "MaxNumberModels = ", Quantico:::CEPP(ArgsList[['MaxNumberModel']]), ",\n    ",
          "MaxRunMinutes = ", Quantico:::CEPP(ArgsList[['MaxRunTimeMinute']]), ",\n    ",
          "MaxRunsWithoutNewWinner = ", Quantico:::CEPP(ArgsList[['MaxConsecutiveFail']]), ",\n    ",
          "NumCores = NumberCores,\n    ",
          "TrainValidateShare = ArgsList[['TrainWeighting']])\n",
          "}, error = function(x) {\n  ",
          "print('AutoQuant:::ParallelAutoARIMA failed')\n  ",
          "NULL\n",
          "})\n"))
        }, error = function(x) CodeList)
      } else {
        if(DebugFC) print("*_Artifacts_Build is NULL")
      }
    }

    # ETS
    if(Algo == "ETS") {

      # 1. Create time series artifacts----
      if(DebugFC) {
        if(DebugFC) print('here 10.56')
        if(DebugFC) print(names(data))
        if(DebugFC) print(str(data))
        if(DebugFC) print(ArgsList[['TargetColumnName']])
        if(DebugFC) print(ArgsList[['DateColumnName']])
        if(DebugFC) print(ArgsList[['TimeUnit']])
        if(DebugFC) print(ArgsList[['FCPeriods']])
        if(DebugFC) print('here 10.561')
      }

      # Data Prepare
      # Experimental Grid Features from Backend Functions
      #   "DataSetName", "BoxCox", "IncludeDrift", "SeasonalDifferences", "SeasonalMovingAverages",
      #   "SeasonalLags", "MaxFourierTerms", "Differences", "MovingAverages	Lags"
      # Experimental Grid Features from App
      #   TODO
      ETS_Artifacts_Build <- tryCatch({
        AutoQuant:::TimeSeriesDataPrepare(
          data = data,
          TargetName = ArgsList[['TargetColumnName']],
          DateName = ArgsList[['DateColumnName']],
          Lags = 0,
          SeasonalLags = 0,
          MovingAverages = 0,
          SeasonalMovingAverages = 0,
          TimeUnit = ArgsList[['TimeUnit']],
          FCPeriods = 1,
          HoldOutPeriods = ArgsList[['FCPeriods']],
          TSClean = TRUE,
          ModelFreq = TRUE,
          FinalBuild = FALSE)
      }, error = function(x) {
        if(DebugFC) print("AutoQuant:::TimeSeriesDataPrepare failed")
        NULL
      })
      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# ETS Args\n",
        "ETS_Artifacts_Build <- tryCatch({\n  ",
        "AutoQuant:::TimeSeriesDataPrepare(\n    ",
        "data = data,\n    ",
        "TargetName = ", Quantico:::CEP(ArgsList[['TargetColumnName']]),",\n    ",
        "DateName = ", Quantico:::CEP(ArgsList[['DatetColumnName']]),",\n    ",
        "Lags = 0,\n    ",
        "SeasonalLags = 0,\n    ",
        "MovingAverages = 0,\n    ",
        "SeasonalMovingAverages = 0,\n    ",
        "TimeUnit = ", Quantico:::CEP(ArgsList[['TimeUnit']]),",\n    ",
        "FCPeriods = ", 1,",\n    ",
        "HoldOutPeriods = ", Quantico:::CEPP(ArgsList[['FCPeriods']]),",\n    ",
        "TSClean = TRUE,\n    ",
        "ModelFreq = TRUE,\n    ",
        "FinalBuilt = FALSE)\n",
        "}, error = function(x) {\n  ",
        "print('AutoQuant:::TimeSeriesDataPrepare failed')\n  ",
        "NULL\n",
        "})\n"))
      }, error = function(x) CodeList)

      # Create tons of models for evaluation
      if(length(ETS_Artifacts_Build) > 0L) {

        if(DebugFC)  print('here 10.57')

        # 2. Find Best ETS Models----
        cores <- parallel::detectCores()
        if(cores >= 4L) {
          NumberCores <- 4L
        } else {
          NumberCores <- cores
        }
        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Parallel Runs\n",
          "cores <- parallel::detectCores()\n",
          "if(cores >= 4L) {\n  ",
          "NumberCores <- 4L\n",
          "} else {\n  ",
          "NumberCores <- cores\n",
          "}\n"))
        }, error = function(x) CodeList)

        ExperimentGrid <- tryCatch({
          AutoQuant:::ParallelAutoETS(
            MetricSelection = ArgsList[['EvaluationMetric']],
            Output = ETS_Artifacts_Build,
            NumCores = NumberCores,
            TrainValidateShare = ArgsList[['TrainWeighting']])
        }, error = function(x) {
          if(DebugFC) print("AutoQuant:::ParallelAutoETS failed")
          NULL
        })
        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ETS Build\n",
          "ExperimentGrid <- tryCatch({\n  ",
          "AutoQuant:::ParallelAutoETS(\n    ",
          "MetricSelection = ArgsList[['EvaluationMetric']],\n    ",
          "Output = ETS_Artifacts_Build,\n    ",
          "NumCores = NumberCores,\n    ",
          "TrainValidateShare = ArgsList[['TrainWeighting']])\n",
          "}, error = function(x) {\n  ",
          "print('AutoQuant:::ParallelAutoETS failed')\n  ",
          "NULL\n",
          "})\n"))
        }, error = function(x) CodeList)
      } else {
        if(DebugFC) print("*_Artifacts_Build is NULL")
      }
    }

    # ARFIMA
    if(Algo == "ARFIMA") {

      # 1. Create time series artifacts----
      if(DebugFC) {
        if(DebugFC) print('here 10.56')
        if(DebugFC) print(names(data))
        if(DebugFC) print(str(data))
        if(DebugFC) print(ArgsList[['TargetColumnName']])
        if(DebugFC) print(ArgsList[['DateColumnName']])
        if(DebugFC) print(ArgsList[['MaxLags']])
        if(DebugFC) print(ArgsList[['MaxMovingAverages']])
        if(DebugFC) print(ArgsList[['TimeUnit']])
        if(DebugFC) print(ArgsList[['FCPeriods']])
        if(DebugFC) print('here 10.561')
      }

      # Data Prepare
      # Experimental Grid Features from Backend Functions
      #   "DataSetName", "BoxCox", "IncludeDrift", "SeasonalDifferences", "SeasonalMovingAverages",
      #   "SeasonalLags", "MaxFourierTerms", "Differences", "MovingAverages	Lags"
      # Experimental Grid Features from App
      #   TODO
      Arfima_Artifacts_Build <- tryCatch({
        AutoQuant:::TimeSeriesDataPrepare(
          data = data,
          TargetName = ArgsList[['TargetColumnName']],
          DateName = ArgsList[['DateColumnName']],
          Lags = ArgsList[['MaxLags']],
          SeasonalLags = 0,
          MovingAverages = ArgsList[['MaxMovingAverages']],
          SeasonalMovingAverages = 0,
          TimeUnit = ArgsList[['TimeUnit']],
          FCPeriods = 1,
          HoldOutPeriods = ArgsList[['FCPeriods']],
          TSClean = TRUE,
          ModelFreq = TRUE,
          FinalBuild = FALSE)
      }, error = function(x) {
        if(DebugFC) print("AutoQuant:::TimeSeriesDataPrepare failed")
        NULL
      })
      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# ARFIMA Args\n",
        "ARFIMA_Artifacts_Build <- tryCatch({\n  ",
        "AutoQuant:::TimeSeriesDataPrepare(\n    ",
        "data = data,\n    ",
        "TargetName = ", Quantico:::CEP(ArgsList[['TargetColumnName']]),",\n    ",
        "DateName = ", Quantico:::CEP(ArgsList[['DatetColumnName']]),",\n    ",
        "Lags = ", Quantico:::CEPP(ArgsList[['MaxLags']]),",\n    ",
        "SeasonalLags = 0,\n    ",
        "MovingAverages = ", Quantico:::CEPP(ArgsList[['MaxMovingAverages']]),",\n    ",
        "SeasonalMovingAverages = 0,\n    ",
        "TimeUnit = ", Quantico:::CEP(ArgsList[['TimeUnit']]),",\n    ",
        "FCPeriods = ", 1,",\n    ",
        "HoldOutPeriods = ", Quantico:::CEPP(ArgsList[['FCPeriods']]),",\n    ",
        "TSClean = TRUE,\n    ",
        "ModelFreq = TRUE,\n    ",
        "FinalBuilt = FALSE)\n",
        "}, error = function(x) {\n  ",
        "print('AutoQuant:::TimeSeriesDataPrepare failed')\n  ",
        "NULL\n",
        "})\n"))
      }, error = function(x) CodeList)

      # Create tons of models for evaluation
      if(length(Arfima_Artifacts_Build) > 0L) {

        if(DebugFC)  print('here 10.57')

        # 2. Find Best ARFIMA Models----
        cores <- parallel::detectCores()
        if(cores >= 4L) {
          NumberCores <- 4L
        } else {
          NumberCores <- cores
        }
        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Parallel Runs\n",
          "cores <- parallel::detectCores()\n",
          "if(cores >= 4L) {\n  ",
          "NumberCores <- 4L\n",
          "} else {\n  ",
          "NumberCores <- cores\n",
          "}\n"))
        }, error = function(x) CodeList)

        ExperimentGrid <- tryCatch({
          AutoQuant:::ParallelAutoArfima(
            MetricSelection = ArgsList[['EvaluationMetric']],
            Output = Arfima_Artifacts_Build,
            NumCores = NumberCores,
            TrainValidateShare = ArgsList[['TrainWeighting']])
        }, error = function(x) {
          if(DebugFC) print("AutoQuant:::ParallelAutoArfima failed")
          NULL
        })
        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# ARFIMA Build\n",
          "ExperimentGrid <- tryCatch({\n  ",
          "AutoQuant:::ParallelAutoARFIMA(\n    ",
          "MetricSelection = ArgsList[['EvaluationMetric']],\n    ",
          "Output = ARFIMA_Artifacts_Build,\n    ",
          "NumCores = NumberCores,\n    ",
          "TrainValidateShare = ArgsList[['TrainWeighting']])\n",
          "}, error = function(x) {\n  ",
          "print('AutoQuant:::ParallelAutoARIMA failed')\n  ",
          "NULL\n",
          "})\n"))
        }, error = function(x) CodeList)
      } else {
        if(DebugFC) print("*_Artifacts_Build is NULL")
      }
    }

    # NNET
    if(Algo == "NNET") {

      # 1. Create time series artifacts----
      if(DebugFC) {
        if(DebugFC) print('here 10.56')
        if(DebugFC) print(names(data))
        if(DebugFC) print(str(data))
        if(DebugFC) print(ArgsList[['TargetColumnName']])
        if(DebugFC) print(ArgsList[['DateColumnName']])
        if(DebugFC) print(ArgsList[['MaxLags']])
        if(DebugFC) print(ArgsList[['MaxMovingAverages']])
        if(DebugFC) print(ArgsList[['TimeUnit']])
        if(DebugFC) print(ArgsList[['FCPeriods']])
        if(DebugFC) print('here 10.561')
      }

      # Data Prepare
      # Experimental Grid Features from Backend Functions
      #   "DataSetName", "BoxCox", "IncludeDrift", "SeasonalDifferences", "SeasonalMovingAverages",
      #   "SeasonalLags", "MaxFourierTerms", "Differences", "MovingAverages	Lags"
      # Experimental Grid Features from App
      #   TODO
      NNET_Artifacts_Build <- tryCatch({
        AutoQuant:::TimeSeriesDataPrepare(
          data = data,
          TargetName = ArgsList[['TargetColumnName']],
          DateName = ArgsList[['DateColumnName']],
          Lags = ArgsList[['MaxLags']],
          SeasonalLags = ArgsList[['MaxSeasonalLags']],
          MovingAverages = 0,
          SeasonalMovingAverages = 0,
          TimeUnit = ArgsList[['TimeUnit']],
          FCPeriods = 1,
          HoldOutPeriods = ArgsList[['FCPeriods']],
          TSClean = TRUE,
          ModelFreq = TRUE,
          FinalBuild = FALSE)
      }, error = function(x) {
        if(DebugFC) print("AutoQuant:::TimeSeriesDataPrepare failed")
        NULL
      })
      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# NNET Args\n",
        "NNET_Artifacts_Build <- tryCatch({\n  ",
        "AutoQuant:::TimeSeriesDataPrepare(\n    ",
        "data = data,\n    ",
        "TargetName = ", Quantico:::CEP(ArgsList[['TargetColumnName']]),",\n    ",
        "DateName = ", Quantico:::CEP(ArgsList[['DatetColumnName']]),",\n    ",
        "Lags = ", Quantico:::CEPP(ArgsList[['MaxLags']]),",\n    ",
        "SeasonalLags = ", Quantico:::CEPP(ArgsList[['MaxSeasonalLags']]),",\n    ",
        "MovingAverages = 0,\n    ",
        "SeasonalMovingAverages = 0,\n    ",
        "TimeUnit = ", Quantico:::CEP(ArgsList[['TimeUnit']]),",\n    ",
        "FCPeriods = ", 1,",\n    ",
        "HoldOutPeriods = ", Quantico:::CEPP(ArgsList[['FCPeriods']]),",\n    ",
        "TSClean = TRUE,\n    ",
        "ModelFreq = TRUE,\n    ",
        "FinalBuilt = FALSE)\n",
        "}, error = function(x) {\n  ",
        "print('AutoQuant:::TimeSeriesDataPrepare failed')\n  ",
        "NULL\n",
        "})\n"))
      }, error = function(x) CodeList)

      # Create tons of models for evaluation
      if(length(NNET_Artifacts_Build) > 0L) {

        if(DebugFC)  print('here 10.57')

        # 2. Find Best NNET Models----
        cores <- parallel::detectCores()
        if(cores >= 4L) {
          NumberCores <- 4L
        } else {
          NumberCores <- cores
        }
        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Parallel Runs\n",
          "cores <- parallel::detectCores()\n",
          "if(cores >= 4L) {\n  ",
          "NumberCores <- 4L\n",
          "} else {\n  ",
          "NumberCores <- cores\n",
          "}\n"))
        }, error = function(x) CodeList)

        ExperimentGrid <- tryCatch({
          AutoQuant:::ParallelAutoNNET(
            MetricSelection = ArgsList[['EvaluationMetric']],
            Output = NNET_Artifacts_Build,
            MaxFourierTerms = ArgsList[['MaxFourierPairs']],
            MaxNumberModels = ArgsList[['MaxNumberModel']],
            MaxRunMinutes = ArgsList[['MaxRunTimeMinute']],
            MaxRunsWithoutNewWinner = ArgsList[['MaxConsecutiveFail']],
            NumCores = NumberCores,
            TrainValidateShare = ArgsList[['TrainWeighting']])
        }, error = function(x) {
          if(DebugFC) print("AutoQuant:::ParallelAutoETS failed")
          NULL
        })
        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# NNET Build\n",
          "ExperimentGrid <- tryCatch({\n  ",
          "AutoQuant:::ParallelAutoNNET(\n    ",
          "MetricSelection = ArgsList[['EvaluationMetric']],\n    ",
          "Output = NNET_Artifacts_Build,\n    ",
          "MaxFourierTerms = ", Quantico:::CEPP(ArgsList[['MaxFourierPairs']]), ",\n    ",
          "MaxNumberModels = ", Quantico:::CEPP(ArgsList[['MaxNumberModel']]), ",\n    ",
          "MaxRunMinutes = ", Quantico:::CEPP(ArgsList[['MaxRunTimeMinute']]), ",\n    ",
          "MaxRunsWithoutNewWinner = ", Quantico:::CEPP(ArgsList[['MaxConsecutiveFail']]), ",\n    ",
          "NumCores = NumberCores,\n    ",
          "TrainValidateShare = ArgsList[['TrainWeighting']])\n",
          "}, error = function(x) {\n  ",
          "print('AutoQuant:::ParallelAutoARIMA failed')\n  ",
          "NULL\n",
          "})\n"))
        }, error = function(x) CodeList)
      } else {
        if(DebugFC) print("*_Artifacts_Build is NULL")
      }
    }

    if(DebugFC) {
      if(DebugFC) print('here 10.58')
      if(DebugFC) print(tryCatch({ExperimentGrid}, error = function(x) NULL))
    }

    if(!exists("ExperimentGrid")) return(NULL)

    # Change Names of Columns in Experiment Grid
    if(length(ExperimentGrid) > 0L) {
      data.table::setnames(
        ExperimentGrid,
        c(
          "Train_MSE",
          "Train_MAE",
          "Train_MAPE",
          "Validate_MSE",
          "Validate_MAE",
          "Validate_MAPE",
          "Blended_MSE",
          "Blended_MAE",
          "Blended_MAPE"
        ),
        c(
          "MSE Train",
          "MAE Train",
          "MAPE Train",
          "MSE Validate",
          "MAE Validate",
          "MAPE Validate",
          "MSE Blended",
          "MAE Blended",
          "MAPE Blended"
        ), skip_absent = TRUE
      )
    }

    # Update Collection Lists
    ArgsList[[paste0(Algo, "_", ModelID, '_ExperimentGrid')]] <- ExperimentGrid
    if(DebugFC) print(names(ArgsList))
    DataList[[paste0(Algo, "_", ModelID, '_ExperimentGrid')]][['data']] <- ExperimentGrid

    # Rename and update list code blocks
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Change Names of Columns in Experiment Grid\n",
      "if(length(ExperimentGrid) > 0L) {\n  ",
      "data.table::setnames(\n    ",
      "ExperimentGrid,\n    ",
      "c(\n      ",
      "'Train_MSE',\n      ",
      "'Train_MAE',\n      ",
      "'Train_MAPE',\n      ",
      "'Validate_MSE',\n      ",
      "'Validate_MAE',\n      ",
      "'Validate_MAPE',\n      ",
      "'Blended_MSE',\n      ",
      "'Blended_MAE',\n      ",
      "'Blended_MAPE',\n    ",
      "),\n    ",
      "c(\n      ",
      "'MSE Train',\n      ",
      "'MAE Train',\n      ",
      "'MAPE Train',\n      ",
      "'MSE Validate',\n      ",
      "'MAE Validate',\n      ",
      "'MAPE Validate',\n      ",
      "'MSE Blended',\n      ",
      "'MAE Blended',\n      ",
      "'MAPE Blended',\n    ",
      ")\n  ",
      ")\n",
      "}\n\n",
      "# Update Collection Lists\n",
      "ArgsList[[", paste0(Quantico:::CEP(Algo), "_", Quantico:::CEP(ModelID), "_ExperimentGrid"), "]] <- ExperimentGrid\n",
      "DataList[[", paste0(Quantico:::CEP(Algo), "_", Quantico:::CEP(ModelID), "_ExperimentGrid"), "]][['data']] <- ExperimentGrid\n\n",
      "# GRID TUNING END ::::::::::: \n"))
    }, error = function(x) CodeList)

  }

  # ----

  # **************************************** ----
  # Forecast                                 ----
  # **************************************** ----
  if(DebugFC)  print('here 5.0 b')
  if(ArgsList[['RunMode']] == "Forecast") {

    # Logic to check to see if the ARFIMA functions will be able to run without error:
    #   ExperimentGrid is the Grid Tune output data.table. It stores parameter settings and evalution metrics for every model
    #   data is the user-supplied training data
    if(DebugFC) {# Check the contents of the if-statement below
      if(DebugFC) print("here 6.0")
      if(DebugFC) print(length(ArgsList))
      if(DebugFC) print(names(ArgsList))
      if(DebugFC) print(ModelID)
      if(DebugFC) print(ArgsList[[paste0(ModelID, '_ExperimentGrid')]])
      if(DebugFC) print(data.table::is.data.table(ArgsList[['data']]))
    }

    # Determine if a run is valid: if not, return NULL
    if(length(ArgsList[[paste0(ModelID, '_ExperimentGrid')]]) > 0L && ArgsList[[paste0(ModelID, '_ExperimentGrid')]] != "" && data.table::is.data.table(ArgsList[['data']])) {
      ValidRun <- TRUE
    } else {
      ValidRun <- FALSE
    }

    # Valid run code block
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n\n",
      "# FORECASTING ::::::::::: \n\n",
      "# Determine if a run is valid: if not, return NULL\n",
      "if(length(ArgsList[[paste0(ModelID, '_ExperimentGrid')]]) > 0L && ArgsList[[paste0(ModelID, '_ExperimentGrid')]] != '' && data.table::is.data.table(ArgsList[['data']])) {\n  ",
      "ValidRun <- TRUE\n",
      "} else {\n  ",
      "ValidRun <- FALSE\n",
      "}\n"))
    }, error = function(x) CodeList)

    if(DebugFC)  print('here 7.0')

    # Proceed or return NULL
    if(!ValidRun) return(NULL)

    #  ----

    # ********************************* ----
    # Common args                       ----
    # ********************************* ----

    if(DebugFC)  print('here 8.0')
    ExperimentGrid <- ArgsList[[paste0(ModelID, '_ExperimentGrid')]]
    data.table::setnames(
      ExperimentGrid,
      c("MSE Train", "MAE Train", "MAPE Train",
        "MSE Validate", "MAE Validate", "MAPE Validate",
        "MSE Blended", "MAE Blended", "MAPE Blended"),
      c("Train_MSE", "Train_MAE", "Train_MAPE",
        "Validate_MSE", "Validate_MAE", "Validate_MAPE",
        "Blended_MSE", "Blended_MAE", "Blended_MAPE"
      ), skip_absent = TRUE)

    # Rename columns code block
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Change Names of Columns in Experiment Grid\n",
      "data.table::setnames(\n  ",
      "ExperimentGrid,\n  ",
      "c(\n    ",
      "'MSE Train',\n    ",
      "'MAE Train',\n    ",
      "'MAPE Train',\n    ",
      "'MSE Validate',\n    ",
      "'MAE Validate',\n    ",
      "'MAPE Validate',\n    ",
      "'MSE Blended',\n    ",
      "'MAE Blended',\n    ",
      "'MAPE Blended',\n  ",
      "),\n  ",
      "c(\n    ",
      "'Train_MSE',\n    ",
      "'Train_MAE',\n    ",
      "'Train_MAPE',\n    ",
      "'Validate_MSE',\n    ",
      "'Validate_MAE',\n    ",
      "'Validate_MAPE',\n    ",
      "'Blended_MSE',\n    ",
      "'Blended_MAE',\n    ",
      "'Blended_MAPE',\n  ",
      "), skip_absent = TRUE\n",
      ")\n\n"))
    }, error = function(x) CodeList)

    # Update Variables and data
    ExperimentGrid <- ExperimentGrid[order(-get(paste0("Blended_", ArgsList[['EvaluationMetric']])))]
    data.table::setorderv(ExperimentGrid, cols = paste0("Blended_", ArgsList[['EvaluationMetric']]), order = -1L)
    ArgsList[['DebugMode']] <- DebugFC
    data <- data.table::copy(ArgsList[['data']][, .SD, .SDcols = c(ArgsList[['DateColumnName']], ArgsList[['TargetColumnName']])])
    data <- data[, mean(get(ArgsList[['TargetColumnName']])), by = eval(ArgsList[['DateColumnName']])]
    data.table::setnames(data, c(names(data)[1L], names(data)[2L]), c(ArgsList[['DateColumnName']], ArgsList[['TargetColumnName']]))
    FCPeriods <- Quantico::ReturnParam(xx = input[[paste0(Algo, "_FCPeriods")]], Type = "numeric", Default = 5)
    ArgsList[['TimeUnit']] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, "_TimeUnit")]]}, error = NULL), Type = "character", Default = NULL)

    # Update Variables code block
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Update Variables and data\n",
      "ExperimentGrid <- ExperimentGrid[order(-get(paste0('Blended_', ArgsList[['EvaluationMetric']])))]\n",
      "data.table::setorderv(ExperimentGrid, cols = paste0('Blended_', ArgsList[['EvaluationMetric']]), order = -1L)\n",
      "ArgsList[['DebugMode']] <- DebugFC\n",
      "data <- data.table::copy(ArgsList[['data']][, .SD, .SDcols = c(ArgsList[['DateColumnName']], ArgsList[['TargetColumnName']])])\n",
      "data <- data[, mean(get(ArgsList[['TargetColumnName']])), by = eval(ArgsList[['DateColumnName']])]\n",
      "data.table::setnames(data, c(names(data)[1L], names(data)[2L]), c(ArgsList[['DateColumnName']], ArgsList[['TargetColumnName']]))\n",
      "FCPeriods <- ", Quantico:::CEPP(FCPeriods), "\n",
      "ArgsList[['TimeUnit']] <- ", Quantico:::CEP(ArgsList[['TimeUnit']]), "\n",
    ))
    }, error = function(x) {
      if(DebugFC) print("# Update Variables code block failed")
      if(DebugFC) print(FCPeriods)
      if(DebugFC) print(ArgsList[['TimeUnit']])
      CodeList
    })

    # Convert IDate class to Date class
    if(class(data[[ArgsList[['DateColumnName']]]])[1L] %in% "IDate") {
      data[, eval(ArgsList[['DateColumnName']]) := as.Date(get(ArgsList[['DateColumnName']]))]
    }

    # Ensure DateVariable is a Date Type
    check <- class(data[[ArgsList[['DateColumnName']]]])[1L]
    if(check %in% c("numeric", "integer", "character", "factor", "logical")) {
      if(!(tolower(ArgsList[['TimeUnit']]) %chin% c('1min','5min','10min','15min','30min','hour'))) {
        x <- data[1L, get(ArgsList[['DateColumnName']])]
        x1 <- lubridate::guess_formats(x, orders = c('mdY', 'BdY', 'Bdy', 'bdY', 'bdy', 'mdy', 'dby', 'Ymd', 'Ydm'))
        data[, eval(ArgsList[['DateColumnName']]) := as.Date(get(ArgsList[['DateColumnName']]), tryFormats = x1)]
      } else {
        data[, eval(ArgsList[['DateColumnName']]) := as.POSIXct(get(ArgsList[['DateColumnName']]))]
      }
    }

    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Convert IDate class to Date class\n",
      "if(class(data[[ArgsList[['DateColumnName']]]])[1L] %in% 'IDate') {\n  ",
      "data[, eval(ArgsList[['DateColumnName']]) := as.Date(get(ArgsList[['DateColumnName']]))]\n",
      "}\n")
    )
    }, error = function(x) CodeList)

    if(DebugFC)  print('here 9.0')

    # ----

    # ********************************* ----
    # ALGO SPECIFIC Forecast            ----
    # ********************************* ----

    # TBATS
    if(Algo == "TBATS") {

      if(DebugFC)  print('here 10.5')

      # Pick up where Grid Tuning Left off

      # Features
      if(DebugFC) {
        if(DebugFC) print(paste0("TargetName == ", ArgsList[['TargetColumnName']]))
        if(DebugFC) print(paste0("DateName == ", ArgsList[['DateColumnName']]))
        if(DebugFC) print(paste0("Lags == ", as.integer(ExperimentGrid[1L, Lags])))
        if(DebugFC) print(paste0("MovingAverages == ", as.integer(ExperimentGrid[1L, MovingAverages])))
        if(DebugFC) print(paste0("TimeUnit == ", ArgsList[['TimeUnit']]))
        if(DebugFC) print(paste0("FCPeriods == ", FCPeriods))
        if(DebugFC) print(paste0("HoldOutPeriods == ", 0))
        if(DebugFC) print(paste0("TSClean == ", TRUE))
        if(DebugFC) print(paste0("ModelFreq == ", TRUE))
        if(DebugFC) print(paste0("FinalBuild == ", TRUE))
        if(DebugFC) print(paste0("ArgsList[['EvaluationMetric']] == ", ArgsList[['EvaluationMetric']]))
      }

      # Data Prepare
      # Experimental Grid Features from Backend Functions
      #   "DataSetName", "BoxCox", "IncludeDrift", "SeasonalDifferences", "SeasonalMovingAverages",
      #   "SeasonalLags", "MaxFourierTerms", "Differences", "MovingAverages	Lags"
      # Experimental Grid Features from App
      #   TODO
      TBATS_Artifacts_Score <- tryCatch({
        AutoQuant:::TimeSeriesDataPrepare(
          data = data,
          TargetName = ArgsList[['TargetColumnName']],
          DateName = ArgsList[['DateColumnName']],
          Lags = as.integer(ExperimentGrid[1L, Lags]),
          SeasonalLags = 0,
          MovingAverages = as.integer(ExperimentGrid[1L, MovingAverages]),
          SeasonalMovingAverages = 0,
          TimeUnit = ArgsList[['TimeUnit']],
          FCPeriods = FCPeriods,
          HoldOutPeriods = 0,
          TSClean = TRUE,
          ModelFreq = TRUE,
          FinalBuild = TRUE)
      }, error = function(x) {
        if(DebugFC) print("AutoQuant:::TimeSeriesDataPrepare failed")
        if(DebugFC) print(paste0("data[,.N] = ", tryCatch({data[,.N]}, error = function(x) NULL)))
        if(DebugFC) print(paste0("TargetName = ", ArgsList[['TargetColumnName']]))
        if(DebugFC) print(paste0("DateName = ", ArgsList[['DateColumnName']]))
        if(DebugFC) print(paste0("Lags = ", as.integer(ExperimentGrid[1L, Lags])))
        if(DebugFC) print(paste0("SeasonalLags = ", 0))
        if(DebugFC) print(paste0("MovingAverages = ", as.integer(ExperimentGrid[1L, MovingAverages])))
        if(DebugFC) print(paste0("SeasonalMovingAverages = ", 0))
        if(DebugFC) print(paste0("TimeUnit = ", ArgsList[['TimeUnit']]))
        if(DebugFC) print(paste0("FCPeriods = ", FCPeriods))
        if(DebugFC) print(paste0("HoldOutPeriods = ", 0))
        if(DebugFC) print(paste0("TSClean = ", TRUE))
        if(DebugFC) print(paste0("ModelFreq = ", TRUE))
        if(DebugFC) print(paste0("FinalBuild = ", TRUE))
        NULL
      })

      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Data Prepare\n",
        "TBATS_Artifacts_Score <- tryCatch({\n  ",
        "AutoQuant:::TimeSeriesDataPrepare(\n    ",
        "data = data,\n    ",
        "TargetName = ArgsList[['TargetColumnName']],\n    ",
        "DateName = ArgsList[['DateColumnName']],\n    ",
        "Lags = 0,\n    ",
        "SeasonalLags = 0,\n    ",
        "MovingAverages = 0,\n    ",
        "SeasonalMovingAverages = 0,\n    ",
        "TimeUnit = ArgsList[['TimeUnit']],\n    ",
        "FCPeriods = FCPeriods,\n    ",
        "HoldOutPeriods = 0,\n    ",
        "TSClean = TRUE,\n    ",
        "ModelFreq = TRUE,\n    ",
        "FinalBuild = TRUE)\n",
        "}, error = function(x) {\n  ",
        "print('AutoQuant:::TimeSeriesDataPrepare failed')\n  ",
        "NULL\n",
        "})\n"))
      }, error = function(x) CodeList)

      if(DebugFC)  print('here 10.53')

      if(length(TBATS_Artifacts_Score) > 0L) {

        # Generate Final TBATS Forecast ----
        # repeat so long as there isn't a forecast generated or no more models left to use
        # upon generating a final forecast, a new model is built utilizing 100% of training data
        # Sometimes, this new model doesn't get built
        #   If it doesn't get built then the next best version will be attempted (based on ExperimentGrid data.table)
        #   the ExperimentGrid is sorted by paste('Blended_', ArgsList[['EvaluationMetric']])
        counter <- ExperimentGrid[, .N] + 1L
        repeat {
          counter <- counter - 1L
          if(counter == 0) break
          ForecastOutput <- tryCatch({
            AutoQuant:::FinalBuildTBATS(
              ModelOutputGrid = ExperimentGrid,
              SavePath = NULL,
              TimeSeriesPrepareOutput = TBATS_Artifacts_Score,
              FCPeriods = FCPeriods,
              NumberModelsScore = 1,
              MetricSelection = ArgsList[['EvaluationMetric']],
              DebugMode = DebugFC,
              ByDataType = FALSE)
          }, error = function(x) {
            if(DebugFC) print("AutoQuant:::FinalBuildTBATS failed")
            if(DebugFC) print(paste0("counter = ", counter))
            if(DebugFC) print(paste0("ModelOutputGrid = ", ExperimentGrid))
            if(DebugFC) print(paste0("SavePath = NULL"))
            if(DebugFC) print(paste0("TimeSeriesPrepareOutput = ", TBATS_Artifacts_Score))
            if(DebugFC) print(paste0("FCPeriods = ", FCPeriods))
            if(DebugFC) print(paste0("NumberModelsScore = 1"))
            if(DebugFC) print(paste0("MetricSelection = ", ArgsList[['EvaluationMetric']]))
            if(DebugFC) print(paste0("DebugMode = ", DebugFC))
            if(DebugFC) print(paste0("ByDataType = FALSE"))
            NULL
          })
          if(length(ForecastOutput) > 0L && !is.na(ForecastOutput[.N][["Forecast"]])) {
            break
          } else {
            if(length(ForecastOutput) == 0L) {
              if(DebugFC) print("length(ForecastOutput) == 0L")
            } else if(!is.na(ForecastOutput[.N][["Forecast"]])) {
              if(DebugFC) print("!is.na(ForecastOutput[.N][['Forecast']]) == FALSE")
            }
          }
        }

        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Generate Final TBATS Forecast\n",
          "counter <- ExperimentGrid[, .N] + 1L\n",
          "repeat {\n  ",
          "counter <- counter - 1L\n  ",
          "if(counter == 0) break\n  ",
          "ForecastOutput <- tryCatch({\n    ",
          "AutoQuant:::FinalBuildTBATS(\n      ",
          "ModelOutputGrid = ExperimentGrid,\n      ",
          "SavePath = NULL,\n      ",
          "TimeSeriesPrepareOutput = TBATS_Artifacts_Score,\n      ",
          "FCPeriods = FCPeriods,\n      ",
          "NumberModelsScore = 1,\n      ",
          "MetricSelection = ArgsList[['EvaluationMetric']],\n      ",
          "ByDataType = FALSE)\n  ",
          "}, error = function(x) {\n    ",
          "print('AutoQuant:::FinalBuildTBATS failed')\n    ",
          "NULL\n  ",
          "})\n  ",
          "if(length(ForecastOutput) > 0L && !is.na(ForecastOutput[.N][['Forecast']])) {\n    ",
          "break\n  ",
          "} else {\n    ",
          "if(length(ForecastOutput) == 0L) {\n      ",
          "print('length(ForecastOutput) == 0L')\n    ",
          "} else if(!is.na(ForecastOutput[.N][['Forecast']])) {\n      ",
          "print('!is.na(ForecastOutput[.N][['Forecast']]) == FALSE')\n    ",
          "}\n  ", # closes else if
          "}\n", # closes else
          "}\n" # closes repeat
        ))
        }, error = function(x) CodeList)
      }
    }

    # SARIMA
    if(Algo == "SARIMA") {
      if(DebugFC)  print('here 10.5')

      # Pick up where Grid Tuning Left off

      # Features ----
      if(DebugFC) {
        if(DebugFC) print(paste0("TargetName == ", ArgsList[['TargetColumnName']]))
        if(DebugFC) print(paste0("DateName == ", ArgsList[['DateColumnName']]))
        if(DebugFC) print(paste0("Lags == ", as.integer(ExperimentGrid[1L, Lags])))
        if(DebugFC) print(paste0("SeasonalLags == ", ExperimentGrid[1L, SeasonalLags]))
        if(DebugFC) print(paste0("MovingAverages == ", as.integer(ExperimentGrid[1L, MovingAverages])))
        if(DebugFC) print(paste0("SeasonalMovingAverages == ", as.integer(ExperimentGrid[1L, SeasonalMovingAverages])))
        if(DebugFC) print(paste0("TimeUnit == ", ArgsList[['TimeUnit']]))
        if(DebugFC) print(paste0("FCPeriods == ", FCPeriods))
        if(DebugFC) print(paste0("HoldOutPeriods == ", 0))
        if(DebugFC) print(paste0("TSClean == ", TRUE))
        if(DebugFC) print(paste0("ModelFreq == ", TRUE))
        if(DebugFC) print(paste0("FinalBuild == ", TRUE))
        if(DebugFC) print(paste0("ArgsList[['EvaluationMetric']] == ", ArgsList[['EvaluationMetric']]))
      }

      # Data Prepare
      # Experimental Grid Features from Backend Functions
      #   "DataSetName", "BoxCox", "IncludeDrift", "SeasonalDifferences", "SeasonalMovingAverages",
      #   "SeasonalLags", "MaxFourierTerms", "Differences", "MovingAverages	Lags"
      # Experimental Grid Features from App
      #   TODO
      SARIMA_Artifacts_Score <- tryCatch({
        AutoQuant:::TimeSeriesDataPrepare(
          data = data,
          TargetName = ArgsList[['TargetColumnName']],
          DateName = ArgsList[['DateColumnName']],
          Lags = as.integer(ExperimentGrid[1L, Lags]),
          SeasonalLags = as.integer(ExperimentGrid[1L, SeasonalLags]),
          MovingAverages = as.integer(ExperimentGrid[1L, MovingAverages]),
          SeasonalMovingAverages = as.integer(ExperimentGrid[1L, SeasonalMovingAverages]),
          TimeUnit = ArgsList[['TimeUnit']],
          FCPeriods = FCPeriods,
          HoldOutPeriods = 0,
          TSClean = TRUE,
          ModelFreq = TRUE,
          FinalBuild = TRUE)
      }, error = function(x) {
        if(DebugFC) print("AutoQuant:::TimeSeriesDataPrepare failed")
        NULL
      })

      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Data Prepare\n",
        "SARIMA_Artifacts_Score <- tryCatch({\n  ",
        "AutoQuant:::TimeSeriesDataPrepare(\n    ",
        "data = data,\n    ",
        "TargetName = ArgsList[['TargetColumnName']],\n    ",
        "DateName = ArgsList[['DateColumnName']],\n    ",
        "Lags = as.integer(ExperimentGrid[1L, Lags]),\n    ",
        "SeasonalLags = as.integer(ExperimentGrid[1L, SeasonalLags]),\n    ",
        "MovingAverages = as.integer(ExperimentGrid[1L, MovingAverages]),\n    ",
        "SeasonalMovingAverages = as.integer(ExperimentGrid[1L, SeasonalMovingAverages]),\n    ",
        "TimeUnit = ArgsList[['TimeUnit']],\n    ",
        "FCPeriods = FCPeriods,\n    ",
        "HoldOutPeriods = 0,\n    ",
        "TSClean = TRUE,\n    ",
        "ModelFreq = TRUE,\n    ",
        "FinalBuild = TRUE)\n",
        "}, error = function(x) {\n  ",
        "print('AutoQuant:::TimeSeriesDataPrepare failed')\n  ",
        "NULL\n",
        "})\n"))
      }, error = function(x) CodeList)

      if(DebugFC)  print('here 10.53')

      if(length(SARIMA_Artifacts_Score) > 0L) {

        # Generate Final Sarima Forecast ----
        # repeat so long as there isn't a forecast generated or no more models left to use
        # upon generating a final forecast, a new model is built utilizing 100% of training data
        # Sometimes, this new model doesn't get built
        #   If it doesn't get built then the next best version will be attempted (based on ExperimentGrid data.table)
        #   the ExperimentGrid is sorted by paste('Blended_', ArgsList[['EvaluationMetric']])
        counter <- ExperimentGrid[, .N] + 1L
        repeat {
          counter <- counter - 1L
          if(counter == 0) break
          ForecastOutput <- tryCatch({
            AutoQuant:::FinalBuildArima(
              ModelOutputGrid = ExperimentGrid,
              SavePath = NULL,
              TimeSeriesPrepareOutput = SARIMA_Artifacts_Score,
              FCPeriods = FCPeriods,
              NumberModelsScore = 1,
              MetricSelection = ArgsList[['EvaluationMetric']],
              DebugMode = DebugFC,
              ByDataType = FALSE)
          }, error = function(x) {
            if(DebugFC) print("AutoQuant:::FinalBuildSARIMA failed")
            if(DebugFC) print(paste0("counter = ", counter))
            if(DebugFC) print(paste0("SavePath = NULL"))
            if(DebugFC) print(paste0("FCPeriods = ", FCPeriods))
            if(DebugFC) print(paste0("NumberModelsScore = 1"))
            if(DebugFC) print(paste0("MetricSelection = ", ArgsList[['EvaluationMetric']]))
            if(DebugFC) print(paste0("DebugMode = ", DebugFC))
            if(DebugFC) print(paste0("ByDataType = FALSE"))
            NULL
          })
          if(length(ForecastOutput) > 0L && !is.na(ForecastOutput[.N][["Forecast"]])) {
            break
          } else {
            if(length(ForecastOutput) == 0L) {
              if(DebugFC) print("length(ForecastOutput) == 0L")
            } else if(!is.na(ForecastOutput[.N][["Forecast"]])) {
              if(DebugFC) print("!is.na(ForecastOutput[.N][['Forecast']]) == FALSE")
            }
          }
        }

        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Generate Final SARIMA Forecast\n",
          "counter <- ExperimentGrid[, .N] + 1L\n",
          "repeat {\n  ",
          "counter <- counter - 1L\n  ",
          "if(counter == 0) break\n  ",
          "ForecastOutput <- tryCatch({\n    ",
          "AutoQuant:::FinalBuildArima(\n      ",
          "ModelOutputGrid = ExperimentGrid,\n      ",
          "SavePath = NULL,\n      ",
          "TimeSeriesPrepareOutput = SARIMA_Artifacts_Score,\n      ",
          "FCPeriods = FCPeriods,\n      ",
          "NumberModelsScore = 1,\n      ",
          "MetricSelection = ArgsList[['EvaluationMetric']],\n      ",
          "ByDataType = FALSE)\n    ",
          "}, error = function(x) {\n    ",
          "print('AutoQuant:::FinalBuildSARIMA failed')\n  ",
          "NULL\n  ",
          "})\n  ",
          "if(length(ForecastOutput) > 0L && !is.na(ForecastOutput[.N][['Forecast']])) {\n    ",
          "break\n  ",
          "} else {\n    ",
          "if(length(ForecastOutput) == 0L) {\n      ",
          "print('length(ForecastOutput) == 0L')\n    ",
          "} else if(!is.na(ForecastOutput[.N][['Forecast']])) {\n      ",
          "print('!is.na(ForecastOutput[.N][['Forecast']]) == FALSE')\n    ",
          "}\n  ", # closes else if
          "}\n", # closes else
          "}\n" # closes repeat
        ))
        }, error = function(x) CodeList)
      }
    }

    # ETS
    if(Algo == "ETS") {

      if(DebugFC)  print('here 10.5')

      # Pick up where Grid Tuning Left off

      # Features
      if(DebugFC) {
        if(DebugFC) print(paste0("TargetName == ", ArgsList[['TargetColumnName']]))
        if(DebugFC) print(paste0("DateName == ", ArgsList[['DateColumnName']]))
        if(DebugFC) print(paste0("TimeUnit == ", ArgsList[['TimeUnit']]))
        if(DebugFC) print(paste0("FCPeriods == ", FCPeriods))
        if(DebugFC) print(paste0("HoldOutPeriods == ", 0))
        if(DebugFC) print(paste0("TSClean == ", TRUE))
        if(DebugFC) print(paste0("ModelFreq == ", TRUE))
        if(DebugFC) print(paste0("FinalBuild == ", TRUE))
        if(DebugFC) print(paste0("ArgsList[['EvaluationMetric']] == ", ArgsList[['EvaluationMetric']]))
      }

      # Data Prepare
      # Experimental Grid Features from Backend Functions
      #   "DataSetName", "BoxCox", "IncludeDrift", "SeasonalDifferences", "SeasonalMovingAverages",
      #   "SeasonalLags", "MaxFourierTerms", "Differences", "MovingAverages	Lags"
      # Experimental Grid Features from App
      #   TODO
      ETS_Artifacts_Score <- tryCatch({
        AutoQuant:::TimeSeriesDataPrepare(
          data = data,
          TargetName = ArgsList[['TargetColumnName']],
          DateName = ArgsList[['DateColumnName']],
          Lags = 0, # as.integer(ExperimentGrid[1L, Lags]),
          SeasonalLags = 0,
          MovingAverages = 0, #as.integer(ExperimentGrid[1L, MovingAverages]),
          SeasonalMovingAverages = 0,
          TimeUnit = ArgsList[['TimeUnit']],
          FCPeriods = FCPeriods,
          HoldOutPeriods = 0,
          TSClean = TRUE,
          ModelFreq = TRUE,
          FinalBuild = TRUE)
      }, error = function(x) {
        if(DebugFC) print("AutoQuant:::TimeSeriesDataPrepare failed")
        NULL
      })

      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Data Prepare\n",
        "ETS_Artifacts_Score <- tryCatch({\n  ",
        "AutoQuant:::TimeSeriesDataPrepare(\n    ",
        "data = data,\n    ",
        "TargetName = ArgsList[['TargetColumnName']],\n    ",
        "DateName = ArgsList[['DateColumnName']],\n    ",
        "Lags = 0,\n    ",
        "SeasonalLags = 0,\n    ",
        "MovingAverages = 0,\n    ",
        "SeasonalMovingAverages = 0,\n    ",
        "TimeUnit = ArgsList[['TimeUnit']],\n    ",
        "FCPeriods = FCPeriods,\n    ",
        "HoldOutPeriods = 0,\n    ",
        "TSClean = TRUE,\n    ",
        "ModelFreq = TRUE,\n    ",
        "FinalBuild = TRUE)\n",
        "}, error = function(x) {\n  ",
        "print('AutoQuant:::TimeSeriesDataPrepare failed')\n  ",
        "NULL\n",
        "})\n"))
      }, error = function(x) CodeList)

      if(DebugFC)  print('here 10.53')

      if(length(ETS_Artifacts_Score) > 0L) {

        # Generate Final ETS Forecast ----
        # repeat so long as there isn't a forecast generated or no more models left to use
        # upon generating a final forecast, a new model is built utilizing 100% of training data
        # Sometimes, this new model doesn't get built
        #   If it doesn't get built then the next best version will be attempted (based on ExperimentGrid data.table)
        #   the ExperimentGrid is sorted by paste('Blended_', ArgsList[['EvaluationMetric']])
        counter <- ExperimentGrid[, .N] + 1L
        repeat {
          counter <- counter - 1L
          if(counter == 0) break
          ForecastOutput <- tryCatch({
            AutoQuant:::FinalBuildETS(
              ModelOutputGrid = ExperimentGrid,
              SavePath = NULL,
              TimeSeriesPrepareOutput = ETS_Artifacts_Score,
              FCPeriods = FCPeriods,
              NumberModelsScore = 1,
              MetricSelection = ArgsList[['EvaluationMetric']],
              DebugMode = DebugFC,
              ByDataType = FALSE)
          }, error = function(x) {
            if(DebugFC) print("AutoQuant:::FinalBuildETS failed")
            if(DebugFC) print(paste0("counter = ", counter))
            if(DebugFC) print(paste0("ModelOutputGrid = ", ExperimentGrid))
            if(DebugFC) print(paste0("SavePath = NULL"))
            if(DebugFC) print(paste0("TimeSeriesPrepareOutput = ", ETS_Artifacts_Score))
            if(DebugFC) print(paste0("FCPeriods = ", FCPeriods))
            if(DebugFC) print(paste0("NumberModelsScore = 1"))
            if(DebugFC) print(paste0("MetricSelection = ", ArgsList[['EvaluationMetric']]))
            if(DebugFC) print(paste0("DebugMode = ", DebugFC))
            if(DebugFC) print(paste0("ByDataType = FALSE"))
            NULL
          })
          if(length(ForecastOutput) > 0L && !is.na(ForecastOutput[.N][["Forecast"]])) {
            break
          } else {
            if(length(ForecastOutput) == 0L) {
              if(DebugFC) print("length(ForecastOutput) == 0L")
            } else if(!is.na(ForecastOutput[.N][["Forecast"]])) {
              if(DebugFC) print("!is.na(ForecastOutput[.N][['Forecast']]) == FALSE")
            }
          }
        }

        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Generate Final ETS Forecast\n",
          "counter <- ExperimentGrid[, .N] + 1L\n",
          "repeat {\n  ",
          "counter <- counter - 1L\n  ",
          "if(counter == 0) break\n  ",
          "ForecastOutput <- tryCatch({\n    ",
          "AutoQuant:::FinalBuildETS(\n      ",
          "ModelOutputGrid = ExperimentGrid,\n      ",
          "SavePath = NULL,\n      ",
          "TimeSeriesPrepareOutput = ETS_Artifacts_Score,\n      ",
          "FCPeriods = FCPeriods,\n      ",
          "NumberModelsScore = 1,\n      ",
          "MetricSelection = ArgsList[['EvaluationMetric']],\n      ",
          "ByDataType = FALSE)\n    ",
          "}, error = function(x) {\n    ",
          "print('AutoQuant:::FinalBuildETS failed')\n  ",
          "NULL\n  ",
          "})\n  ",
          "if(length(ForecastOutput) > 0L && !is.na(ForecastOutput[.N][['Forecast']])) {\n    ",
          "break\n  ",
          "} else {\n    ",
          "if(length(ForecastOutput) == 0L) {\n      ",
          "print('length(ForecastOutput) == 0L')\n    ",
          "} else if(!is.na(ForecastOutput[.N][['Forecast']])) {\n      ",
          "print('!is.na(ForecastOutput[.N][['Forecast']]) == FALSE')\n    ",
          "}\n  ", # closes else if
          "}\n", # closes else
          "}\n" # closes repeat
        ))
        }, error = function(x) CodeList)
      }
    }

    # ARFIMA
    if(Algo == "ARFIMA") {

      # Pick up where Grid Tuning Left off

      # Features ----
      if(DebugFC) {
        if(DebugFC) print('here 10.5')
        if(DebugFC) print(paste0("TargetName == ", ArgsList[['TargetColumnName']]))
        if(DebugFC) print(paste0("DateName == ", ArgsList[['DateColumnName']]))
        if(DebugFC) print(paste0("Lags == ", as.integer(ExperimentGrid[1L, Lags])))
        if(DebugFC) print(paste0("MovingAverages == ", as.integer(ExperimentGrid[1L, MovingAverages])))
        if(DebugFC) print(paste0("TimeUnit == ", ArgsList[['TimeUnit']]))
        if(DebugFC) print(paste0("FCPeriods == ", FCPeriods))
        if(DebugFC) print(paste0("HoldOutPeriods == ", 0))
        if(DebugFC) print(paste0("TSClean == ", TRUE))
        if(DebugFC) print(paste0("ModelFreq == ", TRUE))
        if(DebugFC) print(paste0("FinalBuild == ", TRUE))
        if(DebugFC) print(paste0("ArgsList[['EvaluationMetric']] == ", ArgsList[['EvaluationMetric']]))
      }

      # Data Prepare
      # Experimental Grid Features from Backend Functions
      #   "DataSetName", "BoxCox", "IncludeDrift", "SeasonalDifferences", "SeasonalMovingAverages",
      #   "SeasonalLags", "MaxFourierTerms", "Differences", "MovingAverages	Lags"
      # Experimental Grid Features from App
      #   TODO
      Arfima_Artifacts_Score <- tryCatch({
        AutoQuant:::TimeSeriesDataPrepare(
          data = data,
          TargetName = ArgsList[['TargetColumnName']],
          DateName = ArgsList[['DateColumnName']],
          Lags = as.integer(ExperimentGrid[1L, Lags]),
          SeasonalLags = 0,
          MovingAverages = as.integer(ExperimentGrid[1L, MovingAverages]),
          SeasonalMovingAverages = 0,
          TimeUnit = ArgsList[['TimeUnit']],
          FCPeriods = FCPeriods,
          HoldOutPeriods = 0,
          TSClean = TRUE,
          ModelFreq = TRUE,
          FinalBuild = TRUE)
      }, error = function(x) {
        if(DebugFC) print("AutoQuant:::TimeSeriesDataPrepare failed")
        NULL
      })

      # Data Prepare code block
      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Data Prepare\n",
        "ARFIMA_Artifacts_Score <- tryCatch({\n  ",
        "AutoQuant:::TimeSeriesDataPrepare(\n    ",
        "data = data,\n    ",
        "TargetName = ArgsList[['TargetColumnName']],\n    ",
        "DateName = ArgsList[['DateColumnName']],\n    ",
        "Lags = 0,\n    ",
        "SeasonalLags = 0,\n    ",
        "MovingAverages = 0,\n    ",
        "SeasonalMovingAverages = 0,\n    ",
        "TimeUnit = ArgsList[['TimeUnit']],\n    ",
        "FCPeriods = FCPeriods,\n    ",
        "HoldOutPeriods = 0,\n    ",
        "TSClean = TRUE,\n    ",
        "ModelFreq = TRUE,\n    ",
        "FinalBuild = TRUE)\n",
        "}, error = function(x) {\n  ",
        "print('AutoQuant:::TimeSeriesDataPrepare failed')\n  ",
        "NULL\n",
        "})\n"))
      }, error = function(x) CodeList)

      if(DebugFC)  print('here 10.53')

      if(length(Arfima_Artifacts_Score) > 0L) {

        # Generate Final Arfima Forecast ----
        # repeat so long as there isn't a forecast generated or no more models left to use
        # upon generating a final forecast, a new model is built utilizing 100% of training data
        # Sometimes, this new model doesn't get built
        #   If it doesn't get built then the next best version will be attempted (based on ExperimentGrid data.table)
        #   the ExperimentGrid is sorted by paste('Blended_', ArgsList[['EvaluationMetric']])
        counter <- ExperimentGrid[, .N] + 1L
        repeat {
          counter <- counter - 1L
          if(counter == 0) break
          ForecastOutput <- tryCatch({
            AutoQuant:::FinalBuildArfima(
              ModelOutputGrid = ExperimentGrid,
              SavePath = NULL,
              TimeSeriesPrepareOutput = Arfima_Artifacts_Score,
              FCPeriods = FCPeriods,
              NumberModelsScore = 1,
              MetricSelection = ArgsList[['EvaluationMetric']],
              DebugMode = DebugFC,
              ByDataType = FALSE)
          }, error = function(x) {
            if(DebugFC) print("AutoQuant:::FinalBuildArfima failed")
            if(DebugFC) print(paste0("counter = ", counter))
            if(DebugFC) print(paste0("ModelOutputGrid = ", ExperimentGrid))
            if(DebugFC) print(paste0("SavePath = NULL"))
            if(DebugFC) print(paste0("TimeSeriesPrepareOutput = ", ARFIMA_Artifacts_Score))
            if(DebugFC) print(paste0("FCPeriods = ", FCPeriods))
            if(DebugFC) print(paste0("NumberModelsScore = 1"))
            if(DebugFC) print(paste0("MetricSelection = ", ArgsList[['EvaluationMetric']]))
            if(DebugFC) print(paste0("DebugMode = ", DebugFC))
            if(DebugFC) print(paste0("ByDataType = FALSE"))
            NULL
          })
          if(length(ForecastOutput) > 0L && !is.na(ForecastOutput[.N][["Forecast"]])) {
            break
          } else {
            if(length(ForecastOutput) == 0L) {
              if(DebugFC) print("length(ForecastOutput) == 0L")
            } else if(!is.na(ForecastOutput[.N][["Forecast"]])) {
              if(DebugFC) print("!is.na(ForecastOutput[.N][['Forecast']]) == FALSE")
            }
          }
        }

        # Forecasting code block
        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Generate Final ARFIMA Forecast\n",
          "counter <- ExperimentGrid[, .N] + 1L\n",
          "repeat {\n  ",
          "counter <- counter - 1L\n  ",
          "if(counter == 0) break\n  ",
          "ForecastOutput <- tryCatch({\n    ",
          "AutoQuant:::FinalBuildARFIMA(\n      ",
          "ModelOutputGrid = ExperimentGrid,\n      ",
          "SavePath = NULL,\n      ",
          "TimeSeriesPrepareOutput = ARFIMA_Artifacts_Score,\n      ",
          "FCPeriods = FCPeriods,\n      ",
          "NumberModelsScore = 1,\n      ",
          "MetricSelection = ArgsList[['EvaluationMetric']],\n      ",
          "ByDataType = FALSE)\n    ",
          "}, error = function(x) {\n    ",
          "print('AutoQuant:::FinalBuildARFIMA failed')\n  ",
          "NULL\n  ",
          "})\n  ",
          "if(length(ForecastOutput) > 0L && !is.na(ForecastOutput[.N][['Forecast']])) {\n    ",
          "break\n  ",
          "} else {\n    ",
          "if(length(ForecastOutput) == 0L) {\n      ",
          "print('length(ForecastOutput) == 0L')\n    ",
          "} else if(!is.na(ForecastOutput[.N][['Forecast']])) {\n      ",
          "print('!is.na(ForecastOutput[.N][['Forecast']]) == FALSE')\n    ",
          "}\n  ", # closes else if
          "}\n", # closes else
          "}\n" # closes repeat
        ))
        }, error = function(x) CodeList)
      }
    }

    # NNET
    if(Algo == "NNET") {

      if(DebugFC)  print('here 10.5')

      # Pick up where Grid Tuning Left off

      # Features ----
      if(DebugFC) {
        if(DebugFC) print(paste0("TargetName == ", ArgsList[['TargetColumnName']]))
        if(DebugFC) print(paste0("DateName == ", ArgsList[['DateColumnName']]))
        if(DebugFC) print(paste0("Lags == ", as.integer(ExperimentGrid[1L, Lags])))
        if(DebugFC) print(paste0("SeasonalLags == ", as.integer(ExperimentGrid[1L, SeasonalLags])))
        if(DebugFC) print(paste0("MovingAverages == ", 0))
        if(DebugFC) print(paste0("SeasonalMovingAverages == ", 0))
        if(DebugFC) print(paste0("TimeUnit == ", ArgsList[['TimeUnit']]))
        if(DebugFC) print(paste0("FCPeriods == ", FCPeriods))
        if(DebugFC) print(paste0("HoldOutPeriods == ", 0))
        if(DebugFC) print(paste0("TSClean == ", TRUE))
        if(DebugFC) print(paste0("ModelFreq == ", TRUE))
        if(DebugFC) print(paste0("FinalBuild == ", TRUE))
        if(DebugFC) print(paste0("ArgsList[['EvaluationMetric']] == ", ArgsList[['EvaluationMetric']]))
      }

      # Data Prepare
      # Experimental Grid Features from Backend Functions
      #   "DataSetName", "BoxCox", "IncludeDrift", "SeasonalDifferences", "SeasonalMovingAverages",
      #   "SeasonalLags", "MaxFourierTerms", "Differences", "MovingAverages	Lags"
      # Experimental Grid Features from App
      #   TODO
      NNET_Artifacts_Score <- tryCatch({
        AutoQuant:::TimeSeriesDataPrepare(
          data = data,
          TargetName = ArgsList[['TargetColumnName']],
          DateName = ArgsList[['DateColumnName']],
          Lags = as.integer(ExperimentGrid[1L, Lags]),
          SeasonalLags = as.integer(ExperimentGrid[1L, SeasonalLags]),
          MovingAverages = 0,
          SeasonalMovingAverages = 0,
          TimeUnit = ArgsList[['TimeUnit']],
          FCPeriods = FCPeriods,
          HoldOutPeriods = 0,
          TSClean = TRUE,
          ModelFreq = TRUE,
          FinalBuild = TRUE)
      }, error = function(x) {
        if(DebugFC) print("AutoQuant:::TimeSeriesDataPrepare failed")
        NULL
      })

      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Data Prepare\n",
        "NNET_Artifacts_Score <- tryCatch({\n  ",
        "AutoQuant:::TimeSeriesDataPrepare(\n    ",
        "data = data,\n    ",
        "TargetName = ArgsList[['TargetColumnName']],\n    ",
        "DateName = ArgsList[['DateColumnName']],\n    ",
        "Lags = as.integer(ExperimentGrid[1L, Lags]),\n    ",
        "SeasonalLags = as.integer(ExperimentGrid[1L, SeasonalLags]),\n    ",
        "MovingAverages = 0,\n    ",
        "SeasonalMovingAverages = 0,\n    ",
        "TimeUnit = ArgsList[['TimeUnit']],\n    ",
        "FCPeriods = FCPeriods,\n    ",
        "HoldOutPeriods = 0,\n    ",
        "TSClean = TRUE,\n    ",
        "ModelFreq = TRUE,\n    ",
        "FinalBuild = TRUE)\n",
        "}, error = function(x) {\n  ",
        "print('AutoQuant:::TimeSeriesDataPrepare failed')\n  ",
        "NULL\n",
        "})\n"))
      }, error = function(x) CodeList)

      if(DebugFC)  print('here 10.53')

      if(length(NNET_Artifacts_Score) > 0L) {

        # Generate Final Arfima Forecast ----
        # repeat so long as there isn't a forecast generated or no more models left to use
        # upon generating a final forecast, a new model is built utilizing 100% of training data
        # Sometimes, this new model doesn't get built
        #   If it doesn't get built then the next best version will be attempted (based on ExperimentGrid data.table)
        #   the ExperimentGrid is sorted by paste('Blended_', ArgsList[['EvaluationMetric']])
        counter <- ExperimentGrid[, .N] + 1L
        repeat {
          counter <- counter - 1L
          if(counter == 0) break
          ForecastOutput <- tryCatch({
            AutoQuant:::FinalBuildNNET(
              ModelOutputGrid = ExperimentGrid,
              SavePath = NULL,
              TimeSeriesPrepareOutput = NNET_Artifacts_Score,
              FCPeriods = FCPeriods,
              NumberModelsScore = 1,
              MetricSelection = ArgsList[['EvaluationMetric']],
              DebugMode = DebugFC,
              ByDataType = FALSE)
          }, error = function(x) {
            if(DebugFC) print("AutoQuant:::FinalBuildNNET failed")
            if(DebugFC) print(paste0("counter = ", counter))
            # print(paste0("ModelOutputGrid = ", ExperimentGrid))
            if(DebugFC) print(paste0("SavePath = NULL"))
            # print(paste0("TimeSeriesPrepareOutput = ", NNET_Artifacts_Score))
            if(DebugFC) print(paste0("FCPeriods = ", FCPeriods))
            if(DebugFC) print(paste0("NumberModelsScore = 1"))
            if(DebugFC) print(paste0("MetricSelection = ", ArgsList[['EvaluationMetric']]))
            if(DebugFC) print(paste0("DebugMode = ", DebugFC))
            if(DebugFC) print(paste0("ByDataType = FALSE"))
            NULL
          })
          if(length(ForecastOutput) > 0L && !is.na(ForecastOutput[.N][["Forecast"]])) {
            break
          } else {
            if(length(ForecastOutput) == 0L) {
              if(DebugFC) print("length(ForecastOutput) == 0L")
            } else if(!is.na(ForecastOutput[.N][["Forecast"]])) {
              if(DebugFC) print("!is.na(ForecastOutput[.N][['Forecast']]) == FALSE")
            }
          }
        }

        CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
          "\n",
          "# Generate Final NNET Forecast\n",
          "counter <- ExperimentGrid[, .N] + 1L\n",
          "repeat {\n  ",
          "counter <- counter - 1L\n  ",
          "if(counter == 0) break\n  ",
          "ForecastOutput <- tryCatch({\n    ",
          "AutoQuant:::FinalBuildNNET(\n      ",
          "ModelOutputGrid = ExperimentGrid,\n      ",
          "SavePath = NULL,\n      ",
          "TimeSeriesPrepareOutput = NNET_Artifacts_Score,\n      ",
          "FCPeriods = FCPeriods,\n      ",
          "NumberModelsScore = 1,\n      ",
          "MetricSelection = ArgsList[['EvaluationMetric']],\n      ",
          "ByDataType = FALSE)\n    ",
          "}, error = function(x) {\n    ",
          "print('AutoQuant:::FinalBuildNNET failed')\n  ",
          "NULL\n  ",
          "})\n  ",
          "if(length(ForecastOutput) > 0L && !is.na(ForecastOutput[.N][['Forecast']])) {\n    ",
          "break\n  ",
          "} else {\n    ",
          "if(length(ForecastOutput) == 0L) {\n      ",
          "print('length(ForecastOutput) == 0L')\n    ",
          "} else if(!is.na(ForecastOutput[.N][['Forecast']])) {\n      ",
          "print('!is.na(ForecastOutput[.N][['Forecast']]) == FALSE')\n    ",
          "}\n  ", # closes else if
          "}\n", # closes else
          "}\n" # closes repeat
        ))
        }, error = function(x) CodeList)
      }
    }

    # Final Modifications: Convert names back to user friendly for Experiment Grid
    # Forecast output: Convert Target and Date to original names
    # Change Names of Columns in Experiment Grid
    if(length(ExperimentGrid) > 0L) {
      data.table::setnames(
        ExperimentGrid,
        c("Train_MSE", "Train_MAE", "Train_MAPE",
          "Validate_MSE", "Validate_MAE", "Validate_MAPE",
          "Blended_MSE", "Blended_MAE", "Blended_MAPE"
        ),
        c("MSE Train", "MAE Train", "MAPE Train",
          "MSE Validate", "MAE Validate", "MAPE Validate",
          "MSE Blended", "MAE Blended", "MAPE Blended"
        )
      )
    }

    # Rename columns code print
    CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
      "\n",
      "# Change Names of Columns in Experiment Grid\n",
      "if(length(ExperimentGrid) > 0L) {\n  ",
      "data.table::setnames(\n    ",
      "ExperimentGrid,\n      ",
      "c(\n        ",
      "'Train_MSE',\n        ",
      "'Train_MAE',\n        ",
      "'Train_MAPE',\n        ",
      "'Validate_MSE',\n        ",
      "'Validate_MAE',\n        ",
      "'Validate_MAPE',\n        ",
      "'Blended_MSE',\n        ",
      "'Blended_MAE',\n        ",
      "'Blended_MAPE',\n      ",
      "),\n      ",
      "c(\n        ",
      "'MSE Train',\n        ",
      "'MAE Train',\n        ",
      "'MAPE Train',\n        ",
      "'MSE Validate',\n        ",
      "'MAE Validate',\n        ",
      "'MAPE Validate',\n        ",
      "'MSE Blended',\n        ",
      "'MAE Blended',\n        ",
      "'MAPE Blended',\n      ",
      "), skip_absent = TRUE\n  ",
      ")\n",
      "}\n"))
    }, error = function(x) CodeList)

    # Update Output
    if(length(ForecastOutput) > 0L) {
      data.table::setnames(ForecastOutput, c("Target","Date"), c(ArgsList[['TargetColumnName']], ArgsList[['DateColumnName']]))
      data.table::set(ForecastOutput, j = c("ModelID", "ModelRank"), value = NULL)
      ForecastOutput[, ModelID := NULL]
      DataList[[paste0(ModelID, "_Forecast")]][['data']] <- ForecastOutput
      for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
      for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)

      # Code Collect
      CodeList <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = CodeList, x = paste0(
        "\n",
        "# Update Output\n",
        "if(length(ForecastOutput) > 0L) {\n  ",
        "data.table::setnames(ForecastOutput, c('Target','Date'), c(ArgsList[['TargetColumnName']], ArgsList[['DateColumnName']]))\n  ",
        "data.table::set(ForecastOutput, j = c('ModelID', 'ModelRank'), value = NULL)\n  ",
        "ForecastOutput[, ModelID := NULL]\n  ",
        "DataList[[paste0(ModelID, '_Forecast')]][['data']] <- ForecastOutput\n",
        "}\n\n",
        "# FORECASTING END ::::::::::: \n"))
      }, error = function(x) CodeList)
    }
  }

  # ----

  # **************************************** ----
  # Return                                   ----
  # **************************************** ----
  returnList <- list()
  returnList[["RunMode"]] <- ArgsList[['RunMode']]
  returnList[["DataList"]] <- DataList
  returnList[["ArgsList"]] <- ArgsList
  returnList[["CodeList"]] <- CodeList
  returnList[["ForecastingCode"]] <- ForecastingCode
  return(returnList)
}

# ----

# ----
