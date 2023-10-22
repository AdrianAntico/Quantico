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
