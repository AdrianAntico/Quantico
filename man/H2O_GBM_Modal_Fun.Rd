% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ShinyUI.R
\name{H2O_GBM_Modal_Fun}
\alias{H2O_GBM_Modal_Fun}
\title{H2O_GBM_Modal_Fun}
\usage{
H2O_GBM_Modal_Fun(
  id,
  AppWidth = 12L,
  H2O_GBM_eval_metric_Selected = NULL,
  H2O_GBM_TargetColumnName_Selected = NULL,
  H2O_GBM_FeatureColNames_Selected = NULL,
  H2O_GBM_TransformNumericColumns_Selected = NULL,
  H2O_GBM_WeightsColumn_Selected = NULL,
  H2O_GBM_Runs_Choices = NULL,
  H2O_GBM_TargetType_Choices = NULL,
  H2O_GBM_data_Choices = NULL,
  H2O_GBM_ValidationData_Choices = NULL,
  H2O_GBM_TestData_Choices = NULL,
  H2O_GBM_EncodeMethod_Choices = NULL,
  H2O_GBM_Methods_Choices = NULL,
  H2O_GBM_NThreads_Choices = NULL,
  H2O_GBM_MaxMem_Choices = NULL,
  H2O_GBM_TrainOnFull_Choices = NULL,
  H2O_GBM_Trees_Choices = NULL,
  H2O_GBM_MaxDepth_Choices = NULL,
  H2O_GBM_SampleRate_Choices = NULL,
  H2O_GBM_ColSampleRatePerTree_Choices = NULL,
  H2O_GBM_LearnRate_Choices = NULL,
  H2O_GBM_LearnRateAnnealing_Choices = NULL,
  H2O_GBM_MinRows_Choices = NULL,
  H2O_GBM_NBinsCats_Choices = NULL,
  H2O_GBM_NBinsTopLevel_Choices = NULL,
  H2O_GBM_HistogramType_Choices = NULL,
  H2O_GBM_CategoricalEncoding_Choices = NULL,
  H2O_GBM_StoppingRounds_Choices = NULL,
  H2O_GBM_GridTune_Choices = NULL,
  H2O_GBM_MaxModelsInGrid_Choices = NULL,
  H2O_GBM_ModelID_Selected = NULL,
  H2O_GBM_Runs_Selected = NULL,
  H2O_GBM_TargetType_Selected = NULL,
  H2O_GBM_data_Selected = NULL,
  H2O_GBM_ValidationData_Selected = NULL,
  H2O_GBM_TestData_Selected = NULL,
  H2O_GBM_EncodeMethod_Selected = NULL,
  H2O_GBM_Methods_Selected = NULL,
  H2O_GBM_NThreads_Selected = NULL,
  H2O_GBM_MaxMem_Selected = NULL,
  H2O_GBM_TrainOnFull_Selected = NULL,
  H2O_GBM_Trees_Selected = NULL,
  H2O_GBM_MaxDepth_Selected = NULL,
  H2O_GBM_SampleRate_Selected = NULL,
  H2O_GBM_LearnRate_Selected = NULL,
  H2O_GBM_LearnRateAnnealing_Selected = NULL,
  H2O_GBM_MTries_Selected = NULL,
  H2O_GBM_ColSampleRatePerTree_Selected = NULL,
  H2O_GBM_MinRows_Selected = NULL,
  H2O_GBM_NBinsCats_Selected = NULL,
  H2O_GBM_NBinsTopLevel_Selected = NULL,
  H2O_GBM_HistogramType_Selected = NULL,
  H2O_GBM_CategoricalEncoding_Selected = NULL,
  H2O_GBM_StoppingRounds_Selected = NULL,
  H2O_GBM_GridTune_Selected = NULL,
  H2O_GBM_MaxModelsInGrid_Selected = NULL,
  H2O_GBM_Distribution_Selected = NULL,
  H2O_GBM_ClassWeights0_Selected = NULL,
  H2O_GBM_ClassWeights1_Selected = NULL
)
}
\arguments{
\item{id}{= 'H2O_GBM_ID'}

\item{AppWidth}{= 12L}
}
\description{
H2O GBM Parameters Modal with OK button to close
}
\seealso{
Other ML: 
\code{\link{ARFIMA_Modal_Fun}()},
\code{\link{CatBoostCARMA_Modal_Fun}()},
\code{\link{CatBoost_Modal_Fun}()},
\code{\link{CausalMediation_Modal_Fun}()},
\code{\link{ETS_Modal_Fun}()},
\code{\link{H2O_DRF_Modal_Fun}()},
\code{\link{H2O_GLM_Modal_Fun}()},
\code{\link{H2O_HGLM_Modal_Fun}()},
\code{\link{LightGBMCARMA_Modal_Fun}()},
\code{\link{LightGBM_Modal_Fun}()},
\code{\link{NNET_Modal_Fun}()},
\code{\link{Sarima_Modal_Fun}()},
\code{\link{Shiny.ML.CatBoost.EvalMetricOptions}()},
\code{\link{Shiny.ML.CatBoost.GridEvalMetricsOptions}()},
\code{\link{Shiny.ML.CatBoost.LossFunctionOptions}()},
\code{\link{Shiny.ML.CatBoost_Params}()},
\code{\link{Shiny.ML.Evaluation.Table}()},
\code{\link{Shiny.ML.H2O.DistributionOptions}()},
\code{\link{Shiny.ML.H2O.GLM.DistributionOptions}()},
\code{\link{Shiny.ML.H2O.LinkOptions}()},
\code{\link{Shiny.ML.H2O_DRF.EvalMetricOptions}()},
\code{\link{Shiny.ML.H2O_DRF_Params}()},
\code{\link{Shiny.ML.H2O_GBM.EvalMetricOptions}()},
\code{\link{Shiny.ML.H2O_GBM_Params}()},
\code{\link{Shiny.ML.H2O_GLM.EvalMetricOptions}()},
\code{\link{Shiny.ML.H2O_GLM_Params}()},
\code{\link{Shiny.ML.H2O_HGLM_Params}()},
\code{\link{Shiny.ML.LightGBM.LossFunctionOptions}()},
\code{\link{Shiny.ML.LightGBM_Params}()},
\code{\link{Shiny.ML.ModelDataObjects}()},
\code{\link{Shiny.ML.Scoring}()},
\code{\link{Shiny.ML.Trainer}()},
\code{\link{Shiny.ML.XGBoost.EvalMetricOptions}()},
\code{\link{Shiny.ML.XGBoost.LossFunctionOptions}()},
\code{\link{Shiny.ML.XGBoost_Params}()},
\code{\link{TBATS_Modal_Fun}()},
\code{\link{XGBoostCARMA_Modal_Fun}()},
\code{\link{XGBoost_Modal_Fun}()}
}
\author{
Adrian Antico
}
\concept{ML}
