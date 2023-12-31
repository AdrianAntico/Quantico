% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/InferenceStats.R
\name{Correlation.Analysis}
\alias{Correlation.Analysis}
\title{Correlation.Analysis}
\usage{
Correlation.Analysis(
  dt = NULL,
  CorrVars = NULL,
  DateVar = NULL,
  SampleSize = 10000,
  EchartsTheme = "macarons",
  TextColor = "black",
  PlotHeight = "600px",
  PlotWidth = "300px",
  P_Adjust = "holm",
  Bayesian = FALSE,
  Bayesian_Prior = "medium",
  MultiLevel = FALSE,
  Include_Factors = FALSE,
  Partial = FALSE,
  Partial_Bayesian = FALSE
)
}
\arguments{
\item{dt}{Numeric vector of values to test. Must have positive standard deviation and must be of length greater than or equal to 8}

\item{CorrVars}{character}

\item{DateVar}{character}

\item{SampleSize}{sub sampling of data}

\item{EchartsTheme}{"macarons"}

\item{TextColor}{"black"}

\item{PlotHeight}{"300px"}

\item{PlotWidth}{"600px"}

\item{P_Adjust}{Frequentist corrections, which include: default "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "somers" or "none"}

\item{Bayesian}{logical}

\item{Bayesian_Prior}{Choose from default "medium", "medium.narrow", "medium", "wide", and "ultrawide"}

\item{MultiLevel}{logical. TRUE using random effects, FALSE uses fixed effects}

\item{Include_Factors}{logical}

\item{Partial}{logical}

\item{Partial_Bayesian}{logical}
}
\description{
Correlation.Analysis
}
\seealso{
Other Inference: 
\code{\link{Agostino.Test}()},
\code{\link{Anderson.Darling.Test}()},
\code{\link{ChiSq.Test}()},
\code{\link{ChiSquareTest_Modal_Fun}()},
\code{\link{Correlation_Modal_Fun}()},
\code{\link{Cramer.Von.Mises.Test}()},
\code{\link{F.Test}()},
\code{\link{FTest_Modal_Fun}()},
\code{\link{Inference_1STTest}()},
\code{\link{Inference_2STTest}()},
\code{\link{Inference_ChiSq}()},
\code{\link{Inference_Correlation}()},
\code{\link{Inference_FTest}()},
\code{\link{Inference_Normality}()},
\code{\link{Jarque.Bera.Test}()},
\code{\link{Kolmogorov.Smirnov.Test}()},
\code{\link{Normality.Analysis}()},
\code{\link{Normality_Modal_Fun}()},
\code{\link{One.Sample.TTest}()},
\code{\link{OneSampleTTest_Modal_Fun}()},
\code{\link{Shapiro.Test}()},
\code{\link{Two.Sample.TTest}()},
\code{\link{TwoSampleTTest_Modal_Fun}()}
}
\concept{Inference}
