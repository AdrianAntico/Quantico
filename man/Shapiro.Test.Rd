% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/InferenceStats.R
\name{Shapiro.Test}
\alias{Shapiro.Test}
\title{Shapiro.Test}
\usage{
Shapiro.Test(Vals, SampleSize = NULL, Samples = 1)
}
\arguments{
\item{Vals}{a vector of values to test}

\item{SampleSize}{sub sampling of data}

\item{Samples}{number of iterations to run}
}
\description{
Shapiro.Test
}
\examples{
\dontrun{
output <- Shapiro.Test(Vals = runif(1000), SampleSize = 100, Samples = 500)
}

}
\seealso{
Other Inference: 
\code{\link{Agostino.Test}()},
\code{\link{Anderson.Darling.Test}()},
\code{\link{ChiSq.Test}()},
\code{\link{ChiSquareTest_Modal_Fun}()},
\code{\link{Correlation.Analysis}()},
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
\code{\link{Two.Sample.TTest}()},
\code{\link{TwoSampleTTest_Modal_Fun}()}
}
\concept{Inference}
