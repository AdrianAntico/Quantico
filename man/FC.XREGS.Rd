% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ShinyFC.R
\name{FC.XREGS}
\alias{FC.XREGS}
\title{FC.XREGS}
\usage{
FC.XREGS(
  dt,
  tv,
  dv,
  fcp,
  tu,
  gv = NULL,
  xrs = NULL,
  em = NULL,
  wd = NULL,
  db = FALSE
)
}
\arguments{
\item{dt}{data a data.table}

\item{tv}{TargetColumnName}

\item{dv}{DateColumnName}

\item{fcp}{PC_Periods}

\item{tu}{TimeUnit}

\item{gv}{GroupVariables}

\item{xrs}{XREGS a data.table}

\item{em}{EncodingMethod}

\item{wd}{working directory}

\item{db}{DebugMode}
}
\value{
a list of columns names by data type
}
\description{
Management of xregs either generated, user supplied, or modified
}
\seealso{
Other FC: 
\code{\link{BasicLoop.SingleTest}()},
\code{\link{FC.Backtest.CalendarGroups}()},
\code{\link{FC.Backtest.DateGroups}()},
\code{\link{FC.DateCast}()},
\code{\link{FC.FCPeriods}()},
\code{\link{FC.TimeUnit}()},
\code{\link{Shiny.EDA.ReportOutput}()},
\code{\link{Shiny.FC.CARMA}()},
\code{\link{Shiny.FC.Panel.Backest.FeatureEval}()},
\code{\link{Shiny.FC.Panel.Backest.RollingEval}()},
\code{\link{Shiny.FC.Panel.Backtest}()},
\code{\link{Shiny.FC.Panel.Forecast}()},
\code{\link{Shiny.FC.Panel.Metric.Rollup}()},
\code{\link{Shiny.FC.Panel.Metrics.Agg}()},
\code{\link{Shiny.FC.Panel.Metrics.Raw}()},
\code{\link{Shiny.FC.Panel.ReportOutput}()},
\code{\link{Shiny.FC.Panel.Retrain}()},
\code{\link{Shiny.FC.Panel.Train}()},
\code{\link{Shiny.FC.SS.ReportOutput}()},
\code{\link{Shiny.FC.SS}()},
\code{\link{Shiny.ML.ReportOutput}()}
}
\author{
Adrian Antico
}
\concept{FC}
