% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ShinyDW.R
\name{Shiny.DW.JoinData}
\alias{Shiny.DW.JoinData}
\title{Shiny.DW.JoinData}
\usage{
Shiny.DW.JoinData(
  input,
  output,
  session,
  DataList,
  CodeList,
  TabCount = 5L,
  CacheDir = CacheDir,
  CacheName = CacheName,
  Debug = Debug
)
}
\arguments{
\item{input}{shiny input}

\item{output}{shiny output}

\item{session}{shiny session}

\item{DataList}{DataList contains the data sets in session}

\item{CodeList}{From app}

\item{CacheDir}{From app}

\item{CacheName}{'data' from app. Function will strip any '.csv' from CacheName if it exists and then place one there manually.}

\item{Debug}{Debug from app}
}
\description{
Join two data.table's
}
\seealso{
Other DW: 
\code{\link{Columns_Modal_Fun}()},
\code{\link{DW_Columns}()},
\code{\link{DW_GrowData}()},
\code{\link{DW_Misc}()},
\code{\link{DW_Pivot}()},
\code{\link{DW_ShrinkData}()},
\code{\link{DataSets_Modal_Fun}()},
\code{\link{Grow_Modal_Fun}()},
\code{\link{Misc_Modal_Fun}()},
\code{\link{Pivot_Modal_Fun}()},
\code{\link{Shiny.DW.AggregateData}()},
\code{\link{Shiny.DW.CastData}()},
\code{\link{Shiny.DW.ConcatenateColumns}()},
\code{\link{Shiny.DW.DeleteColumns}()},
\code{\link{Shiny.DW.MeltData}()},
\code{\link{Shiny.DW.MetaProgramming}()},
\code{\link{Shiny.DW.RemoveData}()},
\code{\link{Shiny.DW.RenameColumns}()},
\code{\link{Shiny.DW.SampleData}()},
\code{\link{Shiny.DW.SortData}()},
\code{\link{Shiny.DW.SubsetData}()},
\code{\link{Shiny.DW.TimeSeriesFill}()},
\code{\link{Shiny.DW.TimeSeriesRoll}()},
\code{\link{Shiny.DW.TimeTrendColumn}()},
\code{\link{Shiny.DW.TypeCast}()},
\code{\link{Shiny.DW.UnionData}()},
\code{\link{Shrink_Modal_Fun}()}
}
\author{
Adrian Antico
}
\concept{DW}
