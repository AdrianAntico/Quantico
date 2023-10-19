# AutoPlots is a package for quickly creating high quality visualizations under a common and easy api.
# Copyright (C) <year>  <name of author>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program. If not, see <https://www.gnu.org/licenses/>.

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# :: Helper Functions ::                                                      ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @noRd
SummaryFunction <- function(AggMethod) {
  if(AggMethod == "count") {
    aggFunc <- function(x) .N
  } else if(AggMethod == "mean") {
    aggFunc <- function(x) mean(x, na.rm = TRUE)
  } else if(AggMethod == "log(mean(x))") {
    aggFunc <- function(x) log(mean(x, na.rm = TRUE))
  } else if(AggMethod == "mean(abs(x))") {
    aggFunc <- function(x) mean(abs(x), na.rm = TRUE)
  } else if(AggMethod == "sum") {
    aggFunc <- function(x) sum(x, na.rm = TRUE)
  } else if(AggMethod == "log(sum(x))") {
    aggFunc <- function(x) log(sum(x, na.rm = TRUE))
  } else if(AggMethod == "sum(abs(x))") {
    aggFunc <- function(x) sum(abs(x), na.rm = TRUE)
  } else if(AggMethod == "median") {
    aggFunc <- function(x) median(x, na.rm = TRUE)
  } else if(AggMethod == "log(median(x))") {
    aggFunc <- function(x) log(median(x, na.rm = TRUE))
  } else if(AggMethod == "median(abs(x))") {
    aggFunc <- function(x) median(abs(x), na.rm = TRUE)
  } else if(AggMethod == "sd") {
    aggFunc <- function(x) sd(x, na.rm = TRUE)
  } else if(AggMethod == "log(sd(x))") {
    aggFunc <- function(x) log(sd(x, na.rm = TRUE))
  } else if(AggMethod == "sd(abs(x))") {
    aggFunc <- function(x) sd(abs(x), na.rm = TRUE)
  } else if(AggMethod == "skewness") {
    aggFunc <- function(x) e1071::skewness(x, na.rm = TRUE)
  } else if(AggMethod == "skewness(abs(x))") {
    aggFunc <- function(x) e1071::skewness(abs(x), na.rm = TRUE)
  } else if(AggMethod == "kurtosis") {
    aggFunc <- function(x) e1071::kurtosis(x, na.rm = TRUE)
  } else if(AggMethod == "kurtosis(abs(x))") {
    aggFunc <- function(x) e1071::kurtosis(abs(x), na.rm = TRUE)
  } else if(AggMethod == "CoeffVar") {
    aggFunc <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
  } else if(AggMethod == "CoeffVar(abs(x))") {
    aggFunc <- function(x) sd(abs(x), na.rm = TRUE) / mean(abs(x), na.rm = TRUE)
  }
  return(aggFunc)
}

#' @noRd
ColTypes <- function(data) {
  CT <- c()
  for(Col in names(data)) CT <- c(CT, class(data[[Col]])[1L])
  CT
}

#' @noRd
bold_ <- function(x) paste0('<b>',x,'</b>')

#' @noRd
font_ <- function(family = "Segoe UI Symbol", size = 12, color = 'white') list(family = family, size = size, color = color)

#' @noRd
ColNameFilter <- function(data, Types = 'all') {
  if(Types == 'all') return(names(data))
  nam <- c()
  for(t in Types) {
    if(tolower(t) == 'numeric') {
      nam <- NumericColNames(data)
    } else if(tolower(t) == 'character') {
      nam <- CharacterColNames(data)
    } else if(tolower(t) == 'factor') {
      nam <- FactorColNames(data)
    } else if(tolower(t) == 'logical') {
      nam <- LogicalColNames(data)
    } else if(tolower(t) %chin% c("date","idate","idatetime","posixct","posix")) {
      nam <- DateColNames(data)
    }
  }
  return(nam)
}

#' @noRd
NumericColNames <- function(data) {
  x <- as.list(names(data)[which(sapply(data, is.numeric))])
  if(!identical(x, character(0))) return(x) else return(NULL)
}

#' @noRd
CharacterColNames <- function(data) {
  x <- as.list(names(data)[which(sapply(data, is.character))])
  if(!identical(x, character(0))) return(x) else return(NULL)
}

#' @noRd
FactorColNames <- function(data) {
  x <- as.list(names(data)[which(sapply(data, is.factor))])
  if(!identical(x, character(0))) return(x) else return(NULL)
}

#' @noRd
LogicalColNames <- function(data) {
  x <- as.list(names(data)[which(sapply(data, is.logical))])
  if(!identical(x, character(0))) return(x) else return(NULL)
}

#' @noRd
DateColNames <- function(data) {
  x <- list()
  counter <- 0L
  for(i in names(data)) {
    if(class(data[[i]])[1L] %in% c("IDate","Date","date","POSIXct","POSIX")) {
      counter <- counter + 1L
      x[[counter]] <- i
    }
  }
  if(length(x) > 0L) return(x) else return(NULL)
}

#' @title FakeDataGenerator
#'
#' @description Create fake data for examples
#'
#' @author Adrian Antico
#' @family Data Wrangling
#'
#' @param Correlation Set the correlation value for simulated data
#' @param N Number of records
#' @param ID Number of IDcols to include
#' @param ZIP Zero Inflation Model target variable creation. Select from 0 to 5 to create that number of distinctly distributed data, stratifed from small to large
#' @param FactorCount Number of factor type columns to create
#' @param AddDate Set to TRUE to include a date column
#' @param AddComment Set to TRUE to add a comment column
#' @param TimeSeries For testing AutoBanditSarima
#' @param TimeSeriesTimeAgg Choose from "1min", "5min", "10min", "15min", "30min", "hour", "day", "week", "month", "quarter", "year",
#' @param ChainLadderData Set to TRUE to return Chain Ladder Data for using AutoMLChainLadderTrainer
#' @param Classification Set to TRUE to build classification data
#' @param MultiClass Set to TRUE to build MultiClass data
#' @export
FakeDataGenerator <- function(Correlation = 0.70,
                              N = 1000L,
                              ID = 5L,
                              FactorCount = 2L,
                              AddDate = TRUE,
                              AddComment = FALSE,
                              AddWeightsColumn = FALSE,
                              ZIP = 5L,
                              TimeSeries = FALSE,
                              TimeSeriesTimeAgg = "day",
                              ChainLadderData = FALSE,
                              Classification = FALSE,
                              MultiClass = FALSE) {

  # Error checking
  if(sum(TimeSeries, Classification, MultiClass) > 1) stop("Only one of the following can be set to TRUE: TimeSeries, Classifcation, and MultiClass")

  # TimeSeries
  if(TimeSeries) {

    # Error msg
    if(is.null(TimeSeriesTimeAgg)) stop("TimeSeriesAgg cannot be NULL when using TimeSeries = TRUE")

    # Pull in data
    data <- data.table::as.data.table(as.numeric(fpp::cafe))

    # Change names to common names for other calls in this function
    data.table::setnames(data, "V1", "Weekly_Sales")

    # Pick a starting date
    data.table::set(data, j = "Date", value = "1982-01-01")
    data.table::setcolorder(data, c(2L, 1L))
    data[, Date := as.Date(Date)]

    # "1min"
    if(tolower(TimeSeriesTimeAgg) %chin% c("1min","1mins","minutes","min","mins","01min","01mins")) {
      data[, xx := 1:.N][, Date := Date + lubridate::minutes(1 * 1:.N)][, xx := NULL]
    }

    # "5min"
    if(tolower(TimeSeriesTimeAgg) %chin% c("5min","5mins","5minutes","min5","mins5","05min")) {
      data[, Date := Date + lubridate::minutes(5 * 1:.N)]
    }

    # "10min"
    if(tolower(TimeSeriesTimeAgg) %chin% c("10min","10mins","10minutes","min10","mins10")) {
      data[, Date := Date + lubridate::minutes(10 * 1:.N)]
    }

    # "15min"
    if(tolower(TimeSeriesTimeAgg) %chin% c("15min","15mins","15minutes","min15","mins15")) {
      data[, Date := Date + lubridate::minutes(15 * 1:.N)]
    }

    # "30min"
    if(tolower(TimeSeriesTimeAgg) %chin% c("30min","30mins","30minutes","min30","mins30")) {
      data[, Date := Date + lubridate::minutes(30 * 1:.N)]
    }

    # "hour"
    if(tolower(TimeSeriesTimeAgg) %chin% c("hour","hours","hr","hrs","our","ours")) {
      data[, Date := Date + lubridate::hours(1:.N)]
    }

    # "day"
    if(tolower(TimeSeriesTimeAgg) %chin% c("day","days","daily","dy","das")) {
      data[, Date := Date + lubridate::days(1:.N)]
    }

    # "week"
    if(tolower(TimeSeriesTimeAgg) %chin% c("week","weeks","wk","wks")) {
      data[, Date := Date + lubridate::weeks(1:.N)]
    }

    # "month"
    if(tolower(TimeSeriesTimeAgg) %chin% c("month","months","mth","mths")) {
      data[, Date := Date %m+% months(1:.N)]
    }

    # "quarter"
    if(tolower(TimeSeriesTimeAgg) %chin% c("quarter","quarters"," qtr","qtrs","qarter")) {
      data[, Date := Date %m+% months(3 * 1:.N)]
    }

    # "year"
    if(tolower(TimeSeriesTimeAgg) %chin% c("year","years","yr","yrs","yts")) {
      data[, Date := Date + lubridate::years(1:.N)]
    }

    # Return data
    return(data)
  }

  # Create ChainLadderData
  if(ChainLadderData) {

    # Overwrite N
    N <- 1000

    # Define constants
    MaxCohortDays <- 15L

    # Start date
    CalendarDateData <- data.table::data.table(CalendarDateColumn = rep(as.Date("2018-01-01"), N), key = "CalendarDateColumn")

    # Increment date column so it is sequential
    CalendarDateData[, temp := seq_len(N)]
    CalendarDateData[, CalendarDateColumn := CalendarDateColumn + lubridate::days(temp) - 1L]
    CohortDate_temp <- data.table::copy(CalendarDateData)
    data.table::setnames(x = CohortDate_temp, old = c("CalendarDateColumn"), new = c("CohortDate_temp"))

    # Cross join the two data sets
    ChainLadderData <- data.table::setkeyv(data.table::CJ(
      CalendarDateColumn = CalendarDateData$CalendarDateColumn,
      CohortDateColumn = CohortDate_temp$CohortDate_temp,
      sorted = TRUE,
      unique = TRUE),
      cols = c("CalendarDateColumn", "CohortDateColumn"))

    # Remove starter data sets and N
    rm(CalendarDateData, CohortDate_temp, N)

    # Remove impossible dates
    ChainLadderData <- ChainLadderData[CohortDateColumn >= CalendarDateColumn]

    # Add CohortPeriods
    ChainLadderData[, CohortDays := as.numeric(difftime(CohortDateColumn, CalendarDateColumn, tz = "MST", units = "day"))]

    # Limit the number of CohortTime
    ChainLadderData <- ChainLadderData[CohortDays < MaxCohortDays]

    # Add measure columns placeholder values
    ChainLadderData[, ":=" (Leads = 0, Appointments = 0, Rates = 0)]

    # Sort decending both date columns
    data.table::setorderv(x = ChainLadderData, cols = c("CalendarDateColumn","CohortDateColumn"), order = c(-1L, 1L))

    # Add columns for BaselineMeasure and ConversionMeasure
    UniqueCalendarDates <- unique(ChainLadderData$CalendarDateColumn)
    NN <- length(UniqueCalendarDates)
    LoopSeq <- c(1:15)
    LoopSeq <- cumsum(LoopSeq)
    LoopSeq <- c(1, LoopSeq)
    LoopSeq <- c(LoopSeq, seq(135, 15*993, 15))
    for(cal in seq(NN)) {

      # Generate first element of decay data
      DecayCurveData <- dgeom(x = 0, prob = runif(n = 1L, min = 0.45, max = 0.55), log = FALSE)

      # Fill in remain elements in vector
      if(cal > 1L) {
        zz <- seq_len(min(15L, cal))
        for(i in zz[1:min(cal-1L,15)]) {
          DecayCurveData <- c(DecayCurveData, c(dgeom(x = i, prob = runif(n = 1L, min = 0.45, max = 0.55), log = FALSE)))
        }
      }

      # Fill ChainLadderData
      data.table::set(ChainLadderData, i = (LoopSeq[cal]+1L):LoopSeq[cal + 1L], j = "Rates", value = DecayCurveData[seq_len(min(15L, cal))])
    }

    # Fill in Leads and Conversions----
    x <- unique(ChainLadderData[, .SD, .SDcols = c("CalendarDateColumn","Leads")])
    x[, Leads := runif(n = x[, .N], min = 100, max = 500)]
    ChainLadderData <- merge(ChainLadderData[, .SD, .SDcols = c("CalendarDateColumn","CohortDateColumn","CohortDays","Appointments","Rates")], x, by = "CalendarDateColumn", all = FALSE)
    ChainLadderData[, Appointments := Leads * Rates]
    ChainLadderData[, Sales := Appointments * Rates * (runif(.N))]
    ChainLadderData[, Rates := NULL]
    data.table::setcolorder(ChainLadderData, c(1,2,3,5,4))
    return(ChainLadderData)
  }

  # Modify----
  if(MultiClass && FactorCount == 0L) {
    FactorCount <- 1L
    temp <- 1L
  }

  # Create data----
  Correl <- Correlation
  data <- data.table::data.table(Adrian = runif(N))
  data[, x1 := qnorm(Adrian)]
  data[, x2 := runif(N)]
  data[, Independent_Variable1 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
  data[, Independent_Variable2 := log(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
  data[, Independent_Variable3 := exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
  data[, Independent_Variable4 := exp(exp(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2))))]
  data[, Independent_Variable5 := sqrt(pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))]
  data[, Independent_Variable6 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.10]
  data[, Independent_Variable7 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.25]
  data[, Independent_Variable8 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^0.75]
  data[, Independent_Variable9 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^2]
  data[, Independent_Variable10 := (pnorm(Correl * x1 + sqrt(1-Correl^2) * qnorm(x2)))^4]
  if(ID > 0L) for(i in seq_len(ID)) data[, paste0("IDcol_", i) := runif(N)]
  data[, ":=" (x2 = NULL)]

  # FactorCount----
  for(i in seq_len(FactorCount)) {
    RandomValues <- sort(c(runif(n = 4L, min = 0.01, max = 0.99)))
    RandomLetters <- sort(c(sample(x = LETTERS, size = 5L, replace = FALSE)))
    data[, paste0("Factor_", i) := as.factor(
      data.table::fifelse(Independent_Variable1 < RandomValues[1L], RandomLetters[1L],
                          data.table::fifelse(Independent_Variable1 < RandomValues[2L], RandomLetters[2L],
                                              data.table::fifelse(Independent_Variable1 < RandomValues[3L],  RandomLetters[3L],
                                                                  data.table::fifelse(Independent_Variable1 < RandomValues[4L],  RandomLetters[4L], RandomLetters[5L])))))]
  }

  # Add date----
  if(AddDate) {
    if(FactorCount == 0) {
      data <- data[, DateTime := as.Date(Sys.time())]
      data[, temp := seq_len(.N)][, DateTime := DateTime - temp][, temp := NULL]
      data <- data[order(DateTime)]
    } else {
      data <- data[, DateTime := as.Date(Sys.time())]
      CatFeatures <- sort(c(as.numeric(which(sapply(data, is.factor))), as.numeric(which(sapply(data, is.character)))))
      data[, temp := seq_len(.N), by = c(names(data)[c(CatFeatures)])][, DateTime := DateTime - temp][, temp := NULL]
      data.table::setorderv(x = data, cols = c("DateTime", c(names(data)[c(CatFeatures)])), order = rep(1, length(c(names(data)[c(CatFeatures)]))+1))
    }
  }

  # Zero Inflation Setup
  if(!Classification && !MultiClass) {
    if(ZIP == 1L) {
      data[, Adrian := data.table::fifelse(Adrian < 0.5, 0, Independent_Variable8)][, Independent_Variable8 := NULL]
    } else if(ZIP == 2L) {
      data[, Adrian := data.table::fifelse(Adrian < 0.33, 0, data.table::fifelse(Adrian < 0.66, log(Adrian * 10), log(Adrian*20)))]
    } else if(ZIP == 3L) {
      data[, Adrian := data.table::fifelse(Adrian < 0.25, 0, data.table::fifelse(Adrian < 0.50, log(Adrian * 10), data.table::fifelse(Adrian < 0.75, log(Adrian * 50), log(Adrian * 150))))]
    } else if(ZIP == 4L) {
      data[, Adrian := data.table::fifelse(Adrian < 0.20, 0, data.table::fifelse(Adrian < 0.40, log(Adrian * 10), data.table::fifelse(Adrian < 0.60, log(Adrian * 50), data.table::fifelse(Adrian < 0.80, log(Adrian * 150), log(Adrian * 250)))))]
    } else if(ZIP == 5L) {
      data[, Adrian := data.table::fifelse(Adrian < 1/6, 0, data.table::fifelse(Adrian < 2/6, log(Adrian * 10), data.table::fifelse(Adrian < 3/6, log(Adrian * 50), data.table::fifelse(Adrian < 4/6, log(Adrian * 250), data.table::fifelse(Adrian < 5/6, log(Adrian * 500), log(Adrian * 1000))))))]
    }
  }

  # Classification
  if(Classification) data[, Adrian := data.table::fifelse(jitter(x = Adrian, factor = 100) > 0.63, 1, 0)]

  # Remove----
  data[, ":=" (x1 = NULL)]

  # MultiClass
  if(MultiClass) {
    data[, Adrian := NULL]
    data.table::setnames(data, "Factor_1", "Adrian")
  }

  # Comment data
  if(AddComment) {
    a <- c('Hello', 'Hi', 'Howdy')
    b <- c('really like', 'absolutely adore', 'sucks ass')
    c <- c('noload', 'download', 'upload')
    N1 <- 1/length(a)
    N2 <- 1/length(b)
    N3 <- 1/length(c)
    N11 <- 1/N1
    N22 <- 1/N2
    N33 <- 1/N3
    RandomText <- function(N1,N11,N2,N22,N3,N33,a,b,c) {
      paste(sample(x = a, size = 1, replace = TRUE, prob = rep(N1, N11)),
            sample(x = b, size = 1, replace = TRUE, prob = rep(N2, N22)),
            sample(x = c, size = 1, replace = TRUE, prob = rep(N3, N33)))
    }
    data[, Comment := "a"]
    for(i in seq_len(data[, .N])) {
      data.table::set(data, i = i, j = "Comment", value = RandomText(N1,N11,N2,N22,N3,N33,a,b,c))
    }
  }

  # Add weights column
  if(AddWeightsColumn) {
    data[, Weights := runif(.N)]
  }

  # Return data
  return(data)
}

#' @title Standardize
#'
#' @description Generate standardized values for multiple variables, by groups if provided, and with a selected granularity
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data Source data.table
#' @param ColNames Character vector of column names
#' @param GroupVars Character vector of column names to have percent ranks by the group levels
#' @param Center TRUE
#' @param Scale TRUE
#' @param ScoreTable FALSE. Set to TRUE to return a data.table that can be used to apply or backtransform via StandardizeScoring
#'
#' @examples
#' \dontrun{
#' data <- data.table::fread(file.choose())
#' x <- Standardize(data = data, ColNames = c('Weekly_Sales', 'XREG3'), GroupVars = c('Region','Store','Dept'), Center = TRUE, Scale = TRUE, ScoreTable = TRUE)
#' }
#'
#' @noRd
Standardize <- function(data, ColNames, GroupVars = NULL, Center = TRUE, Scale = TRUE, ScoreTable = FALSE) {

  # Standardize
  if(length(GroupVars) == 0L) {
    data[, paste0(ColNames, '_Standardize') := lapply(.SD, FUN = function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)), .SDcols = c(ColNames)]
  } else {
    data[, paste0(ColNames, '_Standardize') := lapply(.SD, FUN = function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)), .SDcols = c(ColNames), by = c(eval(GroupVars))]
  }

  # ScoreTable creation
  if(ScoreTable) {
    x <- data[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(ColNames), by = c(GroupVars)]
    data.table::setnames(x = x, old = ColNames, new = paste0(ColNames, "_mean"))
    y <- data[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(ColNames), by = c(GroupVars)]
    data.table::setnames(x = y, old = ColNames, new = paste0(ColNames, "_sd"))
    xy <- cbind(x,y[, (GroupVars) := NULL])
  }

  # Return
  if(!ScoreTable) {
    return(data)
  } else {
    return(list(
      data = data,
      ScoreTable = xy
    ))
  }
}

#' @title StandardizeScoring
#'
#' @description Generate standardized values for multiple variables, by groups if provided, and with a selected granularity
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data Source data.table
#' @param Apply 'apply' or 'backtransform'
#' @param ColNames Character vector of column names
#' @param GroupVars Character vector of column names to have percent ranks by the group levels
#' @param Center TRUE
#' @param Scale TRUE
#'
#' @examples
#' \dontrun{
#' x <- Standardize(data = data, ColNames = c('Weekly_Sales', 'XREG1'), GroupVars = c('Region','Store','Dept'), Center = TRUE, Scale = TRUE)
#' }
#'
#' @noRd
StandardizeScoring <- function(data, ScoreTable, Apply = 'apply', GroupVars = NULL) {

  # Facts
  nam <- names(ScoreTable)[which(!names(ScoreTable) %in% GroupVars)]

  # Apply will apply standardization to new data
  # Backtransform will undo standardization
  if(Apply == 'apply') {
    data.table::setkeyv(x = data, cols = GroupVars)
    data.table::setkeyv(x = ScoreTable, cols = GroupVars)
    data[ScoreTable, paste0(nam) := mget(paste0('i.', nam))]
    nams <- nam[seq_len(length(nam) / 2)]
    ColNames <- gsub(pattern = "_mean", replacement = "", x = nams)
    for(i in ColNames) data[, paste0(i, "_Standardize") := (get(i) - get(paste0(i, "_mean"))) / get(paste0(i, "_sd"))]
    data.table::set(data, j = c(nam), value = NULL)
  } else {
    data.table::setkeyv(x = data, cols = GroupVars)
    data.table::setkeyv(x = ScoreTable, cols = GroupVars)
    data[ScoreTable, paste0(nam) := mget(paste0('i.', nam))]
    nams <- nam[seq_len(length(nam) / 2)]
    ColNames <- gsub(pattern = "_mean", replacement = "", x = nams)
    for(i in ColNames) data[, eval(i) := get(paste0(i, "_Standardize")) * get(paste0(i, "_sd")) + get(paste0(i, "_mean"))]
    data.table::set(data, j = c(nam), value = NULL)
  }

  # Return
  return(data)
}

#' @title PercRank
#'
#' @description Generate percent ranks for multiple variables, by groups if provided, and with a selected granularity
#'
#' @author Adrian Antico
#' @family Feature Engineering
#'
#' @param data Source data.table
#' @param ColNames Character vector of column names
#' @param GroupVars Character vector of column names to have percent ranks by the group levels
#' @param Granularity Provide a value such that data.table::frank(Variable) * (1 / Granularity) / .N * Granularity. Default is 0.001
#' @param ScoreTable = FALSE. Set to TRUE to get the reference values for applying to new data. Pass to scoring version of this function
#'
#' @examples
#' \dontrun{
#' data <- data.table::fread(file.choose())
#' x <- PercRank(data, ColNames = c('Weekly_Sales', 'XREG1'), GroupVars = c('Region','Store','Dept'), Granularity = 0.001, ScoreTable = TRUE)
#' }
#'
#' @noRd
PercRank <- function(data, ColNames, GroupVars = NULL, Granularity = 0.001, ScoreTable = FALSE) {
  if(length(GroupVars) == 0L) {
    data[, paste0(ColNames, '_PercRank') := lapply(.SD, FUN = function(x) data.table::frank(x) * (1 / Granularity) / .N * Granularity), .SDcols = c(ColNames)]
  } else {
    data[, paste0(ColNames, '_PercRank') := lapply(.SD, FUN = function(x) data.table::frank(x) * (1 / Granularity) / .N * Granularity), .SDcols = c(ColNames), by = c(eval(GroupVars))]
  }
  if(!ScoreTable) {
    return(data)
  } else {
    return(list(
      data = data,
      ScoreTable = unique(data[, .SD, .SDcols = c(ColNames, paste0(ColNames, '_PercRank'))])
    ))
  }
}

#' Test YeoJohnson Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param eps erorr tolerance
#' @param ... Arguments to pass along
#' @return YeoJohnson results
Test_YeoJohnson <- function(x,
                            eps = 0.001,
                            ...) {
  stopifnot(is.numeric(x))
  lambda <- Estimate_YeoJohnson_Lambda(x, eps = eps, ...)
  trans_data <- x
  na_idx <- is.na(x)
  trans_data[!na_idx] <- Apply_YeoJohnson(x[!na_idx], lambda, eps)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "YeoJohnson", Data = trans_data, Lambda = lambda, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Estimate YeoJohnson Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lower the lower bound for search
#' @param upper the upper bound for search
#' @param eps erorr tolerance
#' @return YeoJohnson results
Estimate_YeoJohnson_Lambda <- function(x,
                                       lower = -5,
                                       upper = 5,
                                       eps = 0.001) {

  n <- length(x)
  ccID <- !is.na(x)
  x <- x[ccID]

  # See references, Yeo & Johnson Biometrika (2000)
  yj_loglik <- function(lambda) {
    x_t <- Apply_YeoJohnson(x, lambda, eps)
    x_t_bar <- mean(x_t)
    x_t_var <- var(x_t) * (n - 1) / n
    constant <- sum(sign(x) * log(abs(x) + 1))
    - 0.5 * n * log(x_t_var) + (lambda - 1) * constant
  }

  results <- optimize(
    yj_loglik,
    lower = lower,
    upper = upper,
    maximum = TRUE,
    tol = .0001)
  return(results$maximum)
}

#' Apply YeoJohnson Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lambda optimal lambda
#' @param eps erorr tolerance
#' @return YeoJohnson results
Apply_YeoJohnson <- function(x,
                             lambda,
                             eps = 0.001) {
  pos_idx <- x >= 0
  neg_idx <- x < 0

  # Transform negative values
  if(any(pos_idx)) {
    if(abs(lambda) < eps) {
      x[pos_idx] <- log(x[pos_idx] + 1)
    } else {
      x[pos_idx] <- ((x[pos_idx] + 1) ^ lambda - 1) / lambda
    }
  }

  # Transform nonnegative values
  if(any(neg_idx)) {
    if(abs(lambda - 2) < eps) {
      x[neg_idx] <- -log(-x[neg_idx] + 1)
    } else {
      x[neg_idx] <- -((-x[neg_idx] + 1) ^ (2 - lambda) - 1) / (2 - lambda)
    }
  }
  return(x)
}

#' Inverse YeoJohnson Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lambda optimal lambda
#' @param eps erorr tolerance
#' @return YeoJohnson results
InvApply_YeoJohnson <- function(x,
                                lambda,
                                eps = 0.001) {
  val <- x
  neg_idx <- x < 0
  if(any(!neg_idx)) {
    if(abs(lambda) < eps) {
      val[!neg_idx] <- exp(x[!neg_idx]) - 1
    } else {
      val[!neg_idx] <- (x[!neg_idx] * lambda + 1) ^ (1 / lambda) - 1
    }
  }
  if(any(neg_idx)) {
    if(abs(lambda - 2) < eps) {
      val[neg_idx] <- -expm1(-x[neg_idx])
    } else {
      val[neg_idx] <- 1 - (-(2 - lambda) * x[neg_idx] + 1) ^ (1 / (2 - lambda))
    }
  }
  return(val)
}

#' Test BoxCox Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param ... Arguments to pass along
#' @return BoxCox results
Test_BoxCox <- function(x, ...) {
  stopifnot(is.numeric(x))
  lambda <- Estimate_BoxCox_Lambda(x, ...)
  trans_data <- Apply_BoxCox(x, lambda)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "BoxCox", Data = trans_data, Lambda = lambda, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Estimate BoxCox Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lower the lower bound for search
#' @param upper the upper bound for search
#' @param eps erorr tolerance
#' @return BoxCox results
Estimate_BoxCox_Lambda <- function(x,
                                   lower = -1,
                                   upper = 2,
                                   eps = 0.001) {
  n <- length(x)
  ccID <- !is.na(x)
  x <- x[ccID]
  if (any(x <= 0)) stop("x must be positive")
  log_x <- log(x)
  xbar <- exp(mean(log_x))
  fit <- lm(x ~ 1, data = data.frame(x = x))
  xqr <- fit$qr
  boxcox_loglik <- function(lambda) {
    if (abs(lambda) > eps)
      xt <- (x ^ lambda - 1) / lambda
    else
      xt <- log_x * (1 + (lambda * log_x) / 2 *
                       (1 + (lambda * log_x) / 3 *
                          (1 + (lambda * log_x) / 4)))
    - n / 2 * log(sum(qr.resid(xqr, xt / xbar ^ (lambda - 1)) ^ 2))
  }

  results <- optimize(
    boxcox_loglik,
    lower = lower,
    upper = upper,
    maximum = TRUE,
    tol = .0001)
  return(results$maximum)
}

#' Apply BoxCox Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lambda optimal lambda
#' @param eps erorr tolerance
#' @return BoxCox results
Apply_BoxCox <- function(x,
                         lambda,
                         eps = 0.001) {
  if(lambda < 0) x[x < 0] <- NA
  if(abs(lambda) < eps) {
    val <- log(x)
  } else {
    val <- (sign(x) * abs(x) ^ lambda - 1) / lambda
  }
  return(val)
}

#' Inverse BoxCox Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @param lambda optimal lambda
#' @param eps erorr tolerance
#' @return BoxCox results
InvApply_BoxCox <- function(x,
                            lambda,
                            eps = 0.001) {
  if(lambda < 0) x[x > -1 / lambda] <- NA
  if(abs(lambda) < eps) {
    val <- exp(x)
  } else {
    x <- x * lambda + 1
    val <- sign(x) * abs(x) ^ (1 / lambda)
  }
  return(val)
}

#' Test Asinh Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asinh results
Test_Asinh <- function(x) {
  stopifnot(is.numeric(x))
  trans_data <- asinh(x)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "Asinh", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Inverse Asinh Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asinh results
Apply_Asinh <- function(x) {
  return(asinh(x))
}

#' Inverse Asinh Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asinh results
InvApply_Asinh <- function(x) {
  return(sinh(x))
}

#' Test Asin Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asin results
Test_Asin <- function(x) {
  stopifnot(is.numeric(x))
  trans_data <- asin(sqrt(x))
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "Asin", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Inverse Asin Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asin results
Apply_Asin <- function(x) {
  return(asin(sqrt(x)))
}

#' Inverse Asin Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Asin results
InvApply_Asin <- function(x) {
  return(sin(x) ^ 2)
}

#' Test Logit Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Logit results
Test_Logit <- function(x) {
  stopifnot(is.numeric(x))
  trans_data <- log(x / (1 - x))
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "Logit", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Apply Logit Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Logit results
Apply_Logit <- function(x) {
  return(log(x / (1 - x)))
}

#' Inverse Logit Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Logit results
InvApply_Logit <- function(x) {
  return(1 / (1 + exp(-x)))
}

#' Test Identity Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Identity results
Test_Identity <- function(x) {
  stopifnot(is.numeric(x))
  x.t <- x
  mu <- mean(x.t, na.rm = TRUE)
  sigma <- sd(x.t, na.rm = TRUE)
  x.t <- (x.t - mu) / sigma
  ptest <- nortest::pearson.test(x.t)
  val <- list(Name = "Identity", Data = x, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Test Log Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
Test_Log <- function(x) {
  stopifnot(is.numeric(x))
  trans_data <- log(x)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "Log", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Apply Log Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
Apply_Log <- function(x) {
  return(log(x))
}

#' Inverse Log Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
InvApply_Log <- function(x) {
  return(exp(x))
}

#' Test LogPlus1 Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return LogPlus1 results
Test_LogPlus1 <- function(x) {
  stopifnot(is.numeric(x))
  xx <- min(x, na.rm = TRUE)
  if(xx <= 0) trans_data <- log(x+abs(xx)+1) else trans_data <- log(x)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "LogPlus1", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Apply LogPlus1 Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
Apply_LogPlus1 <- function(x) {
  return(log(x+1))
}

#' Inverse LogPlus1 Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
InvApply_LogPlus1 <- function(x) {
  return(exp(x)-1)
}

#' Test Sqrt Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Sqrt results
Test_Sqrt <- function(x) {
  stopifnot(is.numeric(x))
  trans_data <- sqrt(x)
  mu <- mean(trans_data, na.rm = TRUE)
  sigma <- sd(trans_data, na.rm = TRUE)
  trans_data_standardized <- (trans_data - mu) / sigma
  ptest <- nortest::pearson.test(trans_data_standardized)
  val <- list(Name = "Sqrt", Data = trans_data, Lambda = NA, Normalized_Statistic = unname(ptest$statistic / ptest$df))
  return(val)
}

#' Apply Sqrt Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
Apply_Sqrt <- function(x) {
  return(sqrt(x))
}

#' Inverse Sqrt Transformation
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @noRd
#' @param x The data in numerical vector form
#' @return Log results
InvApply_Sqrt <- function(x) {
  return(x^2)
}

#' @title AutoTransformationCreate
#'
#' @description AutoTransformationCreate is a function for automatically identifying the optimal transformations for numeric features and transforming them once identified. This function will loop through your selected transformation options (YeoJohnson, BoxCox, Asinh, Asin, and Logit) and find the one that produces data that is the closest to normally distributed data. It then makes the transformation and collects the metadata information for use in the AutoTransformationScore() function, either by returning the objects (always) or saving them to file (optional).
#'
#' @author Adrian Antico
#' @family Feature Engineering
#' @param data This is your source data
#' @param ColumnNames List your columns names in a vector, for example, c("Target", "IV1")
#' @param Methods Choose from "YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Asin", "Logit", and "Identity". Note, LogPlus1 runs
#' @param Path Set to the directly where you want to save all of your modeling files
#' @param TransID Set to a character value that corresponds with your modeling project
#' @param SaveOutput Set to TRUE to save necessary file to run AutoTransformationScore()
#' @return data with transformed columns and the transformation object for back-transforming later
#' @examples
#' \dontrun{
#' # Create Fake Data
#' data <- AutoQuant::FakeDataGenerator(
#'   Correlation = 0.85,
#'   N = 25000,
#'   ID = 2L,
#'   ZIP = 0,
#'   FactorCount = 2L,
#'   AddDate = FALSE,
#'   Classification = FALSE,
#'   MultiClass = FALSE)
#'
#' # Columns to transform
#' Cols <- names(data)[1L:11L]
#' print(Cols)
#'
#' # Run function
#' data <- AutoQuant::AutoTransformationCreate(
#'   data,
#'   ColumnNames = Cols,
#'   Methods = c("YeoJohnson", "BoxCox", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "Identity"),
#'   Path = getwd(),
#'   TransID = "Trans",
#'   SaveOutput = TRUE)
#' }
#' @noRd
AutoTransformationCreate <- function(data,
                                     ColumnNames = NULL,
                                     Methods = c("BoxCox","YeoJohnson","Asinh","Log","LogPlus1","Sqrt","Asin","Logit","Identity"),
                                     Path = NULL,
                                     TransID = "ModelID",
                                     SaveOutput = FALSE) {

  # Check arguments
  Methods <- unique(tolower(Methods))
  if(!data.table::is.data.table(data)) data.table::setDT(data)
  if(!any(tolower(Methods) %chin% c("boxcox", "yeojohnson", "asinh", "sqrt", "log", "logplus1", "asin", "logit"))) stop("Methods not supported")
  # if(!"identity" %chin% Methods) Methods <- c(Methods, "identity")
  if(is.numeric(ColumnNames) || is.integer(ColumnNames)) ColumnNames <- names(data)[ColumnNames]
  for(i in ColumnNames) if(!(any(class(data[[eval(i)]]) %chin% c("numeric", "integer")))) stop("ColumnNames must be for numeric or integer columns")

  # Loop through ColumnNames
  # colNames = 1
  for(colNames in seq_along(ColumnNames)) {# colNames = 1L

    # Collection Object
    if(length(Methods) < 5) {
      EvaluationTable <- data.table::data.table(
        ColumnName = rep("BLABLA", length(ColumnNames) * (length(Methods)+1)),
        MethodName = rep("BLABLA", length(ColumnNames) * (length(Methods)+1)),
        Lambda = rep(1.0, length(ColumnNames) * (length(Methods)+1)),
        NormalizedStatistics = rep(1.0, length(ColumnNames) * (length(Methods)+1)))
    } else {
      EvaluationTable <- data.table::data.table(
        ColumnName = rep("BLABLA", length(ColumnNames) * (length(Methods) + 1)),
        MethodName = rep("BLABLA", length(ColumnNames) * (length(Methods) + 1)),
        Lambda = rep(1.0, length(ColumnNames) * (length(Methods) + 1)),
        NormalizedStatistics = rep(1.0, length(ColumnNames) * (length(Methods) + 1)))
    }
    DataCollection <- list()
    Counter <- 0L

    # Check range of data
    MinVal <- min(data[[eval(ColumnNames[colNames])]], na.rm = TRUE)
    MaxVal <- max(data[[eval(ColumnNames[colNames])]], na.rm = TRUE)

    # Create Final Methods Object
    FinalMethods <- Methods

    # Update Methods
    if(MinVal <= 0) FinalMethods <- FinalMethods[!(tolower(FinalMethods) %chin% c("boxcox","log","logit"))]
    if(MinVal < 0) FinalMethods <- FinalMethods[!(tolower(FinalMethods) %chin% c("sqrt","asin"))]
    if(MaxVal > 1) FinalMethods <- FinalMethods[!(tolower(FinalMethods) %chin% c("asin"))]
    if(MaxVal >= 1) FinalMethods <- FinalMethods[!(tolower(FinalMethods) %chin% c("logit"))]

    # Store column data as vector
    x <- data[[eval(ColumnNames[colNames])]]

    # YeoJohnson
    if(any(tolower(FinalMethods) %chin% "yeojohnson")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_YeoJohnson(x)
      DataCollection[["yeojohnson"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # Log
    if(any(tolower(FinalMethods) %chin% "log")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Log(x)
      DataCollection[["log"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = NA)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # LogPlus1
    if(any(tolower(FinalMethods) %chin% "logplus1")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- AutoPlots:::Test_LogPlus1(x)
      DataCollection[["logplus1"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = NA)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # Sqrt
    if(any(tolower(FinalMethods) %chin% "sqrt")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Sqrt(x)
      DataCollection[["sqrt"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = NA)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # BoxCox
    if(any(tolower(FinalMethods) %chin% "boxcox")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_BoxCox(x)
      DataCollection[["boxcox"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # Asinh
    if(any(tolower(FinalMethods) %chin% "asinh")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Asinh(x)
      DataCollection[["asinh"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # Asin
    if(any(tolower(FinalMethods) %chin% "asin")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Asin(x)
      DataCollection[["asin"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # Logit
    if(any(tolower(FinalMethods) %chin% "logit")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Logit(x)
      DataCollection[["logit"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # Identity
    if(any(tolower(FinalMethods) %chin% "identity")) {
      Counter <- Counter + 1L
      data.table::set(EvaluationTable, i = Counter, j = "ColumnName", value = eval(ColumnNames[colNames]))
      output <- Test_Identity(x)
      DataCollection[["identity"]] <- output$Data
      data.table::set(EvaluationTable, i = Counter, j = "MethodName", value = output$Name)
      data.table::set(EvaluationTable, i = Counter, j = "Lambda", value = output$Lambda)
      data.table::set(EvaluationTable, i = Counter, j = "NormalizedStatistics", value = output$Normalized_Statistic)
    }

    # Pick winner
    EvaluationTable <- EvaluationTable[MethodName != "BLABLA"]
    if(colNames == 1L) {
      Results <- EvaluationTable[order(NormalizedStatistics)][1L]
    } else {
      Results <- data.table::rbindlist(list(Results, EvaluationTable[order(NormalizedStatistics)][1L]))
    }

    # Apply to data----
    data <- tryCatch({data[, ColumnNames[colNames] := DataCollection[[tolower(Results[eval(colNames), MethodName])]]]}, error = function(x) data)
  }

  # Save output----
  if(SaveOutput && !is.null(Path)) data.table::fwrite(Results, file = file.path(normalizePath(Path), paste0(TransID, "_transformation.csv")))

  # Return data----
  return(list(Data = data, FinalResults = Results))
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# > Automated Plot Functions                                                  ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Plot.StandardPlots
#'
#' @description Helper for standard plots
#'
#' @author Adrian Antico
#' @family Auto Plotting
#'
#' @param PlotType character
#' @param dt source data.table
#' @param PreAgg FALSE
#' @param AggMethod character
#' @param SampleSize character
#' @param YVar Y-Axis variable name
#' @param DualYVar Secondary Axis for Line, Step, and Area plots
#' @param XVar X-Axis variable name
#' @param ZVar Z-Axis variable name
#' @param GroupVar Character variable variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param DualYVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins For histograms
#' @param Height NULL or valid css unit
#' @param Width NULL or valid css unit
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine character
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param TextColor character
#' @param FontSize numeric
#' @param Debug Debugging purposes
#'
#' @export
Plot.StandardPlots <- function(dt = NULL,
                               PreAgg = FALSE,
                               PlotType = 'Scatter',
                               SampleSize = 100000L,
                               AggMethod = 'mean',
                               NumberBins = 30,
                               YVar = NULL,
                               DualYVar = NULL,
                               XVar = NULL,
                               ZVar = NULL,
                               GroupVar = NULL,
                               YVarTrans = NULL,
                               DualYVarTrans = NULL,
                               XVarTrans = NULL,
                               ZVarTrans = NULL,
                               FacetRows = 1,
                               FacetCols = 1,
                               FacetLevels = NULL,
                               Height = NULL,
                               Width = NULL,
                               EchartsTheme = "dark-blue",
                               TimeLine = FALSE,
                               Title = NULL,
                               ShowLabels = FALSE,
                               Title.YAxis = NULL,
                               Title.XAxis = NULL,
                               NumLevels_Y = 75,
                               NumLevels_X = 40,
                               TextColor =        "white",
                               FontSize = 14,
                               Debug = FALSE) {

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # Debug
  if(Debug) print(paste0('Plot.StandardPlots() begin, PlotType = ', PlotType))

  Title.FontSize <- FontSize + 8L

  # Pie Plot
  if(tolower(PlotType) == 'pieplot') {
    p1 <- AutoPlots:::Plot.Pie(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = if(length(XVar) == 0 && length(GroupVar) > 0L) GroupVar[1L] else XVar,
      YVar = YVar,
      GroupVar = NULL,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Donut Plot
  if(tolower(PlotType) == 'donutplot') {
    p1 <- AutoPlots:::Plot.Donut(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = if(length(XVar) == 0 && length(GroupVar) > 0L) GroupVar[1L] else XVar,
      YVar = YVar,
      GroupVar = NULL,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Rosetype Plot
  if(tolower(PlotType) == 'rosetypeplot') {
    p1 <- AutoPlots:::Plot.Rosetype(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = if(length(XVar) == 0 && length(GroupVar) > 0L) GroupVar[1L] else XVar,
      YVar = YVar,
      GroupVar = NULL,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Box Plot
  if(tolower(PlotType) == 'boxplot') {
    p1 <- AutoPlots:::Plot.Box(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Histogram Plot
  if(tolower(PlotType) == 'histogramplot') {
    p1 <- AutoPlots:::Plot.Histogram(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      NumberBins = NumberBins,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Density Plot
  if(tolower(PlotType) == 'densityplot') {
    p1 <- AutoPlots:::Plot.Density(
      dt = dt,
      SampleSize = SampleSize,
      GroupVar=GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      XVar = NULL,
      YVar = if(length(YVar) > 0L) YVar else XVar,
      Width = Width,
      Height = Height,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Line Plot
  if(tolower(PlotType) == 'lineplot') {
    p1 <- AutoPlots::Plot.Line(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = XVar,
      YVar = YVar,
      DualYVar = DualYVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      DualYVarTrans = DualYVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Area Plot
  if(tolower(PlotType) == 'areaplot') {
    p1 <- AutoPlots::Plot.Area(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = XVar,
      YVar = YVar,
      DualYVar = DualYVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      DualYVarTrans = DualYVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Step Plot
  if(tolower(PlotType) == 'stepplot') {
    p1 <- AutoPlots::Plot.Step(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = XVar,
      YVar = YVar,
      DualYVar = DualYVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      DualYVarTrans = DualYVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # River Plot
  if(tolower(PlotType) == 'riverplot') {
    p1 <- AutoPlots::Plot.River(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      ShowSymbol = FALSE,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Polar Plot
  if(tolower(PlotType) == 'polarplot') {
    p1 <- AutoPlots:::Plot.Polar(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = XVar,
      YVar = YVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)

    # dt = dt
    # PreAgg = PreAgg
    # AggMethod = AggMethod
    # XVar = XVar
    # YVar = YVar
    # YVarTrans = YVarTrans
    # XVarTrans = XVarTrans
    # FacetRows = FacetRows
    # FacetCols = FacetCols
    # FacetLevels = FacetLevels
    # Width = "1100px"
    # Height = "600px"
    # Title = Title
    # ShowLabels = ShowLabels
    # Title.YAxis = Title.YAxis
    # Title.XAxis = Title.XAxis
    # EchartsTheme = EchartsTheme
    # TimeLine = TimeLine
    # X_Scroll = TRUE
    # Y_Scroll = TRUE
    # TextColor = TextColor
    # title.fontSize = Title.FontSize
    # Debug = Debug

  }

  # Bar Plot
  if(tolower(PlotType) == 'barplot') {
    p1 <- AutoPlots:::Plot.Bar(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = XVar,
      YVar = YVar,
      GroupVar = if(all(XVar == GroupVar)) NULL else GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Stacked Bar Plot
  if(tolower(PlotType) == 'stackedbarplot') {
    p1 <- AutoPlots:::Plot.StackedBar(
      dt = dt,
      PreAgg = PreAgg,
      AggMethod = AggMethod,
      XVar = XVar,
      YVar = YVar,
      GroupVar = if(all(XVar == GroupVar)) NULL else GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # 3D Bar Plot
  if(tolower(PlotType) %in% c('barplot3d','barplotd')) {
    p1 <- AutoPlots::Plot.BarPlot3D(
      PreAgg = PreAgg,
      dt = dt,
      YVar = YVar,
      XVar = XVar,
      ZVar = ZVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      AggMethod = AggMethod,
      NumberBins = 21,
      NumLevels_X = NumLevels_Y,
      NumLevels_Y = NumLevels_X,
      Width = Width,
      Height = Height,
      EchartsTheme = EchartsTheme,
      X_Scroll = X_Scroll,
      Y_Scroll = Y_Scroll,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Heat Map
  if(tolower(PlotType) %in% 'heatmapplot') {
    p1 <- AutoPlots::Plot.HeatMap(
      PreAgg = PreAgg,
      dt = dt,
      YVar = YVar,
      XVar = XVar,
      ZVar = ZVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      AggMethod = AggMethod,
      NumberBins = 21,
      NumLevels_X = NumLevels_Y,
      NumLevels_Y = NumLevels_X,
      Width = Width,
      Height = Height,
      EchartsTheme = EchartsTheme,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Correlation Matrix Plot
  if(tolower(PlotType) == 'correlogramplot') {
    p1 <- AutoPlots:::Plot.CorrMatrix(
      dt = dt,
      PreAgg = PreAgg,
      CorrVars = YVar,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      EchartsTheme = EchartsTheme,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      Method = "spearman",
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Scatter Plot
  if(tolower(PlotType) %in% 'scatterplot') {
    if(SampleSize > 30000) SampleSize <- 30000
    p1 <- AutoPlots:::Plot.Scatter(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      GroupVar = if(all(XVar == GroupVar)) NULL else GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Copula Plot
  if(tolower(PlotType) %in% 'copulaplot') {
    if(SampleSize > 30000) SampleSize <- 30000
    p1 <- AutoPlots:::Plot.Copula(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      GroupVar = if(all(XVar == GroupVar)) NULL else GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Scatter3D Plot
  if(tolower(PlotType) %in% c('scatterplot3d','scatterplotd')) {
    p1 <- AutoPlots:::Plot.Scatter3D(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      ZVar = ZVar,
      GroupVar = if(all(XVar == GroupVar)) NULL else GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # Copula Plot
  if(tolower(PlotType) %in% c('copulaplot3d','copulaplotd')) {
    p1 <- AutoPlots:::Plot.Copula3D(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      ZVar = ZVar,
      GroupVar = if(all(XVar == GroupVar)) NULL else GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Width = Width,
      Height = Height,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }
}

#' @title Plots.ModelEvaluation
#'
#' @description Plot helper for model evaluation plot types
#'
#' @author Adrian Antico
#' @family Auto Plotting
#'
#' @param dt source data.table
#' @param AggMethod character
#' @param SampleSize 100000L
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param PlotType character
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param ZVar Z-Axis variable name
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumLevels_Y = 75
#' @param NumLevels_X = 40
#' @param Height = NULL,
#' @param Width = NULL,
#' @param TextColor hex
#' @param NumberBins numeric
#' @param Debug Debugging purposes
#' @export
Plots.ModelEvaluation <- function(dt = NULL,
                                  AggMethod = "mean",
                                  SampleSize = 100000L,
                                  PlotType = NULL,
                                  YVar = NULL,
                                  TargetLevel = NULL,
                                  ZVar = NULL,
                                  XVar = NULL,
                                  GroupVar = NULL,
                                  YVarTrans = "Identity",
                                  XVarTrans = "Identity",
                                  ZVarTrans = "Identity",
                                  FacetRows = 1,
                                  FacetCols = 1,
                                  FacetLevels = NULL,
                                  NumLevels_Y = 75,
                                  NumLevels_X = 40,
                                  Height = NULL,
                                  Width = NULL,
                                  Title = NULL,
                                  ShowLabels = FALSE,
                                  Title.YAxis = NULL,
                                  Title.XAxis = NULL,
                                  EchartsTheme = "dark-blue",
                                  TimeLine = FALSE,
                                  TextColor =        "white",
                                  FontSize = 14L,
                                  NumberBins = 20,
                                  Debug = FALSE) {

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # Debugging
  if(Debug) {print('Running Plots.ModelEvaluation')}
  if(length(SampleSize) == 0L) SampleSize <- 30000L
  Title.FontSize = FontSize + 8L

  if(Debug) print(paste0("Plots.ModelEvaluation == ", PlotType))

  # Copula Plot ----
  if(PlotType %in% 'Residuals') {
    p1 <- AutoPlots::Plot.Residuals.Histogram(
      dt = dt,
      SampleSize = 50000L,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      NumberBins = NumberBins,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      TextColor = TextColor,
      title.fontSize = Title.FontSize,
      Debug = Debug)
    return(p1)
  }

  # ----
  # Residuals_2 Scatter Plot ----
  if(PlotType %chin% "ResidScatter") {
    p1 <- AutoPlots::Plot.Residuals.Scatter(
      dt = dt,
      SampleSize = min(SampleSize, 30000L),
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      TextColor = TextColor,
      Debug = Debug)
    return(p1)
  }

  # ----

  # Evaluation Plot ----
  if(PlotType == "CalibrationLine") {
    p1 <- AutoPlots::Plot.Calibration.Line(
      dt = dt,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      AggMethod = AggMethod,
      NumberBins = 21,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      TextColor = TextColor,
      Debug = Debug)
    return(eval(p1))
  }

  # ----

  # Evaluation Heatmap ----
  if(PlotType == "CalibrationBox") {
    p1 <- AutoPlots::Plot.Calibration.Box(
      dt = dt,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      AggMethod = 'mean',
      NumberBins = 21,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      SampleSize = SampleSize,
      TextColor = TextColor,
      Debug = FALSE)
    return(eval(p1))
  }

  # ----

  # ROC Plot ----
  if(PlotType == "ROCPlot") {
    p1 <- tryCatch({AutoPlots::Plot.ROC(
      dt = dt,
      SampleSize = SampleSize,
      XVar = XVar,
      YVar = YVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = "True Positive Rate",
      Title.XAxis = "1 - False Positive Rate",
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      TextColor = TextColor,
      Debug = Debug)}, error = function(x) NULL)
    return(p1)
  }

  # ----

  # Gains Plot ----
  if(PlotType == "GainsPlot") {
    p1 <- AutoPlots::Plot.Gains(
      dt = dt,
      PreAgg = FALSE,
      XVar = XVar,
      YVar = YVar,
      ZVar = NULL,
      GroupVar = NULL,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      NumberBins = 20,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      TextColor = TextColor,
      Debug = FALSE)
    return(p1)
  }

  # ----

  # Lift Plot ----
  if(PlotType == "LiftPlot") {
    p1 <- AutoPlots::Plot.Lift(
      dt = dt,
      PreAgg = FALSE,
      XVar = XVar,
      YVar = YVar,
      ZVar = NULL,
      GroupVar = NULL,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      NumberBins = 20,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = "Lift",
      Title.XAxis = "% Positive Classified",
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      TextColor = TextColor,
      Debug = FALSE)
    return(p1)
  }

  # ----

  # Variable Importance Plot ----
  if(PlotType == "VariableImportance") {
    p1 <- AutoPlots::Plot.VariableImportance(
      dt = dt,
      AggMethod = 'mean',
      XVar = "Variable",
      YVar = "Importance",
      GroupVar = NULL,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      TextColor = TextColor,
      Debug = FALSE)
    return(p1)
  }

  # ----

  # Shap VI ----
  if(PlotType == 'ShapleyImportance') {
    p1 <- AutoPlots::Plot.ShapImportance(
      PreAgg = FALSE,
      dt = dt,
      YVar = NULL,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      AggMethod = AggMethod,
      NumberBins = 21,
      NumLevels_X = NumLevels_Y,
      NumLevels_Y = NumLevels_X,
      EchartsTheme = EchartsTheme,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis)
    return(p1)
  }

  # ----

  # Confusion Matrix Heatmap ----
  if(PlotType == "ConfusionMatrixHeatmap") {
    p1 <- AutoPlots::Plot.ConfusionMatrix(
      dt = dt,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      XVar = XVar,
      YVar = YVar,
      ZVar = NULL,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      PreAgg = FALSE,
      NumberBins = 21,
      NumLevels_X = 50,
      NumLevels_Y = 50,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      TextColor = TextColor)
    return(p1)
  }

  # ----

  # Partial Dependence Plot ----
  if(PlotType == 'PartialDependenceLine' && length(XVar) > 0L) {
    p1 <- AutoPlots::Plot.PartialDependence.Line(
      dt = dt,
      XVar = XVar,
      YVar = YVar,
      ZVar = ZVar,
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      AggMethod = 'mean',
      NumberBins = 21,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = TextColor,
      Debug = Debug)
    return(p1)
  }

  # ----

  # Partial Dependence Box Plot ----
  if(PlotType == 'PartialDependenceHeatMap' && length(XVar) > 0L) {
    p1 <- tryCatch({AutoPlots::Plot.PartialDependence.HeatMap(
      dt = dt,
      AggMethod = 'mean',
      XVar = XVar,
      YVar = YVar,
      ZVar = ZVar,
      GroupVar = NULL,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      NumberBins = 21,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = Title.YAxis,
      Title.XAxis = Title.XAxis,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = TextColor,
      Debug = Debug)}, error = function(x) NULL)
    return(p1)
  }

  # ----

  if(!exists('p1')) p1 <- NULL
  return(p1)
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# > Distribution Plot Functions                                               ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Plot.ProbabilityPlot
#'
#' @description Build a normal probability plot
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param YVar Y-Axis variable name
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title 'Violin Plot'
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme = "macaron"
#' @param TimeLine Logical
#' @param TextColor 'darkblue'
#' @param Debug Debugging purposes
#' @export
Plot.ProbabilityPlot <- function(dt = NULL,
                                 SampleSize = 1000L,
                                 YVar = NULL,
                                 YVarTrans = "Identity",
                                 Height = NULL,
                                 Width = NULL,
                                 Title = 'Normal Probability Plot',
                                 ShowLabels = FALSE,
                                 EchartsTheme = "macarons",
                                 Y_Scroll = TRUE,
                                 TextColor =        "white",
                                 title.fontSize = 22,
                                 title.fontWeight = "bold",
                                 title.textShadowColor = '#63aeff',
                                 title.textShadowBlur = 3,
                                 title.textShadowOffsetY = 1,
                                 title.textShadowOffsetX = -1,
                                 yaxis.fontSize = 14,
                                 yaxis.rotate = 0,
                                 ContainLabel = TRUE,
                                 tooltip.trigger = "axis",
                                 Debug = FALSE) {

  # dt = data.table::fread(file.choose())
  # SampleSize = 30000L
  # XVar = NULL
  # YVar = "Daily Margin"
  # GroupVar = NULL
  # YVarTrans = "Identity"
  # XVarTrans = "Identity"
  # FacetRows = 1
  # FacetCols = 1
  # FacetLevels = NULL
  # Height = NULL
  # Width = NULL
  # Title = 'Scatter Plot'
  # ShowLabels = FALSE
  # AddGLM = FALSE
  # Title.YAxis = NULL
  # Title.XAxis = NULL
  # EchartsTheme = "dark"
  # TimeLine = FALSE
  # X_Scroll = TRUE
  # Y_Scroll = TRUE
  # TextColor =        "white"
  # title.fontSize = 22
  # title.fontWeight = "bold"
  # title.textShadowColor = '#63aeff'
  # title.textShadowBlur = 3
  # title.textShadowOffsetY = 1
  # title.textShadowOffsetX = -1
  # yaxis.fontSize = 14
  # xaxis.fontSize = 14
  # xaxis.rotate = 0
  # yaxis.rotate = 0
  # ContainLabel = TRUE
  # tooltip.trigger = "axis"
  # Debug = FALSE

  # Subset cols, define Target - Predicted, NULL YVar in data, Update YVar def, Ensure GroupVar is length(1)
  if(length(SampleSize) == 0L) SampleSize <- 30000L
  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  if(Debug) print("here 1")
  if(Debug) print(head(dt))

  # Subset columns
  dt1 <- dt[, .SD, .SDcols = c(YVar)]

  # Transformation
  # "PercRank"  "Standardize"
  # "Asinh"  "Log"  "LogPlus1"  "Sqrt"  "Asin"  "Logit"  "BoxCox"  "YeoJohnson"
  if(YVarTrans != "Identity") {
    dt1 <- tryCatch({AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data}, error = function(x) dt1)
  }

  # Theoretical Quantiles
  data.table::setorderv(x = dt1, cols = YVar, 1)
  dt1[, temp_i := seq_len(.N)]
  dt1[, `Theoretical Quantiles` := qnorm((temp_i-0.5)/.N)]
  dt1[, temp_i := NULL]

  # Normal Line
  meanX <- dt1[, mean(get(YVar), na.rm = TRUE)]
  sdX <- dt1[, sd(get(YVar), na.rm = TRUE)]
  dt1[, `Normal Line` := eval(meanX) + sdX * `Theoretical Quantiles`]

  # Actual Quantiles
  p1 <- AutoPlots::Plot.Scatter(
    dt = dt1,
    SampleSize = SampleSize,
    XVar = "Theoretical Quantiles",
    YVar = YVar,
    YVarTrans = "Identity",
    Height = Height,
    Width = Width,
    Title = Title,
    Title.YAxis = YVar,
    Title.XAxis = "Theoretical Quantiles",
    EchartsTheme = EchartsTheme,
    Y_Scroll = Y_Scroll,
    TextColor = TextColor,
    title.fontSize = title.fontSize,
    title.fontWeight = title.fontWeight,
    title.textShadowColor = title.textShadowColor,
    title.textShadowBlur = title.textShadowBlur,
    title.textShadowOffsetY = title.textShadowOffsetY,
    title.textShadowOffsetX = title.textShadowOffsetX,
    yaxis.fontSize = yaxis.fontSize,
    yaxis.rotate = yaxis.rotate,
    ContainLabel = ContainLabel,
    tooltip.trigger = tooltip.trigger,
    Debug = Debug)

  # Add Normal Line
  p1 <- echarts4r::e_line_(e = p1, "Normal Line")
  return(p1)
}

#' @title Plot.Histogram
#'
#' @description Build a histogram plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins = 30
#' @param Height = NULL,
#' @param Width = NULL,
#' @param EchartsTheme = EchartsTheme,
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param Debug Debugging purposes
#' @export
Plot.Histogram <- function(dt = NULL,
                           SampleSize = 30000L,
                           XVar = NULL,
                           YVar = NULL,
                           GroupVar = NULL,
                           YVarTrans = "Identity",
                           XVarTrans = "Identity",
                           FacetRows = 1,
                           FacetCols = 1,
                           FacetLevels = NULL,
                           NumberBins = 30,
                           Height = NULL,
                           Width = NULL,
                           Title = "Histogram",
                           ShowLabels = FALSE,
                           Title.YAxis = NULL,
                           Title.XAxis = NULL,
                           EchartsTheme = "macarons",
                           TimeLine = FALSE,
                           X_Scroll = TRUE,
                           Y_Scroll = TRUE,
                           TextColor =        "white",
                           title.fontSize = 22,
                           title.fontWeight = "bold", # normal
                           title.textShadowColor = '#63aeff',
                           title.textShadowBlur = 3,
                           title.textShadowOffsetY = 1,
                           title.textShadowOffsetX = -1,
                           xaxis.fontSize = 14,
                           yaxis.fontSize = 14,
                           Debug = FALSE) {

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # Convert factor to character
  if(length(GroupVar) > 0L && class(dt[[GroupVar]])[1L] == "factor") {
    dt[, eval(GroupVar) := as.character(get(GroupVar))]
  }

  TimeLine <- FALSE

  # Cap number of records
  if(length(SampleSize) == 0L) SampleSize <- 30000
  if(dt[, .N] > SampleSize) {
    dt1 <- dt[order(runif(.N))][seq_len(SampleSize)]
  } else {
    dt1 <- data.table::copy(dt)
  }

  # Define Plotting Variable
  if(length(YVar) == 0L && length(XVar) == 0) return(NULL)
  if(length(YVar) == 0L) {
    YVar <- XVar
    YVarTrans <- XVarTrans
  }
  if(length(XVar) > 0L && length(GroupVar) == 0L) {
    GroupVar <- XVar
    XVar <- NULL
  }

  GroupVar <- tryCatch({GroupVar[1L]}, error = function(x) NULL)

  # Faceting shrink
  if(length(GroupVar) > 0L && (FacetRows > 1L || FacetCols > 1L)) {
    dt1 <- dt1[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,GroupVar)]
  } else {
    dt1 <- dt1[, .SD, .SDcols = c(YVar,GroupVar)]
  }

  # Multiple YVars
  if(length(YVar) > 1L) {
    sqroots <- sqrt(length(YVar))
    if(FacetCols == 1 && FacetRows == 1L) {
      FacetCols <- max(ceiling(sqroots), 6)
      FacetRows <- ceiling(sqroots)
      if((FacetRows - 1L) * FacetCols == length(YVar)) {
        FacetRows <- FacetRows - 1L
      } else if(FacetRows * FacetCols < length(YVar)) {
        while(FacetRows * FacetCols < length(YVar)) {
          FacetRows <- FacetRows + 1L
        }
      }
    }
    XVar <- NULL
    GroupVar <- NULL
    dt1[, temp__ := "a"]
    dt1 <- data.table::melt.data.table(data = dt1, id.vars = "temp__", measure.vars = YVar, variable.name = "Measures", value.name = "Values")
    dt1[, temp__ := NULL]
    GroupVar <- "Measures"
    YVar <- "Values"
  }

  # Transformation
  # "PercRank"  "Standardize"
  # "Asinh"  "Log"  "LogPlus1"  "Sqrt"  "Asin"  "Logit"  "BoxCox"  "YeoJohnson"
  if(YVarTrans != "Identity") {
    for(ggss in YVar) {
      dt1 <- tryCatch({AutoTransformationCreate(data = dt1, ColumnNames = ggss, Methods = YVarTrans)$Data}, error = function(x) dt1)
    }
  }

  # Create histogram data
  if(length(GroupVar) == 0L) {
    Min <- dt1[, min(get(YVar), na.rm = TRUE)]
    Max <- dt1[, max(get(YVar), na.rm = TRUE)]
    Range <- Max - Min
    acc <- ceiling(Range / NumberBins)
    dt1[, Buckets := round(get(YVar) / acc) * acc]
    dt1 <- dt1[, .N, by = "Buckets"][order(Buckets)]
  } else {
    levs <- unique(as.character(dt1[[GroupVar]]))
    gg <- list()
    for(i in levs) {# i <- levs[1]
      temp <- dt1[get(GroupVar) == eval(i)]
      Min <- temp[, min(get(YVar), na.rm = TRUE)]
      Max <- temp[, max(get(YVar), na.rm = TRUE)]
      Range <- Max - Min
      acc <- ceiling(Range / NumberBins)
      temp[, Buckets := round(get(YVar) / acc) * acc]
      gg[[i]] <- temp[, .N, by = c("Buckets",GroupVar)][order(Buckets)]
    }
    dt1 <- data.table::rbindlist(gg)
  }

  # Run Bar Plot for no Group and Stacked Bar for Groups?
  dt1[, Buckets := as.character(Buckets)]
  if(length(GroupVar) == 0L) {
    p1 <- Plot.Bar(
      dt = dt1,
      PreAgg = TRUE,
      XVar = "Buckets",
      YVar = "N",
      Height = Height,
      Width = Width,
      Title = 'Histogram Plot',
      Title.YAxis = "Counts",
      Title.XAxis = YVar,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = X_Scroll,
      Y_Scroll = Y_Scroll,
      TextColor = TextColor,
      title.fontSize = title.fontSize,
      title.fontWeight = title.fontWeight,
      title.textShadowColor = title.textShadowColor,
      title.textShadowBlur = title.textShadowBlur,
      title.textShadowOffsetY = title.textShadowOffsetY,
      title.textShadowOffsetX = title.textShadowOffsetX,
      xaxis.fontSize = xaxis.fontSize,
      yaxis.fontSize = yaxis.fontSize,
      Debug = Debug)
  } else {
    p1 <- Plot.Bar(
      dt = dt1,
      PreAgg = TRUE,
      XVar = "Buckets",
      YVar = "N",
      GroupVar = GroupVar,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      Title = 'Histogram Plot',
      Title.YAxis = "Counts",
      Title.XAxis = YVar,
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      X_Scroll = X_Scroll,
      Y_Scroll = Y_Scroll,
      TextColor = TextColor,
      title.fontSize = title.fontSize,
      title.fontWeight = title.fontWeight,
      title.textShadowColor = title.textShadowColor,
      title.textShadowBlur = title.textShadowBlur,
      title.textShadowOffsetY = title.textShadowOffsetY,
      title.textShadowOffsetX = title.textShadowOffsetX,
      xaxis.fontSize = xaxis.fontSize,
      yaxis.fontSize = yaxis.fontSize,
      Debug = Debug)

    dt = dt1
    PreAgg = TRUE
    XVar = "Buckets"
    YVar = "N"
    GroupVar = GroupVar
    FacetRows = FacetRows
    FacetCols = FacetCols
    FacetLevels = FacetLevels
    Height = Height
    Width = Width
    Title = 'Histogram Plot'
    Title.YAxis = "Counts"
    Title.XAxis = YVar
    EchartsTheme = EchartsTheme
    TimeLine = TimeLine
    X_Scroll = X_Scroll
    Y_Scroll = Y_Scroll
    TextColor = TextColor
    title.fontSize = title.fontSize
    title.fontWeight = title.fontWeight
    title.textShadowColor = title.textShadowColor
    title.textShadowBlur = title.textShadowBlur
    title.textShadowOffsetY = title.textShadowOffsetY
    title.textShadowOffsetX = title.textShadowOffsetX
    xaxis.fontSize = xaxis.fontSize
    yaxis.fontSize = yaxis.fontSize
    Debug = Debug
  }
  return(p1)
}

#' @title Plot.Density
#'
#' @description Density plots, by groups, with transparent continuous plots
#'
#' @family Standard Plots
#'
#' @param dt source data.table
#' @param SampleSize = 100000L
#' @param GroupVar From App
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title = "Density Plot"
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor "white",
#' @param Debug Debugging purposes
#' @export
Plot.Density <- function(dt = NULL,
                         SampleSize = 100000L,
                         YVar = NULL,
                         XVar = NULL,
                         GroupVar = NULL,
                         YVarTrans = "Identity",
                         XVarTrans = "Identity",
                         FacetRows = 1,
                         FacetCols = 1,
                         FacetLevels = NULL,
                         Height = NULL,
                         Width = NULL,
                         Title = "Density Plot",
                         ShowLabels = FALSE,
                         Title.YAxis = NULL,
                         Title.XAxis = NULL,
                         EchartsTheme = "macarons",
                         TimeLine = FALSE,
                         X_Scroll = TRUE,
                         Y_Scroll = TRUE,
                         TextColor =        "white",
                         title.fontSize = 22,
                         title.fontWeight = "bold", # normal
                         title.textShadowColor = '#63aeff',
                         title.textShadowBlur = 3,
                         title.textShadowOffsetY = 1,
                         title.textShadowOffsetX = -1,
                         xaxis.fontSize = 14,
                         yaxis.fontSize = 14,
                         xaxis.rotate = 0,
                         yaxis.rotate = 0,
                         ContainLabel = TRUE,
                         Debug = FALSE) {

  TimeLine <- FALSE

  # Cap number of records
  if(length(SampleSize) == 0L) SampleSize <- 30000
  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # Convert factor to character
  if(length(GroupVar) > 0L && class(dt[[GroupVar]])[1L] == "factor") {
    dt[, eval(GroupVar) := as.character(get(GroupVar))]
  }

  if(dt[, .N] > SampleSize) {
    dt1 <- dt[order(runif(.N))][seq_len(SampleSize)]
  } else {
    dt1 <- data.table::copy(dt)
  }

  # Define Plotting Variable
  if(length(YVar) == 0L && length(XVar) == 0) return(NULL)
  if(length(YVar) == 0L) {
    YVar <- XVar
    YVarTrans <- XVarTrans
  }
  if(length(XVar) > 0L && length(GroupVar) == 0L) {
    GroupVar <- XVar
    XVar <- NULL
  }

  GroupVar <- tryCatch({GroupVar[1L]}, error = function(x) NULL)
  YVar <- tryCatch({YVar}, error = function(x) NULL)

  # Faceting shrink
  if(length(GroupVar) > 0L && (FacetRows > 1L || FacetCols > 1L) && length(FacetLevels) > 0L) {
    dt1 <- dt1[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,GroupVar)]
  } else {
    dt1 <- dt1[, .SD, .SDcols = c(YVar,GroupVar)]
  }

  # Multiple YVars
  if(length(YVar) > 1L) {
    sqroots <- sqrt(length(YVar))
    if(FacetCols == 1 && FacetRows == 1L) {
      FacetCols <- max(ceiling(sqroots), 6)
      FacetRows <- ceiling(sqroots)
      if((FacetRows - 1L) * FacetCols == length(YVar)) {
        FacetRows <- FacetRows - 1L
      } else if(FacetRows * FacetCols < length(YVar)) {
        while(FacetRows * FacetCols < length(YVar)) {
          FacetRows <- FacetRows + 1L
        }
      }
    }
    XVar <- NULL
    GroupVar <- NULL
    dt1[, temp__ := "a"]
    dt1 <- data.table::melt.data.table(data = dt1, id.vars = "temp__", measure.vars = YVar, variable.name = "Measures", value.name = "Values")
    dt1[, temp__ := NULL]
    GroupVar <- "Measures"
    YVar <- "Values"
  }

  # Transformation
  # "PercRank"  "Standardize"
  # "Asinh"  "Log"  "LogPlus1"  "Sqrt"  "Asin"  "Logit"  "BoxCox"  "YeoJohnson"
  if(YVarTrans != "Identity") {
    for(ggss in YVar) {
      dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = ggss, Methods = YVarTrans)$Data
    }
  }

  # Create base plot object
  if(Debug) print('Create Plot with only data')

  if(length(GroupVar) == 0L) {

    p1 <- echarts4r::e_charts_(
      dt1,
      x = NULL,
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_density_(
        e = p1,
        YVar,
        areaStyle = list(opacity = .4),
        smooth = TRUE,
        y_index = 1,
        label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_density_(
        e = p1,
        YVar,
        areaStyle = list(opacity = .4),
        smooth = TRUE,
        y_index = 1)
    }

    p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L && length(Title.YAxis) == 0L) {
      if(length(XVar) > 0L) {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      } else {
        p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = YVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
      }
    } else if(length(Title.XAxis) > 0L) {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.XAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    } else {
      p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = Title.YAxis, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    }

    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))


    return(p1)

  } else {

    data.table::setorderv(x = dt1, cols = GroupVar[1L], 1)

    if(ShowLabels) {
      p1 <- echarts4r::e_charts_(
        dt1 |> dplyr::group_by(get(GroupVar[1L])),
        timeline = TimeLine,
        dispose = TRUE,
        darkMode = TRUE,
        emphasis = list(focus = "series"),
        width = Width,
        height = Height,
        label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_charts_(
        dt1 |> dplyr::group_by(get(GroupVar[1L])),
        timeline = TimeLine,
        dispose = TRUE,
        darkMode = TRUE,
        emphasis = list(focus = "series"),
        width = Width,
        height = Height)
    }

    p1 <- echarts4r::e_density_(e = p1, YVar, areaStyle = list(opacity = .4), smooth = TRUE, y_index = 1)
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
    if(FacetRows > 1L || FacetCols > 1L) {
      p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    } else {
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    }

    return(p1)
  }
}

#' @title Plot.Pie
#'
#' @description Build a pie chart by simply passing arguments to a single function
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param PreAgg logical
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title title
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo","essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired","jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal","sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor 'darkblue'
#' @param title.fontSize Defaults to size 22. Numeric. This changes the size of the title.
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param Debug Debugging purposes
#'
#' @export
Plot.Pie <- function(dt = NULL,
                     PreAgg = FALSE,
                     XVar = NULL,
                     YVar = NULL,
                     GroupVar = NULL,
                     YVarTrans = "Identity",
                     XVarTrans = "Identity",
                     FacetRows = 1,
                     FacetCols = 1,
                     FacetLevels = NULL,
                     AggMethod = 'mean',
                     Height = NULL,
                     Width = NULL,
                     Title = 'Pie Chart',
                     ShowLabels = FALSE,
                     Title.YAxis = NULL,
                     Title.XAxis = NULL,
                     EchartsTheme = "macarons",
                     TimeLine = TRUE,
                     X_Scroll = TRUE,
                     Y_Scroll = TRUE,
                     TextColor =        "white",
                     title.fontSize = 22,
                     title.fontWeight = "bold", # normal
                     title.textShadowColor = '#63aeff',
                     title.textShadowBlur = 3,
                     title.textShadowOffsetY = 1,
                     title.textShadowOffsetX = -1,
                     xaxis.fontSize = 14,
                     yaxis.fontSize = 14,
                     Debug = FALSE) {

  if(length(YVar) > 0L) YVar <- YVar[1L]
  if(length(XVar) > 0L) XVar <- XVar[1L]

  # Used multiple times
  check1 <- length(XVar) != 0 && length(YVar) != 0

  if(!PreAgg) {
    if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
      dt <- data.table::as.data.table(dt)
    })
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)
  }

  # Convert factor to character
  if(length(GroupVar) > 0L && class(dt[[GroupVar]])[1L] == "factor") {
    dt[, eval(GroupVar) := as.character(get(GroupVar))]
  }

  if(length(XVar) > 0L && class(dt[[XVar]])[1L] == "factor") {
    dt[, eval(XVar) := as.character(get(XVar))]
  }

  # Create base plot object
  numvars <- c()
  byvars <- c()
  if(check1) {
    if(Debug) print("BarPlot 2.b")
    if(!PreAgg) {
      if(tryCatch({class(dt[[eval(YVar)]])[1L]}, error = function(x) "bla") %in% c('numeric','integer')) {
        numvars <- unique(c(numvars, YVar))
      } else {
        byvars <- unique(c(byvars, YVar))
      }
      if(tryCatch({class(dt[[eval(XVar)]])[1L]}, error = function(x) "bla") %in% c('numeric','integer')) {
        if(length(numvars) > 0) {
          x <- length(unique(dt[[XVar]]))
          y <- length(unique(dt[[YVar]]))
          if(x > y) {
            byvars <- unique(c(byvars, YVar))
            numvars[1L] <- XVar
          } else {
            byvars <- unique(c(byvars, XVar))
          }
        } else {
          numvars <- unique(c(numvars, XVar))
        }
      } else {
        byvars <- unique(c(byvars, XVar))
      }
      if(!is.null(byvars)) {
        temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars), by = c(byvars)]
        for(i in byvars) {
          if(class(temp[[i]])[1L] %in% c('numeric','integer')) {
            temp[, eval(i) := as.character(get(i))]
          }
        }
      } else {
        temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars)]
      }
    } else {
      if(Debug) print("BarPlot 2.bb")
      temp <- data.table::copy(dt)
      if(Debug) print("BarPlot 2.bbb")
      numvars <- AutoPlots:::ColNameFilter(data = temp, Types = 'numeric')[[1L]]
      byvars <- unlist(AutoPlots:::ColNameFilter(data = temp, Types = "character"))
    }

    # yvar <- temp[[YVar]]
    # xvar <- temp[[XVar]]

    if(Debug) print("BarPlot 2.bbbb")

    # Transformation
    if(YVarTrans != "Identity") {
      temp <- AutoPlots:::AutoTransformationCreate(data = temp, ColumnNames = numvars, Methods = YVarTrans)$Data
    }



    p1 <- echarts4r::e_charts_(
      temp,
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      emphasis = list(focus = "series"),
      width = Width, height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_pie_(e = p1, YVar, stack = XVar, label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_pie_(e = p1, YVar, stack = XVar)
    }

    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "item", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
    p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))

    return(p1)
  }
}

#' @title Plot.Donut
#'
#' @description Build a donut plot by simply passing arguments to a single function
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param PreAgg logical
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title title
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo","essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired","jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal","sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor 'darkblue'
#' @param title.fontSize Defaults to size 22. Numeric. This changes the size of the title.
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param Debug Debugging purposes
#'
#' @export
Plot.Donut <- function(dt = NULL,
                       PreAgg = FALSE,
                       XVar = NULL,
                       YVar = NULL,
                       GroupVar = NULL,
                       YVarTrans = "Identity",
                       XVarTrans = "Identity",
                       FacetRows = 1,
                       FacetCols = 1,
                       FacetLevels = NULL,
                       AggMethod = 'mean',
                       Height = NULL,
                       Width = NULL,
                       Title = 'Donut Plot',
                       ShowLabels = FALSE,
                       Title.YAxis = NULL,
                       Title.XAxis = NULL,
                       EchartsTheme = "macarons",
                       TimeLine = TRUE,
                       X_Scroll = TRUE,
                       Y_Scroll = TRUE,
                       TextColor =        "white",
                       title.fontSize = 22,
                       title.fontWeight = "bold", # normal
                       title.textShadowColor = '#63aeff',
                       title.textShadowBlur = 3,
                       title.textShadowOffsetY = 1,
                       title.textShadowOffsetX = -1,
                       xaxis.fontSize = 14,
                       yaxis.fontSize = 14,
                       Debug = FALSE) {

  if(length(YVar) > 0L) YVar <- YVar[1L]
  if(length(XVar) > 0L) XVar <- XVar[1L]

  # Used multiple times
  check1 <- length(XVar) != 0 && length(YVar) != 0

  if(!PreAgg) {
    if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
      dt <- data.table::as.data.table(dt)
    })
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)
  }

  # Convert factor to character
  if(length(GroupVar) > 0L && class(dt[[GroupVar]])[1L] == "factor") {
    dt[, eval(GroupVar) := as.character(get(GroupVar))]
  }

  if(length(XVar) > 0L && class(dt[[XVar]])[1L] == "factor") {
    dt[, eval(XVar) := as.character(get(XVar))]
  }

  # Create base plot object
  numvars <- c()
  byvars <- c()
  if(check1) {
    if(Debug) print("BarPlot 2.b")
    if(!PreAgg) {
      if(tryCatch({class(dt[[eval(YVar)]])[1L]}, error = function(x) "bla") %in% c('numeric','integer')) {
        numvars <- unique(c(numvars, YVar))
      } else {
        byvars <- unique(c(byvars, YVar))
      }
      if(tryCatch({class(dt[[eval(XVar)]])[1L]}, error = function(x) "bla") %in% c('numeric','integer')) {
        if(length(numvars) > 0) {
          x <- length(unique(dt[[XVar]]))
          y <- length(unique(dt[[YVar]]))
          if(x > y) {
            byvars <- unique(c(byvars, YVar))
            numvars[1L] <- XVar
          } else {
            byvars <- unique(c(byvars, XVar))
          }
        } else {
          numvars <- unique(c(numvars, XVar))
        }
      } else {
        byvars <- unique(c(byvars, XVar))
      }
      if(!is.null(byvars)) {
        temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars), by = c(byvars)]
        for(i in byvars) {
          if(class(temp[[i]])[1L] %in% c('numeric','integer')) {
            temp[, eval(i) := as.character(get(i))]
          }
        }
      } else {
        temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars)]
      }
    } else {
      temp <- data.table::copy(dt)
      numvars <- AutoPlots:::ColNameFilter(data = temp, Types = 'numeric')[[1L]]
      byvars <- unlist(AutoPlots:::ColNameFilter(data = temp, Types = "character"))
    }

    yvar <- temp[[YVar]]
    xvar <- temp[[XVar]]

    # Transformation
    if(YVarTrans != "Identity") {
      temp <- AutoTransformationCreate(data = temp, ColumnNames = numvars, Methods = YVarTrans)$Data
    }

    p1 <- echarts4r::e_charts_(
      temp,
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      emphasis = list(focus = "series"),
      width = Width, height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_pie_(e = p1, YVar, stack = XVar, label = list(show = TRUE), radius = c("50%", "70%"))
    } else {
      p1 <- echarts4r::e_pie_(e = p1, YVar, stack = XVar, radius = c("50%", "70%"))
    }

    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "item", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
    p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))

    return(p1)
  }
}

#' @title Plot.Rosetype
#'
#' @description Build a donut plot by simply passing arguments to a single function
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param PreAgg logical
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title title
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo","essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired","jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal","sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor 'darkblue'
#' @param title.fontSize Defaults to size 22. Numeric. This changes the size of the title.
#' @param BackGroundColor color outside of plot window. Rcolors and hex outside of plot window. Rcolors and hex character
#' @param Debug Debugging purposes
#'
#' @export
Plot.Rosetype <- function(dt = NULL,
                          PreAgg = FALSE,
                          XVar = NULL,
                          YVar = NULL,
                          GroupVar = NULL,
                          YVarTrans = "Identity",
                          XVarTrans = "Identity",
                          FacetRows = 1,
                          FacetCols = 1,
                          FacetLevels = NULL,
                          AggMethod = 'mean',
                          Height = NULL,
                          Width = NULL,
                          Title = 'Donut Plot',
                          ShowLabels = FALSE,
                          Title.YAxis = NULL,
                          Title.XAxis = NULL,
                          EchartsTheme = "macarons",
                          TimeLine = TRUE,
                          X_Scroll = TRUE,
                          Y_Scroll = TRUE,
                          TextColor =        "white",
                          title.fontSize = 22,
                          title.fontWeight = "bold", # normal
                          title.textShadowColor = '#63aeff',
                          title.textShadowBlur = 3,
                          title.textShadowOffsetY = 1,
                          title.textShadowOffsetX = -1,
                          xaxis.fontSize = 14,
                          yaxis.fontSize = 14,
                          Debug = FALSE) {

  if(length(YVar) > 0L) YVar <- YVar[1L]
  if(length(XVar) > 0L) XVar <- XVar[1L]

  # Used multiple times
  check1 <- length(XVar) != 0 && length(YVar) != 0

  if(!PreAgg) {
    if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
      dt <- data.table::as.data.table(dt)
    })
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)
  }

  # Convert factor to character
  if(length(GroupVar) > 0L && class(dt[[GroupVar]])[1L] == "factor") {
    dt[, eval(GroupVar) := as.character(get(GroupVar))]
  }

  if(length(XVar) > 0L && class(dt[[XVar]])[1L] == "factor") {
    dt[, eval(XVar) := as.character(get(XVar))]
  }

  # Create base plot object
  numvars <- c()
  byvars <- c()
  if(check1) {
    if(Debug) print("BarPlot 2.b")
    if(!PreAgg) {
      if(tryCatch({class(dt[[eval(YVar)]])[1L]}, error = function(x) "bla") %in% c('numeric','integer')) {
        numvars <- unique(c(numvars, YVar))
      } else {
        byvars <- unique(c(byvars, YVar))
      }
      if(tryCatch({class(dt[[eval(XVar)]])[1L]}, error = function(x) "bla") %in% c('numeric','integer')) {
        if(length(numvars) > 0) {
          x <- length(unique(dt[[XVar]]))
          y <- length(unique(dt[[YVar]]))
          if(x > y) {
            byvars <- unique(c(byvars, YVar))
            numvars[1L] <- XVar
          } else {
            byvars <- unique(c(byvars, XVar))
          }
        } else {
          numvars <- unique(c(numvars, XVar))
        }
      } else {
        byvars <- unique(c(byvars, XVar))
      }
      if(!is.null(byvars)) {
        temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars), by = c(byvars)]
        for(i in byvars) {
          if(class(temp[[i]])[1L] %in% c('numeric','integer')) {
            temp[, eval(i) := as.character(get(i))]
          }
        }
      } else {
        temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars)]
      }
    } else {
      temp <- data.table::copy(dt)
      numvars <- AutoPlots:::ColNameFilter(data = temp, Types = 'numeric')[[1L]]
      byvars <- unlist(AutoPlots:::ColNameFilter(data = temp, Types = "character"))
    }

    yvar <- temp[[YVar]]
    xvar <- temp[[XVar]]

    # Transformation
    if(YVarTrans != "Identity") {
      temp <- AutoTransformationCreate(data = temp, ColumnNames = numvars, Methods = YVarTrans)$Data
    }

    p1 <- echarts4r::e_charts_(
      temp,
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      emphasis = list(focus = "series"),
      width = Width, height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_pie_(e = p1, YVar, stack = XVar, label = list(show = TRUE), roseType = "radius")
    } else {
      p1 <- echarts4r::e_pie_(e = p1, YVar, stack = XVar, roseType = "radius")
    }

    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "item", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
    p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))

    return(p1)
  }
}

#' @title Plot.Box
#'
#' @description Build a box plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param SampleSize numeric
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine Logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor character hex
#' @param Debug Debugging purposes
#' @export
Plot.Box <- function(dt = NULL,
                     SampleSize = 100000L,
                     XVar = NULL,
                     YVar = NULL,
                     GroupVar = NULL,
                     YVarTrans = "Identity",
                     XVarTrans = "Identity",
                     FacetRows = 1,
                     FacetCols = 1,
                     FacetLevels = NULL,
                     Height = NULL,
                     Width = NULL,
                     Title = 'Box Plot',
                     ShowLabels = FALSE,
                     Title.YAxis = NULL,
                     Title.XAxis = NULL,
                     EchartsTheme = "macarons",
                     TimeLine = FALSE,
                     X_Scroll = TRUE,
                     Y_Scroll = TRUE,
                     TextColor =        "white",
                     title.fontSize = 22,
                     title.fontWeight = "bold", # normal
                     title.textShadowColor = '#63aeff',
                     title.textShadowBlur = 3,
                     title.textShadowOffsetY = 1,
                     title.textShadowOffsetX = -1,
                     xaxis.fontSize = 14,
                     yaxis.fontSize = 14,
                     xaxis.rotate = 0,
                     yaxis.rotate = 0,
                     ContainLabel = TRUE,
                     Debug = FALSE) {

  if(Debug) print("Box 1")

  # Turn off Faceting until I can figure out how to supply it
  FacetRows <- 1L
  FacetCols <- 1L

  # Ensure data.table
  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  if(Debug) print("Box 2")

  # Convert factor to character
  if(length(GroupVar) > 0L && class(dt[[GroupVar]])[1L] == "factor") {
    dt[, eval(GroupVar) := as.character(get(GroupVar))]
  }

  if(Debug) print("Box 3")

  if(length(XVar) > 0L && class(dt[[XVar]])[1L] == "factor") {
    dt[, eval(XVar) := as.character(get(XVar))]
  }

  if(Debug) print("Box 4")

  if(Debug) print("Plot.BoxPlot 1")

  # Cap number of records
  if(length(YVar) > 0L) {
    SampleSize <- SampleSize / length(YVar)
  }
  if(dt[,.N] > SampleSize) {
    dt1 <- dt[order(runif(.N))][seq_len(SampleSize)]
  } else {
    dt1 <- data.table::copy(dt)
  }

  if(Debug) print("Box 5")

  if(length(YVar) > 0L && length(XVar) == 0L && length(GroupVar) > 0L) {
    XVar <- GroupVar; GroupVar <- NULL
    CoordFlip <- FALSE
  } else if(length(XVar) > 0L && length(YVar) == 0L && length(GroupVar) > 0L) {
    YVar <- XVar; XVar <- GroupVar; GroupVar <- NULL
    CoordFlip <- TRUE
  } else {
    CoordFlip <- FALSE
    if(length(XVar) > 0L && class(dt1[[XVar]])[1L] %in% c("numeric","integer")) {
      YVarTrans <- XVarTrans
      YVar <- XVar
      XVar <- NULL
    }
  }

  # Multiple YVars
  if(length(YVar) > 1L) {
    XVar <- NULL
    GroupVar <- NULL
    dt1[, temp__ := "a"]
    dt1 <- data.table::melt.data.table(data = dt1, id.vars = "temp__", measure.vars = YVar, variable.name = "Measures", value.name = "Values")
    dt1[, temp__ := NULL]
    XVar <- "Measures"
    YVar <- "Values"
  }

  if(Debug) print("Box 6")

  if(length(GroupVar) > 0L && FacetRows > 1L && FacetCols > 1L) {
    dt1 <- dt1[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,XVar,GroupVar)]
  }

  if(Debug) print("Box 7")

  # Transformation
  if(YVarTrans != "Identity") {
    for(ggss in YVar) {
      dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = ggss, Methods = YVarTrans)$Data
    }
  }

  if(Debug) print("Box 8")

  # Build Plot Based on Available Variables
  # Create logic checks to determine each case distinctly
  if(Debug) print("Plot.BoxPlot 2")
  X_and_Y_and_GroupVars <- length(XVar) > 0L && length(YVar) > 0L && length(GroupVar) > 0L
  X_and_Y <- length(XVar) > 0L && length(YVar) > 0L

  if(Debug) print("Box 9")

  # X,Y,GroupVar
  if(X_and_Y_and_GroupVars) {

    if(Debug) print("Box 10")

    if(Debug) print("Plot.Box Echarts")
    p1 <- echarts4r::e_charts_(
      data = dt1 |> dplyr::group_by(get(GroupVar[1L])),
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      emphasis = list(focus = "series"),
      width = Width,
      height = Height)

    if(Debug) print("Box 11")

    if(ShowLabels) {
      p1 <- echarts4r::e_boxplot_(e = p1, YVar, label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_boxplot_(e = p1, YVar)
    }

    if(Debug) print("Box 12")

    p1 <- echarts4r::e_visual_map_(e = p1, YVar, show = FALSE)
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }

    if(Debug) print("Box 13")

    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "item", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(Debug) print("Box 14")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(Debug) print("Box 15")

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(Debug) print("Box 16")

    if(CoordFlip) p1 <- echarts4r::e_flip_coords(e = p1)
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
    if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
    return(p1)
  }

  # X,Y
  if(X_and_Y) {

    if(Debug) print("Box 10.a")

    if(Debug) print("Plot.Box X_and_Y")
    if(Debug) print("Plot.Box Echarts")
    p1 <- echarts4r::e_charts_(
      dt1 |> dplyr::group_by(get(XVar)),
      x = YVar,
      darkMode = TRUE,
      dispose = TRUE,
      color = GroupVar,
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_boxplot_(e = p1, YVar, label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_boxplot_(e = p1, YVar)
    }

    p1 <- echarts4r::e_visual_map_(e = p1, YVar, show = FALSE)
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "item", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(CoordFlip) p1 <- echarts4r::e_flip_coords(e = p1)
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
    if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")



    # Return
    return(p1)
  }

  # Y Only
  if(length(YVar) > 0L) {

    if(Debug) print("Box 10.b")

    if(Debug) print("Plot.Box Y Only")

    if(Debug) print("Plot.Box Echarts")
    p1 <- echarts4r::e_charts_(
      dt1,
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)
    if(ShowLabels) {
      p1 <- echarts4r::e_boxplot_(e = p1, YVar, label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_boxplot_(e = p1, YVar)
    }
    p1 <- echarts4r::e_visual_map_(e = p1, YVar, show = FALSE)
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "item", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))

    return(p1)
  }

  # X Only
  if(length(XVar) > 0L) {

    if(Debug) print("Box 10.c")

    if(Debug) print("Plot.Box X Only")

    if(Debug) print("Plot.Box Echarts")
    p1 <- echarts4r::e_charts_(
      dt1,
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)
    if(ShowLabels) {
      p1 <- echarts4r::e_boxplot_(e = p1, XVar, label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_boxplot_(e = p1, XVar)
    }
    p1 <- echarts4r::e_visual_map_(e = p1, XVar, show = FALSE)
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "item", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))


    # Return
    return(p1)
  }
  return(NULL)
}

#' @title Plot.WordCloud
#'
#' @description WordCloud plots
#'
#' @family Standard Plots
#'
#' @param dt source data.table
#' @param YVar Y-Axis variable name
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title = "Density Plot"
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TextColor "white",
#' @param Debug Debugging purposes
#' @export
Plot.WordCloud <- function(dt = NULL,
                           YVar = NULL,
                           Height = NULL,
                           Width = NULL,
                           Title = "Word Cloud",
                           EchartsTheme = "macarons",
                           TextColor = "white",
                           title.fontSize = 22,
                           title.fontWeight = "bold",
                           title.textShadowColor = '#63aeff',
                           title.textShadowBlur = 3,
                           title.textShadowOffsetY = 1,
                           title.textShadowOffsetX = -1,
                           xaxis.fontSize = 14,
                           yaxis.fontSize = 14,
                           xaxis.rotate = 0,
                           yaxis.rotate = 0,
                           ContainLabel = TRUE,
                           Debug = FALSE) {

  if(EchartsTheme == 'auritus') {
    ColorVals <- c("#3e4359", "#c5a805", "#4d267e", "#22904f", "red")
  } else if(EchartsTheme == 'azul') {
    ColorVals <- c("#bfcca6", "#b07a9a", "#65deff", "#f73372", "#d08e1f")
  } else if(EchartsTheme == 'bee-inspired') {
    ColorVals <- c("#24243b", "#c2ba38", "#deeb25", "#ebc625", "#ffe700")
  } else if(EchartsTheme == 'blue') {
    ColorVals <- c("#2e69aa", "#99b8d9", "#3a84d4", "#1b67b9", "#046fe1")
  } else if(EchartsTheme == 'caravan') {
    ColorVals <- c("#18536d", "#d44545", "#eba565", "#e1c3a7", "#e1dda7")
  } else if(EchartsTheme == 'carp') {
    ColorVals <- c("#ff3300", "#fff0bb", "#679898", "#ff8870", "#4d3935")
  } else if(EchartsTheme == 'chalk') {
    ColorVals <- c("#e8c69e", "#54afec", "#d9dc89", "#f1a7d6", "#927294")
  } else if(EchartsTheme == 'cool') {
    ColorVals <- c("#20146a", "#591b89", "#911ea6", "#8081ba", "#2a74c4")
  } else if(EchartsTheme == 'dark-bold') {
    ColorVals <- c("#922e2e", "#d06363", "#d0a463", "#5c845e", "#63d0b9")
  } else if(EchartsTheme == 'dark') {
    ColorVals <- c("#e17d7d", "#c1ba54", "#66d5b0", "#b366d5", "#66a9d5")
  } else if(EchartsTheme == 'eduardo') {
    ColorVals <- c("#352a61", "#696284", "#c190ba", "#9e8a9b", "#615b60")
  } else if(EchartsTheme == 'essos') {
    ColorVals <- c("#753751", "#cfc995", "#c2b53c", "#d89c41")
  } else if(EchartsTheme == 'forest') {
    ColorVals <- c("#101010", "#bdb892", "#6c7955", "#3e6e86", "#37412e")
  } else if(EchartsTheme == 'fresh-cut') {
    ColorVals <- c("#74b936", "#76e314", "#cfbcb2", "#26609e", "#11b1cf")
  } else if(EchartsTheme == 'fruit') {
    ColorVals <- c("#dc965e", "#955828", "#c2b3a6", "#a16464", "#ae8c74")
  } else if(EchartsTheme == 'gray') {
    ColorVals <- c("#333333", "#696969", "#989898", "#bababa", "#e3e3e3")
  } else if(EchartsTheme == 'green') {
    ColorVals <- c("#2c5e25", "#387830", "#56a14d", "#7cbe74", "#b5e3af")
  } else if(EchartsTheme == 'halloween') {
    ColorVals <- c("#d1d134", "#d1953c", "#cc735d", "#7a5dcc", "#564f6a")
  } else if(EchartsTheme == 'helianthus') {
    ColorVals <- c("#6235e1", "#e16235", "#e1c135", "#c46aa5", "#5bcf3e")
  } else if(EchartsTheme == 'infographic') {
    ColorVals <- c("#d5cb2b", "#b4e771", "#cc4d3d", "#e78971", "#82b053")
  } else if(EchartsTheme == 'inspired') {
    ColorVals <- c("#8e1212", "#0f6310", "#d39f03", "#ff0000", "#265d82")
  } else if(EchartsTheme == 'jazz') {
    ColorVals <- c("#5e4832", "#000000", "#265057", "#d5dcdd")
  } else if(EchartsTheme == 'london') {
    ColorVals <- c("#881010", "#b8d1d4", "#227e89", "#041137", "#1c86c4")
  } else if(EchartsTheme == 'macarons') {
    ColorVals <- c("#6382cf", "#8776b9", "#318c9d", "#6d5739", "#7f7f98")
  } else if(EchartsTheme == 'macarons2') {
    ColorVals <- c("#6d6ddb", "#d45315", "#6e9fe4", "#b9bc89", "#d37c7c")
  } else if(EchartsTheme == 'mint') {
    ColorVals <- c("#c3ebd6", "#859d90", "#6dbaba", "#6dba9b", "#62d17f")
  } else if(EchartsTheme == 'purple-passion') {
    ColorVals <- c("#9385ba", "#779fbe", "#b86aac", "#5d9dc8", "#5f3a89")
  } else if(EchartsTheme == 'red-velvet') {
    ColorVals <- c("#6f4c41", "#db8469", "#f13d67", "#5e1d2c", "#ff00a9")
  } else if(EchartsTheme == 'red') {
    ColorVals <- c("#b4342a", "#8a4d49", "#c08480", "#df745a", "#cca69d")
  } else if(EchartsTheme == 'roma') {
    ColorVals <- c("#a580e9", "#d56426", "#cd1450", "#8bbec0", "#91836b")
  } else if(EchartsTheme == 'royal') {
    ColorVals <- c("#a06156", "#756054", "#5fac21", "#34708a", "#692525")
  } else if(EchartsTheme == 'sakura') {
    ColorVals <- c("#d75869", "#cb979e", "#b12a3d", "#adabc7", "#d0a79b")
  } else if(EchartsTheme == 'shine') {
    ColorVals <- c("#3d5995", "#296537", "#3390f7", "#b81717", "#50868c")
  } else if(EchartsTheme == 'tech-blue') {
    ColorVals <- c("#356499", "#4e487e", "#524f4b", "#b9addc", "#1c70d8")
  } else if(EchartsTheme == 'vintage') {
    ColorVals <- c("#a47e5f", "#638176", "#a46969", "#5d3a3a", "#4f8090")
  } else if(EchartsTheme == 'walden') {
    ColorVals <- c("#3b96c4", "#8babba", "#a5d9a2", "#535d84", "#7f79ad")
  } else if(EchartsTheme == 'wef') {
    ColorVals <- c("#5981d5", "#3268d9", "#9d938a", "#1f457c", "#524e48")
  } else if(EchartsTheme == 'weforum') {
    ColorVals <- c("#8a1b6f", "#4d2876", "#d5bf24", "#2792aa", "#a27322")
  } else if(EchartsTheme == 'westeros') {
    ColorVals <- c("#4b4d66", "#a681b0", "#8acccf", "#41a7cf")
  } else if(EchartsTheme == 'wonderland') {
    ColorVals <- c("#629291", "#3ec5c2", "#cf95ad", "#cd7097")
  }

  # Cap number of records
  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # Convert factor to character
  if(length(YVar) > 0L && class(dt[[YVar]])[1L] == "factor") {
    dt[, eval(YVar) := as.character(get(YVar))]
  }

  # Copy Data
  dt1 <- data.table::copy(dt)

  # Define Plotting Variable
  if(length(YVar) == 0L) return(NULL)

  # Data YVar <- "Comment"
  # dt <- AutoNLP::FakeDataGenerator(N = 1000, AddComment = TRUE)
  dt1 <- quanteda::tokens(dt[[YVar]], remove_punct = TRUE)
  dt2 <- quanteda::dfm(dt1)
  dt3 <- data.table::setDT(quanteda.textstats::textstat_frequency(dt2))
  dt4 <- dt3[, .SD, .SDcols = c("feature", "frequency")]
  data.table::setnames(dt4, c("feature", "frequency"),c("term", "freq"))

  # Create base plot object
  if(Debug) print('Create Plot with only data')
  dt5 <- echarts4r::e_color_range_(
    data = dt4,
    input = "freq",
    output = "Color",
    colors = ColorVals)
  p1 <- echarts4r::e_charts(data = dt5)
  p1 <- echarts4r::e_cloud_(e = p1, "term", "freq", "Color", shape = "circle", sizeRange = c(20, 42))
  p1 <- echarts4r::e_title(
    p1, Title,
    textStyle = list(
      color = TextColor,
      fontWeight = title.fontWeight,
      overflow = "truncate",
      ellipsis = '...',
      fontSize = title.fontSize,
      textShadowColor = title.textShadowColor,
      textShadowBlur = title.textShadowBlur,
      textShadowOffsetY = title.textShadowOffsetY,
      textShadowOffsetX = title.textShadowOffsetX))
  p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
  return(p1)
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# > Aggreagated Plot Functions                                                ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Plot.Radar
#'
#' @author Adrian Antico
#' @family Standard Plots
#'
#' @param dt source data.table
#' @param PreAgg logical
#' @param AggMethod character
#' @param YVar Y-Axis variable name. You can supply multiple YVars
#' @param GroupVar One Grouping Variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title "Title"
#' @param ShowLabels character
#' @param EchartsTheme Provide an "Echarts" theme
#' @param ShowSymbol = FALSE
#' @param BackGroundColor color outside of plot window. Rcolors and hex
#' @param TextColor "Not Implemented"
#' @param DarkMode FALSE
#' @param Debug Debugging purposes
#'
#' @export
Plot.Radar <- function(dt = NULL,
                       AggMethod = "mean",
                       PreAgg = TRUE,
                       YVar = NULL,
                       GroupVar = NULL,
                       YVarTrans = "Identity",
                       Height = NULL,
                       Width = NULL,
                       Title = 'Radar Plot',
                       ShowLabels = FALSE,
                       EchartsTheme = "macarons",
                       ShowSymbol = FALSE,
                       TextColor = "white",
                       title.fontSize = 22,
                       title.fontWeight = "bold",
                       title.textShadowColor = '#63aeff',
                       title.textShadowBlur = 3,
                       title.textShadowOffsetY = 1,
                       title.textShadowOffsetX = -1,
                       ContainLabel = TRUE,
                       DarkMode = FALSE,
                       Debug = FALSE) {

  # print(dt)

  # dt = data.table::fread(file.choose())
  # AggMethod = "mean"
  # PreAgg = FALSE
  # YVar = c("Daily Margin", "Daily Liters")
  # GroupVar = "Brand"
  # YVarTrans = "Identity"
  # Height = "600px"
  # Width = "300px"
  # Title = 'Radar Plot'
  # ShowLabels = FALSE
  # EchartsTheme = "dark"
  # ShowSymbol = FALSE
  # TextColor = "white"
  # title.fontSize = 22
  # title.fontWeight = "bold"
  # title.textShadowColor = '#63aeff'
  # title.textShadowBlur = 3
  # title.textShadowOffsetY = 1
  # title.textShadowOffsetX = -1
  # ContainLabel = TRUE
  # DarkMode = FALSE
  # Debug = FALSE

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # Convert factor to character
  if(length(GroupVar) > 0L && class(dt[[GroupVar]])[1L] == "factor") {
    dt[, eval(GroupVar) := as.character(get(GroupVar))]
  }

  # If User Supplies more than 1 YVar, then structure data to be long instead of wide
  dt1 <- data.table::copy(dt)

  # Subset columns
  dt1 <- dt1[, .SD, .SDcols = c(YVar, GroupVar)]

  # Minimize data before moving on
  if(!PreAgg) {

    # Define Aggregation function
    if(Debug) print("Plot.Radar # Define Aggregation function")
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

    # Aggregate data
    dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(GroupVar[1L])]
    data.table::setorderv(x = dt1, cols = c(GroupVar[1L]), 1L)
  }

  # Transformation
  if(YVarTrans != "Identity") {
    for(yvar in YVar) {
      dt1 <- AutoPlots:::AutoTransformationCreate(data = dt1, ColumnNames = yvar, Methods = YVarTrans)$Data
    }
  }

  # Build base plot depending on GroupVar availability
  p1 <- echarts4r::e_charts_(
    data = dt1,
    x = GroupVar,
    darkMode = TRUE,
    emphasis = list(focus = "series"),
    dispose = TRUE, width = Width, height = Height)

  # Make sure the variable with the largest value goes first
  # Otherwise the radar plot will size to a smaller variable and look stupid
  mv <- c(rep(0, length(YVar)))
  for(i in seq_along(YVar)) {
    mv[i] <- dt1[, max(get(YVar[i]), na.rm = TRUE)]
  }
  YVarMod <- YVar[which(mv == max(mv, na.rm = TRUE))]
  YVarMod <- c(YVarMod, YVar[!YVar %in% YVarMod])
  mv <- max(mv, na.rm = TRUE)
  for(yvar in YVarMod) {
    p1 <- echarts4r::e_radar_(e = p1, serie = yvar, max = mv, name = yvar)
  }

  if(Debug) print("Plot.Radar() Build Echarts 5")
  p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
  if(Debug) print("Plot.Radar() Build Echarts 6")
  p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
  p1 <- echarts4r::e_tooltip(e = p1, trigger = "item", backgroundColor = "aliceblue")
  p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
  p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

  p1 <- echarts4r::e_title(
    p1, Title,
    textStyle = list(
      color = TextColor,
      fontWeight = title.fontWeight,
      overflow = "truncate",
      ellipsis = '...',
      fontSize = title.fontSize,
      textShadowColor = title.textShadowColor,
      textShadowBlur = title.textShadowBlur,
      textShadowOffsetY = title.textShadowOffsetY,
      textShadowOffsetX = title.textShadowOffsetX))
  if(Debug) print("Plot.Radar() Build Echarts 8")
  p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
  return(p1)
}

#' @title Plot.Line
#'
#' @description This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#'
#' @author Adrian Antico
#' @family Standard Plots
#'
#' @param dt source data.table
#' @param PreAgg logical
#' @param AggMethod character
#' @param YVar Y-Axis variable name. You can supply multiple YVars
#' @param DualYVar Secondary Y-Axis variables. Leave NULL for no secondary axis. Only one variable is allowed and when this is set only one YVar is allowed. An error will be thrown if those conditions are not met
#' @param XVar X-Axis variable name
#' @param GroupVar One Grouping Variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title "Title"
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme Provide an "Echarts" theme
#' @param TimeLine Logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param Area logical
#' @param Alpha 0 to 1 for setting transparency
#' @param Smooth = TRUE
#' @param ShowSymbol = FALSE
#' @param BackGroundColor color outside of plot window. Rcolors and hex
#' @param TextColor "Not Implemented"
#' @param DarkMode FALSE
#' @param Debug Debugging purposes
#'
#' @examples
#' \dontrun{
#' # Create fake data
#' data <- AutoPlots::FakeDataGenerator(N = 1000)
#'
#' # Build Line plot
#' AutoPlots::Plot.Line(
#'   dt = data,
#'   PreAgg = FALSE,
#'   AggMethod = "mean",
#'   XVar = "DateTime",
#'   YVar = "Independent_Variable3",
#'   YVarTrans = "LogPlus1",
#'   DualYVar = "Independent_Variable6",
#'   DualYVarTrans = "LogPlus1",
#'   GroupVar = NULL,
#'   EchartsTheme = "macarons")
#'
#' # Step through function
#' dt = data
#' PreAgg = FALSE
#' AggMethod = "mean"
#' XVar = "DateTime"
#' YVar = "Independent_Variable1"
#' YVarTrans = "Identity"
#' DualYVar = "Independent_Variable4"
#' DualYVarTrans = "Identity"
#' XVarTrans = "Identity"
#' GroupVar = "Factor_1"
#' FacetRows = 1
#' FacetCols = 1
#' FacetLevels = NULL
#' Height = NULL
#' Width = NULL
#' Title = 'Line Plot'
#' ShowLabels = FALSE
#' Title.YAxis = NULL
#' Title.XAxis = NULL
#' EchartsTheme = "macarons"
#' X_Scroll = FALSE
#' Y_Scroll = FALSE
#' TimeLine = TRUE
#' Area = FALSE
#' Alpha = 0.50
#' Smooth = TRUE
#' ShowSymbol = FALSE
#' TextColor =        "white"
#' title.fontSize = 22
#' title.fontWeight = "bold"
#' title.textShadowColor = '#63aeff'
#' title.textShadowBlur = 3
#' title.textShadowOffsetY = 1
#' title.textShadowOffsetX = -1
#' xaxis.fontSize = 14
#' yaxis.fontSize = 14
#' xaxis.rotate = 0
#' yaxis.rotate = 0
#' ContainLabel = TRUE
#' DarkMode = FALSE
#' Debug = FALSE
#' }
#'
#' @export
Plot.Line <- function(dt = NULL,
                      AggMethod = "mean",
                      PreAgg = TRUE,
                      XVar = NULL,
                      YVar = NULL,
                      DualYVar = NULL,
                      GroupVar = NULL,
                      YVarTrans = "Identity",
                      DualYVarTrans = "Identity",
                      XVarTrans = "Identity",
                      FacetRows = 1,
                      FacetCols = 1,
                      FacetLevels = NULL,
                      Height = NULL,
                      Width = NULL,
                      Title = 'Line Plot',
                      ShowLabels = FALSE,
                      Title.YAxis = NULL,
                      Title.XAxis = NULL,
                      EchartsTheme = "macarons",
                      X_Scroll = FALSE,
                      Y_Scroll = FALSE,
                      TimeLine = TRUE,
                      Area = FALSE,
                      Alpha = 0.50,
                      Smooth = TRUE,
                      ShowSymbol = FALSE,
                      TextColor =        "white",
                      title.fontSize = 22,
                      title.fontWeight = "bold", # normal
                      title.textShadowColor = '#63aeff',
                      title.textShadowBlur = 3,
                      title.textShadowOffsetY = 1,
                      title.textShadowOffsetX = -1,
                      xaxis.fontSize = 14,
                      yaxis.fontSize = 14,
                      xaxis.rotate = 0,
                      yaxis.rotate = 0,
                      ContainLabel = TRUE,
                      DarkMode = FALSE,
                      Debug = FALSE) {

  if(TimeLine && length(FacetLevels) == 0L) X_Scroll <- FALSE
  if(length(GroupVar) == 0L) TimeLine <- FALSE

  # Correct args
  if(length(GroupVar) > 0L && length(XVar) == 0L) {
    XVar <- GroupVar
    GroupVar <- NULL
  }

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # Convert factor to character
  if(length(GroupVar) > 0L && class(dt[[GroupVar]])[1L] == "factor") {
    dt[, eval(GroupVar) := as.character(get(GroupVar))]
  }

  # If length(YVar) > 1 and a DualYVar is supplied, dual axis take precedence
  # Throw an error instead of trimming YVar to only the first value
  if(length(YVar) > 1L && length(DualYVar) > 0) stop("When DualYVar is utilized only one DualYVar is allowed and only one YVar is allowed")
  if(length(GroupVar) > 0L && length(DualYVar) > 0) stop("When DualYVar is utilized a GroupVar is not allowed")

  # If User Supplies more than 1 YVar, then structure data to be long instead of wide
  if(length(YVar) > 1L) {
    if(length(GroupVar) > 0L) {
      dt1 <- data.table::melt.data.table(data = dt, id.vars = c(XVar,GroupVar), measure.vars = YVar, variable.name = "Measures", value.name = "Values")
      dt1[, GroupVars := paste0(Measures, GroupVar)]
      dt1[, Measures := NULL]
      dt1[, eval(GroupVar) := NULL]
      GroupVar <- "GroupVars"
      YVar <- "Values"
    } else {
      dt1 <- data.table::melt.data.table(data = dt, id.vars = XVar, measure.vars = YVar, variable.name = "Measures", value.name = "Values")
      GroupVar <- "Measures"
      YVar <- "Values"
    }
  } else {
    dt1 <- data.table::copy(dt)
  }

  # Subset columns
  Ncols <- ncol(dt1)
  if(Ncols > 2L && length(GroupVar) == 0L) {
    dt1 <- dt1[, .SD, .SDcols = c(YVar, XVar, DualYVar)]
  } else if(length(GroupVar) > 0L) {
    dt1 <- dt1[, .SD, .SDcols = c(YVar, XVar, DualYVar, GroupVar[1L])]
    if(length(FacetLevels) > 0) {
      dt1 <- dt1[get(GroupVar[1L]) %in% eval(FacetLevels)]
    }
  }

  # Minimize data before moving on
  if(!PreAgg) {

    # Define Aggregation function
    if(Debug) print("Plot.Calibration.Line # Define Aggregation function")
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

    # Aggregate data
    if(length(GroupVar) > 0L) {
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar,GroupVar[1L])]
      data.table::setorderv(x = dt1, cols = c(GroupVar[1L], XVar), c(1L,1L))
    } else {
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar)]
      data.table::setorderv(x = dt1, cols = XVar, 1L)
    }
  }

  # Transformation
  if(YVarTrans != "Identity") {
    dt1 <- AutoPlots:::AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data
  }
  if(length(DualYVar > 0L) && DualYVarTrans != "Identity") {
    dt1 <- AutoPlots:::AutoTransformationCreate(data = dt1, ColumnNames = DualYVar, Methods = DualYVarTrans)$Data
  }

  # Group Variable Case
  if(length(GroupVar) > 0L) {

    # Prepare Data
    if(Debug) print("Plot.Line() Build 1")
    gv <- GroupVar[1L]
    if(PreAgg) data.table::setorderv(x = dt1, cols = c(GroupVar[1L], XVar), c(1L,1L))

    cxv <- class(dt1[[XVar]])[1L]
    if(cxv %in% "IDate") {
      dt1[, eval(XVar) := as.Date(get(XVar))]
    } else if(cxv %in% "IDateTime") {
      dt1[, eval(XVar) := as.POSIXct(get(XVar))]
    }

    # Build base plot depending on GroupVar availability
    p1 <- echarts4r::e_charts_(
      data = dt1 |> dplyr::group_by(get(gv)),
      x = XVar,
      darkMode = TRUE,
      emphasis = list(focus = "series"),
      timeline = TimeLine, dispose = TRUE, width = Width, height = Height)

    # Finalize Plot Build
    if(ShowLabels) {
      p1 <- echarts4r::e_line_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol, label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_line_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol)
    }

    if(Debug) print("Plot.Line() Build Echarts 4 1")
    if(FacetRows == 1L && FacetCols == 1L) {
      if(Debug) print("Plot.Line() Build Echarts 4 2")
      if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    if(Debug) print("Plot.Line() Build Echarts 5")
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    if(Debug) print("Plot.Line() Build Echarts 6")
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_brush(e = p1)
    if(Debug) print("Plot.Line() Build Echarts 6")
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
    if(Debug) print("Plot.Line() Build Echarts 8")
    if((FacetRows > 1L || FacetCols > 1) && length(FacetLevels) > 0L) {
      if(Debug) print("Plot.Line() Build Echarts 8 2")
      p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      if(Debug) print("Plot.Line() Build Echarts 8 3")
    } else {
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    }

  } else {

    # Plot
    data.table::setorderv(x = dt1, cols = XVar, 1L)
    cxv <- class(dt1[[XVar]])[1L]
    if(cxv %in% "IDate") {
      dt1[, eval(XVar) := as.Date(get(XVar))]
    } else if(cxv %in% "IDateTime") {
      dt1[, eval(XVar) := as.POSIXct(get(XVar))]
    }

    # Build base plot depending on GroupVar availability
    if(Debug) print("Plot.Line no group Echarts")
    p1 <- echarts4r::e_charts_(
      data = dt1,
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_line_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol, label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_line_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol)
    }

    # DualYVar
    if(length(DualYVar) > 0L) {
      if(ShowLabels) {
        p1 <- echarts4r::e_line_(e = p1, serie = DualYVar, smooth = Smooth, showSymbol = ShowSymbol, label = list(show = TRUE), x_index = 1, y_index = 1)
      } else {
        p1 <- echarts4r::e_line_(e = p1, serie = DualYVar, smooth = Smooth, showSymbol = ShowSymbol, x_index = 1, y_index = 1)
      }
    }

    # Finalize Plot Build
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
  }
  return(p1)
}

#' @title Plot.Area
#'
#' @description This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#'
#' @author Adrian Antico
#' @family Standard Plots
#'
#' @param dt source data.table
#' @param PreAgg logical
#' @param AggMethod character
#' @param YVar Y-Axis variable name. You can supply multiple YVars
#' @param DualYVar Secondary Y-Axis variables. Leave NULL for no secondary axis. Only one variable is allowed and when this is set only one YVar is allowed. An error will be thrown if those conditions are not met
#' @param XVar X-Axis variable name
#' @param GroupVar One Grouping Variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param DualYVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title "Title"
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme Provide an "Echarts" theme
#' @param TimeLine Logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param Area logical
#' @param Alpha 0 to 1 for setting transparency
#' @param Smooth = TRUE
#' @param ShowSymbol = FALSE
#' @param TextColor "Not Implemented"
#' @param Debug Debugging purposes
#' @export
Plot.Area <- function(dt = NULL,
                      AggMethod = "mean",
                      PreAgg = TRUE,
                      XVar = NULL,
                      YVar = NULL,
                      DualYVar = NULL,
                      GroupVar = NULL,
                      YVarTrans = "Identity",
                      DualYVarTrans = "Identity",
                      XVarTrans = "Identity",
                      FacetRows = 1,
                      FacetCols = 1,
                      FacetLevels = NULL,
                      Height = NULL,
                      Width = NULL,
                      Title = 'Line Plot',
                      ShowLabels = FALSE,
                      Title.YAxis = NULL,
                      Title.XAxis = NULL,
                      EchartsTheme = "macarons",
                      X_Scroll = FALSE,
                      Y_Scroll = FALSE,
                      TimeLine = TRUE,
                      Alpha = 0.50,
                      Smooth = TRUE,
                      ShowSymbol = FALSE,
                      TextColor =        "white",
                      title.fontSize = 22,
                      title.fontWeight = "bold", # normal
                      title.textShadowColor = '#63aeff',
                      title.textShadowBlur = 3,
                      title.textShadowOffsetY = 1,
                      title.textShadowOffsetX = -1,
                      xaxis.fontSize = 14,
                      yaxis.fontSize = 14,
                      xaxis.rotate = 0,
                      yaxis.rotate = 0,
                      ContainLabel = TRUE,
                      Debug = FALSE) {

  if(length(GroupVar) == 0L) TimeLine <- FALSE
  if(TimeLine && length(FacetLevels) > 0) X_Scroll <- FALSE

  # Correct args
  if(length(GroupVar) > 0L && length(XVar) == 0L) {
    XVar <- GroupVar
    GroupVar <- NULL
  }

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # Convert factor to character
  if(length(GroupVar) > 0L && class(dt[[GroupVar]])[1L] == "factor") {
    dt[, eval(GroupVar) := as.character(get(GroupVar))]
  }

  # If length(YVar) > 1 and a DualYVar is supplied, dual axis take precedence
  # Throw an error instead of trimming YVar to only the first value
  if(length(YVar) > 1L && length(DualYVar) > 0) stop("When DualYVar is utilized only one DualYVar is allowed and only one YVar is allowed")
  if(length(GroupVar) > 0L && length(DualYVar) > 0) stop("When DualYVar is utilized a GroupVar is not allowed")

  # If User Supplies more than 1 YVar, then structure data to be long instead of wide
  if(length(YVar) > 1L) {
    if(length(GroupVar) > 0L) {
      dt1 <- data.table::melt.data.table(data = dt, id.vars = c(XVar,GroupVar), measure.vars = YVar, variable.name = "Measures", value.name = "Values")
      dt1[, GroupVars := paste0(Measures, GroupVar)]
      dt1[, Measures := NULL]
      dt1[, eval(GroupVar) := NULL]
      GroupVar <- "GroupVars"
      YVar <- "Values"
    } else {
      dt1 <- data.table::melt.data.table(data = dt, id.vars = XVar, measure.vars = YVar, variable.name = "Measures", value.name = "Values")
      GroupVar <- "Measures"
      YVar <- "Values"
    }
  } else {
    dt1 <- data.table::copy(dt)
  }

  # Subset columns
  Ncols <- ncol(dt1)
  if(Ncols > 2L && length(GroupVar) == 0L) {
    dt1 <- dt1[, .SD, .SDcols = c(YVar, XVar, DualYVar)]
  } else if(length(GroupVar) > 0L) {
    dt1 <- dt1[, .SD, .SDcols = c(YVar, XVar, DualYVar, GroupVar[1L])]
    if(length(FacetLevels) > 0) {
      dt1 <- dt1[get(GroupVar[1L]) %in% eval(FacetLevels)]
    }
  }

  # Minimize data before moving on
  if(!PreAgg) {

    # Define Aggregation function
    if(Debug) print("Plot.Calibration.Line # Define Aggregation function")
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

    # Aggregate data
    if(length(GroupVar) > 0L) {
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar,GroupVar[1L])]
      data.table::setorderv(x = dt1, cols = c(GroupVar[1L], XVar), c(1L,1L))
    } else {
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar)]
      data.table::setorderv(x = dt1, cols = XVar, 1L)
    }
  }

  # Transformation
  if(YVarTrans != "Identity") {
    dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data
  }
  if(length(DualYVar > 0L) && DualYVarTrans != "Identity") {
    dt1 <- AutoPlots:::AutoTransformationCreate(data = dt1, ColumnNames = DualYVar, Methods = DualYVarTrans)$Data
  }

  # Group Variable Case
  if(length(GroupVar) > 0L) {

    # Prepare Data
    if(Debug) print("Plot.Line() Build 1")
    gv <- GroupVar[1L]
    if(PreAgg) data.table::setorderv(x = dt1, cols = c(GroupVar[1L], XVar), c(1L,1L))

    cxv <- class(dt1[[XVar]])[1L]
    if(cxv %in% "IDate") {
      dt1[, eval(XVar) := as.Date(get(XVar))]
    } else if(cxv %in% "IDateTime") {
      dt1[, eval(XVar) := as.POSIXct(get(XVar))]
    }

    # Plot
    if(Debug) print("Plot.Line() Build Echarts 1")

    # Build base plot depending on GroupVar availability
    if(Debug) print(paste0("Plot.Line TimeLine = ", TimeLine))
    p1 <- echarts4r::e_charts_(
      data = dt1 |> dplyr::group_by(get(gv)),
      x = XVar,
      darkMode = TRUE,
      emphasis = list(focus = "series"),
      timeline = TimeLine,
      dispose = TRUE,
      width = Width,
      height = Height)

    # Finalize Plot Build
    if(Debug) print("Plot.Line() Build Echarts 4")
    if(ShowLabels) {
      p1 <- echarts4r::e_area_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol, label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_area_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol)
    }
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
    if((FacetRows > 1L || FacetCols > 1) && length(FacetLevels) > 0L) {
      p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    } else {
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    }

  } else {

    # Plot
    data.table::setorderv(x = dt1, cols = XVar, 1L)
    cxv <- class(dt1[[XVar]])[1L]
    if(cxv %in% "IDate") {
      dt1[, eval(XVar) := as.Date(get(XVar))]
    } else if(cxv %in% "IDateTime") {
      dt1[, eval(XVar) := as.POSIXct(get(XVar))]
    }

    # Build base plot depending on GroupVar availability
    if(Debug) print("Plot.Line no group Echarts")
    p1 <- echarts4r::e_charts_(
      data = dt1,
      x = XVar,
      darkMode = TRUE,
      dispose = TRUE,
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_area_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol, label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_area_(e = p1, serie = YVar, smooth = Smooth, showSymbol = ShowSymbol)
    }

    if(length(DualYVar) > 0L) {
      if(ShowLabels) {
        p1 <- echarts4r::e_area_(e = p1, serie = DualYVar, smooth = Smooth, showSymbol = ShowSymbol, label = list(show = TRUE), x_index = 1, y_index = 1)
      } else {
        p1 <- echarts4r::e_area_(e = p1, serie = DualYVar, smooth = Smooth, showSymbol = ShowSymbol, x_index = 1, y_index = 1)
      }
    }

    # Finalize Plot Build
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
  }
  return(p1)
}

#' @title Plot.Step
#'
#' @description This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#'
#' @author Adrian Antico
#' @family Standard Plots
#'
#' @param dt source data.table
#' @param PreAgg logical
#' @param AggMethod character
#' @param YVar Y-Axis variable name. You can supply multiple YVars
#' @param DualYVar Secondary Y-Axis variables. Leave NULL for no secondary axis. Only one variable is allowed and when this is set only one YVar is allowed. An error will be thrown if those conditions are not met
#' @param XVar X-Axis variable name
#' @param GroupVar One Grouping Variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param DualYVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title "Title"
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme Provide an "Echarts" theme
#' @param TimeLine Logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param ShowSymbol = FALSE
#' @param TextColor "Not Implemented"
#' @param Debug Debugging purposes
#' @export
Plot.Step <- function(dt = NULL,
                      AggMethod = "mean",
                      PreAgg = TRUE,
                      XVar = NULL,
                      YVar = NULL,
                      DualYVar = NULL,
                      GroupVar = NULL,
                      YVarTrans = "Identity",
                      DualYVarTrans = "Identity",
                      XVarTrans = "Identity",
                      FacetRows = 1,
                      FacetCols = 1,
                      FacetLevels = NULL,
                      Height = NULL,
                      Width = NULL,
                      Title = 'Line Plot',
                      ShowLabels = FALSE,
                      Title.YAxis = NULL,
                      Title.XAxis = NULL,
                      EchartsTheme = "macarons",
                      X_Scroll = FALSE,
                      Y_Scroll = FALSE,
                      TimeLine = TRUE,
                      ShowSymbol = FALSE,
                      TextColor =        "white",
                      title.fontSize = 22,
                      title.fontWeight = "bold", # normal
                      title.textShadowColor = '#63aeff',
                      title.textShadowBlur = 3,
                      title.textShadowOffsetY = 1,
                      title.textShadowOffsetX = -1,
                      xaxis.fontSize = 14,
                      yaxis.fontSize = 14,
                      xaxis.rotate = 0,
                      yaxis.rotate = 0,
                      ContainLabel = TRUE,
                      Debug = FALSE) {

  if(length(GroupVar) == 0L) TimeLine <- FALSE
  if(TimeLine && length(FacetLevels) > 0) X_Scroll <- FALSE

  # Correct args
  if(length(GroupVar) > 0L && length(XVar) == 0L) {
    XVar <- GroupVar
    GroupVar <- NULL
  }

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # Convert factor to character
  if(length(GroupVar) > 0L && class(dt[[GroupVar]])[1L] == "factor") {
    dt[, eval(GroupVar) := as.character(get(GroupVar))]
  }

  # If length(YVar) > 1 and a DualYVar is supplied, dual axis take precedence
  # Throw an error instead of trimming YVar to only the first value
  if(length(YVar) > 1L && length(DualYVar) > 0) stop("When DualYVar is utilized only one DualYVar is allowed and only one YVar is allowed")
  if(length(GroupVar) > 0L && length(DualYVar) > 0) stop("When DualYVar is utilized a GroupVar is not allowed")

  # If User Supplies more than 1 YVar, then structure data to be long instead of wide
  if(length(YVar) > 1L) {
    if(length(GroupVar) > 0L) {
      dt1 <- data.table::melt.data.table(data = dt, id.vars = c(XVar,GroupVar), measure.vars = YVar, variable.name = "Measures", value.name = "Values")
      dt1[, GroupVars := paste0(Measures, GroupVar)]
      dt1[, Measures := NULL]
      dt1[, eval(GroupVar) := NULL]
      GroupVar <- "GroupVars"
      YVar <- "Values"
    } else {
      dt1 <- data.table::melt.data.table(data = dt, id.vars = XVar, measure.vars = YVar, variable.name = "Measures", value.name = "Values")
      GroupVar <- "Measures"
      YVar <- "Values"
    }
  } else {
    dt1 <- data.table::copy(dt)
  }

  # Subset columns
  Ncols <- ncol(dt1)
  if(Ncols > 2L && length(GroupVar) == 0L) {
    dt1 <- dt1[, .SD, .SDcols = c(YVar, XVar, DualYVar)]
  } else if(length(GroupVar) > 0L) {
    dt1 <- dt1[, .SD, .SDcols = c(YVar, XVar, DualYVar, GroupVar[1L])]
    if(length(FacetLevels) > 0) {
      dt1 <- dt1[get(GroupVar[1L]) %in% eval(FacetLevels)]
    }
  }

  # Minimize data before moving on
  if(!PreAgg) {

    # Define Aggregation function
    if(Debug) print("Plot.Calibration.Line # Define Aggregation function")
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

    # Aggregate data
    if(length(GroupVar) > 0L) {
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar,GroupVar[1L])]
      data.table::setorderv(x = dt1, cols = c(GroupVar[1L], XVar), c(1L,1L))
    } else {
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar)]
      data.table::setorderv(x = dt1, cols = XVar, 1L)
    }
  }

  # Transformation
  if(YVarTrans != "Identity") {
    dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data
  }
  if(length(DualYVar > 0L) && DualYVarTrans != "Identity") {
    dt1 <- AutoPlots:::AutoTransformationCreate(data = dt1, ColumnNames = DualYVar, Methods = DualYVarTrans)$Data
  }

  # Group Variable Case
  if(length(GroupVar) > 0L) {

    # Prepare Data
    if(Debug) print("Plot.Line() Build 1")
    gv <- GroupVar[1L]
    if(PreAgg) data.table::setorderv(x = dt1, cols = c(GroupVar[1L], XVar), c(1L,1L))

    cxv <- class(dt1[[XVar]])[1L]
    if(cxv %in% "IDate") {
      dt1[, eval(XVar) := as.Date(get(XVar))]
    } else if(cxv %in% "IDateTime") {
      dt1[, eval(XVar) := as.POSIXct(get(XVar))]
    }

    # Plot
    if(Debug) print("Plot.Line() Build Echarts 1")

    # Build base plot depending on GroupVar availability
    if(Debug) print(paste0("Plot.Line TimeLine = ", TimeLine))
    p1 <- echarts4r::e_charts_(
      data = dt1 |> dplyr::group_by(get(gv)),
      x = XVar,
      timeline = TimeLine,
      darkMode = TRUE,
      emphasis = list(focus = "series"),
      dispose = TRUE,
      width = Width,
      height = Height)

    # Finalize Plot Build
    if(Debug) print("Plot.Line() Build Echarts 4")
    if(ShowLabels) {
      p1 <- echarts4r::e_step_(e = p1, serie = YVar, showSymbol = ShowSymbol, label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_step_(e = p1, serie = YVar, showSymbol = ShowSymbol)
    }
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
    if((FacetRows > 1L || FacetCols > 1) && length(FacetLevels) > 0L) {
      p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    } else {
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    }

  } else {

    # Plot
    data.table::setorderv(x = dt1, cols = XVar, 1L)
    cxv <- class(dt1[[XVar]])[1L]
    if(cxv %in% "IDate") {
      dt1[, eval(XVar) := as.Date(get(XVar))]
    } else if(cxv %in% "IDateTime") {
      dt1[, eval(XVar) := as.POSIXct(get(XVar))]
    }

    # Build base plot depending on GroupVar availability
    if(Debug) print("Plot.Line no group Echarts")
    p1 <- echarts4r::e_charts_(
      data = dt1,
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_step_(e = p1, serie = YVar, showSymbol = ShowSymbol, label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_step_(e = p1, serie = YVar, showSymbol = ShowSymbol)
    }

    if(length(DualYVar) > 0L) {
      if(ShowLabels) {
        p1 <- echarts4r::e_step_(e = p1, serie = DualYVar, showSymbol = ShowSymbol, label = list(show = TRUE), x_index = 1, y_index = 1)
      } else {
        p1 <- echarts4r::e_step_(e = p1, serie = DualYVar, showSymbol = ShowSymbol, x_index = 1, y_index = 1)
      }
    }

    # Finalize Plot Build
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))

  }
  return(p1)
}

#' @title Plot.River
#'
#' @description This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#'
#' @author Adrian Antico
#' @family Standard Plots
#'
#' @param dt source data.table
#' @param PreAgg logical
#' @param AggMethod character
#' @param YVar Y-Axis variable name. You can supply multiple YVars
#' @param XVar X-Axis variable name
#' @param GroupVar One Grouping Variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title "Title"
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme Provide an "Echarts" theme
#' @param TimeLine Logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param ShowSymbol = FALSE
#' @param ZeroLineColor color
#' @param ZeroLineWidth 1
#' @param BackGroundColor color outside of plot window. Rcolors and hex
#' @param ChartColor color
#' @param FillColor color
#' @param FillColorReverse character
#' @param TextColor "Not Implemented"
#' @param Debug Debugging purposes
#' @export
Plot.River <- function(dt = NULL,
                       AggMethod = "mean",
                       PreAgg = TRUE,
                       XVar = NULL,
                       YVar = NULL,
                       GroupVar = NULL,
                       YVarTrans = "Identity",
                       XVarTrans = "Identity",
                       FacetRows = 1,
                       FacetCols = 1,
                       FacetLevels = NULL,
                       Height = NULL,
                       Width = NULL,
                       Title = 'River Plot',
                       ShowLabels = FALSE,
                       Title.YAxis = NULL,
                       Title.XAxis = NULL,
                       EchartsTheme = "macarons",
                       X_Scroll = FALSE,
                       Y_Scroll = FALSE,
                       TimeLine = TRUE,
                       ShowSymbol = FALSE,
                       TextColor =        "white",
                       title.fontSize = 22,
                       title.fontWeight = "bold", # normal
                       title.textShadowColor = '#63aeff',
                       title.textShadowBlur = 3,
                       title.textShadowOffsetY = 1,
                       title.textShadowOffsetX = -1,
                       xaxis.fontSize = 14,
                       yaxis.fontSize = 14,
                       Debug = FALSE) {

  if(length(GroupVar) == 0L) TimeLine <- FALSE
  if(length(GroupVar) == 0L && length(YVar) <= 1L) {
    if(Debug) print("if(length(GroupVar) == 0L && length(YVar) <= 1L) return(NULL)")
    return(NULL)
  }
  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })
  Ncols <- ncol(dt)
  if(length(FacetLevels) > 0L) {
    dt1 <- data.table::copy(dt[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar, XVar, GroupVar)])
  } else {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, GroupVar)])
  }

  if(Debug) print("Plot.River 3")

  # Minimize data before moving on
  if(!PreAgg) {

    if(Debug) print("Plot.River 4")

    # DCast -> redefine YVar -> Proceed as normal
    if(length(YVar) == 1L && length(GroupVar) > 0L) {
      dt1 <- data.table::dcast.data.table(
        data = dt1,
        formula = get(XVar) ~ get(GroupVar[1L]),
        fun.aggregate = sum,
        value.var = eval(YVar))
      data.table::setnames(x = dt1, "XVar", c(XVar))
      YVar <- names(dt1)[-1L]
      GroupVar <- NULL
    }

    # Define Aggregation function
    if(Debug) print("Plot.Calibration.Line # Define Aggregation function")
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

    # Aggregate data
    if(length(GroupVar) > 0L) {
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar,GroupVar[1L])]
      data.table::setorderv(x = dt1, cols = c(GroupVar[1L], XVar), rep(1L, length(c(GroupVar[1L], XVar))))
    } else {
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar)]
      data.table::setorderv(x = dt1, cols = XVar, 1L)
    }
  }

  # Transformation
  for(yvart in YVarTrans) {
    if(YVarTrans != "Identity") {
      dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = yvart, Methods = YVarTrans)$Data
    }
  }

  if(Debug) print("Plot.River 6b")

  # Plot
  data.table::setorderv(x = dt1, cols = XVar, 1L)
  cxv <- class(dt1[[XVar]])[1L]
  if(cxv %in% "IDate") {
    dt1[, eval(XVar) := as.Date(get(XVar))]
  } else if(cxv %in% "IDateTime") {
    dt1[, eval(XVar) := as.POSIXct(get(XVar))]
  }

  if(Debug) print("Plot.River 7b")

  # Build base plot depending on GroupVar availability
  if(Debug) print("Plot.Line no group Echarts")
  p1 <- echarts4r::e_charts_(
    data = dt1,
    x = XVar,
    dispose = TRUE,
    darkMode = TRUE,
    width = Width,
    height = Height)
  for(i in YVar) p1 <- echarts4r::e_river_(e = p1, serie = i)

  if(Debug) print("Plot.River 8b")

  # Finalize Plot Build
  if(FacetRows == 1L && FacetCols == 1L) {
    if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
    if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
  }
  p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
  p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
  p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
  p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
  p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
  p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
  p1 <- echarts4r::e_brush(e = p1)
  p1 <- echarts4r::e_title(
    p1, Title,
    textStyle = list(
      color = TextColor,
      fontWeight = title.fontWeight,
      overflow = "truncate", # "none", "truncate", "break",
      ellipsis = '...',
      fontSize = title.fontSize,
      textShadowColor = title.textShadowColor,
      textShadowBlur = title.textShadowBlur,
      textShadowOffsetY = title.textShadowOffsetY,
      textShadowOffsetX = title.textShadowOffsetX))
  return(p1)
}

#' @title Plot.Bar
#'
#' @description Build a bar plot by simply passing arguments to a single function
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param PreAgg logical
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param LabelValues A vector of values. Requires PreAgg to be set to TRUE and you'll need to ensure LabelValues are ordered the same as dt. If NULL and ShowLabels is TRUE, then bar values will be displayed
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title title
#' @param Title.YAxis NULL. If NULL, YVar name will be used
#' @param Title.XAxis NULL. If NULL, XVar name will be used
#' @param ShowLabels logical
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor 'darkblue'
#' @param Debug Debugging purposes
#' @export
Plot.Bar <- function(dt = NULL,
                     PreAgg = FALSE,
                     XVar = NULL,
                     YVar = NULL,
                     GroupVar = NULL,
                     LabelValues = NULL,
                     YVarTrans = "Identity",
                     XVarTrans = "Identity",
                     FacetRows = 1,
                     FacetCols = 1,
                     FacetLevels = NULL,
                     AggMethod = 'mean',
                     Height = NULL,
                     Width = NULL,
                     Title = 'Bar Plot',
                     ShowLabels = FALSE,
                     Title.YAxis = NULL,
                     Title.XAxis = NULL,
                     EchartsTheme = "macarons",
                     TimeLine = TRUE,
                     X_Scroll = TRUE,
                     Y_Scroll = TRUE,
                     TextColor = "white",
                     title.fontSize = 22,
                     title.fontWeight = "bold", # normal
                     title.textShadowColor = '#63aeff',
                     title.textShadowBlur = 3,
                     title.textShadowOffsetY = 1,
                     title.textShadowOffsetX = -1,
                     xaxis.fontSize = 14,
                     yaxis.fontSize = 14,
                     xaxis.rotate = 0,
                     yaxis.rotate = 0,
                     ContainLabel = TRUE,
                     Debug = FALSE) {

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  if(length(GroupVar) == 0L) TimeLine <- FALSE

  # Convert factor to character
  if(length(GroupVar) > 0L && class(dt[[GroupVar]])[1L] == "factor") {
    dt[, eval(GroupVar) := as.character(get(GroupVar))]
  }
  if(length(XVar) > 0L && class(dt[[XVar]])[1L] == "factor") {
    dt[, eval(XVar) := as.character(get(XVar))]
  }
  if(length(YVar) > 0L && class(dt[[YVar]])[1L] == "factor") {
    dt[, eval(YVar) := as.character(get(YVar))]
  }

  # Used multiple times
  check1 <- length(XVar) != 0 && length(YVar) != 0
  check2 <- length(XVar) == 0 && length(YVar) != 0
  check3 <- length(XVar) != 0 && length(YVar) == 0

  # Define Aggregation function
  if(!PreAgg) {
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)
  }

  # Create base plot object
  numvars <- c()
  byvars <- c()
  if(check1) {
    if(length(GroupVar) != 0L) {
      if(!PreAgg) {

        if(length(FacetLevels) > 0L) {
          dt <- dt[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,XVar,GroupVar)]
        }

        if(any(tryCatch({class(dt[[eval(YVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
          numvars <- unique(c(numvars, YVar))
        } else {
          byvars <- unique(c(byvars, YVar))
        }
        if(any(tryCatch({class(dt[[eval(XVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
          if(length(numvars) > 0) {
            x <- length(unique(dt[[XVar]]))
            y <- length(unique(dt[[YVar]]))
            if(x > y) {
              byvars <- unique(c(byvars, YVar))
              numvars[1L] <- XVar
            } else {
              byvars <- unique(c(byvars, XVar))
            }
          } else {
            numvars <- unique(c(numvars, XVar))
          }
        } else {
          byvars <- unique(c(byvars, XVar))
        }
        if(any(tryCatch({class(dt[[eval(GroupVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
          dt[, eval(GroupVar) := as.character(get(GroupVar))]
          byvars <- unique(c(byvars, GroupVar))
        } else {
          byvars <- unique(c(byvars, GroupVar))
        }
        if(!is.null(byvars)) {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars), by = c(byvars)]
          for(i in byvars) {
            if(class(temp[[i]]) %in% c('numeric','integer')) {
              temp[, eval(i) := as.character(get(i))]
            }
          }
        } else {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars)]
        }
      } else {
        temp <- data.table::copy(dt)
        numvars <- AutoPlots:::ColNameFilter(data = temp, Types = 'numeric')[[1L]]
        byvars <- unlist(AutoPlots:::ColNameFilter(data = temp, Types = "character"))
      }

      # Transformation
      if(length(XVar) > 0L && class(temp[[XVar]])[1L] %in% c("numeric","integer")) {
        YVarTrans <- XVarTrans
      }
      if(YVarTrans != "Identity") {
        temp <- AutoTransformationCreate(data = temp, ColumnNames = numvars, Methods = YVarTrans)$Data
      }

      # Plot
      p1 <- echarts4r::e_charts_(
        temp |> dplyr::group_by(get(GroupVar[1L])),
        x = XVar,
        darkMode = TRUE,
        emphasis = list(focus = "series"),
        dispose = TRUE,
        width = Width,
        height = Height)

      if(ShowLabels) {
        p1 <- echarts4r::e_bar_(e = p1, YVar, label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_bar_(e = p1, YVar)
      }

      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))
      if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(
        e = p1,
        rows = FacetRows,
        cols = FacetCols,
        legend_space = 16,
        legend_pos = "top")
      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(
          e = p1,
          serie = NULL,
          axis = "x",
          name = XVar,
          nameLocation = "middle",
          nameGap = 45,
          nameTextStyle = list(
            color = TextColor,
            fontStyle = "normal",
            fontWeight = "bold",
            fontSize = xaxis.fontSize),
          axisLabel = list(
            rotate = xaxis.rotate,
            grid = list(containLabel = ContainLabel)))
      } else {
        p1 <- echarts4r::e_axis_(
          e = p1,
          serie = NULL,
          axis = "x",
          name = Title.XAxis,
          nameLocation = "middle",
          nameGap = 45,
          nameTextStyle = list(
            color = TextColor,
            fontStyle = "normal",
            fontWeight = "bold",
            fontSize = xaxis.fontSize),
          axisLabel = list(
            rotate = xaxis.rotate,
            grid = list(containLabel = ContainLabel)))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(
          e = p1,
          serie = NULL,
          axis = "y",
          name = YVar,
          nameLocation = "middle",
          nameGap = 45,
          nameTextStyle = list(
            color = TextColor,
            fontStyle = "normal",
            fontWeight = "bold",
            fontSize = yaxis.fontSize),
          axisLabel = list(
            rotate = yaxis.rotate,
            grid = list(containLabel = ContainLabel)))
      } else {
        p1 <- echarts4r::e_axis_(
          e = p1,
          serie = NULL,
          axis = "y",
          name = Title.YAxis,
          nameLocation = "middle",
          nameGap = 45,
          nameTextStyle = list(
            color = TextColor,
            fontStyle = "normal",
            fontWeight = "bold",
            fontSize = yaxis.fontSize),
          axisLabel = list(
            rotate = yaxis.rotate,
            grid = list(containLabel = ContainLabel)))
      }

      return(p1)

    } else {

      if(Debug) {
        print("BarPlot 2.b")
        print(PreAgg)
      }

      if(!PreAgg) {
        if(tryCatch({class(dt[[eval(YVar)]])[1L]}, error = function(x) "bla") %in% c('numeric','integer')) {
          numvars <- unique(c(numvars, YVar))
        } else {
          byvars <- unique(c(byvars, YVar))
        }
        if(tryCatch({class(dt[[eval(XVar)]])[1L]}, error = function(x) "bla") %in% c('numeric','integer')) {
          if(length(numvars) > 0) {
            x <- length(unique(dt[[XVar]]))
            y <- length(unique(dt[[YVar]]))
            if(x > y) {
              byvars <- unique(c(byvars, YVar))
              numvars[1L] <- XVar
            } else {
              byvars <- unique(c(byvars, XVar))
            }
          } else {
            numvars <- unique(c(numvars, XVar))
          }
        } else {
          byvars <- unique(c(byvars, XVar))
        }
        if(!is.null(byvars)) {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars), by = c(byvars)]
          for(i in byvars) {
            if(class(temp[[i]])[1L] %in% c('numeric','integer')) {
              temp[, eval(i) := as.character(get(i))]
            }
          }
        } else {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars)]
        }
      } else {
        temp <- data.table::copy(dt)
        if(Debug) print("BarPlot 2.bb")
        numvars <- AutoPlots:::ColNameFilter(data = temp, Types = 'numeric')[[1L]]
        byvars <- unlist(AutoPlots:::ColNameFilter(data = temp, Types = "character"))
      }

      if(Debug) print("BarPlot 2.bbb")

      # Transformation
      if(length(XVar) > 0L && class(temp[[XVar]])[1L] %in% c("numeric","integer")) {
        YVarTrans <- XVarTrans
      }

      if(Debug) print("BarPlot 2.bbbb")

      if(YVarTrans != "Identity") {
        temp <- AutoTransformationCreate(data = temp, ColumnNames = numvars, Methods = YVarTrans)$Data
      }

      if(Debug) print("BarPlot 2.bbbbb")

      # yvar <- temp[[YVar]]
      # xvar <- temp[[XVar]]

      # Plot
      if(XVar == "Importance" && YVar == "Variable") {
        XVar <- "Variable"
        YVar <- "Importance"
      }

      if(Debug) print("BarPlot 2.bbbbbb")

      p1 <- echarts4r::e_charts_(
        temp,
        x = XVar,
        dispose = TRUE,
        darkMode = TRUE,
        width = Width,
        height = Height)

      if(Debug) print("BarPlot 2.c")

      if(ShowLabels) {
        if(length(LabelValues) > 0L && PreAgg) {
          p1 <- echarts4r::e_charts_(
            temp,
            x = XVar,
            dispose = TRUE,
            darkMode = TRUE,
            width = Width,
            height = Height) |>
            echarts4r::e_bar_(
              YVar,
              bind = LabelValues,
              label = list(
                show = TRUE,
                formatter = "{b}",
                position = "outside"))

        } else {
          p1 <- echarts4r::e_bar_(e = p1, YVar, label = list(show = TRUE))
        }

      } else {
        if(Debug) print("BarPlot 2.cc")
        p1 <- echarts4r::e_bar_(e = p1, YVar)
      }
      if(FacetRows == 1L && FacetCols == 1L) {
        if(Debug) print("BarPlot 2.ccc")
        if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      if(Debug) print("BarPlot 2.cccc")
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(Debug) print("BarPlot 2.d")
      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(
          e = p1,
          serie = NULL,
          axis = "x",
          name = XVar,
          nameLocation = "middle",
          nameGap = 45,
          nameTextStyle = list(
            color = TextColor,
            fontStyle = "normal",
            fontWeight = "bold",
            fontSize = xaxis.fontSize),
          axisLabel = list(
            rotate = xaxis.rotate,
            grid = list(containLabel = ContainLabel)))
      } else {
        p1 <- echarts4r::e_axis_(
          e = p1,
          serie = NULL,
          axis = "x",
          name = Title.XAxis,
          nameLocation = "middle",
          nameGap = 45,
          nameTextStyle = list(
            color = TextColor,
            fontStyle = "normal",
            fontWeight = "bold",
            fontSize = xaxis.fontSize),
          axisLabel = list(
            rotate = xaxis.rotate,
            grid = list(containLabel = ContainLabel)))
      }

      if(Debug) print("BarPlot 2.e")
      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(
          e = p1,
          serie = NULL,
          axis = "y",
          name = YVar,
          nameLocation = "middle",
          nameGap = 45,
          nameTextStyle = list(
            color = TextColor,
            fontStyle = "normal",
            fontWeight = "bold",
            fontSize = yaxis.fontSize),
          axisLabel = list(
            rotate = yaxis.rotate,
            grid = list(containLabel = ContainLabel)))
      } else {
        p1 <- echarts4r::e_axis_(
          e = p1,
          serie = NULL,
          axis = "y",
          name = Title.YAxis,
          nameLocation = "middle",
          nameGap = 45,
          nameTextStyle = list(
            color = TextColor,
            fontStyle = "normal",
            fontWeight = "bold",
            fontSize = yaxis.fontSize),
          axisLabel = list(
            rotate = yaxis.rotate,
            grid = list(containLabel = ContainLabel)))
      }

      if(Debug) print("BarPlot 2.f")
      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))
      if(Debug) print("BarPlot 2.g")
      if(FacetRows > 1L || FacetCols > 1L) {
        p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      } else {
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      }
      if(Debug) print("BarPlot 2.h")
      return(p1)
    }

  }

  if(check2) {

    if(length(GroupVar) != 0) {
      if(!PreAgg) {
        if(any(tryCatch({class(dt[[eval(YVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
          numvars <- unique(c(numvars, YVar))
        } else {
          byvars <- unique(c(byvars, YVar))
        }
        if(any(tryCatch({class(dt[[eval(GroupVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
          numvars <- unique(c(numvars, GroupVar))
        } else {
          byvars <- unique(c(byvars, GroupVar))
        }
        if(!is.null(byvars)) {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars), by = c(byvars)]
        } else {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars)]
        }
      } else {
        temp <- data.table::copy(dt)
        numvars <- AutoPlots:::ColNameFilter(data = temp, Types = 'numeric')[[1L]]
        byvars <- unlist(AutoPlots:::ColNameFilter(data = temp, Types = "character"))
      }

      # Transformation
      if(length(XVar) > 0L && class(temp[[XVar]])[1L] %in% c("numeric","integer")) {
        YVarTrans <- XVarTrans
      }
      if(YVarTrans != "Identity") {
        temp <- AutoTransformationCreate(data = temp, ColumnNames = numvars, Methods = YVarTrans)$Data
      }

      p1 <- echarts4r::e_charts_(
        temp, x = GroupVar[1L],
        dispose = TRUE,
        darkMode = TRUE,
        width = Width,
        height = Height)
      if(ShowLabels) {
        p1 <- echarts4r::e_bar_(e = p1, YVar, label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_bar_(e = p1, YVar)
      }
      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(
          e = p1,
          serie = NULL,
          axis = "x",
          name = XVar,
          nameLocation = "middle",
          nameGap = 45,
          nameTextStyle = list(
            color = TextColor,
            fontStyle = "normal",
            fontWeight = "bold",
            fontSize = xaxis.fontSize),
          axisLabel = list(
            rotate = xaxis.rotate,
            grid = list(containLabel = ContainLabel)))
      } else {
        p1 <- echarts4r::e_axis_(
          e = p1,
          serie = NULL,
          axis = "x",
          name = Title.XAxis,
          nameLocation = "middle",
          nameGap = 45,
          nameTextStyle = list(
            color = TextColor,
            fontStyle = "normal",
            fontWeight = "bold",
            fontSize = xaxis.fontSize),
          axisLabel = list(
            rotate = xaxis.rotate,
            grid = list(containLabel = ContainLabel)))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(
          e = p1,
          serie = NULL,
          axis = "y",
          name = YVar,
          nameLocation = "middle",
          nameGap = 45,
          nameTextStyle = list(
            color = TextColor,
            fontStyle = "normal",
            fontWeight = "bold",
            fontSize = yaxis.fontSize),
          axisLabel = list(
            rotate = yaxis.rotate,
            grid = list(containLabel = ContainLabel)))
      } else {
        p1 <- echarts4r::e_axis_(
          e = p1,
          serie = NULL,
          axis = "y",
          name = Title.YAxis,
          nameLocation = "middle",
          nameGap = 45,
          nameTextStyle = list(
            color = TextColor,
            fontStyle = "normal",
            fontWeight = "bold",
            fontSize = yaxis.fontSize),
          axisLabel = list(
            rotate = yaxis.rotate,
            grid = list(containLabel = ContainLabel)))
      }

      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))
      if(FacetRows > 1L || FacetCols > 1L) {
        p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      } else {
        p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
      }


      return(p1)
    } else {
      return(NULL)
    }

  }

  if(check3) {

    if(length(GroupVar) != 0) {
      if(!PreAgg) {
        if(any(tryCatch({class(dt[[eval(XVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
          numvars <- unique(c(numvars, XVar))
        } else {
          byvars <- unique(c(byvars, XVar))
        }
        if(any(tryCatch({class(dt[[eval(GroupVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
          numvars <- unique(c(numvars, GroupVar))
        } else {
          byvars <- unique(c(byvars, GroupVar))
        }
        if(!is.null(byvars)) {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars), by = c(byvars)]
        } else {
          temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars)]
        }
      } else {
        temp <- data.table::copy(dt)
        numvars <- AutoPlots:::ColNameFilter(data = temp, Types = 'numeric')[[1L]]
        byvars <- unlist(AutoPlots:::ColNameFilter(data = temp, Types = "character"))
      }

      # Transformation
      if(length(XVar) > 0L && class(temp[[XVar]])[1L] %in% c("numeric","integer")) {
        YVarTrans <- XVarTrans
      }
      if(YVarTrans != "Identity") {
        temp <- AutoTransformationCreate(data = temp, ColumnNames = numvars, Methods = YVarTrans)$Data
      }

      # Plot
      p1 <- echarts4r::e_charts_(
        temp,
        x = GroupVar[1L],
        dispose = TRUE,
        darkMode = TRUE,
        width = Width,
        height = Height)
      if(ShowLabels) {
        p1 <- echarts4r::e_bar_(e = p1, XVar, label = list(show = TRUE))
      } else {
        p1 <- echarts4r::e_bar_(e = p1, XVar)
      }
      if(FacetRows == 1L && FacetCols == 1L) {
        if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
        if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
      }
      p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
      p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
      p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
      p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
      p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

      if(length(Title.XAxis) == 0L) {
        p1 <- echarts4r::e_axis_(
          e = p1,
          serie = NULL,
          axis = "x",
          name = XVar,
          nameLocation = "middle",
          nameGap = 45,
          nameTextStyle = list(
            color = TextColor,
            fontStyle = "normal",
            fontWeight = "bold",
            fontSize = xaxis.fontSize),
          axisLabel = list(
            rotate = xaxis.rotate,
            grid = list(containLabel = ContainLabel)))
      } else {
        p1 <- echarts4r::e_axis_(
          e = p1,
          serie = NULL,
          axis = "x",
          name = Title.XAxis,
          nameLocation = "middle",
          nameGap = 45,
          nameTextStyle = list(
            color = TextColor,
            fontStyle = "normal",
            fontWeight = "bold",
            fontSize = xaxis.fontSize),
          axisLabel = list(
            rotate = xaxis.rotate,
            grid = list(containLabel = ContainLabel)))
      }

      if(length(Title.YAxis) == 0L) {
        p1 <- echarts4r::e_axis_(
          e = p1,
          serie = NULL,
          axis = "y",
          name = YVar,
          nameLocation = "middle",
          nameGap = 45,
          nameTextStyle = list(
            color = TextColor,
            fontStyle = "normal",
            fontWeight = "bold",
            fontSize = yaxis.fontSize),
          axisLabel = list(
            rotate = yaxis.rotate,
            grid = list(containLabel = ContainLabel)))
      } else {
        p1 <- echarts4r::e_axis_(
          e = p1,
          serie = NULL,
          axis = "y",
          name = Title.YAxis,
          nameLocation = "middle",
          nameGap = 45,
          nameTextStyle = list(
            color = TextColor,
            fontStyle = "normal",
            fontWeight = "bold",
            fontSize = yaxis.fontSize),
          axisLabel = list(
            rotate = yaxis.rotate,
            grid = list(containLabel = ContainLabel)))
      }

      p1 <- echarts4r::e_brush(e = p1)
      p1 <- echarts4r::e_title(
        p1, Title,
        textStyle = list(
          color = TextColor,
          fontWeight = title.fontWeight,
          overflow = "truncate", # "none", "truncate", "break",
          ellipsis = '...',
          fontSize = title.fontSize,
          textShadowColor = title.textShadowColor,
          textShadowBlur = title.textShadowBlur,
          textShadowOffsetY = title.textShadowOffsetY,
          textShadowOffsetX = title.textShadowOffsetX))
      if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")


      return(p1)
    } else {
      return(NULL)
    }
  }

  if(!check1 && !check2 && !check3) return(NULL)

  # Return plot
  return(p1)
}

#' @title Plot.ACF
#'
#' @description Build an autocorrelation plot by simply passing arguments to a single function
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param PreAgg logical
#' @param YVar Y-Axis variable name
#' @param DateVar Date column in data
#' @param TimeUnit Select from "hour", "day", "week", "month", "quarter", "year"
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title title
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param TextColor 'darkblue'
#' @param Debug Debugging purposes
#' @export
Plot.ACF <- function(dt = NULL,
                     YVar = NULL,
                     DateVar = NULL,
                     TimeUnit = NULL,
                     MaxLags = 50,
                     YVarTrans = "Identity",
                     AggMethod = 'sum',
                     Height = NULL,
                     Width = NULL,
                     Title = 'Autocorrelation Plot',
                     EchartsTheme = "macarons",
                     TextColor = "white",
                     title.fontSize = 22,
                     title.fontWeight = "bold", # normal
                     title.textShadowColor = '#63aeff',
                     title.textShadowBlur = 3,
                     title.textShadowOffsetY = 1,
                     title.textShadowOffsetX = -1,
                     xaxis.fontSize = 14,
                     yaxis.fontSize = 14,
                     xaxis.rotate = 0,
                     yaxis.rotate = 0,
                     ContainLabel = TRUE,
                     Debug = FALSE) {

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  dt1 <- data.table::copy(dt)

  # Convert factor to character
  if(length(YVar) > 0L && class(dt1[[YVar]])[1L] == "factor") {
    return(NULL)
  }

  # Define Aggregation function
  if(Debug) print("Plot.ACH 1")
  aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

  if(Debug) print("Plot.ACH 2")

  # Transformation
  if(YVarTrans != "Identity") {
    dt1 <- AutoPlots:::AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data
  }

  if(Debug) print("Plot.ACH 3")

  # Aggregate dt1
  dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), .SDcols = c(YVar), by = c(DateVar)]

  if(Debug) print("Plot.ACH 3.5")

  dt1 <- Rodeo::AutoLagRollStats(
    data = dt1,
    DateColumn = DateVar,
    Targets = YVar,
    TimeUnitAgg = TimeUnit,
    TimeGroups = TimeUnit,
    TimeUnit = TimeUnit,
    RollOnLag1 = TRUE,
    Type = "Lag",
    SimpleImpute = TRUE,
    Lags = seq_len(MaxLags))

  if(Debug) print("Plot.ACH 4")

  # Autocorrelation data creation
  ACF_Data <- data.table::data.table(Lag = 1:50, Cor = 0.0, `Lower 95th` = 0.0, `Upper 95th` = 0.0)
  if(Debug) print("Plot.ACH 5")
  for(i in seq_len(MaxLags)) {# i = 1
    lagCol <- names(dt1)[which(grepl(pattern = paste0("_LAG_",i,"_"), x = names(dt1)))]
    lag_test <- cor.test(x = dt1[[YVar]], y = dt1[[lagCol]])
    data.table::set(ACF_Data, i = i, j = "Lag", value = i)
    data.table::set(ACF_Data, i = i, j = "Cor", value = lag_test$estimate)
    data.table::set(ACF_Data, i = i, j = "Lower 95th", value = lag_test$conf.int[1L])
    data.table::set(ACF_Data, i = i, j = "Upper 95th", value = lag_test$conf.int[2L])
  }

  if(Debug) print("Plot.ACH 6")

  # Plot
  p1 <- echarts4r::e_charts_(
    ACF_Data,
    x = "Lag",
    dispose = TRUE,
    darkMode = TRUE,
    width = Width,
    height = Height)

  if(Debug) print("Plot.ACH 7")
  p1 <- echarts4r::e_bar_(e = p1, "Cor")

  if(Debug) print("Plot.ACH 8")

  # MAX Band is not working currently so plot looks stupid with this
  # p1 <- echarts4r::e_band_(
  #   e = p1,
  #   min = "Lower 95th", max = "Upper 95th", stack = "confidence-band",
  #   areaStyle = list(list(color = "#54535387"), list(color = "#54535387"))
  # )

  # Alternative bands: just lines but they are correct
  p1 <- echarts4r::e_line_(e = p1, "Lower 95th", smooth = TRUE)
  p1 <- echarts4r::e_line_(e = p1, "Upper 95th", smooth = TRUE)

  # Extras
  if(Debug) print("Plot.ACH 10")
  p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
  p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
  p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
  p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
  p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
  p1 <- echarts4r::e_axis_(
    e = p1,
    serie = NULL,
    axis = "x",
    name = "Lags",
    nameLocation = "middle",
    nameGap = 45,
    nameTextStyle = list(
      color = TextColor,
      fontStyle = "normal",
      fontWeight = "bold",
      fontSize = xaxis.fontSize),
    axisLabel = list(
      rotate = xaxis.rotate,
      grid = list(containLabel = ContainLabel)))
  p1 <- echarts4r::e_axis_(
    e = p1,
    serie = NULL,
    axis = "y",
    name = "Correlation",
    nameLocation = "middle",
    nameGap = 45,
    nameTextStyle = list(
      color = TextColor,
      fontStyle = "normal",
      fontWeight = "bold",
      fontSize = yaxis.fontSize),
    axisLabel = list(
      rotate = yaxis.rotate,
      grid = list(containLabel = ContainLabel)))
  p1 <- echarts4r::e_brush(e = p1)
  p1 <- echarts4r::e_title(
    p1, Title,
    textStyle = list(
      color = TextColor,
      fontWeight = title.fontWeight,
      overflow = "truncate", # "none", "truncate", "break",
      ellipsis = '...',
      fontSize = title.fontSize,
      textShadowColor = title.textShadowColor,
      textShadowBlur = title.textShadowBlur,
      textShadowOffsetY = title.textShadowOffsetY,
      textShadowOffsetX = title.textShadowOffsetX))
  return(p1)
}

#' @title Plot.PACF
#'
#' @description Build a partial autocorrelation plot by simply passing arguments to a single function
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param PreAgg logical
#' @param YVar Y-Axis variable name
#' @param DateVar Date column in data
#' @param TimeUnit Select from "hour", "day", "week", "month", "quarter", "year"
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title title
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param TextColor 'darkblue'
#' @param Debug Debugging purposes
#' @export
Plot.PACF <- function(dt = NULL,
                      YVar = NULL,
                      DateVar = NULL,
                      TimeUnit = NULL,
                      MaxLags = 50,
                      YVarTrans = "Identity",
                      AggMethod = 'sum',
                      Height = NULL,
                      Width = NULL,
                      Title = 'Partial Autocorrelation Plot',
                      EchartsTheme = "macarons",
                      TextColor = "white",
                      title.fontSize = 22,
                      title.fontWeight = "bold", # normal
                      title.textShadowColor = '#63aeff',
                      title.textShadowBlur = 3,
                      title.textShadowOffsetY = 1,
                      title.textShadowOffsetX = -1,
                      xaxis.fontSize = 14,
                      yaxis.fontSize = 14,
                      xaxis.rotate = 0,
                      yaxis.rotate = 0,
                      ContainLabel = TRUE,
                      Debug = FALSE) {


  # dt = data.table::fread(file.choose())
  # YVar = "Daily Liters"
  # DateVar = "Date"
  # TimeUnit = "days"
  # MaxLags = 50
  # YVarTrans = "Identity"
  # AggMethod = 'sum'
  # Height = "600px"
  # Width = "300px"
  # Title = 'Partial Autocorrelation Plot'
  # EchartsTheme = "macarons"
  # TextColor = "white"
  # title.fontSize = 22
  # title.fontWeight = "bold"
  # title.textShadowColor = '#63aeff'
  # title.textShadowBlur = 3
  # title.textShadowOffsetY = 1
  # title.textShadowOffsetX = -1
  # xaxis.fontSize = 14
  # yaxis.fontSize = 14
  # xaxis.rotate = 0
  # yaxis.rotate = 0
  # ContainLabel = TRUE
  # Debug = FALSE

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  dt1 <- data.table::copy(dt)

  if(grepl(" ", YVar)) {
    data.table::setnames(x = dt1, old = YVar, new = gsub(pattern = " ", replacement = ".", x = YVar))
    YVar <- gsub(pattern = " ", replacement = ".", x = YVar)
  }

  # Convert factor to character
  if(length(YVar) > 0L && class(dt1[[YVar]])[1L] == "factor") {
    return(NULL)
  }

  # Define Aggregation function
  if(Debug) print("Plot.PACH 1")
  aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

  if(Debug) print("Plot.PACH 2")

  # Transformation
  if(YVarTrans != "Identity") {
    dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data
  }

  if(Debug) print("Plot.PACH 3")

  # Aggregate dt1
  dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), .SDcols = c(YVar), by = c(DateVar)]

  if(Debug) print("Plot.PACH 3.5")

  dt1 <- Rodeo::AutoLagRollStats(
    data = dt1,
    DateColumn = DateVar,
    Targets = YVar,
    TimeUnitAgg = TimeUnit,
    TimeGroups = TimeUnit,
    TimeUnit = TimeUnit,
    RollOnLag1 = TRUE,
    Type = "Lag",
    SimpleImpute = TRUE,
    Lags = seq_len(MaxLags))

  if(Debug) print("Plot.PACH 4")

  # Autocorrelation data creation
  PACF_Data <- data.table::data.table(Lag = 1:50, Cor = 0.0, `Lower 95th` = 0.0, `Upper 95th` = 0.0)
  LagCols <- c()
  if(Debug) print("Plot.ACH 5")
  for(i in seq_len(MaxLags)) {# i = 1L  i = 2L
    LagCols[i] <- names(dt1)[which(grepl(pattern = paste0("_LAG_",i,"_"), x = names(dt1)))]
    if(i == 1L) {
      lag_test <- cor.test(x = dt1[[YVar]], y = dt1[[LagCols]])
      data.table::set(PACF_Data, i = i, j = "Lag", value = i)
      data.table::set(PACF_Data, i = i, j = "Cor", value = lag_test$estimate)
      data.table::set(PACF_Data, i = i, j = "Lower 95th", value = lag_test$conf.int[1L])
      data.table::set(PACF_Data, i = i, j = "Upper 95th", value = lag_test$conf.int[2L])
    } else {
      x <- as.vector(lm(formula = as.formula(paste0(YVar, " ~ ", paste0(LagCols, collapse = " + "))), data = dt1)$residuals)
      lag_test <- cor.test(x = x, y = dt1[[LagCols[i]]])
      data.table::set(PACF_Data, i = i, j = "Lag", value = i)
      data.table::set(PACF_Data, i = i, j = "Cor", value = lag_test$estimate)
      data.table::set(PACF_Data, i = i, j = "Lower 95th", value = lag_test$conf.int[1L])
      data.table::set(PACF_Data, i = i, j = "Upper 95th", value = lag_test$conf.int[2L])
    }
  }

  if(Debug) print("Plot.PACH 6")

  # Plot
  p1 <- echarts4r::e_charts_(
    PACF_Data,
    x = "Lag",
    dispose = TRUE,
    darkMode = TRUE,
    width = Width,
    height = Height)

  if(Debug) print("Plot.PACH 7")
  p1 <- echarts4r::e_bar_(e = p1, "Cor")

  if(Debug) print("Plot.PACH 8")

  p1 <- echarts4r::e_line_(e = p1, "Lower 95th", smooth = TRUE)

  if(Debug) print("Plot.PACH 9")

  p1 <- echarts4r::e_line_(e = p1, "Upper 95th", smooth = TRUE)

  # Extras
  if(Debug) print("Plot.PACH 10")
  p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
  p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
  p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
  p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
  p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
  p1 <- echarts4r::e_axis_(
    e = p1,
    serie = NULL,
    axis = "x",
    name = "Lags",
    nameLocation = "middle",
    nameGap = 45,
    nameTextStyle = list(
      color = TextColor,
      fontStyle = "normal",
      fontWeight = "bold",
      fontSize = xaxis.fontSize),
    axisLabel = list(
      rotate = xaxis.rotate,
      grid = list(containLabel = ContainLabel)))
  p1 <- echarts4r::e_axis_(
    e = p1,
    serie = NULL,
    axis = "y",
    name = "Correlation",
    nameLocation = "middle",
    nameGap = 45,
    nameTextStyle = list(
      color = TextColor,
      fontStyle = "normal",
      fontWeight = "bold",
      fontSize = yaxis.fontSize),
    axisLabel = list(
      rotate = yaxis.rotate,
      grid = list(containLabel = ContainLabel)))
  p1 <- echarts4r::e_brush(e = p1)
  p1 <- echarts4r::e_title(
    p1, Title,
    textStyle = list(
      color = TextColor,
      fontWeight = title.fontWeight,
      overflow = "truncate", # "none", "truncate", "break",
      ellipsis = '...',
      fontSize = title.fontSize,
      textShadowColor = title.textShadowColor,
      textShadowBlur = title.textShadowBlur,
      textShadowOffsetY = title.textShadowOffsetY,
      textShadowOffsetX = title.textShadowOffsetX))
  return(p1)
}

#' @title Plot.StackedBar
#'
#' @description Build a stacked bar plot vs a grouped bar plot
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param PreAgg logical
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param Height NULL
#' @param Width NULL
#' @param Title title
#' @param Title.YAxis NULL. If NULL, YVar name will be used
#' @param Title.XAxis NULL. If NULL, XVar name will be used
#' @param ShowLabels logical
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor 'darkblue'
#' @param Debug Debugging purposes
#' @export
Plot.StackedBar <- function(dt = NULL,
                            PreAgg = FALSE,
                            XVar = NULL,
                            YVar = NULL,
                            GroupVar = NULL,
                            YVarTrans = "Identity",
                            XVarTrans = "Identity",
                            FacetRows = 1,
                            FacetCols = 1,
                            FacetLevels = NULL,
                            AggMethod = 'mean',
                            Height = NULL,
                            Width = NULL,
                            Title = "Stacked Bar",
                            Title.YAxis = NULL,
                            Title.XAxis = NULL,
                            ShowLabels = FALSE,
                            EchartsTheme = "macarons",
                            TimeLine = TRUE,
                            X_Scroll = TRUE,
                            Y_Scroll = TRUE,
                            TextColor =        "white",
                            title.fontSize = 22,
                            title.fontWeight = "bold", # normal
                            title.textShadowColor = '#63aeff',
                            title.textShadowBlur = 3,
                            title.textShadowOffsetY = 1,
                            title.textShadowOffsetX = -1,
                            yaxis.fontSize = 14,
                            xaxis.fontSize = 14,
                            xaxis.rotate = 0,
                            yaxis.rotate = 0,
                            ContainLabel = TRUE,
                            Debug = FALSE) {

  if(length(XVar) == 0L) return(NULL)
  if(length(YVar) == 0L) return(NULL)
  if(length(GroupVar) == 0L) return(NULL)

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # Convert factor to character
  if(length(GroupVar) > 0L && class(dt[[GroupVar]])[1L] %in% c("factor","integer","numeric")) {
    dt[, eval(GroupVar) := as.character(get(GroupVar))]
  }
  if(length(XVar) > 0L && class(dt[[XVar]])[1L] %in% c("factor","integer","numeric")) {
    dt[, eval(XVar) := as.character(get(XVar))]
  }
  if(length(YVar) > 0L && class(dt[[YVar]])[1L] == "factor") {
    dt[, eval(YVar) := as.character(get(YVar))]
  }

  if(class(dt[[YVar]])[1L] %in% c("character","factor") && class(dt[[XVar]])[1L] %in% c("numeric","integer")) {
    l <- YVar
    YVar <- XVar
    XVar <- l
    rm(l)
  }

  if(length(GroupVar) == 0L) TimeLine <- FALSE

  # Used multiple times
  check1 <- length(XVar) != 0 && length(YVar) != 0 && length(GroupVar) > 0L

  if(!PreAgg) {
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)
  }

  # Create base plot object
  numvars <- c()
  byvars <- c()
  if(check1) {
    if(!PreAgg) {

      if(length(FacetLevels) > 0L) {
        dt <- dt[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,XVar,GroupVar)]
      }

      if(any(tryCatch({class(dt[[eval(YVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        numvars <- unique(c(numvars, YVar))
      } else {
        byvars <- unique(c(byvars, YVar))
      }
      if(any(tryCatch({class(dt[[eval(XVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        if(length(numvars) > 0) {
          x <- length(unique(dt[[XVar]]))
          y <- length(unique(dt[[YVar]]))
          if(x > y) {
            byvars <- unique(c(byvars, YVar))
            numvars[1L] <- XVar
          } else {
            byvars <- unique(c(byvars, XVar))
          }
        } else {
          numvars <- unique(c(numvars, XVar))
        }
      } else {
        byvars <- unique(c(byvars, XVar))
      }
      if(any(tryCatch({class(dt[[eval(GroupVar)]])}, error = function(x) "bla") %in% c('numeric','integer'))) {
        dt[, eval(GroupVar) := as.character(get(GroupVar))]
        byvars <- unique(c(byvars, GroupVar))
      } else {
        byvars <- unique(c(byvars, GroupVar))
      }
      if(!is.null(byvars)) {
        temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars), by = c(byvars)]
        for(i in byvars) {
          if(class(temp[[i]]) %in% c('numeric','integer')) {
            temp[, eval(i) := as.character(get(i))]
          }
        }
      } else {
        temp <- dt[, lapply(.SD, noquote(aggFunc)), .SDcols = c(numvars)]
      }
    } else {
      temp <- data.table::copy(dt)
      numvars <- AutoPlots:::ColNameFilter(data = temp, Types = 'numeric')[[1L]]
      byvars <- unlist(AutoPlots:::ColNameFilter(data = temp, Types = "character"))
    }

    # Transformation
    if(length(XVar) > 0L && class(temp[[XVar]])[1L] %in% c("numeric","integer")) {
      YVarTrans <- XVarTrans
    }
    if(YVarTrans != "Identity") {
      temp <- AutoTransformationCreate(data = temp, ColumnNames = numvars, Methods = YVarTrans)$Data
    }

    p1 <- echarts4r::e_charts_(
      data = temp |> dplyr::group_by(get(GroupVar[1L])),
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      emphasis = list(focus = "series"),
      width = Width,
      height = Height)
    if(ShowLabels) {
      p1 <- echarts4r::e_bar_(
        e = p1,
        YVar,
        stack = XVar,
        label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_bar_(
        e = p1,
        YVar,
        stack = XVar)
    }
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
    if(FacetRows > 1L || FacetCols > 1L) {
      p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    } else {
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    }
    return(p1)

  } else {
    if(Debug) print("XVar, YVar, and GroupVar need to have length > 0")
  }
}

#' @title Plot.BarPlot3D
#'
#' @description Build a 3D Bar Plot
#'
#' @family Standard Plots
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param ZVar Z-Axis variable name
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param EchartsTheme "dark-blue"
#' @param AggMethod 'mean', 'median', 'sum', 'sd', 'coeffvar', 'count'
#' @param NumberBins = 21
#' @param NumLevels_Y = 20
#' @param NumLevels_X = 20
#' @param Title "Heatmap"
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Debug Debugging purposes
#' @export
Plot.BarPlot3D <- function(dt,
                           PreAgg = FALSE,
                           AggMethod = 'mean',
                           XVar = NULL,
                           YVar = NULL,
                           ZVar = NULL,
                           YVarTrans = "Identity",
                           XVarTrans = "Identity",
                           ZVarTrans = "Identity",
                           FacetRows = 1,
                           FacetCols = 1,
                           FacetLevels = NULL,
                           NumberBins = 21,
                           NumLevels_Y = 33,
                           NumLevels_X = 33,
                           Height = NULL,
                           Width = NULL,
                           Title = "3D Bar Plot",
                           ShowLabels = FALSE,
                           Title.YAxis = NULL,
                           Title.XAxis = NULL,
                           EchartsTheme = "dark",
                           X_Scroll = TRUE,
                           Y_Scroll = TRUE,
                           TextColor =        "white",
                           title.fontSize = 22,
                           title.fontWeight = "bold", # normal
                           title.textShadowColor = '#63aeff',
                           title.textShadowBlur = 3,
                           title.textShadowOffsetY = 1,
                           title.textShadowOffsetX = -1,
                           yaxis.fontSize = 14,
                           xaxis.fontSize = 14,
                           zaxis.fontSize = 14,
                           xaxis.rotate = 0,
                           yaxis.rotate = 0,
                           ContainLabel = TRUE,
                           Debug = FALSE) {

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # Convert factor to character
  if(length(ZVar) > 0L && class(dt[[ZVar]])[1L] %in% c("factor","character")) {
    dt[, eval(ZVar) := as.numeric(get(ZVar))]
  }
  if(length(XVar) > 0L && class(dt[[XVar]])[1L] == "factor") {
    dt[, eval(XVar) := as.character(get(XVar))]
  }
  if(length(YVar) > 0L && class(dt[[YVar]])[1L] == "factor") {
    dt[, eval(YVar) := as.character(get(YVar))]
  }

  # Subset cols
  dt1 <- dt[, .SD, .SDcols = c(XVar,YVar,ZVar)]
  x_check <- class(dt1[[XVar]])[1L] %in% c('numeric','integer')
  y_check <- class(dt1[[YVar]])[1L] %in% c('numeric','integer')
  x_y_num <- x_check && y_check
  x_num <- x_check && !y_check
  x_char <- !x_check && y_check
  all_char <- !x_check && !y_check


  Z.HoverFormat <- "%{zaxis.title.text}: %{y:,.2f}<br>"

  TimeLine <- FALSE
  if(TimeLine && length(FacetLevels) > 0) X_Scroll <- FALSE

  if(!PreAgg) {
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)
  }

  # XVar == numeric or integer && YVar == numeric or integer
  if(x_y_num) {

    # rank XVar and YVar
    if(!PreAgg) {
      dt1[, eval(XVar) := round(data.table::frank(dt1[[XVar]]) * NumberBins /.N) / NumberBins]
      dt1[, eval(YVar) := round(data.table::frank(dt1[[YVar]]) * NumberBins /.N) / NumberBins]
      data.table::setnames(dt1, eval(ZVar), 'Measure_Variable')
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), .SDcols = c(ZVar), by = c(XVar,YVar)]
    }

    # Transformation
    if(ZVarTrans != "Identity") {
      dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = "Measure_Variable", Methods = ZVarTrans)$Data
    }

    # Formatting
    vals <- unique(scales::rescale(c(dt1[['Measure_Variable']])))
    o <- order(vals, decreasing = FALSE)
    cols <- scales::col_numeric("Purples", domain = NULL)(vals)
    colz <- setNames(data.frame(vals[o], cols[o]), NULL)

    # Create final data for plot
    g <- "Measure_Variable"
    p1 <- echarts4r::e_charts_(
      data = dt1,
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_bar_3d_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)), label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_bar_3d_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
    }

    p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
    if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")


    return(p1)
  }

  # XVar == character && YVar == numeric or integer
  if(x_char) {

    # rank YVar
    data.table::setnames(dt1, eval(ZVar), 'Measure_Variable')
    if(!PreAgg) {
      dt1[, eval(YVar) := round(data.table::frank(dt1[[YVar]]) * NumberBins /.N) / NumberBins]
      temp <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('Measure_Variable'), by = c(YVar)][order(-Measure_Variable)]
      temp <- temp[seq_len(min(NumLevels_X, temp[, .N]))][[1L]]
      dt1 <- dt1[get(YVar) %in% eval(temp)]
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), .SDcols = c(ZVar), by = c(XVar,YVar)]
    }

    # Formatting
    vals <- unique(scales::rescale(c(dt1[['Measure_Variable']])))
    o <- order(vals, decreasing = FALSE)
    cols <- scales::col_numeric("Purples", domain = NULL)(vals)
    colz <- setNames(data.frame(vals[o], cols[o]), NULL)

    # Transformation
    if(ZVarTrans != "Identity") {
      dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = "Measure_Variable", Methods = ZVarTrans)$Data
    }

    # Create final data for plot
    g <- "Measure_Variable"
    p1 <- echarts4r::e_charts_(
      data = dt1,
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)), label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
    }

    p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
    if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")


    return(p1)
  }

  # XVar == numeric or integer && YVar == character
  if(x_num) {

    # rank XVar
    if(!PreAgg) {
      dt1[, eval(XVar) := round(data.table::frank(dt1[[XVar]]) * NumberBins /.N) / NumberBins]
      data.table::setnames(dt1, eval(ZVar), 'Measure_Variable')

      # Top YVar Levels
      temp <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('Measure_Variable'), by = c(YVar)][order(-Measure_Variable)]
      temp <- temp[seq_len(min(NumLevels_Y, temp[, .N]))][[1L]]
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), .SDcols = c(ZVar), by = c(XVar,YVar)]


      # Transformation
      if(ZVarTrans != "Identity") {
        dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = "Measure_Variable", Methods = ZVarTrans)$Data
      }

      # Formatting
      dt1 <- dt1[get(YVar) %in% eval(temp)]
      vals <- unique(scales::rescale(c(dt1[['Measure_Variable']])))
      o <- order(vals, decreasing = FALSE)
      cols <- scales::col_numeric("Purples", domain = NULL)(vals)
      colz <- setNames(data.frame(vals[o], cols[o]), NULL)
    }

    # Create final dt1 for plot
    g <- "Measure_Variable"
    p1 <- echarts4r::e_charts_(
      data = dt1,
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)), label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
    }
    p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
    p1 <- echarts4r::e_datazoom(e = p1, y_index = c(0,1))
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
    if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")


    return(p1)
  }

  # XVar == character or integer && YVar == character
  if(all_char) {

    # Starter pack
    if(!PreAgg) {
      temp1 <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
      temp1 <- temp1[seq_len(min(NumLevels_Y, temp1[, .N]))][[1L]]
      temp2 <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
      temp2 <- temp2[seq_len(min(NumLevels_X, temp2[, .N]))][[1L]]
      dt1 <- dt1[get(YVar) %in% eval(temp1) & get(XVar) %in% eval(temp2), lapply(.SD, noquote(aggFunc)), .SDcols = c(ZVar), by = c(XVar,YVar)]
    }

    # Transformation
    if(length(ZVarTrans) > 0 && ZVarTrans != "Identity") {
      dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = ZVar, Methods = ZVarTrans)$Data
    }

    if(XVar %in% c("Predict","p1")) data.table::setorderv(x = dt1, "Predict")
    p1 <- echarts4r::e_charts_(
      data = dt1,
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_bar_3d_(e = p1, YVar, ZVar, coord_system = "cartesian3D", itemStyle = list(emphasis = list(shadowBlur = 10)), label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_bar_3d_(e = p1, YVar, ZVar, coord_system = "cartesian3D", itemStyle = list(emphasis = list(shadowBlur = 10)))
    }

    p1 <- echarts4r::e_visual_map_(e = p1, ZVar, show = FALSE)
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    # They do nothing for this plot type
    # p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))  # They do nothing for this plot type
    # p1 <- echarts4r::e_datazoom(e = p1, y_index = c(0,1))
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
    if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
    return(p1)
  }
}

#' @title Plot.HeatMap
#'
#' @description Create heat maps with numeric or categorical dt
#'
#' @family Standard Plots
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param ZVar Z-Axis variable name
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param EchartsTheme "dark-blue"
#' @param AggMethod 'mean', 'median', 'sum', 'sd', 'coeffvar', 'count'
#' @param NumberBins = 21
#' @param NumLevels_Y = 20
#' @param NumLevels_X = 20
#' @param Title "Heatmap"
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @export
Plot.HeatMap <- function(dt,
                         PreAgg = FALSE,
                         AggMethod = 'mean',
                         XVar = NULL,
                         YVar = NULL,
                         ZVar = NULL,
                         YVarTrans = "Identity",
                         XVarTrans = "Identity",
                         ZVarTrans = "Identity",
                         FacetRows = 1,
                         FacetCols = 1,
                         FacetLevels = NULL,
                         NumberBins = 21,
                         NumLevels_Y = 33,
                         NumLevels_X = 33,
                         Height = NULL,
                         Width = NULL,
                         Title = "Heatmap",
                         ShowLabels = FALSE,
                         Title.YAxis = NULL,
                         Title.XAxis = NULL,
                         EchartsTheme = "dark",
                         X_Scroll = TRUE,
                         Y_Scroll = TRUE,
                         TextColor =        "white",
                         title.fontSize = 22,
                         title.fontWeight = "bold", # normal
                         title.textShadowColor = '#63aeff',
                         title.textShadowBlur = 3,
                         title.textShadowOffsetY = 1,
                         title.textShadowOffsetX = -1,
                         yaxis.fontSize = 14,
                         xaxis.fontSize = 14,
                         xaxis.rotate = 0,
                         yaxis.rotate = 0,
                         ContainLabel = TRUE,
                         Debug = FALSE) {

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # Convert factor to character
  if(length(ZVar) > 0L && class(dt[[ZVar]])[1L] %in% c("factor","character")) {
    dt[, eval(ZVar) := as.numeric(get(ZVar))]
  }
  if(length(XVar) > 0L && class(dt[[XVar]])[1L] == "factor") {
    dt[, eval(XVar) := as.character(get(XVar))]
  }
  if(length(YVar) > 0L && class(dt[[YVar]])[1L] == "factor") {
    dt[, eval(YVar) := as.character(get(YVar))]
  }

  # Subset cols
  dt1 <- dt[, .SD, .SDcols = c(XVar,YVar,ZVar)]
  x_check <- class(dt1[[XVar]])[1L] %in% c('numeric','integer')
  y_check <- class(dt1[[YVar]])[1L] %in% c('numeric','integer')
  x_y_num <- x_check && y_check
  x_num <- x_check && !y_check
  x_char <- !x_check && y_check
  all_char <- !x_check && !y_check


  Z.HoverFormat <- "%{zaxis.title.text}: %{y:,.2f}<br>"

  # XVar == numeric or integer && YVar == numeric or integer
  if(x_y_num) {

    # rank XVar and YVar
    if(!PreAgg) {
      dt1[, eval(XVar) := round(data.table::frank(dt1[[XVar]]) * NumberBins /.N) / NumberBins]
      dt1[, eval(YVar) := round(data.table::frank(dt1[[YVar]]) * NumberBins /.N) / NumberBins]
    }

    # Transformation
    if(ZVarTrans != "Identity") {
      dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = ZVar, Methods = ZVarTrans)$Data
    }

    # Formatting
    vals <- unique(scales::rescale(c(dt1[[ZVar]])))
    o <- order(vals, decreasing = FALSE)
    cols <- scales::col_numeric("Purples", domain = NULL)(vals)
    colz <- setNames(data.frame(vals[o], cols[o]), NULL)
    data.table::setnames(dt1, ZVar, "Measure_Variable")
    data.table::setorderv(x = dt1, cols = c(XVar,YVar),c(1L,1L))

    # Create final data for plot
    g <- "Measure_Variable"
    p1 <- echarts4r::e_charts_(
      data = dt1,
      x = XVar,
      darkMode = TRUE,
      width = Width,
      height = Height)#, dispose = TRUE)

    if(ShowLabels) {
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)), label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
    }

    p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))

    return(p1)
  }

  # XVar == character && YVar == numeric or integer
  if(x_char) {

    # rank YVar
    if(!PreAgg) {
      dt1[, eval(YVar) := round(data.table::frank(dt1[[YVar]]) * NumberBins /.N) / NumberBins]
      data.table::setnames(dt1, eval(ZVar), 'Measure_Variable')

      # Top XVar Levels
      temp <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('Measure_Variable'), by = c(XVar)][order(-Measure_Variable)]
      temp <- temp[seq_len(min(NumLevels_X, temp[, .N]))][[1L]]
      dt1 <- dt1[get(XVar) %in% eval(temp)]

      # Transformation
      if(ZVarTrans != "Identity") {
        dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = "Measure_Variable", Methods = ZVarTrans)$Data
      }

      # Formatting
      vals <- unique(scales::rescale(c(dt1[['Measure_Variable']])))
      o <- order(vals, decreasing = FALSE)
      cols <- scales::col_numeric("Purples", domain = NULL)(vals)
      colz <- setNames(data.frame(vals[o], cols[o]), NULL)
    }

    # Create final data for plot
    g <- "Measure_Variable"
    p1 <- echarts4r::e_charts_(
      data = dt1,
      x = XVar,
      darkMode = TRUE,
      dispose = TRUE,
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)), label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
    }

    p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))

    return(p1)
  }

  # XVar == numeric or integer && YVar == character
  if(x_num) {

    # rank XVar
    if(!PreAgg) {
      dt1[, eval(XVar) := round(data.table::frank(dt1[[XVar]]) * NumberBins /.N) / NumberBins]
      data.table::setnames(dt1, eval(ZVar), 'Measure_Variable')

      # Top YVar Levels
      temp <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c('Measure_Variable'), by = c(YVar)][order(-Measure_Variable)]
      temp <- temp[seq_len(min(NumLevels_Y, temp[, .N]))][[1L]]

      # Transformation
      if(ZVarTrans != "Identity") {
        dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = "Measure_Variable", Methods = ZVarTrans)$Data
      }

      # Formatting
      dt1 <- dt1[get(YVar) %in% eval(temp)]
      vals <- unique(scales::rescale(c(dt1[['Measure_Variable']])))
      o <- order(vals, decreasing = FALSE)
      cols <- scales::col_numeric("Purples", domain = NULL)(vals)
      colz <- setNames(data.frame(vals[o], cols[o]), NULL)
    }

    # Create final dt1 for plot
    g <- "Measure_Variable"
    p1 <- echarts4r::e_charts_(
      data = dt1,
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)), label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, g, itemStyle = list(emphasis = list(shadowBlur = 10)))
    }

    p1 <- echarts4r::e_visual_map_(e = p1, g, show = FALSE)
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
    p1 <- echarts4r::e_datazoom(e = p1, y_index = c(0,1))
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = yaxis.fontSize))
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))

    return(p1)
  }

  # XVar == character or integer && YVar == character
  if(all_char) {

    # Starter pack
    if(!PreAgg) {
      if(Debug) print("Echarts PreAgg 1")
      if(AggMethod == 'mean') {
        temp_y <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        temp_x <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        temp_yy <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        temp_xx <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        dt1 <- dt1[get(YVar) %in% eval(temp_yy) & get(XVar) %in% eval(temp_xx)]
        dt1 <- dt1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
      } else if(AggMethod == 'median') {
        temp_y <- dt1[, lapply(.SD, median, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        temp_x <- dt1[, lapply(.SD, median, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        temp_y <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        temp_x <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        dt1 <- dt1[get(YVar) %in% eval(temp_y) & get(XVar) %in% eval(temp_x)]
        dt1 <- dt1[, lapply(.SD, median, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
      } else if(AggMethod == 'sum') {
        temp_y <- dt1[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        temp_x <- dt1[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        temp_y <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        temp_x <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        dt1 <- dt1[get(YVar) %in% eval(temp_y) & get(XVar) %in% eval(temp_x)]
        dt1 <- dt1[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
      } else if(AggMethod == 'sd') {
        temp_y <- dt1[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        temp_x <- dt1[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        temp_y <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        temp_x <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        dt1 <- dt1[get(YVar) %in% eval(temp_y) & get(XVar) %in% eval(temp_x)]
        dt1 <- dt1[, lapply(.SD, sd, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
      } else if(AggMethod == 'coeffvar') {
        temp_y <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        temp_x <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        temp_y <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        temp_x <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        dt1 <- dt1[get(YVar) %in% eval(temp_y) & get(XVar) %in% eval(temp_x)]
        dt1 <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
      } else if(AggMethod == 'count') {
        temp_y <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(YVar)][order(-get(ZVar))]
        temp_x <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar)][order(-get(ZVar))]
        temp_y <- temp_y[seq_len(min(NumLevels_X, temp_y[, .N]))][[1L]]
        temp_x <- temp_x[seq_len(min(NumLevels_Y, temp_x[, .N]))][[1L]]
        dt1 <- dt1[get(YVar) %in% eval(temp_y) & get(XVar) %in% eval(temp_x)]
        dt1 <- dt1[, lapply(.SD, .N, na.rm = TRUE), .SDcols = c(ZVar), by = c(XVar,YVar)]
      }
    }

    # Transformation
    if(ZVarTrans != "Identity") {
      dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = ZVar, Methods = ZVarTrans)$Data
    }

    # Create final dt1 for plot
    if(XVar %in% c("Predict","p1")) data.table::setorderv(x = dt1, "Predict")
    p1 <- echarts4r::e_charts_(
      data = dt1,
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, ZVar, itemStyle = list(emphasis = list(shadowBlur = 10)), label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_heatmap_(e = p1, YVar, ZVar, itemStyle = list(emphasis = list(shadowBlur = 10)))
    }

    p1 <- echarts4r::e_visual_map_(e = p1, ZVar, show = FALSE)
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
    p1 <- echarts4r::e_datazoom(e = p1, y_index = c(0,1))
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))

    return(p1)
  }
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# > Relationships Plot Functions                                              ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Plot.CorrMatrix
#'
#' @description Build a correlation matrix plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param CorrVars vector of variable names
#' @param CorrVarsTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Method character
#' @param MaxNAPercent numeric
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param PreAgg logical
#' @param TextColor character hex
#' @param Debug Debugging purposes
#' @export
Plot.CorrMatrix <- function(dt = NULL,
                            CorrVars = NULL,
                            CorrVarTrans = "Identity",
                            FacetRows = 1,
                            FacetCols = 1,
                            FacetLevels = NULL,
                            Method = 'spearman',
                            PreAgg = FALSE,
                            MaxNAPercent = 0.05,
                            Height = NULL,
                            Width = NULL,
                            Title = "Correlation Matrix",
                            ShowLabels = FALSE,
                            Title.YAxis = NULL,
                            Title.XAxis = NULL,
                            EchartsTheme = "macarons",
                            X_Scroll = TRUE,
                            Y_Scroll = TRUE,
                            TextColor =        "white",
                            title.fontSize = 22,
                            title.fontWeight = "bold", # normal
                            title.textShadowColor = '#63aeff',
                            title.textShadowBlur = 3,
                            title.textShadowOffsetY = 1,
                            title.textShadowOffsetX = -1,
                            yaxis.fontSize = 14,
                            xaxis.fontSize = 14,
                            Debug = FALSE) {

  # Filter out bad vars
  x <- c(); for(i in CorrVars) if(dt[, sd(get(i), na.rm = TRUE)] > 0L) x <- c(x, i)
  CorrVars <- x
  NN <- dt[,.N]
  x <- c(); for(i in CorrVars) if(sum(dt[, is.na(get(i))]) / NN <= MaxNAPercent) x <- c(x, i)
  CorrVars <- x

  # Plot
  if(!PreAgg) {
    if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
      dt <- data.table::as.data.table(dt)
    })
    dt1 <- na.omit(dt[, .SD, .SDcols = c(CorrVars)])

    # Transformation
    if(CorrVarTrans != "Identity") {
      dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = CorrVars, Methods = CorrVarTrans)$Data
    }
    for(i in seq_along(names(dt1))) {
      yy <- names(dt1)[i]
      zz <- nchar(yy)
      data.table::setnames(dt1, yy, substr(x = yy, start = max(0L, zz - 40L), stop = nchar(yy)))
    }
    corr_mat <- cor(method = tolower(Method), x = dt1)
  } else {
    corr_mat <- dt
  }

  if(Debug) {
    print("Plot.CorrMatrix Echarts")
    print(Width)
    print(Height)
    print(corr_mat)
  }

  p1 <- echarts4r::e_charts(data = corr_mat, width = Width, height = Height)
  p1 <- echarts4r::e_correlations(e = p1, order = "hclust")
  p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
  if(FacetRows == 1L && FacetCols == 1L) {
    if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
    if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
  }
  p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
  p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
  p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
  p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
  p1 <- echarts4r::e_brush(e = p1)
  p1 <- echarts4r::e_title(
    p1, Title,
    textStyle = list(
      color = TextColor,
      fontWeight = title.fontWeight,
      overflow = "truncate", # "none", "truncate", "break",
      ellipsis = '...',
      fontSize = title.fontSize,
      textShadowColor = title.textShadowColor,
      textShadowBlur = title.textShadowBlur,
      textShadowOffsetY = title.textShadowOffsetY,
      textShadowOffsetX = title.textShadowOffsetX))

  # Return plot
  return(p1)
}

#' @title Plot.Parallel
#'
#' @description Build a parallel plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param CorrVars vector of variable names
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param PreAgg logical
#' @param TextColor character hex
#' @param Debug Debugging purposes
#'
#' @examples
#' \dontrun{
#' dt = data.table::fread(file.choose())
#' SampleSize = 5000
#' CorrVars = c("Customer","Brand","Category") #names(dt)[!names(dt) %in% "Date"]
#' CorrVarTrans = "Identity"
#' FacetRows = 1
#' FacetCols = 1
#' FacetLevels = NULL
#' PreAgg = FALSE
#' Height = "400px"
#' Width = "600px"
#' Title = "Parallel Plot"
#' ShowLabels = FALSE
#' Title.YAxis = "bla"
#' Title.XAxis = "bloke"
#' EchartsTheme = "macarons"
#' X_Scroll = TRUE
#' Y_Scroll = TRUE
#' TextColor = "white"
#' title.fontSize = 22
#' title.fontWeight = "bold"
#' title.textShadowColor = '#63aeff'
#' title.textShadowBlur = 3
#' title.textShadowOffsetY = 1
#' title.textShadowOffsetX = -1
#' yaxis.fontSize = 14
#' xaxis.fontSize = 14
#' Debug = FALSE
#'
#' AutoPlots::Plot.Parallel(
#'   dt = dt,
#'   CorrVars = CorrVars,
#'   FacetRows = FacetRows,
#'   FacetCols = FacetCols,
#'   FacetLevels = FacetLevels,
#'   PreAgg = PreAgg,
#'   Height = Height,
#'   Width = Width,
#'   Title = Title,
#'   ShowLabels = ShowLabels,
#'   Title.YAxis = Title.YAxis,
#'   Title.XAxis = Title.XAxis,
#'   EchartsTheme = EchartsTheme,
#'   X_Scroll = X_Scroll,
#'   Y_Scroll = Y_Scroll,
#'   TextColor = TextColor,
#'   title.fontSize = title.fontSize,
#'   title.fontWeight = title.fontWeight,
#'   title.textShadowColor = title.textShadowColor,
#'   title.textShadowBlur = title.textShadowBlur,
#'   title.textShadowOffsetY = title.textShadowOffsetY,
#'   title.textShadowOffsetX = title.textShadowOffsetX,
#'   yaxis.fontSize = yaxis.fontSize,
#'   xaxis.fontSize = xaxis.fontSize,
#'   Debug = Debug
#' )
#' }
#'
#' @export
Plot.Parallel <- function(dt = NULL,
                          SampleSize = 50000,
                          CorrVars = NULL,
                          FacetRows = 1,
                          FacetCols = 1,
                          FacetLevels = NULL,
                          PreAgg = FALSE,
                          Height = NULL,
                          Width = NULL,
                          Title = "Parallel Plot",
                          ShowLabels = FALSE,
                          Title.YAxis = NULL,
                          Title.XAxis = NULL,
                          EchartsTheme = "macarons",
                          X_Scroll = TRUE,
                          Y_Scroll = TRUE,
                          TextColor =        "white",
                          title.fontSize = 22,
                          title.fontWeight = "bold", # normal
                          title.textShadowColor = '#63aeff',
                          title.textShadowBlur = 3,
                          title.textShadowOffsetY = 1,
                          title.textShadowOffsetX = -1,
                          yaxis.fontSize = 14,
                          xaxis.fontSize = 14,
                          Debug = FALSE) {

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # Plot
  if(!PreAgg) {
    if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
      dt <- data.table::as.data.table(dt)
    })
    dt1 <- na.omit(dt[, .SD, .SDcols = c(CorrVars)])

  } else {
    dt1 <- dt
  }

  if(length(SampleSize) > 0L && dt1[,.N] > SampleSize) {
    dt1 <- dt1[order(runif(.N))][seq_len(SampleSize)]
  }

  if(Debug) {
    print("Plot.CorrMatrix Echarts")
    print(Width)
    print(Height)
  }

  # Names modification: because of the parse() I can't have spaces in the colnames
  old <- c()
  new <- c()
  for(i in seq_along(CorrVars)) {
    if(grepl(pattern = " ", x = CorrVars[i])) {
      old <- c(old, CorrVars[i])
      new <- c(new, gsub(pattern = " ", replacement = ".", x = CorrVars[i]))
    }
  }
  if(length(new) > 0L) {
    CorrVars <- new
    data.table::setnames(dt1, old = old, new = new)
  }

  # Build Plot
  p1 <- echarts4r::e_charts(data = dt1, width = Width, height = Height)

  # Metaprog because issue with function accepting vector of names
  p1 <- eval(
    parse(
      text = c(
        "echarts4r::e_parallel_(e = p1, ",
        noquote(
          c(
            paste0(CorrVars[seq_len(length(CorrVars)-1L)], collpase = ","),
            CorrVars[length(CorrVars)])
        ),
        ", opts = list(smooth = TRUE))"
      )
    )
  )

  # Warning message:
  #   Using an external vector in selections was deprecated in tidyselect 1.1.0.
  # ℹ Please use `all_of()` or `any_of()` instead.
  # # Was:
  # data %>% select(v)
  #
  # # Now:
  # data %>% select(all_of(v))
  #
  # See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
  # This warning is displayed once every 8 hours.
  # Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

  p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
  if(FacetRows == 1L && FacetCols == 1L) {
    if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
    if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
  }
  p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
  p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
  p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
  p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
  p1 <- echarts4r::e_brush(e = p1)
  p1 <- echarts4r::e_title(
    p1, Title,
    textStyle = list(
      color = TextColor,
      fontWeight = title.fontWeight,
      overflow = "truncate", # "none", "truncate", "break",
      ellipsis = '...',
      fontSize = title.fontSize,
      textShadowColor = title.textShadowColor,
      textShadowBlur = title.textShadowBlur,
      textShadowOffsetY = title.textShadowOffsetY,
      textShadowOffsetX = title.textShadowOffsetX))

  # Return plot
  return(p1)
}


#' @title Plot.Copula
#'
#' @description Build a copula plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#' @param dt source data.table
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Requires an XVar and YVar already be defined
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title 'Copula Plot'
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme = "dark-blue",
#' @param TimeLine Logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor 'darkblue'
#' @param Debug Debugging purposes
#' @export
Plot.Copula <- function(dt = NULL,
                        SampleSize = 30000L,
                        XVar = NULL,
                        YVar = NULL,
                        GroupVar = NULL,
                        YVarTrans = "Identity",
                        XVarTrans = "Identity",
                        FacetRows = 1,
                        FacetCols = 1,
                        FacetLevels = NULL,
                        Height = NULL,
                        Width = NULL,
                        Title = 'Copula Plot',
                        ShowLabels = FALSE,
                        AddGLM = FALSE,
                        Title.YAxis = NULL,
                        Title.XAxis = NULL,
                        EchartsTheme = "dark-blue",
                        TimeLine = FALSE,
                        X_Scroll = TRUE,
                        Y_Scroll = TRUE,
                        TextColor =        "white",
                        yaxis.fontSize = 14,
                        xaxis.fontSize = 14,
                        title.fontSize = 22,
                        title.fontWeight = "bold", # normal
                        title.textShadowColor = '#63aeff',
                        title.textShadowBlur = 3,
                        title.textShadowOffsetY = 1,
                        title.textShadowOffsetX = -1,
                        xaxis.rotate = 0,
                        yaxis.rotate = 0,
                        ContainLabel = TRUE,
                        Debug = FALSE) {

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  if(length(GroupVar) == 0L) TimeLine <- FALSE

  if(TimeLine && length(FacetLevels) > 0) X_Scroll <- FALSE

  # Cap number of records
  if(Debug) print('Plot.Copula # Cap number of records')
  if(dt[,.N] > SampleSize) {
    dt1 <- dt[order(runif(.N))][seq_len(SampleSize)]
  } else {
    dt1 <- data.table::copy(dt)
  }
  dt1[, eval(YVar) := data.table::frank(get(YVar)) * (1 / 0.001) / .N * 0.001]
  dt1[, eval(XVar) := data.table::frank(get(XVar)) * (1 / 0.001) / .N * 0.001]

  if(length(GroupVar) == 0L) {
    if(Debug) print('Plot.Copula length(GroupVar) == 0L')

    if(Debug) print('Plot.Copula Echarts')
    dt1[, size_vals := seq_len(.N)/1000]
    sv <- "size_vals"
    p1 <- echarts4r::e_charts_(
      dt1,
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_scatter_(e = p1, YVar, color = YVar, label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_scatter_(e = p1, YVar, color = YVar)
    }

    # Add GLM
    if(AddGLM) {
      p1 <- echarts4r::e_glm(
        e = p1,
        smooth = TRUE,
        formula = get(YVar) ~ get(XVar))
    }

    p1 <- echarts4r::e_visual_map_(e = p1, scale = echarts4r::e_scale, show = FALSE)
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
    p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))


  } else {

    if(length(FacetLevels) > 0L) {
      dt1 <- dt1[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,XVar,GroupVar)]
    }

    if(Debug) print('Plot.Copula length(GroupVar) > 0L')
    if(Debug) print('Plot.Copula Echarts')
    if(TimeLine) {
      p1 <- echarts4r::e_charts_(
        dt1 |> dplyr::group_by(get(GroupVar[1L])),
        x = XVar,
        colorBy = GroupVar[1L],
        timeline = TRUE,
        darkMode = TRUE,
        emphasis = list(focus = "series"),
        dispose = TRUE,
        width = Width,
        height = Height)
    } else {
      p1 <- echarts4r::e_charts_(
        dt1 |> dplyr::group_by(get(GroupVar[1L])),
        x = XVar,
        dispose = TRUE,
        #darkMode = TRUE,
        emphasis = list(focus = "series"),
        width = Width,
        height = Height)
    }
    p1 <- echarts4r::e_scatter_(e = p1, YVar)

    # Add GLM
    if(AddGLM) {
      p1 <- echarts4r::e_glm(
        e = p1,
        smooth = TRUE,
        formula = get(YVar) ~ get(XVar))
    }

    p1 <- echarts4r::e_visual_map_(e = p1, scale = echarts4r::e_scale, show = FALSE)
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = XVar)
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
    if(FacetRows > 1L || FacetCols > 1L) {
      p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    } else {
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    }


  }

  # Return plot
  return(p1)
}

#' @title Plot.Copula3D
#'
#' @description Build a 3D-copula plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param ZVar Z-Axis variable name
#' @param GroupVar Requires an XVar and YVar already be defined
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title 'Copula3D Plot'
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme = "dark-blue"
#' @param TimeLine Logical
#' @param TextColor 'darkblue'
#' @param Debug Debugging purposes
#' @export
Plot.Copula3D <- function(dt = NULL,
                          SampleSize = 100000,
                          XVar = NULL,
                          YVar = NULL,
                          ZVar = NULL,
                          YVarTrans = "Identity",
                          XVarTrans = "Identity",
                          ZVarTrans = "Identity",
                          FacetRows = 1,
                          FacetCols = 1,
                          FacetLevels = NULL,
                          GroupVar = NULL,
                          Height = NULL,
                          Width = NULL,
                          Title = 'Copula 3D',
                          ShowLabels = FALSE,
                          Title.YAxis = NULL,
                          Title.XAxis = NULL,
                          EchartsTheme = "dark-blue",
                          TimeLine = FALSE,
                          TextColor =        "white",
                          title.fontSize = 22,
                          title.fontWeight = "bold", # normal
                          title.textShadowColor = '#63aeff',
                          title.textShadowBlur = 3,
                          title.textShadowOffsetY = 1,
                          title.textShadowOffsetX = -1,
                          yaxis.fontSize = 14,
                          xaxis.fontSize = 14,
                          zaxis.fontSize = 14,
                          xaxis.rotate = 0,
                          yaxis.rotate = 0,
                          zaxis.rotate = 0,
                          ContainLabel = TRUE,
                          Debug = FALSE) {

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  if(length(GroupVar) == 0L) TimeLine <- FALSE

  # Cap number of records
  if(Debug) print('Plot.Copula3D # Cap number of records')
  N <- dt[,.N]
  if(SampleSize > 50000L) SampleSize <- 50000L
  if(N > SampleSize) dt <- dt[order(runif(.N))][seq_len(SampleSize)]
  dt1 <- data.table::copy(dt)
  dt1[, eval(YVar) := data.table::frank(get(YVar)) * (1 / 0.001) / .N * 0.001]
  dt1[, eval(XVar) := data.table::frank(get(XVar)) * (1 / 0.001) / .N * 0.001]
  dt1[, eval(ZVar) := data.table::frank(get(ZVar)) * (1 / 0.001) / .N * 0.001]
  if(length(GroupVar) > 0L) {
    if(Debug) print('Plot.Copula3D length(GroupVar) > 0L')

    if(Debug) print('Plot.Copula3D Echarts')
    p1 <- echarts4r::e_charts_(
      dt1 |> dplyr::group_by(get(GroupVar[1L])),
      x = XVar,
      darkMode = TRUE,
      emphasis = list(focus = "series"),
      timeline = TimeLine,
      colorBy = GroupVar[1L], dispose = TRUE, width = Width, height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, GroupVar[[1L]], label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, GroupVar[[1L]])
    }

    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor))
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
    p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))



  } else {

    if(Debug) print('Plot.Copula3D length(GroupVar) == 0L')
    if(Debug) print('Plot.Copula3D Echarts')
    p1 <- echarts4r::e_charts_(
      dt1,
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar)
    }

    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
    if(FacetRows > 1L || FacetCols > 1L) p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")


  }

  # Return plot
  return(p1)
}

#' @title Plot.Scatter
#'
#' @description Build a copula plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param SampleSize numeric
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor character hex
#' @param Debug Debugging purposes
#' @export
Plot.Scatter <- function(dt = NULL,
                         SampleSize = 30000L,
                         XVar = NULL,
                         YVar = NULL,
                         GroupVar = NULL,
                         YVarTrans = "Identity",
                         XVarTrans = "Identity",
                         FacetRows = 1,
                         FacetCols = 1,
                         FacetLevels = NULL,
                         Height = NULL,
                         Width = NULL,
                         Title = 'Scatter Plot',
                         ShowLabels = FALSE,
                         AddGLM = FALSE,
                         Title.YAxis = NULL,
                         Title.XAxis = NULL,
                         EchartsTheme = "macarons",
                         TimeLine = FALSE,
                         X_Scroll = TRUE,
                         Y_Scroll = TRUE,
                         TextColor =        "white",
                         title.fontSize = 22,
                         title.fontWeight = "bold", # normal
                         title.textShadowColor = '#63aeff',
                         title.textShadowBlur = 3,
                         title.textShadowOffsetY = 1,
                         title.textShadowOffsetX = -1,
                         yaxis.fontSize = 14,
                         xaxis.fontSize = 14,
                         xaxis.rotate = 0,
                         yaxis.rotate = 0,
                         ContainLabel = TRUE,
                         tooltip.trigger = "axis",
                         Debug = FALSE) {

  if(length(GroupVar) == 0L) TimeLine <- FALSE

  if(TimeLine && length(FacetLevels) > 0) X_Scroll <- FALSE

  # Cap number of records
  if(Debug) print('Plot.Scatter # Cap number of records')
  if(length(SampleSize) == 0L) SampleSize <- 30000L
  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })
  if(dt[,.N] > SampleSize) {
    dt1 <- dt[order(runif(.N))][seq_len(SampleSize)]
  } else {
    dt1 <- data.table::copy(dt)
  }

  # Transformation
  if(YVarTrans != "Identity") {
    dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data
  }

  # Transformation
  if(XVarTrans != "Identity") {
    dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = XVar, Methods = XVarTrans)$Data
  }

  if(length(GroupVar) == 0L) {
    if(Debug) print('Plot.Scatter  length(GroupVar) == 0L')
    if(Debug) print('Plot.Scatter  Echarts')
    p1 <- echarts4r::e_charts_(
      dt1,
      x = XVar,
      dispose = TRUE,
      darkMode = TRUE,
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_scatter_(e = p1, YVar)
    }

    # Add GLM
    if(AddGLM) {
      p1 <- echarts4r::e_glm(
        e = p1,
        smooth = TRUE,
        formula = get(YVar) ~ get(XVar))
    }

    p1 <- echarts4r::e_visual_map_(e = p1, scale = echarts4r::e_scale, show = FALSE)
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }
    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = tooltip.trigger, backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
    p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))



  } else {

    if(Debug) print("SCatter 1")
    if((FacetRows > 1L || FacetCols > 1L) && length(FacetLevels) > 0L) {
      dt1 <- dt1[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,XVar,GroupVar)]
    }

    if(Debug) print("SCatter 2")

    if(Debug) print('Plot.Scatter  length(GroupVar) > 0L')
    if(Debug) print('Plot.Scatter  Echarts')
    p1 <- echarts4r::e_charts_(
      dt1 |> dplyr::group_by(get(GroupVar[1L])),
      x = XVar,
      timeline = TimeLine,
      darkMode = TRUE,
      emphasis = list(focus = "series"),
      colorBy = GroupVar[1L], dispose = TRUE, width = Width, height = Height)

    if(Debug) print("SCatter 3")

    if(ShowLabels) {
      p1 <- echarts4r::e_scatter_(e = p1, YVar, label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_scatter_(e = p1, YVar)
    }

    if(Debug) print("SCatter 4")

    # Add GLM
    if(AddGLM) {
      p1 <- echarts4r::e_glm(
        e = p1,
        smooth = TRUE,
        formula = get(YVar) ~ get(XVar))
    }

    if(Debug) print("SCatter 5")

    p1 <- echarts4r::e_visual_map_(e = p1, scale = echarts4r::e_scale, show = FALSE)
    if(FacetRows == 1L && FacetCols == 1L) {
      if(X_Scroll && length(GroupVar) == 0L) p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
      if(Y_Scroll) p1 <- echarts4r::e_datazoom(e = p1, y_Index = c(0,1))
    }

    if(Debug) print("SCatter 6")

    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(Debug) print("SCatter 7")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(Debug) print("SCatter 8")

    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
    if(FacetRows > 1L || FacetCols > 1L) {
      p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    } else {
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    }
  }

  # Return plot
  return(p1)
}

#' @title Plot.Scatter3D
#'
#' @description Build a 3D-copula plot by simply passing arguments to a single function. It will sample your data using SampleSize number of rows. Sampled data is randomized.
#'
#' @family Standard Plots
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param SampleSize An integer for the number of rows to use. Sampled data is randomized. If NULL then ignored
#' @param YVar Y-Axis variable name
#' @param XVar X-Axis variable name
#' @param ZVar Z-Axis variable name
#' @param GroupVar Requires an XVar and YVar already be defined
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Height = NULL,
#' @param Width = NULL,
#' @param Title 'Violin Plot'
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme = "macaron"
#' @param TimeLine Logical
#' @param TextColor 'darkblue'
#' @param Debug Debugging purposes
#' @export
Plot.Scatter3D <- function(dt = NULL,
                           SampleSize = 100000,
                           XVar = NULL,
                           YVar = NULL,
                           ZVar = NULL,
                           GroupVar = NULL,
                           YVarTrans = "Identity",
                           XVarTrans = "Identity",
                           ZVarTrans = "Identity",
                           FacetRows = 1,
                           FacetCols = 1,
                           FacetLevels = NULL,
                           Height = NULL,
                           Width = NULL,
                           Title = '3D Scatter',
                           ShowLabels = FALSE,
                           Title.YAxis = NULL,
                           Title.XAxis = NULL,
                           EchartsTheme = "macarons",
                           TimeLine = FALSE,
                           TextColor =        "white",
                           title.fontSize = 22,
                           title.fontWeight = "bold", # normal
                           title.textShadowColor = '#63aeff',
                           title.textShadowBlur = 3,
                           title.textShadowOffsetY = 1,
                           title.textShadowOffsetX = -1,
                           yaxis.fontSize = 14,
                           xaxis.fontSize = 14,
                           zaxis.fontSize = 14,
                           xaxis.rotate = 0,
                           yaxis.rotate = 0,
                           zaxis.rotate = 0,
                           ContainLabel = TRUE,
                           Debug = FALSE) {

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  if(length(GroupVar) == 0L) TimeLine <- FALSE

  # Cap number of records
  if(Debug) print('Plot.Scatter3D # Cap number of records')
  if(length(SampleSize) == 0L) SampleSize <- 30000L
  if(dt[,.N] > SampleSize) {
    dt1 <- dt[order(runif(.N))][seq_len(SampleSize)]
  } else {
    dt1 <- data.table::copy(dt)
  }

  # Transformation
  if(YVarTrans != "Identity") {
    dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data
  }

  # Transformation
  if(XVarTrans != "Identity") {
    dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = XVar, Methods = XVarTrans)$Data
  }

  # Transformation
  if(ZVarTrans != "Identity") {
    dt1 <- AutoTransformationCreate(data = dt1, ColumnNames = ZVar, Methods = ZVarTrans)$Data
  }

  if(length(GroupVar) > 0L) {
    if(Debug) print('Plot.Scatter3D length(GroupVar) > 0L')
    if(Debug) print('Plot.Scatter3D  Echarts')
    p1 <- echarts4r::e_charts_(
      dt1 |> dplyr::group_by(get(GroupVar[1L])),
      x = XVar,
      timeline = FALSE,
      colorBy = GroupVar[1L],
      dispose = TRUE,
      darkMode = TRUE,
      emphasis = list(focus = "series"),
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, GroupVar[1L], label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, GroupVar[1L])
    }

    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))
    if(FacetRows > 1L || FacetCols > 1L) {
      p1 <- echarts4r::e_facet(e = p1, rows = FacetRows, cols = FacetCols, legend_space = 16, legend_pos = "top")
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "horizontal", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    } else {
      p1 <- echarts4r::e_legend(e = p1, type = "scroll", orient = "vertical", right = 50, top = 40, height = "240px", textStyle = list(color = TextColor, fontWeight = "bold"))
    }



  } else {

    if(Debug) print('Plot.Scatter3D length(GroupVar) == 0L')
    if(Debug) print('Plot.Scatter3D  Echarts')
    p1 <- echarts4r::e_charts_(
      dt1 |> dplyr::group_by(GroupVar[[1L]]),
      x = XVar,
      timeline = FALSE,
      dispose = TRUE,
      darkMode = TRUE,
      emphasis = list(focus = "series"),
      width = Width,
      height = Height)

    if(ShowLabels) {
      p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, GroupVar[[1L]], label = list(show = TRUE))
    } else {
      p1 <- echarts4r::e_scatter_3d_(e = p1, YVar, ZVar, ZVar, GroupVar[[1L]])
    }

    p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
    p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
    p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
    p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
    p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")

    if(length(Title.XAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = XVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "x",
        name = Title.XAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = xaxis.fontSize),
        axisLabel = list(
          rotate = xaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    if(length(Title.YAxis) == 0L) {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = YVar,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    } else {
      p1 <- echarts4r::e_axis_(
        e = p1,
        serie = NULL,
        axis = "y",
        name = Title.YAxis,
        nameLocation = "middle",
        nameGap = 45,
        nameTextStyle = list(
          color = TextColor,
          fontStyle = "normal",
          fontWeight = "bold",
          fontSize = yaxis.fontSize),
        axisLabel = list(
          rotate = yaxis.rotate,
          grid = list(containLabel = ContainLabel)))
    }

    p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "z", name = ZVar, nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
    p1 <- echarts4r::e_brush(e = p1)
    p1 <- echarts4r::e_title(
      p1, Title,
      textStyle = list(
        color = TextColor,
        fontWeight = title.fontWeight,
        overflow = "truncate", # "none", "truncate", "break",
        ellipsis = '...',
        fontSize = title.fontSize,
        textShadowColor = title.textShadowColor,
        textShadowBlur = title.textShadowBlur,
        textShadowOffsetY = title.textShadowOffsetY,
        textShadowOffsetX = title.textShadowOffsetX))

  }

  # Return plot
  return(p1)
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# > Model Evaluation Plots                                                    ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Plot.Residuals.Histogram
#'
#' @description Residuals Plot
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param AggMethod character
#' @param SampleSize numeric
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param ZeroLineColor character hex
#' @param ZeroLineWidth numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor Not Implemented
#' @param Debug Debugging purposes
#' @export
Plot.Residuals.Histogram <- function(dt = NULL,
                                     AggMethod = 'mean',
                                     SampleSize = 100000,
                                     XVar = NULL,
                                     YVar = NULL,
                                     GroupVar = NULL,
                                     YVarTrans = "Identity",
                                     XVarTrans = "Identity",
                                     FacetRows = 1,
                                     FacetCols = 1,
                                     FacetLevels = NULL,
                                     NumberBins = 20,
                                     Height = NULL,
                                     Width = NULL,
                                     Title = 'Residuals Histogram',
                                     ShowLabels = FALSE,
                                     Title.YAxis = NULL,
                                     Title.XAxis = "Target - Predicted",
                                     EchartsTheme = "macarons",
                                     TimeLine = FALSE,
                                     X_Scroll = TRUE,
                                     Y_Scroll = TRUE,
                                     TextColor =        "white",
                                     title.fontSize = 22,
                                     title.fontWeight = "bold", # normal
                                     title.textShadowColor = '#63aeff',
                                     title.textShadowBlur = 3,
                                     title.textShadowOffsetY = 1,
                                     title.textShadowOffsetX = -1,
                                     xaxis.fontSize = 14,
                                     yaxis.fontSize = 14,
                                     xaxis.rotate = 0,
                                     yaxis.rotate = 0,
                                     ContainLabel = TRUE,
                                     Debug = FALSE) {

  # Subset cols, define Target - Predicted, NULL YVar in data, Update YVar def, Ensure GroupVar is length(1)
  if(length(SampleSize) == 0L) SampleSize <- 30000L
  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  if(Debug) print("here 1")
  if(Debug) print(head(dt))

  # Subset columns
  dt1 <- dt[, .SD, .SDcols = c(XVar,YVar,GroupVar)]

  # Shrink display data
  dt1 <- dt1[order(runif(.N))][seq_len(min(.N, SampleSize))]

  # Prepare data
  dt1[, `Target - Predicted` := get(YVar) - get(XVar)]
  data.table::set(dt1, j = c(YVar), value = NULL)
  YVar <- "Target - Predicted"
  if(length(GroupVar) > 0L) GroupVar <- GroupVar[1L]

  if(Debug) print("here 2")
  if(Debug) print(head(dt1))

  # Faceting shrink
  if(length(GroupVar) > 0L) {
    data.table::setorderv(x = dt1, cols = c(GroupVar), 1L)
    if(Debug) print(head(dt1))
    dt1 <- dt1[order(get(GroupVar))]
    if(Debug) print(head(dt1))
  }

  if(Debug) print("here 3.1")
  if(Debug) print(head(dt1))

  if(length(GroupVar) > 0L && (FacetRows > 1L || FacetCols > 1L)) {
    dt1 <- dt1[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,GroupVar)]
  } else {
    dt1 <- dt1[, .SD, .SDcols = c(YVar,GroupVar)]
  }

  if(Debug) print("here 3")
  if(Debug) print(head(dt1))

  # Data Prep2
  if(Debug) print("Plot.Residuals.Histogram")
  tl <- if(length(GroupVar) == 0L || length(FacetLevels) > 0) FALSE else TimeLine

  # Transformation
  # "PercRank"  "Standardize"
  # "Asinh"  "Log"  "LogPlus1"  "Sqrt"  "Asin"  "Logit"  "BoxCox"  "YeoJohnson"
  if(YVarTrans != "Identity") {
    dt1 <- tryCatch({AutoTransformationCreate(data = dt1, ColumnNames = YVar, Methods = YVarTrans)$Data}, error = function(x) dt1)
  }

  if(Debug) print("here 4")
  if(Debug) print(head(dt1))

  # Create base plot object
  if(Debug) print('Create Plot with only data')

  dt1 <- dt1[!is.na(get(YVar))]

  p1 <- AutoPlots::Plot.Histogram(
    dt = dt1,
    SampleSize = SampleSize,
    XVar = NULL,
    YVar = YVar,
    GroupVar = GroupVar,
    YVarTrans = YVarTrans,
    XVarTrans = XVarTrans,
    FacetRows = FacetRows,
    FacetCols = FacetCols,
    FacetLevels = FacetLevels,
    NumberBins = NumberBins,
    Height = Height,
    Width = Width,
    Title = Title,
    ShowLabels = ShowLabels,
    Title.YAxis = Title.YAxis,
    Title.XAxis = Title.XAxis,
    EchartsTheme = EchartsTheme,
    TimeLine = TimeLine,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    TextColor = "white",
    title.fontSize = title.fontSize,
    title.fontWeight = title.fontWeight,
    title.textShadowColor = title.textShadowColor,
    title.textShadowBlur = title.textShadowBlur,
    title.textShadowOffsetY = title.textShadowOffsetY,
    title.textShadowOffsetX = title.textShadowOffsetX,
    xaxis.fontSize = xaxis.fontSize,
    yaxis.fontSize = yaxis.fontSize,
    Debug = Debug)

  return(p1)
}

#' @title Plot.Residuals.Scatter
#'
#' @description Residuals_2 Plot
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param AggMethod character
#' @param SampleSize numeric
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor "Not Implemented"
#' @param Debug Debugging purposes
#' @export
Plot.Residuals.Scatter <- function(dt = NULL,
                                   AggMethod = 'mean',
                                   SampleSize = 100000,
                                   XVar = NULL,
                                   YVar = NULL,
                                   GroupVar = NULL,
                                   YVarTrans = "Identity",
                                   XVarTrans = "Identity",
                                   FacetRows = 1,
                                   FacetCols = 1,
                                   FacetLevels = NULL,
                                   Height = NULL,
                                   Width = NULL,
                                   Title = 'Residual Scatterplot',
                                   ShowLabels = FALSE,
                                   Title.YAxis = "Target - Predicted",
                                   Title.XAxis = "Predicted",
                                   EchartsTheme = "macarons",
                                   TimeLine = FALSE,
                                   X_Scroll = TRUE,
                                   Y_Scroll = TRUE,
                                   TextColor =        "white",
                                   Debug = FALSE) {

  # Data Prep1
  if(length(SampleSize) == 0L) SampleSize <- 30000L
  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })
  dt1 <- dt[, .SD, .SDcols = c(XVar,YVar,GroupVar)]
  if(dt1[, .N] > SampleSize) dt1 <- dt1[order(runif(.N))][seq_len(SampleSize)]
  dt1[, `Target - Predicted` := get(YVar) - get(XVar)]
  if(length(GroupVar) > 0L) GroupVar <- GroupVar[1L]
  if(length(GroupVar) > 0L) {
    dt1[, eval(XVar) := round(data.table::frank(get(XVar)) * 20 / .N) / 20, by = c(GroupVar[1L])]
  } else {
    dt1[, eval(XVar) := round(data.table::frank(get(XVar)) * 20 / .N) / 20]
  }
  YVar <- "Target - Predicted"

  # Data Prep2
  tl <- if(length(GroupVar) == 0L) FALSE else TimeLine
  data.table::setorderv(x = dt1, cols = c(GroupVar[1L], XVar))

  dt1 <- dt1[!is.na(get(YVar))]
  dt1 <- dt1[!is.na(get(XVar))]

  # Build Plot
  p1 <- AutoPlots::Plot.Scatter(
    dt = dt1,
    SampleSize = SampleSize,
    YVar = "Target - Predicted",
    XVar = XVar,
    GroupVar = GroupVar[1L],
    YVarTrans = YVarTrans,
    XVarTrans = XVarTrans,
    FacetRows = FacetRows,
    FacetCols = FacetCols,
    FacetLevels = FacetLevels,
    Height = Height,
    Title.YAxis = YVar,
    Title.XAxis = paste0(XVar, " every 5th Percentile"),
    ShowLabels = ShowLabels,
    Width = Width,
    Title = Title,
    EchartsTheme = EchartsTheme,
    TimeLine = tl,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    TextColor = TextColor,
    tooltip.trigger = "item",
    Debug = Debug)
  return(p1)
}

#' @title Plot.Calibration.Line
#'
#' @description This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param AggMethod character
#' @param SampleSize numeric
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor "Not Implemented"
#' @param Debug Debugging purposes
#' @export
Plot.Calibration.Line <- function(dt = NULL,
                                  AggMethod = 'mean',
                                  XVar = NULL,
                                  YVar = NULL,
                                  GroupVar = NULL,
                                  YVarTrans = "Identity",
                                  XVarTrans = "Identity",
                                  FacetRows = 1,
                                  FacetCols = 1,
                                  FacetLevels = NULL,
                                  NumberBins = 21,
                                  Height = NULL,
                                  Width = NULL,
                                  Title = 'Calibration Line',
                                  ShowLabels = FALSE,
                                  Title.YAxis = NULL,
                                  Title.XAxis = NULL,
                                  EchartsTheme = "macarons",
                                  TimeLine = FALSE,
                                  X_Scroll = TRUE,
                                  Y_Scroll = TRUE,
                                  TextColor = "white",
                                  Debug = FALSE) {

  if(Debug) print("here 1")

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  if(Debug) print("here 2")

  # YVar check
  y_class <- class(dt[[YVar]])[1L]

  if(Debug) print("here 3")

  # Define Aggregation function
  if(Debug) print("here 3.1")
  if(Debug) print("Plot.PartialDependence.Line # Define Aggregation function")
  if(Debug) print("here 3.2")
  aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

  if(Debug) print("here 4")

  # Regression and Classification else MultiClass
  if(!y_class %in% c("character","factor")) {

    if(Debug) print("here 5")

    # Minimize data before moving on
    if(Debug) print("Plot.Calibration.Line # Minimize data before moving on")
    Ncols <- ncol(dt)
    if(Ncols > 2L && length(GroupVar) == 0L) {
      dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar)])
    } else if(Ncols > 3L && length(GroupVar) > 0L) {
      dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, GroupVar[[1L]])])
    } else {
      dt1 <- data.table::copy(dt)
    }

    if(Debug) print("here 6")

    # If actual is in factor form, convert to numeric
    if(Debug) print("Plot.Calibration.Line # If actual is in factor form, convert to numeric")
    if(!is.numeric(dt1[[YVar]])) {
      data.table::set(dt1, j = YVar, value = as.numeric(as.character(dt1[[YVar]])))
    }

    if(Debug) print("here 7")

    # Add a column that ranks predicted values
    if(length(GroupVar) > 0L) {

      if(Debug) print("here 8a")

      if(Debug) print("Plot.Calibration.Line # if(length(GroupVar) > 0L)")

      if(length(FacetLevels) > 0L) {
        dt1 <- dt1[get(GroupVar) %in% c(eval(FacetLevels)), .SD, .SDcols = c(YVar,XVar,GroupVar)]
      }

      dt1[, Percentile := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins, by = c(GroupVar[1L])]
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c("Percentile",GroupVar[1L])]
      dt1[, `Target - Predicted` := get(YVar) - get(XVar)]
      data.table::setorderv(x = dt1, cols = c("Percentile",GroupVar[1L]), c(1L,1L))
    } else {

      if(Debug) print("here 8b")

      if(Debug) print("Plot.Calibration.Line # if(length(GroupVar) == 0L)")
      dt1[, rank := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins]
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = "rank"]
      dt1 <- data.table::melt.data.table(data = dt1, id.vars = "rank", measure.vars = c(YVar,XVar))
      data.table::setnames(dt1, names(dt1), c("Percentile", "Variable", YVar))
      data.table::setorderv(x = dt1, cols = c("Percentile","Variable"), c(1L,1L))
    }

    # Build Plot
    if(Debug) print("Plot.Calibration.Line # AutoPlots::Plot.Line()")
    yvar <- if(length(GroupVar) > 0L) "Target - Predicted" else YVar
    gv <- if(length(GroupVar) == 0L) "Variable" else GroupVar
    tl <- if(length(GroupVar) == 0L) FALSE else TimeLine
    # dt1 <- dt1[!is.na(get(yvar))]

    if(Debug) print(dt1)
    if(Debug) print("here 9")

    p1 <- AutoPlots::Plot.Line(
      dt = dt1,
      PreAgg = TRUE,
      YVar = yvar,
      XVar = "Percentile",
      GroupVar = gv,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Title.YAxis = yvar,
      Title.XAxis = "Predicted",
      ShowLabels = ShowLabels,
      Height = Height,
      Width = Width,
      Title = 'Calibration Line Plot',
      EchartsTheme = EchartsTheme,
      TimeLine = tl,
      X_Scroll = X_Scroll,
      Y_Scroll = Y_Scroll,
      TextColor = TextColor,
      Debug = Debug)

    # dt = dt1
    # PreAgg = TRUE
    # YVar = yvar
    # XVar = "Percentile"
    # GroupVar = gv
    # YVarTrans = YVarTrans
    # XVarTrans = XVarTrans
    # FacetRows = FacetRows
    # FacetCols = FacetCols
    # FacetLevels = FacetLevels
    # Title.YAxis = yvar
    # Title.XAxis = paste0("Predicted every 5th Percentile")
    # ShowLabels = ShowLabels
    # Height = Height
    # Width = Width
    # Title = 'Calibration Line Plot'
    # EchartsTheme = EchartsTheme
    # TimeLine = tl
    # X_Scroll = X_Scroll
    # Y_Scroll = Y_Scroll
    # TextColor = TextColor
    # Debug = Debug

    return(p1)

  } else { # multiclass model

    if(Debug) print("here 5")

    # Minimize data before moving on
    if(Debug) print("Plot.PartialDependence.Line # Minimize data before moving on")
    GroupVar <- tryCatch({GroupVar[1L]}, error = function(x) NULL)

    if(Debug) print("here 6")

    # Shrink data
    if(Debug) print(dt)
    if(Debug) print(YVar)
    yvar_levels <- as.character(dt[, unique(get(YVar))])
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(GroupVar, XVar, YVar, yvar_levels)])

    if(Debug) print("here 7")

    # Dummify Target
    nam <- data.table::copy(names(dt1))
    dt1 <- Rodeo::DummifyDT(data = dt1, cols = YVar, TopN = length(yvar_levels), KeepFactorCols = FALSE, OneHot = FALSE, SaveFactorLevels = FALSE, SavePath = getwd(), ImportFactorLevels = FALSE, FactorLevelsList = NULL, ClustScore = FALSE, ReturnFactorLevels = FALSE)
    nam <- setdiff(names(dt1), nam)

    if(Debug) print("here 8")

    # Melt Predict Cols
    dt2 <- data.table::melt.data.table(
      data = if(length(GroupVar) == 0L) dt1[, .SD, .SDcols = c(names(dt1)[!names(dt1) %in% XVar])] else dt1,
      id.vars = c(GroupVar),
      measure.vars = names(dt1)[!names(dt1) %in% c(GroupVar, YVar, XVar, nam)],
      variable.name = "Level",
      value.name = XVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    if(Debug) print("here 9")

    # Melt Target Cols
    dt3 <- data.table::melt.data.table(
      data = dt1,
      id.vars = c(GroupVar,XVar),
      measure.vars = nam,
      variable.name = "Level",
      value.name = YVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    if(Debug) print("here 10")

    # Join data
    dt2[, eval(YVar) := dt3[[YVar]]]

    if(Debug) print("here 11")

    # Add New Target
    yvar <- "Target - Predicted"
    dt2[, eval(yvar) := get(YVar) - get(XVar)]

    if(length(GroupVar) > 0L) {
      dt2[, GroupVariables := do.call(paste, c(.SD, sep = ' :: ')), .SDcols = c(GroupVar, "Level")]
      GroupVar <- "GroupVariables"
      if(FacetRows > 1L || FacetCols > 1L) {
        FacetLevels <- as.character(dt2[, unique(GroupVariables)])
        FacetLevels <- FacetLevels[seq_len(min(length(FacetLevels),FacetRows*FacetCols))]
        dt2 <- dt2[GroupVariables %chin% c(eval(FacetLevels))]
      }
    } else if(length(GroupVar) == 0L && (FacetRows > 1L || FacetCols > 1L)) {
      FacetLevels <- yvar_levels[seq_len(min(length(yvar_levels), FacetRows * FacetCols))]
      dt2 <- dt2[Level %chin% c(eval(FacetLevels))]
    }

    if(Debug) print("here 12")

    # Subset Cols
    if(length(GroupVar) > 0L) {
      dt2 <- dt2[, .SD, .SDcols = c("GroupVariables", yvar, XVar)]
      dt2[, eval(XVar) := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins, by = c(GroupVar[1L])]
      dt2 <- dt2[, lapply(.SD, noquote(aggFunc)), by = c(XVar,GroupVar)]
    } else {
      dt2 <- dt2[, .SD, .SDcols = c(yvar, XVar, "Level")]
      dt2[, eval(XVar) := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins]
      dt2 <- dt2[, lapply(.SD, noquote(aggFunc)), by = c(XVar,"Level")]
    }

    if(Debug) print("here 13")

    # Build
    if(Debug) print("Plot.PartialDependence.Line --> AutoPlots::Plot.Line()")
    dt2 <- dt2[!is.na(get(yvar))]

    if(Debug) print("here 14")

    p1 <- AutoPlots::Plot.Line(
      dt = dt2,
      PreAgg = TRUE,
      AggMethod = "mean",
      EchartsTheme = EchartsTheme,
      TimeLine = FALSE,
      XVar = XVar,
      YVar = yvar,
      GroupVar = if(length(GroupVar) > 0L) "GroupVariables" else "Level",
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Area = FALSE,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      Height = Height,
      Width = Width,
      Title = "Calibration Line Plot",
      Title.YAxis = yvar,
      Title.XAxis = "Predicted",
      TextColor = TextColor,
      X_Scroll = X_Scroll,
      Y_Scroll = Y_Scroll,
      Debug = Debug)
    return(p1)
  }
}

#' @title Plot.Calibration.Box
#'
#' @description This function automatically builds calibration plots and calibration boxplots for model evaluation using regression, quantile regression, and binary and multinomial classification
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param SampleSize numeric
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param AggMethod character
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor "Not Implemented"
#' @param Debug Debugging purposes
#' @export
Plot.Calibration.Box <- function(dt = NULL,
                                 SampleSize = 100000L,
                                 AggMethod = 'mean',
                                 XVar = NULL,
                                 YVar = NULL,
                                 GroupVar = NULL,
                                 YVarTrans = "Identity",
                                 XVarTrans = "Identity",
                                 FacetRows = 1,
                                 FacetCols = 1,
                                 FacetLevels = NULL,
                                 NumberBins = 21,
                                 Height = NULL,
                                 Width = NULL,
                                 Title = 'Calibration Box',
                                 ShowLabels = FALSE,
                                 Title.YAxis = NULL,
                                 Title.XAxis = NULL,
                                 EchartsTheme = "macarons",
                                 TimeLine = FALSE,
                                 X_Scroll = TRUE,
                                 Y_Scroll = TRUE,
                                 TextColor =        "white",
                                 Debug = FALSE) {

  if(Debug) print("Plot.Calibration.Box 1")

  # Minimize data before moving on
  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })
  if(Debug) print("Plot.Calibration.Box 2")
  if(dt[, .N] > SampleSize) dt <- dt[order(runif(.N))][seq_len(SampleSize)]
  if(Debug) print("Plot.Calibration.Box 3")
  Ncols <- ncol(dt)
  if(Ncols > 2L && length(GroupVar) == 0L) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar)])
  } else if(Ncols > 3L && length(GroupVar) > 0L) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, GroupVar[[1L]])])
  } else {
    dt1 <- data.table::copy(dt)
  }
  if(Debug) print("Plot.Calibration.Box 4")

  # If actual is in factor form, convert to numeric
  if(!is.numeric(dt1[[YVar]])) {
    data.table::set(dt1, j = YVar, value = as.numeric(as.character(dt1[[YVar]])))
  }
  if(Debug) print("Plot.Calibration.Box 5")

  # Add a column that ranks predicted values
  if(Debug) print(paste0("NumberBins = ", NumberBins))
  if(length(GroupVar) > 0L) {
    dt1[, rank := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins, by = c(GroupVar[1L])]
  } else {
    dt1[, rank := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins]
  }
  if(Debug) print("Plot.Calibration.Box 6")
  dt1[, `Target - Predicted` := get(YVar) - get(XVar)]
  data.table::setnames(dt1, "rank", "Percentile")
  if(length(GroupVar) > 0L) {
    data.table::setorderv(x = dt1, cols = c("Percentile", GroupVar[1L]), c(1L,1L))
  } else {
    data.table::setorderv(x = dt1, cols = "Percentile", 1L)
  }
  if(Debug) print("Plot.Calibration.Box 7")

  dt1 <- dt1[, .SD, .SDcols = c("Target - Predicted","Percentile")]
  if(!is.character(dt1[["Percentile"]])) dt1[, Percentile := as.character(Percentile)]
  if(Debug) print("Plot.Calibration.Box 8")

  # Plot
  if(Debug) print(paste0("TimeLine for AutoPlots:::Plot.Box=", TimeLine))
  dt1 <- dt1[!is.na(`Target - Predicted`)]
  if(Debug) print("Plot.Calibration.Box 9")
  p1 <- AutoPlots:::Plot.Box(
    dt = dt1,
    SampleSize = SampleSize,
    XVar = "Percentile",
    YVar = "Target - Predicted",
    GroupVar = GroupVar,
    YVarTrans = YVarTrans,
    XVarTrans = XVarTrans,
    FacetRows = FacetRows,
    FacetCols = FacetCols,
    ShowLabels = ShowLabels,
    Title.YAxis = "Target - Predicted",
    Title.XAxis = paste0("Predicted Every 5th Percentile"),
    FacetLevels = NULL,
    Height = Height,
    Width = Width,
    Title = 'Calibration Box Plot',
    EchartsTheme = EchartsTheme,
    TimeLine = TimeLine,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    TextColor = TextColor,
    Debug = Debug)
  return(p1)
}

#' @title Plot.PartialDependence.Line
#'
#' @description This function automatically builds partial dependence calibration plots
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param ZVar character
#' @param NumberBins numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param EchartsLabels character
#' @param TimeLine logical
#' @param X_Scroll = TRUE,
#' @param Y_Scroll = TRUE,
#' @param TextColor hex character
#' @param AggMethod character
#' @param GroupVar Character variable
#' @param Debug Debugging purposes
#' @export
Plot.PartialDependence.Line <- function(dt = NULL,
                                        XVar = NULL,
                                        YVar = NULL,
                                        ZVar = NULL,
                                        YVarTrans = "Identity",
                                        XVarTrans = "Identity",
                                        ZVarTrans = "Identity",
                                        FacetRows = 1,
                                        FacetCols = 1,
                                        FacetLevels = NULL,
                                        GroupVar = NULL,
                                        NumberBins = 20,
                                        AggMethod = "mean",
                                        Height = NULL,
                                        Width = NULL,
                                        Title = "Partial Dependence Line",
                                        ShowLabels = FALSE,
                                        Title.YAxis = NULL,
                                        Title.XAxis = NULL,
                                        EchartsTheme = "macarons",
                                        EchartsLabels = FALSE,
                                        TimeLine = TRUE,
                                        X_Scroll = TRUE,
                                        Y_Scroll = TRUE,
                                        TextColor =        "white",
                                        Debug = FALSE) {

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # YVar check
  yvar_class <- class(dt[[YVar]])[1L]
  xvar_class <- class(dt[[XVar]][1L])

  # Define Aggregation function
  if(Debug) print("Plot.PartialDependence.Line # Define Aggregation function")
  aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

  # Regression and Classification else MultiClass
  if(yvar_class %in% c("numeric","integer")) {

    # Minimize data before moving on
    if(Debug) print("Plot.PartialDependence.Line # Minimize data before moving on")
    Ncols <- ncol(dt)
    if(Ncols > 2L && length(GroupVar) == 0L) {
      dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, ZVar)])
    } else if(Ncols > 3L && length(GroupVar) > 0L) {
      dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, ZVar, GroupVar[1L])])
    } else {
      dt1 <- data.table::copy(dt)
    }

    # If actual is in factor form, convert to numeric
    if(Debug) print("Plot.PartialDependence.Line # If actual is in factor form, convert to numeric")
    if(!is.numeric(dt1[[YVar]])) {
      data.table::set(dt1, j = YVar, value = as.numeric(as.character(dt1[[YVar]])))
    }

    # Data Mgt
    if(length(GroupVar) > 0L) {
      if(Debug) print("Plot.PartialDependence.Line # if(length(GroupVar) > 0L)")
      if(!xvar_class %in%  c("factor","character","Date","IDate","POSIXct","IDateTime")) {
        dt1[, eval(XVar) := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins, by = c(GroupVar[1L])]
      }
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(XVar,GroupVar[1L])]
      dt1[, `Target - Predicted` := get(YVar) - get(ZVar)]
      data.table::setorderv(x = dt1, cols = c(XVar,GroupVar[1L]), c(1L,1L))
      yvar <- "Target - Predicted"
      gv <- GroupVar
      tl <- TimeLine
    } else {
      if(Debug) print("Plot.PartialDependence.Line # if(length(GroupVar) == 0L)")
      if(!xvar_class %in%  c("factor","character","Date","IDate","POSIXct","IDateTime")) {
        dt1[, eval(XVar) := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins]
      }
      dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = eval(XVar)]
      dt1 <- data.table::melt.data.table(data = dt1, id.vars = eval(XVar), measure.vars = c(YVar,ZVar))
      data.table::setnames(dt1, names(dt1), c(XVar, "Variable", YVar))
      data.table::setorderv(x = dt1, cols = c(XVar,"Variable"), c(1L,1L))
      yvar <- YVar
      gv <- "Variable"
      tl <- FALSE
    }

    # Build
    if(Debug) print("Plot.PartialDependence.Line --> AutoPlots::Plot.Line()")
    dt1 <- dt1[!is.na(get(yvar))]
    p1 <- AutoPlots::Plot.Line(
      Area = FALSE,
      dt = dt1,
      PreAgg = TRUE,
      AggMethod = "mean",
      EchartsTheme = EchartsTheme,
      TimeLine = tl,
      XVar = XVar,
      YVar = yvar,
      GroupVar = gv,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      ShowLabels = ShowLabels,
      Title.YAxis = if(length(GroupVar) > 0L) "Target - Predicted" else "Target & Predicted",
      Title.XAxis = XVar,
      Height = Height,
      Width = Width,
      Title = "Partial Dependence",
      TextColor = TextColor,
      X_Scroll = X_Scroll,
      Y_Scroll = Y_Scroll,
      Debug = Debug)
    return(p1)

  } else { # multiclass model

    # Minimize data before moving on
    if(Debug) print("Plot.PartialDependence.Line # Minimize data before moving on")
    GroupVar <- tryCatch({GroupVar[1L]}, error = function(x) NULL)

    # Shrink data
    yvar_levels <- as.character(dt[, unique(get(YVar))])
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(GroupVar, XVar, YVar, yvar_levels)])

    # Dummify Target
    nam <- data.table::copy(names(dt1))
    dt1 <- Rodeo::DummifyDT(data = dt1, cols = YVar, TopN = length(yvar_levels), KeepFactorCols = FALSE, OneHot = FALSE, SaveFactorLevels = FALSE, SavePath = getwd(), ImportFactorLevels = FALSE, FactorLevelsList = NULL, ClustScore = FALSE, ReturnFactorLevels = FALSE)
    nam <- setdiff(names(dt1), nam)

    # Melt Predict Cols
    dt2 <- data.table::melt.data.table(
      data = dt1,
      id.vars = c(GroupVar, XVar),
      measure.vars = names(dt1)[!names(dt1) %in% c(GroupVar, XVar, YVar, nam)],
      variable.name = "Level",
      value.name = ZVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    # Melt Target Cols
    dt3 <- data.table::melt.data.table(
      data = dt1,
      id.vars = c(GroupVar, XVar),
      measure.vars = nam,
      variable.name = "Level",
      value.name = YVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    # Join data
    dt2[, eval(YVar) := dt3[[YVar]]]

    # Update Args
    if(length(GroupVar) > 0L) {
      dt2[, GroupVariables := do.call(paste, c(.SD, sep = ' :: ')), .SDcols = c(GroupVar, "Level")]
      GroupVar <- "GroupVariables"
      if(FacetRows > 1L && FacetCols > 1L) {
        FacetLevels <- as.character(dt2[, unique(GroupVariables)])
        FacetLevels <- FacetLevels[seq_len(min(length(FacetLevels),FacetRows*FacetCols))]
        dt2 <- dt2[GroupVariables %chin% c(eval(FacetLevels))]
      }
    } else if(length(GroupVar) == 0L && (FacetRows > 1L || FacetCols > 1L)) {
      FacetLevels <- yvar_levels[seq_len(min(length(yvar_levels), FacetRows * FacetCols))]
      dt2 <- dt2[Level %chin% c(eval(FacetLevels))]
    }

    # Add New Target
    yvar <- "Target - Predicted"
    dt2[, eval(yvar) := get(YVar) - get(ZVar)]

    # Subset Cols
    if(length(GroupVar) > 0L) {
      dt2 <- dt2[, .SD, .SDcols = c("GroupVariables", yvar, XVar)]
      if(!xvar_class %in%  c("factor","character","Date","IDate","POSIXct","IDateTime")) {
        dt2[, eval(XVar) := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins, by = c(GroupVar[1L])]
      }
      dt2 <- dt2[, lapply(.SD, noquote(aggFunc)), by = c(XVar,GroupVar)]
    } else {
      dt2 <- dt2[, .SD, .SDcols = c(yvar, XVar, "Level")]
      if(!xvar_class %in%  c("factor","character","Date","IDate","POSIXct","IDateTime")) {
        dt2[, eval(XVar) := round(data.table::frank(get(XVar)) * NumberBins / .N) / NumberBins]
      }
      dt2 <- dt2[, lapply(.SD, noquote(aggFunc)), by = c(XVar,"Level")]
    }

    # Build
    if(Debug) print("Plot.PartialDependence.Line --> AutoPlots::Plot.Line()")
    dt2 <- dt2[!is.na(get(yvar))]
    p1 <- AutoPlots::Plot.Line(
      dt = dt2,
      PreAgg = TRUE,
      AggMethod = "mean",
      EchartsTheme = EchartsTheme,
      TimeLine = FALSE,
      XVar = XVar,
      YVar = yvar,
      GroupVar = if(length(GroupVar) > 0L) "GroupVariables" else "Level",
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      ShowLabels = ShowLabels,
      Title.YAxis = if(length(GroupVar) > 0L) "Target - Predicted" else "Target & Predicted",
      Title.XAxis = XVar,
      Area = FALSE,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      Height = Height,
      Width = Width,
      Title = "Partial Dependence",
      TextColor = TextColor,
      X_Scroll = X_Scroll,
      Y_Scroll = Y_Scroll,
      Debug = Debug)
    return(p1)
  }
}

#' @title Plot.PartialDependence.Box
#'
#' @description This function automatically builds partial dependence calibration plots
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param SampleSize numeric
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param ZVar character
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param PreAgg logical
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param EchartsLabels character
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor hex character
#' @param AggMethod character
#' @param GroupVar Character variable
#' @param Debug Debugging purposes
#' @export
Plot.PartialDependence.Box <- function(dt = NULL,
                                       PreAgg = FALSE,
                                       SampleSize = 100000L,
                                       XVar = NULL,
                                       YVar = NULL,
                                       ZVar = NULL,
                                       GroupVar = NULL,
                                       YVarTrans = "Identity",
                                       XVarTrans = "Identity",
                                       ZVarTrans = "Identity",
                                       FacetRows = 1,
                                       FacetCols = 1,
                                       FacetLevels = NULL,
                                       NumberBins = 20,
                                       AggMethod = "mean",
                                       Height = NULL,
                                       Width = NULL,
                                       Title = "Partial Dependence Box",
                                       ShowLabels = FALSE,
                                       Title.YAxis = NULL,
                                       Title.XAxis = NULL,
                                       EchartsTheme = "macarons",
                                       EchartsLabels = FALSE,
                                       TimeLine = TRUE,
                                       X_Scroll = TRUE,
                                       Y_Scroll = FALSE,
                                       TextColor =        "white",
                                       Debug = FALSE) {

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  GroupVar <- NULL

  # Minimize data before moving on
  if(Debug) print("Plot.PartialDependence.Box # Minimize data before moving on")
  Ncols <- ncol(dt)
  if(Ncols > 3L) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, ZVar)])
  } else {
    dt1 <- data.table::copy(dt)
  }

  # If actual is in factor form, convert to numeric
  if(Debug) print("Plot.PartialDependence.Box # If actual is in factor form, convert to numeric")
  if(!is.numeric(dt1[[YVar]])) {
    data.table::set(dt1, j = YVar, value = as.numeric(as.character(dt1[[YVar]])))
  }

  # Add a column that ranks predicted values
  dt1[, eval(XVar) := as.character(round(data.table::frank(get(XVar)) * (NumberBins) / .N) / NumberBins)]
  dt1[, `Target - Predicted` := get(YVar) - get(ZVar)]
  data.table::setorderv(x = dt1, cols = XVar, 1L)

  # Build Plot
  tl <- if(length(GroupVar) == 0L) FALSE else TimeLine

  # Build
  if(Debug) print("Plot.PartialDependence.Box --> AutoPlots::Plot.Box()")
  dt1 <- dt1[!is.na(`Target - Predicted`)]
  p1 <- AutoPlots::Plot.Box(
    dt = dt1,
    SampleSize = SampleSize,
    XVar = XVar,
    YVar = "Target - Predicted",
    GroupVar = NULL,
    YVarTrans = YVarTrans,
    XVarTrans = XVarTrans,
    FacetRows = 1,
    FacetCols = 1,
    FacetLevels = NULL,
    ShowLabels = ShowLabels,
    Title.YAxis = "Target & Predicted",
    Title.XAxis = paste0(XVar, " Every 5th Percentile"),
    Height = Height,
    Width = Width,
    Title = "Partial Dependence",
    EchartsTheme = EchartsTheme,
    TimeLine = tl,
    TextColor = TextColor,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    Debug = Debug)

  # dt = dt1
  # SampleSize = SampleSize
  # XVar = XVar
  # YVar = "Target - Predicted"
  # GroupVar = NULL
  # YVarTrans = YVarTrans
  # XVarTrans = XVarTrans
  # FacetRows = 1
  # FacetCols = 1
  # FacetLevels = NULL
  # ShowLabels = ShowLabels
  # Title.YAxis = "Target & Predicted"
  # Title.XAxis = paste0(XVar, " Every 5th Percentile")
  # Height = Height
  # Width = Width
  # Title = "Partial Dependence"
  # EchartsTheme = EchartsTheme
  # TimeLine = tl
  # TextColor = TextColor
  # X_Scroll = X_Scroll
  # Y_Scroll = Y_Scroll
  # Debug = Debug

  return(p1)
}

#' @title Plot.PartialDependence.HeatMap
#'
#' @description This function automatically builds partial dependence calibration plots
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param ZVar character
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param EchartsLabels character
#' @param TimeLine logical
#' @param X_Scroll = TRUE,
#' @param Y_Scroll = TRUE,
#' @param TextColor hex character
#' @param AggMethod character
#' @param GroupVar Character variable
#' @param Debug Debugging purposes
#' @export
Plot.PartialDependence.HeatMap <- function(dt = NULL,
                                           XVar = NULL,
                                           YVar = NULL,
                                           ZVar = NULL,
                                           GroupVar = NULL,
                                           YVarTrans = "Identity",
                                           XVarTrans = "Identity",
                                           ZVarTrans = "Identity",
                                           FacetRows = 1,
                                           FacetCols = 1,
                                           FacetLevels = NULL,
                                           NumberBins = 21,
                                           AggMethod = "mean",
                                           Height = NULL,
                                           Width = NULL,
                                           Title = "Partial Dependence Heatmap",
                                           ShowLabels = FALSE,
                                           Title.YAxis = NULL,
                                           Title.XAxis = NULL,
                                           EchartsTheme = "macarons",
                                           EchartsLabels = FALSE,
                                           TimeLine = TRUE,
                                           X_Scroll = TRUE,
                                           Y_Scroll = TRUE,
                                           TextColor =        "white",
                                           Debug = FALSE) {

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # YVar check
  yvar_class <- class(dt[[YVar]])[1L]

  # Define Aggregation function
  if(Debug) print("Plot.PartialDependence.Line # Define Aggregation function")
  aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

  # Regression and Classification else MultiClass
  if(yvar_class %in% c("numeric","integer")) {

    GroupVar <- NULL

    # Minimize data before moving on
    if(Debug) print("Plot.PartialDependence.HeatMap # Minimize data before moving on")
    Ncols <- ncol(dt)
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, ZVar)])

    if(Debug) print("Plot.PartialDependence.HeatMap # Define Aggregation function")
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)
    if(Debug) print("Plot.PartialDependence.HeatMap # if(length(GroupVar) == 0L)")

    for(i in seq_along(XVar)) {
      if(class(dt[[XVar[i]]][1L]) %in% c("numeric","integer")) {
        if(Debug) print(paste0('here ', XVar[i]))
        dt1[, eval(XVar[i]) := as.character(round(data.table::frank(get(XVar[i])) * NumberBins / .N / NumberBins, 3), 1L)]
      } else {
        if(Debug) print(paste0('there ', XVar[i]))
        dt1[, eval(XVar[i]) := as.character(get(XVar[i]))]
      }
    }

    if(Debug) print("here 2")
    dt1 <- dt1[, lapply(.SD, noquote(aggFunc)), by = c(eval(XVar))]
    if(Debug) print("here 3")
    dt1[, `Target - Predicted` := get(YVar) - get(ZVar)]
    if(Debug) print("here 4")
    ZVar <- "Target - Predicted"
    if(length(XVar) > 1L) {
      if(Debug) print("here 5.1")
      YVar <- XVar[2L]
      XVar <- XVar[1L]
      data.table::setorderv(x = dt1, cols = c(XVar,YVar),c(1L,1L))
      for(i in c(XVar,YVar)) dt1[, eval(i) := get(i)]

      # Build
      if(Debug) print("Plot.PartialDependence.HeatMap --> AutoPlots::Plot.HeatMap()")
      dt1 <- dt1[!is.na(get(YVar))]
      p1 <- AutoPlots::Plot.HeatMap(
        dt = dt1,
        PreAgg = TRUE,
        AggMethod = "mean",
        EchartsTheme = EchartsTheme,
        XVar = XVar,
        YVar = YVar,
        ZVar = ZVar,
        YVarTrans = YVarTrans,
        XVarTrans = XVarTrans,
        ZVarTrans = ZVarTrans,
        ShowLabels = ShowLabels,
        Title.YAxis = YVar,
        Title.XAxis = XVar,
        Height = Height,
        Width = Width,
        Title = "Partial Dependence Heatmap: Target - Predicted",
        TextColor = TextColor,
        X_Scroll = X_Scroll,
        Y_Scroll = Y_Scroll,
        NumberBins = NumberBins,
        Debug = Debug)
      return(p1)

    } else {
      if(Debug) print("here 5.2")
      data.table::setorderv(x = dt1, cols = XVar,1L)
      if(Debug) print("here 5.3")
      dt1 <- dt1[!is.na(get(ZVar))]
      if(Debug) print("here 5.4")
      # data.table::fwrite(dt1, file = "C:/Users/Bizon/Documents/GitHub/rappwd/dt1.csv")
      # dt1 <- data.table::fread(file = "C:/Users/Bizon/Documents/GitHub/rappwd/dt1.csv")
      # EchartsTheme <- "macarons"
      # ShowLabels <- FALSE
      # Height = "200px"
      # Width = "400px"
      # XVar = "GroupVariable"
      # ZVar = "Target - Predicted"
      print(XVar)
      print(ZVar)
      dt1 <- dt1[, .SD, .SDcols = c(XVar, ZVar)]
      print(dt1)
      p1 <- AutoPlots::Plot.Bar(
        dt = dt1,
        PreAgg = TRUE,
        XVar = XVar,
        YVar = ZVar,
        GroupVar = NULL,
        YVarTrans = "Identity",
        XVarTrans = "Identity",
        FacetRows = 1,
        FacetCols = 1,
        FacetLevels = NULL,
        AggMethod = "mean",
        Height = Height,
        Width = Width,
        Title = "Partial Dependence Bar Plot: Target - Predicted",
        ShowLabels = ShowLabels,
        Title.YAxis = "Target - Predicted",
        Title.XAxis = XVar,
        EchartsTheme = EchartsTheme,
        TimeLine = TRUE,
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
      return(p1)
    }

  } else {

    # Minimize data before moving on
    if(Debug) print("Plot.PartialDependence.Line # Minimize data before moving on")

    # Shrink data
    yvar_levels <- as.character(dt[, unique(get(YVar))])
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(XVar, YVar, yvar_levels)])

    # Dummify Target
    nam <- data.table::copy(names(dt1))
    dt1 <- Rodeo::DummifyDT(data = dt1, cols = YVar, TopN = length(yvar_levels), KeepFactorCols = FALSE, OneHot = FALSE, SaveFactorLevels = FALSE, SavePath = getwd(), ImportFactorLevels = FALSE, FactorLevelsList = NULL, ClustScore = FALSE, ReturnFactorLevels = FALSE)
    nam <- setdiff(names(dt1), nam)

    # Melt Predict Cols
    dt2 <- data.table::melt.data.table(
      data = dt1,
      id.vars = XVar,
      measure.vars = names(dt1)[!names(dt1) %in% c(XVar, YVar, nam)],
      variable.name = "Level",
      value.name = ZVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    # Melt Target Cols
    dt3 <- data.table::melt.data.table(
      data = dt1,
      id.vars = XVar,
      measure.vars = nam,
      variable.name = "Level",
      value.name = YVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    # Join data
    dt2[, eval(YVar) := dt3[[YVar]]]

    # Add New Target
    yvar <- "Target - Predicted"
    dt2[, eval(yvar) := get(YVar) - get(ZVar)]

    # Subset Cols
    dt2 <- dt2[, .SD, .SDcols = c(yvar, XVar, "Level")]
    for(i in seq_along(XVar)) {
      if(class(dt[[XVar]][i]) %in% c("numeric","integer")) {
        dt1[, eval(XVar[i]) := as.character(round(data.table::frank(get(XVar[i])) * NumberBins / .N / NumberBins, 3), 1L)]
      } else {
        dt1[, eval(XVar[i]) := as.character(get(XVar[i]))]
      }
    }

    dt2 <- dt2[, lapply(.SD, noquote(aggFunc)), by = c(XVar,"Level")]

    # Build
    if(Debug) print("Plot.PartialDependence.HeatMap --> AutoPlots::Plot.HeatMap()")
    dt2 <- dt2[!is.na(get(yvar))]
    p1 <- AutoPlots::Plot.HeatMap(
      dt = dt2,
      PreAgg = TRUE,
      AggMethod = "mean",
      EchartsTheme = EchartsTheme,
      XVar = XVar,
      YVar = "Level",
      ZVar = yvar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      ZVarTrans = ZVarTrans,
      Title.YAxis = YVar,
      Title.XAxis = XVar,
      Height = Height,
      Width = Width,
      Title = "Partial Dependence Heatmap: Target - Predicted",
      TextColor = TextColor,
      X_Scroll = X_Scroll,
      Y_Scroll = Y_Scroll,
      NumberBins = NumberBins,
      Debug = Debug)
    return(p1)
  }
}

#' @title Plot.VariableImportance
#'
#' @description Generate variable importance plots
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Title title
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor 'darkblue'
#' @param Debug Debugging purposes
#' @export
Plot.VariableImportance <- function(dt = NULL,
                                    XVar = NULL,
                                    YVar = NULL,
                                    GroupVar = NULL,
                                    YVarTrans = "Identity",
                                    XVarTrans = "Identity",
                                    FacetRows = 1,
                                    FacetCols = 1,
                                    FacetLevels = NULL,
                                    AggMethod = 'mean',
                                    Height = NULL,
                                    Width = NULL,
                                    Title = 'Variable Importance Plot',
                                    ShowLabels = FALSE,
                                    Title.YAxis = NULL,
                                    Title.XAxis = NULL,
                                    EchartsTheme = "macarons",
                                    TimeLine = TRUE,
                                    X_Scroll = TRUE,
                                    Y_Scroll = TRUE,
                                    TextColor =        "white",
                                    title.fontSize = 22,
                                    title.fontWeight = "bold", # normal
                                    title.textShadowColor = '#63aeff',
                                    title.textShadowBlur = 3,
                                    title.textShadowOffsetY = 1,
                                    title.textShadowOffsetX = -1,
                                    xaxis.fontSize = 14,
                                    yaxis.fontSize = 14,
                                    Debug = FALSE) {

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # Plot
  dt <- dt[order(Importance)]
  Var <- names(which(unlist(lapply(dt, is.character))))
  Var2 <- names(which(unlist(lapply(dt, is.numeric))))[1L]
  if(length(Var) == 0L) {
    Var <- names(which(unlist(lapply(dt, is.factor))))
    dt[, eval(Var) := as.character(get(Var))]
  }
  dt <- dt[!is.na(get(YVar))]
  p1 <- echarts4r::e_charts_(
    dt,
    x = Var,
    dispose = TRUE,
    darkMode = TRUE,
    width = Width,
    height = Height)
  p1 <- echarts4r::e_bar_(e = p1, Var2)
  p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
  p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
  p1 <- echarts4r::e_tooltip(e = p1, trigger = "axis", backgroundColor = "aliceblue")
  p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
  p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
  p1 <- echarts4r::e_brush(e = p1)
  p1 <- echarts4r::e_title(
    p1, Title,
    textStyle = list(
      color = TextColor,
      fontWeight = title.fontWeight,
      overflow = "truncate", # "none", "truncate", "break",
      ellipsis = '...',
      fontSize = title.fontSize,
      textShadowColor = title.textShadowColor,
      textShadowBlur = title.textShadowBlur,
      textShadowOffsetY = title.textShadowOffsetY,
      textShadowOffsetX = title.textShadowOffsetX))
  p1 <- echarts4r::e_flip_coords(e = p1)
  return(p1)
}

#' @title Plot.ROC
#'
#' @description ROC Plot
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param AggMethod character
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param SampleSize numeric
#' @param TextColor character hex
#' @param Debug Debugging purposes
#' @export
Plot.ROC <- function(dt = NULL,
                     SampleSize = 100000,
                     XVar = NULL,
                     YVar = NULL,
                     GroupVar = NULL,
                     YVarTrans = "Identity",
                     XVarTrans = "Identity",
                     FacetRows = 1,
                     FacetCols = 1,
                     FacetLevels = NULL,
                     AggMethod = 'mean',
                     Height = NULL,
                     Width = NULL,
                     Title = 'ROC Plot',
                     ShowLabels = FALSE,
                     Title.YAxis = "True Positive Rate",
                     Title.XAxis = "1 - False Positive Rate",
                     EchartsTheme = "macarons",
                     TimeLine = FALSE,
                     X_Scroll = TRUE,
                     Y_Scroll = TRUE,
                     TextColor =        "white",
                     Debug = FALSE) {

  # ROC
  fastROC <- function(preds, target) {
    class_sorted <- target[order(preds, decreasing = TRUE)]
    TPR <- cumsum(class_sorted) / sum(target)
    FPR <- cumsum(class_sorted == 0) / sum(target == 0)
    return(
      list(
        tpr = TPR,
        fpr = FPR
      )
    )
  }

  # AUC
  fastAUC <- function(preds, target) {
    x <- preds
    y <- target
    x1 = x[y == 1]; n1 = length(x1);
    x2 = x[y == 0]; n2 = length(x2);
    r = rank(c(x1,x2))
    auc = (sum(r[1:n1]) - n1 * (n1 + 1) / 2) / n1 / n2
    return(auc)
  }

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # YVar check
  yvar_class <- class(dt[[YVar]])[1L]
  if(yvar_class %in% c("factor","character")) {

    # Shrink data
    yvar_levels <- as.character(dt[, unique(get(YVar))])
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(XVar, YVar, yvar_levels, GroupVar)])

    # Dummify Target
    nam <- data.table::copy(names(dt1))
    dt1 <- Rodeo::DummifyDT(data = dt1, cols = YVar, TopN = length(yvar_levels), KeepFactorCols = FALSE, OneHot = FALSE, SaveFactorLevels = FALSE, SavePath = getwd(), ImportFactorLevels = FALSE, FactorLevelsList = NULL, ClustScore = FALSE, ReturnFactorLevels = FALSE)
    nam <- setdiff(names(dt1), nam)

    # Melt Predict Cols
    dt2 <- data.table::melt.data.table(
      data = dt1[, .SD, .SDcols = c(names(dt1)[!names(dt1) %in% c(nam,XVar)])],
      id.vars = GroupVar,
      measure.vars = names(dt1)[!names(dt1) %in% c(nam,XVar,GroupVar)],
      variable.name = "Level",
      value.name = XVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    # Melt Target Cols
    dt3 <- data.table::melt.data.table(
      data = dt1[, .SD, .SDcols = c(names(dt1)[!names(dt1) %in% c(yvar_levels,XVar)])],
      id.vars = GroupVar,
      measure.vars = nam,
      variable.name = "Level",
      value.name = YVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    # Join data
    dt2[, eval(YVar) := dt3[[YVar]]]

    # Update Args
    if(length(GroupVar) > 0L) {
      dt2[, GroupVariables := do.call(paste, c(.SD, sep = ' :: ')), .SDcols = c(GroupVar, "Level")]
      GroupVar <- "GroupVariables"
      if(FacetRows > 1L && FacetCols > 1L) {
        FacetLevels <- as.character(dt2[, unique(GroupVariables)])
        FacetLevels <- FacetLevels[seq_len(min(length(FacetLevels),FacetRows*FacetCols))]
        dt2 <- dt2[GroupVariables %chin% c(eval(FacetLevels))]
      }
    } else if(length(GroupVar) == 0L && (FacetRows > 1L || FacetCols > 1L)) {
      FacetLevels <- yvar_levels[seq_len(min(length(yvar_levels), FacetRows * FacetCols))]
      dt2 <- dt2[Level %chin% c(eval(FacetLevels))]
      GroupVar <- "Level"
    } else {
      GroupVar <- "Level"
    }

  } else {
    dt2 <- data.table::copy(dt)
  }

  # Data Prep1
  if(Debug) print("ROC 1")
  if(length(GroupVar) > 0L) {
    vals <- sort(unique(dt2[[GroupVar]]))
    for(i in seq_along(vals)) { # i = 1
      temp <- dt2[get(GroupVar) %in% eval(vals[i])]
      if(Debug) {
        print(i)
        print("ROC 2")
      }
      ROC <- tryCatch({fastROC(temp[[XVar]], temp[[YVar]])}, error = function(x) NULL)
      if(i == 1L && length(ROC) > 0L) {
        data <- data.table::data.table(
          GroupLevels = vals[i],
          Sensitivity = 1-ROC$fpr,
          Specificity = ROC$tpr)
      } else if(length(ROC) > 0L) {
        data <- data.table::rbindlist(list(
          data,
          data.table::data.table(
            GroupLevels = vals[i],
            Sensitivity = 1-ROC$fpr,
            Specificity = ROC$tpr)
        ))
      }
    }

    if(Debug) print("ROC 3")

    # For Title: auc = AUC
    AUC <- tryCatch({fastAUC(temp[[XVar]], temp[[YVar]])}, error = function(x) NULL)
    if(Debug) print("ROC 4")

  } else {
    ROC <- tryCatch({fastROC(dt2[[XVar]], dt2[[YVar]])}, error = function(x) NULL)
    AUC <- tryCatch({fastAUC(dt2[[XVar]], dt2[[YVar]])}, error = function(x) NULL)
    data <- data.table::data.table(
      GroupLevels = 0L,
      Sensitivity = 1-ROC$fpr,
      Specificity = ROC$tpr)
  }

  if(Debug) print("ROC 5")

  # Data Prep2
  if(Debug) print("Plot.Calibration.Line # AutoPlots::Plot.Line()")
  data[, `1 - Specificity` := 1 - Specificity]
  data.table::set(data, j = "Specificity", value = NULL)
  YVar <- "Sensitivity"
  XVar <- "1 - Specificity"
  tl <- if(length(GroupVar) == 0L) FALSE else TimeLine
  if(length(GroupVar) > 0L && (FacetRows > 1L && FacetCols > 1L)) {
    title <- paste0(Title, ":\nMicro-AUC: ", 100 * round(AUC, 3), "%\n*Excluding cases of all 1's or 0's")
  }
  title <- paste0(Title, ":\nMicro-AUC: ", 100 * round(AUC, 3), "%")
  gv <- if(length(GroupVar) > 0L) "GroupLevels" else NULL
  data.table::setorderv(x = data, cols = c(gv, "Sensitivity"))

  if(Debug) print("ROC 6")

  # Build Plot (Line or Area)
  if(length(GroupVar) > 0L && FacetRows == 1L && FacetCols == 1L) {
    p1 <- AutoPlots::Plot.Line(
      dt = data,
      PreAgg = TRUE,
      Smooth = TRUE,
      Area = FALSE,
      ShowSymbol = FALSE,
      Alpha = 0.50,
      EchartsTheme = EchartsTheme,
      TimeLine = tl,
      YVar = YVar,
      XVar = XVar,
      GroupVar = gv,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      Title = title,
      TextColor = TextColor,
      X_Scroll = FALSE,
      Y_Scroll = FALSE,
      Debug = Debug)
  } else {
    p1 <- AutoPlots::Plot.Area(
      dt = data,
      PreAgg = TRUE,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      Alpha = 0.50,
      EchartsTheme = EchartsTheme,
      TimeLine = tl,
      YVar = YVar,
      XVar = XVar,
      GroupVar = gv,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      Title = title,
      TextColor = TextColor,
      X_Scroll = FALSE,
      Y_Scroll = FALSE,
      Debug = Debug)
  }

  # Y == X dashed line
  if(class(p1)[1L] == "plotly") p1 <- plotly::add_segments(p = p1, x = 0, xend = 1, y = 0, yend = 1, line = list(dash = "dash", color = TextColor),inherit = FALSE, showlegend = FALSE)

  # Return
  return(p1)
}

#' @title Plot.ConfusionMatrix
#'
#' @description Generate variable importance plots
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param dt source data.table
#' @param PreAgg FALSE
#' @param XVar Column name of X-Axis variable. If NULL then ignored
#' @param YVar Column name of Y-Axis variable. If NULL then ignored
#' @param ZVar = "N"
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins = 21,
#' @param NumLevels_X = NumLevels_Y,
#' @param NumLevels_Y = NumLevels_X,
#' @param GroupVar Column name of Group Variable for distinct colored histograms by group levels
#' @param Title title
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param GroupVar = NULL
#' @param AggMethod Choose from 'mean', 'sum', 'sd', and 'median'
#' @param TextColor 'darkblue'
#' @param Debug Debugging purposes
#'
#' @examples
#' \dontrun{
#'
#' # Debugging
#' dt <- data.table::fread(file.choose())
#' XVar <- c("Brand", "Category")
#' YVar <- "ClassTarget"
#' ZVar <- "p1"
#' YVarTrans <- "Identity"
#' XVarTrans <- "Identity"
#' ZVarTrans <- "Identity"
#' FacetRows <- 1
#' FacetCols <- 1
#' FacetLevels <- NULL
#' Height <- NULL
#' Width <- NULL
#' Title <- NULL
#' ShowLabels <- FALSE
#' Title.YAxis <- NULL
#' Title.XAxis <- NULL
#' EchartsTheme <- "macarons"
#' TimeLine <- FALSE
#' TextColor <- "white"
#' AggMethod <- "mean"
#' Debug <- FALSE
#'
#' }
#'
#' @export
Plot.ConfusionMatrix <- function(dt = NULL,
                                 PreAgg = FALSE,
                                 XVar = NULL,
                                 YVar = NULL,
                                 ZVar = "N",
                                 YVarTrans = "Identity",
                                 XVarTrans = "Identity",
                                 ZVarTrans = "Identity",
                                 FacetRows = 1,
                                 FacetCols = 1,
                                 FacetLevels = NULL,
                                 NumberBins = 21,
                                 NumLevels_X = 50,
                                 NumLevels_Y = 50,
                                 Height = NULL,
                                 Width = NULL,
                                 Title = "Confusion Matrix",
                                 ShowLabels = FALSE,
                                 Title.YAxis = NULL,
                                 Title.XAxis = NULL,
                                 EchartsTheme = "macarons",
                                 TimeLine = TRUE,
                                 X_Scroll = TRUE,
                                 Y_Scroll = TRUE,
                                 TextColor =        "white",
                                 AggMethod = "count",
                                 GroupVar = NULL,
                                 xaxis.rotate = 0,
                                 yaxis.rotate = 0,
                                 ContainLabel = TRUE,
                                 Debug = FALSE) {

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # YVar check
  yvar_class <- class(dt[[YVar]])[1L]

  if(yvar_class %in% c("factor","character")) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(XVar, YVar, GroupVar)])
    dt1[, paste0(XVar,"_") := .N, by = XVar]
    dt1[, paste0(YVar,"_") := .N, by = YVar]
    dt4 <- dt1[, list(N = .N, Mean.X = mean(get(paste0(XVar,"_")), na.rm = TRUE)), by = c(YVar,XVar)]
    dt4[, `Mean.X` := N / Mean.X]
    ZVar <- "Mean.X"
  } else if(!PreAgg) {
    if(length(unique(dt[[XVar]])) > 2L) {
      dt[, classPredict := data.table::fifelse(get(XVar) > 0.5, 1, 0)]
    }
    dt4 <- data.table::CJ(unique(dt[[YVar]]), unique(dt[["classPredict"]]))
    data.table::setnames(dt4, c("V1","V2"), c(YVar, XVar))
    dt3 <- dt[, list(Metric = .N), by = c(YVar, "classPredict")]
    data.table::setkeyv(x = dt3, cols = c(YVar, "classPredict"))
    data.table::setkeyv(x = dt4, cols = c(YVar, XVar))
    dt4[dt3, Metric := i.Metric]
    data.table::set(dt4, i = which(is.na(dt4[["Metric"]])), j = "Metric", value = 0)
    if(Debug) print("Confusion Matrix Plot.Heatmap")
    dt4[, `Proportion in Target` := sum(Metric), by = eval(YVar)]
    dt4[, `Proportion in Target` := data.table::fifelse(`Proportion in Target` > 0, Metric / `Proportion in Target`, 0)]
    ZVar = "Proportion in Target"
  } else {
    dt4 <- data.table::copy(dt)
  }

  # Corr Matrix for the automatic ordering
  data.table::setorderv(dt4, c(XVar,YVar), c(1L,1L))
  dt4 <- dt4[!is.na(get(ZVar))]
  p1 <- AutoPlots:::Plot.HeatMap(
    PreAgg = TRUE,
    EchartsTheme = EchartsTheme,
    Title = Title,
    dt = dt4,
    YVar = YVar,
    XVar = XVar,
    ZVar = ZVar,
    Height = Height,
    Width = Width,
    AggMethod = if(!PreAgg) "centroidial" else AggMethod,
    NumberBins = NumberBins,
    NumLevels_X = NumLevels_X,
    NumLevels_Y = NumLevels_Y,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    xaxis.rotate = xaxis.rotate,
    yaxis.rotate = yaxis.rotate,
    ContainLabel = ContainLabel)
  return(p1)
}

#' @title Plot.Lift
#'
#' @description Create a cumulative gains chart
#'
#' @family Model Evaluation
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param ZVar character
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param PreAgg logical
#' @param NumberBins numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor character hex
#' @param Debug Debugging purposes
#'
#' @export
Plot.Lift <- function(dt = NULL,
                      PreAgg = FALSE,
                      XVar = NULL,
                      YVar = NULL,
                      ZVar = "N",
                      GroupVar = NULL,
                      YVarTrans = "Identity",
                      XVarTrans = "Identity",
                      ZVarTrans = "Identity",
                      FacetRows = 1,
                      FacetCols = 1,
                      FacetLevels = NULL,
                      NumberBins = 20,
                      Height = NULL,
                      Width = NULL,
                      Title = "Confusion Matrix",
                      ShowLabels = FALSE,
                      Title.YAxis = "Lift",
                      Title.XAxis = "Population",
                      EchartsTheme = "macarons",
                      TimeLine = TRUE,
                      X_Scroll = TRUE,
                      Y_Scroll = TRUE,
                      TextColor =        "white",
                      Debug = FALSE) {
  if(Debug) print("here 0")
  if(Debug) print(data.table::is.data.table(dt))
  if(!data.table::is.data.table(dt)) {
    tryCatch({data.table::setDT(dt)}, error = function(x) {
      dt <- data.table::as.data.table(dt)
    })
  }

  if(Debug) print("here 1")

  # YVar check
  yvar_class <- class(dt[[YVar]])[1L]
  if(yvar_class %in% c("factor","character")) {

    if(Debug) print("here 2")

    # Shrink data
    yvar_levels <- as.character(as.character(dt[, unique(get(YVar))]))
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(XVar, YVar, yvar_levels, GroupVar)])

    if(Debug) print("here 3")

    # Dummify Target
    nam <- data.table::copy(names(dt1))
    dt1 <- Rodeo::DummifyDT(data = dt1, cols = YVar, TopN = length(yvar_levels), KeepFactorCols = FALSE, OneHot = FALSE, SaveFactorLevels = FALSE, SavePath = getwd(), ImportFactorLevels = FALSE, FactorLevelsList = NULL, ClustScore = FALSE, ReturnFactorLevels = FALSE)
    nam <- setdiff(names(dt1), nam)

    if(Debug) print("here 4")

    # Melt Predict Cols
    dt2 <- data.table::melt.data.table(
      data = dt1[, .SD, .SDcols = c(names(dt1)[!names(dt1) %in% c(nam,XVar)])],
      id.vars = GroupVar,
      measure.vars = names(dt1)[!names(dt1) %in% c(nam,XVar,GroupVar)],
      variable.name = "Level",
      value.name = XVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    if(Debug) print("here 5")

    # Melt Target Cols
    dt3 <- data.table::melt.data.table(
      data = dt1[, .SD, .SDcols = c(names(dt1)[!names(dt1) %in% c(yvar_levels,XVar)])],
      id.vars = GroupVar,
      measure.vars = nam,
      variable.name = "Level",
      value.name = YVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    if(Debug) print("here 6")

    # Join data
    dt2[, eval(YVar) := dt3[[YVar]]]

    if(Debug) print("here 7")

    # Update Args
    if(length(GroupVar) > 0L) {
      dt2[, GroupVariables := do.call(paste, c(.SD, sep = ' :: ')), .SDcols = c(GroupVar, "Level")]
      GroupVar <- "GroupVariables"
      if(FacetRows > 1L && FacetCols > 1L) {
        FacetLevels <- as.character(dt2[, unique(GroupVariables)])
        FacetLevels <- FacetLevels[seq_len(min(length(FacetLevels),FacetRows*FacetCols))]
        dt2 <- dt2[GroupVariables %chin% c(eval(FacetLevels))]
      } else {
        FacetLevels <- yvar_levels
      }
    } else {
      if(FacetRows > 1L && FacetCols > 1L) {
        FacetLevels <- yvar_levels
        FacetLevels <- FacetLevels[seq_len(min(length(FacetLevels),FacetRows*FacetCols))]
        dt2 <- dt2[Level %chin% c(eval(FacetLevels))]
      } else {
        FacetLevels <- yvar_levels
      }
      GroupVar <- "Level"
      dt2 <- dt2[Level %chin% c(eval(FacetLevels))]
      GroupVar <- "Level"
    }

  } else {
    dt2 <- data.table::copy(dt)
  }

  if(Debug) print("here 9")

  if(yvar_class %in% c("factor","character") || length(GroupVar) > 0L) {
    Levels <- sort(as.character(unique(dt2[[GroupVar]])))
    dl <- list()
    if(Debug) print("Start For-Loop")
    if(length(NumberBins) == 0L) NumberBins <- 21
    if(max(NumberBins) > 1L) NumberBins <- c(seq(1/NumberBins, 1 - 1/NumberBins, 1/NumberBins), 1)
    for(i in Levels) {# i = Levels[i]
      if(Debug) print("iter")
      if(Debug) print(i)
      dt_ <- dt2[get(GroupVar) %in% eval(i)]
      if(Debug) print(" iter 2")
      dt_[, NegScore := -get(XVar)]
      if(Debug) print(" iter 3")
      if(Debug) print(" iter 4")
      Cuts <- quantile(x = dt_[["NegScore"]], na.rm = TRUE, probs = NumberBins)
      if(Debug) print(" iter 5")
      dt_[, eval(YVar) := as.character(get(YVar))]
      if(Debug) print(" iter 6")
      grp <- dt_[, .N, by = eval(YVar)][order(N)]
      if(Debug) print(" iter 7")
      smaller_class <- grp[1L, 1L][[1L]]
      if(Debug) print(" iter 8")
      dt3 <- round(100 * sapply(Cuts, function(x) {
        dt_[NegScore <= x & get(YVar) == eval(smaller_class), .N] / dt_[get(YVar) == eval(smaller_class), .N]
      }), 2)
      if(Debug) print(" iter 9")
      dt3 <- rbind(dt3, -Cuts)
      if(Debug) print(" iter 10")
      rownames(dt3) <- c("Lift", "Score.Point")
      if(Debug) print(" iter 11")
      dt4 <- grp[1,2] / (grp[2,2] + grp[1,2])
      if(Debug) print(" iter 12")
      dt5 <- data.table::as.data.table(t(dt3))
      if(Debug) print(" iter 13")
      dt5[, Population := as.numeric(100 * eval(NumberBins))]
      if(Debug) print(" iter 14")
      dt5[, Lift := round(Lift / 100 / NumberBins, 2)]
      if(Debug) print(" iter 15")
      dt5[, Level := eval(i)]
      if(Debug) print(" iter 16")
      if(data.table::is.data.table(dt5)) {
        if(Debug) print(" iter rbindlist")
        dl[[i]] <- data.table::rbindlist(list(
          data.table::data.table(Lift = 0, Score.Point = 0, Population = 0, Level = eval(i)),
          dt5
        ), use.names = TRUE)
      }
    }
    if(Debug) print(" For Loop Done: rbindlist")
    dt6 <- data.table::rbindlist(dl)

  } else {

    if(Debug) print("here 10")

    # Data Prep
    dt2[, NegScore := -get(XVar)]
    NumberBins <- c(seq(1/NumberBins, 1 - 1/NumberBins, 1/NumberBins), 1)
    Cuts <- quantile(x = dt2[["NegScore"]], na.rm = TRUE, probs = NumberBins)
    dt2[, eval(YVar) := as.character(get(YVar))]
    grp <- dt2[, .N, by = eval(YVar)][order(N)]
    smaller_class <- grp[1L, 1L][[1L]]
    dt3 <- round(100 * sapply(Cuts, function(x) {
      dt2[NegScore <= x & get(YVar) == eval(smaller_class), .N] / dt2[get(YVar) == eval(smaller_class), .N]
    }), 2)
    dt3 <- rbind(dt3, -Cuts)
    rownames(dt3) <- c("Lift", "Score.Point")
    dt4 <- grp[1,2] / (grp[2,2] + grp[1,2])
    dt5 <- data.table::as.data.table(t(dt3))
    dt5[, Population := as.numeric(100 * eval(NumberBins))]
    dt5[, Lift := round(Lift / 100 / NumberBins, 2)]
    if(data.table::is.data.table(dt5)) {
      dt6 <- data.table::rbindlist(list(
        data.table::data.table(Score.Point = 0, Population = 0, Lift = 0),
        dt5
      ), use.names = TRUE)
    }
  }

  if(Debug) print("here 11")

  # Build
  if(Debug) print(names(dt6))
  if("Level" %in% names(dt6)) {
    dt6 <- dt6[Population > 0, .SD, .SDcols = c("Population","Lift", "Level")]
    GroupVar <- "Level"
  } else {
    dt6 <- dt6[Population > 0, .SD, .SDcols = c("Population","Lift")]
  }

  if(Debug) print("here 12")

  if(FacetRows == 1L && FacetCols == 1L && length(GroupVar) > 0L) {

    if(Debug) print("here 13")
    #dt6 <- dt6[!is.na(Lift)]
    p1 <- AutoPlots::Plot.Line(
      dt = dt6,
      PreAgg = TRUE,
      XVar = "Population",
      YVar = "Lift",
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      ShowLabels = ShowLabels,
      Title.YAxis = "Lift",
      Title.XAxis = "Population",
      FacetLevels = NULL,
      Height = Height,
      Width = Width,
      Title = Title,
      Area = FALSE,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      EchartsTheme = EchartsTheme,
      TimeLine = FALSE,
      X_Scroll = X_Scroll,
      Y_Scroll = Y_Scroll,
      TextColor = TextColor,
      Debug = FALSE)
  } else {

    if(Debug) print("here 14")
    #dt6 <- dt6[!is.na(Lift)]
    p1 <- AutoPlots::Plot.Area(
      dt = dt6,
      PreAgg = TRUE,
      XVar = "Population",
      YVar = "Lift",
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = NULL,
      Height = Height,
      Width = Width,
      Title = Title,
      ShowLabels = ShowLabels,
      Title.YAxis = "Lift",
      Title.XAxis = "Population",
      Smooth = TRUE,
      ShowSymbol = FALSE,
      EchartsTheme = EchartsTheme,
      TimeLine = FALSE,
      X_Scroll = X_Scroll,
      Y_Scroll = Y_Scroll,
      TextColor = TextColor,
      Debug = FALSE)
  }

  if(Debug) print("here 16")


  if(Debug) print("here 17")
  p1 <- echarts4r::e_labels(e = p1, show = TRUE)

  # Return
  return(p1)
}

#' @title Plot.Gains
#'
#' @description Create a cumulative gains chart
#'
#' @family Model Evaluation
#'
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param ZVar character
#' @param GroupVar Character variable
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param NumberBins numeric
#' @param PreAgg logical
#' @param NumberBins numeric
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor character hex
#' @param Debug Debugging purposes
#'
#' @export
Plot.Gains <- function(dt = NULL,
                       PreAgg = FALSE,
                       XVar = NULL,
                       YVar = NULL,
                       ZVar = "N",
                       GroupVar = NULL,
                       YVarTrans = "Identity",
                       XVarTrans = "Identity",
                       ZVarTrans = "Identity",
                       FacetRows = 1,
                       FacetCols = 1,
                       FacetLevels = NULL,
                       NumberBins = 20,
                       Height = NULL,
                       Width = NULL,
                       Title = "Gains Plot",
                       ShowLabels = FALSE,
                       Title.YAxis = "Gain",
                       Title.XAxis = "Population",
                       EchartsTheme = "macarons",
                       TimeLine = TRUE,
                       X_Scroll = TRUE,
                       Y_Scroll = TRUE,
                       TextColor =        "white",
                       Debug = FALSE) {

  if(Debug) print("here 1")
  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # YVar check
  yvar_class <- class(dt[[YVar]])[1L]
  if(yvar_class %in% c("factor","character")) {

    if(Debug) print("here 2")

    # Shrink data
    yvar_levels <- as.character(as.character(dt[, unique(get(YVar))]))
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(XVar, YVar, yvar_levels, GroupVar)])

    if(Debug) print("here 3")

    # Dummify Target
    nam <- data.table::copy(names(dt1))
    dt1 <- Rodeo::DummifyDT(data = dt1, cols = YVar, TopN = length(yvar_levels), KeepFactorCols = FALSE, OneHot = FALSE, SaveFactorLevels = FALSE, SavePath = getwd(), ImportFactorLevels = FALSE, FactorLevelsList = NULL, ClustScore = FALSE, ReturnFactorLevels = FALSE)
    nam <- setdiff(names(dt1), nam)

    if(Debug) print("here 4")

    # Melt Predict Cols
    dt2 <- data.table::melt.data.table(
      data = dt1[, .SD, .SDcols = c(names(dt1)[!names(dt1) %in% c(nam,XVar)])],
      id.vars = GroupVar,
      measure.vars = names(dt1)[!names(dt1) %in% c(nam,XVar,GroupVar)],
      variable.name = "Level",
      value.name = XVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    if(Debug) print("here 5")

    # Melt Target Cols
    dt3 <- data.table::melt.data.table(
      data = dt1[, .SD, .SDcols = c(names(dt1)[!names(dt1) %in% c(yvar_levels,XVar)])],
      id.vars = GroupVar,
      measure.vars = nam,
      variable.name = "Level",
      value.name = YVar,
      na.rm = TRUE,
      variable.factor = FALSE)

    if(Debug) print("here 6")

    # Join data
    dt2[, eval(YVar) := dt3[[YVar]]]

    if(Debug) print("here 7")

    # Update Args
    if(length(GroupVar) > 0L) {
      dt2[, GroupVariables := do.call(paste, c(.SD, sep = ' :: ')), .SDcols = c(GroupVar, "Level")]
      GroupVar <- "GroupVariables"
      if(FacetRows > 1L && FacetCols > 1L) {
        FacetLevels <- as.character(dt2[, unique(GroupVariables)])
        FacetLevels <- FacetLevels[seq_len(min(length(FacetLevels),FacetRows*FacetCols))]
        dt2 <- dt2[GroupVariables %chin% c(eval(FacetLevels))]
      } else {
        FacetLevels <- yvar_levels
      }
    } else {
      if(FacetRows > 1L && FacetCols > 1L) {
        FacetLevels <- yvar_levels
        FacetLevels <- FacetLevels[seq_len(min(length(FacetLevels),FacetRows*FacetCols))]
        dt2 <- dt2[Level %chin% c(eval(FacetLevels))]
      } else {
        FacetLevels <- yvar_levels
      }
      GroupVar <- "Level"
      dt2 <- dt2[Level %chin% c(eval(FacetLevels))]
      GroupVar <- "Level"
    }

  } else {
    dt2 <- data.table::copy(dt)
  }

  if(Debug) print("here 9")

  if(yvar_class %in% c("factor","character") || length(GroupVar) > 0L) {
    Levels <- sort(as.character(unique(dt2[[GroupVar]])))
    dl <- list()
    if(Debug) print("Start For-Loop")
    if(length(NumberBins) == 0L) NumberBins <- 21
    if(max(NumberBins) > 1L) NumberBins <- c(seq(1/NumberBins, 1 - 1/NumberBins, 1/NumberBins), 1)
    for(i in Levels) {# i = 1
      if(Debug) print("iter")
      if(Debug) print(i)
      dt_ <- dt2[get(GroupVar) %in% eval(i)]
      if(Debug) print(" iter 2")
      dt_[, NegScore := -get(XVar)]
      if(Debug) print(" iter 3")
      if(Debug) print(" iter 4")
      Cuts <- quantile(x = dt_[["NegScore"]], na.rm = TRUE, probs = NumberBins)
      if(Debug) print(" iter 5")
      dt_[, eval(YVar) := as.character(get(YVar))]
      if(Debug) print(" iter 6")
      grp <- dt_[, .N, by = eval(YVar)][order(N)]
      if(Debug) print(" iter 7")
      smaller_class <- grp[1L, 1L][[1L]]
      if(Debug) print(" iter 8")
      dt3 <- round(100 * sapply(Cuts, function(x) {
        dt_[NegScore <= x & get(YVar) == eval(smaller_class), .N] / dt_[get(YVar) == eval(smaller_class), .N]
      }), 2)
      if(Debug) print(" iter 9")
      dt3 <- rbind(dt3, -Cuts)
      if(Debug) print(" iter 10")
      rownames(dt3) <- c("Gain", "Score.Point")
      if(Debug) print(" iter 11")
      dt4 <- grp[1,2] / (grp[2,2] + grp[1,2])
      if(Debug) print(" iter 12")
      dt5 <- data.table::as.data.table(t(dt3))
      if(Debug) print(" iter 13")
      dt5[, Population := as.numeric(100 * eval(NumberBins))]
      if(Debug) print(" iter 14")
      dt5[, Gain := round(Gain / 100 / NumberBins, 2)]
      if(Debug) print(" iter 15")
      dt5[, Level := eval(i)]
      if(Debug) print(" iter 16")
      if(data.table::is.data.table(dt5)) {
        if(Debug) print(" iter rbindlist")
        dl[[i]] <- data.table::rbindlist(list(
          data.table::data.table(Gain = 0, Score.Point = 0, Population = 0, Level = eval(i)),
          dt5
        ), use.names = TRUE)
      }
    }
    dt6 <- data.table::rbindlist(dl)
    if(Debug) print(" For Loop Done: rbindlist")
    if("Level" %in% names(dt5)) {
      dt6 <- dt6[Population > 0, .SD, .SDcols = c("Population","Gain", "Level")]
      GroupVar <- "Level"
    } else {
      dt6 <- dt6[Population > 0, .SD, .SDcols = c("Population","Gain")]
    }

  } else {

    if(Debug) print("here 10")

    # Data Prep
    dt2[, NegScore := -get(XVar)]
    NumberBins <- c(seq(1/NumberBins, 1 - 1/NumberBins, 1/NumberBins), 1)
    Cuts <- quantile(x = dt2[["NegScore"]], na.rm = TRUE, probs = NumberBins)
    dt2[, eval(YVar) := as.character(get(YVar))]
    grp <- dt2[, .N, by = eval(YVar)][order(N)]
    smaller_class <- grp[1L, 1L][[1L]]
    dt3 <- round(100 * sapply(Cuts, function(x) {
      dt2[NegScore <= x & get(YVar) == eval(smaller_class), .N] / dt2[get(YVar) == eval(smaller_class), .N]
    }), 2)
    dt3 <- rbind(dt3, -Cuts)
    rownames(dt3) <- c("Gain", "Score.Point")
    dt4 <- grp[1,2] / (grp[2,2] + grp[1,2])
    dt5 <- data.table::as.data.table(t(dt3))
    dt5[, Population := as.numeric(100 * eval(NumberBins))]
    dt5[, Gain := round(Gain / 100 / NumberBins, 2)]
    if(data.table::is.data.table(dt5)) {
      dt6 <- data.table::rbindlist(list(
        data.table::data.table(Gain = 0, Score.Point = 0, Population = 0),
        dt5
      ), use.names = TRUE)
    }
  }

  if(Debug) print("here 11")

  # Build
  if(Debug) print(names(dt6))
  if(length(GroupVar) > 0L && GroupVar %in% names(dt6)) {
    dt6 <- dt6[Population > 0, .SD, .SDcols = c("Population","Gain", GroupVar)]
  } else {
    dt6 <- dt6[Population > 0, .SD, .SDcols = c("Population","Gain")]
  }

  if(Debug) print("here 12")

  if(FacetRows == 1L && FacetCols == 1L && length(GroupVar) > 0L) {

    if(Debug) print("here 13")
    #dt6 <- dt6[!is.na(Gain)]
    p1 <- AutoPlots::Plot.Line(
      dt = dt6,
      PreAgg = TRUE,
      XVar = "Population",
      YVar = "Gain",
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = NULL,
      ShowLabels = ShowLabels,
      Title.YAxis = "Gain",
      Title.XAxis = "Population",
      Height = Height,
      Width = Width,
      Title = Title,
      Area = FALSE,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      EchartsTheme = EchartsTheme,
      TimeLine = FALSE,
      X_Scroll = X_Scroll,
      Y_Scroll = Y_Scroll,
      TextColor = TextColor,
      Debug = FALSE)
  } else {

    if(Debug) print("here 14")
    #dt6 <- dt6[!is.na(Gain)]
    p1 <- AutoPlots::Plot.Area(
      dt = dt6,
      PreAgg = TRUE,
      XVar = "Population",
      YVar = "Gain",
      GroupVar = GroupVar,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = NULL,
      ShowLabels = ShowLabels,
      Title.YAxis = "Gain",
      Title.XAxis = "Population",
      Height = Height,
      Width = Width,
      Title = Title,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      EchartsTheme = EchartsTheme,
      TimeLine = FALSE,
      X_Scroll = X_Scroll,
      Y_Scroll = Y_Scroll,
      TextColor = TextColor,
      Debug = FALSE)
  }

  if(Debug) print("here 16")


  if(Debug) print("here 17")
  p1 <- echarts4r::e_labels(e = p1, show = TRUE)

  # Return
  return(p1)
}

#' @title Plot.BinaryMetrics
#'
#' @description Line plot of evaluation metrics across thresholds
#'
#' @author Adrian Antico
#' @family Model Evaluation
#'
#' @param dt source data.table
#' @param SampleSize numeric
#' @param XVar X-Axis variable name
#' @param YVar Y-Axis variable name
#' @param ZVar character
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param Metrics Multiple selection "Utility","MCC","Accuracy","F1_Score","F2_Score","F0.5_Score","ThreatScore","TPR","TNR","FNR","FPR","FDR","FOR"
#' @param NumberBins numeric
#' @param PreAgg logical
#' @param Title character
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param EchartsTheme "auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", #' "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired", #' "jazz","london","dark","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal", #' "sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland"
#' @param EchartsLabels character
#' @param TimeLine logical
#' @param X_Scroll logical
#' @param Y_Scroll logical
#' @param TextColor hex character
#' @param AggMethod character
#' @param GroupVar Character variable
#' @param Debug Debugging purposes
#' @export
Plot.BinaryMetrics <- function(dt = NULL,
                               PreAgg = FALSE,
                               AggMethod = "mean",
                               SampleSize = 100000L,
                               XVar = NULL,
                               YVar = NULL,
                               ZVar = NULL,
                               Metrics = c("Utility","MCC","Accuracy","F1_Score","F2_Score","F0.5_Score","ThreatScore","TPR","TNR","FNR","FPR","FDR","FOR"),
                               GroupVar = NULL,
                               YVarTrans = "Identity",
                               XVarTrans = "Identity",
                               ZVarTrans = "Identity",
                               FacetRows = 1,
                               FacetCols = 1,
                               FacetLevels = NULL,
                               CostMatrixWeights = c(0,1,1,0),
                               NumberBins = 20,
                               Height = NULL,
                               Width = NULL,
                               Title = "Binary Metrics",
                               ShowLabels = FALSE,
                               Title.YAxis = NULL,
                               Title.XAxis = NULL,
                               EchartsTheme = "macarons",
                               EchartsLabels = FALSE,
                               TimeLine = TRUE,
                               X_Scroll = TRUE,
                               Y_Scroll = FALSE,
                               TextColor =        "white",
                               Debug = FALSE) {

  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # Minimize data before moving on
  if(Debug) print("Plot.PartialDependence.Box # Minimize data before moving on")
  Ncols <- ncol(dt)
  if(Ncols > 2L && length(GroupVar) == 0L) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, ZVar)])
  } else if(Ncols > 3L && length(GroupVar) > 0L) {
    dt1 <- data.table::copy(dt[, .SD, .SDcols = c(YVar, XVar, ZVar, GroupVar[1L])])
  } else {
    dt1 <- data.table::copy(dt)
  }

  # If actual is in factor form, convert to numeric
  if(Debug) print("Plot.PartialDependence.Box # If actual is in factor form, convert to numeric")
  if(!is.numeric(dt1[[YVar]])) {
    data.table::set(dt1, j = YVar, value = as.numeric(as.character(dt1[[YVar]])))
  }

  # Build Plot
  tl <- if(length(GroupVar) == 0L) FALSE else TimeLine
  dt2 <- AutoQuant:::BinaryMetrics(
    ValidationData. = dt1,
    TargetColumnName. = "BinaryTarget",
    CostMatrixWeights. = CostMatrixWeights,
    SaveModelObjects. = FALSE)
  dt3 <- data.table::melt.data.table(
    data = dt2,
    id.vars = "Threshold",
    measure.vars = Metrics)

  # Build
  if(Debug) print("AutoPlots::Plot.BinaryMetrics --> AutoPlots::Plot.Line()")
  p1 <- AutoPlots::Plot.Line(
    dt = dt3,
    PreAgg = TRUE,
    AggMethod = "mean",
    Area = FALSE,
    SampleSize = SampleSize,
    XVar = XVar,
    YVar = YVar,
    GroupVar = GroupVar,
    YVarTrans = YVarTrans,
    XVarTrans = XVarTrans,
    FacetRows = FacetRows,
    FacetCols = FacetCols,
    FacetLevels = FacetLevels,
    Height = Height,
    Width = Width,
    Title = Title,
    EchartsTheme = EchartsTheme,
    TimeLine = tl,
    TextColor = TextColor,
    X_Scroll = X_Scroll,
    Y_Scroll = Y_Scroll,
    Debug = Debug)
  return(p1)
}

#' @title Plot.ShapImportance
#'
#' @description Plot.ShapImportance variable importance
#'
#' @family Model Evaluation
#' @author Adrian Antico
#'
#' @param dt source data.table
#' @param EchartsTheme "dark-blue"
#' @param YVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param XVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param ZVarTrans "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit", "PercRank", "Standardize", "BoxCox", "YeoJohnson"
#' @param FacetRows Defaults to 1 which causes no faceting to occur vertically. Otherwise, supply a numeric value for the number of output grid rows
#' @param FacetCols Defaults to 1 which causes no faceting to occur horizontally. Otherwise, supply a numeric value for the number of output grid columns
#' @param FacetLevels Faceting rows x columns is the max number of levels allowed in a grid. If your GroupVar has more you can supply the levels to display.
#' @param AggMethod "mean", "median", "sum", "sd", "skewness","kurtosis", "coeffvar", "meanabs", "medianabs", "sumabs", "sdabs", "skewnessabs", "kurtosisabs", "CoeffVarabs"
#' @param NumberBins = 21
#' @param NumLevels_Y = 20
#' @param NumLevels_X = 20
#' @param Title "Heatmap"
#' @param ShowLabels character
#' @param Title.YAxis character
#' @param Title.XAxis character
#' @param Debug = FALSE
#'
#' @export
Plot.ShapImportance <- function(dt,
                                PreAgg = FALSE,
                                AggMethod = 'meanabs',
                                YVar = NULL,
                                GroupVar = NULL,
                                YVarTrans = "Identity",
                                XVarTrans = "Identity",
                                ZVarTrans = "Identity",
                                FacetRows = 1,
                                FacetCols = 1,
                                FacetLevels = NULL,
                                NumberBins = 21,
                                NumLevels_X = 33,
                                NumLevels_Y = 33,
                                Height = NULL,
                                Width = NULL,
                                Title = "Shap Importance",
                                ShowLabels = FALSE,
                                Title.YAxis = NULL,
                                Title.XAxis = NULL,
                                EchartsTheme = "dark",
                                X_Scroll = TRUE,
                                Y_Scroll = TRUE,
                                TextColor =        "white",
                                Debug = FALSE) {

  if(Debug) print("ShapImportance Step 1")

  # Subset columns
  if(!PreAgg) {

    if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
      dt <- data.table::as.data.table(dt)
    })
    if(Debug) print("ShapImportance Step 2")
    if(length(GroupVar) > 1L) GroupVar <- GroupVar[1L]
    if(length(YVar) == 0L) YVar <- names(dt)[names(dt) %like% "Shap_"]
    dt1 <- dt[, .SD, .SDcols = c(YVar, GroupVar)]

    # Define Aggregation function
    if(Debug) print("Plot.ShapImportance # Define Aggregation function")
    if(Debug) print(AggMethod)
    aggFunc <- AutoPlots:::SummaryFunction(AggMethod)

    if(length(GroupVar) > 0L) {
      dt1 <- dt1[, lapply(.SD, FUN = noquote(aggFunc)), by = c(GroupVar)]
      dt2 <- data.table::melt.data.table(data = dt1, id.vars = c(GroupVar), measure.vars = YVar, variable.name = "Variable", value.name = "Importance")
    } else {
      dt1 <- dt1[, lapply(.SD, FUN = noquote(aggFunc))]
      dt2 <- data.table::melt.data.table(data = dt1, id.vars = NULL, measure.vars = YVar, variable.name = "Variable", value.name = "Importance")
    }
  } else {
    dt2 <- data.table::copy(dt)
  }

  # Add a column that ranks predicted values
  if(length(GroupVar) > 0L) {
    p1 <- AutoPlots::Plot.HeatMap(
      dt = dt2,
      PreAgg = TRUE,
      AggMethod = "mean",
      YVar = "Variable",
      XVar = GroupVar,
      ZVar = "Importance",
      NumberBins = 21,
      NumLevels_X = NumLevels_Y,
      NumLevels_Y = NumLevels_X,
      Height = Height,
      Width = Width,
      Title = paste0("Shap Importance: AggMethod = ", AggMethod),
      EchartsTheme = EchartsTheme,
      X_Scroll = X_Scroll,
      Y_Scroll = Y_Scroll)
    return(p1)
  } else {

    if(Debug) print("Right Here Yo")

    p1 <- AutoPlots::Plot.VariableImportance(
      dt = dt2,
      AggMethod = 'mean',
      XVar = "Variable",
      YVar = "Importance",
      GroupVar = NULL,
      YVarTrans = YVarTrans,
      XVarTrans = XVarTrans,
      FacetRows = FacetRows,
      FacetCols = FacetCols,
      FacetLevels = FacetLevels,
      Height = Height,
      Width = Width,
      Title = paste0("Shap Importance: AggMethod = ", AggMethod),
      EchartsTheme = EchartsTheme,
      TimeLine = TimeLine,
      TextColor = TextColor,
      Debug = Debug)
    return(p1)
  }
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# > Stocks Plots Functions                                                    ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# #' @noRd
# holidayNYSE <- function(year = getRmetricsOptions("currentYear")) {
#   # A function implemented by Diethelm Wuertz
#   # improved speed and handling of time zone by Yohan Chalabi
#
#   # Description:
#   #   Returns 'timeDate' object for full-day NYSE holidays
#
#   # Arguments:
#   #   year - an integer variable or vector for the year(s)
#   #       ISO-8601 formatted as "CCYY" where easter or
#   #       easter related feasts should be computed.
#
#   # Value:
#   #   Returns the holiday calendar for the NYSE formatted as
#   #   'timeDate' object.
#
#   # Details:
#   #   The "New York Stock Exchange" calendar starts from year 1885.
#   #   The rules are listed at the web site http://www.nyse.com.
#
#   # Example:
#   #   > holiday.NYSE(2004)
#   #   [1] "America/New_York"
#   #   [1] [2004-01-01] [2004-01-19] [2004-02-16] [2004-04-09]
#   #   [5] [2004-05-31] [2004-07-05] [2004-09-06] [2004-11-25]
#
#   # FUNCTION:
#   library(timeDate)
#   #  Settings:
#   holidays <- NULL
#
#   # Iterate years:
#   for (y in year ) {
#     if (y >= 1885)
#       holidays <- c(holidays, as.character(USNewYearsDay(y)))
#     if (y >= 1885)
#       holidays <- c(holidays, as.character(USIndependenceDay(y)))
#     if (y >= 1885)
#       holidays <- c(holidays, as.character(USThanksgivingDay(y)))
#     if (y >= 1885)
#       holidays <- c(holidays, as.character(USChristmasDay(y)))
#     if (y >= 1887)
#       holidays <- c(holidays, as.character(USLaborDay(y)))
#     if (y != 1898 & y != 1906 & y != 1907)
#       holidays <- c(holidays, as.character(USGoodFriday(y)))
#     if (y >= 1909 & y <= 1953)
#       holidays <- c(holidays, as.character(USColumbusDay(y)))
#     if (y >= 1998)
#       holidays <- c(holidays, as.character(USMLKingsBirthday(y)))
#     if (y >= 1896 & y <= 1953)
#       holidays <- c(holidays, as.character(USLincolnsBirthday(y)))
#     if (y <= 1970)
#       holidays <- c(holidays, as.character(USWashingtonsBirthday(y)))
#     if (y > 1970)
#       holidays <- c(holidays, as.character(USPresidentsDay(y)))
#     if (y == 1918 | y == 1921 | (y >= 1934 & y <= 1953))
#       holidays <- c(holidays, as.character(USVeteransDay(y)))
#     if (y <= 1968 | y == 1972 | y == 1976 | y == 1980)
#       holidays <- c(holidays, as.character(USElectionDay(y)))
#     if (y <= 1970)
#       holidays <- c(holidays, as.character(USDecorationMemorialDay(y)))
#     if (y >= 1971)
#       holidays <- c(holidays, as.character(USMemorialDay(y)))
#   }
#
#   # Sort and Convert to 'timeDate':
#   holidays <- sort(holidays)
#   ans <- timeDate(format(holidays), zone = "NewYork", FinCenter = "NewYork")
#
#   # Move Sunday Holidays to Monday:
#   posix1 <- as.POSIXlt(ans, tz = "GMT")
#   ans <- ans + as.integer(posix1$wday==0) * 24 * 3600
#
#   # After July 3, 1959, move Saturday holidays to Friday
#   # ... except if at the end of monthly/yearly accounting period
#   # this is the last business day of a month.
#   posix2 <- as.POSIXlt(as.POSIXct(ans, tz = "GMT") - 24 * 3600)
#   y <- posix2$year + 1900
#   m <- posix2$mon + 1
#   calendar <- timeCalendar(y = y+(m+1)%/%13,
#                            m = m+1-(m+1)%/%13*12, d = 1,
#                            zone = "GMT", FinCenter = "GMT")
#   lastday <- as.POSIXlt(calendar - 24*3600, tz = "GMT")$mday
#   lon <- .last.of.nday(year = y, month = m, lastday = lastday, nday = 5)
#   ExceptOnLastFriday <- timeDate(format(lon), zone = "NewYork",
#                                  FinCenter = "NewYork")
#   ans <- ans - as.integer(ans >= timeDate("1959-07-03",
#                                           zone ="GMT", FinCenter = "GMT") &
#                             as.POSIXlt(ans, tz = "GMT")$wday == 6  &
#                             (ans - 24*3600) != ExceptOnLastFriday ) * 24 * 3600
#
#   # Remove Remaining Weekend Dates:
#   posix3 <- as.POSIXlt(ans, tz = "GMT")
#   ans <- ans[ !(posix3$wday == 0 | posix3$wday == 6)]
#
#   # Return Value:
#   ans
# }
#
# #' @noRd
# StockSymbols <- function() {
#   x <- jsonlite::fromJSON("https://api.polygon.io/v3/reference/tickers?active=true&sort=ticker&order=asc&limit=1000&apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20")
#   xx <- data.table::setDT(x$results)
#   return(xx[, .SD, .SDcols = c(names(xx)[c(1,2,5,6,12)])])
# }
#
# #' @noRd
# GetAllTickers <- function() {
#   x <- jsonlite::fromJSON("https://api.polygon.io/v3/reference/tickers?active=true&sort=ticker&order=asc&limit=1000&apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20")
#   xx <- data.table::setDT(x$results)
#   counter <- 1000L
#   while(is.list(x)) {
#     if(Debug) print(paste0('Working on first ', counter, ' ticker symbols'))
#     x <- tryCatch({jsonlite::fromJSON(paste0(x$next_url, "&apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20"))}, error = function(x) 1)
#     if(x != 1) {
#       xx <- data.table::rbindlist(list(xx, data.table::setDT(x$results)), fill = TRUE, use.names = TRUE)
#       counter <- counter + 1000L
#       Sys.sleep(12L)
#     } else {
#       break
#     }
#   }
#   #xx <- xx[, .SD, .SDcols = c(names(xx)[c(1,2,5,6,12)])]
#   AutoQuant::PostGRE_RemoveCreateAppend(
#     data = xx,
#     TableName = "ticker_data",
#     CloseConnection = TRUE,
#     CreateSchema = NULL,
#     Host = "localhost",
#     DBName = "RemixAutoML",
#     User = "postgres",
#     Port = 5432,
#     Password = "Aa1028#@",
#     Temporary = FALSE,
#     Connection = NULL,
#     Append = TRUE)
#   return(xx)
# }
#
# #' @noRd
# OptionsSymbols <- function() {
#   x <- jsonlite::fromJSON('https://api.polygon.io/v3/reference/tickers/types?asset_class=options&locale=us&apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20')
#   xx <- data.table::setDT(x$results)
#   return(xx[, .SD, .SDcols = c(names(xx)[c(1,2,5,6,12)])])
# }
#
# #' @noRd
# CryptoSymbols <- function() {
#   x <- jsonlite::fromJSON('https://api.polygon.io/v3/reference/tickers/types?asset_class=crypto&locale=us&apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20')
#   xx <- data.table::setDT(x$results)
#   return(xx[, .SD, .SDcols = c(names(xx)[c(1,2,5,6,12)])])
# }
#
# #' @noRd
# Financials <- function() {
#   x <- jsonlite::fromJSON("https://api.polygon.io/vX/reference/financials?apiKey=hvyL7ZOsKK_5PNplOmv55tBTRd8rdA20")
# }
#
# #' @title StockData
# #'
# #' @description  Create stock data for plotting using Plot.Stock()
# #'
# #' @family Stock Plots
# #' @author Adrian Antico
# #'
# #' @param PolyOut NULL. If NULL, data is pulled. If supplied, data is not pulled.
# #' @param Type 'candlestick', 'ohlc'
# #' @param Metric Stock Price, Percent Returns (use symbol for percent), Percent Log Returns (use symbol for percent), Index, Quadratic Variation
# #' @param TimeAgg = 'days', 'weeks', 'months'
# #' @param Symbol ticker symbol string
# #' @param CompanyName company name if you have it. ends up in title, that is all
# #' @param StartDate Supply a start date. E.g. '2022-01-01'
# #' @param EndDate Supply an end date. E.g. `Sys.Date()`
# #' @param APIKey Supply your polygon API key
# #' @param timeElapsed = 60
# #'
# #' @export
# StockData <- function(PolyOut = NULL,
#                       Symbol = 'TSLA',
#                       CompanyName = 'Tesla Inc. Common Stock',
#                       Metric = 'Stock Price',
#                       TimeAgg = 'days',
#                       StartDate = '2022-01-01',
#                       EndDate = Sys.Date(),
#                       APIKey = NULL,
#                       timeElapsed = 61,
#                       Debug = FALSE) {
#
#   if(length(APIKey) == 0L) return(NULL)
#
#   StartDate <- as.Date(StartDate)
#   EndDate <- min(Sys.Date()-1, as.Date(EndDate))
#
#   # Use data if provided
#   if(!data.table::is.data.table(PolyOut)) {
#     if(Debug) print("here 1a")
#     PolyOut <- jsonlite::fromJSON(paste0("https://api.polygon.io/v2/aggs/ticker/",Symbol,"/range/1/day/",StartDate, "/", EndDate, "?adjusted=true&sort=asc&limit=10000&apiKey=", APIKey))
#
#     data <- data.table::as.data.table(PolyOut$results)
#     data[, Date := as.Date(lubridate::as_datetime((t+10800000)/1000, origin = "1970-01-01"))]
#     if(Debug) print(head(data))
#
#     tryCatch({
#       if(TimeAgg == 'weeks') {
#         data[, Date := lubridate::floor_date(Date, unit = 'weeks')]
#         data <- data[, lapply(.SD, mean, na.rm = TRUE), .SD = c('v','vw','o','c','h','l','t','n'), by = 'Date']
#       } else if(TimeAgg == 'months') {
#         data[, Date := lubridate::floor_date(Date, unit = 'months')]
#         data <- data[, lapply(.SD, mean, na.rm = TRUE), .SD = c('v','vw','o','c','h','l','t','n'), by = 'Date']
#       } else if(TimeAgg == 'quarters') {
#         data[, Date := lubridate::floor_date(Date, unit = 'quarters')]
#         data <- data[, lapply(.SD, mean, na.rm = TRUE), .SD = c('v','vw','o','c','h','l','t','n'), by = 'Date']
#       } else if(TimeAgg == 'years') {
#         data[, Date := lubridate::floor_date(Date, unit = 'years')]
#         data <- data[, lapply(.SD, mean, na.rm = TRUE), .SD = c('v','vw','o','c','h','l','t','n'), by = 'Date']
#       }
#
#       if(Metric == '% Returns') {
#         for(i in c('o','c','h','l')) data[, paste0(i) := get(i) / data.table::shift(x = get(i)) - 1]
#         data <- data[seq_len(.N)[-1L]]
#       } else if(Metric  == '% Log Returns') {
#         for(i in c('o','c','h','l')) data[, paste0(i) := log(get(i)) - log(data.table::shift(x = get(i)))]
#         data <- data[seq_len(.N)[-1L]]
#       } else if(Metric  == 'Index') {
#         for(i in c('o','c','h','l')) data[, paste0(i) := get(i) / data.table::first(get(i))]
#       } else if(Metric  == 'Quadratic Variation') {
#         for(i in c('o','c','h','l')) data[, temp_temp := data.table::shift(x = get(i), n = 1L, fill = NA, type = 'lag')][, paste0(i) := (get(i) - temp_temp)^2][, temp_temp := NULL]
#         data <- data[seq_len(.N)[-1L]]
#       }
#     }, error = function(x) NULL)
#
#   } else {
#     if(Debug) print("here 1b")
#     data <- PolyOut
#     if(Debug) print(head(data))
#   }
#
#   return(list(results = data, PolyOut = PolyOut, CompanyName = CompanyName, Symbol = Symbol, Metric = Metric, StartDate = StartDate, EndDate = EndDate, APIKey = APIKey))
# }

# #' @title Plot.Stock
# #'
# #' @description  Create a candlestick plot for stocks. See https://plotly.com/r/figure-labels/
# #'
# #' @family Stock Plots
# #' @author Adrian Antico
# #'
# #' @param Type 'candlestick', 'ohlc'
# #' @param StockDataOutput PolyOut returned from StockData()
# #' @param Width = "1450px"
# #' @param Height = "600px"
# #' @param EchartsTheme = "macarons"
# #' @param ShadowBlur = 5. Chart boxes' shadow blur amount. This attribute should be used along with shadowColor,shadowOffsetX, shadowOffsetY to set shadow to component
# #' @param ShadowColor "black"
# #' @param ShadowOffsetX 0
# #' @param ShadowOffsetY 0
# #' @param TextColor = "white"
# #' @param title.fontSize = 22
# #' @param title.fontWeight = "bold", # norma
# #' @param title.textShadowColor = '#63aeff'
# #' @param title.textShadowBlur = 3
# #' @param title.textShadowOffsetY = 1
# #' @param title.textShadowOffsetX = -1
# #' @param xaxis.fontSize = 14
# #' @param yaxis.fontSize = 14
# #'
# #' @export
# Plot.Stock <- function(StockDataOutput,
#                        Type = 'candlestick',
#                        Metric = "Stock Price",
#                        Width = NULL,
#                        Height = NULL,
#                        EchartsTheme = "macarons",
#                        TextColor = "white",
#                        ShadowBlur = 0,
#                        ShadowColor = "black",
#                        ShadowOffsetX = 0,
#                        ShadowOffsetY = 0,
#                        title.fontSize = 14,
#                        title.fontWeight = "bold",
#                        title.textShadowColor = '#63aeff',
#                        title.textShadowBlur = 3,
#                        title.textShadowOffsetY = 1,
#                        title.textShadowOffsetX = -1,
#                        Color = "green",
#                        Color0 = "red",
#                        BorderColor = "transparent",
#                        BorderColor0 = "transparent",
#                        BorderColorDoji = "transparent",
#                        xaxis.fontSize = 14,
#                        yaxis.fontSize = 14,
#                        Debug = FALSE) {
#
#   # Width = "1450px"
#   # Height = "600px"
#   # EchartsTheme = "macarons"
#   # TextColor = "white"
#   # ShadowBlur = 5
#   # title.fontSize = 22
#   # title.fontWeight = "bold"
#   # title.textShadowColor = '#63aeff'
#   # title.textShadowBlur = 3
#   # title.textShadowOffsetY = 1
#   # title.textShadowOffsetX = -1
#   # Color = "green"
#   # Color0 = "red"
#   # BorderColor = "transparent"
#   # BorderColor0 = "transparent"
#   # BorderColorDoji = "transparent"
#   # xaxis.fontSize = 14
#   #if(missing(StockDataOutput)) stop('StockDataOutput cannot be missing')
#   #if(Type == 'CandlestickPlot') Type <- 'candlestick'
#   # Build base plot depending on GroupVar availability
#   dt <- StockDataOutput$results
#   dt[, Date := as.character(Date)]
#   p1 <- echarts4r::e_charts_(
#     data = dt,
#     x = "Date",
#     dispose = TRUE,
#     darkMode = TRUE,
#     width = Width,
#     height = Height)
#   p1 <- echarts4r::e_candle_(
#     e = p1,
#     high = "h",
#     low = "l",
#     closing = "c",
#     opening = "o",
#     itemStyle = list(
#       #shadowBlur = ShadowBlur,
#       #shadowColor = ShadowColor,
#       #shadowOffsetX = ShadowOffsetX,
#       #shadowOffsetY = ShadowOffsetY,
#       color = Color,
#       color0 = Color0,
#       backgroundColor = "white",
#       borderColor = BorderColor,
#       borderColor0 = BorderColor0,
#       borderColorDoji = BorderColorDoji
#     ),
#     name = StockDataOutput$Symbol)
#
#   # Finalize Plot Build
#   p1 <- echarts4r::e_legend(e = p1, show = FALSE)
#   p1 <- echarts4r::e_theme(e = p1, name = EchartsTheme)
#   p1 <- echarts4r::e_aria(e = p1, enabled = TRUE)
#   p1 <- echarts4r::e_tooltip(e = p1 , trigger = "axis")
#   p1 <- echarts4r::e_toolbox_feature(e = p1, feature = c("saveAsImage","dataZoom"))
#   p1 <- echarts4r::e_show_loading(e = p1, hide_overlay = TRUE, text = "Calculating...", color = "#000", text_color = TextColor, mask_color = "#000")
#
#   p1 <- echarts4r::e_axis_(e = p1, serie = NULL, axis = "x", name = "Date", nameLocation = "middle", nameGap = 45, nameTextStyle = list(color = TextColor, fontStyle = "normal", fontWeight = "bold", fontSize = xaxis.fontSize))
#   p1 <- echarts4r::e_brush(e = p1)
#   p1 <- echarts4r::e_datazoom(e = p1, x_index = c(0,1))
#   p1 <- echarts4r::e_title(
#     p1,
#     text = if(length(StockDataOutput$CompanyName) == 0L) paste0(StockDataOutput$Symbol, ": ", StockDataOutput$StartDate, " to ", StockDataOutput$EndDate) else paste0(StockDataOutput$CompanyName, " - ", StockDataOutput$Symbol, ": ", StockDataOutput$StartDate, " to ", StockDataOutput$EndDate, " :: Measure: ", Metric),
#     textStyle = list(
#       color = TextColor,
#       fontWeight = title.fontWeight,
#       overflow = "truncate", # "none", "truncate", "break",
#       ellipsis = '...',
#       fontSize = title.fontSize,
#       textShadowColor = title.textShadowColor,
#       textShadowBlur = title.textShadowBlur,
#       textShadowOffsetY = title.textShadowOffsetY,
#       textShadowOffsetX = title.textShadowOffsetX))
#   if(Debug) print("Plot.Line no group Echarts 9")
#   return(p1)
# }

# ----

# ----
