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

  if(length(ArgsList$TimeUnit) == 0L) return(NULL)

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

  if(length(ArgsList$TimeUnit) == 0L) return(NULL)

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
    vars <- c(ArgsList[['DateColumnName']], ArgsList[['TargetColumnName']], 'Predictions', Metrics)
  } else {
    x <- paste0(ModelID, '_BT_Raw')
    xx <- paste0(ModelID, '_BT_Rollup')
    vars <- c(ArgsList[['TargetColumnName']], 'Predictions', Metrics)
  }

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
  aggD[, MAPE := NULL]
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
#' @param Algo 'CatBoost', 'XGBoost', 'LightGBM'
#' @param VD ValidationData
#' @param DebugFC From app
#'
#' @export
Shiny.FC.Panel.Train <- function(ArgsList, CodeList, DataList, ModelID, Algo = 'CatBoost', VD = NULL, DebugFC = FALSE) {

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
  ArgsList$Model <- NULL
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
    "ArgsList$Model <- Output$TestModel$Model\n",
    "ArgsList$FactorLevelsList <- Output$TestModel$FactorLevelsList\n",
    "if(!(length(ArgsList$data) > 0 && data.table::is.data.table(ArgsList$data))) {\n  ",
    "if(length(VD) > 0L) {\n    ",
    "ArgsList[['data']] <- unique(data.table::rbindlist(list(dtc, VD), use.names = TRUE, fill = TRUE))\n  ",
    "}\n",
    "}\n"))}, error = function(x) NULL)

  ArgsList$Model <- Output$TestModel$Model
  ArgsList$FactorLevelsList <- Output$TestModel$FactorLevelsList
  if(DebugFC) print("Shiny.FC.Panel.Train 7")

  # ML Data: just like in the ML function shiny.ML.Trainer()
  Output <- Quantico:::Shiny.ML.ModelDataObjects(Output$TestModel, DebugFC, TT = Algo)

  if(DebugFC) print("Shiny.FC.Panel.Train 8")
  if(length(Output$ScoringDataCombined) > 0L) {
    if(DebugFC) print("Shiny.FC.Panel.Train 8.a")
    DataList[[paste0(Algo, 'FC_', ModelID, '_ScoringData')]][['data']] <- Output$ScoringDataCombined
    if(DebugFC) print("Shiny.FC.Panel.Train 8.b")
  }

  if(DebugFC) print("Shiny.FC.Panel.Train 9")
  if(length(Output$VI_Train) > 0L) {
    if(DebugFC) print("Shiny.FC.Panel.Train 9.a")
    DataList[[paste0(Algo, 'FC_', ModelID, '_Test_VI_Data')]][['data']] <- Output$VI_Train
    if(DebugFC) print("Shiny.FC.Panel.Train 9.b")
  }

  if(DebugFC) print("Shiny.FC.Panel.Train 10")
  if(length(Output$VI_Validation) > 0L) {
    if(DebugFC) print("Shiny.FC.Panel.Train 10.a")
    DataList[[paste0(Algo, 'FC_', ModelID, '_Train_VI_Data')]][['data']] <- Output$VI_Validation
    if(DebugFC) print("Shiny.FC.Panel.Train 10.b")
  }

  if(DebugFC) print("Shiny.FC.Panel.Train 11")
  if(length(Output$VI_Test) > 0L) {
    if(DebugFC) print("Shiny.FC.Panel.Train 11.a")
    DataList[[paste0(Algo, 'FC_', ModelID, '_Validation_VI_Data')]][['data']] <- Output$VI_Test
    if(DebugFC) print("Shiny.FC.Panel.Train 11.b")
  }

  if(DebugFC) print("Shiny.FC.Panel.Train 12")
  if(length(Output$II_Train) > 0L) {
    if(DebugFC) print("Shiny.FC.Panel.Train 12.a")
    DataList[[paste0(Algo, 'FC_', ModelID, '_All_II_Data')]][['data']] <- Output$II_Train
    if(DebugFC) print("Shiny.FC.Panel.Train 12.b")
  }

  if(DebugFC) print("Shiny.FC.Panel.Train return")
  return(list(
    DataList = DataList,
    CodeList = CodeList,
    ArgsList = ArgsList,
    ValidationData = VD
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
  if(DebugFC) {
    print("Shiny.FC.Panel.Retrain 2")
    print(length(ArgsList))
  }
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

  ArgsList$Model <- Output$TestModel$Model
  ArgsList$FactorLevelsList <- Output$TestModel$FactorLevelsList
  x <- Output$ModelInformation
  Output <- Quantico:::Shiny.ML.ModelDataObjects(x, Debug, TT = 'catboost')
  DataList[[paste0('CatBoostFC_', ModelID, '_ScoringData')]][['data']] <- Output$ScoringDataCombined

  if(DebugFC) print("Shiny.FC.Panel.Retrain 5")
  DataList[[paste0('CatBoostFC_', ModelID, '_Test_VI_Data')]][['data']] <- Output$VI_Train

  if(DebugFC) print("Shiny.FC.Panel.Retrain 6")
  DataList[[paste0('CatBoostFC_', ModelID, '_Train_VI_Data')]][['data']] <- Output$VI_Validation

  if(DebugFC) print("Shiny.FC.Panel.Retrain 7")
  DataList[[paste0('CatBoostFC_', ModelID, '_Validation_VI_Data')]][['data']] <- Output$VI_Test

  if(DebugFC) print("Shiny.FC.Panel.Retrain 8")
  DataList[[paste0('CatBoostFC_', ModelID, '_All_II_Data')]][['data']] <- Output$II_Train

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
      Output <- tryCatch({AutoQuant::AutoCatBoostCARMA(data = DataList[[NewDataName]][['data']], FC_Periods = ArgsList$FC_Periods, TrainOnFull = TRUE, SaveModel = sm, ArgsList = ArgsList)}, error = function(x) NULL)
    } else if(tolower(Algo) == 'xgboost') {
      Output <- tryCatch({AutoQuant::AutoXGBoostCARMA(data = DataList[[NewDataName]][['data']], FC_Periods = ArgsList$FC_Periods, TrainOnFull = TRUE, SaveModel = sm, ArgsList = ArgsList)}, error = function(x) NULL)
    } else if(tolower(Algo) == 'lightgbm') {
      Output <- tryCatch({AutoQuant::AutoLightGBMCARMA(data = DataList[[NewDataName]][['data']], FC_Periods = ArgsList$FC_Periods, TrainOnFull = TRUE, SaveModel = sm, ArgsList = ArgsList)}, error = function(x) NULL)
    }

    if(length(Output) == 0L) return(NULL)

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

  ArgsList <- list()

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
  if(DebugFC) print(ArgsList)
  if(tolower(Algo) == 'catboost') {
    Output <- do.call(what = AutoQuant::AutoCatBoostCARMA, args = ArgsList)
  } else if(tolower(Algo) == 'xgboost') {
    Output <- do.call(what = AutoQuant::AutoXGBoostCARMA, args = ArgsList)
  } else if(tolower(Algo) == 'lightgbm') {
    Output <- do.call(what = AutoQuant::AutoLightGBMCARMA, args = ArgsList)
  }

  ArgsList$Model <- Output$TestModel$Model

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
    DataList[[paste0(ModelID, dlrname)]][['sample']] <- DataList[[paste0(ModelID, dlrname)]][['data']]
  }

  # Return
  if(DebugFC) {
    print('Shiny.FC.Panel.Backtest 10 Done')
    print(paste0("TYT has length: ", length(ArgsList$TVT)))
  }
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
    "ArgsList <- Output$ArgsList; LoopMetrics <- Output$LoopMetrics\n",
    "}\n\n",
    "# Store LoopMetrics in DataList \n",
    "mcn <- paste0(ModelID, '_FeatureEngineeringTest')\n",
    "DataList[[mcn]][['data']] <- LoopMetrics\n"))}, error = function(x) CodeList)

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
#' @param Algo 'CatBoost', 'XGBoost', 'LightGBM'
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
  ModelID <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0(Algo, 'CARMA_ModelID')]]}, error=function(x) NULL), Type='character', Default="FC001")

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
  # if(RunMode == 'Backtest' && data.table::is.data.table(ValidationData) && length(ArgsList[['Model']]) > 0L) {
  #
  #   ValidationDataCheck <- TRUE
  #
  #   if(DebugFC) print('Shiny.FC.CARMA 8.a')
  #
  #   # Backtest CatBoost for Forecasting Purposes
  #   if(DebugFC) {print('Backtest 1st version'); print(ArgsList[['Model']])}
  #   Output <- Quantico::Shiny.FC.Panel.Backtest(ArgsList, CodeList, DataList, ModelID, VD = ValidationData, DebugFC = DebugFC, Algo = Algo)
  #   DataList <- Output$DataList; CodeList <- Output$CodeList; ArgsList <- Output$ArgsList
  #   for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
  #   for(i in seq_len(TabCount)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
  #
  #   # Return results
  #   if(DebugFC) {print('Return from Shiny.FC.CARMA')}
  #   return(list(
  #     DataList = DataList,
  #     CodeList = CodeList,
  #     ArgsList = ArgsList,
  #     RunMode = RunMode,
  #     ModelID = ModelID
  #   ))
  # } else {
  #   ValidationDataCheck <- FALSE
  # }
  ValidationDataCheck <- FALSE

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
    "ArgsList[['MA_Periods']] <- ", Quantico:::ExpandText(ArgsList[['MA_Periods']] ), "\n",
    "ArgsList[['SD_Periods']] <- ", Quantico:::ExpandText(ArgsList[['SD_Periods']]), "\n",
    "ArgsList[['Kurt_Periods']] <- ", Quantico:::ExpandText(ArgsList[['Kurt_Periods']]), "\n",
    "ArgsList[['Skew_Periods']] <- ", Quantico:::ExpandText(ArgsList[['Skew_Periods']]), "\n",
    "ArgsList[['Quantile_Periods']] <- ", Quantico:::ExpandText(ArgsList[['Quantile_Periods']]), "\n",

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
        ModelID = ModelID
      ))
    }

    # *************************************
    # Backtest
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

      # Column Updates
      DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']][, `AvgError On` := NULL]
      DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']][, `AvgError Off` := NULL]
      DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']][, `RMSE On` := round(sqrt(`RMSE On`), 2)]
      DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']][, `RMSE Off` := round(sqrt(`RMSE Off`), 2)]
      DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']][, `MAE On` := round(`MAE On`, 2)]
      DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']][, `MAE Off` := round(`MAE Off`, 2)]
      DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']][, `MAPE On` := NULL]
      DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']][, `MAPE Off` := NULL]
      DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']][, `SMAPE On` := paste0(round(`SMAPE On` * 100, 2), "%")]
      DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']][, `SMAPE Off` := paste0(round(`SMAPE Off` * 100, 2), "%")]

      if(DebugFC) print('Shiny.FC.Panel.Backest.FeatureEval post step 2')

      DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']][, TestOutcome := data.table::fifelse(`SMAPE On` < `SMAPE Off`, 'Success','Failure')]
      DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']][, RunNumber := NULL]
      DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']][, Methods := NULL]
      data.table::setcolorder(
        DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']],
        c(1L,
          ncol(DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']]),
          2L:(ncol(DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']])-1L)))

      if(DebugFC) print('Shiny.FC.Panel.Backest.FeatureEval post step 4')

      if(DebugFC) print(DataList[[paste0(ModelID, "_FeatureEngineeringTest")]][['data']])

      if(DebugFC) print('Shiny.FC.Panel.Backest.FeatureEval post step 6')
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
      "if(!exists('ArgsList')) ArgsList <- list()\n",
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
  if(length(ArgsList) > 1L) {
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
      "ArgsList[['TimeUnit']] <- ", Quantico:::CEP(ArgsList[['TimeUnit']]), "\n"))
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
        "DateName = ", Quantico:::CEP(ArgsList[['DateColumnName']]),",\n    ",
        "Lags = ", Quantico:::CEPP(ArgsList[['MaxLags']]),",\n    ",
        "SeasonalLags = ", 0,",\n    ",
        "MovingAverages = ", Quantico:::CEPP(ArgsList[['MaxMovingAverages']]),",\n    ",
        "SeasonalMovingAverages = ", 0,",\n    ",
        "TimeUnit = ", Quantico:::CEP(ArgsList[['TimeUnit']]),",\n    ",
        "FCPeriods = ", 1,",\n    ",
        "HoldOutPeriods = ", Quantico:::CEPP(ArgsList[['FCPeriods']]),",\n    ",
        "TSClean = TRUE,\n    ",
        "ModelFreq = TRUE,\n    ",
        "FinalBuild = FALSE)\n",
        "}, error = function(x) {\n  ",
        "print('AutoQuant:::TimeSeriesDataPrepare failed')\n  ",
        "NULL\n",
        "})\n"))
      }, error = function(x) {
        if(DebugFC) print("TimeSeriesDataPrepare code collect failure")
        if(DebugFC) print(Quantico:::CEPP(ArgsList[['TargetColumnName']]))
        if(DebugFC) print(Quantico:::CEPP(ArgsList[['DateColumnName']]))
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
        "DateName = ", Quantico:::CEP(ArgsList[['DateColumnName']]),",\n    ",
        "Lags = ", Quantico:::CEPP(ArgsList[['MaxLags']]),",\n    ",
        "SeasonalLags = ", Quantico:::CEPP(ArgsList[['MaxSeasonalLags']]),",\n    ",
        "MovingAverages = ", Quantico:::CEPP(ArgsList[['MaxMovingAverages']]),",\n    ",
        "SeasonalMovingAverages = ", Quantico:::CEPP(ArgsList[['MaxSeasonalMovingAverages']]),",\n    ",
        "TimeUnit = ", Quantico:::CEP(ArgsList[['TimeUnit']]),",\n    ",
        "FCPeriods = ", 1,",\n    ",
        "HoldOutPeriods = ", Quantico:::CEPP(ArgsList[['FCPeriods']]),",\n    ",
        "TSClean = TRUE,\n    ",
        "ModelFreq = TRUE,\n    ",
        "FinalBuild = FALSE)\n",
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
        "DateName = ", Quantico:::CEP(ArgsList[['DateColumnName']]),",\n    ",
        "Lags = 0,\n    ",
        "SeasonalLags = 0,\n    ",
        "MovingAverages = 0,\n    ",
        "SeasonalMovingAverages = 0,\n    ",
        "TimeUnit = ", Quantico:::CEP(ArgsList[['TimeUnit']]),",\n    ",
        "FCPeriods = ", 1,",\n    ",
        "HoldOutPeriods = ", Quantico:::CEPP(ArgsList[['FCPeriods']]),",\n    ",
        "TSClean = TRUE,\n    ",
        "ModelFreq = TRUE,\n    ",
        "FinalBuild = FALSE)\n",
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
        "DateName = ", Quantico:::CEP(ArgsList[['DateColumnName']]),",\n    ",
        "Lags = ", Quantico:::CEPP(ArgsList[['MaxLags']]),",\n    ",
        "SeasonalLags = 0,\n    ",
        "MovingAverages = ", Quantico:::CEPP(ArgsList[['MaxMovingAverages']]),",\n    ",
        "SeasonalMovingAverages = 0,\n    ",
        "TimeUnit = ", Quantico:::CEP(ArgsList[['TimeUnit']]),",\n    ",
        "FCPeriods = ", 1,",\n    ",
        "HoldOutPeriods = ", Quantico:::CEPP(ArgsList[['FCPeriods']]),",\n    ",
        "TSClean = TRUE,\n    ",
        "ModelFreq = TRUE,\n    ",
        "FinalBuild = FALSE)\n",
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
        "DateName = ", Quantico:::CEP(ArgsList[['DateColumnName']]),",\n    ",
        "Lags = ", Quantico:::CEPP(ArgsList[['MaxLags']]),",\n    ",
        "SeasonalLags = ", Quantico:::CEPP(ArgsList[['MaxSeasonalLags']]),",\n    ",
        "MovingAverages = 0,\n    ",
        "SeasonalMovingAverages = 0,\n    ",
        "TimeUnit = ", Quantico:::CEP(ArgsList[['TimeUnit']]),",\n    ",
        "FCPeriods = ", 1,",\n    ",
        "HoldOutPeriods = ", Quantico:::CEPP(ArgsList[['FCPeriods']]),",\n    ",
        "TSClean = TRUE,\n    ",
        "ModelFreq = TRUE,\n    ",
        "FinalBuild = FALSE)\n",
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
      print('here 10.58')
      print(tryCatch({ExperimentGrid}, error = function(x) NULL))
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
      "'Blended_MAPE'\n    ",
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
      "'MAPE Blended'\n    ",
      ")\n  ",
      ")\n",
      "}\n\n",
      "# Update Collection Lists\n",
      "ArgsList[[", Quantico:::CEP(paste0(Algo, "_", ModelID, "_ExperimentGrid")), "]] <- ExperimentGrid\n",
      "DataList[[", Quantico:::CEP(paste0(Algo, "_", ModelID, "_ExperimentGrid")), "]][['data']] <- ExperimentGrid\n\n",
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
      "'MAPE Blended'\n  ",
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
      "'Blended_MAPE'\n  ",
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
        print(paste0("TargetName == ", ArgsList[['TargetColumnName']]))
        print(paste0("DateName == ", ArgsList[['DateColumnName']]))
        print(paste0("Lags == ", as.integer(ExperimentGrid[1L, Lags])))
        print(paste0("MovingAverages == ", as.integer(ExperimentGrid[1L, MovingAverages])))
        print(paste0("TimeUnit == ", ArgsList[['TimeUnit']]))
        print(paste0("FCPeriods == ", FCPeriods))
        print(paste0("HoldOutPeriods == ", 0))
        print(paste0("TSClean == ", TRUE))
        print(paste0("ModelFreq == ", TRUE))
        print(paste0("FinalBuild == ", TRUE))
        print(paste0("ArgsList[['EvaluationMetric']] == ", ArgsList[['EvaluationMetric']]))
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
      "'Blended_MAPE'\n      ",
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
      "'MAPE Blended'\n      ",
      "), skip_absent = TRUE\n  ",
      ")\n",
      "}\n"))
    }, error = function(x) CodeList)

    # Update Output
    if(length(ForecastOutput) > 0L) {
      data.table::setnames(ForecastOutput, c("Target","Date"), c(ArgsList[['TargetColumnName']], ArgsList[['DateColumnName']]), skip_absent = TRUE)
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
        "data.table::setnames(ForecastOutput, c('Target','Date'), c(ArgsList[['TargetColumnName']], ArgsList[['DateColumnName']]), skip_absent = TRUE)\n  ",
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

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# FC Reporting                                                                               ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Shiny.FC.Panel.ReportOutput
#'
#' @description Shiny FC Panel Report
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
Shiny.FC.Panel.ReportOutput <- function(input,
                                        output,
                                        DataList,
                                        CodeList,
                                        Page,
                                        Debug = FALSE,
                                        MOL = NULL,
                                        ModelID = NULL,
                                        RunMode = NULL,
                                        Theme = "dark",
                                        FontColor = NULL,
                                        PlotWidth = "1450px",
                                        PlotHeight = "850px") {

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Organize Objects                                                          ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(Debug) print("FC Reports 1")

  # Checks
  if(length(ModelID) == 0L) return(NULL)
  if(!exists("DataList")) return(NULL)
  if(!exists("CodeList")) return(NULL)

  # temp_model_rdata$ArgsList$TargetColumnName
  TargetColumnName <- MOL[[ModelID]][["TargetColumnName"]]

  # temp_model_rdata$ArgsList$
  PredictionColumnName <- "Predict"

  # temp_model_rdata$ArgsList$FeatureColNames
  FeatureColumnNames <- MOL[[ModelID]][["FeatureColNames"]]

  # DateCol
  DateColumnName <- MOL[[ModelID]]$DateColumnName

  # Algo
  temp_algo <- class(MOL[[ModelID]]$Model)[1L]
  if(temp_algo == "catboost.Model") {
    Algo <- "CatBoost"
  } else if(temp_algo == "xgb.Booster") {
    Algo <- "XGBoost"
  } else if(temp_algo == "lgb.Booster") {
    Algo <- "LightGBM"
  }

  # Group Var
  GroupVariableInclude <- "GroupVar"

  # Data sets
  if(Debug) print("FC Reports 2")

  if(tolower(Algo) == "catboost") {
    TestData <- DataList[[paste0("CatBoostFC_", ModelID, "_ScoringData")]]$data
  } else if(tolower(Algo) == "xgboost") {
    TestData <- DataList[[paste0("XGBoostFC_", ModelID, "_ScoringData")]]$data
  } else if(tolower(Algo) == "lightgbm") {
    TestData <- DataList[[paste0("LightGBMFC_", ModelID, "_ScoringData")]]$data
  }

  OutputList <- list()

  if(Debug) print("FC Reports 3")

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Forecast Outputs                                                          ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # FC Data Output
  if(Debug) print("Forecasting Outputs")
  FCData <- tryCatch({DataList[[paste0("FC_", ModelID)]][['data']]}, error = function(x) NULL)
  if(length(FCData) > 0L) {
    if(Debug) print("Panel Forecast Data")
    OutputList[["Panel Forecast Data"]] <- reactable::reactable(
      data = FCData,
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

    # Backtest
    if(Debug) print("Forecast LinePlot 1")
    minD <- FCData[is.na(get(TargetColumnName)), min(get(DateColumnName), na.rm = TRUE)]
    if(Debug) {
      print("Forecast LinePlot 2")
      print(FCData)
    }
    p7 <- AutoPlots::Plot.Line(
      dt = FCData,
      AggMethod = "mean",
      PreAgg = FALSE,
      XVar = DateColumnName,
      YVar = c(TargetColumnName, "Predictions"),
      DualYVar = NULL,
      GroupVar = NULL,
      YVarTrans = "Identity",
      DualYVarTrans = "Identity",
      XVarTrans = "Identity",
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      Height = PlotHeight,
      Width = PlotWidth,
      Title = "Line Plot",
      ShowLabels = FALSE,
      Title.YAxis = paste0(TargetColumnName, " | Predict"),
      Title.XAxis = DateColumnName,
      EchartsTheme = Theme,
      X_Scroll = FALSE,
      Y_Scroll = FALSE,
      TimeLine = TRUE,
      Alpha = 0.5,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      TextColor = "white",
      title.fontSize = 22,
      title.fontWeight = "bold",
      title.textShadowColor = "#63aeff",
      title.textShadowBlur = 3,
      title.textShadowOffsetY = 1,
      title.textShadowOffsetX = -1,
      xaxis.fontSize = 14,
      yaxis.fontSize = 14,
      xaxis.rotate = 0,
      yaxis.rotate = 0,
      ContainLabel = TRUE,
      Debug = FALSE)
    if(Debug) print("Forecast LinePlot 3")
    OutputList[["Forecast LinePlot"]] <- echarts4r::e_mark_line(e = p7, data = list(xAxis = minD), title = "")
  }

  # ----

  # ----


  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Cross Eval Outputs                                                        ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # FE_Test
  if(Debug) print("Cross Eval Outputs")
  CE_Rollup <- tryCatch({DataList[[paste0(ModelID, "_CE_Rollup")]]$data}, error = function(x) NULL)
  if(length(CE_Rollup) > 0L) {
    OutputList[["Cross Eval Test Metrics"]] <- reactable::reactable(
      data = CE_Rollup,
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

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # FE Test Outputs                                                           ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # FE_Test
  if(Debug) print("Feature Tuning Outputs")
  FE_Test <- tryCatch({DataList[[paste0(ModelID, "_FeatureEngineeringTest")]]$data}, error = function(x) NULL)
  if(length(FE_Test) > 0L) {
    OutputList[["Feature Tuning Metrics"]] <- reactable::reactable(
      data = FE_Test,
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

  # FE_Test[, ]

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Backtest Outputs                                                          ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # BT_Rollup
  if(Debug) print("Backtesting Outputs")
  BT_Rollup <- tryCatch({DataList[[paste0(ModelID, "_BT_Rollup")]]$data}, error = function(x) NULL)
  if(length(BT_Rollup) > 0L) {
    OutputList[["Back Test Metrics"]] <- reactable::reactable(
      data = BT_Rollup,
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

  # BT_Raw
  BT_Raw <- tryCatch({DataList[[paste0(ModelID, "_BT_Raw")]]$data}, error = function(x) NULL)
  if(length(BT_Raw) > 0L) {

    # Backtest
    minD <- BT_Raw[DataSet == "Evaluation", min(get(DateColumnName))]
    p1 <- AutoPlots::Plot.Line(
      dt = BT_Raw,
      AggMethod = "mean",
      PreAgg = FALSE,
      XVar = DateColumnName,
      YVar = c(TargetColumnName, "Predictions"),
      DualYVar = NULL,
      GroupVar = NULL,
      YVarTrans = "Identity",
      DualYVarTrans = "Identity",
      XVarTrans = "Identity",
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      Height = PlotHeight,
      Width = PlotWidth,
      Title = "Line Plot",
      ShowLabels = FALSE,
      Title.YAxis = paste0(TargetColumnName, " | Predict"),
      Title.XAxis = DateColumnName,
      EchartsTheme = Theme,
      X_Scroll = FALSE,
      Y_Scroll = FALSE,
      TimeLine = TRUE,
      Alpha = 0.5,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      TextColor = "white",
      title.fontSize = 22,
      title.fontWeight = "bold",
      title.textShadowColor = "#63aeff",
      title.textShadowBlur = 3,
      title.textShadowOffsetY = 1,
      title.textShadowOffsetX = -1,
      xaxis.fontSize = 14,
      yaxis.fontSize = 14,
      xaxis.rotate = 0,
      yaxis.rotate = 0,
      ContainLabel = TRUE,
      Debug = FALSE)
    OutputList[["Backtest LinePlot"]] <- echarts4r::e_mark_line(e = p1, data = list(xAxis = minD), title = "")

    # Backtest AvgError
    p2 <- AutoPlots::Plot.Box(
      dt = BT_Raw,
      SampleSize = 100000L,
      XVar = DateColumnName,
      YVar = "AvgError",
      GroupVar = NULL,
      YVarTrans = "Identity",
      XVarTrans = "Identity",
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      Height = PlotHeight,
      Width = PlotWidth,
      Title = "Box Plot: Avg Error",
      ShowLabels = FALSE,
      Title.YAxis = paste0(TargetColumnName, " - Predicted"),
      Title.XAxis = DateColumnName,
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
      xaxis.rotate = 0,
      yaxis.rotate = 0,
      ContainLabel = TRUE,
      Debug = FALSE)
    OutputList[["Backtest BoxPlot"]] <- echarts4r::e_mark_line(e = p2, data = list(xAxis = minD), title = "")

    # Backtest MAE
    p3 <- AutoPlots::Plot.Area(
      dt = BT_Raw,
      AggMethod = "mean",
      PreAgg = FALSE,
      XVar = DateColumnName,
      YVar = "MAE",
      DualYVar = NULL,
      GroupVar = NULL,
      YVarTrans = "Identity",
      DualYVarTrans = "Identity",
      XVarTrans = "Identity",
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      Height = PlotHeight,
      Width = PlotWidth,
      Title = "Line Plot",
      ShowLabels = FALSE,
      Title.YAxis = "Absolute Error",
      Title.XAxis = DateColumnName,
      EchartsTheme = Theme,
      X_Scroll = FALSE,
      Y_Scroll = FALSE,
      TimeLine = TRUE,
      Alpha = 0.5,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      TextColor = "white",
      title.fontSize = 22,
      title.fontWeight = "bold",
      title.textShadowColor = "#63aeff",
      title.textShadowBlur = 3,
      title.textShadowOffsetY = 1,
      title.textShadowOffsetX = -1,
      xaxis.fontSize = 14,
      yaxis.fontSize = 14,
      xaxis.rotate = 0,
      yaxis.rotate = 0,
      ContainLabel = TRUE,
      Debug = FALSE)
    OutputList[["Backtest MAE LinePlot"]] <- echarts4r::e_mark_line(e = p3, data = list(xAxis = minD), title = "")

    # Backtest RMSE
    temp <- BT_Raw[, list(RMSE = sqrt(mean(RMSE))), by = DateColumnName]
    p4 <- AutoPlots::Plot.Area(
      dt = temp,
      AggMethod = "mean",
      PreAgg = TRUE,
      XVar = DateColumnName,
      YVar = "RMSE",
      DualYVar = NULL,
      GroupVar = NULL,
      YVarTrans = "Identity",
      DualYVarTrans = "Identity",
      XVarTrans = "Identity",
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      Height = PlotHeight,
      Width = PlotWidth,
      Title = "Line Plot",
      ShowLabels = FALSE,
      Title.YAxis = "MSE",
      Title.XAxis = DateColumnName,
      EchartsTheme = Theme,
      X_Scroll = FALSE,
      Y_Scroll = FALSE,
      TimeLine = TRUE,
      Alpha = 0.5,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      TextColor = "white",
      title.fontSize = 22,
      title.fontWeight = "bold",
      title.textShadowColor = "#63aeff",
      title.textShadowBlur = 3,
      title.textShadowOffsetY = 1,
      title.textShadowOffsetX = -1,
      xaxis.fontSize = 14,
      yaxis.fontSize = 14,
      xaxis.rotate = 0,
      yaxis.rotate = 0,
      ContainLabel = TRUE,
      Debug = FALSE)
    OutputList[["Backtest MSE LinePlot"]] <- echarts4r::e_mark_line(e = p4, data = list(xAxis = minD), title = "")

    # # Backtest MAPE
    # temp1 <- BT_Raw[, list(MAPE = mean(MAPE)), by = DateColumnName]
    # p5 <- AutoPlots::Plot.Area(
    #   dt = temp1,
    #   AggMethod = "mean",
    #   PreAgg = TRUE,
    #   XVar = DateColumnName,
    #   YVar = "MAPE",
    #   DualYVar = NULL,
    #   GroupVar = NULL,
    #   YVarTrans = "Identity",
    #   DualYVarTrans = "Identity",
    #   XVarTrans = "Identity",
    #   FacetRows = 1,
    #   FacetCols = 1,
    #   FacetLevels = NULL,
    #   Height = PlotHeight,
    #   Width = PlotWidth,
    #   Title = "Line Plot",
    #   ShowLabels = FALSE,
    #   Title.YAxis = "MAPE",
    #   Title.XAxis = DateColumnName,
    #   EchartsTheme = Theme,
    #   X_Scroll = FALSE,
    #   Y_Scroll = FALSE,
    #   TimeLine = TRUE,
    #   Alpha = 0.5,
    #   Smooth = TRUE,
    #   ShowSymbol = FALSE,
    #   TextColor = "white",
    #   title.fontSize = 22,
    #   title.fontWeight = "bold",
    #   title.textShadowColor = "#63aeff",
    #   title.textShadowBlur = 3,
    #   title.textShadowOffsetY = 1,
    #   title.textShadowOffsetX = -1,
    #   xaxis.fontSize = 14,
    #   yaxis.fontSize = 14,
    #   xaxis.rotate = 0,
    #   yaxis.rotate = 0,
    #   ContainLabel = TRUE,
    #   Debug = FALSE)
    # OutputList[["Backtest MAPE LinePlot"]] <- echarts4r::e_mark_line(e = p5, data = list(xAxis = minD), title = "")

    # Backtest SMAPE
    temp2 <- BT_Raw[, list(SMAPE = mean(SMAPE)), by = DateColumnName]
    p6 <- AutoPlots::Plot.Area(
      dt = temp2,
      AggMethod = "mean",
      PreAgg = TRUE,
      XVar = DateColumnName,
      YVar = "SMAPE",
      DualYVar = NULL,
      GroupVar = NULL,
      YVarTrans = "Identity",
      DualYVarTrans = "Identity",
      XVarTrans = "Identity",
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      Height = PlotHeight,
      Width = PlotWidth,
      Title = "Line Plot",
      ShowLabels = FALSE,
      Title.YAxis = "SMAPE",
      Title.XAxis = DateColumnName,
      EchartsTheme = Theme,
      X_Scroll = FALSE,
      Y_Scroll = FALSE,
      TimeLine = TRUE,
      Alpha = 0.5,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      TextColor = "white",
      title.fontSize = 22,
      title.fontWeight = "bold",
      title.textShadowColor = "#63aeff",
      title.textShadowBlur = 3,
      title.textShadowOffsetY = 1,
      title.textShadowOffsetX = -1,
      xaxis.fontSize = 14,
      yaxis.fontSize = 14,
      xaxis.rotate = 0,
      yaxis.rotate = 0,
      ContainLabel = TRUE,
      Debug = FALSE)
    OutputList[["Backtest SMAPE LinePlot"]] <- echarts4r::e_mark_line(e = p6, data = list(xAxis = minD), title = "")

  }

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Train Outputs                                                             ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  if(tolower(Algo) == "catboost") {
    ScoringData <- tryCatch({DataList[[paste0("CatBoostFC_", ModelID, "_ScoringData")]]$data}, error = function(x) NULL)
  } else if(tolower(Algo) == "xgboost") {
    ScoringData <- tryCatch({DataList[[paste0("XGBoostFC_", ModelID, "_ScoringData")]]$data}, error = function(x) NULL)
  } else if(tolower(Algo) == "lightgbm") {
    ScoringData <- tryCatch({DataList[[paste0("LightGBMFC_", ModelID, "_ScoringData")]]$data}, error = function(x) NULL)
  }

  if(length(ScoringData) > 0L) {

    # Metrics
    ScoringData[, AvgError := get(TargetColumnName) - Predict]
    AvgError <- round(ScoringData[, mean(AvgError, na.rm = TRUE)], 4)
    ScoringData[, MAE := abs(AvgError)]
    MAE <- round(ScoringData[, mean(MAE, na.rm = TRUE)], 4)
    ScoringData[, MSE := (get(TargetColumnName) - Predict)^2]
    RMSE <- round(sqrt(ScoringData[, mean(MSE, na.rm = TRUE)]), 4)
    # ScoringData[, MAPE := get(TargetColumnName) / Predict - 1]
    # MAPE <- paste0(round(ScoringData[Predict != 0, mean(MAPE, na.rm = TRUE)], 4) * 100, "%")
    ScoringData[, SMAPE := 2 * MAE / (abs(get(TargetColumnName)) + abs(Predict))]
    SMAPE <- paste0(round(ScoringData[, mean(SMAPE, na.rm = TRUE)], 4) * 100, "%")

    MetricsTable <- data.table::data.table(AvgError, MAE, RMSE, SMAPE)

    # Evaluation Metrics ----
    OutputList[["ML Evaluation Metrics"]] <- reactable::reactable(
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

    if(Debug) print("FC Reports 4")

    # Variable Importance
    if(tolower(Algo) == "catboost") {
      Test_Importance <- DataList[[paste0("CatBoostFC_", ModelID, "_Test_VI_Data")]]$data
    } else if(tolower(Algo) == "xgboost") {
      Test_Importance <- DataList[[paste0("XGBoostFC_", ModelID, "_Test_VI_Data")]]$data
    } else if(tolower(Algo) == "lightgbm") {
      Test_Importance <- DataList[[paste0("LightGBMFC_", ModelID, "_Test_VI_Data")]]$data
    }

    # Test Importance ----
    OutputList[["ML Test Importance"]] <- reactable::reactable(
      data = Test_Importance,
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

    if(Debug) print("FC Reports 6")


    Validation_Importance <- tryCatch({DataList[[paste0("CatBoostFC_", ModelID, "_Validation_VI_Data")]]$data}, error = function(x) NULL)

    # Validation Importance ----
    if(length(Validation_Importance) > 0) {
      OutputList[["ML Validation Importance"]] <- reactable::reactable(
        data = Validation_Importance,
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

    Train_Importance <- tryCatch({DataList[[paste0("CatBoostFC_", ModelID, "_Train_VI_Data")]]$data}, error = function(x) NULL)

    # Train Importance ----
    if(length(Train_Importance) > 0) {
      OutputList[["ML Train Importance"]] <- reactable::reactable(
        data = Train_Importance,
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

    # Interaction Importances
    All_Interaction <- tryCatch({DataList[[paste0("CatBoostFC_", ModelID, "_All_II_Data")]]$data}, error = function(x) NULL)

    # Interaction Importance ----
    if(length(All_Interaction) > 0) {
      OutputList[["ML Interaction Importance"]] <- reactable::reactable(
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

    # Evaluation Plots ----
    if(Debug) print("FC Reports 7.1")
    OutputList[["ScoringData Residual Histogram"]] <- AutoPlots::Plot.Residuals.Histogram(
      dt = ScoringData,
      AggMethod = "mean",
      SampleSize = 30000,
      XVar = 'Predict',
      YVar = TargetColumnName,
      GroupVar = NULL,
      YVarTrans = "Identity",
      XVarTrans = "Identity",
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      NumberBins = 20,
      Height = PlotHeight,
      Width = PlotWidth,
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

    if(Debug) print("FC Reports 7.3")

    OutputList[["ScoringData Calibration Plot"]] <- AutoPlots::Plot.Calibration.Line(
      dt = ScoringData,
      AggMethod = "mean",
      XVar = 'Predict',
      YVar = TargetColumnName,
      GroupVar = NULL,
      YVarTrans = "Identity",
      XVarTrans = "Identity",
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      NumberBins = 20,
      Height = PlotHeight,
      Width = PlotWidth,
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

    if(Debug) print("FC Reports 7.5")

    OutputList[["ScoringData Calibration Box Plot"]] <- AutoPlots::Plot.Calibration.Box(
      dt = ScoringData,
      SampleSize = 30000L,
      AggMethod = "mean",
      XVar = 'Predict',
      YVar = TargetColumnName,
      GroupVar = NULL,
      YVarTrans = "Identity",
      XVarTrans = "Identity",
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      NumberBins = 20,
      Height = PlotHeight,
      Width = PlotWidth,
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

    # Add Plots
    if(!is.null(ScoringData) && !is.null(FeatureColumnNames)) {
      for(g in FeatureColumnNames) {
        if(is.numeric(ScoringData[[g]])) {
          OutputList[[paste0('ScoringData Partial Dependence Line Plot: ', g)]] <- AutoPlots::Plot.PartialDependence.Line(
            dt = ScoringData,
            XVar = g,
            YVar = TargetColumnName,
            ZVar = 'Predict',
            YVarTrans = "Identity",
            XVarTrans = "Identity",
            ZVarTrans = "Identity",
            FacetRows = 1,
            FacetCols = 1,
            FacetLevels = NULL,
            GroupVar = NULL,
            NumberBins = 20,
            AggMethod = "mean",
            Height = PlotHeight,
            Width = PlotWidth,
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
    if(!is.null(ScoringData) && !is.null(FeatureColumnNames)) {
      for(g in FeatureColumnNames) {
        if(is.numeric(ScoringData[[g]])) {
          if(Debug) print(paste0("g = ", g))
          OutputList[[paste0('ScoringData Partial Dependence Box Plot: ', g)]] <- AutoPlots::Plot.PartialDependence.Box(
            dt = ScoringData,
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
            Height = PlotHeight,
            Width = PlotWidth,
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
    if(!is.null(ScoringData) && !is.null(FeatureColumnNames)) {
      if("GroupVar" %in% names(ScoringData)) {
        data.table::setnames(ScoringData, "GroupVar", "GroupVariable")
        FeatureColumnNames <- FeatureColumnNames[!FeatureColumnNames %in% "GroupVar"]
        FeatureColumnNames[[length(FeatureColumnNames) + 1]] <- "GroupVariable"
      } else if("GroupVariable" %in% names(ScoringData)) {
        FeatureColumnNames <- FeatureColumnNames[!FeatureColumnNames %in% "GroupVar"]
        FeatureColumnNames[[length(FeatureColumnNames) + 1]] <- "GroupVariable"
      }

      if(Debug) {
        print(FeatureColumnNames)
        print(names(ScoringData))
        print(ScoringData)
        data.table::fwrite(ScoringData, file = "C:/Users/Bizon/Documents/GitHub/rappwd/Scoring.csv")
      }

      for(g in FeatureColumnNames) {
        if(g %in% names(ScoringData) && !is.numeric(ScoringData[[g]])) {
          if(Debug) {
            print(TargetColumnName)
            print(g)
          }
          OutputList[[paste0('ScoringData Partial Dependence BarPlot: ', g)]] <- AutoPlots::Plot.PartialDependence.HeatMap(
            dt = ScoringData,
            XVar = g,
            YVar = TargetColumnName,
            ZVar = 'Predict',
            YVarTrans = "Identity",
            XVarTrans = "Identity",
            ZVarTrans = "Identity",
            FacetRows = 1,
            FacetCols = 1,
            FacetLevels = NULL,
            GroupVar = NULL,
            NumberBins = 20,
            AggMethod = "mean",
            Height = PlotHeight,
            Width = PlotWidth,
            Title = "Partial Dependence BarPlot",
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

  # ----

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

#' @title Shiny.FC.SS.ReportOutput
#'
#' @description Shiny FC SS Report
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
Shiny.FC.SS.ReportOutput <- function(input,
                                     output,
                                     DataList,
                                     CodeList,
                                     Page,
                                     Debug = FALSE,
                                     MOL = NULL,
                                     Algo = NULL,
                                     ModelID = NULL,
                                     RunMode = NULL,
                                     Theme = "dark",
                                     FontColor = NULL,
                                     PlotWidth = "1450px",
                                     PlotHeight = "850px") {

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Organize Objects                                                          ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(Debug) print("FC Reports 1")

  # Checks
  if(length(ModelID) == 0L) return(NULL)
  if(!exists("DataList")) return(NULL)
  if(!exists("CodeList")) return(NULL)

  EG <- paste0(ModelID, "_ExperimentGrid")
  FCD <- paste0(ModelID, "_Forecast")
  if(EG %in% names(DataList)) {
    GridTune_proc <- TRUE
  } else {
    GridTune_proc <- FALSE
  }
  if(FCD %in% names(DataList)) {
    Forecast_proc <- TRUE
  } else {
    Forecast_proc <- TRUE
  }

  # temp_model_rdata$ArgsList$TargetColumnName
  TargetColumnName <- MOL[[ModelID]][["TargetColumnName"]]

  # temp_model_rdata$ArgsList$
  PredictionColumnName <- "Forecast"

  # DateCol
  DateColumnName <- MOL[[ModelID]]$DateColumnName

  # Collection List
  OutputList <- list()

  if(Debug) print("FC Reports 2")

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Grid Tuning Output                                                        ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(GridTune_proc) {
    OutputList[["Experiment Grid"]] <- reactable::reactable(
      data = DataList[[EG]]$data,
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

  # ----

  # ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # Forecasting Output                                                        ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(Forecast_proc) {
    OutputList[["Forecast Data"]] <- reactable::reactable(
      data = DataList[[FCD]]$data,
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

    # Plot with Prediction Intervals
    OutputList[["Forecast Plot"]] <- AutoPlots::Plot.Line(
      dt = DataList[[FCD]]$data,
      AggMethod = "mean",
      PreAgg = TRUE,
      XVar = DateColumnName,
      YVar = c(TargetColumnName,"Forecast","Low95","Low80","High80","High95"),
      DualYVar = NULL,
      GroupVar = NULL,
      YVarTrans = "Identity",
      DualYVarTrans = "Identity",
      XVarTrans = "Identity",
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      Height = PlotHeight,
      Width = PlotWidth,
      Title = "Line Plot",
      ShowLabels = FALSE,
      Title.YAxis = TargetColumnName,
      Title.XAxis = DateColumnName,
      EchartsTheme = Theme,
      X_Scroll = FALSE,
      Y_Scroll = FALSE,
      TimeLine = TRUE,
      Alpha = 0.5,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      TextColor = "white",
      title.fontSize = 22,
      title.fontWeight = "bold",
      title.textShadowColor = "#63aeff",
      title.textShadowBlur = 3,
      title.textShadowOffsetY = 1,
      title.textShadowOffsetX = -1,
      xaxis.fontSize = 14,
      yaxis.fontSize = 14,
      xaxis.rotate = 0,
      yaxis.rotate = 0,
      ContainLabel = TRUE,
      Debug = FALSE)
  }

  # ----

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

  # ----
}

# ----

# ----
