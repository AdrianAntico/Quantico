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
#' @param Debug DebugFC from app a
#'
#' @return a list of columns names by data type
#'
#' @export
Shiny.EDA.ReportOutput <- function(input,
                                   output,
                                   DataList,
                                   CodeList,
                                   Page,
                                   Debug = FALSE,
                                   Theme = "dark",
                                   FontColor = NULL) {

  if(Debug) print("Shiny.EDA.ReportOutput 1")

  if(!exists("DataList")) return(NULL)
  if(!exists("CodeList")) return(NULL)
  OutputList <- list()

  # Comment
  # Args Mgt
  if(Debug) print("Shiny.EDA.ReportOutput 2")
  data <- DataList[[DataMuse:::ReturnParam(xx = tryCatch({input[[paste0("EDAData", Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)]][['data']]
  if(!data.table::is.data.table(data)) return(NULL)
  UnivariateVars <- DataMuse:::ReturnParam(xx = tryCatch({input[[paste0("EDAUnivariateVars", Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
  if(length(UnivariateVars) > 50L) UnivariateVars <- UnivariateVars[seq_len(50L)]
  CorrVars <- DataMuse:::ReturnParam(xx = tryCatch({input[[paste0("EDACorrVars", Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
  if(length(CorrVars) > 50L) CorrVars <- CorrVars[seq_len(50L)]
  TrendVars <- DataMuse:::ReturnParam(xx = tryCatch({input[[paste0("EDATrendVars", Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
  if(length(TrendVars) > 50L) TrendVars <- TrendVars[seq_len(50L)]
  TrendDateVar <- DataMuse:::ReturnParam(xx = tryCatch({input[[paste0("EDADateVar", Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
  if(length(TrendDateVar) > 1L) {
    for(zzz in TrendDateVar) {
      if(class(data[[zzz]])[1L] %in% c("Date", "posix", "IDate", "IDateTime")) {
        TrendDateVar <- zzz
        break
      }
    }
  } else if(length(TrendDateVar) > 0L) {
    if(!class(data[[TrendDateVar]])[1L] %in% c("Date", "posix", "IDate", "IDateTime")) {
      x <- data[1L, get(TrendDateVar)]
      x1 <- lubridate::guess_formats(x, orders = c('mdY', 'BdY', 'Bdy', 'bdY', 'bdy', 'mdy', 'dby', 'Ymd', 'Ydm', 'dmy'))
      data[, eval(TrendDateVar) := as.Date(get(TrendDateVar), tryFormats = x1)]
      if(!class(data[[TrendDateVar]])[1L] %in% c("Date", "posix", "IDate", "IDateTime")) {
        TrendDateVar <- NULL
      }
    }
  }
  TrendGroupVar <- DataMuse:::ReturnParam(xx = tryCatch({input[[paste0("EDAGroupVar", Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
  PlotHeighteda <- DataMuse:::ReturnParam(xx = tryCatch({input[[paste0("PlotHeighteda", Page)]]}, error = function(x) NULL), Type = "numeric", Default = 950)
  PlotWidtheda <- DataMuse:::ReturnParam(xx = tryCatch({input[[paste0("PlotWidtheda", Page)]]}, error = function(x) NULL), Type = "numeric", Default = 1450)
  PlotHeighteda <- paste0(PlotHeighteda, "px")
  PlotWidtheda <- paste0(PlotWidtheda, "px")

  # Describe data (str replicate?)
  if(Debug) print("Shiny.EDA.ReportOutput 3")
  DescribeData <- data.table::data.table(`Variable Name` = names(data))
  x <- c(); for(i in names(data)) x <- c(x, class(data[[i]])[1L])
  DescribeData[, `Variable Type` := x]
  x <- c(); for(i in names(data)) x <- c(x, length(which(is.null(data[[i]]))))
  DescribeData[, `NULL Counts` := x]
  x <- c(); for(i in names(data)) x <- c(x, length(which(is.na(data[[i]]))))
  DescribeData[, `NA Counts` := x]
  DescribeData[, `First 5 Values` := "a"]
  for(i in seq_along(names(data))) {
    g <- paste0(data[[names(data)[i]]][1L:5L], collapse = ", ")
    data.table::set(DescribeData, i = i, j = "First 5 Values", value = g)
  }

  # Describe Data
  if(Debug) print("Shiny.EDA.ReportOutput 4")
  OutputList[["Describe Data"]] <- reactable::reactable(
    data = DescribeData,
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

  # Create a sampled dataset to speed up computations
  if(data[,.N] > 100000L) {
    data1 <- data[order(runif(.N))][seq_len(100000L)]
  }

  # Numeric
  if(Debug) print("Shiny.EDA.ReportOutput 5")

  # Full Data
  x <- c(); for(i in names(data)) if(class(data[[i]])[1L] %in% c("numeric","integer")) x <- c(x, i)
  UnivariateStats <- data.table::data.table(Variables = x)
  x <- c(); for(i in names(data)) if(class(data[[i]])[1L] %in% c("numeric","integer")) x <- c(x, data[, min(get(i), na.rm = TRUE)])
  UnivariateStats[, Min := round(x,2)]
  x <- c(); for(i in names(data)) if(class(data[[i]])[1L] %in% c("numeric","integer")) x <- c(x, data[, max(get(i), na.rm = TRUE)])
  UnivariateStats[, Max := round(x,2)]

  # Sampled data
  x <- c(); for(i in names(data1)) if(class(data1[[i]])[1L] %in% c("numeric","integer")) x <- c(x, data1[, mean(get(i), na.rm = TRUE)])
  UnivariateStats[, Mean := round(x, 2)]
  x <- c(); for(i in names(data1)) if(class(data1[[i]])[1L] %in% c("numeric","integer")) x <- c(x, data1[, median(get(i), na.rm = TRUE)])
  UnivariateStats[, Median := round(x,2)]
  x <- c(); for(i in names(data1)) if(class(data1[[i]])[1L] %in% c("numeric","integer")) x <- c(x, data1[, sd(get(i), na.rm = TRUE)])
  UnivariateStats[, `Standard Deviation` := round(x,2)]
  x <- c(); for(i in names(data1)) if(class(data1[[i]])[1L] %in% c("numeric","integer")) x <- c(x, data1[, e1071::skewness(get(i), na.rm = TRUE)])
  UnivariateStats[, Skewness := round(x,2)]
  x <- c(); for(i in names(data1)) if(class(data1[[i]])[1L] %in% c("numeric","integer")) x <- c(x, data1[, e1071::kurtosis(get(i), na.rm = TRUE)])
  UnivariateStats[, Kurtosis := round(x,2)]
  x <- c(); for(i in names(data1)) if(class(data1[[i]])[1L] %in% c("numeric","integer")) x <- c(x, data1[, sd(get(i), na.rm = TRUE) / mean(get(i), na.rm = TRUE)])
  UnivariateStats[, `Coef of Variation` := round(x,2)]
  x <- c(); for(i in names(data1)) if(class(data1[[i]])[1L] %in% c("numeric","integer")) x <- c(x, data1[, quantile(get(i), probs = 0.01, na.rm = TRUE)])
  UnivariateStats[, q01 := round(x,2)]
  x <- c(); for(i in names(data1)) if(class(data1[[i]])[1L] %in% c("numeric","integer")) x <- c(x, data1[, quantile(get(i), probs = 0.05, na.rm = TRUE)])
  UnivariateStats[, q05 := round(x,2)]
  x <- c(); for(i in names(data1)) if(class(data1[[i]])[1L] %in% c("numeric","integer")) x <- c(x, data1[, quantile(get(i), probs = 0.10, na.rm = TRUE)])
  UnivariateStats[, q10 := round(x,2)]
  x <- c(); for(i in names(data1)) if(class(data1[[i]])[1L] %in% c("numeric","integer")) x <- c(x, data1[, quantile(get(i), probs = 0.25, na.rm = TRUE)])
  UnivariateStats[, q25 := round(x,2)]
  x <- c(); for(i in names(data1)) if(class(data1[[i]])[1L] %in% c("numeric","integer")) x <- c(x, data1[, quantile(get(i), probs = 0.75, na.rm = TRUE)])
  UnivariateStats[, q75 := round(x,2)]
  x <- c(); for(i in names(data1)) if(class(data1[[i]])[1L] %in% c("numeric","integer")) x <- c(x, data1[, quantile(get(i), probs = 0.90, na.rm = TRUE)])
  UnivariateStats[, q90 := round(x,2)]
  x <- c(); for(i in names(data1)) if(class(data1[[i]])[1L] %in% c("numeric","integer")) x <- c(x, data1[, quantile(get(i), probs = 0.95, na.rm = TRUE)])
  UnivariateStats[, q95 := round(x,2)]
  x <- c(); for(i in names(data1)) if(class(data1[[i]])[1L] %in% c("numeric","integer")) x <- c(x, data1[, quantile(get(i), probs = 0.99, na.rm = TRUE)])
  UnivariateStats[, q99 := round(x,2)]

  # Describe Data
  if(Debug) print("Shiny.EDA.ReportOutput 6")
  OutputList[["Univariate Stats"]] <- tryCatch({reactable::reactable(
    data = UnivariateStats,
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
  )}, error = function(x) NULL)

  # Box Plots
  if(Debug) print("Shiny.EDA.ReportOutput 7")
  for(i in UnivariateVars) {
    if(class(data1[[i]])[1L] %in% c("numeric","integer")) {
      OutputList[[paste0("Univariate Plots: ", i)]] <- tryCatch({AutoPlots::Plot.Box(
        dt = data1,
        SampleSize = 100000L,
        XVar = TrendGroupVar,
        YVar = i,
        GroupVar = NULL,
        YVarTrans = "Identity",
        XVarTrans = "Identity",
        FacetRows = 1,
        FacetCols = 1,
        FacetLevels = NULL,
        Height = PlotHeighteda,
        Width = PlotWidtheda,
        Title = "Box Plot",
        ShowLabels = FALSE,
        Title.YAxis = i,
        Title.XAxis = NULL,
        EchartsTheme = Theme,
        TimeLine = TimeLine,
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
        Debug = FALSE)}, error = function(x) NULL)
    }
  }

  # Bar Plots
  if(Debug) print("Shiny.EDA.ReportOutput 8")
  for(i in UnivariateVars) {# i = UnivariateVars[1]
    if(class(data1[[i]])[1L] %in% c("factor","character")) {
      data2 <- data1[, list(Counts = .N), by = c(i)]
      OutputList[[paste0("Univariate Plots: ", i)]] <- tryCatch({AutoPlots::Plot.Bar(
        dt = data2,
        PreAgg = TRUE,
        XVar = i,
        YVar = "Counts",
        GroupVar = NULL,
        LabelValues = NULL,
        YVarTrans = "Identity",
        XVarTrans = "Identity",
        FacetRows = 1,
        FacetCols = 1,
        FacetLevels = NULL,
        AggMethod = "count",
        Height = NULL,
        Width = NULL,
        Title = "Bar Plot",
        ShowLabels = FALSE,
        Title.YAxis = NULL,
        Title.XAxis = NULL,
        EchartsTheme = Theme,
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
        xaxis.rotate = 0,
        yaxis.rotate = 0,
        ContainLabel = TRUE,
        Debug = FALSE)}, error = function(x) NULL)
    }
  }

  # Correlation Stats / Plots
  if(Debug) print("Shiny.EDA.ReportOutput 9")
  x <- c(); for(i in CorrVars) if(class(data1[[i]])[1L] %in% c("numeric","integer")) x <- c(x, i)
  if(length(x) > 0L) {
    OutputList[[paste0("Correlogram: ", i)]] <- tryCatch({AutoPlots::Plot.CorrMatrix(
      dt = data1,
      CorrVars = x,
      CorrVarTrans = "Identity",
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      Method = "spearman",
      PreAgg = FALSE,
      Height = NULL,
      Width = NULL,
      Title = "Correlation Matrix",
      ShowLabels = FALSE,
      Title.YAxis = NULL,
      Title.XAxis = NULL,
      EchartsTheme = Theme,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TextColor = "white",
      title.fontSize = 22,
      title.fontWeight = "bold",
      title.textShadowColor = "#63aeff",
      title.textShadowBlur = 3,
      title.textShadowOffsetY = 1,
      title.textShadowOffsetX = -1,
      yaxis.fontSize = 14,
      xaxis.fontSize = 14,
      Debug = FALSE)
      }, error = function(x) NULL)

    # dt = data1
    # CorrVars = x
    # CorrVarTrans = "Identity"
    # FacetRows = 1
    # FacetCols = 1
    # FacetLevels = NULL
    # Method = "spearman"
    # PreAgg = FALSE
    # Height = NULL
    # Width = NULL
    # Title = "Correlation Matrix"
    # ShowLabels = FALSE
    # Title.YAxis = NULL
    # Title.XAxis = NULL
    # EchartsTheme = Theme
    # X_Scroll = TRUE
    # Y_Scroll = TRUE
    # TextColor = "white"
    # title.fontSize = 22
    # title.fontWeight = "bold"
    # title.textShadowColor = "#63aeff"
    # title.textShadowBlur = 3
    # title.textShadowOffsetY = 1
    # title.textShadowOffsetX = -1
    # yaxis.fontSize = 14
    # xaxis.fontSize = 14
    # Debug = FALSE

  }

  # Trend Stats / Plots
  if(Debug) print("Shiny.EDA.ReportOutput 10")
  if(length(TrendDateVar) > 0L) {
    x <- c(); for(i in TrendVars) if(class(data[[i]])[1L] %in% c("numeric","integer")) x <- c(x, i)
    if(length(x) > 0L) {
      for(i in x) {
        OutputList[[paste0("Trend: ", i)]] <- tryCatch({AutoPlots::Plot.Line(
          dt = data,
          AggMethod = "mean",
          PreAgg = FALSE,
          XVar = TrendDateVar,
          YVar = i,
          DualYVar = NULL,
          GroupVar = TrendGroupVar,
          YVarTrans = "Identity",
          DualYVarTrans = "Identity",
          XVarTrans = "Identity",
          FacetRows = 1,
          FacetCols = 1,
          FacetLevels = NULL,
          Height = NULL,
          Width = NULL,
          Title = "Line Plot",
          ShowLabels = FALSE,
          Title.YAxis = NULL,
          Title.XAxis = NULL,
          EchartsTheme = Theme,
          X_Scroll = FALSE,
          Y_Scroll = FALSE,
          TimeLine = TRUE,
          Area = FALSE,
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
          DarkMode = FALSE,
          Debug = FALSE)}, error = function(x) NULL)
      }
    }
  }

  ############################################################### ----
  # Return                                                        ----
  ############################################################### ----
  if(Debug) print("Shiny.EDA.ReportOutput 11")
  returnList <- list()
  returnList[["OutputList"]] <- OutputList
  returnList[["DataList"]] <- DataList
  returnList[["CodeList"]] <- CodeList
  return(returnList)
}
