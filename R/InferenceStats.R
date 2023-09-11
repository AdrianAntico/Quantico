# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Automated Functions                                                        ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title All.Normality.Tests
#'
#' @family Inference
#'
#' @export
Normality.Analysis <- function(dt = NULL,
                               YVars = NULL,
                               EchartsTheme = "macarons",
                               TextColor = "black",
                               PlotHeight = "300px",
                               PlotWidth = "600px",
                               SampleSize.ADT = NULL,
                               Samples.ADT = 1,
                               SampleSize.CVMT = NULL,
                               Samples.CVMT = 1,
                               SampleSize.KST = NULL,
                               Samples.KST = 1,
                               SampleSize.ST = NULL,
                               Samples.ST = 1,
                               SampleSize.JBT = NULL,
                               Samples.JBT = 1,
                               SampleSize.AT = NULL,
                               Samples.AT = 1) {

  # library(DataMuse)
  # dt <- data.table::fread(file.choose())
  # YVars <- c("Daily Liters", "Daily Margin", "Daily Revenue", "Daily Units")
  # SampleSize.ADT = NULL
  # Samples.ADT = 1
  # SampleSize.CVMT = NULL
  # Samples.CVMT = 1
  # SampleSize.KST = NULL
  # Samples.KST = 1
  # SampleSize.ST = NULL
  # Samples.ST = 1
  # SampleSize.JBT = NULL
  # Samples.JBT = 1
  # SampleSize.AT = NULL
  # Samples.AT = 1
  # EchartsTheme = "macarons"
  # TextColor = "black"
  # PlotHeight = "300px"
  # PlotWidth = "600px"

  # KS Test throws warnings for duplicate values
  options(warn = -1)

  # Convert to data.table
  if(!data.table::is.data.table(dt)) tryCatch({data.table::setDT(dt)}, error = function(x) {
    dt <- data.table::as.data.table(dt)
  })

  # YVars to Test
  YVars2Test <- c()
  for(i in YVars) {
    if(class(dt[[i]])[1L] %in% c("numeric","integer")) {
      YVars2Test <- c(YVars2Test, i)
    }
  }

  # Results table
  gg <- data.table::CJ(
    Variable = YVars2Test,
    Test = c("Anderson.Darling.Test",
             "Cramer.Von.Mises.Test",
             "Kolmogorov.Smirnov.Test",
             "Shapiro.Test",
             "Jarque.Bera.Test",
             "Agostino.Test"),
    P_Value = c(-1.0)
  )

  # Loop through testing and results gathering
  OutputList <- list()
  for(val in YVars2Test) {# val = "Daily Margin"

    print(val)

    # Create Vals for tests
    Vals <- dt[[val]] # unique(dt[[val]])

    # Run tests
    x1 <- Anderson.Darling.Test(Vals, SampleSize = SampleSize.ADT, Samples = Samples.ADT)
    gg <- gg[Variable == eval(val) & Test == "Anderson.Darling.Test", P_Value := mean(x1$P_Value, na.rm = TRUE)]

    x1 <- Cramer.Von.Mises.Test(Vals, SampleSize = SampleSize.CVMT, Samples = Samples.CVMT)
    gg <- gg[Variable == eval(val) & Test == "Cramer.Von.Mises.Test", P_Value := mean(x1$P_Value, na.rm = TRUE)]

    x1 <- Kolmogorov.Smirnov.Test(Vals, SampleSize = SampleSize.KST, Samples = Samples.KST)
    gg <- gg[Variable == eval(val) & Test == "Kolmogorov.Smirnov.Test", P_Value := mean(x1$P_Value_2s, na.rm = TRUE)]

    x1 <- Shapiro.Test(Vals, SampleSize = SampleSize.ST, Samples = Samples.ST)
    gg <- gg[Variable == eval(val) & Test == "Shapiro.Test", P_Value := mean(x1$P_Value, na.rm = TRUE)]

    x1 <- Jarque.Bera.Test(Vals, SampleSize = SampleSize.JBT, Samples = Samples.JBT)
    gg <- gg[Variable == eval(val) & Test == "Jarque.Bera.Test", P_Value := mean(x1$P_Value, na.rm = TRUE)]

    x1 <- Agostino.Test(Vals, SampleSize = SampleSize.AT, Samples = Samples.AT)
    gg <- gg[Variable == eval(val) & Test == "Agostino.Test", P_Value := mean(x1$P_Value_2s, na.rm = TRUE)]

    # Radar plot of P_Values
    print(paste0("Radar Plot", " ", val))
    OutputList[[paste0("Radar_", val)]] <- AutoPlots::Plot.Radar(
      dt = gg[Variable == eval(val)],
      PreAgg = TRUE,
      YVar = c("P_Value"),
      GroupVar = "Test",
      EchartsTheme = EchartsTheme,
      TextColor = TextColor,
      Height = PlotHeight,
      Width = PlotWidth)

    # Normal Probability Plot of Vals
    print(paste0("Probability Plot", " ", val))
    OutputList[[paste0("Probability_", val)]] <- AutoPlots::Plot.ProbabilityPlot(
      dt = dt,
      SampleSize = 2500,
      YVar = val,
      EchartsTheme = EchartsTheme,
      TextColor = TextColor,
      Height = PlotHeight,
      Width = PlotWidth)
  }

  # FontColor <- list()
  # FontColor$flv = "black"
  print("Reactable Metrics")
  OutputList[["Metrics"]] <- reactable::reactable(
    data = gg,
    compact = TRUE,
    defaultPageSize = 6,
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
      color = TextColor,
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

  # Reorder elements and return
  print("Return 1")
  v <- names(OutputList)
  v <- v[c(length(v), 1:(length(v)-1))]
  OutputList <- OutputList[v]
  print("Return 2")
  return(OutputList)
}

#' @title Correlation.Analysis
#'
#' @family Inference
#'
#' @param dt Numeric vector of values to test. Must have positive standard deviation and must be of length greater than or equal to 8
#' @param CorrVars character
#' @param DateVar character
#' @param SampleSize sub sampling of data
#' @param EchartsTheme "macarons"
#' @param TextColor "black"
#' @param PlotHeight "300px"
#' @param PlotWidth "600px"
#' @param P_Adjust Frequentist corrections, which include: default "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "somers" or "none"
#' @param Bayesian logical
#' @param Bayesian_Prior Choose from default "medium", "medium.narrow", "medium", "wide", and "ultrawide"
#' @param MultiLevel logical. TRUE using random effects, FALSE uses fixed effects
#' @param Include_Factors logical
#' @param Partial logical
#' @param Partial_Bayesian logical
#'
#' @export
Correlation.Analysis <- function(dt = NULL,
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
                                 Partial_Bayesian = FALSE) {

  options(warn = -1)

  # dt = data.table::fread(file.choose())
  # CorrVars = c("Category","Beverage Flavor","Daily Liters","Daily Margin","Daily Revenue","Daily Units","ClassTarget")
  # DateVar <- "Date"
  # SampleSize = 10000
  # Samples = 1
  # Method = NULL
  # P_Adjust = "holm"
  # Bayesian = FALSE
  # Bayesian_Prior = "medium"
  # MultiLevel = FALSE
  # Include_Factors = TRUE
  # Partial = FALSE
  # Partial_Bayesian = FALSE
  # EchartsTheme = "macarons"
  # TextColor = "black"
  # PlotHeight = "300px"
  # PlotWidth = "600px"

  if(dt[,.N] > SampleSize) {
    dt1 <- dt[order(runif(.N))][seq_len(SampleSize)][, .SD, .SDcols = c(CorrVars, DateVar)]
  } else {
    dt1 <- data.table::copy(dt)
  }

  # Names modification: because of the parse() I can't have spaces in the colnames
  old <- c()
  new <- c()
  for(i in seq_along(CorrVars)) {# i = 3
    if(grepl(pattern = " ", x = CorrVars[i])) {
      old <- c(old, CorrVars[i])
      new <- c(new, gsub(pattern = " ", replacement = ".", x = CorrVars[i]))
      CorrVars[i] <- gsub(pattern = " ", replacement = ".", x = CorrVars[i])
    }
  }
  if(length(new) > 0L) {
    data.table::setnames(dt1, old = old, new = new)
  }

  # Correlation Metrics
  Output <- list()
  dt2 <- dt1[, .SD, .SDcols = c(CorrVars)]
  corrMetrics <- tryCatch({data.table::as.data.table(
    correlation::correlation(
      data = dt2,
      method = "pearson",
      p_adjust = P_Adjust,
      ci = 0.95,
      bayesian = Bayesian,
      bayesian_prior = Bayesian_Prior,
      include_factors = Include_Factors,
      partial = Partial,
      partial_bayesian = Partial_Bayesian,
      bayesian_ci_method = "hdi",
      bayesian_test = c("pd", "rope", "bf"),
      multilevel = MultiLevel,
      redundant = TRUE,
      ranktransform = FALSE,
      winsorize = FALSE,
      verbose = TRUE,
      standardize_names = getOption("easystats.standardize_names", FALSE)
    )
  )}, error = function(x) NULL)

  if(!Bayesian) {
    if(length(corrMetrics) > 0L) {
      data.table::setnames(
        corrMetrics,
        old = c("Parameter1","Parameter2","r","CI","CI_low","CI_high","t","df_error","p","Method","n_Obs"),
        new = c("Variable1","Variable2","Correl","CI","CI Low","CI High","T-Stat", "df_error","P_Value","Method","N"),
        skip_absent = TRUE)
      for(var in c("Correl","CI Low", "CI High", "T-Stat", "P_Value")) {
        data.table::set(corrMetrics, j = var, value = round(corrMetrics[[var]], digits = 4))
      }
    }
  } else {
    corrMetrics[, BF := NULL]
    data.table::setnames(
      corrMetrics,
      old = c("Parameter1","Parameter2","rho","CI","CI_low","CI_high","ROPE_Percentage","Prior_Distribution","Prior_Location","Prior_Scale","Method","n_Obs"),
      new = c("Variable1","Variable2","Correl","CI","CI Low","CI High","ROPE Value","Prior","Prior Location","Prior Scale","Method","N"),
      skip_absent = TRUE)
    for(var in c("Correl","CI Low","CI High","pd","ROPE Value")) {
      data.table::set(corrMetrics, j = var, value = round(corrMetrics[[var]], digits = 4))
    }
  }

  # Reactable Table
  if(length(corrMetrics) > 0L) {
    Output[["CorrelationMetrics"]] <- reactable::reactable(
      data = corrMetrics,
      compact = TRUE,
      defaultPageSize = 20,
      wrap = FALSE,
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
        color = TextColor,
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

  # Numeric only CorrVars
  CorrVars2 <- c()
  for(xxx in CorrVars) {# xxx = "Daily.Units"
    if(class(dt1[[xxx]])[1L] %in% c("numeric","integer")) {
      CorrVars2 <- c(CorrVars2, xxx)
    }
  }

  # Correlation Matrix
  if(length(CorrVars2) > 1L) {
    Output[["CorrelogramPlot"]] <- tryCatch({AutoPlots::Plot.CorrMatrix(
      dt = dt1,
      CorrVars = CorrVars2,
      CorrVarTrans = "Identity",
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      Method = "spearman",
      PreAgg = FALSE,
      MaxNAPercent = 0.05,
      Height = PlotHeight,
      Width = PlotWidth,
      Title = "Correlation Matrix",
      ShowLabels = FALSE,
      Title.YAxis = NULL,
      Title.XAxis = NULL,
      EchartsTheme = EchartsTheme,
      X_Scroll = FALSE,
      Y_Scroll = FALSE,
      TextColor = "white",
      title.fontSize = 22,
      title.fontWeight = "bold",
      title.textShadowColor = "#63aeff",
      title.textShadowBlur = 3,
      title.textShadowOffsetY = 1,
      title.textShadowOffsetX = -1,
      yaxis.fontSize = 14,
      xaxis.fontSize = 14,
      Debug = FALSE
    )}, error = function(x) NULL)
  }

  # Parallel Plots
  if(length(CorrVars) > 1L) {
    Output[["ParallelPlot"]] <- tryCatch({AutoPlots::Plot.Parallel(
      dt = dt1,
      SampleSize = 5000L,
      CorrVars = CorrVars,
      FacetRows = 1,
      FacetCols = 1,
      FacetLevels = NULL,
      PreAgg = FALSE,
      Height = PlotHeight,
      Width = PlotWidth,
      Title = "Parallel Plot",
      ShowLabels = FALSE,
      Title.YAxis = NULL,
      Title.XAxis = NULL,
      EchartsTheme = EchartsTheme,
      X_Scroll = FALSE,
      Y_Scroll = FALSE,
      TextColor = "white",
      title.fontSize = 22,
      title.fontWeight = "bold",
      title.textShadowColor = "#63aeff",
      title.textShadowBlur = 3,
      title.textShadowOffsetY = 1,
      title.textShadowOffsetX = -1,
      yaxis.fontSize = 14,
      xaxis.fontSize = 14,
      Debug = FALSE
    )}, error = function(x) NULL)
  }

  # Trend Correlation
  if(length(DateVar) > 0L && length(CorrVars2) > 1L) {

    # Standardize variables so they scale together
    dt2 <- Rodeo::Standardize(
      data = dt1,
      ColNames = CorrVars2,
      GroupVars = NULL,
      Center = TRUE,
      Scale = TRUE,
      ScoreTable = FALSE)

    # Line Plot
    Output[["LinePlot"]] <- tryCatch({AutoPlots::Plot.Line(
      dt = dt2,
      AggMethod = "sum",
      PreAgg = FALSE,
      XVar = DateVar,
      YVar = paste0(CorrVars2, "_Standardize"),
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
      Title = "Correlation Trend",
      ShowLabels = FALSE,
      Title.YAxis = "Standardized Variables",
      Title.XAxis = DateVar,
      EchartsTheme = EchartsTheme,
      X_Scroll = TRUE,
      Y_Scroll = TRUE,
      TimeLine = FALSE,
      Area = FALSE,
      Alpha = 0.5,
      Smooth = TRUE,
      ShowSymbol = FALSE,
      TextColor = TextColor,
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

  # Return
  return(Output)
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Normality Tests                                                            ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

#' @title Anderson.Darling.Test
#'
#' @param Vals Numeric vector of values to test. Must have positive standard deviation and must be of length greater than or equal to 8
#' @param SampleSize sub sampling of data
#' @param Samples number of iterations to run
#'
#' @family Inference
#'
#' @references https://en.m.wikipedia.org/wiki/Anderson%E2%80%93Darling_test, https://real-statistics.com/non-parametric-tests/goodness-of-fit-tests/anderson-darling-test/
#'
#' @examples
#' \dontrun{
#' Vals <- qnorm(p = runif(100000))
#' Anderson.Darling.Test(Vals)
#' }
#'
#' @export
Anderson.Darling.Test <- function(Vals, SampleSize = NULL, Samples = 1) {

  # Sample Size
  if(length(SampleSize) == 0L) {
    SampleSize <- length(Vals)
    Samples <- 1
  } else {
    SampleSize <- min(SampleSize, length(Vals))
    if(SampleSize == length(Vals)) Samples <- 1
  }

  # Collection Table
  gg <- data.table::data.table(
    A_Statistic = rep(-1.0, Samples),
    P_Value = rep(-1.0, Samples)
  )

  # Ensure no missing values
  Vals <- sort(x = Vals[complete.cases(Vals)], decreasing = FALSE)
  n <- length(Vals)
  if(n < 8) return(NULL)
  sigma <- sd(Vals)
  if(sigma == 0) return(NULL)

  for(i in seq_len(Samples)) {

    # Sample
    if(length(Vals) > SampleSize) {
      samp <- sample(x = Vals, size = SampleSize)
    } else {
      samp <- Vals
    }

    # Compute log(F(X)) and log(1-F(X))
    pnorm_vals <- (samp - mean(samp)) / sigma
    logF_x <- pnorm(pnorm_vals, log.p = TRUE)
    logS_x <- pnorm(pnorm_vals, log.p = TRUE, lower.tail = FALSE)

    # Compute Anderson Darling Statistic
    A <- -n - 1/n * sum((2 * seq(1:n) - 1) * (logF_x + rev(logS_x)))
    A2 <- (1 + 0.75/n + 2.25/n^2) * A

    # Compute p-value
    if(A2 <= 0.2) {
      p_value <- 1 - exp(-13.436 + 101.14 * A2 - 223.73 * A2^2)
    } else if(A2 <= 0.34) {
      p_value <- 1 - exp(-8.318 + 42.796 * A2 - 59.938 * A2^2)
    } else if(A2 < 0.6) {
      p_value <- exp(1.2937 - 5.709 * A2 - 0.0186 * A2^2)
    } else {
      p_value <- 3.7e-24
    }

    # Collect
    data.table::set(gg, i = i, j = "A_Statistic", value = A)
    data.table::set(gg, i = i, j = "P_Value", value = p_value)
  }

  # Return
  return(gg)
}

#' @title Cramer.Von.Mises.Test
#'
#' @param Vals Numeric vector of values to test. Must have positive standard deviation and must be of length greater than or equal to 8
#' @param SampleSize sub sampling of data
#' @param Samples number of iterations to run
#'
#' @family Inference
#'
#' @examples
#' \dontrun{
#' Vals <- qnorm(p = runif(100000))
#' Cramer.Von.Mises.Test(Vals)
#' }
#'
#' @export
Cramer.Von.Mises.Test <- function(Vals, SampleSize = NULL, Samples = 1) {

  # Sample Size
  if(length(SampleSize) == 0L) {
    SampleSize <- length(Vals)
    Samples <- 1
  } else {
    SampleSize <- min(SampleSize, length(Vals))
    if(SampleSize == length(Vals)) Samples <- 1
  }

  # Minimum 8 samples required
  n <- length(Vals)
  if(n < 8) return(NULL)

  # Collection Table
  gg <- data.table::data.table(
    A_Statistic = rep(-1.0, Samples),
    P_Value = rep(-1.0, Samples)
  )

  # Ensure no missing values
  Vals <- sort(x = Vals[complete.cases(Vals)], decreasing = FALSE)
  sigma <- sd(Vals)
  meanS <- mean(Vals)
  if(sigma == 0) return(NULL)

  for(i in seq_len(Samples)) {

    # Sample
    if(n > SampleSize) {
      samp <- sample(x = Vals, size = SampleSize)
    } else {
      samp <- Vals
    }

    # Compute Cramer-von Mises
    p <- pnorm((samp - meanS) / sigma)
    A <- (1 / (12 * n) + sum((p - (2 * seq(1:n) - 1)/(2 * n)) ^ 2))
    A2 <- (1 + 0.5/n) * A

    # Compute p-value
    if(A2 < 0.0275) {
      p_value <- 1 - exp(-13.953 + 775.5 * A2 - 12542.61 * A2 ^ 2)
    } else if(A2 < 0.051) {
      p_value <- 1 - exp(-5.903 + 179.546 * A2 - 1515.29 * A2 ^ 2)
    } else if(A2 < 0.092) {
      p_value <- exp(0.886 - 31.62 * A2 + 10.897 * A2 ^ 2)
    } else if(A2 < 1.1) {
      p_value <- exp(1.111 - 34.242 * A2 + 12.832 * A2 ^ 2)
    } else {
      p_value <- 7.37e-10
    }

    # Collect
    data.table::set(gg, i = i, j = "A_Statistic", value = A)
    data.table::set(gg, i = i, j = "P_Value", value = p_value)
  }

  # Return
  return(gg)
}

#' @title Kolmogorov.Smirnov.Test
#'
#' @family Inference
#'
#' @param Vals a vector of values to test
#' @param SampleSize sub sampling of data
#' @param Samples number of iterations to run
#'
#' @examples
#' \dontrun{
#' output <- Kolmogorov.Smirnov.Test(runif(10000))
#' }
#'
#' @export
Kolmogorov.Smirnov.Test <- function(Vals, SampleSize = NULL, Samples = 1) {

  # Setup
  Vals <- Vals[complete.cases(Vals)]
  SampleSize <- min(SampleSize, length(Vals))

  # Collection Table
  gg <- data.table::data.table(
    Alternative_2s = rep("na", Samples),
    Statistic_2s = rep(-1.0, Samples),
    P_Value_2s = rep(-1.0, Samples),
    Alternative_greater = rep("na", Samples),
    Statistic_greater = rep(-1.0, Samples),
    P_Value_greater = rep(-1.0, Samples),
    Alternative_less = rep("na", Samples),
    Statistic_less = rep(-1.0, Samples),
    P_Value_less = rep(-1.0, Samples)
  )

  for(i in seq_len(Samples)) {

    # Sample
    if(length(Vals) > SampleSize) {
      samp <- sample(x = Vals, size = SampleSize)
    } else {
      samp <- Vals
    }

    # Two Sided
    KSTest1 <- ks.test(x = samp, y = "pnorm", alternative = "two.sided")
    data.table::set(gg, i = i, j = "Alternative_2s", value = KSTest1$alternative)
    data.table::set(gg, i = i, j = "Statistic_2s", value = KSTest1$statistic)
    data.table::set(gg, i = i, j = "P_Value_2s", value = KSTest1$p.value)

    KSTest2 <- ks.test(x = samp, y = "pnorm", alternative = "greater")
    data.table::set(gg, i = i, j = "Alternative_greater", value = KSTest2$alternative)
    data.table::set(gg, i = i, j = "Statistic_greater", value = KSTest2$statistic)
    data.table::set(gg, i = i, j = "P_Value_greater", value = KSTest2$p.value)

    KSTest3 <- ks.test(x = samp, y = "pnorm", alternative = "less")
    data.table::set(gg, i = i, j = "Alternative_less", value = KSTest3$alternative)
    data.table::set(gg, i = i, j = "Statistic_less", value = KSTest3$statistic)
    data.table::set(gg, i = i, j = "P_Value_less", value = KSTest3$p.value)

  }
  return(gg)
}

#' @title Shapiro.Test
#'
#' @family Inference
#'
#' @param Vals a vector of values to test
#' @param SampleSize sub sampling of data
#' @param Samples number of iterations to run
#'
#' @examples
#' \dontrun{
#' output <- Shapiro.Test(Vals = runif(1000), SampleSize = 100, Samples = 500)
#' }
#'
#' @export
Shapiro.Test <- function(Vals, SampleSize = NULL, Samples = 1) {

  # Setup
  Vals <- Vals[complete.cases(Vals)]
  SampleSize <- min(SampleSize, length(Vals), 5000)

  # Collection Table
  gg <- data.table::data.table(
    Statistic = rep(-1.0, Samples),
    P_Value = rep(-1.0, Samples)
  )

  # Bootstrap
  for(i in seq_len(Samples)) {

    # Sample
    if(length(Vals) > SampleSize) {
      samp <- sample(x = Vals, size = SampleSize)
    } else {
      samp <- Vals
    }

    # Test
    STest <- shapiro.test(x = samp)

    # Collect
    data.table::set(gg, i = i, j = "Statistic", value = STest$statistic)
    data.table::set(gg, i = i, j = "P_Value", value = STest$p.value)
  }

  return(gg)
}

#' @title Jarque.Bera.Test
#'
#' @family Inference
#'
#' @param Vals a vector of values to test
#' @param SampleSize sub sampling of data
#' @param Samples number of iterations to run
#'
#' @examples
#' \dontrun{
#' output <- Jarque.Bera.Test(Vals = runif(100000), SampleSize = 1000, Samples = 500)
#' }
#'
#' @export
Jarque.Bera.Test <- function(Vals, SampleSize = NULL, Samples = 1) {

  # Setup
  Vals <- Vals[complete.cases(Vals)]
  SampleSize <- min(SampleSize, length(Vals))

  # Collection Table
  gg <- data.table::data.table(
    Statistic = rep(-1.0, Samples),
    P_Value = rep(-1.0, Samples)
  )

  # Bootstrap
  for(i in seq_len(Samples)) {

    # Sample
    if(length(Vals) > SampleSize) {
      samp <- sample(x = Vals, size = SampleSize)
    } else {
      samp <- Vals
    }

    # Stats
    n <- length(samp)
    m1 <- sum(samp) / n
    m2 <- sum((samp - m1) ^ 2) / n
    m3 <- sum((samp - m1) ^ 3) / n
    m4 <- sum((samp - m1) ^ 4) / n
    b1 <- (m3 / m2 ^ (3 / 2)) ^ 2
    b2 <- (m4 / m2 ^ 2)

    # Statistic
    statistic <- n * b1 / 6 + n * (b2 - 3) ^ 2 / 24
    p.value <- 1 - pchisq(statistic, df = 2)

    # Collect
    data.table::set(gg, i = i, j = "Statistic", value = statistic)
    data.table::set(gg, i = i, j = "P_Value", value = p.value)
  }
  return(gg)
}

#' @title Agostino.Test
#'
#' @family Inference
#'
#' @param Vals a vector of values to test
#' @param SampleSize sub sampling of data
#' @param Samples number of iterations to run
#'
#' @examples
#' \dontrun{
#' output <- Agostino.Test(Vals = runif(100000), SampleSize = 1000, Samples = 500)
#' }
#'
#' @export
Agostino.Test <- function(Vals, SampleSize = NULL, Samples = 1) {

  # Setup
  Vals <- Vals[complete.cases(Vals)]

  # SampleSize
  if(length(SampleSize) == 0L) {
    SampleSize <- min(length(Vals), 46340)
  } else {
    SampleSize <- min(SampleSize, length(Vals), 46340)
  }

  # Samples
  if(SampleSize == length(Vals)) Samples <- 1

  # Collection Table
  gg <- data.table::data.table(
    Skew = rep(-1.0, Samples),
    Alternative_2s = rep("na", Samples),
    P_Value_2s = rep(-1.0, Samples),
    Alternative_greater = rep("na", Samples),
    P_Value_greater = rep(-1.0, Samples),
    Alternative_less = rep("na", Samples),
    P_Value_less = rep(-1.0, Samples)
  )

  # Bootstrap
  for(i in seq_len(Samples)) {

    # Sample
    if(length(Vals) > SampleSize) {
      samp <- sample(x = Vals, size = SampleSize)
    } else {
      samp <- Vals
    }

    samp <- sort(samp[complete.cases(samp)])
    n <- length(samp)
    if ((n < 8 || n > 46340)) return(NULL)
    s3 <- (sum((samp - mean(samp))^3)/n)/(sum((samp - mean(samp))^2)/n)^(3/2)
    y <- s3 * sqrt((n + 1) * (n + 3)/(6 * (n - 2)))
    b2 <- 3 * (n * n + 27 * n - 70) * (n + 1) * (n + 3)/((n - 2) * (n + 5) * (n + 7) * (n + 9))
    w <- sqrt(-1 + sqrt(2 * (b2 - 1)))
    d <- 1/sqrt(log(w))
    a <- sqrt(2/(w * w - 1))
    z <- d * log(y/a + sqrt((y/a)^2 + 1))
    p.value <- pnorm(z, lower.tail = FALSE)

    # Store stats
    data.table::set(gg, i = i, j = "Skew", value = s3)
    pval2s <- 2 * p.value
    if(pval2s > 1) pval2s <- 2 - pval2s
    data.table::set(gg, i = i, j = "Alternative_2s", value = "skewness exists")
    data.table::set(gg, i = i, j = "P_Value_2s", value = pval2s)

    data.table::set(gg, i = i, j = "Alternative_greater", value = "negative skewness exists")
    data.table::set(gg, i = i, j = "P_Value_greater", value = p.value)

    pval <- 1 - p.value
    data.table::set(gg, i = i, j = "Alternative_less", value = "positive skewness exists")
    data.table::set(gg, i = i, j = "P_Value_less", value = pval)
  }
  return(gg)
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Stationarity Tests                                                         ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

# ----

# ----
