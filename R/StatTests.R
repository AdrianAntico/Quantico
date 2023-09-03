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
Anderson.Darling.Test <- function(Vals, SampleSize = 5000, Samples = 30) {

  # Samples > 5000 not allowed
  SampleSize <- min(SampleSize, length(Vals))

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
Kolmogorov.Smirnov.Test <- function(Vals, SampleSize = 500, Samples = 30) {

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
Shapiro.Test <- function(Vals, SampleSize = 5000, Samples = 30) {

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
Jarque.Bera.Test <- function(Vals, SampleSize = 5000, Samples = 30) {

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
Agostino.Test <- function(Vals, SampleSize = 5000, Samples = 30) {

  # Setup
  Vals <- Vals[complete.cases(Vals)]
  SampleSize <- min(SampleSize, length(Vals), 46340)

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

