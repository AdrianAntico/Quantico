% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ShinyInputs.R
\name{DateInput}
\alias{DateInput}
\title{DateInput}
\usage{
DateInput(
  InputID = NULL,
  Label = NULL,
  Value = Sys.Date(),
  Min = NULL,
  input = NULL,
  Max = NULL,
  PlotDropDown = NULL,
  Key = NULL,
  Format = "yyyy-mm-dd",
  Update = FALSE,
  session = NULL
)
}
\arguments{
\item{InputID}{ID of the input}

\item{Label}{Feeds label}

\item{Value}{Default}

\item{Min}{Min value}

\item{input}{For update usage. Provide input if you want your inputs to update with previous values when screens reload}

\item{Max}{Max value}

\item{PlotDropDown}{From App}

\item{Key}{From App}

\item{Format}{Date format}

\item{session}{For update usgae}

\item{server}{For update usage}
}
\value{
PickerInput object for server.R to go into renderUI({PickerInput()})
}
\description{
DateInput automatically builds a date input with ProjectList argument usage if it exists
}
\examples{
\dontrun{
output$TS_Date <- renderUI({
  Quantico:::DateInput(InputID = "TS_CARMA_HolidayMovingAverages",
                         Label = "Import Data Creation Date",
                         Value = Sys.Date(),
                         Min = "1970-01-01",
                         Max = "2100-01-01",
                         Format = "yyyy-mm-dd")})
}
}
\seealso{
Other Shiny: 
\code{\link{BlankLine}()},
\code{\link{BlankRow}()},
\code{\link{GenerateEvaluationMetrics}()},
\code{\link{NumericInput}()},
\code{\link{PickerInput_GetLevels2}()},
\code{\link{PickerInput_GetLevels}()},
\code{\link{PickerInput}()},
\code{\link{ReturnParam}()},
\code{\link{SelectizeInput1}()},
\code{\link{SelectizeInput}()},
\code{\link{SliderInput}()},
\code{\link{TextAreaInput}()},
\code{\link{TextInput}()}
}
\author{
Adrian Antico
}
\concept{Shiny}
