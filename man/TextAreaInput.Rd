% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ShinyInputs.R
\name{TextAreaInput}
\alias{TextAreaInput}
\title{TextAreaInput}
\usage{
TextAreaInput(
  inputId,
  label,
  value = "",
  width = NULL,
  height = NULL,
  cols = NULL,
  rows = NULL,
  placeholder = NULL,
  resize = NULL,
  textAlign = "left"
)
}
\arguments{
\item{InputID}{ID of the input}

\item{session}{For update usgae}

\item{input}{For update usage. Provide input if you want your inputs to update with previous values when screens reload}

\item{server}{For update usage}

\item{Update}{FALSE. Set to TRUE to run updateSelectizeInput}

\item{Label}{Feeds label}

\item{Choices}{Feeds choices}

\item{SelectedDefault}{Feeds selected for cases where ProjectList has a null element}

\item{Multiple}{Feeds multiple for enabling selecting more than one element from list}

\item{MaxVars}{= NULL}

\item{CloseAfterSelect}{= FALSE,}

\item{PlotDropDown}{From App}

\item{Key}{From App}

\item{Debug}{FALSE}
}
\value{
SelectizeInput object for server.R to go into renderUI({SelectizeInput()})
}
\description{
TextAreaInput automatically builds a picker input with tryCatch's and ProjectList argument usage if it exists
}
\examples{
\dontrun{
output$TS_CARMA_HolidayMovingAverages <- renderUI({
  Quantico:::SelectizeInput(InputID = "TS_CARMA_HolidayMovingAverages", Update = FALSE, Label = "Select Holiday Count MA's", Choices = as.character(0:50),
                           SelectedDefault = as.character(c(1,2)), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)})
}
}
\seealso{
Other Shiny: 
\code{\link{BlankLine}()},
\code{\link{BlankRow}()},
\code{\link{DateInput}()},
\code{\link{GenerateEvaluationMetrics}()},
\code{\link{NumericInput}()},
\code{\link{PickerInput_GetLevels2}()},
\code{\link{PickerInput_GetLevels}()},
\code{\link{PickerInput}()},
\code{\link{ReturnParam}()},
\code{\link{SelectizeInput1}()},
\code{\link{SelectizeInput}()},
\code{\link{SliderInput}()},
\code{\link{TextInput}()}
}
\author{
Adrian Antico
}
\concept{Shiny}
