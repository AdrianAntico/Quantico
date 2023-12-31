% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ShinyInputs.R
\name{PickerInput_GetLevels}
\alias{PickerInput_GetLevels}
\title{PickerInput_GetLevels}
\usage{
PickerInput_GetLevels(
  input,
  data = NULL,
  NumGroupVar = 0,
  InputID = NULL,
  InputID2 = NULL,
  Choices = NULL,
  PlotDropDown = NULL,
  Key = NULL,
  SelectedDefault = NULL,
  Size = 10,
  SelectedText = "count > 1",
  Multiple = TRUE,
  ActionBox = TRUE,
  Update = FALSE,
  session = NULL
)
}
\arguments{
\item{input}{input object within shiny context}

\item{data}{'SourceData' or whatever the name of your data is}

\item{NumGroupVar}{Which group var to select}

\item{InputID}{Feeds ProjectList and inputId. Argument saved in ProjectList}

\item{InputID2}{Secondary object name}

\item{Choices}{Feeds choices}

\item{PlotDropDown}{From App}

\item{Key}{From App}

\item{SelectedDefault}{Feeds selected for cases where ProjectList has a null element}

\item{Size}{Feeds size in the options list}

\item{SelectedText}{Feeds selected-text-format in options list}

\item{Multiple}{Feeds multiple for enabling selecting more than one element from list}

\item{ActionBox}{Feeds actions-box for option list}

\item{Update}{= FALSE,}

\item{session}{= NULL}
}
\value{
PickerInput object for server.R to go into renderUI({PickerInput()})
}
\description{
PickerInput_GetLevels automatically builds a picker input with tryCatch's and ProjectList argument usage if it exists
}
\examples{
\dontrun{
output$TS_CARMA_HolidayMovingAverages <- renderUI({
  Quantico:::PickerInput_GetLevels(
    input, InputID = "TS_CARMA_HolidayMovingAverages", Label = "Select Holiday Count MA's", Choices = as.character(0:50),
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
