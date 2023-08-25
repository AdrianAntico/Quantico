#' @title TextAreaInput
#'
#' @description TextAreaInput automatically builds a picker input with tryCatch's and ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param InputID ID of the input
#' @param session For update usgae
#' @param input For update usage. Provide input if you want your inputs to update with previous values when screens reload
#' @param server For update usage
#' @param Update FALSE. Set to TRUE to run updateSelectizeInput
#' @param Label Feeds label
#' @param Choices Feeds choices
#' @param SelectedDefault Feeds selected for cases where ProjectList has a null element
#' @param Multiple Feeds multiple for enabling selecting more than one element from list
#' @param MaxVars = NULL
#' @param CloseAfterSelect = FALSE,
#' @param PlotDropDown From App
#' @param Key From App
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' output$TS_CARMA_HolidayMovingAverages <- renderUI({
#'   DataMuse:::SelectizeInput(InputID = "TS_CARMA_HolidayMovingAverages", Update = FALSE, Label = "Select Holiday Count MA's", Choices = as.character(0:50),
#'                            SelectedDefault = as.character(c(1,2)), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)})
#' }
#' @return SelectizeInput object for server.R to go into renderUI({SelectizeInput()})
#' @export
TextAreaInput <- function(inputId,
                          label,
                          value = "",
                          width = NULL,
                          height = NULL,
                          cols = NULL,
                          rows = NULL,
                          placeholder = NULL,
                          resize = NULL,
                          textAlign = "left") {

  value <- shiny::restoreInput(id = inputId, default = value)
  if(!is.null(resize)) {
    resize <- match.arg(resize, c("both", "none", "vertical", "horizontal"))
  }
  style <- paste0("width: ", width, "; text-align: ", textAlign)
  htmltools::div(
    class = "form-group shiny-input-container",
    shiny:::shinyInputLabel(inputId, label),
    style = if(!is.null(width)) paste0("width: ", htmltools::validateCssUnit(width), "; text-align: ", textAlign),
    shiny::tags$textarea(
      id = inputId,
      class = "form-control",
      placeholder = placeholder,
      style = style,
      rows = rows,
      cols = cols, value))
}

#' @title SelectizeInput
#'
#' @description SelectizeInput automatically builds a picker input with tryCatch's and ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param InputID ID of the input
#' @param session For update usgae
#' @param input For update usage. Provide input if you want your inputs to update with previous values when screens reload
#' @param server For update usage
#' @param Update FALSE. Set to TRUE to run updateSelectizeInput
#' @param Label Feeds label
#' @param Choices Feeds choices
#' @param SelectedDefault Feeds selected for cases where ProjectList has a null element
#' @param Multiple Feeds multiple for enabling selecting more than one element from list
#' @param MaxVars = NULL
#' @param CloseAfterSelect = FALSE,
#' @param PlotDropDown From App
#' @param Key From App
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' output$TS_CARMA_HolidayMovingAverages <- renderUI({
#'   DataMuse:::SelectizeInput(InputID = "TS_CARMA_HolidayMovingAverages", Update = FALSE, Label = "Select Holiday Count MA's", Choices = as.character(0:50),
#'                            SelectedDefault = as.character(c(1,2)), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)})
#' }
#' @return SelectizeInput object for server.R to go into renderUI({SelectizeInput()})
#' @export
SelectizeInput <- function(InputID = "",
                           Update = FALSE,
                           session = NULL,
                           input = NULL,
                           Label = "",
                           Choices = NULL,
                           SelectedDefault = NULL,
                           Multiple = TRUE,
                           MaxVars = 5000,
                           CloseAfterSelect = FALSE,
                           PlotDropDown = NULL,
                           Key = NULL,
                           Debug = FALSE) {
  Options <- list()
  Options[['allowEmptyOption']] <- TRUE
  if(Multiple) Options[['maxItems']] <- MaxVars else Options[['maxItems']] <- 1
  Options[['highlight']] <- TRUE
  Options[['closeAfterSelect']] <- CloseAfterSelect
  # Options[['hideSelected']] <- TRUE
  if(length(PlotDropDown) > 0L && length(Key) > 0L) {
    if(Debug) print("length(PlotDropDown) > 0L && length(Key) > 0L")
    x <- tryCatch({PlotDropDown[[Key]][[InputID]]}, error = function(x) NULL)
    if(length(x) == 0L) x <- tryCatch({input[[InputID]]}, error = function(x) SelectedDefault)
  } else if(length(PlotDropDown) > 0L) {
    x <- tryCatch({input[[InputID]]}, error = function(x) SelectedDefault)
  } else {
    x <- SelectedDefault
  }
  if(all(x %in% c("0","1"))) x <- SelectedDefault
  if(length(Choices) > 0L && data.table::is.data.table(Choices)) Choices <- Choices[[1L]]
  return(
    tryCatch({
      if(!Update) {
        shiny::selectizeInput(inputId = InputID, label = Label, choices = c(Choices), selected = SelectedDefault, options = Options, multiple = Multiple)
      } else {
        shiny::updateSelectizeInput(session = session, server = TRUE, inputId = InputID, label = Label, choices = Choices, selected = x, options = Options)
      }}, error = function(x) {
        shiny::selectizeInput(inputId = InputID, label = Label, choices = "No Data Loaded", selected = "No Data Loaded", options = Options, multiple = Multiple)
      })
  )
}

#' @title SelectizeInput1
#'
#' @description SelectizeInput1 automatically builds a picker input with tryCatch's and ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param InputID ID of the input
#' @param session For update usgae
#' @param input For update usage. Provide input if you want your inputs to update with previous values when screens reload
#' @param server For update usage
#' @param Update FALSE. Set to TRUE to run updateSelectizeInput
#' @param Label Feeds label
#' @param Choices Feeds choices
#' @param SelectedDefault Feeds selected for cases where ProjectList has a null element
#' @param Multiple Feeds multiple for enabling selecting more than one element from list
#' @param MaxVars = NULL
#' @param CloseAfterSelect = FALSE,
#' @param PlotDropDown From App
#' @param Key From App
#' @param Debug FALSE
#'
#' @examples
#' \dontrun{
#' output$TS_CARMA_HolidayMovingAverages <- renderUI({
#'   DataMuse:::SelectizeInput(InputID = "TS_CARMA_HolidayMovingAverages", Update = FALSE, Label = "Select Holiday Count MA's", Choices = as.character(0:50),
#'                            SelectedDefault = as.character(c(1,2)), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)})
#' }
#' @return SelectizeInput object for server.R to go into renderUI({SelectizeInput()})
#' @export
SelectizeInput1 <- function(InputID = "",
                            Update = FALSE,
                            session = NULL,
                            input = NULL,
                            Label = "",
                            Choices = NULL,
                            SelectedDefault = NULL,
                            Multiple = TRUE,
                            MaxVars = 5000,
                            CloseAfterSelect = FALSE,
                            Debug = FALSE) {

  Options <- list()
  Options[['allowEmptyOption']] <- TRUE
  if(Multiple) Options[['maxItems']] <- MaxVars else Options[['maxItems']] <- 1
  Options[['highlight']] <- TRUE
  Options[['closeAfterSelect']] <- CloseAfterSelect
  if(length(Choices) > 0L && data.table::is.data.table(Choices)) Choices <- Choices[[1L]]
  return(
    tryCatch({
      if(!Update) {
        shiny::selectizeInput(inputId = InputID, label = Label, choices = c(unique(Choices)), selected = SelectedDefault, options = Options, multiple = Multiple)
      } else {
        if(Debug) print("Update Selectize Input")
        if(Debug) print(x)
        shiny::updateSelectizeInput(session = session, server = TRUE, inputId = InputID, label = Label, choices = Choices, selected = NULL, options = Options)
      }}, error = function(x) {
        shiny::selectizeInput(inputId = InputID, label = Label, choices = "No Data Loaded", selected = "No Data Loaded", options = Options, multiple = Multiple)
      })
  )
}

#' @title SliderInput
#'
#' @description NumericInput automatically builds a numeric input with tryCatch's and ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param InputID ID of the input
#' @param session For update usgae
#' @param input For update usage. Provide input if you want your inputs to update with previous values when screens reload
#' @param server For update usage
#' @param Update FALSE. Set to TRUE to run updateSelectizeInput
#' @param Label Feeds label
#' @param Step Feeds size in the options list
#' @param Value Default
#' @param Min Min value
#' @param Max Max value
#' @param PlotDropDown From App
#' @param Key From App
#'
#' @examples
#' \dontrun{
#' output$TS_CARMA_HolidayMovingAverages <- renderUI({
#'   DataMuse:::SliderInput(InputID = "TS_CARMA_HolidayMovingAverages",
#'                             Label = "Select Holiday Count MA's",
#'                             Min = 1,
#'                             Max = 1,
#'                             Value = 1,
#'                             Step = 1)})
#' }
#' @return PickerInput object for server.R to go into renderUI({PickerInput()})
#' @export
SliderInput <- function(InputID = NULL,
                        Update = FALSE,
                        session = NULL,
                        input = NULL,
                        server = NULL,
                        Label = NULL,
                        PlotDropDown = NULL,
                        Key = NULL,
                        Step = 10,
                        Value = 1,
                        Min = 1,
                        Max = 10,
                        MainPanel = NULL) {
  if(length(PlotDropDown) > 0L && length(Key) > 0L) {
    if(length(MainPanel) > 0L) {
      x <- tryCatch({PlotDropDown[[Key]][[length(PlotDropDown[[Key]])]]}, error = function(x) NULL)
    } else {
      x <- tryCatch({PlotDropDown[[Key]][[InputID]][[length(PlotDropDown[[Key]][[InputID]])]]}, error = function(x) NULL)
    }
    if(length(x) == 0L) {
      x <- tryCatch({input[[InputID]]}, error = function(x) NULL)
      if(length(x) == 0L) x <- Value
    }
  } else {
    if(length(input[[InputID]]) > 0L && input[[InputID]] %in% seq(Min,Max,Step)) {
      x <- input[[InputID]]
    } else {
      x <- Value
    }
  }
  return(
    tryCatch({
      if(!Update) {
        shiny::sliderInput(inputId = InputID, label = Label, value = x, min = Min, max = Max, step = Step)
      } else {
        shiny::updateSliderInput(session = session, inputId = InputID, value = x)
      }}, error = function(x) {
        shiny::sliderInput(inputId = InputID, label = Label, value = x, min = Min, max = Max, step = Step)
      })
  )
}

#' @title NumericInput
#'
#' @description NumericInput automatically builds a numeric input with tryCatch's and ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param InputID ID of the input
#' @param session For update usgae
#' @param input For update usage. Provide input if you want your inputs to update with previous values when screens reload
#' @param server For update usage
#' @param Update FALSE. Set to TRUE to run updateSelectizeInput
#' @param Label Feeds label
#' @param Step Feeds size in the options list
#' @param Value Default
#' @param Min Min value
#' @param Max Max value
#' @param PlotDropDown From App
#' @param Key From App
#'
#' @examples
#' \dontrun{
#' output$TS_CARMA_HolidayMovingAverages <- renderUI({
#'   DataMuse:::NumericInput(InputID = "TS_CARMA_HolidayMovingAverages",
#'                             Label = "Select Holiday Count MA's",
#'                             Min = 1,
#'                             Max = 1,
#'                             Value = 1,
#'                             Step = 1)})
#' }
#' @return PickerInput object for server.R to go into renderUI({PickerInput()})
#' @export
NumericInput <- function(InputID = NULL,
                         Update = FALSE,
                         session = NULL,
                         input = NULL,
                         server = NULL,
                         Label = NULL,
                         PlotDropDown = NULL,
                         Key = NULL,
                         Step = 10,
                         Value = 1,
                         Min = 1,
                         Max = 10) {
  if(length(PlotDropDown) > 0L && length(Key) > 0L) {
    x <- tryCatch({PlotDropDown[[Key]][[InputID]][[length(PlotDropDown[[Key]][[InputID]])]]}, error = function(x) NULL)
    if(length(x) == 0L) {
      x <- tryCatch({input[[InputID]]}, error = function(x) Value)
    }
  } else {
    if(length(input[[InputID]]) > 0L && input[[InputID]] %in% seq(Min,Max,Step)) {
      x <- input[[InputID]]
    } else {
      x <- Value
    }
  }
  return(
    tryCatch({
      if(!Update) {
        shiny::numericInput(inputId = InputID, label = Label, value = x, min = Min, max = Max, step = Step)
      } else {
        shiny::updateNumericInput(session = session, inputId = InputID, label = Label, value = x, min = Min, max = Max, step = Step)
      }}, error = function(x) {
        shiny::numericInput(inputId = InputID, label = Label, value = x, min = Min, max = Max, step = Step)
      })
  )
}

#' @title DateInput
#'
#' @description DateInput automatically builds a date input with ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param InputID ID of the input
#' @param session For update usgae
#' @param input For update usage. Provide input if you want your inputs to update with previous values when screens reload
#' @param server For update usage
#' @param Label Feeds label
#' @param Value Default
#' @param Min Min value
#' @param Max Max value
#' @param Format Date format
#' @param PlotDropDown From App
#' @param Key From App
#'
#' @examples
#' \dontrun{
#' output$TS_Date <- renderUI({
#'   DataMuse:::DateInput(InputID = "TS_CARMA_HolidayMovingAverages",
#'                          Label = "Import Data Creation Date",
#'                          Value = Sys.Date(),
#'                          Min = "1970-01-01",
#'                          Max = "2100-01-01",
#'                          Format = "yyyy-mm-dd")})
#' }
#' @return PickerInput object for server.R to go into renderUI({PickerInput()})
#' @export
DateInput <- function(InputID = NULL,
                      Label = NULL,
                      Value = Sys.Date(),
                      Min = NULL,
                      input = NULL,
                      Max = NULL,
                      PlotDropDown = NULL,
                      Key = NULL,
                      Format = "yyyy-mm-dd",
                      Update = FALSE,
                      session = NULL) {
  if(length(PlotDropDown) > 0L && length(Key) > 0L) {
    x <- tryCatch({PlotDropDown[[Key]][[InputID]][[length(PlotDropDown[[Key]][[InputID]])]]}, error = function(x) NULL)
    if(length(x) == 0L) {
      x <- tryCatch({input[[InputID]]}, error = function(x) Value)
    }
  } else {
    if(length(input[[InputID]]) > 0L && input[[InputID]] != "") {
      x <- input[[InputID]]
    } else {
      x <- Value
    }
  }
  return(
    tryCatch({
      if(!Update) {
        shiny::dateInput(inputId = InputID, label = Label, value = Sys.Date(), min = Min, max = Max, format = Format)
      } else {
        Value <- input[[InputID]]
        shiny::updateDateInput(session = session, inputId = InputID, label = Label, value = Value, min = Min, max = Max, format = Format)
      }}, error = function(x) {
        shiny::dateInput(inputId = InputID, label = Label, value = Sys.Date(), min = Min, max = Max, format = Format)
      })
  )
}

#' @title TextInput
#'
#' @description TextInput automatically builds a text input with ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param InputID ID of the input
#' @param session For update usgae
#' @param input For update usage. Provide input if you want your inputs to update with previous values when screens reload
#' @param server For update usage
#' @param Label Feeds label
#' @param Value Default
#' @param Width width arg
#' @param Format Date format
#' @param Update FALSE
#' @param PlotDropDown From App
#' @param Key From App
#'
#' @examples
#' \dontrun{
#' output$TS_Date <- renderUI({
#'   DataMuse:::TextInput(InputID = "TS_CARMA_HolidayMovingAverages",
#'                          Label = "Import Data Creation Date",
#'                          Value = Sys.Date(),
#'                          Placeholder = "yyyy-mm-dd")})
#' }
#' @return PickerInput object for server.R to go into renderUI({PickerInput()})
#' @export
TextInput <- function(InputID = NULL,
                      input = NULL,
                      Label = NULL,
                      Value = NULL,
                      PlotDropDown = NULL,
                      Key = NULL,
                      Width = "100%",
                      Placeholder = "",
                      session = NULL,
                      Update = FALSE) {

  # print("here 1")
  if(length(PlotDropDown) > 0L && length(Key) > 0L) {
    # print("here 2")
    x <- tryCatch({PlotDropDown[[Key]][[InputID]][[length(PlotDropDown[[Key]][[InputID]])]]}, error = function(x) NULL)
    # print("here 3")
    if(length(x) == 0L) {
      # print("here 4")
      if(length(input[[InputID]]) > 0L && input[[InputID]] != "") {
        # print("here 5")
        x <- input[[InputID]]
      } else {
        # print("here 6")
        x <- Value
      }
    }
  } else {
    # print("here 8")
    if(length(input[[InputID]]) > 0L && input[[InputID]] != "") {
      # print("here 9")
      x <- input[[InputID]]
    } else {
      # print("here 10")
      x <- Value
    }
  }
  return(
    tryCatch({
      if(!Update) {
        shiny::textInput(inputId = InputID, label = Label, value = x, width = Width, placeholder = Placeholder)
      } else {
        shiny::updateTextInput(session = session, inputId = InputID, label = Label, value = x)
      }}, error = function(x) {
        shiny::textInput(inputId = InputID, label = '!! No Data Loaded', value = NULL, width = Width, placeholder = Placeholder)
      })
  )
}

#' @title PickerInput
#'
#' @description PickerInput automatically builds a picker input with tryCatch's and ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param InputID ID of the input
#' @param session For update usgae
#' @param input For update usage. Provide input if you want your inputs to update with previous values when screens reload
#' @param server For update usage
#' @param Label Feeds label
#' @param Choices Feeds choices
#' @param SelectedDefault Feeds selected for cases where ProjectList has a null element
#' @param SubText = NULL
#' @param Size Feeds size in the options list
#' @param ContentList NULL
#' @param SelectedText Feeds selected-text-format in options list
#' @param Multiple Feeds multiple for enabling selecting more than one element from list
#' @param ActionBox Feeds actions-box for option list
#' @param Debug FALSE
#' @param Update = FALSE
#' @param PlotDropDown From App
#' @param Key From App
#'
#' @examples
#' \dontrun{
#' output$TS_CARMA_HolidayMovingAverages <- renderUI({
#'   DataMuse:::PickerInput(InputID = "TS_CARMA_HolidayMovingAverages", Label = "Select Holiday Count MA's", Choices = as.character(0:50),
#'                            SelectedDefault = as.character(c(1,2)), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)})
#' }
#' @return PickerInput object for server.R to go into renderUI({PickerInput()})
#' @export
PickerInput <- function(session = NULL,
                        input = NULL,
                        Update = FALSE,
                        InputID = NULL,
                        Label = NULL,
                        Choices = NULL,
                        SelectedDefault = NULL,
                        SubText = NULL,
                        ContentList = NULL,
                        Size = 18,
                        SelectedText = "count > 10",
                        PlotDropDown = NULL,
                        Key = NULL,
                        Multiple = TRUE,
                        MaxVars = NULL,
                        ActionBox = TRUE,
                        PlotSelection = NULL,
                        Debug = FALSE) {

  Options <- list()

  # Limit to max_options_group per group
  if(length(MaxVars) > 0L) {
    Options[["max-options-group"]] <- MaxVars
    Options[["maxOptions"]] <- MaxVars
    Options[["max-options-text"]] <- paste0("Max of ", MaxVars, " per group")
  }

  Options[["multiple-separator"]] <- " | "
  Options[["actions-box"]] <- ActionBox
  Options[["size"]] <- Size
  Options[["selected-text-format"]] <- SelectedText
  Options[["live-search"]] <- TRUE
  Options[["deselect-all-text"]] <- "Remove All"
  Options[["select-all-text"]] <- "Select All"
  if(length(PlotSelection) == 0L) Options[["none-selected-text"]] <- "Nothing Selected" else Options[["none-selected-text"]] <- PlotSelection
  if(length(PlotDropDown) > 0L && length(Key) > 0L) {
    x <- tryCatch({PlotDropDown[[Key]][[InputID]]}, error = function(x) NULL)
    print("Picker Input x")
    print(x)
    if(length(x) == 0L) {
      x <- tryCatch({input[[InputID]]}, error = function(x) NULL)
      if(length(x) == 0L) x <- SelectedDefault
    }
  } else {
    if(length(input[[InputID]]) > 0L && all(input[[InputID]] %in% Choices)) {
      x <- input[[InputID]]
    } else {
      x <- SelectedDefault
    }
  }

  return(
    tryCatch({
      if(!Update) {
        shinyWidgets::pickerInput(
          inputId = InputID,
          label = Label, choices = Choices, selected = x, options = Options, multiple = Multiple)#, choicesOpt = list(content = ContentList),
      } else {
        if(length(Choices) > 0L) {
          shinyWidgets::updatePickerInput(session = session, inputId = InputID, label = Label, choices = Choices, selected = c(x), options = Options)
        } else {
          shinyWidgets::updatePickerInput(session = session, inputId = InputID, label = Label, choices = NULL, selected = "", options = Options)
        }

      }}, error = function(x) {
        shinyWidgets::pickerInput(inputId = InputID, label = Label, choices = "No Data Loaded", selected = "No Data Loaded", options = Options, multiple = Multiple)
      })
  )
}

#' @title PickerInput_GetLevels
#'
#' @description PickerInput_GetLevels automatically builds a picker input with tryCatch's and ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param input input object within shiny context
#' @param data 'SourceData' or whatever the name of your data is
#' @param NumGroupVar Which group var to select
#' @param InputID Feeds ProjectList and inputId. Argument saved in ProjectList
#' @param InputID2 Secondary object name
#' @param Choices Feeds choices
#' @param SelectedDefault Feeds selected for cases where ProjectList has a null element
#' @param Size Feeds size in the options list
#' @param SelectedText Feeds selected-text-format in options list
#' @param Multiple Feeds multiple for enabling selecting more than one element from list
#' @param ActionBox Feeds actions-box for option list
#' @param Update = FALSE,
#' @param session = NULL
#' @param PlotDropDown From App
#' @param Key From App
#'
#' @examples
#' \dontrun{
#' output$TS_CARMA_HolidayMovingAverages <- renderUI({
#'   DataMuse:::PickerInput_GetLevels(
#'     input, InputID = "TS_CARMA_HolidayMovingAverages", Label = "Select Holiday Count MA's", Choices = as.character(0:50),
#'     SelectedDefault = as.character(c(1,2)), Size = 10, SelectedText = "count > 1", Multiple = TRUE, ActionBox = TRUE)})
#' }
#' @return PickerInput object for server.R to go into renderUI({PickerInput()})
#' @export
PickerInput_GetLevels <- function(input,
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
                                  session = NULL) {
  if(length(PlotDropDown) > 0L && length(Key) > 0L) {
    x <- tryCatch({PlotDropDown[[Key]][[InputID]][[length(PlotDropDown[[Key]][[InputID]])]]}, error = function(x) SelectedDefault)
  } else {
    x <- tryCatch({input[[InputID]]}, error = function(x) SelectedDefault)
  }
  return(
    if(Update) {
      if(!is.null(input[[InputID2]])) {
        if(length(input[[InputID2]]) >= NumGroupVar && !'None' %in% input[[InputID2]]) {
          shinyWidgets::updatePickerInput(session = session, inputId = InputID, label = paste0(input[[InputID2]][[NumGroupVar]]," Levels"),
                                          choices = Choices, selected = x,
                                          options = list(
                                            `actions-box` = TRUE,
                                            size = 10,
                                            `selected-text-format` = "count > 1",
                                            `live-search` = TRUE))
        } else {
          shinyWidgets::updatePickerInput(session = session, inputId = InputID, label = "< N/A >", choices = x, selected = x, multiple = TRUE, width = "100%")
        }
      } else {
        shinyWidgets::updatePickerInput(session = session, inputId = InputID, label = "< N/A >", choices = x, selected = x, multiple = TRUE, width = "100%")
      }
    } else {
      shinyWidgets::pickerInput(session = session, inputId = InputID, label = "< N/A >", choices = x, selected = x, multiple = TRUE, width = "100%")
    }
  )
}

#' @title PickerInput_GetLevels2
#'
#' @description PickerInput_GetLevels2 automatically builds a picker input with tryCatch's and ProjectList argument usage if it exists
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param InputID Feeds ProjectList and inputId. Argument saved in ProjectList
#' @param Choices Feeds choices
#' @param SelectedDefault Feeds selected for cases where ProjectList has a null element
#' @param session = NULL
#' @param input = NULL
#' @param PlotDropDown From App
#' @param Key From App
#' @param GroupVariable From App
#'
#' @examples
#' \dontrun{
#' output$TS_CARMA_HolidayMovingAverages <- renderUI({
#'   DataMuse:::PickerInput_GetLevels2(
#'     InputID = "TS_CARMA_HolidayMovingAverages", Label = "Select Holiday Count MA's", Choices = as.character(0:50),
#'     SelectedDefault = as.character(c(1,2)), Multiple = TRUE)})
#' }
#' @return PickerInput object for server.R to go into renderUI({PickerInput()})
#' @export
PickerInput_GetLevels2 <- function(session = NULL,
                                   input = NULL,
                                   Update = FALSE,
                                   InputID = NULL,
                                   PlotDropDown = NULL,
                                   Key = NULL,
                                   Choices = NULL,
                                   SelectedDefault = NULL,
                                   GroupVariable = NULL) {

  if(length(PlotDropDown) > 0L && length(Key) > 0L) {
    x <- tryCatch({PlotDropDown[[Key]][[InputID]]}, error = function(x) NULL)
    if(length(x) == 0L) {
      x <- tryCatch({input[[InputID]]}, error = function(x) SelectedDefault)
    }
  } else {
    x <- tryCatch({input[[InputID]]}, error = function(x) SelectedDefault)
  }
  return(
    if(length(Choices) > 0L) {
      print("x")
      print(x)
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = InputID, label = paste0(GroupVariable," Levels"),
        choices = Choices,
        selected = x,
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 1",
          `live-search` = TRUE))

    } else if(Update) {
      print("here 2.1")
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = InputID,
        label = "< N/A >",
        choices = NULL,
        selected = "",
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 1",
          `live-search` = TRUE))
    } else {
      shinyWidgets::pickerInput(
        inputId = InputID,
        label = "< N/A >",
        choices = NULL,
        selected = NULL,
        multiple = TRUE,
        width = "100%",
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 1",
          `live-search` = TRUE))
    })
}
