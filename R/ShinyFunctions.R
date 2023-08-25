#' @export
vec2list <- function(x) {

  # options <- list(
  #   list(key = "A", text = "Option A"),
  #   list(key = "B", text = "Option B"),
  #   list(key = "C", text = "Option C")
  # )
  #
  # x <- c('A', 'B', 'C')
  #
  xx <- list()
  for(i in seq_along(x)) {
    xx[[i]] <- list(key = x[i], text = as.character(x[i]))
  }
  xx
}

#' @export
makePage <- function(title = NULL,
                     subtitle = NULL,
                     contents = NULL,
                     titleFontSize = 32,
                     subtitleFontSize = 14,
                     titleColor = "#323130",
                     subtitleColor = "#605E5C",
                     subtitleMargin = "14px") {

  # Title on & Subtitle On:
  if(length(title) > 0L && length(subtitle) > 0L) {
    return(
      shiny::tagList(
        shiny::div(
          class = "page-title",
          shiny::span(title, class = glue::glue("ms-fontSize-{titleFontSize} ms-fontWeight-semibold"), style = glue::glue("color: {titleColor}")),
          shiny::span(subtitle, class = glue::glue("ms-fontSize-{subtitleFontSize} ms-fontWeight-regular"), style = glue::glue("color: {subtitleColor}; margin: {subtitleMargin};"))),
        contents))

  }

  # Title on & Subtitle Off:
  if(length(title) > 0L && length(subtitle) == 0L) {

    return(
      shiny::tagList(
        shiny::div(
          class = "page-title",
          shiny::span(title, class = glue::glue("ms-fontSize-{titleFontSize} ms-fontWeight-semibold"), style = glue::glue("color: {titleColor}"))),
        contents))

  }

  # Title off & Subtitle on:
  if(length(title) == 0L && length(subtitle) > 0L) {

    return(
      shiny::tagList(
        shiny::div(
          class = "page-title",
          shiny::span(subtitle, class = glue::glue("ms-fontSize-{subtitleFontSize} ms-fontWeight-regular"), style = glue::glue("color: {subtitleColor}; margin: {subtitleMargin};"))),
        contents))

  }

  # Title off & Subtitle off:
  return(shiny::tagList(contents))
}

#' @export
makeCard <- function(title = NULL,
                     content = NULL,
                     Stack_Horizontal = FALSE,
                     Stack_ChildrenGap = 5,
                     Text_variant = 'large',
                     ms_sm_size = 12,
                     ms_depth = 8,
                     style = "") {
  shiny::div(
    class = glue::glue("card ms-depth-{ms_depth} ms-sm{ms_sm_size} ms-xl{ms_sm_size}"),
    style = style,
    shiny.fluent::Stack(
      tokens = list(childrenGap = Stack_ChildrenGap),
      shiny.fluent::Text(
        variant = Text_variant,
        title,
        block = TRUE,
        nowrap = TRUE),
      content
    )
  )
}

#' @export
makeExamplePage <- function(name, ui) {
  makePage(
    name,
    "Fluent UI component",
    shiny::div(
      makeCard(
        "Live example",
        shiny::div(
          style = "padding: 20px",
          ui))))
}

#' @title DataTable2
#'
#' @description Fully loaded DT::datatable() with args prefilled
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param data source data.table
#' @param FixedCols = 2L
#'
#' @examples
#' \dontrun{
#' # Rmarkdown example of DataTable2 inside a <details> </Details> section
#'
#' ```{r Get Dependencies For DT::datatable(), echo=FALSE,include = FALSE}
#' # You need this code to conduct the magic dependences attaching...
#' DT::datatable(matrix())
#' ```
#'
#' ```{js Nest All DT::datatable() inside a details drop down, echo=FALSE}
#' setTimeout(function() {
#'   var codes = document.querySelectorAll('.dataTables_wrapper');
#'   var code, i, d, s, p;
#'   for (i = 0; i < codes.length; i++) {
#'     code = codes[i];
#'     p = code.parentNode;
#'     d = document.createElement('details');
#'     s = document.createElement('summary');
#'     s.innerText = 'Details';
#'     // <details><summary>Details</summary></details>
#'       d.appendChild(s);
#'     // move the code into <details>
#'       p.replaceChild(d, code);
#'     d.appendChild(code);
#'   }
#' });
#' ```
#'
#' ```{r Example, echo = FALSE}
#' DataMuse:::DataTable2(data)
#' ````
#'
#' # Shiny Usage
#' output$Table <- shiny::renderUI({DataMuse:::DataTable2(data)})
#'
#' }
#'
#' @noRd
DataTable2 <- function(data, FixedCols = 2L) {
  DT::datatable(
    data,
    filter = 'bottom',
    editable = TRUE,
    rownames = FALSE,
    extensions = c('Buttons','ColReorder','FixedColumns'), # Only usable in Rmarkdown  'Select'),
    options = list(
      select = list(style = 'os', items = 'row'),
      dom = 'Brtip', #Bfrtip
      #dom = 'ltipr',
      fixedColumns = list(leftColumns = 0L),
      buttons = c('copy','pdf', 'selectRows', 'selectColumns', 'selectCells', 'selectAll', 'selectNone'),
      colReorder = TRUE,
      autoWidth = TRUE,
      selection = list(mode = 'multiple', target = 'row+column'), # 'row', 'column'
      style = 'bootstrap', # 'auto', 'default', 'bootstrap', or 'bootstrap4'
      columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(data)-1L))),
      targets = "_all",
      scrollX = TRUE,
      fillContainer = TRUE,
      autoHideNavigation = TRUE,
      lengthMenu = c(5, 30, 50),
      pageLength = 10))
}

#' @title PlotLimits
#'
#' @param p data.table
#' @param YMin Y Min Value
#' @param YMax Y Max Value
#' @param XMin X Min Value
#' @param XMax X Max Value
#' @param Debug = FALSE
#'
#' @noRd
PlotLimits <- function(p, YMin, YMax, XMin, XMax, Debug = FALSE) {

  if(Debug) {
    print(YMin)
    print(YMax)
    print(XMin)
    print(XMax)
    print(length(p))
  }

  if(missing(p) || length(p) == 0) return(NULL)
  if(Debug) {print('here 1'); print(missing(p))}
  if(missing(YMin)) YMin <- NA else if(length(YMin) == 0) YMin <- NA else if(YMin == "") YMin <- NA
  if(Debug) {print('here 2'); print(YMin)}
  if(missing(YMax)) YMax <- NA else if(length(YMax) == 0) YMax <- NA else if(YMax == "") YMax <- NA
  if(Debug) {print('here 3'); print(YMax)}
  if(missing(XMin)) XMin <- NA else if(length(XMin) == 0) XMin <- NA else if(XMin == "") XMin <- NA
  if(Debug) {print('here 4'); print(XMin)}
  if(missing(XMax)) XMax <- NA else if(length(XMax) == 0) XMax <- NA else if(XMax == "") XMax <- NA
  if(Debug) {
    print(is.na(YMin))
    print(is.na(YMax))
    print(is.na(XMin))
    print(is.na(XMax))
    print(!all(is.na(YMin), is.na(YMax), is.na(XMin), is.na(XMax)))
  }
  if(!all(is.na(YMin), is.na(YMax), is.na(XMin), is.na(XMax))) {
    p <- p + ggplot2::coord_cartesian(xlim = c(as.numeric(XMin), as.numeric(XMax)), ylim = c(as.numeric(YMin), as.numeric(YMax)))
    return(eval(p))
  } else {
    return(eval(p))
  }
}

# Creates two small text inputs for min and max
# div(style='display:inline-block', textInput3(inputId="xlimitsmin", label="x-min", value = 0.0, class="input-small")),
# div(style='display:inline-block', textInput3(inputId="xlimitsmax", label="x-max", value = 0.5, class="input-small")),
textInput2 <- function(inputId, label, value = "", ...) {
  tagList(
    tags$label(
      label, `for` = inputId),
    tags$input(
      id = inputId,
      type = "text",
      value = value,...))
}
textInput3 <- function (inputId, label, value = "", ...) {
  div(
    style="display:inline-block",
    tags$label(
      label,
      `for` = inputId),
    tags$input(
      id = inputId,
      type = "text",
      value = value,
      ...))
}

#' @title FL_Default
#'
#' @param data data.table
#' @param x input[['FilterVariable_1_1']]
#'
#' @noRd
FL_Default <- function(data, x = NULL) {
  if(missing(data)) {
    print('FL_Default: data is missing')
    return('>=')
  }
  if(length(data) == 0L) {
    print('FL_Default: data is NULL')
    return('>=')
  }
  if(missing(x)) {
    print('FL_Default: x is missing')
    return('>=')
  }
  if(length(x) == 0) {
    print('FL_Default: x is NULL')
    return('>=')
  }
  if(x != 'None') {
    z <- class(data[[eval(x)]])
  } else {
    z <- 'missing'
  }
  if(any(z %in% c('factor', 'character'))) return('%chin%') else return('>=')
}

#' @noRd
LevelValues <- function(x) {
  if(missing(x)) {
    print('LevelValues x is missing')
    return(NULL)
  } else if(!exists('x')) {
    print('LevelValues x does not exist')
    return(NULL)
  } else if(length(x) == 0) {
    print('LevelValues x has length 0')
    return(NULL)
  } else if(!'None' %in% x && length(x) >= 1L) {
    return(x)
  } else if('None' %in% x && length(x) >= 1L) {
    return(x[!x %in% 'None'])
  } else {
    return(NULL)
  }
}

#' @noRd
PDPVar <- function(ModelOutputList) {
  if(!is.null(ModelOutputList)) {
    x <- names(ModelOutputList$PlotList$Test_ParDepPlots)
    y <- x[1L]
  } else {
    x <- NULL
    y <- NULL
  }
  return(list(Names = x, Default = y))
}

#' @noRd
YTicks <- function(data, yvar = NULL) {
  if(length(yvar) == 1L) {
    Uniques <- tryCatch({data[, unique(get(yvar))]}, error = function(x) NULL)
    if(!is.null(Uniques) && all(Uniques != 'Default')) {
      if(any(class(data[[eval(yvar)]]) %in% c('numeric','integer')) && Uniques > 10L) {
        x <- c('Default', 'percentiles', '5th-tiles', 'Deciles', 'Quantiles', 'Quartiles', as.character(data[, quantile(round(get(yvar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]))
      } else {
        x <- c('Default', Uniques)
      }
    } else {
      x <- 'Default'
    }
  } else {
    x <- 'Default'
  }
  return(x)
}

#' @noRd
XTicks <- function(data, xvar=NULL, Debug = FALSE) {
  if(Debug) print('XTicks 1')
  if(length(xvar) == 1L) {
    if(Debug) print('XTicks step 2')
    Uniques <- tryCatch({data[, unique(get(xvar))]}, error = function(x) NULL)
    # print(paste0("Uniques", Uniques))
    if(Debug) print('XTicks step 3')
    x <- class(data[[eval(xvar)]])[1L]
    print(paste0('All classes: ', class(data[[eval(xvar)]])))
    if(Debug) print('XTicks step 3')
    print(length(Uniques) > 10L)
    print(any(x %chin% c('numeric','integer')))
    if(any(x %chin% c('numeric','integer')) && length(Uniques) > 10L) {
      print("xticks Numeric")
      choices <- c('Default', 'Percentiles', 'Every 5th percentile', 'Deciles', 'Quantiles', 'Quartiles', as.character(data[, quantile(round(get(xvar), 4L), na.rm = TRUE, probs = c(seq(0, 1, 0.01)))]))
    } else if(any(x %chin% c('Date'))) {
      print("xticks Date")
      choices <- c('Default', '1 year', '1 day', '1 week', '1 month', '3 day', '2 week', '3 month', '6 month', '2 year', '5 year', '10 year')
    } else if(any(x %like% c('POSIX'))) {
      print("xticks Posix")
      choices <- c('Default', '1 year', '1 day', '3 day', '1 week', '2 week', '1 month', '3 month', '6 month', '2 year', '5 year', '10 year', '1 minute', '15 minutes', '30 minutes', '1 hour', '3 hour', '6 hour', '12 hour')
    } else if(any(x %like% c('character','numeric','integer'))) {
      print("xticks like character, numeric, integer")
      choices <- c('Default', Uniques)
    } else {
      print("xticks else")
      choices <- c('Default', Uniques)
    }
  } else {
    print("xticks else baby")
    choices <- 'Default'
  }
  return(choices)
}

#' @title UniqueLevels
#'
#' @param input passthrough
#' @param data data.table
#' @param GroupVars passthrough
#'
#' @export
UniqueLevels <- function(input, data, n, GroupVars=NULL) {
  if(missing(n) || missing(GroupVars) && is.null(GroupVars[[n]]) || is.na(GroupVars[[n]])) {
    return(NULL)
  } else {
    gg <- tryCatch({data[[GroupVars[[n]]]]}, error = function(x) NULL)
    if(any(class(gg) %in% c('factor'))) {
      return(tryCatch({c(sort(as.character(unique(gg))))}, error = function(x)  NULL))
    } else {
      return(tryCatch({c(sort(unique(gg)))}, error = function(x)  NULL))
    }
  }
}

#' @title FilterValues
#'
#' @param data data.table
#' @param VarName Variable name
#' @param type 1 for min, 2 for max
#'
#' @export
FilterValues <- function(data, VarName = NULL, type = 1) {

  if(missing(data)) {
    print('FilterValues(): data was missing')
    x <- NULL
  } else if(is.null(data)) {
    print('FilterValues(): data was NULL')
    x <- NULL
  } else if(length(VarName) == 0) {
    print('FilterValues(): VarName was length 0')
    x <- NULL
  }
  gg <- tolower(class(data[[eval(VarName)]]))
  if(tolower(VarName) != 'none') {
    if(any(gg %chin% c('numeric', 'integer'))) {
      if(type == 1) {
        x <- sort(decreasing = FALSE, unique(as.numeric(data[, quantile(get(VarName), probs = c(seq(0, 1, 0.05)), na.rm = TRUE)])))
      } else {
        x <- sort(decreasing = TRUE, unique(as.numeric(data[, quantile(get(VarName), probs = c(seq(0, 1, 0.05)), na.rm = TRUE)])))
      }
    } else if(any(gg %chin% c('factor','character','date','idate','posixct'))) {
      if(type == 1) {
        if(lubridate::is.Date(data[[eval(VarName)]])) {
          x <- as.Date(sort(decreasing = FALSE, data[, unique(get(VarName))]))
        } else {
          x <- sort(decreasing = FALSE, data[, unique(get(VarName))])
        }
      } else {
        if(lubridate::is.Date(data[[eval(VarName)]])) {
          x <- as.Date(sort(decreasing = TRUE, data[, unique(get(VarName))]))
        } else {
          x <- sort(decreasing = TRUE, data[, unique(get(VarName))])
        }
      }
    } else {
      x <- NULL
    }
  } else {
    x <- NULL
  }
  x
}

#' @title FilterLogicData
#'
#' @param data1 data.table
#' @param FilterLogic passthrough
#' @param FilterVariable passthrough
#' @param FilterValue passthrough
#' @param FilterValue2 passthrough
#' @param Debug passthrough
#'
#' @export
FilterLogicData <- function(data1, FilterLogic = NULL, FilterVariable = NULL, FilterValue = NULL, FilterValue2 = NULL, Debug = FALSE) {

  if(Debug) {
    print(FilterLogic)
    print(FilterVariable)
    print(FilterValue)
    print(FilterValue2)
  }

  if(missing(data1)) {
    return(NULL)
  }

  if(length(FilterVariable) != 0 && FilterVariable %in% names(data1)) {
    if(any(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('factor', 'character')) || FilterLogic %in% c('%in%', '%chin%', '%like')) {

      # Debug
      if(Debug) {
        print('FilterLogicData else if 1')
        print(any(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('factor', 'character')))
      }

      # Factor & Character Subsetting
      if(FilterLogic %in% c('%in%','%chin%')) {
        data1 <- data1[get(FilterVariable) %in% c(eval(FilterValue))]
      } else if(FilterLogic == '%like%') {
        data1 <- data1[get(eval(FilterVariable)) %like% c(eval(FilterValue))]
      }

    } else if(any(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('numeric', 'integer', 'date', 'posix'))) {

      # Debug
      if(Debug) {
        print('FilterLogicData else if 2')
        print(tolower(class(data1[[eval(FilterVariable)]])) %chin% c('numeric', 'integer', 'date', 'posix'))
      }

      # Other types
      if(FilterLogic == '>') {
        if(length(FilterValue) > 0L) {
          data1 <- data1[get(FilterVariable) > eval(FilterValue)]
        } else {
          data1 <- data1[get(FilterVariable) > eval(FilterValue2)]
        }
      } else if(FilterLogic == '>=') {
        if(length(FilterValue) > 0L) {
          data1 <- data1[get(FilterVariable) >= eval(FilterValue)]
        } else {
          data1 <- data1[get(FilterVariable) >= eval(FilterValue2)]
        }
      } else if(FilterLogic == '<') {
        if(length(FilterValue) > 0L) {
          data1 <- data1[get(FilterVariable) < eval(FilterValue)]
        } else {
          data1 <- data1[get(FilterVariable) < eval(FilterValue2)]
        }
      } else if(FilterLogic == '<=') {
        if(length(FilterValue) > 0L) {
          data1 <- data1[get(FilterVariable) <= eval(FilterValue)]
        } else {
          data1 <- data1[get(FilterVariable) <= eval(FilterValue2)]
        }
      } else if(FilterLogic == '%between%') {
        if(Debug) print('At %between% section')
        if(Debug) print(as.numeric(FilterVariable))
        if(Debug) print(as.numeric(FilterValue))
        if(Debug) print(as.numeric(FilterValue2))
        if(Debug) print(data1)
        if(Debug) print('Run data.table operation')
        data1 <- data1[get(FilterVariable) >= eval(FilterValue) & get(FilterVariable) <= eval(FilterValue2)]
        if(Debug) print('Done with data.table operation')
        if(Debug) print(data1)
      } else if(FilterLogic == 'not %between%') {
        data1 <- data1[get(FilterVariable) < eval(FilterValue) | get(FilterVariable) > eval(FilterValue2)]
      } else {
        data1 <- data1[get(FilterVariable) <= eval(FilterValue2)]
      }
    }
  }
  data1
}

#' @title KeyVarsInit
#'
#' @param data data.table
#' @param VarName Variable name
#' @param type 1 for min, 2 for max
#'
#' @export
KeyVarsInit <- function(data, VarName = NULL, type = 1) {

  # Return of data is missing altogether
  if(missing(data)) {
    print('KeyVarsInit: data is missing')
    return(list(MinVal = NULL, MaxVal = NULL, ChoiceInput = NULL))
  }

  # Run VarName through function to NULL it out if any issues
  VarName <- DataMuse:::CEPP(VarName, Default = NULL)

  # Return if VarName is NULL or if some sort of character(0) slipped through the cracks
  if(length(VarName) == 0) {
    print('KeyVarsInit: VarName is length 0')
    return(list(MinVal = NULL, MaxVal = NULL, ChoiceInput = NULL))
  }

  # If data doesnt have VarName in it, the next check will be ModelData
  if(!VarName %in% names(data)) {
    minn <- NULL
    maxx <- NULL
    choices <- NULL
    return(list(MinVal = minn, MaxVal = maxx, ChoiceInput = choices))
  }

  # If VarName is set to 'None' then just return NULLs for list values
  if(tolower(VarName) == 'none') {
    minn <- NULL
    maxx <- NULL
    choices <- NULL
    return(list(MinVal = minn, MaxVal = maxx, ChoiceInput = choices))
  }

  # If VarName is
  if(any(class(data[[eval(VarName)]]) %chin% c('numeric','integer','double','float'))) {
    minn <- tryCatch({floor(data[, min(get(VarName), na.rm = TRUE)])}, error = function(x) NULL)
    maxx <- tryCatch({ceiling(data[, max(get(VarName), na.rm = TRUE)])}, error = function(x) NULL)
    UData <- tryCatch({sort(data[, unique(get(VarName))])}, error = function(x) NULL)
    if(!is.null(UData) && length(UData) <= 10L) {
      choices <- UData
    } else {
      choices <- tryCatch({unique(sort(round(as.numeric(data[, quantile(get(VarName), probs = c(seq(0, 1, 0.05)), na.rm = TRUE)]), 5L)))}, error = function(x) {
        tryCatch({UData}, error = NULL)
      })
    }
  } else if(any(tolower(class(data[[(eval(VarName))]])) %chin% c('date','idate','date','posixct','posixt','character','factor'))) {
    choices <- tryCatch({sort(unique(data[[eval(VarName)]]))}, error = function(x) NULL)
    maxx <- tryCatch({data[, max(get(VarName), na.rm = TRUE)]}, error = function(x) NULL)
    minn <- tryCatch({data[, min(get(VarName), na.rm = TRUE)]}, error = function(x) NULL)
  } else {
    minn <- NULL
    maxx <- NULL
    choices <- NULL
  }
  return(list(MinVal = minn, MaxVal = maxx, ChoiceInput = choices))
}

#' @title GetFilterValueLabel
#'
#' @param data data.table
#' @param VarName Variable name
#' @param type 1 for min, 2 for max
#'
#' @export
GetFilterValueLabel <- function(data, VarName = NULL, type = 1) {
  if(missing(data)) {
    print('GetFilterValueLabel(): data was missing')
    x <- 'N/A'
  } else if(is.null(data)) {
    print('GetFilterValueLabel(): data was NULL')
    x <- 'N/A'
  } else if(length(VarName) == 0) {
    print('GetFilterValueLabel(): VarName was length 0')
    x <- 'N/A'
  } else if(tolower(VarName) != 'none') {
    if(any(tolower(class(data[[eval(VarName)]])) %chin% c('numeric', 'integer', 'float', 'double', 'date', 'idate', 'posixct'))) {
      if(type == 1) x <- 'Min Value' else x <- 'Max Value'
    }  else {
      x <- 'Select Levels'
    }
  } else {
    x <- 'N/A'
  }
  x
}

#' @title GetFilterValueMultiple
#'
#' @param data data.table
#' @param VarName Variable name
#' @param type 1 for min, 2 for max
#'
#' @export
GetFilterValueMultiple <- function(data, VarName = NULL, type = 1) {
  if(missing(data)) {
    print('GetFilterValueMultiple(): data was missing')
    x <- FALSE
  } else if(is.null(data)) {
    print('GetFilterValueMultiple(): data was NULL')
    x <- FALSE
  } else if(length(VarName) == 0) {
    print('GetFilterValueMultiple(): VarName was length 0')
    x <- FALSE
  } else if(tolower(VarName) != 'none') {
    if(any(tolower(class(data[[eval(VarName)]])) %in% c('character', 'factor', 'date', 'idate', 'posixct'))) {
      x <- TRUE
    } else {
      x <- FALSE
    }
  } else {
    x <- FALSE
  }
  x
}

#' @title CharNull
#'
#' @param x Value
#'
#' @export
CharNull <- function(x, Char = FALSE) {

  if(missing(x)) {
    print('CharNull: missing x')
    return(NULL)
  }

  if(!exists('x')) {
    print('CharNull: x does not exist')
    return(NULL)
  }

  if(length(x) == 0) {
    print('CharNull: length(x) == 0')
    return(NULL)
  }

  if(all(is.na(suppressWarnings(as.character(x))))) {

    return(NULL)

  } else if(any(is.na(suppressWarnings(as.character(x)))) && length(x) > 1) {

    x <- x[!is.na(x)]
    x <- suppressWarnings(as.character(x))
    return(x)

  } else if(any(is.na(suppressWarnings(as.character(x)))) && length(x) == 1) {

    return(NULL)

  } else {

    x <- suppressWarnings(as.character(x))
    return(x)

  }

  if(!Char) {
    return(NULL)
  } else {
    return("NULL")
  }
}

#' @title NumNull
#'
#' @param x value
#'
#' @export
NumNull <- function(x, Char = FALSE) {

  if(missing(x)) {
    print('NumNull: missing x')
    return(NULL)
  }

  if(!exists('x')) {
    print('NumNull: x does not exist')
    return(NULL)
  }

  if(length(x) == 0) {
    print('NumNull: length(x) == 0')
  }

  if(all(is.na(suppressWarnings(as.numeric(x))))) {

    return(NULL)

  } else if(any(is.na(suppressWarnings(as.numeric(x)))) && length(x) > 1) {

    x <- x[!is.na(x)]
    x <- suppressWarnings(as.numeric(x))
    return(x)

  } else if(any(is.na(suppressWarnings(as.numeric(x)))) && length(x) == 1) {

    return(NULL)

  } else {

    x <- suppressWarnings(as.numeric(x))
    return(x)

  }

  if(!Char) {
    return(NULL)
  } else {
    return("NULL")
  }
}

#' @title IntNull
#'
#' @param x value
#'
#' @export
IntNull <- function(x, Char = FALSE) {

  if(missing(x)) {
    print('IntNull: missing x')
    return(NULL)
  }

  if(!exists('x')) {
    print('IntNull: x does not exist')
    return(NULL)
  }

  if(length(x) == 0) {
    print('IntNull: length(x) == 0')
  }

  if(all(is.na(suppressWarnings(as.integer(x))))) {

    return(NULL)

  } else if(any(is.na(suppressWarnings(as.integer(x)))) && length(x) > 1) {

    x <- x[!is.na(x)]
    x <- suppressWarnings(as.integer(x))
    return(x)

  } else if(any(is.na(suppressWarnings(as.integer(x)))) && length(x) == 1) {

    return(NULL)

  } else {

    x <- suppressWarnings(as.integer(x))
    return(x)

  }

  if(!Char) {
    return(NULL)
  } else {
    return("NULL")
  }
}

#' @title BlankRow
#'
#' @description BlankRow add blank row with width w
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param W width of column
#'
#' @examples
#' \dontrun{
#' DataMuse:::BlankRow(12)
#' }
#' @return Adds a row to your UI of width W
#' @export
BlankRow <- function(W = 12) {
  shiny::fluidRow(shiny::column(width = W, htmltools::tags$br()))
}

#' @title BlankLine
#'
#' @description BlankRow add blank row with width w
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param W width of column
#'
#' @examples
#' \dontrun{
#' DataMuse:::BlankLine(12)
#' }
#' @return Adds a row to your UI of width W
#' @export
BlankLine <- function(w) shiny::fluidRow(shiny::column(w, shiny::hr()))

#' @title Save VarName values within a project
#'
#' @description Automatically save VarNames to project list
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param xx This is the input value within a Shiny context; input$value or better yet, tryCatch({input$value}, error = function(x) NULL)
#' @param VarName The name of the argument you want to store
#' @param Type 'character' 'numeric' 'logical' 'date'
#' @param Default default value
#' @param Switch = FALSE
#' @param Debug = FALSE
#'
#' @examples
#' \dontrun{
#' Aggregate <- DataMuse:::ReturnParam(input, VarName = "TS_AggregateFunction", Type = "character", Default = "mean")
#' }
#'
#' @return Updates ProjectList inside function
#' @export
ReturnParam <- function(xx = NULL,
                        VarName = NULL,
                        Type = 'numeric',
                        Default = NULL,
                        Switch = TRUE,
                        Debug = FALSE) {

  if(length(xx) > 0L && Type == 'logical' && xx %in% c("0","1")) {
    if(Debug) print("logical returned as '0' or '1' from updateSelectizeInput")
    if(xx == "0") return(FALSE)
    if(xx == "1") return(TRUE)
  }

  # Catches NULL, character(0), numeric(0), ...
  if(length(xx) == 0) {
    if(Debug) print('ReturnParam: length(xx) == 0 -> TRUE')
    return(Default)
  }

  # NA's
  if(all(is.na(xx))) {
    if(Debug) print('ReturnParam: all(is.na(xx)) -> TRUE')
    return(Default)
  }
  if(any(is.na(xx))) {
    if(Debug) print('ReturnParam: any(is.na(xx)) -> TRUE')
    xx <- xx[!is.na(xx)]
    if(Debug) {print(xx); print('ReturnParam: any(is.na(xx)) -> TRUE END')}
  }

  # NaN's
  if(all(is.nan(xx))) {
    if(Debug) print('ReturnParam: all(is.nan(xx)) -> TRUE')
    return(Default)
  }
  if(any(is.nan(xx))) {
    if(Debug) print('ReturnParam: any(is.nan(xx)) -> TRUE')
    xx <- xx[!is.nan(xx)]
    if(Debug) {print(xx); print('ReturnParam: any(is.nan(xx)) -> TRUE END')}
  }

  # ""
  if(all(xx %in% "")) {
    if(Debug) {print("all(xx %in% '')")}
    return(Default)
  }
  if(any(xx %in% "")) {
    if(Debug) print("any(xx %in% '') -> TRUE")
    xx <- xx[!xx %in% ""]
    if(Debug) {print(xx); print("any(xx %in% '') -> TRUE END")}
    return(Default)
  }

  # 'No Data Loaded'
  if(all(xx %in% 'No Data Loaded')) {
    if(Debug) print("all(xx %in% 'No Data Loaded') -> TRUE")
    return(Default)
  }
  if(any(xx %in% 'No Data Loaded')) {
    if(Debug) print("any(xx %in% 'No Data Loaded') -> TRUE")
    xx <- xx[!xx %in% 'No Data Loaded']
    if(Debug) {print(xx); print("any(xx %in% 'No Data Loaded') -> TRUE END")}
    return(Default)
  }


  # Get Value then apply type casting
  if(!all(xx %in% c('None', 'Default'))) {
    if(any(xx %in% c('None', 'Default'))) xx <- xx[!xx %in% c('None','Default')]
    Value <- xx
  } else {
    Value <- Default
  }

  # Type Casting
  if(Type == 'numeric') {
    return(as.numeric(Value))
  } else if(Type == 'character') {
    return(as.character(Value))
  } else if(Type == 'logical') {
    return(as.logical(Value))
  } else if(Type == 'factor') {
    return(as.factor(Value))
  } else if(Type == 'date') {
    return(as.Date(Value))
  } else if(Type == 'posix') {
    return(as.POSIXct(Value))
  }
}

#' @title GenerateEvaluationMetrics
#'
#' @description GenerateEvaluationMetrics calculates evaluation metrics for out of sample forecast and evaluation data
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param EvalData Source data in shiny app
#' @param TargetName Target variable name
#' @param GroupNames Group variable names
#' @param DateName Date variable name
#'
#' @examples
#' \dontrun{
#'   PlotData <- DataMuse:::PreparePlotData(input, TargetVariable = "TargetVariables", DateVariable = "DateVariables", GroupVariables = GroupVariables, G1Levels = "TS_Group1Levels", G2Levels = "TS_Group2Levels", G3Levels = "TS_Group3Levels")
#' }
#' @return PreparePlotData object for server.R to
#' @keywords internal
GenerateEvaluationMetrics <- function(EvalData = NULL,
                                      TargetName = NULL,
                                      DateName = NULL,
                                      GroupNames = NULL) {

  # Weekly Rollup ----
  Metrics1 <- data.table::rollup(x = EvalData, j = lapply(.SD, sum), .SDcols = c(eval(TargetName), "Predictions"), by = c(eval(DateName), eval(GroupNames)))
  for(x in GroupNames) Metrics1[, eval(x) := data.table::fifelse(is.na(get(x)), "Total", get(x))]
  Metrics1 <- Metrics1[!is.na(get(DateName))]

  # Add Metrics ----
  Metrics1[, Error := get(TargetName) - Predictions]
  Metrics1[, PercError := data.table::fifelse(get(TargetName) == 0, 12345, Error / get(TargetName))]
  Metrics1[, MAE := abs(get(TargetName) - Predictions)]
  Metrics1[, RMSE := (get(TargetName) - Predictions) ^ 2]
  Metrics1[, MAPE := data.table::fifelse(get(TargetName) == 0, 12345, MAE / get(TargetName))]

  # Remove Date and Final Calcs ----
  if(any(Metrics1$MAPE == 12345)) Metrics1 <- Metrics1[MAPE != 12345]
  FinalMetrics <- Metrics1[, list(Error = mean(Error), MAE = mean(MAE), PercError = mean(PercError), MAPE = mean(MAPE), RMSE = sqrt(mean(RMSE))), by = c(eval(GroupNames))]
  FinalMetrics[, PercError := round(100 * PercError, 3)]
  FinalMetrics[, MAPE := round(100 * MAPE, 3)]
  FinalMetrics[, RMSE := round(RMSE, 1)]
  FinalMetrics[, Error := round(100 * Error, 1)]
  FinalMetrics[, MAE := round(MAE, 1)]

  # Return data
  return(FinalMetrics)
}

#' @title ColumnSubsetDataTable
#'
#' @description ColumnSubsetDataTable will subset data tables by column
#'
#' @family Data Wrangling
#'
#' @author Adrian Antico
#'
#' @param data data.table
#' @param TargetColumnName Target variable
#' @param DateColumnName Date variable
#' @param GroupVars Group variables
#'
#' @noRd
ColumnSubsetDataTable <- function(data,
                                  TargetColumnName = NULL,
                                  DateColumnName = NULL,
                                  GroupVars = NULL) {

  # Check to see if data is actual data----
  if(!(any(class(data) %in% c("data.table","data.frame","tibble")))) {
    return(NULL)
  }

  # Subset----
  data <- data[, .SD, .SDcols = c(eval(TargetColumnName),eval(DateColumnName),eval(GroupVars))]

  # Ensure Date Column is Date----
  if(is.character(data[[eval(DateColumnName)]])) {
    x <- data[1,get(DateColumnName)]
    x1 <- lubridate::guess_formats(x, orders = c("mdY","BdY","Bdy","bdY","bdy","mdy","dby","Ymd","Ydm"))
    data[, eval(DateColumnName) := as.Date(get(DateColumnName), tryFormats = x1)]
  }

  # Return data----
  return(data)
}

#' @title DataDisplayMeta
#'
#' @description DataDisplayMeta
#'
#' @author Adrian Antico
#'
#' @family Data Wrangling
#'
#' @param data Source data
#'
#' @noRd
DataDisplayMeta <- function(data) {

  # Check to see if data is actual data ----
  if(!(any(class(data) %in% c("data.table","data.frame","tibble")))) stop("data is not a data.table")

  # Begin process----
  Counter <- 0L
  N <- data[, .N]
  x <- data.table::data.table(Variable = rep("donotuse", N), Type = rep("donotuse", N))
  for(name in names(data)) {
    Counter <- Counter + 1L
    data.table::set(x, i = Counter, j = "Variable", value = eval(name))
    data.table::set(x, i = Counter, j = "DataType", value = class(data[[eval(name)]]))
  }

  # Return table
  return(x[Variable != "donotuse"])
}

#' @title TimeSeriesMelt
#'
#' @description TimeSeriesMelt
#'
#' @family Data Wrangling
#'
#' @author Adrian Antico
#'
#' @param data source data
#' @param TargetVariable vector of target variable names
#' @param DateVariable Name of date variable
#' @param GroupVariables Vector of group variable names
#'
#' @noRd
TimeSeriesMelt <- function(data,
                           TargetVariable = NULL,
                           DateVariable = NULL,
                           GroupVariables = NULL) {

  # 2 Cases:
  #  Multiple Targets + Grouping Variables
  #  Multiple Targets + No Grouping Variables
  if(length(TargetVariable) > 1) {
    if(!is.null(GroupVariables)) {
      data <- data.table::melt(
        data = data,
        id.vars = c(eval(DateVariable),eval(GroupVariables)),
        measure.vars = eval(TargetVariable),
        variable.name = "GroupVar",
        value.name = "TargetSeries")
    } else {
      data <- data.table::melt(
        data = data,
        id.vars = eval(DateVariable),
        measure.vars = c(eval(TargetVariable)),
        variable.name = "GroupVar",
        value.name = "TargetSeries")
    }
  }

  # Return
  return(data)
}

#' @title Store Args values within a project
#'
#' @description Automatically save arguments to project list
#'
#' @author Adrian Antico
#' @family Shiny
#'
#' @param input This is the input value within a Shiny context
#' @param ProjectList This is the VarNameument collection list
#' @param VarName The name of the VarNameument you want to store
#' @param Type "character" "numeric" "logical"
#' @param Default default value that gets saved
#'
#' @examples
#' \dontrun{
#' StoreVarNames(input, ProjectList, "NTrees", "numeric", 1000)
#' }
#'
#' @return Updates ProjectList inside function. Do not assign function to anything
#' @noRd
StoreArgs <- function(input,
                      ProjectList,
                      VarName,
                      Type,
                      Default) {
  if(Type == "character") {
    tryCatch({if(any(class(input[[VarName]]) != "NULL")) {
      ProjectList[[VarName]] <<- as.character(input[[VarName]])
    } else {
      ProjectList[[VarName]] <<- Default
    }}, error = function(x) Default)

  } else if(Type == "numeric") {
    tryCatch({if(any(class(input[[VarName]]) != "NULL")) {
      ProjectList[[VarName]] <<- as.numeric(input[[VarName]])
    } else {
      ProjectList[[VarName]] <<- Default
    }}, error = function(x) Default)

  } else if(Type == "logical") {
    tryCatch({if(any(class(input[[VarName]]) != "NULL")) {
      ProjectList[[VarName]] <<- as.logical(input[[VarName]])
    } else {
      ProjectList[[VarName]] <<- Default
    }}, error = function(x) Default)
  }
}

#' Assign a data.table by name from an environment
#'
#' @param data character, name of the object
#' @param env an environment
#'
#' @return the object
#' @noRd
AssignData <- function(data, env = globalenv()) {
  if(deparse(substitute(data)) %in% ls(name = env)) {
    get(x = data, envir = env)
  } else {
    NULL
  }
}
