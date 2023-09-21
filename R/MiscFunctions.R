#' @title DeleteFile
#'
#' @description DeleteFile will prompt you for a file to delete and then permanently delete a file. You won't have to go the the recycle bin to delete it a second time
#'
#' @family System Functions
#'
#' @author Adrian Antico
#'
#' @param File If NULL a prompt will allow you to click on the file to have it removed. Otherwise, supply a path to the file including its name and extension
#' @noRd
DeleteFile <- function(File = NULL) {
  if(is.null(File)) {
    shell(paste0("del ", file.choose()))
  } else {
    shell(paste0("del ", File))
  }
}

#' @title Logger
#'
#' @description Logging errors and warnings from repeated calls to a function
#'
#' @author Adrian Antico
#'
#' @family Misc
#'
#' @param x Function to call repeatedly
#'
#' @examples
#' \dontrun{
#' Output <- lapply(1:10, FUN = Logger(PrintToPDF))
#' }
#'
#' @noRd
Logger <- function(x) {
  function(...) {
    warn <- err <- NULL
    res <- withCallingHandlers(
      tryCatch(x(...), error = function(e) {
        err <<- conditionMessage(e)
        NULL
      }), warning = function(w) {
        warn <<- append(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    list(res, warn = warn, err = err)
  }
}

#' @title BuildBinary
#'
#' @description Build package binary
#'
#' @author Adrian Antico
#'
#' @family Utilities
#'
#' @param Root NULL will setwd to project root as defined in function
#'
#' @export
BuildBinary <- function(Root = NULL) {
  x <- getwd()
  if(!is.null(Root)) {
    setwd(Root)
    devtools::install(pkg = "Quantico", dependencies = FALSE)
  } else {
    setwd("C:/Users/Bizon/Documents/GitHub")
    devtools::build(pkg = "Quantico")
  }
  setwd(x)
}

#' @title Install
#'
#' @description To install the package
#'
#' @author Adrian Antico
#'
#' @family Utilities
#'
#' @param Root NULL will setwd to project root as defined in function
#'
#' @export
Install <- function(Root = NULL) {
  x <- getwd()
  if(!is.null(Root)) {
    setwd(Root)
    devtools::install(pkg = "Quantico", dependencies = FALSE)
  } else {
    setwd("C:/Users/Bizon/Documents/GitHub")
    devtools::install(pkg = "Quantico", dependencies = FALSE)
  }
  setwd(x)
}

#' @title UpdateDocs
#'
#' @description Update helf files and reference manual
#'
#' @author Adrian Antico
#'
#' @family Utilities
#'
#' @export
UpdateDocs <- function(BuildVignette = FALSE, Root = NULL) {
  x <- getwd()
  if(!is.null(Root)) {
    setwd(Root)
    devtools::document()
    if(BuildVignette) devtools::build_manual()
  } else {
    setwd("C:/Users/Bizon/Documents/GitHub/Quantico")
    devtools::document()
    if(BuildVignette) devtools::build_manual()
  }
  setwd(x)
}

#' @title ParseOptParse
#' @param x object value from optparse. e.g. ArgsList$TargetVariables
#' @noRd
ParseOptParse <- function(x) {
  if(!is.null(x)) {
    return(as.character(if(is.list(strsplit(x, ","))) unlist(strsplit(x, ",")) else x))
  } else {
    return(x)
  }
}

#' @title ColTypes
#' @param data Source data.table
#' @noRd
ColTypes <- function(data) {
  CT <- c()
  for(Col in names(data)) CT <- c(CT, class(data[[Col]])[[1L]])
  CT
}

#' @title LoadAssign
#'
#' @description LoadAssign will assign the loaded object to a new object. xx <- LoadAssign`(FilePath)
#'
#' @author Adrian Antico
#' @family Utilities
#'
#' @param FilePath
#'
#' @noRd
LoadAssign <- function(FilePath) {
  load(FilePath, envir = .GlobalEnv)
  get(ls()[ls() != "FilePath"])
}

#' @title ColNames
#'
#' @family Misc
#' @author Adrian Antico
#'
#' @param data source data.table
#' @param Types 'all' (basically names(data)), 'numeric', 'character', 'factor', 'logical', 'posix', 'date', 'integer64'
#'
#' @noRd
ColNameFilter <- function(data, Types = 'all') {
  # Types <- unique(CT)
  # Types <- "integer"
  if(Types == "all") return(names(data))
  nam <- c()
  for(t in Types) {
    if(tolower(t) == "numeric") {
      nam <- NumericColNames(data)
    } else if(tolower(t) == "integer") {
      nam <- IntegerColNames(data)
    } else if(tolower(t) == "character") {
      nam <- CharacterColNames(data)
    } else if(tolower(t) == "factor") {
      nam <- FactorColNames(data)
    } else if(tolower(t) == "logical") {
      nam <- LogicalColNames(data)
    } else if(tolower(t) == "date") {
      nam <- DateColNames(data)
    } else if(tolower(t) == "idate") {
      nam <- IDateColNames(data)
    } else if(tolower(t) == "idatetime") {
      nam <- IDateTimeColNames(data)
    } else if(tolower(t) == "posixct") {
      nam <- POSIXColNames(data)
    }
  }
  return(nam)
}

#' @noRd
NumericColNames <- function(data) {
  x <- list()
  for(z in names(data)) {
    xx <- class(data[[z]])[1L]
    if(xx %in% "numeric") x[[length(x) + 1]] <- z
  }
  if(!identical(x, character(0))) return(x) else return(NULL)
}

#' @noRd
IntegerColNames <- function(data) {
  x <- list()
  for(z in names(data)) {
    xx <- class(data[[z]])[1L]
    if(xx %in% "integer") x[[length(x) + 1]] <- z
  }
  if(length(x) > 0L) return(x) else return(NULL)
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
    if(tolower(class(data[[i]])[1L]) == "date") {
      counter <- counter + 1L
      x[[counter]] <- i
    }
  }
  if(length(x) > 0L) return(x) else return(NULL)
}

#' @noRd
POSIXColNames <- function(data) {
  x <- list()
  counter <- 0L
  for(i in names(data)) {
    if(tolower(class(data[[i]])[1L]) == "posixct") {
      counter <- counter + 1L
      x[[counter]] <- i
    }
  }
  if(length(x) > 0L) return(x) else return(NULL)
}

#' @noRd
IDateColNames <- function(data) {
  x <- list()
  counter <- 0L
  for(i in names(data)) {
    if(tolower(class(data[[i]])[1L]) == "idate") {
      counter <- counter + 1L
      x[[counter]] <- i
    }
  }
  if(length(x) > 0L) return(x) else return(NULL)
}

#' @noRd
IDateTimeColNames <- function(data) {
  x <- list()
  counter <- 0L
  for(i in names(data)) {
    if(tolower(class(data[[i]])[1L]) == "idatetime") {
      counter <- counter + 1L
      x[[counter]] <- i
    }
  }
  if(length(x) > 0L) return(x) else return(NULL)
}

#' @noRd
Names2Vector <- function(xx) {
  for(gg in seq_along(names(xx))) if(gg == 1L) cat(paste0("c('",names(xx)[gg], "',\n", collapse = "")) else if(gg != max(seq_along(names(xx)))) cat(paste0("  '", names(xx)[gg], "',\n")) else cat(paste0("  '", names(xx)[gg], "')\n"))
}

#' @noRd
DeleteVariablesServer <- function(session,id,DataList) {
  ns <- NS(id)
  shiny::observeEvent(input$DeleteVariablesInputs, {
    print('Delete Variables Inputs Dropdown')
    Quantico::SelectizeInput(session = session, Update = TRUE, InputID='DeleteVariables_SelectData', Label='Choose data set', Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = names(DataList)[1L], CloseAfterSelect = FALSE)
    dt <- shiny::reactive({shiny::req(tryCatch({DataList[[input$DeleteVariables_SelectData]][['colnames']]}, error = function(x) DataList[[1L]]))})
    Quantico::SelectizeInput(InputID='DeleteVariables', Label=tags$span(style=paste0('color: ', AppTextColor, ';'),'Delete Columns'), Choices = dt(), Multiple = TRUE, MaxVars = 1000, CloseAfterSelect = FALSE)
  })
}
