#' @title Lists4JS
#'
#' @description Run a bash script in linux
#'
#' @author Adrian Antico
#' @family Utils
#'
#' @param x typical named R list
#'
#' @noRd
Lists4JS <- function(l) {
  nameMe <- function(x) {if(is.null(names(x))) names(x) <- character(length(x)); return(x)}
  out <- lapply(l, function(val) {
    if(is.list(val)) {
      Lists4JS(val)
    } else if(length(val) == 1 && is.null(names(val))) {
      val
    } else {
      nameMe(as.list(val))
    }
  })
  return(nameMe(out))
}

#' @title LinesCounter
#'
#' @description Run a bash script in linux
#'
#' @author Adrian Antico
#' @family Utils
#'
#' @param pattern String to count up. Defaults to new line
#'
#' @export
Utils.Code.Lines <- function(code, pattern = "\n", ...) {
  vapply(regmatches(code, gregexpr(pattern, code, ...)), length, 1L)
}

#' @title Utils.LinuxRunSH
#'
#' @description Run a bash script in linux
#'
#' @author Adrian Antico
#' @family Utils
#'
#' @param PathName Path and the file name with .sh extension
#'
#' @export
Utils.LinuxRunSH <- function(PathName) {
  system(command = paste0("chmod u+x ", file.path(PathName)))
}


#' @title Utils.Nest
#'
#' @description Creates nested categorical columns and also returns the column names to use
#'
#' @author Adrian Antico
#' @family Utils
#'
#' @param CacheDir Path to folder
#' @param CacheName Name of file
#'
#' @export
Utils.Nest <- function(data = NULL,GroupVariables = NULL) {
  Vars <- c()
  Vars <- c(Vars, GroupVariables[1L])
  # gg = 1 # gg = 2
  for(gg in seq_along(GroupVariables)[-1]) {
    Vars <- c(Vars, paste0(Vars[length(Vars)], "_", GroupVariables)[gg])
    data[, eval(Vars[gg]) := paste0(get(Vars[gg-1L]), "_", get(GroupVariables[gg]))]
  }
  return(list(
    data = data,
    NestVars = Vars
  ))
}

#' @title Shiny.Utils.CachePath
#'
#' @description Combines a directory path with a file name and extension
#'
#' @author Adrian Antico
#' @family Utils
#'
#' @param CacheDir Path to folder
#' @param CacheName Name of file
#'
#' @keywords internal
Shiny.Utils.CachePath <- function(CacheName, CacheDir, Ext = '.csv') {
  file.path(CacheDir, paste0(gsub(Ext,'',CacheName),Ext))
}

#' # text & logical with NULL default
#' @noRd
CEP <- function(x) if(any(missing(x))) 'NULL' else if(!exists('x')) 'NULL' else if(is.null(x)) "NULL" else if(identical(x, character(0))) "NULL" else if(identical(x, numeric(0))) "NULL" else if(identical(x, integer(0))) "NULL" else if(identical(x, logical(0))) "NULL" else if(any(x == "")) "NULL" else if(any(is.na(x))) "NULL" else if(any(x == 'None')) "NULL" else if(is.numeric(x)) x else if(length(x) > 1) paste0("c(", noquote(paste0("'", x, "'", collapse = ',')), ")") else paste0("'", x, "'")

#' # number and logical with FALSE / TRUE default
#' @noRd
CEPP <- function(x, Default = NULL, Type = 'character') if(missing(x)) 'NULL' else if(!exists('x')) 'NULL' else if(length(x) == 0) 'NULL' else if(any(is.na(x))) 'NULL' else if(all(x == "")) 'NULL' else if(Type == 'numeric') Quantico:::NumNull(x) else if(Type == 'character') Quantico:::CharNull(x)

#' @title ExpandText
#'
#' @description This function is for pasting character vector arguments into their respective parameter slots for code printing (and command line vector argument passing)
#'
#'
#' @keywords internal
ExpandText <- function(x) {
  if(length(x) > 0L) {
    if(is.character(x) || is.factor(x) || lubridate::is.Date(x) || lubridate::is.POSIXct(x)) {
      return(paste0("c('", paste0(x, collapse = "','"), "')"))
    } else if(is.numeric(x) || is.logical(x)) {
      return(paste0("c(", paste0(x, collapse = ","), ")"))
    }
  } else {
    return('NULL')
  }
}

#' @noRd
Shiny.Utils.underscore_removal <- function(x) {
  for(i in 7L:ncol(x)) {
    nam <- names(x)[i]
    if(length(nam) > 0L && grep(pattern = '_', x = nam)) {
      data.table::setnames(x, old = nam, gsub(pattern = '_', replacement = ' ', x = nam), skip_absent = TRUE)
    }
  }
  return(x)
}

#' @title Shiny.Utils.ColumnTypes
#'
#' @param data data.table
#'
#' @return Two column data.table with colnames and classes
#'
#' @export
Shiny.Utils.ColumnTypes <- function(data) {
  x <- data.table::as.data.table(sapply(data, FUN = function(x) class(x)[1]), keep.rownames = TRUE)
  xxx <- data.table::transpose(x[, 'V2'])
  return(data.table::setnames(x = xxx, old = names(xxx), x$V1))
}

#' @title DataTable
#'
#' @description Fully loaded DT::datatable() with args prefilled
#'
#' @author Adrian Antico
#' @family Utils
#'
#' @param x vector
#'
#' @export
CleanVector <- function(x) {
  if(missing(x)) return(NULL)
  x <- x[!x %in% c(""," ")]
  x <- x[!is.na(x)]
  xx <- any(x %in% c(logical(0),character(0),integer(0),numeric(0)))
  if(length(x) == 1 && xx) return(NULL)
  if(length(x) > 1 && xx) {
    x <- x[!x %in% c(logical(0),character(0),integer(0),numeric(0))]
    if(length(x) == 1 && any(x %in% c(logical(0),character(0),integer(0),numeric(0)))) {
      return(NULL)
    }
  }
  return(x)
}

#' @title IntraSessionDefaults
#'
#' @description Default generator
#'
#' @author Adrian Antico
#' @family Utils
#'
#' @param List named list of args where the args can be many
#' @param InputName 'Plot1_SelectData' in input$Plot1_SelectData
#' @param ArgName 'SelectedDefault' in List[[InputName]][[Default]]
#' @param Default default value to assign
#' @param Debug Logical
#'
#' @export
IntraSessionDefaults <- function(List = NULL,
                                 InputName = 'Plot1_SelectData',
                                 ArgName = 'SelectedDefault',
                                 Default = NULL,
                                 Debug = Debug) {

  # Select last item from history or use Default
  x <- tryCatch({List[[InputName]][[ArgName]]}, error = function(x) NULL)
  if(length(x) == 0L) {
    if(Debug) print('IntraSessionDefaults: length(List[[InputName]][[ArgName]]) == 0L')
    selected_default <- Default
  } else {
    if(Debug) print('IntraSessionDefaults: length(List[[InputName]][[ArgName]]) != 0L')
    selected_default <- x[[length(x)]]
  }

  # Return Default Value
  return(selected_default)
}

#' @title Shiny.CodePrint.Collect
#'
#' @description Collect code
#'
#' @author Adrian Antico
#' @family Utils
#'
#' @param x code being collected
#' @param y collection list to append to
#' @param Debug passthrough
#'
#' @export
Shiny.CodePrint.Collect <- function(y,x) {
  options(digits.secs = 6)
  if(missing(y) || length(y) == 0L) y <- list()
  y[['TimeStamp']] <- c(y[['TimeStamp']], Sys.time())
  y[['Code']] <- c(y[['Code']], x)
  return(y)
}

#' @title Shiny.CodePrint.OrganizeCode
#'
#' @description Organizes code
#'
#' @author Adrian Antico
#' @family Utils
#'
#' @param DM = NULL
#' @param DW = NULL
#' @param FE = NULL
#' @param ML = NULL
#' @param FC = NULL
#' @param PL = NULL
#'
#' @examples
#' \dontrun{
#' # Step through
#' # #listname in c('DataMgtCode','DataWranglingCode','FeatureEngineeringCode','MachineLearningCode','PlottingCode')
#' # listname <- 'DataMgtCode'
#' # DataMgtCode <- list()
#' # DataMgtCode[[paste0(Sys.time())]] <- 'data.table::data.table(A = 1:12)'
#' # DataMgtCode[[paste0(Sys.time())]] <- 'data.table::data.table(B = 1:12)'
#' # DataWranglingCode <- list()
#' # DataWranglingCode[[paste0(Sys.time())]] <- 'data.table::data.table(C = 1:12)'
#' # DataWranglingCode[[paste0(Sys.time())]] <- 'data.table::data.table(D = 1:12)'
#' # MasterSet <- data.table::data.table(TimeStamp = Sys.time(), Code = 'Sys.time() # Lists intialization')
#' }
#'
#' @export
Shiny.CodePrint.OrganizeCode <- function(DM = NULL,
                                         DW = NULL,
                                         FE = NULL,
                                         ML = NULL,
                                         FC = NULL,
                                         PL = NULL) {

  # Create data.table with 3 columns:
  #   TimeStamp, Code, Type
  #   Timestamp is for the time the code was run
  #   Code is the code that was run at the associated TimeStamp
  #   Type is the name of the list the code came from (functional area of app)
  # Sorting by TimeStamp only will work because of the order of arrival for
  #   the code that comes in. Sorting will not rearrange for ties unless we
  #   specify Type as well and then it could rearrange but that would mess up
  #   things.
  if(length(DM) > 0L) {
    if((xx <- length(DM$TimeStamp)) != (yy <- length(DM$Code))) {
      for(xxx in seq_len(as.numeric(abs(xx-yy)))) DM$TimeStamp <- c(DM$TimeStamp, DM$TimeStamp[length(DM$TimeStamp)]+xxx)
    }
    MasterSet <- tryCatch({data.table::data.table(TimeStamp = DM$TimeStamp, Code = DM$Code)}, error = function(x) NULL)
    if(length(MasterSet) > 0L) {
      MasterSet[, Type := 'DM']
      data.table::setorderv(MasterSet, cols = 'TimeStamp', order = 1)
    }
  }
  if(length(DW) > 0L) {
    if(exists('MasterSet') && length(MasterSet) > 0L) {
      if((xx <- length(DW$TimeStamp)) != (yy <- length(DW$Code))) {
        for(xxx in seq_len(as.numeric(abs(xx-yy)))) DW$TimeStamp <- c(DW$TimeStamp, DW$TimeStamp[length(DW$TimeStamp)]+xxx)
      }
      temp <- tryCatch({data.table::data.table(TimeStamp = DW$TimeStamp, Code = DW$Code)}, error = function(x) NULL)
      if(length(temp) > 0L) {
        temp[, Type := 'DW']
        MasterSet <- data.table::rbindlist(list(MasterSet, temp))
      }
    } else {
      if((xx <- length(DW$TimeStamp)) != (yy <- length(DW$Code))) {
        for(xxx in seq_len(as.numeric(abs(xx-yy)))) DW$TimeStamp <- c(DW$TimeStamp, DW$TimeStamp[length(DW$TimeStamp)]+xxx)
      }
      MasterSet <- tryCatch({data.table::data.table(TimeStamp = DW$TimeStamp, Code = DW$Code)}, error = function(x) NULL)
      if(length(MasterSet) > 0L) {
        MasterSet[, Type := 'DW']
      }
    }
  }
  if(length(FE) > 0L) {
    print('FE 2')
    if(exists('MasterSet') && length(MasterSet) > 0L) {
      if((xx <- length(FE$TimeStamp)) != (yy <- length(FE$Code))) {
        for(xxx in seq_len(as.numeric(abs(xx-yy)))) FE$TimeStamp <- c(FE$TimeStamp, FE$TimeStamp[length(FE$TimeStamp)]+xxx)
      }
      print('FE a1')
      temp <- tryCatch({data.table::data.table(TimeStamp = FE$TimeStamp, Code = FE$Code)}, error = function(x) NULL)
      print('FE b1')
      if(length(temp) > 0L) {
        print('FE c1')
        temp[, Type := 'FE']
        MasterSet <- data.table::rbindlist(list(MasterSet, temp))
        print(MasterSet)
      }
    } else {
      if((xx <- length(FE$TimeStamp)) != (yy <- length(FE$Code))) {
        for(xxx in seq_len(as.numeric(abs(xx-yy)))) FE$TimeStamp <- c(FE$TimeStamp, FE$TimeStamp[length(FE$TimeStamp)]+xxx)
      }
      print('FE a2')
      MasterSet <- tryCatch({data.table::data.table(TimeStamp = FE$TimeStamp, Code = FE$Code)}, error = function(x) NULL)
      print('FE b1')
      if(length(MasterSet) > 0L) {
        print('FE c1')
        MasterSet[, Type := 'FE']
        print(MasterSet)
      }
    }
  }
  if(length(ML) > 0L) {
    if(exists('MasterSet') && length(MasterSet) > 0L) {
      if((xx <- length(ML$TimeStamp)) != (yy <- length(ML$Code))) {
        for(xxx in seq_len(as.numeric(abs(xx-yy)))) ML$TimeStamp <- c(ML$TimeStamp, ML$TimeStamp[length(ML$TimeStamp)]+xxx)
      }
      temp <- tryCatch({data.table::data.table(TimeStamp = ML$TimeStamp, Code = ML$Code)}, error = function(x) NULL)
      if(length(temp) > 0L) {
        temp[, Type := 'ML']
        MasterSet <- data.table::rbindlist(list(MasterSet, temp))
      }
    } else {
      if((xx <- length(ML$TimeStamp)) != (yy <- length(ML$Code))) {
        for(xxx in seq_len(as.numeric(abs(xx-yy)))) ML$TimeStamp <- c(ML$TimeStamp, ML$TimeStamp[length(ML$TimeStamp)]+xxx)
      }
      MasterSet <- tryCatch({data.table::data.table(TimeStamp = ML$TimeStamp, Code = ML$Code)}, error = function(x) NULL)
      if(length(MasterSet) > 0L) {
        MasterSet[, Type := 'ML']
      }
    }
  }
  if(length(FC) > 0L) {
    if(exists('MasterSet') && length(MasterSet) > 0L) {
      if((xx <- length(FC$TimeStamp)) != (yy <- length(FC$Code))) {
        for(xxx in seq_len(as.numeric(abs(xx-yy)))) FC$TimeStamp <- c(FC$TimeStamp, FC$TimeStamp[length(FC$TimeStamp)]+xxx)
      }
      temp <- tryCatch({data.table::data.table(TimeStamp = FC$TimeStamp, Code = FC$Code)}, error = function(x) NULL)
      if(length(temp) > 0L) {
        temp[, Type := 'FC']
        MasterSet <- data.table::rbindlist(list(MasterSet, temp))
      }
    } else {
      if((xx <- length(FC$TimeStamp)) != (yy <- length(FC$Code))) {
        for(xxx in seq_len(as.numeric(abs(xx-yy)))) FC$TimeStamp <- c(FC$TimeStamp, FC$TimeStamp[length(FC$TimeStamp)]+xxx)
      }
      MasterSet <- tryCatch({data.table::data.table(TimeStamp = FC$TimeStamp, Code = FC$Code)}, error = function(x) NULL)
      if(length(MasterSet) > 0L) {
        MasterSet[, Type := 'FC']
      }
    }
  }
  if(length(PL) > 0L) {
    if(exists('MasterSet') && length(MasterSet) > 0L) {
      if((xx <- length(PL$TimeStamp)) != (yy <- length(PL$Code))) {
        for(xxx in seq_len(as.numeric(abs(xx-yy)))) PL$TimeStamp <- c(PL$TimeStamp, PL$TimeStamp[length(PL$TimeStamp)]+xxx)
      }
      temp <- tryCatch({data.table::data.table(TimeStamp = PL$TimeStamp, Code = PL$Code)}, error = function(x) NULL)
      if(length(temp) > 0L) {
        temp[, Type := 'PL']
        MasterSet <- data.table::rbindlist(list(MasterSet, temp))
      }
    } else {
      if((xx <- length(PL$TimeStamp)) != (yy <- length(PL$Code))) {
        for(xxx in seq_len(as.numeric(abs(xx-yy)))) PL$TimeStamp <- c(PL$TimeStamp, PL$TimeStamp[length(PL$TimeStamp)]+xxx)
      }
      MasterSet <- tryCatch({data.table::data.table(TimeStamp = PL$TimeStamp, Code = PL$Code)}, error = function(x) NULL)
      if(length(MasterSet) > 0L) {
        MasterSet[, Type := 'PL']
      }
    }
  }
  if(exists('MasterSet')) return(MasterSet) else return(NULL)
}

#' @title DataTable
#'
#' @description Fully loaded DT::datatable() with args prefilled
#'
#' @author Adrian Antico
#' @family Utils
#'
#' @param data source data.table
#' @param FixedCols Number of columns from the left to Freeze, like freeze panes in Excel. Default is 2
#'
#' @examples
#' \dontrun{
#' # Rmarkdown example of DataTable inside a <details> </Details> section
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
#' Quantico:::DataTable(data)
#' ````
#'
#' # Shiny Usage
#' output$Table <- shiny::renderUI({Quantico:::DataTable(data)})
#'
#' }
#'
#' @export
DataTable <- function(data, FixedCols = 2) {
  # rowCallback <- c(
  #   "function(row, data, num, index){",
  #   "  var $row = $(row);",
  #   "  if($row.hasClass('even')){",
  #   "    $row.css('background-color', 'green');",
  #   "    $row.hover(function(){",
  #   "      $(this).css('background-color', 'yellow');",
  #   "     }, function(){",
  #   "      $(this).css('background-color', 'green');",
  #   "     }",
  #   "    );",
  #   "  }else{",
  #   "    $row.css('background-color', 'cyan');",
  #   "    $row.hover(function(){",
  #   "      $(this).css('background-color', 'orange');",
  #   "     }, function(){",
  #   "      $(this).css('background-color', 'cyan');",
  #   "     }",
  #   "    );",
  #   "  }",
  #   "}")

  #x <- Quantico:::NumericColNames(data)
  #if(identical(x, numeric(0))) x <- NULL
  #numcols <- ncol(data)
  table <- DT::datatable(
    data,
    # filter = 'bottom',
    # editable = TRUE,
    rownames = FALSE,
    extensions = c('Buttons','ColReorder'), #,'FixedColumns'), # Only usable in Rmarkdown  'Select'),
    options = list(
      #rowCallback = DT::JS(rowCallback),
      select = list(style = 'os', items = 'row'),
      dom = 'Brtip', #Bfrtip
      #dom = 'ltipr',
      #fixedColumns = list(leftColumns = max(0, min(numcols - 2, FixedCols))),
      #buttons = c('copy','pdf'), # Only usable in Rmarkdown 'selectRows', 'selectColumns', 'selectCells', 'selectAll', 'selectNone'),
      colReorder = TRUE,
      autoWidth = TRUE,
      selection = list(mode = 'multiple', target = 'row+column'), # 'row', 'column'
      #style = 'bootstrap', # 'auto', 'default', 'bootstrap', or 'bootstrap4'
      # 'background-color': '#6800ff3d',
      initComplete = DT::JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({
        'background-color': '#a3ff00',
        'color': '#000'});",
        "}"),
      columnDefs = list(list(
        className = 'dt-center',
        targets = 0:(ncol(data)-1L),
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data != null && data.length > 30 ?",
          "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
          "}"
        ))),
      targets = "_all",
      scrollX = TRUE,
      fillContainer = TRUE,
      autoHideNavigation = TRUE,
      lengthMenu = c(5, 30, 50),
      pageLength = 10))
  table
  #table <-
  # DT::formatStyle(
  #   table = table,
  #   columns = names(data),
  #   target = 'row',
  #   color = '#49db00',
  #   backgroundColor = "#202124") #252529 #121212 #'#04022e' #0000'#DT::styleEqual(c('A', 'B'), c('#71a89f', '#00e1ff'))
  #if(length(x) > 0L) DT::formatRound(table = table, columns = x, digits = 3) else table
}

#' @title InitializePlotObjects
#'
#' @param TotalPlots data.table
#'
#' @noRd
InitializePlotObjects <- function(TotalPlots) {

  # Initalize PlotObjectLists
  for(po in seq_len(TotalPlots)) {
    if(po < 10L) {
      assign(x = paste0('PlotObjectList_0', po), value = list())
    } else {
      assign(x = paste0('PlotObjectList_', po), value = list())
    }
  }

  x <- list()
  for(num in seq_len(TotalPlots)) {

    # Fill out all slots ahead of time and update value blow when encountered, otherwise pass through what's in there
    PlotMetaData <- list(

      # MetaData:
      #  PlotID -> connect plot metadata to fixed plot button to drag and drop
      #  DataSource -> enable multiple data sets to be loaded
      #  PlotType -> reactive so that options below can adjust accordingly
      #  UpdateMethod -> not sure if needed but idea is to ensure that no action is taken that isn't needed, such as filtering
      #               -> Modify value of this as list gets updated via user selection
      'PlotType' = NULL,               # (listed for reference)

      # Data Usage:
      #   Sample Size -> would like to add sampling options or even allow for bootstrapping
      'SampleSize' = 100000L,

      # Plot extras
      'NumberBins' = 30L,

      # Variables Selection (listed for reference)
      'YVars' = NULL,
      'XVars' = NULL,
      'GroupVars' = NULL,
      'Levels1' = NULL,
      'Levels2' = NULL,
      'Levels3' = NULL,

      # Filter Variables, logic, and values (listed for reference)
      'FilterVar1' = NULL,
      'FilterVar2' = NULL,
      'FilterVar3' = NULL,
      'FilterVar4' = NULL,
      'FilterLogic1' = NULL,
      'FilterLogic2' = NULL,
      'FilterLogic3' = NULL,
      'FilterLogic4' = NULL,
      'FilterValue_1_1' = NULL,
      'FilterValue_1_2' = NULL,
      'FilterValue_1_3' = NULL,
      'FilterValue_1_4' = NULL,
      'FilterValue_2_1' = NULL,
      'FilterValue_2_2' = NULL,
      'FilterValue_2_3' = NULL,
      'FilterValue_2_4' = NULL,

      # Separate Button to Update These Inside DropDown Box
      "ColorBackground" = "#6a6969", #"#1b1959", #'#00060b',
      "ColorChart" = '#001748', # #5757576e
      "ColorFont" = '#e2e2e2',
      "ColorFill" = '#0066ff',
      "ZeroLineColor" = '#e2e2e2',
      "ZeroLineWidth" = 1.75,
      "LegendPosition" = 'bottom',
      'AngleY' = 0,
      'AngleX' = 90,
      'TextSize' = 14,
      'OutlierSize' = 0.01,
      'LegendBorderSize' = 0.01,
      'LegendLineType' = 'solid')

    # Fill in master list
    for(meta in names(PlotMetaData)) {
      x[[paste0('Plot_', num)]][[meta]] <- PlotMetaData[[meta]]
    }
  }
  return(x)
}
