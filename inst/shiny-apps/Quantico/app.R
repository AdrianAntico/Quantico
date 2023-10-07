options(scipen = 999);options(shiny.maxRequestSize = 250000*1024^2);options(warn = -1)
library(AutoQuant);library(Rodeo);library(data.table);library(shiny);library(magrittr);library(shinydashboard)
EchartThemes <- c("auritus","azul","bee-inspired","blue","caravan","carp","chalk","cool","dark-bold","dark","eduardo", "essos","forest","fresh-cut","fruit","gray","green","halloween","helianthus","infographic","inspired","jazz","london","macarons","macarons2","mint","purple-passion","red-velvet","red","roma","royal","sakura","shine","tech-blue","vintage","walden","wef","weforum","westeros","wonderland")
LogoStyle <- "max-width: 98.5%; height: auto; max-height: 143px; padding-left: 0px; padding-right: 0px; border-radius: 30px; padding-bottom: 0px; background-color: #0000; box-shadow: 0px 0px 0px 0px #7bc1ff; padding-top: 5px;"
LogoBoxStyle <- "padding-left: 15px; padding-right: 16px; border-radius: 30px; margin-right:0px; min-height: 168px;"
Logo <- "LogoWhite.png" # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/LogoWhite.png?raw=true"

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# UI Code                              ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# telemetry <- Telemetry$new()
ui <- shinydashboard::dashboardPage(
  title = 'Quantico',
  skin = 'blue',
  shinydashboard::dashboardHeader(disable = TRUE),
  shinydashboard::dashboardSidebar(disable = TRUE),
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$script(
        shiny::HTML(
          "$(document).on('click', '.sw-dropdown', function () {
            Shiny.onInputChange('last_btn',this.id);
        });")
      )
    ),

    shinybusy::add_busy_spinner(spin = "fading-circle", position = "full-page", height = "100px", width = "100px"),

    # ----

    # ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Header                               ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    shiny::fluidRow(
      shiny::column(
        width = 2L,
        shiny::fluidRow(
          shiny::column(
            width = 12L, align = "center",
            style = "padding-left: 15px; padding-right: 16px;",
            Quantico::BlankRow(12L),
            shinydashboard::box(
              title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'danger', width = 12L,
              style=LogoBoxStyle,
              shiny::fluidRow(
                shiny::tags$img(
                  src = Logo,
                  style = LogoStyle,
                  alt = "Quantico",
                  `data-view-component` = "true")
              ) # fluid row
            ) # jqui resizable
          )
        ),

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Side Bar                             ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        Quantico::BlankRow(12L),
        shiny::fluidRow(
          shiny::column(
            width = 12L,
            Quantico::SideBarUI(id = "SideBarUIID", AppWidth = 12L)
          ) # column
        ) # fluid Row
      ),# end column width =  2L

      shiny::column(
        width = 10L,

        # ----

        # ----

        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        # Output Panels                        ----
        # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
        Quantico:::BlankRow(12),
        shiny::fluidRow(
          shiny::column(
            12L, style="padding-left: 0px",
            shiny::tags$div(
              id = "output3", class = "row",
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'danger', width = 12L,
                style = "min-height: 1655px; max-height: 1655px;",

                shinyjqui::sortableTabsetPanel(
                  id = 'tabss', selected = 'Home',

                  # ----

                  # ----

                  # @@@@@@@@@@@@@@@@@@@@ ----
                  # Home                 ----
                  # @@@@@@@@@@@@@@@@@@@@ ----
                  shiny::tabPanel(
                    id = "Home",
                    title = 'Home',
                    icon = shiny::icon('house'),
                    Quantico:::BlankRow(12L),
                    shiny::fluidRow(
                      style = "padding-top: 20px;",
                      Quantico::HomePage(id = 'HomePageID'))),

                  # ----

                  # ----

                  # @@@@@@@@@@@@@@@@@@@@ ----
                  # Code Print           ----
                  # @@@@@@@@@@@@@@@@@@@@ ----
                  Quantico::CodePrintPage(id = "CodePrintPageID", AppWidth = 12L)

                ), # end sortableTabsetPanel

                # ----

                # ----

                # @@@@@@@@@@@@@@@@@@@@ ----
                # Last Body Section    ----
                # @@@@@@@@@@@@@@@@@@@@ ----

                # Style Sheet Reference: supply one to start to avoid plain white startup (so ugly)
                # the uiOutput overwrites the tags$link() version when user changes selection
                tags$link(rel = 'stylesheet', type = 'text/css', href = "medium-gray.css"),
                shiny::uiOutput('css'),
                shiny::uiOutput('BackgroundImage'),
                shiny::HTML("<script async defer src='https://buttons.github.io/buttons.js'></script>")

              ) # box
            ) # Main Body done
          ) # column
        ) # fluid Rows
      ) # end column width = 10L
    ) # Header / first fluid row
  ) # dashboardBody
) # dashboardPage

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
# Server Code                          ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
server <- function(input, output, session) {

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Misc                              ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Change css in session
  output$css <- shiny::renderUI({
    cssfile <- paste0(input$CssSelect, ".css")
    tags$link(rel = 'stylesheet', type = 'text/css', href = cssfile)
  })

  # Change background image in session
  output$BackgroundImage <- shiny::renderUI({
    bgimage <- input$BackgroundImageSelect
    final <- paste0(bgimage, '.jpg')
    shinyWidgets::setBackgroundImage(src = final, shinydashboard = TRUE)
  })

  # Logo Selection
  # shiny::observeEvent(input$Theme, {
  #   output$LogoID <- shiny::renderUI({
  #     theme_check <- tryCatch({grepl(pattern = "day", x = input[["CssSelect"]])}, error = function(x) FALSE)
  #     Quantico:::Logo(
  #       id = "logoHeader",
  #       Day = theme_check)
  #   })
  # })

  shiny::observeEvent(input$CssSelect, {
    if(input$CssSelect == "day-light-blue") {
      shiny::updateSelectInput(session = session, inputId = "EchartsTheme", label = "Plots Theme", choices = EchartThemes, selected = "macarons")
      shiny.fluent::updateColorPicker.shinyInput(session = session, inputId = "ColorFont", value = "#161617")
    } else if(input$CssSelect == "light-gray") {
      shiny::updateSelectInput(session, inputId = "EchartsTheme", label = "Plots Theme", choices = EchartThemes, selected = "tech-blue")
      shiny.fluent::updateColorPicker.shinyInput(session = session, inputId = "ColorFont", value = "#dcdce3")
    } else if(input$CssSelect == "medium-gray") {
      shiny::updateSelectInput(session = session, inputId = "EchartsTheme", label = "Plots Theme", choices = EchartThemes, selected = "wef")
      shiny.fluent::updateColorPicker.shinyInput(session = session, inputId = "ColorFont", value = "#dcdce3")
    } else if(input$CssSelect == "dark-gray") {
      shiny::updateSelectInput(session = session, inputId = "EchartsTheme", label = "Plots Theme", choices = EchartThemes, selected = "shine")
      shiny.fluent::updateColorPicker.shinyInput(session = session, inputId = "ColorFont", value = "#dcdce3")
    } else if(input$CssSelect == "piano-black") {
      shiny::updateSelectInput(session = session, inputId = "EchartsTheme", label = "Plots Theme", choices = EchartThemes, selected = "helianthus")
      shiny.fluent::updateColorPicker.shinyInput(session = session, inputId = "ColorFont", value = "#dcdce3")
    } else if(input$CssSelect == "yellow") {
      shiny::updateSelectInput(session = session, inputId = "EchartsTheme", label = "Plots Theme", choices = EchartThemes, selected = "bee-inspired")
      shiny.fluent::updateColorPicker.shinyInput(session = session, inputId = "ColorFont", value = "#dcdce3")
    } else if(input$CssSelect == "yellow-green") {
      shiny::updateSelectInput(session = session, inputId = "EchartsTheme", label = "Plots Theme", choices = EchartThemes, selected = "fresh-cut")
      shiny.fluent::updateColorPicker.shinyInput(session = session, inputId = "ColorFont", value = "#dcdce3")
    } else if(input$CssSelect == "green") {
      shiny::updateSelectInput(session = session, inputId = "EchartsTheme", label = "Plots Theme", choices = EchartThemes, selected = "infographic")
      shiny.fluent::updateColorPicker.shinyInput(session = session, inputId = "ColorFont", value = "#dcdce3")
    } else if(input$CssSelect == "green-blue") {
      shiny::updateSelectInput(session = session, inputId = "EchartsTheme", label = "Plots Theme", choices = EchartThemes, selected = "mint")
      shiny.fluent::updateColorPicker.shinyInput(session = session, inputId = "ColorFont", value = "#dcdce3")
    } else if(input$CssSelect == "light-blue") {
      shiny::updateSelectInput(session = session, inputId = "EchartsTheme", label = "Plots Theme", choices = EchartThemes, selected = "walden")
      shiny.fluent::updateColorPicker.shinyInput(session = session, inputId = "ColorFont", value = "#dcdce3")
    } else if(input$CssSelect == "dodger-blue") {
      shiny::updateSelectInput(session = session, inputId = "EchartsTheme", label = "Plots Theme", choices = EchartThemes, selected = "walden")
      shiny.fluent::updateColorPicker.shinyInput(session = session, inputId = "ColorFont", value = "#dcdce3")
    } else if(input$CssSelect == "blue") {
      shiny::updateSelectInput(session = session, inputId = "EchartsTheme", label = "Plots Theme", choices = EchartThemes, selected = "walden")
      shiny.fluent::updateColorPicker.shinyInput(session = session, inputId = "ColorFont", value = "#dcdce3")
    } else if(input$CssSelect == "blue-purple") {
      shiny::updateSelectInput(session = session, inputId = "EchartsTheme", label = "Plots Theme", choices = EchartThemes, selected = "macarons")
      shiny.fluent::updateColorPicker.shinyInput(session = session, inputId = "ColorFont", value = "#dcdce3")
    } else if(input$CssSelect == "purple") {
      shiny::updateSelectInput(session = session, inputId = "EchartsTheme", label = "Plots Theme", choices = EchartThemes, selected = "walden")
      shiny.fluent::updateColorPicker.shinyInput(session = session, inputId = "ColorFont", value = "#dcdce3")
    } else if(input$CssSelect == "pink") {
      shiny::updateSelectInput(session = session, inputId = "EchartsTheme", label = "Plots Theme", choices = EchartThemes, selected = "wonderland")
      shiny.fluent::updateColorPicker.shinyInput(session = session, inputId = "ColorFont", value = "#dcdce3")
    } else if(input$CssSelect == "red") {
      shiny::updateSelectInput(session = session, inputId = "EchartsTheme", label = "Plots Theme", choices = EchartThemes, selected = "red")
      shiny.fluent::updateColorPicker.shinyInput(session = session, inputId = "ColorFont", value = "#dcdce3")
    }
  })

  # App and Plot related
  NumTabs <- shiny::getShinyOption('MaxPlotTabs')
  MaxPlotsAvailable <- NumTabs * 12#shiny::getShinyOption('MaxPlots')
  NumPlotsAvailable <- NumTabs * 12#MaxPlotsAvailable
  AzureCredsFile <- shiny::getShinyOption('AzureCredsFile')
  Debug <- shiny::getShinyOption('Debug')
  DebugSessionLoad <- shiny::getShinyOption('DebugSessionLoad')
  DebugAddPlotTab <- shiny::getShinyOption('DebugAddPlotTab')
  DebugPlotButtonTextUpdate <- shiny::getShinyOption('DebugPlotButtonTextUpdate')
  DebugAddPlotButton <- shiny::getShinyOption('DebugAddPlotButton')
  DebugPlottingOE <- shiny::getShinyOption('DebugPlottingOE')
  DebugFC <- shiny::getShinyOption('DebugFC')
  CacheDir <- shiny::getShinyOption('CacheDir')
  CacheName <- shiny::getShinyOption('CacheName')
  PostGRE_DBNames <- shiny::getShinyOption('PostGRE_DBNames')
  PostGRE_Host <- shiny::getShinyOption('PostGRE_Host')
  PostGRE_Port <- shiny::getShinyOption('PostGRE_Port')
  PostGRE_User <- shiny::getShinyOption('PostGRE_User')
  PostGRE_Password <- shiny::getShinyOption('PostGRE_Password')
  WorkingDirectory <- shiny::getShinyOption('WorkingDirectory')
  setwd(WorkingDirectory)
  BlobStorageURL <- shiny::getShinyOption('BlobStorageURL', default = NULL)

  # Initalize validators
  PlotOutput_Start1 <- TRUE

  # Initialize ML Results Table
  ML_RegressionTable <- data.table::data.table(
    Algo = rep('zzz', 1000L), `Model ID` = rep('zzz', 1000L),
    Date = rep(Sys.time(), 1000L), `Grid Tune` = rep(FALSE, 1000L),
    `Test r-sq` = rep(0.0, 1000L), `Train r-sq` = rep(0.0, 1000L),
    `Test RMSE` = rep(0.0, 1000L), `Train RMSE` = rep(0.0, 1000L),
    `Test MAE` = rep(0.0, 1000L), `Train MAE` = rep(0.0, 1000L),
    `Test MAPE` = rep(0.0, 1000L), `Train MAPE` = rep(0.0, 1000L))
  ML_ClassificationTable <- data.table::data.table(
    Algo = rep('zzz', 1000L), `Model ID` = rep('zzz', 1000L),
    Date = rep(Sys.time(), 1000L), `Grid Tune` = rep(FALSE, 1000L),
    `Test Accuracy` = rep(0.0, 1000L), `Test Accuracy Thresh` = rep(0.0, 1000L),
    `Train Accuracy` = rep(0.0, 1000L), `Train Accuracy Thresh` = rep(0.0, 1000L),
    `Test MCC` = rep(0.0, 1000L), `Test MCC Thresh` = rep(0.0, 1000L),
    `Train MCC` = rep(0.0, 1000L), `Train MCC Thresh` = rep(0.0, 1000L),
    `Test Utility` = rep(0.0, 1000L), `Test Utility Thresh` = rep(0.0, 1000L),
    `Train Utility` = rep(0.0, 1000L), `Train Utility Thresh` = rep(0.0, 1000L))
  ML_MultiClassTable <- data.table::data.table(
    Algo = rep('zzz', 1000L), `Model ID` = rep('zzz', 1000L),
    Date = rep(Sys.time(), 1000L), `Grid Tune` = rep(FALSE, 1000L),
    `Test Accuracy` = rep(0.0, 1000L), `Train Accuracy` = rep(0.0, 1000L),
    `Test MCC` = rep(0.0, 1000L), `Train MCC` = rep(0.0, 1000L),
    `Test MicroAUC` = rep(0.0, 1000L), `Train MicroAUC` = rep(0.0, 1000L),
    `Test LogLoss` = rep(0.0, 1000L), `Train LogLoss` = rep(0.0, 1000L))

  # Initialize Data
  data <- NULL; InitalizeInputs <- TRUE; ModelData <- NULL; ModelOutputList <- NULL; OutputList <- NULL

  # List of Plot Types to choose
  AvailablePlots <- c(
    'HistogramPlot','BoxPlot','PiePlot','DonutPlot',"RosetypePlot",'WordCloud','ProbabilityPlot',
    'BarPlot','StackedBarPlot','LinePlot','ScatterPlot','Autocorrelation','PartialAutocorr',
    'AreaPlot','StepPlot','RiverPlot','BarPlot3D','RadarPlot','ParallelPlot',
    'CopulaPlot','CorrelogramPlot','HeatMapPlot','ScatterPlot3D','CopulaPlot3D','DensityPlot',
    'PartialDependenceLine','PartialDependenceBox','PartialDependenceHeatMap',
    'CalibrationLine','CalibrationBox','ShapleyImportance',
    'Residuals','ResidScatter','ResidualsCopulaPlot',
    'VariableImportance','ConfusionMatrixHeatmap',
    'GainsPlot','LiftPlot','ROCPlot')

  # Load credentials
  if(!is.null(AzureCredsFile)) {
    creds <- data.table::fread(file = file.path(AzureCredsFile, 'AutoPlotterCreds.csv'))
    StorageAccount <- creds[Account == 'StorageAccount', Values]
    Container <- creds[Account == 'Container', Values]
    Key <- creds[Account == 'Key', Values]
  } else {
    StorageAccount <- NULL; Container <- NULL; Key <- NULL
  }

  # Code Collection Lists
  DataMgtCode <<- list(); DataWranglingCode <<- list(); FeatureEngineeringCode <<- list()
  MachineLearningCode <<- list(); ForecastingCode <<- list(); PlotterCode <<- list()
  MasterSet <- NULL

  # Object Storage Lists
  DataList <- list()
  FinanceDataList <- list(Debug = Debug)
  StockSymbolsData <- NULL  #Quantico:::LoadCSV(Infile = system.file(package = "Quantico", "shiny-apps", "Quantico", "ticker_data.csv"))
  StockSymbolsData <<- NULL #StockSymbolsData
  TickerSymbols <- NULL     #tryCatch({sort(unique(StockSymbolsData$ticker))}, error = function(x) NULL)
  TickerSymbols <<- NULL    #TickerSymbols
  DragulaSelectedList <- list()
  DragulaSelectedList <<- DragulaSelectedList
  PlotPanelInputsList <- list()
  PlotPanelInputsList <<- PlotPanelInputsList
  DisplayPlots <- list()

  # Dropdown args collection lists
  PlotDropDown <- list(Debug = Debug)

  # Usernames and Passwords
  # UserName_Password_DT <- shiny::getShinyOption(name = 'UserName_Password_DT', default = NULL)
  # if(!is.null(UserName_Password_DT) && 'UserName' %in% names(UserName_Password_DT) && 'Password' %in% names(UserName_Password_DT)) {
  #   Credentials <- UserName_Password_DT
  # } else {
  #   Credentials <- data.table::data.table(UserName = c('Guest'), Password = c('Password'))
  # }

  # Dynamic Observers
  PlotExecuteExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$TrendPlotExecute", seq_len(NumTabs), collapse = ",\n  "),
    ")"
  ))
  DropDownExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$PlotDropDown", seq_len(MaxPlotsAvailable), collapse = ",\n  "),
    ")"
  ))
  PlotTextNamesExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$PlotID_", seq_len(MaxPlotsAvailable), collapse = ",\n  "),
    ")"
  ))
  DragulaExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$PlotTypeDragula", seq_len(NumTabs), collapse = ",\n  "),
    ")"
  ))
  MLOutputExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$MLOutputExecute", seq_len(NumTabs), collapse = ",\n  "),
    ")"
  ))
  MLReportOutputExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$MLMarkdownExecute", seq_len(NumTabs), collapse = ",\n  "),
    ")"
  ))
  MLModelSelectionExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$MLReportsModelSelection", seq_len(NumTabs), collapse = ",\n  "),
    ")"
  ))
  DataOutputExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$DataOutputExecute", seq_len(NumTabs), collapse = ",\n  "),
    ")"
  ))
  EDAOutputExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$EDAOutputExecute", seq_len(NumTabs), collapse = ",\n  "),
    ")"
  ))
  EDASelectionExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$EDAData", seq_len(NumTabs), collapse = ",\n  "),
    ")"
  ))
  EDAReportOutputExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$EDAMarkdownExecute", seq_len(NumTabs), collapse = ",\n  "),
    ")"
  ))
  PlottingReportOutputExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$PlottingMarkdownExecute", seq_len(NumTabs), collapse = ",\n  "),
    ")"
  ))
  TablesReportOutputExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$TablesMarkdownExecute", seq_len(NumTabs), collapse = ",\n  "),
    ")"
  ))
  InferenceOutputExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$InferenceOutputExecute", seq_len(NumTabs), collapse = ",\n  "),
    ")"
  ))
  InferenceReportOutputExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$InferenceMarkdownExecute", seq_len(NumTabs), collapse = ",\n  "),
    ")"
  ))
  FCOutputExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$FCOutputExecute", seq_len(NumTabs), collapse = ",\n  "),
    ")"
  ))
  FCReportOutputExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$FCMarkdownExecute", seq_len(NumTabs), collapse = ",\n  "),
    ")"
  ))
  FCModelSelectionExpression <- parse(text = paste0(
    "list(\n  ",
    paste0("input$FCReportsModelSelection", seq_len(NumTabs), collapse = ",\n  "),
    ")"
  ))

  # Dragula Name Mapping
  # PlotMap <- list()
  # for(i in seq_len(NumPlotsAvailable)) PlotMap[[paste0("Plot", i)]] <- paste0("Plot", i)
  PlotMap1 <- data.table::data.table(
    PlotNameOriginal = paste0("Plot", seq_len(12L)),
    PlotName = paste0("Plot", seq_len(12L)),
    PlotNumber = seq_len(12L),
    PlotType = "None",
    TabNumber = 1,
    Selected = FALSE)
  if(NumPlotsAvailable > 12L) {
    PlotMap2 <- data.table::data.table(
      PlotNameOriginal = paste0("Plot", 13L:24L),
      PlotName = paste0("Plot", 13L:24L),
      PlotNumber = 13L:24L,
      PlotType = "None",
      TabNumber = 2,
      Selected = FALSE)
  }
  if(NumPlotsAvailable > 24L) {
    PlotMap3 <- data.table::data.table(
      PlotNameOriginal = paste0("Plot", 25L:36L),
      PlotName = paste0("Plot", 25L:36L),
      PlotNumber = 25L:36L,
      PlotType = "None",
      TabNumber = 3,
      Selected = FALSE)
  }

  if(NumPlotsAvailable == 12L) {
    PlotMap <- PlotMap1; rm(PlotMap1)
  } else if(NumPlotsAvailable > 12L) {
    PlotMap <- data.table::rbindlist(list(PlotMap1, PlotMap2))
    rm(PlotMap1)
    rm(PlotMap2)
  } else if(NumPlotsAvailable > 24L) {
    PlotMap <- data.table::rbindlist(list(PlotMap1, PlotMap2, PlotMap3))
    rm(PlotMap1)
    rm(PlotMap2)
    rm(PlotMap3)
  }
  PlotMap <<- PlotMap

  # Inputs
  # UserName <- shiny::reactive({input$UserName})
  # Password <- shiny::reactive({input$Password})

  # login modal
  # shiny::showModal(
  #   shiny::modalDialog(
  #     title = 'Login',
  #     easyClose = FALSE,
  #     tagList(
  #       shiny::selectInput(inputId = 'UserName', label =  "Fill out username", choices = Credentials[['UserName']], selected = 'UserName'),
  #       shiny::passwordInput(inputId = "Password", label =  "Fill out password", value = 'Password', placeholder = 'password')),
  #     footer = shiny::tagList(shiny::fluidRow(shiny::column(width = 6L, align = 'center', shinyWidgets::actionBttn(inputId = 'Check_Credentials', label = 'Sign In', icon = shiny::icon('chevron-right', lib = 'font-awesome'), style ='gradient', color='royal'))))))

  # Logout - takes you back to login modal
  # shiny::observeEvent(input$Logout, {
  #   DisplayPlots <<- list()
  #   ModelOutputList <<- NULL
  #   DataList <<- list()
  #   FinanceDataList <<- list(Debug = Debug)
  #   PlotDropDown <<- list(Debug = Debug)
  #   data <<- NULL
  #   DataMgtCode <<- list(); DataWranglingCode <<- list(); FeatureEngineeringCode <<- list()
  #   MachineLearningCode <<- list(); ForecastingCode <<- list(); PlotterCode <<- list()
  #   MasterSet <- NULL; gc()
  # })

  # Login Check
  # shiny::observeEvent(input$Check_Credentials, {
  #   if(UserName() %in% Credentials$UserName && Password() %in% Credentials[UserName == eval(UserName())]$Password) {
  #     shiny::removeModal()
  #   } else {
  #     shiny::showModal(
  #       shiny::modalDialog(
  #         title = 'Login Failed: Please Try Again',
  #         easyClose = FALSE,
  #         list(
  #           shiny::selectInput(inputId = 'UserName', label =  "Fill out username", choices = Credentials[['UserName']], selected = 'UserName'),
  #           shiny::passwordInput(inputId = "Password", label =  "Fill out password", value = 'Password', placeholder = 'password')),
  #         footer = shiny::tagList(
  #           shiny::fluidRow(shiny::column(width = 3L, align = 'center', shinyWidgets::actionBttn(inputId = 'Check_Credentials', label = 'Sign In', icon = shiny::icon('chevron-right', lib = 'font-awesome'), style ='gradient', color='royal')))
  #         )))
  #   }
  # })

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Output Tab Creation Removal       ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Add Plot Tab
  NumPlotTabsCurrent <- 0L
  shiny::observeEvent(input$NewPlotTab, {
    NumPlotTabsCurrent <- NumPlotTabsCurrent + 1L
    NumPlotTabsCurrent <<- NumPlotTabsCurrent
    if(NumPlotTabsCurrent <= NumTabs) {
      shiny::appendTab(
        inputId = "tabss",
        tab = Quantico:::PlotPanels(
          id = paste0("PlotPanels", NumPlotTabsCurrent),
          Page = NumPlotTabsCurrent,
          12L,
          DragulaChoices = NULL,
          AppWidth=12L),
        select = TRUE)
    } else {
      NumPlotTabsCurrent <- NumPlotTabsCurrent - 1L
      NumPlotTabsCurrent <<- NumPlotTabsCurrent
      shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("This session is restricted to ", NumTabs, " Plot Ouput Panels"), type = NULL, btn_labels = "warning", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Remove Plot Tab
  shiny::observeEvent(input$RemovePlotTab, {
    if(grepl(pattern = "Plots", input$tabss)) {
      NumPlotTabsCurrent <- NumPlotTabsCurrent - 1L
      NumPlotTabsCurrent <<- NumPlotTabsCurrent
      shiny::removeTab("tabss", target = input$tabss)
    }
  }, ignoreInit = TRUE)

  # Add Table Tab
  NumTableTabsCurrent <- 0L
  shiny::observeEvent(input$NewDataTab, {
    NumTableTabsCurrent <- NumTableTabsCurrent + 1L
    NumTableTabsCurrent <<- NumTableTabsCurrent
    if(NumTableTabsCurrent <= NumTabs) {
      shiny::appendTab(
        inputId = "tabss",
        tab = Quantico:::DataPanels(
          id = paste0("DataPanels", NumTableTabsCurrent),
          Page = NumTableTabsCurrent,
          AppWidth=12L,
          DL = tryCatch({DataList}, error = function(x) NULL)),
        select = TRUE)
    } else {
      NumTableTabsCurrent <- NumTableTabsCurrent - 1L
      NumTableTabsCurrent <<- NumTableTabsCurrent
      shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("This session is restricted to ", NumTabs, " Data Ouput Panels"), type = NULL, btn_labels = "warning", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Remove Table Tab
  shiny::observeEvent(input$RemoveDataTab, {
    if(grepl(pattern = "Tables", input$tabss)) {
      NumTableTabsCurrent <- NumTableTabsCurrent - 1L
      NumTableTabsCurrent <<- NumTableTabsCurrent
      shiny::removeTab("tabss", target = input$tabss)
    }
  }, ignoreInit = TRUE)

  # Add ML Tab
  NumMLTabsCurrent <- 0L
  shiny::observeEvent(input$NewMLTab, {
    NumMLTabsCurrent <- NumMLTabsCurrent + 1L
    NumMLTabsCurrent <<- NumMLTabsCurrent
    if(NumMLTabsCurrent <= NumTabs) {
      shiny::appendTab(
        inputId = "tabss",
        tab = Quantico:::MLPanels(
          id = paste0("MLPanels", NumMLTabsCurrent),
          NumMLTabsCurrent,
          AppWidth=12L,
          MOL = tryCatch({ModelOutputList}, error = function(x) NULL)),
        select = TRUE)
    } else {
      NumMLTabsCurrent <- NumMLTabsCurrent - 1L
      NumMLTabsCurrent <<- NumMLTabsCurrent
      shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("This session is restricted to ", NumTabs, " ML Ouput Panels"), type = NULL, btn_labels = "warning", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Remove ML Tab
  shiny::observeEvent(input$RemoveMLTab, {
    if(grepl(pattern = "ML", input$tabss)) {
      NumMLTabsCurrent <- NumMLTabsCurrent - 1L
      NumMLTabsCurrent <<- NumMLTabsCurrent
      shiny::removeTab("tabss", target = input$tabss)
    }
  }, ignoreInit = TRUE)

  # Add EDA Tab
  NumEDATabsCurrent <- 0L
  shiny::observeEvent(input$NewEDATab, {
    NumEDATabsCurrent <- NumEDATabsCurrent + 1L
    NumEDATabsCurrent <<- NumEDATabsCurrent
    if(NumEDATabsCurrent <= NumTabs) {
      shiny::appendTab(
        inputId = "tabss",
        tab = Quantico:::EDAPanels(
          id = paste0("EDAPanels", NumEDATabsCurrent),
          NumEDATabsCurrent,
          AppWidth=12L,
          DL = tryCatch({DataList}, error = function(x) NULL)),
        select = TRUE)
    } else {
      NumEDATabsCurrent <- NumEDATabsCurrent - 1L
      NumEDATabsCurrent <<- NumEDATabsCurrent
      shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("This session is restricted to ", NumTabs, " EDA Ouput Panels"), type = NULL, btn_labels = "warning", btn_colors = NULL, htEDA = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Remove EDA Tab
  shiny::observeEvent(input$RemoveEDATab, {
    if(grepl(pattern = "EDA", input$tabss)) {
      NumEDATabsCurrent <- NumEDATabsCurrent - 1L
      NumEDATabsCurrent <<- NumEDATabsCurrent
      shiny::removeTab("tabss", target = input$tabss)
    }
  }, ignoreInit = TRUE)

  # Add Inference Tab
  NumInferenceTabsCurrent <- 0L
  shiny::observeEvent(input$NewInferenceTab, {
    NumInferenceTabsCurrent <- NumInferenceTabsCurrent + 1L
    NumInferenceTabsCurrent <<- NumInferenceTabsCurrent
    if(NumInferenceTabsCurrent <= NumTabs) {
      shiny::appendTab(
        inputId = "tabss",
        tab = Quantico:::InferencePanels(
          id = paste0("InferencePanels", NumInferenceTabsCurrent),
          NumInferenceTabsCurrent,
          AppWidth=12L,
          IOL = tryCatch({InferenceOutputList}, error = function(x) NULL)),
        select = TRUE)
    } else {
      NumInferenceTabsCurrent <- NumInferenceTabsCurrent - 1L
      NumInferenceTabsCurrent <<- NumInferenceTabsCurrent
      shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("This session is restricted to ", NumTabs, " Inference Ouput Panels"), type = NULL, btn_labels = "warning", btn_colors = NULL, htInference = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Remove Inference Tab
  shiny::observeEvent(input$RemoveInferenceTab, {
    if(grepl(pattern = "Inference", input$tabss)) {
      NumInferenceTabsCurrent <- NumInferenceTabsCurrent - 1L
      NumInferenceTabsCurrent <<- NumInferenceTabsCurrent
      shiny::removeTab("tabss", target = input$tabss)
    }
  }, ignoreInit = TRUE)

  # Add FC Tab
  NumFCTabsCurrent <- 0L
  shiny::observeEvent(input$NewFCTab, {
    NumFCTabsCurrent <- NumFCTabsCurrent + 1L
    NumFCTabsCurrent <<- NumFCTabsCurrent
    if(NumFCTabsCurrent <= NumTabs) {
      shiny::appendTab(
        inputId = "tabss",
        tab = Quantico:::FCPanels(
          id = paste0("FCPanels", NumFCTabsCurrent),
          NumFCTabsCurrent,
          AppWidth=12L,
          MOL = tryCatch({MLOutputList}, error = function(x) NULL)),
        select = TRUE)
    } else {
      NumFCTabsCurrent <- NumFCTabsCurrent - 1L
      NumFCTabsCurrent <<- NumFCTabsCurrent
      shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("This session is restricted to ", NumTabs, " FC Ouput Panels"), type = NULL, btn_labels = "warning", btn_colors = NULL, htFC = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Remove FC Tab
  shiny::observeEvent(input$RemoveFCTab, {
    if(grepl(pattern = "FC", input$tabss)) {
      NumFCTabsCurrent <- NumFCTabsCurrent - 1L
      NumFCTabsCurrent <<- NumFCTabsCurrent
      shiny::removeTab("tabss", target = input$tabss)
    }
  }, ignoreInit = TRUE)

  #                                      ----

  #               ----
  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Inputs :: Import Export           ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(Debug) print('Import Export Page Initialization')

  # Session
  shiny::observeEvent(input$Session_Modal, {
    x <- list.dirs(WorkingDirectory, recursive = FALSE); g <- c(); for(i in x) if(!grepl(pattern = "\\.", x = i)) g <- c(g, i)
    Quantico:::Session_Modal_Fun(
      id = 'SessionDMID',

      # Statics
      LoadSessionName_Choices = g,
      LoadSession_IncludeData_Choices = c(TRUE,FALSE),
      SaveSession_IncludeData_Choices = c(TRUE,FALSE),

      # Updaters
      LoadSessionName_Selected = if(length(input$LoadSessionName) > 0L) input$LoadSessionName else NULL,
      SaveSessionName_Selected = if(length(input$SaveSessionName) > 0L) input$SaveSessionName else NULL,
      LoadSession_IncludeData_Selected = if(length(input$LoadSession_IncludeData) > 0L) input$LoadSession_IncludeData else TRUE,
      SaveSession_IncludeData_Selected = if(length(input$SaveSession_IncludeData) > 0L) input$SaveSession_IncludeData else TRUE)
  })
  shiny::observeEvent(input$Session_OK, {shiny::removeModal()})

  # Local
  shiny::observeEvent(input$Local_Modal, {

    # Local .csv/parquet/text
    # output$TabularData <- shiny::renderUI({
    #   shiny::fileInput(inputId = 'TabularData', label = NULL, accept = c('text/csv/parquet', 'text/comma-separated-values,text/plain', '.csv', '.parquet', '.txt'))
    # })

    # Local .Rdata or .rds
    output$ModelObjectLoad <- shiny::renderUI({
      shiny::fileInput(inputId = "ModelObjectLoad", label = NULL, accept = c('.Rdata','.rds'))
    })

    Quantico:::Local_Modal_Fun(
      id = 'LocalDMID',

      # Statics
      SaveData_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      ExportFileType_Choices = c('.csv','.txt','.parquet'),
      SaveData_SelectDataRdata_Choices = tryCatch({names(ModelOutputList)}, error = function(x) NULL),

      # Updaters
      SaveData_SelectData_Selected = if(length(input$SaveData_SelectData) > 0L) input$SaveData_SelectData else NULL,
      ExportFileType_Selected = if(length(input$ExportFileType) > 0L) input$ExportFileType else "csv",
      SaveData_SelectDataRdata_Selected = if(length(input$SaveData_SelectDataRdata) > 0L) input$SaveData_SelectDataRdata else NULL,
      SaveFileNameRdata_Selected = if(length(input$SaveFileNameRdata) > 0L) input$SaveFileNameRdata else NULL)
  })
  shiny::observeEvent(input$Local_OK, {shiny::removeModal()})

  # PostGRE
  shiny::observeEvent(input$PostGRE_Modal, {

    # Editor theme
    shiny::observe({
      shinyAce::updateAceEditor(session,"sql_code",mode = "sql",theme = input$editor_theme)
    })


    PostGRE_Host_Selected  <- Quantico::ReturnParam(xx = tryCatch({input$PostGRE_Host}, error = function(x) NULL), Type = 'character', Default = NULL)
    PostGRE_Port_Selected <- Quantico::ReturnParam(xx = tryCatch({input$PostGRE_Port}, error = function(x) NULL), Type = 'numeric', Default = NULL)
    PostGRE_User_Selected <- Quantico::ReturnParam(xx = tryCatch({input$PostGRE_User}, error = function(x) NULL), Type = 'character', Default = NULL)
    PostGRE_Password_Selected <- Quantico::ReturnParam(xx = tryCatch({input$PostGRE_Password}, error = function(x) NULL), Type = 'character', Default = NULL)

    if(length(PostGRE_Host_Selected) > 0L) {
      PostGRE_Host <- PostGRE_Host_Selected
      PostGRE_Host <<- PostGRE_Host
    }

    if(length(PostGRE_Port_Selected) > 0L) {
      PostGRE_Port <- PostGRE_Port_Selected
      PostGRE_Port <<- PostGRE_Port
    }

    if(length(PostGRE_User_Selected) > 0L) {
      PostGRE_User <- PostGRE_User_Selected
      PostGRE_User <<- PostGRE_User
    }

    if(length(PostGRE_Password_Selected) > 0L) {
      PostGRE_Password <- PostGRE_Password_Selected
      PostGRE_Password <<- PostGRE_Password
    }

    # Check if there is a database connection
    if(length(PostGRE_Host) > 0L && length(PostGRE_Port) > 0L && length(PostGRE_User) > 0L && length(PostGRE_Password) > 0L) {
      PostGRE_DBNames <- Quantico:::DM.pgListDatabases(Host = PostGRE_Host, Port = PostGRE_Port, User = PostGRE_User, Password = PostGRE_Password)
    } else {
      PostGRE_DBNames <- NULL
    }

    # Dispatch modal
    Quantico:::PostGRE_Modal_Fun(
      id = 'PostGREDMID',

      # Reactives
      PostGRE_PullTable_Table_Selected = if(length(input$PostGRE_PullTable_Table) > 0L) input$PostGRE_PullTable_Table else NULL,
      PostGRE_PullTable_Columns_Selected = if(length(input$PostGRE_PullTable_Columns) > 0L) input$PostGRE_PullTable_Columns else NULL,
      PostGRE_PullTable_GroupByColumns_Selected = if(length(input$PostGRE_PullTable_GroupByColumns) > 0L) input$PostGRE_PullTable_GroupByColumns else NULL,
      PostGRE_DropTable_Table_Selected = if(length(input$PostGRE_DropTable_Table) > 0L) input$PostGRE_DropTable_Table else NULL,

      # Statics
      PostGRE_PullTable_AggStat_Choices = c('AVG','COUNT','SUM','MAX','MIN'),
      PostGRE_SamplePercent_Choices = seq(0.01,1.0,0.01),
      PostGRE_PullTable_DB_Choices = PostGRE_DBNames,
      PostGRE_DropTable_DB_Choices = PostGRE_DBNames,
      PostGRE_DropDB_DB_Choices = PostGRE_DBNames,
      SaveData_SelectDataPostGRE_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      SaveData_DataBaseName_Choices = PostGRE_DBNames,

      # Updaters
      PostGRE_Host_Selected = PostGRE_Host,
      PostGRE_Port_Selected = PostGRE_Port,
      PostGRE_User_Selected = PostGRE_User,
      PostGRE_Password_Selected = PostGRE_Password,
      PostGRE_PullTable_AggStat_Selected = if(length(input$PostGRE_PullTable_AggStat)) input$PostGRE_PullTable_AggStat else NULL,
      PostGRE_SamplePercent_Selected = if(length(input$PostGRE_SamplePercent)) input$PostGRE_SamplePercent else NULL,
      PostGRE_PullTable_DB_Selected = if(length(input$PostGRE_PullTable_DB)) input$PostGRE_PullTable_DB else NULL,
      PostGRE_DropTable_DB_Selected = if(length(input$PostGRE_DropTable_DB)) input$PostGRE_DropTable_DB else NULL,
      PostGRE_NewDB_DB_Selected = if(length(input$PostGRE_NewDB_DB)) input$PostGRE_NewDB_DB else NULL,
      PostGRE_DropDB_DB_Selected = if(length(input$PostGRE_DropDB_DB)) input$PostGRE_DropDB_DB else NULL,
      SaveData_SelectDataPostGRE_Selected = if(length(input$SaveData_SelectDataPostGRE)) input$SaveData_SelectDataPostGRE else NULL,
      SaveData_DataBaseName_Choices_Selected = if(length(input$SaveData_DataBaseName_Choices)) input$SaveData_DataBaseName_Choices else NULL,
      SaveData_TableName_Selected = if(length(input$SaveData_TableName)) input$SaveData_TableName else NULL)

    # Reactives
    if(length(PostGRE_DBNames) > 0L) {
      DBName <- shiny::reactive({tryCatch({input[['PostGRE_PullTable_DB']]}, error = function(x) NULL)})
      shiny::observeEvent(DBName(), {
        x <- tryCatch({Quantico::DM.pgListTables(DataBase = shiny::req(DBName()), Host = PostGRE_Host, Port = PostGRE_Port, User = PostGRE_User, Password = PostGRE_Password)}, error = function(x) NULL)
        if(length(x) == 0) x <- NULL
        Quantico:::SelectizeInput(session = session, Update = TRUE, InputID = 'PostGRE_PullTable_Table', Label = NULL, Choices = x, SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1L, CloseAfterSelect = FALSE, Debug = Debug)
        TableName <- shiny::reactive(shiny::req(input$PostGRE_PullTable_Table))
        ColNames <- shiny::eventReactive(eventExpr = shiny::req(TableName()), {
          tryCatch({Quantico::DM.pgTableColnames(Host = PostGRE_Host, DataBase = DBName(), Table = TableName(), User = PostGRE_User, Port = PostGRE_Port, Password = PostGRE_Password)}, error = function(x) NULL)
        })

        # 3. Once a table is selected pick out columns
        shiny::observeEvent(shiny::req(ColNames()), {
          if(length(ColNames()) > 0) x <- sort(ColNames()) else x <- NULL
          Quantico:::PickerInput(session = session, Update = TRUE, InputID = 'PostGRE_PullTable_Columns', Label = NULL, Choices = x, SelectedDefault = NULL, Debug = Debug)
          Quantico:::SelectizeInput(session = session, Update = TRUE, InputID = 'PostGRE_PullTable_GroupByColumns', Label = NULL, Choices = x, SelectedDefault = NULL, Multiple = TRUE, MaxVars = 100L, CloseAfterSelect = FALSE, Debug = Debug)
        }, ignoreInit = TRUE)

      }, ignoreInit = TRUE)
      DropDBName <- shiny::reactive({tryCatch({input[['PostGRE_DropTable_DB']]}, error = function(x) NULL)})
      shiny::observeEvent(shiny::req(DropDBName()), {
        DBNameSelected <- tryCatch({shiny::req(DropDBName())}, error = function(x) NULL)
        x <- Quantico::DM.pgListTables(DataBase = DBNameSelected, Host = PostGRE_Host, Port = PostGRE_Port, User = PostGRE_User, Password = PostGRE_Password)
        if(length(x) == 0L) x <- NULL
        Quantico:::PickerInput(session = session, Update = TRUE, InputID = 'PostGRE_DropTable_Table', Label = NULL, Choices = sort(x), SelectedDefault = NULL, Multiple = TRUE, Debug = Debug)
      }, ignoreInit = TRUE)
    }
  })
  shiny::observeEvent(input$PostGRE_OK, {shiny::removeModal()})

  # Azure Blob
  # shiny::observeEvent(input$AzureBlob_Modal, {
  #
  #   # Azure Blob .csv or .txt
  #   if(Debug) paste0('https://', StorageAccount, '.blob.core.windows.net/', Container)
  #   BlobStorageURL <- paste0('https://', StorageAccount, '.blob.core.windows.net/', Container)
  #   assign(x = 'BlobStorageURL', value = BlobStorageURL, envir = .GlobalEnv)
  #   cont <<- tryCatch({AzureStor::blob_container(BlobStorageURL, key = Key)}, error = function(x) NULL)
  #   rawfiles <<- tryCatch({AzureStor::list_storage_files(cont, info = 'name')}, error = function(x) NULL)
  #   if(length(rawfiles) != 0) {
  #     rawfiles <<- rawfiles[c(which(grepl(pattern = '.csv', x = rawfiles)), which(grepl(pattern = '.Rdata', x = rawfiles)))]
  #     rawfiles_csv <- rawfiles[which(grepl(pattern = '.csv', x = rawfiles))]
  #   } else {
  #     rawfiles_csv <<- NULL
  #   }
  #
  #   # Azure blob .Rdata
  #   if(length(rawfiles) != 0) {
  #     rawfiles_rdata <- rawfiles[which(grepl(pattern = '.Rdata', x = rawfiles))]
  #   } else {
  #     rawfiles_rdata <- NULL
  #   }
  #
  #   # Dispatch Modal
  #   Quantico:::AzureBlob_Modal_Fun(
  #     id = 'AzureBlobDMID',
  #
  #     # Statics
  #     AzureBlobStorageTabular_Choices = rawfiles_csv,
  #     AzureBlobStorageRdata_Choices = rawfiles_rdata,
  #
  #     # Updaters
  #     AzureBlobStorageTabular_Selected = if(length(input$AzureBlobStorageTabular) > 0L) input$AzureBlobStorageTabular else NULL,
  #     AzureBlobStorageRdata_Selected = if(length(input$AzureBlobStorageRdata) > 0L) input$AzureBlobStorageRdata else NULL)
  # })
  # shiny::observeEvent(input$AzureBlob_OK, {shiny::removeModal()})

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Inputs :: Data Wrangling          ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Shrink
  shiny::observeEvent(input$Shrink_Modal, {
    Quantico:::Shrink_Modal_Fun(
      id = 'ShrinkDWID',

      # Reactives
      Aggregate_Columns_Selected = if(length(input$Aggregate_Columns) > 0L) input$Aggregate_Columns else NULL,
      Aggregate_ByVariables_Selected = if(length(input$Aggregate_ByVariables) > 0L) input$Aggregate_ByVariables else NULL,
      Aggregate_DateVariable_Selected = if(length(input$Aggregate_DateVariable) > 0L) input$Aggregate_DateVariable else NULL,

      SubsetData_FilterVariable1_Selected = if(length(input$SubsetData_FilterVariable1) > 0L) input$SubsetData_FilterVariable1 else NULL,
      SubsetData_FilterVariable2_Selected = if(length(input$SubsetData_FilterVariable2) > 0L) input$SubsetData_FilterVariable2 else NULL,
      SubsetData_FilterVariable3_Selected = if(length(input$SubsetData_FilterVariable3) > 0L) input$SubsetData_FilterVariable3 else NULL,
      SubsetData_FilterVariable4_Selected = if(length(input$SubsetData_FilterVariable4) > 0L) input$SubsetData_FilterVariable4 else NULL,
      SubsetData_FilterValue_1_1_Selected = if(length(input$SubsetData_FilterValue_1_1) > 0L) input$SubsetData_FilterValue_1_1 else NULL,
      SubsetData_FilterValue_1_2_Selected = if(length(input$SubsetData_FilterValue_1_2) > 0L) input$SubsetData_FilterValue_1_2 else NULL,
      SubsetData_FilterValue_2_1_Selected = if(length(input$SubsetData_FilterValue_2_1) > 0L) input$SubsetData_FilterValue_2_1 else NULL,
      SubsetData_FilterValue_2_2_Selected = if(length(input$SubsetData_FilterValue_2_2) > 0L) input$SubsetData_FilterValue_2_2 else NULL,
      SubsetData_FilterValue_3_1_Selected = if(length(input$SubsetData_FilterValue_3_1) > 0L) input$SubsetData_FilterValue_3_1 else NULL,
      SubsetData_FilterValue_3_2_Selected = if(length(input$SubsetData_FilterValue_3_2) > 0L) input$SubsetData_FilterValue_3_2 else NULL,
      SubsetData_FilterValue_4_1_Selected = if(length(input$SubsetData_FilterValue_4_1) > 0L) input$SubsetData_FilterValue_4_1 else NULL,
      SubsetData_FilterValue_4_2_Selected = if(length(input$SubsetData_FilterValue_4_2) > 0L) input$SubsetData_FilterValue_4_2 else NULL,
      DeleteVariables_Selected = if(length(input$DeleteVariables) > 0L) input$DeleteVariables else NULL,
      SampleData_StratifyColumns_Selected = if(length(input$SampleData_StratifyColumns) > 0L) input$SampleData_StratifyColumns else NULL,

      # Statics
      AggregateData_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      Aggregate_Stat_Choices = c('mean','count','median','sd','max','min','first','last'),
      Aggregate_TimeAgg_Choices = c("second", "minute", "hour", "day", "week", "month", "bimonth", "quarter", "season", "halfyear", "year"),
      SubsetData_FilterLogic1_Choices = c('<','>','<=','>=','%in%','%like%','%between%','not %between%'),
      SubsetData_FilterLogic2_Choices = c('<','>','<=','>=','%in%','%like%','%between%','not %between%'),
      SubsetData_FilterLogic3_Choices = c('<','>','<=','>=','%in%','%like%','%between%','not %between%'),
      SubsetData_FilterLogic4_Choices = c('<','>','<=','>=','%in%','%like%','%between%','not %between%'),
      SubsetData_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      DeleteVariables_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      SampleData_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      SampleData_Rate_Choices = rev(seq(0.01,1.0,0.01)),

      # Updaters
      AggregateData_SelectData_Selected = if(length(input$AggregateData_SelectData) > 0L) input$AggregateData_SelectData else NULL,
      AggregateData_NewName_Selected = if(length(input$AggregateData_NewName) > 0L) input$AggregateData_NewName else NULL,
      Aggregate_Stat_Selected = if(length(input$Aggregate_Stat) > 0L) input$Aggregate_Stat else "mean",
      Aggregate_TimeAgg_Selected = if(length(input$Aggregate_TimeAgg) > 0L) input$Aggregate_TimeAgg else NULL,
      SubsetData_FilterLogic1_Selected = if(length(input$SubsetData_FilterLogic1) > 0L) input$SubsetData_FilterLogic1 else "<",
      SubsetData_FilterLogic2_Selected = if(length(input$SubsetData_FilterLogic2) > 0L) input$SubsetData_FilterLogic2 else "<",
      SubsetData_FilterLogic3_Selected = if(length(input$SubsetData_FilterLogic3) > 0L) input$SubsetData_FilterLogic3 else "<",
      SubsetData_FilterLogic4_Selected = if(length(input$SubsetData_FilterLogic4) > 0L) input$SubsetData_FilterLogic4 else "<",
      SubsetData_SelectData_Selected = if(length(input$SubsetData_SelectData) > 0L) input$SubsetData_SelectData else NULL,
      SubsetData_NewName_Selected = if(length(input$SubsetData_NewName) > 0L) input$SubsetData_NewName else NULL,
      DeleteVariables_SelectData_Selected = if(length(input$DeleteVariables_SelectData) > 0L) input$DeleteVariables_SelectData else NULL,
      DeleteVariables_NewName_Selected = if(length(input$DeleteVariables_NewName) > 0L) input$DeleteVariables_NewName else NULL,
      SampleData_SelectData_Selected = if(length(input$SampleData_SelectData) > 0L) input$SampleData_SelectData else NULL,
      SampleData_NewName_Selected = if(length(input$SampleData_NewName) > 0L) input$SampleData_NewName else NULL,
      SampleData_Rate_Selected = if(length(input$SampleData_Rate) > 0L) input$SampleData_Rate else 1)

    # Reactives
    AggregateData_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$AggregateData_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(AggregateData_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(AggregateData_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(AggregateData_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='Aggregate_Columns', Label='Aggregate Columns', Choices = ChoiceList, SelectedDefault = if(length(input$Aggregate_Columns) > 0L) input$Aggregate_Columns else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='Aggregate_ByVariables', Label='Aggregate By-Columns', Choices = ChoiceList, SelectedDefault = if(length(input$Aggregate_ByVariables) > 0L) input$Aggregate_ByVariables else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='Aggregate_DateVariable', Label='Aggregate Date Column', Choices = ChoiceList, SelectedDefault = if(length(input$Aggregate_DateVariable) > 0L) input$Aggregate_DateVariable else NULL, Multiple = TRUE)
    })

    SubData <- shiny::reactive({shiny::req(tryCatch({DataList[[input$SubsetData_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(SubData(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(SubData()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(SubData(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = 'SubsetData_FilterVariable1', Label = 'Filter Variable 1', Choices = ChoiceList, SelectedDefault = if(length(input$SubsetData_FilterVariable1) > 0L) input$SubsetData_FilterVariable1 else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = 'SubsetData_FilterVariable2', Label = 'Filter Variable 2', Choices = ChoiceList, SelectedDefault = if(length(input$SubsetData_FilterVariable2) > 0L) input$SubsetData_FilterVariable2 else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = 'SubsetData_FilterVariable3', Label = 'Filter Variable 3', Choices = ChoiceList, SelectedDefault = if(length(input$SubsetData_FilterVariable3) > 0L) input$SubsetData_FilterVariable3 else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = 'SubsetData_FilterVariable4', Label = 'Filter Variable 4', Choices = ChoiceList, SelectedDefault = if(length(input$SubsetData_FilterVariable4) > 0L) input$SubsetData_FilterVariable4 else NULL, Multiple = TRUE, MaxVars = 1L)
      SubsetData_FilterVariable1 <- shiny::reactive({shiny::req(input[['SubsetData_FilterVariable1']])})
      SubsetData_FilterVariable2 <- shiny::reactive({shiny::req(input[['SubsetData_FilterVariable2']])})
      SubsetData_FilterVariable3 <- shiny::reactive({shiny::req(input[['SubsetData_FilterVariable3']])})
      SubsetData_FilterVariable4 <- shiny::reactive({shiny::req(input[['SubsetData_FilterVariable4']])})

      # Values 1
      shiny::observeEvent(SubsetData_FilterVariable1(), {
        zz <- tryCatch({SubData()}, error = function(x) NULL)
        zzz <- tryCatch({SubsetData_FilterVariable1()}, error = function(x) NULL)
        choices <- tryCatch({Quantico:::KeyVarsInit(SubData(), VarName = SubsetData_FilterVariable1())$ChoiceInput}, error = function(x) NULL)
        Quantico:::SelectizeInput(session = session, Update = TRUE, InputID = 'SubsetData_FilterValue_1_1', Label = "Value 1", Choices = choices, SelectedDefault = if(length(input$SubsetData_FilterValue_1_1) > 0L) input$SubsetData_FilterValue_1_1 else NULL, Multiple = TRUE, MaxVars = 100L, Debug = Debug)
        choices <- tryCatch({Quantico:::KeyVarsInit(SubData(), VarName = SubsetData_FilterVariable1())$ChoiceInput}, error = function(x) NULL)
        Quantico:::SelectizeInput(session = session, Update = TRUE, InputID = 'SubsetData_FilterValue_1_2', Label = "Value 2", Choices = choices, SelectedDefault = if(length(input$SubsetData_FilterValue_1_2) > 0L) input$SubsetData_FilterValue_1_2 else NULL, Multiple = TRUE, MaxVars = 100L, Debug = Debug)
      })

      # Values 2
      shiny::observeEvent(SubsetData_FilterVariable2(), {
        zz <- tryCatch({SubData()}, error = function(x) NULL)
        zzz <- tryCatch({SubsetData_FilterVariable2()}, error = function(x) NULL)
        choices <- tryCatch({Quantico:::KeyVarsInit(SubData(), VarName = zzz)$ChoiceInput}, error = function(x) NULL)
        Quantico:::SelectizeInput(session = session, Update = TRUE, InputID = 'SubsetData_FilterValue_2_1', Label = "Value 1", Choices = choices, SelectedDefault = if(length(input$SubsetData_FilterValue_2_1) > 0L) input$SubsetData_FilterValue_2_1 else NULL, Multiple = TRUE, MaxVars = 100L, Debug = Debug)
        choices <- tryCatch({Quantico:::KeyVarsInit(SubData(), VarName = zzz)$ChoiceInput}, error = function(x) NULL)
        Quantico:::SelectizeInput(session = session, Update = TRUE, InputID = 'SubsetData_FilterValue_2_2', Label = "Value 2", Choices = choices, SelectedDefault = if(length(input$SubsetData_FilterValue_2_2) > 0L) input$SubsetData_FilterValue_2_2 else NULL, Multiple = TRUE, MaxVars = 100L, Debug = Debug)
      })

      # Values 3
      shiny::observeEvent(SubsetData_FilterVariable3(), {
        zz <- tryCatch({SubData()}, error = function(x) NULL)
        zzz <- tryCatch({SubsetData_FilterVariable3()}, error = function(x) NULL)
        choices <- tryCatch({Quantico:::KeyVarsInit(SubData(), VarName = zzz)$ChoiceInput}, error = function(x) NULL)
        Quantico:::SelectizeInput(session = session, Update = TRUE, InputID = 'SubsetData_FilterValue_3_1', Label = "Value 1", Choices = choices, SelectedDefault = if(length(input$SubsetData_FilterValue_3_1) > 0L) input$SubsetData_FilterValue_3_1 else NULL, Multiple = TRUE, MaxVars = 100L, Debug = Debug)
        choices <- tryCatch({Quantico:::KeyVarsInit(SubData(), VarName = zzz)$ChoiceInput}, error = function(x) NULL)
        Quantico:::SelectizeInput(session = session, Update = TRUE, InputID = 'SubsetData_FilterValue_3_2', Label = "Value 2", Choices = choices, SelectedDefault = if(length(input$SubsetData_FilterValue_3_2) > 0L) input$SubsetData_FilterValue_3_2 else NULL, Multiple = TRUE, MaxVars = 100L, Debug = Debug)
      })

      # Values 4
      shiny::observeEvent(SubsetData_FilterVariable4(), {
        zz <- tryCatch({SubData()}, error = function(x) NULL)
        zzz <- tryCatch({SubsetData_FilterVariable4()}, error = function(x) NULL)
        choices <- tryCatch({Quantico:::KeyVarsInit(SubData(), VarName = zzz)$ChoiceInput}, error = function(x) NULL)
        Quantico:::SelectizeInput(session = session, Update = TRUE, InputID = 'SubsetData_FilterValue_4_1', Label = "Value 1", Choices = choices, SelectedDefault = if(length(input$SubsetData_FilterValue_4_1) > 0L) input$SubsetData_FilterValue_4_1 else NULL, Multiple = TRUE, MaxVars = 100L, Debug = Debug)
        choices <- tryCatch({Quantico:::KeyVarsInit(SubData(), VarName = zzz)$ChoiceInput}, error = function(x) NULL)
        Quantico:::SelectizeInput(session = session, Update = TRUE, InputID = 'SubsetData_FilterValue_4_2', Label = "Value 2", Choices = choices, SelectedDefault = if(length(input$SubsetData_FilterValue_4_2) > 0L) input$SubsetData_FilterValue_4_2 else NULL, Multiple = TRUE, MaxVars = 100L, Debug = Debug)
      })
    })

    DeleteVariables_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$DeleteVariables_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(DeleteVariables_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(DeleteVariables_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(DeleteVariables_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='DeleteVariables', Label='Delete Columns', Choices = ChoiceList, SelectedDefault = if(length(input$DeleteVariables) > 0L) input$DeleteVariables else NULL, Multiple = TRUE)
    })

    SampleData_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$SampleData_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(SampleData_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(SampleData_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(SampleData_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='SampleData_StratifyColumns', Label='Stratify Variables', Choices = ChoiceList, SelectedDefault = if(length(input$SampleData_StratifyColumns) > 0L) input$SampleData_StratifyColumns else NULL, Multiple = TRUE)
    })
  })
  shiny::observeEvent(input$Shrink_OK, {
    shiny::removeModal()
  })

  # Grow
  shiny::observeEvent(input$Grow_Modal, {
    Quantico:::Grow_Modal_Fun(
      id = 'GrowDWID',

      # Reactives
      JoinData_ByXVariables_Selected = if(length(input$JoinData_ByXVariables) > 0L) input$JoinData_ByXVariables else NULL,
      JoinData_ByYVariables_Selected = if(length(input$JoinData_ByYVariables) > 0L) input$JoinData_ByYVariables else NULL,

      # Statics
      JoinData_SelectData1_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      JoinData_SelectData2_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      JoinData_JoinType_Choices = c('inner','left','full','anti','semi','cross','RollForward','RollBackward'),
      UnionData_SelectData1_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      UnionData_SelectData2_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      UnionData_Fill_Choices = c(TRUE,FALSE),
      UnionData_UseNames_Choices = c(TRUE,FALSE),

      # Updaters
      JoinData_NewName_Selected = if(length(input$JoinData_NewName) > 0L) input$JoinData_NewName else NULL,
      JoinData_SelectData1_Selected = if(length(input$JoinData_SelectData1) > 0L) input$JoinData_SelectData1 else NULL,
      JoinData_SelectData2_Selected = if(length(input$JoinData_SelectData2) > 0L) input$JoinData_SelectData2 else NULL,
      JoinData_JoinType_Selected = if(length(input$JoinData_JoinType) > 0L) input$JoinData_JoinType else "inner",
      UnionData_NewName_Selected = if(length(input$UnionData_NewName) > 0L) input$UnionData_NewName else NULL,
      UnionData_SelectData1_Selected = if(length(input$UnionData_SelectData1) > 0L) input$UnionData_SelectData1 else NULL,
      UnionData_SelectData2_Selected = if(length(input$UnionData_SelectData2) > 0L) input$UnionData_SelectData2 else NULL)

    # Reactives
    JoinData_DataReactive1 <- shiny::reactive({shiny::req(tryCatch({DataList[[input$JoinData_SelectData1]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(JoinData_DataReactive1(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(JoinData_DataReactive1()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(JoinData_DataReactive1(), Types = ColTypes[i])
      Quantico:::SelectizeInput(session = session, Update = TRUE, InputID='JoinData_ByXVariables', Label='Left Table ByVars', Choices = ChoiceList, Selected = if(length(input$JoinData_ByXVariables) > 0L) input$JoinData_ByXVariables else NULL, Multiple = TRUE, MaxVars = 1000, Debug = Debug)
    })
    JoinData_DataReactive2 <- shiny::reactive({shiny::req(tryCatch({DataList[[input$JoinData_SelectData2]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(JoinData_DataReactive2(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(JoinData_DataReactive2()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(JoinData_DataReactive2(), Types = ColTypes[i])
      Quantico:::SelectizeInput(session = session, Update = TRUE, InputID='JoinData_ByYVariables', Label='Right Table ByVars', Choices = ChoiceList, Selected = if(length(input$JoinData_ByYVariables) > 0L) input$JoinData_ByYVariables else NULL, Multiple = TRUE, MaxVars = 1000, Debug = Debug)
    })

  })
  shiny::observeEvent(input$Grow_OK, {shiny::removeModal()})

  # Data Sets
  shiny::observeEvent(input$DataSets_Modal, {
    Quantico:::DataSets_Modal_Fun(
      id = 'DataSetsFEID',

      # Reactives
      AutoDataPartition_StratifyColumnNames_Selected = if(length(input$AutoDataPartition_StratifyColumnNames) > 0L) input$AutoDataPartition_StratifyColumnNames else NULL,
      AutoDataPartition_TimeColumnName_Selected = if(length(input$AutoDataPartition_TimeColumnName) > 0L) input$AutoDataPartition_TimeColumnName else NULL,
      SortData_SortColumns_Selected = if(length(input$SortData_SortColumns) > 0L) input$SortData_SortColumns else NULL,
      ModelDataPrep_IgnoreCols_Selected = if(length(input$ModelDataPrep_IgnoreCols) > 0L) input$ModelDataPrep_IgnoreCols else NULL,

      # Statics
      AutoDataPartition_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      AutoDataPartition_NumDataSets_Choices = c(2,3),
      AutoDataPartition_Ratios_Train_Choices = c(seq(0.50, 0.95, 0.05)),
      AutoDataPartition_Ratios_Validation_Choices = c(seq(0.0, 0.50, 0.05)),
      AutoDataPartition_Ratios_Test_Choices = c(seq(0.0, 0.50, 0.05)),
      AutoDataPartition_PartitionType_Choices = c('random', 'time'),
      SortData_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      SortData_SortOrder_Choices = c('Ascending','Descending'),
      ModelDataPrep_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      ModelDataPrep_CharToFactor_Choices = c(FALSE,TRUE),
      ModelDataPrep_FactorToChar_Choices = c(FALSE,TRUE),
      ModelDataPrep_DateToChar_Choices = c(FALSE,TRUE),
      ModelDataPrep_IDateConversion_Choices = c(FALSE,TRUE),
      ModelDataPrep_RemoveDates_Choices = c(FALSE,TRUE),
      ModelDataPrep_IntToNumeric_Choices = c(FALSE,TRUE),
      ModelDataPrep_LogicalToBinary_Choices = c(FALSE,TRUE),
      RemoveData_Datasets_Choices = tryCatch({names(DataList)}, error = function(x) NULL),

      # Updaters
      AutoDataPartition_SelectData_Selected = if(length(input$AutoDataPartition_SelectData) > 0L) input$AutoDataPartition_SelectData else NULL,
      AutoDataPartition_NumDataSets_Selected = if(length(input$AutoDataPartition_NumDataSets) > 0L) input$AutoDataPartition_NumDataSets else 3,
      AutoDataPartition_Ratios_Train_Selected = if(length(input$AutoDataPartition_Ratios_Train) > 0L) input$AutoDataPartition_Ratios_Train else 0.70,
      AutoDataPartition_Ratios_Validation_Selected = if(length(input$AutoDataPartition_Ratios_Validation) > 0L) input$AutoDataPartition_Ratios_Validation else 0.20,
      AutoDataPartition_Ratios_Test_Selected = if(length(input$AutoDataPartition_Ratios_Test) > 0L) input$AutoDataPartition_Ratios_Test else 0.10,
      AutoDataPartition_PartitionType_Selected = if(length(input$AutoDataPartition_PartitionType) > 0L) input$AutoDataPartition_PartitionType else 'random',
      SortData_SelectData_Selected = if(length(input$SortData_SelectData) > 0L) input$SortData_SelectData else NULL,
      SortData_SortOrder_Selected = if(length(input$SortData_SortOrder) > 0L) input$SortData_SortOrder else 'Ascending',
      ModelDataPrep_SelectData_Selected = if(length(input$ModelDataPrep_SelectData) > 0L) input$ModelDataPrep_SelectData else NULL,
      ModelDataPrep_CharToFactor_Selected = if(length(input$ModelDataPrep_CharToFactor) > 0L) input$ModelDataPrep_CharToFactor else FALSE,
      ModelDataPrep_FactorToChar_Selected = if(length(input$ModelDataPrep_FactorToChar) > 0L) input$ModelDataPrep_FactorToChar else FALSE,
      ModelDataPrep_DateToChar_Selected = if(length(input$ModelDataPrep_DateToChar) > 0L) input$ModelDataPrep_DateToChar else FALSE,
      ModelDataPrep_IDateConversion_Selected = if(length(input$ModelDataPrep_IDateConversion) > 0L) input$ModelDataPrep_IDateConversion else FALSE,
      ModelDataPrep_RemoveDates_Selected = if(length(input$ModelDataPrep_RemoveDates) > 0L) input$ModelDataPrep_RemoveDates else FALSE,
      ModelDataPrep_IntToNumeric_Selected = if(length(input$ModelDataPrep_IntToNumeric) > 0L) input$ModelDataPrep_IntToNumeric else FALSE,
      ModelDataPrep_LogicalToBinary_Selected = if(length(input$ModelDataPrep_LogicalToBinary) > 0L) input$ModelDataPrep_LogicalToBinary else FALSE,
      RemoveData_Datasets_Selected = if(length(input$RemoveData_Datasets) > 0L) input$RemoveData_Datasets else NULL)

    # Reactives
    AutoDataPartition_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$AutoDataPartition_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(AutoDataPartition_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(AutoDataPartition_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(AutoDataPartition_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='AutoDataPartition_StratifyColumnNames', Label='Stratify Variable', Choices = ChoiceList, SelectedDefault = if(length(input$AutoDataPartition_StratifyColumnNames) > 0L) input$AutoDataPartition_StratifyColumnNames else NULL, Multiple = TRUE, Debug = Debug)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='AutoDataPartition_TimeColumnName', Label='Date Variable', Choices = ChoiceList, SelectedDefault = if(length(input$AutoDataPartition_TimeColumnName) > 0L) input$AutoDataPartition_TimeColumnName else NULL, Multiple = TRUE, Debug = Debug)
    })

    SortData_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$SortData_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(SortData_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(SortData_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(SortData_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='SortData_SortColumns', Label='Sort Columns', Choices = ChoiceList, SelectedDefault = if(length(input$SortData_SortColumns) > 0L) input$SortData_SortColumns else NULL, Multiple = TRUE, Debug = Debug)
    })

    ModelDataPrep_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$ModelDataPrep_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(ModelDataPrep_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(ModelDataPrep_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(ModelDataPrep_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='ModelDataPrep_IgnoreCols', Label='Skip Columns', Choices = ChoiceList, SelectedDefault = if(length(input$ModelDataPrep_IgnoreCols) > 0L) input$ModelDataPrep_IgnoreCols else NULL, Multiple = TRUE, Debug = Debug)
    })

  }, ignoreInit = TRUE)
  shiny::observeEvent(input$DataSets_OK, {shiny::removeModal()}, ignoreInit = TRUE)

  # Pivot
  shiny::observeEvent(input$Pivot_Modal, {
    Quantico:::Pivot_Modal_Fun(
      id = 'PivotDWID',

      # Reactives
      MeltData_id.vars_Selected = if(length(input$MeltData_id.vars) > 0L) input$MeltData_id.vars else NULL,
      MeltData_measure.vars_Selected = if(length(input$MeltData_measure.vars) > 0L) input$MeltData_measure.vars else NULL,
      CastData_id.vars_Selected = if(length(input$CastData_id.vars) > 0L) input$CastData_id.vars else NULL,
      CastData_CastColumns_Selected = if(length(input$CastData_CastColumns) > 0L) input$CastData_CastColumns else NULL,
      CastData_value.var_Selected = if(length(input$CastData_value.var) > 0L) input$CastData_value.var else NULL,

      # Statics
      MeltData_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      MeltData_na.rm_Choices = c(FALSE,TRUE),
      MeltData_variable.factor_Choices = c(FALSE,TRUE),
      MeltData_value.factor_Choices = c(FALSE,TRUE),
      CastData_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      CastData_fun.aggregate_Choices = c('length','sum','mean','median','sd','max','min','data.table::first','data.table::last'),
      CastData_fill_Choices = -10:10,

      # Updaters
      MeltData_NewName_Selected = if(length(input$MeltData_NewName) > 0L) input$MeltData_NewName else NULL,
      MeltData_SelectData_Selected = if(length(input$MeltData_SelectData) > 0L) input$MeltData_SelectData else NULL,
      MeltData_na.rm_Selected = if(length(input$MeltData_na.rm) > 0L) input$MeltData_na.rm else FALSE,
      MeltData_variable.name_Selected = if(length(input$MeltData_variable.name) > 0L) input$MeltData_variable.name else NULL,
      MeltData_value.name_Selected = if(length(input$MeltData_value.name) > 0L) input$MeltData_value.name else NULL,
      MeltData_variable.factor_Selected = if(length(input$MeltData_variable.factor) > 0L) input$MeltData_variable.factor else FALSE,
      MeltData_value.factor_Selected = if(length(input$MeltData_value.factor) > 0L) input$MeltData_value.factor else FALSE,
      CastData_SelectData_Selected = if(length(input$CastData_SelectData) > 0L) input$CastData_SelectData else NULL,
      CastData_NewName_Selected = if(length(input$CastData_NewName) > 0L) input$CastData_NewName else NULL,
      CastData_fun.aggregate_Selected = if(length(input$CastData_fun.aggregate) > 0L) input$CastData_fun.aggregate else "mean",
      CastData_fill_Selected = if(length(input$CastData_fill) > 0L) input$CastData_fill else -1)

    # Reactives
    MeltData_DataReactive <- shiny::reactive({tryCatch({DataList[[input$MeltData_SelectData]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(MeltData_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(MeltData_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(MeltData_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='MeltData_id.vars', Label='ID Columns', Choices = ChoiceList, SelectedDefault = if(length(input$MeltData_id.vars) > 0L) input$MeltData_id.vars else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='MeltData_measure.vars', Label='Measure Columns', Choices = ChoiceList, SelectedDefault = if(length(input$MeltData_measure.vars) > 0L) input$MeltData_measure.vars else NULL, Multiple = TRUE)
    })

    CastData_DataReactive <- shiny::reactive({tryCatch({DataList[[input$CastData_SelectData]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(CastData_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(CastData_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(CastData_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='CastData_id.vars', Label='LHS Columns', Choices = ChoiceList, SelectedDefault = if(length(input$CastData_id.vars) > 0L) input$CastData_id.vars else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='CastData_CastColumns', Label='RHS Columns', Choices = ChoiceList, SelectedDefault = if(length(input$CastData_CastColumns) > 0L) input$CastData_CastColumns else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='CastData_value.var', Label='Values Variable', Choices = ChoiceList, SelectedDefault = if(length(input$CastData_value.var) > 0L) input$CastData_value.var else NULL, Multiple = TRUE)
    })

  })
  shiny::observeEvent(input$Pivot_OK, {shiny::removeModal()})

  # Columns
  shiny::observeEvent(input$Columns_Modal, {
    Quantico:::Columns_Modal_Fun(
      id = 'ColumnsDWID',

      # Reactives
      TypeCast_Numeric_Selected = if(length(input$TypeCast_Numeric) > 0L) input$TypeCast_Numeric else NULL,
      TypeCast_Integer_Selected = if(length(input$TypeCast_Integer) > 0L) input$TypeCast_Integer else NULL,
      TypeCast_Character_Selected = if(length(input$TypeCast_Character) > 0L) input$TypeCast_Character else NULL,
      TypeCast_Factor_Selected = if(length(input$TypeCast_Factor) > 0L) input$TypeCast_Factor else NULL,
      TypeCast_Logical_Selected = if(length(input$TypeCast_Logical) > 0L) input$TypeCast_Logical else NULL,
      TypeCast_Date_Selected = if(length(input$TypeCast_Date) > 0L) input$TypeCast_Date else NULL,
      TypeCast_ExcelDate_Selected = if(length(input$TypeCast_ExcelDate) > 0L) input$TypeCast_ExcelDate else NULL,
      TypeCast_Posix_Selected = if(length(input$TypeCast_Posix) > 0L) input$TypeCast_Posix else NULL,
      RenameColumns_RenameColumn_Selected = if(length(input$RenameColumns_RenameColumn) > 0L) input$RenameColumns_RenameColumn else NULL,
      TimeTrendColumn_DateColumn_Selected = if(length(input$TimeTrendColumn_DateColumn) > 0L) input$TimeTrendColumn_DateColumn else NULL,
      TimeTrendColumn_GroupVars_Selected = if(length(input$TimeTrendColumn_GroupVars) > 0L) input$TimeTrendColumn_GroupVars else NULL,
      ConcatColumns_Selected = if(length(input$ConcatColumns) > 0L) input$ConcatColumns else NULL,

      # Statics
      TypeCast_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      RenameColumns_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      TimeTrendColumn_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      TimeTrendColumn_TimeTrendOrder_Choices = c("Ascending","Descending"),
      ConcatColumns_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),

      # Updaters
      TypeCast_SelectData_Selected = if(length(input$TypeCast_SelectData) > 0L) input$TypeCast_SelectData else NULL,
      RenameColumns_SelectData_Selected = if(length(input$RenameColumns_SelectData) > 0L) input$RenameColumns_SelectData else NULL,
      RenameColumns_NewName_Selected = if(length(input$RenameColumns_NewName) > 0L) input$RenameColumns_NewName else NULL,
      TimeTrendColumn_SelectData_Selected = if(length(input$TimeTrendColumn_SelectData) > 0L) input$TimeTrendColumn_SelectData else NULL,
      TimeTrendColumn_TimeTrendOrder_Selected = if(length(input$TimeTrendColumn_TimeTrendOrder) > 0L) input$TimeTrendColumn_TimeTrendOrder else "Ascending",
      TimeTrendColumn_NewName_Selected = if(length(input$TimeTrendColumn_NewName) > 0L) input$TimeTrendColumn_NewName else NULL,
      ConcatColumns_SelectData_Selected = if(length(input$ConcatColumns_SelectData) > 0L) input$ConcatColumns_SelectData else NULL)

    # Reactives
    TypeCast_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$TypeCast_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(TypeCast_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(TypeCast_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(TypeCast_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='TypeCast_Numeric', Label='Convert Numeric', Choices = ChoiceList, SelectedDefault = if(length(input$TypeCast_Numeric) > 0L) input$TypeCast_Numeric else NULL, Multiple = TRUE, Debug = Debug)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='TypeCast_Integer', Label='Convert Integer', Choices = ChoiceList, SelectedDefault = if(length(input$TypeCast_Integer) > 0L) input$TypeCast_Integer else NULL, Multiple = TRUE, Debug = Debug)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='TypeCast_Character', Label='Convert Character', Choices = ChoiceList, SelectedDefault = if(length(input$TypeCast_Character) > 0L) input$TypeCast_Character else NULL, Multiple = TRUE, Debug = Debug)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='TypeCast_Factor', Label='Convert Factor', Choices = ChoiceList, SelectedDefault = if(length(input$TypeCast_Factor) > 0L) input$TypeCast_Factor else NULL, Multiple = TRUE, Debug = Debug)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='TypeCast_Logical', Label='Convert Logical', Choices = ChoiceList, SelectedDefault = if(length(input$TypeCast_Logical) > 0L) input$TypeCast_Logical else NULL, Multiple = TRUE, Debug = Debug)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='TypeCast_Date', Label='Convert Date', Choices = ChoiceList, SelectedDefault = if(length(input$TypeCast_Date) > 0L) input$TypeCast_Date else NULL, Multiple = TRUE, Debug = Debug)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='TypeCast_ExcelDate', Label='Excel Date to R Date', Choices = ChoiceList, SelectedDefault = if(length(input$TypeCast_ExcelDate) > 0L) input$TypeCast_ExcelDate else NULL, Multiple = TRUE, Debug = Debug)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='TypeCast_Posix', Label='Convert Posix', Choices = ChoiceList, SelectedDefault = if(length(input$TypeCast_Posix) > 0L) input$TypeCast_Posix else NULL, Multiple = TRUE, Debug = Debug)
    })

    RenameColumns_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$RenameColumns_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(RenameColumns_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(RenameColumns_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(RenameColumns_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='RenameColumns_RenameColumn', Label='Select Column', Choices = ChoiceList, SelectedDefault = if(length(input$RenameColumns_RenameColumn) > 0L) input$RenameColumns_RenameColumn else NULL, Multiple = TRUE)
    })

    TimeTrendColumn_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$TimeTrendColumn_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(TimeTrendColumn_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(TimeTrendColumn_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(TimeTrendColumn_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='TimeTrendColumn_DateColumn', Label='Date Column', Choices = ChoiceList, SelectedDefault = if(length(input$TimeTrendColumn_DateColumn) > 0L) input$TimeTrendColumn_DateColumn else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='TimeTrendColumn_GroupVars', Label='Group Variables', Choices = ChoiceList, SelectedDefault = if(length(input$TimeTrendColumn_GroupVars) > 0L) input$TimeTrendColumn_GroupVars else NULL, Multiple = TRUE)
    })

    ConcatColumns_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$ConcatColumns_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(ConcatColumns_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(ConcatColumns_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(ConcatColumns_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='ConcatColumns', Label='Concat Columns', Choices = ChoiceList, SelectedDefault = if(length(input$ConcatColumns) > 0L) input$ConcatColumns else NULL, Multiple = TRUE)
    })
  })
  shiny::observeEvent(input$Columns_OK, {shiny::removeModal()})

  # Misc
  shiny::observeEvent(input$Misc_Modal, {
    Quantico:::Misc_Modal_Fun(
      id = 'MiscDWID',

      # Reactives
      TSF_DateColumnName_Selected = if(length(input$TSF_DateColumnName) > 0L) input$TSF_DateColumnName else NULL,
      TSF_GroupVariables_Selected = if(length(input$TSF_GroupVariables) > 0L) input$TSF_GroupVariables else NULL,
      Roll_RollVars_Selected = if(length(input$Roll_RollVars) > 0L) input$Roll_RollVars else NULL,
      Roll_NonRollVars_Selected = if(length(input$Roll_NonRollVars) > 0L) input$Roll_NonRollVars else NULL,
      Roll_DateColumnName_Selected = if(length(input$Roll_DateColumnName) > 0L) input$Roll_DateColumnName else NULL,
      Roll_GroupVariables_Selected = if(length(input$Roll_GroupVariables) > 0L) input$Roll_GroupVariables else NULL,

      # Statics
      MetaProgramming_TextCode_Choices = NULL,
      TSF_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      TSF_TimeUnit_Choices =  c("second", "minute", "hour", "day", "week", "month", "quarter", "year"),
      TSF_MaxMissingPercent_Choices = seq(0.01,0.99,0.01),
      TSF_SimpleImpute_Choices = c(0,-1,-10,-100,-1000),
      Roll_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      Roll_TimeUnit_Choices =  c("second", "minute", "hour", "day", "week", "month", "quarter", "year"),
      Roll_RollDirection_Choices = c('forward','backward'),
      Roll_SimpleImpute_Choices = c(0,-1,-10,-100,-1000),

      # Updaters
      TSF_SelectData_Selected = if(length(input$TSF_SelectData) > 0L) input$TSF_SelectData else NULL,
      TSF_NewName_Selected = if(length(input$TSF_NewName) > 0L) input$TSF_NewName else NULL,
      TSF_TimeUnit_Selected = if(length(input$TSF_TimeUnit) > 0L) input$TSF_TimeUnit else "day",
      TSF_MaxMissingPercent_Selected = if(length(input$TSF_MaxMissingPercent) > 0L) input$TSF_MaxMissingPercent else 0.50,
      TSF_SimpleImpute_Selected = if(length(input$TSF_SimpleImpute) > 0L) input$TSF_SimpleImpute else -1,
      Roll_SelectData_Selected = if(length(input$Roll_SelectData) > 0L) input$Roll_SelectData else NULL,
      Roll_NewName_Selected = if(length(input$Roll_NewName) > 0L) input$Roll_NewName else NULL,
      Roll_TimeUnit_Selected = if(length(input$Roll_TimeUnit) > 0L) input$Roll_TimeUnit else "day",
      Roll_RollDirection_Selected = if(length(input$Roll_RollDirection) > 0L) input$Roll_RollDirection else "forward",
      Roll_SimpleImpute_Selected = if(length(input$Roll_SimpleImpute) > 0L) input$Roll_SimpleImpute else -1)

    # Reactives
    shiny::observe({
      shinyAce::updateAceEditor(session, editorId = "MetaProgramming_TextCode", mode = "r", theme = input$editor_theme1)
    })

    TSF_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$TSF_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(TSF_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(TSF_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(TSF_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='TSF_DateColumnName', Label='Date Column', Choices = ChoiceList, SelectedDefault = if(length(input$TSF_DateColumnName) > 0L) input$TSF_DateColumnName else NULL, Multiple = TRUE, Debug = Debug)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='TSF_GroupVariables', Label='Grouping Columns', Choices = ChoiceList, SelectedDefault = if(length(input$TSF_GroupVariables) > 0L) input$TSF_GroupVariables else NULL, Multiple = TRUE, Debug = Debug)
    })

    Roll_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$Roll_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(Roll_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(Roll_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(Roll_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='Roll_RollVars', Label='Roll Vars', Choices = ChoiceList, SelectedDefault = if(length(input$Roll_RollVars) > 0L) input$Roll_RollVars else NULL, Multiple = TRUE, Debug = Debug)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='Roll_NonRollVars', Label='Non Roll Vars', Choices = ChoiceList, SelectedDefault = if(length(input$Roll_NonRollVars) > 0L) input$Roll_NonRollVars else NULL, Multiple = TRUE, Debug = Debug)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='Roll_DateColumnName', Label='Date Column', Choices = ChoiceList, SelectedDefault = if(length(input$Roll_DateColumnName) > 0L) input$Roll_DateColumnName else NULL, Multiple = TRUE, Debug = Debug)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='Roll_GroupVariables', Label='Grouping Columns', Choices = ChoiceList, SelectedDefault = if(length(input$Roll_GroupVariables) > 0L) input$Roll_GroupVariables else NULL, Multiple = TRUE, Debug = Debug)
    })
  })
  shiny::observeEvent(input$Misc_OK, {shiny::removeModal()})

  # Windowing Methods
  shiny::observeEvent(input$Windowing_Modal, {
    Quantico:::Windowing_Modal_Fun(
      id = 'WindowingFEID',

      # Reactives
      AutoLagRollStats_Targets_Selected = if(length(input$AutoLagRollStats_Targets) > 0L) input$AutoLagRollStats_Targets else NULL,
      AutoLagRollStats_GroupVars_Selected = if(length(input$AutoLagRollStats_GroupVars) > 0L) input$AutoLagRollStats_GroupVars else NULL,
      AutoLagRollStats_DateColumn_Selected = if(length(input$AutoLagRollStats_DateColumn) > 0L) input$AutoLagRollStats_DateColumn else NULL,
      AutoDiffLagN_DateVariable_Selected = if(length(input$AutoDiffLagN_DateVariable) > 0L) input$AutoDiffLagN_DateVariable else NULL,
      AutoDiffLagN_GroupVariables_Selected = if(length(input$AutoDiffLagN_GroupVariables) > 0L) input$AutoDiffLagN_GroupVariables else NULL,
      AutoDiffLagN_DiffVariables_Selected = if(length(input$AutoDiffLagN_DiffVariables) > 0L) input$AutoDiffLagN_DiffVariables else NULL,
      AutoDiffLagN_DiffDateVariables_Selected = if(length(input$AutoDiffLagN_DiffDateVariables) > 0L) input$AutoDiffLagN_DiffDateVariables else NULL,
      AutoDiffLagN_DiffGroupVariables_Selected = if(length(input$AutoDiffLagN_DiffGroupVariables) > 0L) input$AutoDiffLagN_DiffGroupVariables else NULL,
      AutoLagRollMode_Targets_Selected = if(length(input$AutoLagRollMode_Targets) > 0L) input$AutoLagRollMode_Targets else NULL,
      AutoLagRollMode_GroupingVars_Selected = if(length(input$AutoLagRollMode_GroupingVars) > 0L) input$AutoLagRollMode_GroupingVars else NULL,
      AutoLagRollMode_SortDateName_Selected = if(length(input$AutoLagRollMode_SortDateName) > 0L) input$AutoLagRollMode_SortDateName else NULL,

      # Statics
      AutoLagRollStats_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      AutoLagRollStats_TimeUnits_Choices = c('raw','hour','day','week','month','quarter','year'),
      AutoLagRollStats_RollOnLag1_Choices = c(0,1),
      AutoLagRollStats_Lags_Choices = 1L:250L,
      AutoLagRollStats_MA_RollWindows_Choices = 2L:250L,
      AutoLagRollStats_SD_RollWindows_Choices = 3L:250L,
      AutoLagRollStats_Skew_RollWindows_Choices = 4L:250L,
      AutoLagRollStats_Kurt_RollWindows_Choices = 5L:250L,
      AutoLagRollStats_Quantile_RollWindows_Choices = 5L:250L,
      AutoLagRollStats_Quantiles_Selected_Choices = c('q5','q10','q15','q20','q25','q30','q35','q40','q45','q50','q55','q60','q65','q70','q75','q80','q85','q90','q95'),
      AutoDiffLagN_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      AutoDiffLagN_NLag1_Choices = 0L:365L,
      AutoDiffLagN_NLag2_Choices = 1L:365L,
      AutoLagRollMode_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      AutoLagRollMode_Lags_Choices = 2L:365L,
      AutoLagRollMode_ModePeriods_Choices = 1L:250L,
      AutoLagRollMode_WindowingLag_Choices = 0L:1L,

      # Updaters
      AutoLagRollStats_SelectData_Selected = if(length(input$AutoLagRollStats_SelectData) > 0L) input$AutoLagRollStats_SelectData else NULL,
      AutoLagRollStats_TimeUnits_Selected = if(length(input$AutoLagRollStats_TimeUnits) > 0L) input$AutoLagRollStats_TimeUnits else "day",
      AutoLagRollStats_RollOnLag1_Selected = if(length(input$AutoLagRollStats_RollOnLag1) > 0L) input$AutoLagRollStats_RollOnLag1 else TRUE,
      AutoLagRollStats_Lags_Selected = if(length(input$AutoLagRollStats_Lags) > 0L) input$AutoLagRollStats_Lags else NULL,
      AutoLagRollStats_MA_RollWindows_Selected = if(length(input$AutoLagRollStats_MA_RollWindows) > 0L) input$AutoLagRollStats_MA_RollWindows else NULL,
      AutoLagRollStats_SD_RollWindows_Selected = if(length(input$AutoLagRollStats_SD_RollWindows) > 0L) input$AutoLagRollStats_SD_RollWindows else NULL,
      AutoLagRollStats_Skew_RollWindows_Selected = if(length(input$AutoLagRollStats_Skew_RollWindows) > 0L) input$AutoLagRollStats_Skew_RollWindows else NULL,
      AutoLagRollStats_Kurt_RollWindows_Selected = if(length(input$AutoLagRollStats_Kurt_RollWindows) > 0L) input$AutoLagRollStats_Kurt_RollWindows else NULL,
      AutoLagRollStats_Quantile_RollWindows_Selected = if(length(input$AutoLagRollStats_Quantile_RollWindows) > 0L) input$AutoLagRollStats_Quantile_RollWindows else NULL,
      AutoLagRollStats_Quantiles_Selected_Selected = if(length(input$AutoLagRollStats_Quantiles_Selected) > 0L) input$AutoLagRollStats_Quantiles_Selected else NULL,
      AutoDiffLagN_SelectData_Selected = if(length(input$AutoDiffLagN_SelectData) > 0L) input$AutoDiffLagN_SelectData else NULL,
      AutoDiffLagN_NLag1_Selected = if(length(input$AutoDiffLagN_NLag1) > 0L) input$AutoDiffLagN_NLag1 else 0L,
      AutoDiffLagN_NLag2_Selected = if(length(input$AutoDiffLagN_NLag2) > 0L) input$AutoDiffLagN_NLag2 else 1L,
      AutoLagRollMode_SelectData_Selected = if(length(input$AutoLagRollMode_SelectData) > 0L) input$AutoLagRollMode_SelectData else NULL,
      AutoLagRollMode_Lags_Selected = if(length(input$AutoLagRollMode_Lags) > 0L) input$AutoLagRollMode_Lags else NULL,
      AutoLagRollMode_ModePeriods_Selected = if(length(input$AutoLagRollMode_ModePeriods) > 0L) input$AutoLagRollMode_ModePeriods else NULL,
      AutoLagRollMode_WindowingLag_Selected = if(length(input$AutoLagRollMode_WindowingLag) > 0L) input$AutoLagRollMode_WindowingLag else NULL)

    # Reactives
    AutoLagRollStats_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$AutoLagRollStats_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(AutoLagRollStats_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(AutoLagRollStats_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(AutoLagRollStats_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='AutoLagRollStats_Targets', Label='Target Variables', Choices = ChoiceList, SelectedDefault = if(length(input$AutoLagRollStats_Targets) > 0L) input$AutoLagRollStats_Targets else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='AutoLagRollStats_GroupVars', Label='By-Variables', Choices = ChoiceList, SelectedDefault = if(length(input$AutoLagRollStats_GroupVars) > 0L) input$AutoLagRollStats_GroupVars else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='AutoLagRollStats_DateColumn', Label='Date Variable', Choices = ChoiceList, SelectedDefault = if(length(input$AutoLagRollStats_DateColumn) > 0L) input$AutoLagRollStats_DateColumn else NULL, Multiple = TRUE)
    })

    AutoDiffLagN_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$AutoDiffLagN_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(AutoDiffLagN_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(AutoDiffLagN_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(AutoDiffLagN_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='AutoDiffLagN_DateVariable', Label='Date Variable', Choices = ChoiceList, SelectedDefault = if(length(input$AutoDiffLagN_DateVariable) > 0L) input$AutoDiffLagN_DateVariable else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='AutoDiffLagN_GroupVariables', Label='By-Variables', Choices = ChoiceList, SelectedDefault = if(length(input$AutoDiffLagN_GroupVariables) > 0L) input$AutoDiffLagN_GroupVariables else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='AutoDiffLagN_DiffVariables', Label='Numeric Diff Variables', Choices = ChoiceList, SelectedDefault = if(length(input$AutoDiffLagN_DiffVariables) > 0L) input$AutoDiffLagN_DiffVariables else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='AutoDiffLagN_DiffDateVariables', Label='Date Diff Variables', Choices = ChoiceList, SelectedDefault = if(length(input$AutoDiffLagN_DiffDateVariables) > 0L) input$AutoDiffLagN_DiffDateVariables else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='AutoDiffLagN_DiffGroupVariables', Label='Group Diff Variables', Choices = ChoiceList, SelectedDefault = if(length(input$AutoDiffLagN_DiffGroupVariables) > 0L) input$AutoDiffLagN_DiffGroupVariables else NULL, Multiple = TRUE)
    })

    AutoLagRollMode_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$AutoLagRollMode_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(AutoLagRollMode_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(AutoLagRollMode_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(AutoLagRollMode_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='AutoLagRollMode_Targets', Label='Target Variables', Choices = ChoiceList, SelectedDefault = if(length(input$AutoLagRollMode_Targets) > 0L) input$AutoLagRollMode_Targets else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='AutoLagRollMode_GroupingVars', Label='By-Variables', Choices = ChoiceList, SelectedDefault = if(length(input$AutoLagRollMode_GroupingVars) > 0L) input$AutoLagRollMode_GroupingVars else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='AutoLagRollMode_SortDateName', Label='Date Variable', Choices = ChoiceList, SelectedDefault = if(length(input$AutoLagRollMode_SortDateName) > 0L) input$AutoLagRollMode_SortDateName else NULL, Multiple = TRUE)
    })

  }, ignoreInit = TRUE)
  shiny::observeEvent(input$Windowing_OK, {shiny::removeModal()}, ignoreInit = TRUE)

  # CalendarVariables Inputs
  shiny::observeEvent(input$Calendar_Modal, {
    Quantico:::Calendar_Modal_Fun(
      id = 'CalendarFEID',

      # Reactives
      CalendarVariables_DateVariables_Selected = if(length(input$CalendarVariables_DateVariables) > 0L) input$CalendarVariables_DateVariables else NULL,
      HolidayVariables_DateVariables_Selected = if(length(input$HolidayVariables_DateVariables) > 0L) input$HolidayVariables_DateVariables else NULL,

      # Statics
      CalendarVariables_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      CalendarVariables_TimeUnits_Choices = c('second','minute','hour','wday','mday','yday','week','isoweek','wom','month','quarter','year'),
      HolidayVariables_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      HolidayVariables_HolidayGroups_Choices = c('USPublicHolidays','EasterGroup','ChristmasGroup','OtherEcclesticalFeasts'),
      HolidayVariables_LookbackDays_Choices = 1L:100L,

      # Updaters
      CalendarVariables_SelectData_Selected = if(length(input$CalendarVariables_SelectData) > 0L) input$CalendarVariables_SelectData else NULL,
      CalendarVariables_TimeUnits_Selected = if(length(input$CalendarVariables_TimeUnits) > 0L) input$CalendarVariables_TimeUnits else NULL,
      HolidayVariables_SelectData_Selected = if(length(input$HolidayVariables_SelectData) > 0L) input$HolidayVariables_SelectData else NULL,
      HolidayVariables_HolidayGroups_Selected = if(length(input$HolidayVariables_HolidayGroups) > 0L) input$HolidayVariables_HolidayGroups else NULL,
      HolidayVariables_LookbackDays_Selected = if(length(input$HolidayVariables_LookbackDays) > 0L) input$HolidayVariables_LookbackDays else NULL)

    # Reactives
    CalendarVariables_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$CalendarVariables_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(CalendarVariables_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(CalendarVariables_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(CalendarVariables_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='CalendarVariables_DateVariables', Label='Date Columns', Choices = ChoiceList, SelectedDefault = if(length(input$CalendarVariables_DateVariables) > 0L) input$CalendarVariables_DateVariables else NULL, Multiple = TRUE)
    })

    HolidayVariables_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$HolidayVariables_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(HolidayVariables_DataReactive(), {
      if(length(HolidayVariables_DataReactive()) != 0) bla <- c(HolidayVariables_DataReactive()) else bla <- NULL
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(bla))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(bla, Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID=   'HolidayVariables_DateVariables', Label='Date Variables', Choices = ChoiceList, SelectedDefault = if(length(input$HolidayVariables_DateVariables) > 0L) input$HolidayVariables_DateVariables else NULL, Multiple = TRUE)
    })
  })
  shiny::observeEvent(input$Calendar_OK, {shiny::removeModal()})

  # Categorical Encoding Inputs
  shiny::observeEvent(input$Categorical_Modal, {
    Quantico:::Categorical_Modal_Fun(
      id = 'CategoricalFEID',

      # Reactives
      CategoricalEncoding_GroupVariables_Selected = if(length(input$CategoricalEncoding_GroupVariables) > 0L) input$CategoricalEncoding_GroupVariables else NULL,
      CategoricalEncoding_TargetVariable_Selected = if(length(input$CategoricalEncoding_TargetVariable) > 0L) input$CategoricalEncoding_TargetVariable else NULL,
      DummifyDT_Cols_Selected = if(length(input$DummifyDT_Cols) > 0L) input$DummifyDT_Cols else NULL,

      # Statics
      CategoricalEncoding_TrainData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      CategoricalEncoding_Method_Choices = c('meow','credibility','target_encoding','woe','poly_encode','backward_difference','helmert'),
      CategoricalEncoding_ValidationData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      CategoricalEncoding_TestData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      DummifyDT_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      DummifyDT_TopN_Choices = 1L:100L,
      DummifyDT_KeepBaseCols_Choices = c(TRUE,FALSE),

      # Updaters
      CategoricalEncoding_TrainData_Selected = if(length(input$CategoricalEncoding_TrainData) > 0L) input$CategoricalEncoding_TrainData else NULL,
      CategoricalEncoding_Method_Selected = if(length(input$CategoricalEncoding_Method) > 0L) input$CategoricalEncoding_Method else "credibility",
      CategoricalEncoding_ValidationData_Selected = if(length(input$CategoricalEncoding_ValidationData) > 0L) input$CategoricalEncoding_ValidationData else NULL,
      CategoricalEncoding_TestData_Selected = if(length(input$CategoricalEncoding_TestData) > 0L) input$CategoricalEncoding_TestData else NULL,
      DummifyDT_SelectData_Selected = if(length(input$DummifyDT_SelectData) > 0L) input$DummifyDT_SelectData else NULL,
      DummifyDT_TopN_Selected = if(length(input$DummifyDT_TopN) > 0L) input$DummifyDT_TopN else NULL,
      DummifyDT_KeepBaseCols_Selected = if(length(input$DummifyDT_KeepBaseCols) > 0L) input$DummifyDT_KeepBaseCols else TRUE)

    # Reactives
    CategoricalEncoding_TrainDataReactive <- shiny::reactive({tryCatch({DataList[[input$CategoricalEncoding_TrainData]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(CategoricalEncoding_TrainDataReactive(), {
      nam <- tryCatch({CategoricalEncoding_TrainDataReactive()}, error = function(x) NULL)
      if(length(nam) > 0L) {
        ChoiceList <- list()
        ColTypes <- unique(Quantico:::ColTypes(nam))
        for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(nam, Types = ColTypes[i])
        Quantico:::PickerInput(session = session, Update = TRUE, InputID='CategoricalEncoding_GroupVariables', Label='Group Variables', Choices = ChoiceList, SelectedDefault = if(length(input$CategoricalEncoding_GroupVariables) > 0L) input$CategoricalEncoding_GroupVariables else NULL, Multiple = TRUE)
        Quantico:::PickerInput(session = session, Update = TRUE, InputID='CategoricalEncoding_TargetVariable', Label='Target Columns', Choices = ChoiceList, SelectedDefault = if(length(input$CategoricalEncoding_TargetVariable) > 0L) input$CategoricalEncoding_TargetVariable else NULL, Multiple = TRUE)
      }
    })

    DummifyDT_SelectData_Reactive <- shiny::reactive({tryCatch({DataList[[input$DummifyDT_SelectData]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(DummifyDT_SelectData_Reactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(DummifyDT_SelectData_Reactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(DummifyDT_SelectData_Reactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='DummifyDT_Cols', Label='Target Columns', Choices = ChoiceList, SelectedDefault = if(length(input$DummifyDT_Cols) > 0L) input$DummifyDT_Cols else NULL, Multiple = TRUE)
    })
  })
  shiny::observeEvent(input$Categorical_OK, {shiny::removeModal()})

  # Numeric Inputs
  shiny::observeEvent(input$Numeric_Modal, {
    Quantico:::Numeric_Modal_Fun(
      id = 'NumericFEID',

      # Reactives
      PercentRank_ColNames_Selected = if(length(input$PercentRank_ColNames) > 0L) input$PercentRank_ColNames else NULL,
      PercentRank_GroupVars_Selected = if(length(input$PercentRank_GroupVars) > 0L) input$PercentRank_GroupVars else NULL,
      Standardize_ColNames_Selected = if(length(input$Standardize_ColNames) > 0L) input$Standardize_ColNames else NULL,
      Standardize_GroupVars_Selected = if(length(input$Standardize_GroupVars) > 0L) input$Standardize_GroupVars else NULL,
      AutoInteraction_NumericVars_Selected = if(length(input$AutoInteraction_NumericVars) > 0L) input$AutoInteraction_NumericVars else NULL,
      AutoTransformationCreate_ColumnNames_Selected = if(length(input$AutoTransformationCreate_ColumnNames) > 0L) input$AutoTransformationCreate_ColumnNames else NULL,

      # Statics
      PercentRank_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      PercentRank_SelectValidationData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      PercentRank_SelectTestData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      PercentRank_Granularity_Choices = seq(0.001, 0.99, 0.001),
      Standardize_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      Standardize_SelectValidationData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      Standardize_SelectTestData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      Standardize_Center_Choices = c(TRUE,FALSE),
      Standardize_Scale_Choices = c(TRUE,FALSE),
      AutoInteraction_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      AutoInteraction_InteractionDepth_Choices = 2L:10L,
      AutoInteraction_Scale_Choices = c(TRUE,FALSE),
      AutoInteraction_Center_Choices = c(TRUE,FALSE),
      AutoTransformationCreate_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      AutoTransformationCreate_Methods_Choices = c('Asinh','Log','LogPlus1','Sqrt','Asin','Logit','BoxCox','YeoJohnson'),

      # Updaters
      PercentRank_SelectData_Selected = if(length(input$PercentRank_SelectData) > 0L) input$PercentRank_SelectData else NULL,
      PercentRank_SelectValidationData_Selected = if(length(input$PercentRank_SelectValidationData) > 0L) input$PercentRank_SelectValidationData else NULL,
      PercentRank_SelectTestData_Selected = if(length(input$PercentRank_SelectTestData) > 0L) input$PercentRank_SelectTestData else NULL,
      PercentRank_Granularity_Selected = if(length(input$PercentRank_Granularity) > 0L) input$PercentRank_Granularity else 0.001,
      Standardize_SelectData_Selected = if(length(input$Standardize_SelectData) > 0L) input$Standardize_SelectData else NULL,
      Standardize_SelectValidationData_Selected = if(length(input$Standardize_SelectValidationData) > 0L) input$Standardize_SelectValidationData else NULL,
      Standardize_SelectTestData_Selected = if(length(input$Standardize_SelectTestData) > 0L) input$Standardize_SelectTestData else NULL,
      Standardize_Center_Selected = if(length(input$Standardize_Center) > 0L) input$Standardize_Center else TRUE,
      Standardize_Scale_Selected = if(length(input$Standardize_Scale) > 0L) input$Standardize_Scale else TRUE,
      AutoInteraction_SelectData_Selected = if(length(input$AutoInteraction_SelectData) > 0L) input$AutoInteraction_SelectData else NULL,
      AutoInteraction_InteractionDepth_Selected = if(length(input$AutoInteraction_InteractionDepth) > 0L) input$AutoInteraction_InteractionDepth else 2L,
      AutoInteraction_Scale_Selected = if(length(input$AutoInteraction_Scale) > 0L) input$AutoInteraction_Scale else TRUE,
      AutoInteraction_Center_Selected = if(length(input$AutoInteraction_Center) > 0L) input$AutoInteraction_Center else TRUE,
      AutoTransformationCreate_SelectData_Selected = if(length(input$AutoTransformationCreate_SelectData) > 0L) input$AutoTransformationCreate_SelectData else NULL,
      AutoTransformationCreate_Methods_Selected = if(length(input$AutoTransformationCreate_Methods) > 0L) input$AutoTransformationCreate_Methods else NULL)

    # Reactives
    PercentRank_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$PercentRank_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(PercentRank_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(PercentRank_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(PercentRank_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='PercentRank_ColNames', Label='Target Columns', Choices = ChoiceList, SelectedDefault = if(length(input$PercentRank_ColNames) > 0L) input$PercentRank_ColNames else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='PercentRank_GroupVars', Label='By-Variables', Choices = ChoiceList, SelectedDefault = if(length(input$PercentRank_GroupVars) > 0L) input$PercentRank_GroupVars else NULL, Multiple = TRUE)
    })

    PercentRank_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$PercentRank_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(PercentRank_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(PercentRank_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(PercentRank_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='Standardize_ColNames', Label='Target Columns', Choices = ChoiceList, SelectedDefault = if(length(input$Standardize_ColNames) > 0L) input$Standardize_ColNames else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='Standardize_GroupVars', Label='By-Variables', Choices = ChoiceList, SelectedDefault = if(length(input$Standardize_GroupVars) > 0L) input$Standardize_GroupVars else NULL, Multiple = TRUE)
    })

    AutoInteraction_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$AutoInteraction_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(AutoInteraction_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(AutoInteraction_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(AutoInteraction_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='AutoInteraction_NumericVars', Label='Target Columns', Choices = ChoiceList, Multiple = TRUE)
    })

    AutoTransformationCreate_DataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$AutoTransformationCreate_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(AutoTransformationCreate_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(AutoTransformationCreate_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(AutoTransformationCreate_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='AutoTransformationCreate_ColumnNames', Label='Target Columns', Choices = ChoiceList, SelectedDefault = if(length(input$AutoTransformationCreate_ColumnNames) > 0L) input$AutoTransformationCreate_ColumnNames else NULL, Multiple = TRUE)
    })

  }, ignoreInit = TRUE)
  shiny::observeEvent(input$Numeric_OK, {shiny::removeModal()}, ignoreInit = TRUE)

  # Anomaly Detection
  shiny::observeEvent(input$AnomalyDetection_Modal, {
    Quantico:::AnomalyDetection_Modal_Fun(
      id = 'AnomDetectionFEID',

      # Reactives
      IsolationForest_H2O_Features_Selected = if(length(input$IsolationForest_H2O_Features) > 0L) input$IsolationForest_H2O_Features else NULL,

      # Statics
      IsolationForest_H2O_TrainData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      IsolationForest_H2O_ValidationData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      IsolationForest_H2O_TestData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      IsolationForest_H2O_Threshold_Choices = seq(0.01,0.99,0.01),
      IsolationForest_H2O_NTrees_Choices = c(5:50, seq(75,475,25), seq(500,9500,500), seq(10000, 25000, 1000)),
      IsolationForest_H2O_MaxDepth_Choices = 2L:20L,
      IsolationForest_H2O_MinRows_Choices = c(1:10, seq(20,1000,20)),
      IsolationForest_H2O_RowSampleRate_Choices = seq(0.01,1,0.01),
      IsolationForest_H2O_ColSampleRate_Choices = seq(0.01,1,0.01),
      IsolationForest_H2O_ColSampleRatePerLevel_Choices = seq(0.01,1,0.01),
      IsolationForest_H2O_ColSampleRatePerTree_Choices = seq(0.01,1,0.01),

      # Updaters
      IsolationForest_H2O_TrainData_Selected = if(length(input$IsolationForest_H2O_TrainData) > 0L) input$IsolationForest_H2O_TrainData else NULL,
      IsolationForest_H2O_ValidationData_Selected = if(length(input$IsolationForest_H2O_ValidationData) > 0L) input$IsolationForest_H2O_ValidationData else NULL,
      IsolationForest_H2O_TestData_Selected = if(length(input$IsolationForest_H2O_TestData) > 0L) input$IsolationForest_H2O_TestData else NULL,
      IsolationForest_H2O_Threshold_Selected = if(length(input$IsolationForest_H2O_Threshold) > 0L) input$IsolationForest_H2O_Threshold else 0.95,
      IsolationForest_H2O_NTrees_Selected = if(length(input$IsolationForest_H2O_NTrees) > 0L) input$IsolationForest_H2O_NTrees else 50,
      IsolationForest_H2O_MaxDepth_Selected = if(length(input$IsolationForest_H2O_MaxDepth) > 0L) input$IsolationForest_H2O_MaxDepth else 20L,
      IsolationForest_H2O_MinRows_Selected = if(length(input$IsolationForest_H2O_MinRows) > 0L) input$IsolationForest_H2O_MinRows else 1,
      IsolationForest_H2O_RowSampleRate_Selected = if(length(input$IsolationForest_H2O_RowSampleRate) > 0L) input$IsolationForest_H2O_RowSampleRate else 1,
      IsolationForest_H2O_ColSampleRate_Selected = if(length(input$IsolationForest_H2O_ColSampleRate) > 0L) input$IsolationForest_H2O_ColSampleRate else 1,
      IsolationForest_H2O_ColSampleRatePerLevel_Selected = if(length(input$IsolationForest_H2O_ColSampleRatePerLevel) > 0L) input$IsolationForest_H2O_ColSampleRatePerLevel else 1,
      IsolationForest_H2O_ColSampleRatePerTree_Selected = if(length(input$IsolationForest_H2O_ColSampleRatePerTree) > 0L) input$IsolationForest_H2O_ColSampleRatePerTree else 1)

    # Reactives
    IsolationForest_H2O_TrainDataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$IsolationForest_H2O_TrainData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(IsolationForest_H2O_TrainDataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(IsolationForest_H2O_TrainDataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(IsolationForest_H2O_TrainDataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = 'IsolationForest_H2O_Features', Label = 'Features', Choices = ChoiceList, SelectedDefault = if(length(input$IsolationForest_H2O_Features) > 0L) input$IsolationForest_H2O_Features else NULL, Multiple = TRUE, Debug = Debug)
    })
  })
  shiny::observeEvent(input$AnomalyDetection_OK, {shiny::removeModal()})

  # Dimensionality Reduction
  shiny::observeEvent(input$DimensionalityReduction_Modal, {
    Quantico:::DimensionalityReduction_Modal_Fun(
      id = 'DimReductionFEID',

      # Reactives
      AutoEncoder_H2O_Features_Selected = if(length(input$AutoEncoder_H2O_Features) > 0L) input$AutoEncoder_H2O_Features else NULL,

      # Statics
      AutoEncoder_H2O_TrainData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      AutoEncoder_H2O_ValidationData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      AutoEncoder_H2O_TestData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      AutoEncoder_H2O_AnomalyDetection_Choices = c(FALSE,TRUE),
      AutoEncoder_H2O_per_feature_Choices = c(FALSE,TRUE),
      AutoEncoder_H2O_RemoveFeatures_Choices = c(FALSE,TRUE),
      AutoEncoder_H2O_LayerStructure_Choices = 1L:10L,
      AutoEncoder_H2O_NodeShrinkRate_Choices = seq(0.01,0.95,0.01),
      AutoEncoder_H2O_ReturnLayer_Choices = 1L:10L,
      AutoEncoder_H2O_Epochs_Choices = c(10,50,100,250,1000,2500,5000),
      AutoEncoder_H2O_L2_Choices = seq(0.01,0.99,0.01),
      AutoEncoder_H2O_ElasticAveraging_Choices = c(TRUE,FALSE),
      AutoEncoder_H2O_ElasticAveragingMovingRate_Choices = seq(0.1,0.99,0.1),
      AutoEncoder_H2O_ElasticAveragingRegularization_Choices = seq(0.001,0.05,0.001),

      # Updaters
      AutoEncoder_H2O_TrainData_Selected = if(length(input$AutoEncoder_H2O_TrainData) > 0L) input$AutoEncoder_H2O_TrainData else NULL,
      AutoEncoder_H2O_ValidationData_Selected = if(length(input$AutoEncoder_H2O_ValidationData) > 0L) input$AutoEncoder_H2O_ValidationData else NULL,
      AutoEncoder_H2O_TestData_Selected = if(length(input$AutoEncoder_H2O_TestData) > 0L) input$AutoEncoder_H2O_TestData else NULL,
      AutoEncoder_H2O_AnomalyDetection_Selected = if(length(input$AutoEncoder_H2O_AnomalyDetection) > 0L) input$AutoEncoder_H2O_AnomalyDetection else FALSE,
      AutoEncoder_H2O_per_feature_Selected = if(length(input$AutoEncoder_H2O_per_feature) > 0L) input$AutoEncoder_H2O_per_feature else FALSE,
      AutoEncoder_H2O_RemoveFeatures_Selected = if(length(input$AutoEncoder_H2O_RemoveFeatures) > 0L) input$AutoEncoder_H2O_RemoveFeatures else FALSE,
      AutoEncoder_H2O_LayerStructure_Selected = if(length(input$AutoEncoder_H2O_LayerStructure) > 0L) input$AutoEncoder_H2O_LayerStructure else 2L,
      AutoEncoder_H2O_NodeShrinkRate_Selected = if(length(input$AutoEncoder_H2O_NodeShrinkRate) > 0L) input$AutoEncoder_H2O_NodeShrinkRate else 0.62,
      AutoEncoder_H2O_ReturnLayer_Selected = if(length(input$AutoEncoder_H2O_ReturnLayer) > 0L) input$AutoEncoder_H2O_ReturnLayer else 2L,
      AutoEncoder_H2O_Epochs_Selected = if(length(input$AutoEncoder_H2O_Epochs) > 0L) input$AutoEncoder_H2O_Epochs else 10,
      AutoEncoder_H2O_L2_Selected = if(length(input$AutoEncoder_H2O_L2) > 0L) input$AutoEncoder_H2O_L2 else 0.10,
      AutoEncoder_H2O_ElasticAveraging_Selected = if(length(input$AutoEncoder_H2O_ElasticAveraging) > 0L) input$AutoEncoder_H2O_ElasticAveraging else TRUE,
      AutoEncoder_H2O_ElasticAveragingMovingRate_Selected = if(length(input$AutoEncoder_H2O_ElasticAveragingMovingRate) > 0L) input$AutoEncoder_H2O_ElasticAveragingMovingRate else 0.90,
      AutoEncoder_H2O_ElasticAveragingRegularization_Selected = if(length(input$AutoEncoder_H2O_ElasticAveragingRegularization) > 0L) input$AutoEncoder_H2O_ElasticAveragingRegularization else 0.001)

    # Reactives
    AutoEncoder_H2O_TrainDataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$AutoEncoder_H2O_TrainData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(AutoEncoder_H2O_TrainDataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(AutoEncoder_H2O_TrainDataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(AutoEncoder_H2O_TrainDataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = 'AutoEncoder_H2O_Features', Label = 'Features', Choices = ChoiceList, SelectedDefault = if(length(input$AutoEncoder_H2O_Features) > 0L) input$AutoEncoder_H2O_Features else NULL, Multiple = TRUE, Debug = Debug)
    })
  })
  shiny::observeEvent(input$DimensionalityReduction_OK, {shiny::removeModal()})

  # Clustering
  shiny::observeEvent(input$Clustering_Modal, {
    Quantico:::Clustering_Modal_Fun(
      id = 'ClusteringFEID',

      # Reactives
      Kmeans_H2O_Features_Selected = if(length(input$Kmeans_H2O_Features) > 0L) input$Kmeans_H2O_Features else NULL,

      # Statics
      Kmeans_H2O_TrainData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      Kmeans_H2O_ValidationData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      Kmeans_H2O_TestData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      Kmeans_H2O_MaxClusters_Choices = 1L:100L,
      Kmeans_H2O_ClusterMetric_Choices = c('totss','betweenss','withinss'),

      # Updaters
      Kmeans_H2O_TrainData_Selected = if(length(input$Kmeans_H2O_TrainData) > 0L) input$Kmeans_H2O_TrainData else NULL,
      Kmeans_H2O_ValidationData_Selected = if(length(input$Kmeans_H2O_ValidationData) > 0L) input$Kmeans_H2O_ValidationData else NULL,
      Kmeans_H2O_TestData_Selected = if(length(input$Kmeans_H2O_TestData) > 0L) input$Kmeans_H2O_TestData else NULL,
      Kmeans_H2O_MaxClusters_Selected = if(length(input$Kmeans_H2O_MaxClusters) > 0L) input$Kmeans_H2O_MaxClusters else 10,
      Kmeans_H2O_ClusterMetric_Selected = if(length(input$Kmeans_H2O_ClusterMetric) > 0L) input$Kmeans_H2O_ClusterMetric else 'totss')

    # Reactives
    Kmeans_H2O_TrainDataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$Kmeans_H2O_TrainData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(Kmeans_H2O_TrainDataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(Kmeans_H2O_TrainDataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(Kmeans_H2O_TrainDataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = 'Kmeans_H2O_Features', Label = 'Features', Choices = ChoiceList, SelectedDefault = if(length(input$Kmeans_H2O_Features) > 0L) input$Kmeans_H2O_Features else NULL, Multiple = TRUE)
    })
  })
  shiny::observeEvent(input$Clustering_OK, {shiny::removeModal()})

  # NLP Inputs
  shiny::observeEvent(input$NLP_Modal, {
    Quantico:::NLP_Modal_Fun(
      id = "NLPFE_M",
      AppWidth=12L,

      # Reactives
      Word2Vec_H2O_stringCol_Selected = if(length(input$Word2Vec_H2O_stringCol) > 0L) input$Word2Vec_H2O_stringCol else NULL,
      TextSummary_TextColumns_Selected = if(length(input$TextSummary_TextColumns) > 0L) input$TextSummary_TextColumns else NULL,
      Sentiment_TextColumns_Selected = if(length(input$Sentiment_TextColumns) > 0L) input$Sentiment_TextColumns else NULL,
      Sentiment_CombineTextGroupVar_Selected = if(length(input$Sentiment_CombineTextGroupVar) > 0L) input$Sentiment_CombineTextGroupVar else NULL,
      Readability_TextColumns_Selected = if(length(input$Readability_TextColumns) > 0L) input$Readability_TextColumns else NULL,
      LexicalDiversity_TextColumns_Selected = if(length(input$LexicalDiversity_TextColumns) > 0L) input$LexicalDiversity_TextColumns else NULL,

      # Statics
      Word2Vec_H2O_TrainData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      Word2Vec_H2O_ValidationData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      Word2Vec_H2O_TestData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      Word2Vec_H2O_BuildType_Choices = c('individual','combined'),
      Word2Vec_H2O_KeepStringCol_Choices = c(TRUE,FALSE),
      Word2Vec_H2O_vects_Choices = seq(5L,1000L,5L),
      Word2Vec_H2O_MinWords_Choices = seq(1L,200L,1L),
      Word2Vec_H2O_WindowSize_Choices = seq(1L,100L,1L),
      Word2Vec_H2O_Epochs_Choices = seq(10L,1000L,10L),
      TextSummary_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      TextSummary_RemoveStats_Choices = c("document","chars","sents","tokens","types","puncts","numbers","symbols","urls","tags","emojis"),
      Sentiment_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      Sentiment_Response_Choices = c("numeric", "Binary", "Direction"),
      Sentiment_RemoveStopWords_Choices = c(TRUE,FALSE),
      Sentiment_Stemming_Choices = c(TRUE,FALSE),
      Readability_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      Readability_Measures_Choices = c("Flesch","ARI","Bormuth.MC","Bormuth.GP","Coleman","Coleman.C2","Coleman.Liau.ECP","Coleman.Liau.grade","Coleman.Liau.short","Dale.Chall","Danielson.Bryan","Dickes.Steiwer","DRP","ELF","Farr.Jenkins.Paterson","Flesch.PSK","Flesch.Kincaid","FOG","FOG.PSK","FOG.NRI","FORCAST","Fucks","Linsear.Write","LIW","nWS","nWS.2","nWS.3","nWS4","RIX","Scrabble","SMOG","SMOG.C","SMOG.simple","SMOG.de","Spache","Spache.old","Strain","Traenkle.Bailer","Wheeler.Smith","meanSentenceLength","meanWordSyllables"),
      Readability_RemoveHyphens_Choices = c(TRUE,FALSE),
      Readability_MinSentenceLength_Choices = 1L:100L,
      Readability_MaxSentenceLength_Choices = seq(100L,10000L,100L),
      Readability_Intermediate_Choices = c(TRUE,FALSE),
      LexicalDiversity_SelectData_Selected = tryCatch({names(DataList)}, error = function(x) NULL),
      LexicalDiversity_Measures_Choices = c("TTR","C","R","CTTR","U","S","K","I","D","Vm","Maas","MATTR","MSTTR"),
      LexicalDiversity_RemoveSymbols_Choices = c(TRUE,FALSE),
      LexicalDiversity_RemoveHyphens_Choices = c(TRUE,FALSE),
      LexicalDiversity_RemovePunctuation_Choices = c(TRUE,FALSE),
      LexicalDiversity_RemoveNumbers_Choices = c(TRUE,FALSE),
      LexicalDiversity_LogBase_Choices = c(exp(1),1L:10L),
      LexicalDiversity_MATTR_Window_Choices = 1L:100L,
      LexicalDiversity_MSTTR_Segment_Choices = 1L:100L,

      # Updaters
      Word2Vec_H2O_TrainData_Selected = if(length(input$Word2Vec_H2O_TrainData) > 0L) input$Word2Vec_H2O_TrainData else NULL,
      Word2Vec_H2O_ValidationData_Selected = if(length(input$Word2Vec_H2O_ValidationData) > 0L) input$Word2Vec_H2O_ValidationData else NULL,
      Word2Vec_H2O_TestData_Selected = if(length(input$Word2Vec_H2O_TestData) > 0L) input$Word2Vec_H2O_TestData else NULL,
      Word2Vec_H2O_BuildType_Selected = if(length(input$Word2Vec_H2O_BuildType) > 0L) input$Word2Vec_H2O_BuildType else 'individual',
      Word2Vec_H2O_KeepStringCol_Selected = if(length(input$Word2Vec_H2O_KeepStringCol) > 0L) input$Word2Vec_H2O_KeepStringCol else TRUE,
      Word2Vec_H2O_vects_Selected = if(length(input$Word2Vec_H2O_vects) > 0L) input$Word2Vec_H2O_vects else 30,
      Word2Vec_H2O_MinWords_Selected = if(length(input$Word2Vec_H2O_MinWords) > 0L) input$Word2Vec_H2O_MinWords else 1,
      Word2Vec_H2O_WindowSize_Selected = if(length(input$Word2Vec_H2O_WindowSize) > 0L) input$Word2Vec_H2O_WindowSize else 5,
      Word2Vec_H2O_Epochs_Selected = if(length(input$Word2Vec_H2O_Epochs) > 0L) input$Word2Vec_H2O_Epochs else 5,
      TextSummary_RemoveStats_Selected = if(length(input$TextSummary_RemoveStats) > 0L) input$TextSummary_RemoveStats else NULL,
      TextSummary_SelectData_Selected = if(length(input$TextSummary_SelectData) > 0L) input$TextSummary_SelectData else NULL,
      Sentiment_SelectData_Selected = if(length(input$Sentiment_SelectData) > 0L) input$Sentiment_SelectData else NULL,
      Sentiment_Response_Selected = if(length(input$Sentiment_Response) > 0L) input$Sentiment_Response else "numeric",
      Sentiment_RemoveStopWords_Selected = if(length(input$Sentiment_RemoveStopWords) > 0L) input$Sentiment_RemoveStopWords else FALSE,
      Sentiment_Stemming_Selected = if(length(input$Sentiment_Stemming) > 0L) input$Sentiment_Stemming else FALSE,
      Readability_SelectData_Selected = if(length(input$Sentiment_Stemming) > 0L) input$Sentiment_Stemming else NULL,
      Readability_Measures_Selected = if(length(input$Sentiment_Stemming) > 0L) input$Sentiment_Stemming else "Flesch",
      Readability_RemoveHyphens_Selected = if(length(input$Sentiment_Stemming) > 0L) input$Sentiment_Stemming else FALSE,
      Readability_MinSentenceLength_Selected = if(length(input$Sentiment_Stemming) > 0L) input$Sentiment_Stemming else 1,
      Readability_MaxSentenceLength_Selected = if(length(input$Sentiment_Stemming) > 0L) input$Sentiment_Stemming else 10000,
      Readability_Intermediate_Selected = if(length(input$Sentiment_Stemming) > 0L) input$Sentiment_Stemming else FALSE,
      LexicalDiversity_Measures_Selected = if(length(input$LexicalDiversity_Measures) > 0L) input$LexicalDiversity_Measures else "TTR",
      LexicalDiversity_RemoveSymbols_Selected = if(length(input$LexicalDiversity_RemoveSymbols) > 0L) input$LexicalDiversity_RemoveSymbols else FALSE,
      LexicalDiversity_RemoveHyphens_Selected = if(length(input$LexicalDiversity_RemoveHyphens) > 0L) input$LexicalDiversity_RemoveHyphens else FALSE,
      LexicalDiversity_RemovePunctuation_Selected = if(length(input$LexicalDiversity_RemovePunctuation) > 0L) input$LexicalDiversity_RemovePunctuation else FALSE,
      LexicalDiversity_RemoveNumbers_Selected = if(length(input$LexicalDiversity_RemoveNumbers) > 0L) input$LexicalDiversity_RemoveNumbers else FALSE,
      LexicalDiversity_LogBase_Selected = if(length(input$LexicalDiversity_LogBase) > 0L) input$LexicalDiversity_LogBase else 10,
      LexicalDiversity_MATTR_Window_Selected = if(length(input$LexicalDiversity_MATTR_Window) > 0L) input$LexicalDiversity_MATTR_Window else 5,
      LexicalDiversity_MSTTR_Segment_Selected = if(length(input$LexicalDiversity_MSTTR_Segment) > 0L) input$LexicalDiversity_MSTTR_Segment else 5)

    # Reactives
    Word2Vec_H2O_TrainDataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$Word2Vec_H2O_TrainData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(Word2Vec_H2O_TrainDataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(Word2Vec_H2O_TrainDataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(Word2Vec_H2O_TrainDataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID='Word2Vec_H2O_stringCol', Label='Text Variables', Choices = ChoiceList, SelectedDefault = if(length(input$Word2Vec_H2O_stringCol) > 0L) input$Word2Vec_H2O_stringCol else NULL, Multiple = TRUE)
    })

    TextSummary_SelectDataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$TextSummary_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(TextSummary_SelectDataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(TextSummary_SelectDataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(TextSummary_SelectDataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = 'TextSummary_TextColumns', Label = 'Text Columns', Choices = ChoiceList, SelectedDefault = if(length(input$TextSummary_TextColumns) > 0L) input$TextSummary_TextColumns else NULL, Multiple = TRUE)
    })

    Sentiment_SelectDataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$Sentiment_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(Sentiment_SelectDataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(Sentiment_SelectDataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(Sentiment_SelectDataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = 'Sentiment_TextColumns', Label = 'Text Columns', Choices = ChoiceList, SelectedDefault = if(length(input$Sentiment_TextColumns) > 0L) input$Sentiment_TextColumns else NULL, Multiple = TRUE, Debug = Debug)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = 'Sentiment_CombineTextGroupVar', Label = 'Combine Text Group Variable', Choices = ChoiceList, SelectedDefault = if(length(input$Sentiment_CombineTextGroupVar) > 0L) input$Sentiment_CombineTextGroupVar else NULL, Multiple = TRUE, Debug = Debug)
    })

    Readability_SelectDataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$Readability_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(Readability_SelectDataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(Readability_SelectDataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(Readability_SelectDataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = 'Readability_TextColumns', Label = 'Text Columns', Choices = ChoiceList, SelectedDefault = NULL, SelectedText = if(length(input$Readability_TextColumns) > 0L) input$Readability_TextColumns else NULL, Multiple = TRUE)
    })

    LexicalDiversity_SelectDataReactive <- shiny::reactive({shiny::req(tryCatch({DataList[[input$LexicalDiversity_SelectData]][['data']]}, error = function(x) NULL))})
    shiny::observeEvent(LexicalDiversity_SelectDataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(LexicalDiversity_SelectDataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(LexicalDiversity_SelectDataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = 'LexicalDiversity_TextColumns', Label = 'Text Columns', Choices = ChoiceList, SelectedDefault = if(length(input$LexicalDiversity_TextColumns) > 0L) input$LexicalDiversity_TextColumns else NULL, Multiple = TRUE, Debug = Debug)
    })
  })
  shiny::observeEvent(input$NLP_OK, {shiny::removeModal()})

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Inputs ::  ML & Stat Modeling     ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # CatBoost DropDown
  shiny::observeEvent(input$CatBoost_Modal, {

    Quantico:::CatBoost_Modal_Fun(
      id = 'CatBoostMLID',

      # Reactives (see below function)
      CatBoost_TargetColumnName_Selected = if(length(input$CatBoost_TargetColumnName) > 0L) input$CatBoost_TargetColumnName else NULL,
      CatBoost_FeatureColNames_Selected = if(length(input$CatBoost_FeatureColNames) > 0L) input$CatBoost_FeatureColNames else NULL,
      CatBoost_WeightsColumnName_Selected = if(length(input$CatBoost_WeightsColumnName) > 0L) input$CatBoost_WeightsColumnName else NULL,
      CatBoost_TransformNumericColumns_Selected = if(length(input$CatBoost_TransformNumericColumns) > 0L) input$CatBoost_TransformNumericColumns else NULL,
      CatBoost_grid_eval_metric_Selected = if(length(input$CatBoost_grid_eval_metric) > 0L) input$CatBoost_grid_eval_metric else "rmse",
      CatBoost_LossFunction_Selected = if(length(input$CatBoost_LossFunction) > 0L) input$CatBoost_LossFunction else "RMSE",
      CatBoost_EvalMetric_Selected = if(length(input$CatBoost_EvalMetric) > 0L) input$CatBoost_EvalMetric else "RMSE",

      # Static Parameters Choices
      CatBoost_TargetType_Choices = c('Regression','Binary Classification','MultiClass'),
      CatBoost_Runs_Choices = 1L:30L,
      CatBoost_data_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      CatBoost_ValidationData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      CatBoost_TestData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      CatBoost_EncodeMethod_Choices = c('meow','credibility','binary','m_estimator','woe','target_encoding','poly_encode','backward_difference','helmert'),
      CatBoost_Methods_Choices = c('BoxCox', 'Asinh', 'Log', 'LogPlus1', 'Sqrt', 'Asin', 'Logit'),
      CatBoost_PassInGrid_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      CatBoost_NThreads_Choices = c(1L:512L),
      CatBoost_task_type_Choices = c('GPU','CPU'),
      CatBoost_NumGPUs_Choices = seq_len(10L),
      CatBoost_TrainOnFull_Choices = c(FALSE, TRUE),
      CatBoost_Trees_Choices = c(5:50, seq(75,475,25), seq(500,9500,500), seq(10000, 25000, 1000)),
      CatBoost_Depth_Choices = 4:16,
      CatBoost_LearningRate_Choices = seq(0.01,0.50,0.002),
      CatBoost_L2_Leaf_Reg_Choices = 0:50,
      CatBoost_model_size_reg_Choices = seq(0.05,5,0.05),
      CatBoost_langevin_Choices = c(FALSE,TRUE),
      CatBoost_diffusion_temperature_Choices = c(5000,7500,10000,12500,15000),
      CatBoost_RandomStrength_Choices = seq(0.50, 1, 0.05),
      CatBoost_BorderCount_Choices = seq(32,256,32),
      CatBoost_RSM_Choices = seq(0.01,1,0.01),
      CatBoost_BootStrapType_Choices = c('Bayesian', 'Bernoulli', 'Poisson', 'MVS', 'No'),
      CatBoost_GrowPolicy_Choices = c('SymmetricTree', 'Depthwise', 'Lossguide'),
      CatBoost_feature_border_type_Choices = c('GreedyLogSum', 'Median', 'Uniform', 'UniformAndQuantiles', 'MaxLogSum', 'MinEntropy'),
      CatBoost_subsample_Choices = seq(0.50,1,0.01),
      CatBoost_score_function_Choices = c('Cosine', 'L2', 'NewtonL2', 'NewtonCosine'),
      CatBoost_min_data_in_leaf_Choices = 1L:20L,
      CatBoost_GridTune_Choices = c(FALSE, TRUE),
      CatBoost_MaxModelsInGrid_Choices = c(seq(5,20,5),seq(25,2500,25)),
      CatBoost_MaxRunsWithoutNewWinner_Choices = c(seq(5,20,5),seq(25,2500,25)),
      CatBoost_MaxRunMinutes_Choices = c(seq(30,60*24*7,30)),
      CatBoost_BaselineComparison_Choices = c('default','best'),
      CatBoost_MetricPeriods_Choices = c(1,seq(5,500,5)),

      # Static Parameters Defaults: use existing if available otherwise default
      CatBoost_ModelID_Selected = if(length(input$CatBoost_ModelID) > 0L) input$CatBoost_ModelID else "ML1",
      CatBoost_TargetType_Selected = if(length(input$CatBoost_TargetType) > 0L) input$CatBoost_TargetType else "Regression",
      CatBoost_Runs_Selected = 1,
      CatBoost_data_Selected = if(length(input$CatBoost_data) > 0L) input$CatBoost_data else NULL,
      CatBoost_ValidationData_Selected = if(length(input$CatBoost_ValidationData) > 0L) input$CatBoost_ValidationData else NULL,
      CatBoost_TestData_Selected = if(length(input$CatBoost_TestData) > 0L) input$CatBoost_TestData else NULL,
      CatBoost_EncodeMethod_Selected = if(length(input$CatBoost_EncodeMethod) > 0L) input$CatBoost_EncodeMethod else "credibility",
      CatBoost_Methods_Selected = if(length(input$CatBoost_Methods) > 0L) input$CatBoost_Methods else NULL,
      CatBoost_PassInGrid_Selected = if(length(input$CatBoost_PassInGrid) > 0L) input$CatBoost_PassInGrid else NULL,
      CatBoost_NThreads_Selected = if(length(input$CatBoost_NThreads) > 0L) input$CatBoost_NThreads else max(1L, parallel::detectCores()-2L),
      CatBoost_task_type_Selected = if(length(input$CatBoost_task_type) > 0L) input$CatBoost_task_type else "CPU",
      CatBoost_NumGPUs_Selected = if(length(input$CatBoost_NumGPUs) > 0L) input$CatBoost_NumGPUs else 1,
      CatBoost_TrainOnFull_Selected = if(length(input$CatBoost_TrainOnFull) > 0L) input$CatBoost_TrainOnFull else FALSE,
      CatBoost_Trees_Selected = if(length(input$CatBoost_Trees) > 0L) input$CatBoost_Trees else 500,
      CatBoost_Depth_Selected = if(length(input$CatBoost_Depth) > 0L) input$CatBoost_Depth else 6,
      CatBoost_LearningRate_Selected = if(length(input$CatBoost_LearningRate) > 0L) input$CatBoost_LearningRate else NULL,
      CatBoost_L2_Leaf_Reg_Selected = if(length(input$CatBoost_L2_Leaf_Reg) > 0L) input$CatBoost_L2_Leaf_Reg else NULL,
      CatBoost_model_size_reg_Selected = if(length(input$CatBoost_model_size_reg) > 0L) input$CatBoost_model_size_reg else 0.5,
      CatBoost_langevin_Selected = if(length(input$CatBoost_langevin) > 0L) input$CatBoost_langevin else TRUE,
      CatBoost_diffusion_temperature_Selected = if(length(input$CatBoost_diffusion_temperature) > 0L) input$CatBoost_diffusion_temperature else 10000,
      CatBoost_RandomStrength_Selected = if(length(input$CatBoost_RandomStrength) > 0L) input$CatBoost_RandomStrength else 1,
      CatBoost_BorderCount_Selected = if(length(input$CatBoost_BorderCount) > 0L) input$CatBoost_BorderCount else 256,
      CatBoost_RSM_Selected = if(length(input$CatBoost_RSM) > 0L) input$CatBoost_RSM else 1,
      CatBoost_BootStrapType_Selected = if(length(input$CatBoost_BootStrapType) > 0L) input$CatBoost_BootStrapType else "No",
      CatBoost_GrowPolicy_Selected = if(length(input$CatBoost_GrowPolicy) > 0L) input$CatBoost_GrowPolicy else "SymmetricTree",
      CatBoost_feature_border_type_Selected = if(length(input$CatBoost_feature_border_type) > 0L) input$CatBoost_feature_border_type else 'GreedyLogSum',
      CatBoost_subsample_Selected = if(length(input$CatBoost_subsample) > 0L) input$CatBoost_subsample else 1,
      CatBoost_score_function_Selected = if(length(input$CatBoost_score_function) > 0L) input$CatBoost_score_function else 'Cosine',
      CatBoost_min_data_in_leaf_Selected = if(length(input$CatBoost_min_data_in_leaf) > 0L) input$CatBoost_min_data_in_leaf else 1,
      CatBoost_GridTune_Selected = if(length(input$CatBoost_GridTune) > 0L) input$CatBoost_GridTune else FALSE,
      CatBoost_MaxModelsInGrid_Selected = if(length(input$CatBoost_MaxModelsInGrid) > 0L) input$CatBoost_MaxModelsInGrid else 25,
      CatBoost_MaxRunsWithoutNewWinner_Selected = if(length(input$CatBoost_MaxRunsWithoutNewWinner) > 0L) input$CatBoost_MaxRunsWithoutNewWinner else 20,
      CatBoost_MaxRunMinutes_Selected = if(length(input$CatBoost_MaxRunMinutes) > 0L) input$CatBoost_MaxRunMinutes else 30,
      CatBoost_BaselineComparison_Selected = if(length(input$CatBoost_BaselineComparison) > 0L) input$CatBoost_BaselineComparison else "default",
      CatBoost_MetricPeriods_Selected = if(length(input$CatBoost_MetricPeriods) > 0L) input$CatBoost_MetricPeriods else 10,
      CatBoost_ClassWeights0_Selected = if(length(input$CatBoost_ClassWeights0) > 0L) input$CatBoost_ClassWeights0 else 1,
      CatBoost_ClassWeights1_Selected = if(length(input$CatBoost_ClassWeights1) > 0L) input$CatBoost_ClassWeights1 else 1)

    # Reactives
    CatBoost_dataReactive <- shiny::reactive({tryCatch({DataList[[shiny::req(input$CatBoost_data)]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(shiny::req(CatBoost_dataReactive()), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(CatBoost_dataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(CatBoost_dataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='CatBoost_TargetColumnName', Label='Select Target', Choices = ChoiceList, SelectedDefault = if(length(input$CatBoost_TargetColumnName) > 0L) input$CatBoost_TargetColumnName else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='CatBoost_FeatureColNames', Label='Select Features', Choices = ChoiceList, SelectedDefault = if(length(input$CatBoost_FeatureColNames) > 0L) input$CatBoost_FeatureColNames else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='CatBoost_PrimaryDateColumn', Label='Select Date', Choices = ChoiceList, SelectedDefault = if(length(input$CatBoost_PrimaryDateColumn) > 0L) input$CatBoost_PrimaryDateColumn else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='CatBoost_WeightsColumnName', Label='Weights Column', Choices = ChoiceList, SelectedDefault = if(length(input$CatBoost_WeightsColumnName) > 0L) input$CatBoost_WeightsColumnName else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='CatBoost_TransformNumericColumns', Label='Transform Columns', Choices = ChoiceList, SelectedDefault = if(length(input$CatBoost_TransformNumericColumns) > 0L) input$CatBoost_TransformNumericColumns else NULL, Multiple = TRUE)
    })

    CatBoost_TargetTypeReactive <- shiny::reactive({input$CatBoost_TargetType})
    shiny::observeEvent(CatBoost_TargetTypeReactive(), {
      for(i in 1:30) print(CatBoost_TargetTypeReactive())
      out1 <- Quantico:::Shiny.ML.CatBoost.GridEvalMetricsOptions(CatBoost_TargetTypeReactive())
      Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID='CatBoost_grid_eval_metric', Label='Grid Tuning Evaluation Metric', Choices = out1$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = out1$Default, CloseAfterSelect = FALSE)
      out2 <- Quantico:::Shiny.ML.CatBoost.LossFunctionOptions(CatBoost_TargetTypeReactive())
      Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID='CatBoost_LossFunction', Label='Loss Function', Choices = out2$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = out2$Default, CloseAfterSelect = FALSE)
      out3 <- Quantico:::Shiny.ML.CatBoost.EvalMetricOptions(CatBoost_TargetTypeReactive())
      Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID='CatBoost_EvalMetric', Label='Evaluation Metric', Choices = out3$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = out3$Default, CloseAfterSelect = FALSE)
    })
  })
  shiny::observeEvent(input$CatBoost_OK, {shiny::removeModal()})

  # XGBoost DropDown
  shiny::observeEvent(input$XGBoost_Modal, {
    Quantico:::XGBoost_Modal_Fun(
      id = 'XGBoostMLID',

      # Reactives (see below function)
      XGBoost_TargetColumnName_Selected = if(length(input$XGBoost_TargetColumnName) > 0L) input$XGBoost_TargetColumnName else NULL,
      XGBoost_FeatureColNames_Selected = if(length(input$XGBoost_FeatureColNames) > 0L) input$XGBoost_FeatureColNames else NULL,
      XGBoost_WeightsColumnName_Selected = if(length(input$XGBoost_WeightsColumnName) > 0L) input$XGBoost_WeightsColumnName else NULL,
      XGBoost_TransformNumericColumns_Selected = if(length(input$XGBoost_TransformNumericColumns) > 0L) input$XGBoost_TransformNumericColumns else NULL,
      XGBoost_grid_eval_metric_Selected = if(length(input$XGBoost_grid_eval_metric) > 0L) input$XGBoost_grid_eval_metric else NULL,
      XGBoost_LossFunction_Selected = if(length(input$XGBoost_LossFunction) > 0L) input$XGBoost_LossFunction else "reg:squarederror",
      XGBoost_EvalMetric_Selected = if(length(input$XGBoost_EvalMetric) > 0L) input$XGBoost_EvalMetric else "rmse",

      # Static Parameters
      XGBoost_Runs_Choices = 1L:30L,
      XGBoost_TargetType_Choices = c('Regression','Binary Classification','MultiClass'),
      XGBoost_data_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      XGBoost_ValidationData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      XGBoost_TestData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      XGBoost_EncodeMethod_Choices = c('meow','credibility','binary','m_estimator','woe','target_encoding','poly_encode','backward_difference','helmert'),
      XGBoost_Methods_Choices = c('BoxCox', 'Asinh', 'Log', 'LogPlus1', 'Sqrt', 'Asin', 'Logit'),
      XGBoost_NThreads_Choices = c(-1,1:512),
      XGBoost_TreeMethod_Choices = c('GPU','CPU'),
      XGBoost_NumGPUs_Choices = 1L:12L,
      XGBoost_TrainOnFull_Choices = c(TRUE,FALSE),
      XGBoost_Trees_Choices = c(5:50, seq(75,475,25), seq(500,9500,500), seq(10000, 25000, 1000)),
      XGBoost_max_depth_Choices = 4L:20L,
      XGBoost_eta_Choices = seq(0.01,0.50,0.002),
      XGBoost_min_child_weight_Choices = 1L:50L,
      XGBoost_subsample_Choices = seq(0.05,1,0.05),
      XGBoost_colsample_bytree_Choices = seq(0.05,1,0.05),
      XGBoost_alpha_Choices = seq(0.0,1.0,0.05),
      XGBoost_lambda_Choices = seq(0.00,1,0.05),
      XGBoost_PassInGrid_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      XGBoost_GridTune_Choices = c(TRUE,FALSE),
      XGBoost_MaxModelsInGrid_Choices = c(seq(5,20,5),seq(25,2500,25)),
      XGBoost_MaxRunsWithoutNewWinner_Choices = c(seq(5,20,5),seq(25,2500,25)),
      XGBoost_MaxRunMinutes_Choices = c(seq(30,60*24*7,30)),
      XGBoost_BaselineComparison_Choices = c('default','best'),

      # Updaters
      XGBoost_ModelID_Selected = if(length(input$XGBoost_ModelID) > 0L) input$XGBoost_ModelID else "ML1",
      XGBoost_TargetType_Selected = if(length(input$XGBoost_TargetType) > 0L) input$XGBoost_TargetType else 'Regression',
      XGBoost_Runs_Selected = if(length(input$XGBoost_Runs) > 0L) input$XGBoost_Runs else 1,
      XGBoost_data_Selected = if(length(input$XGBoost_data) > 0L) input$XGBoost_data else NULL,
      XGBoost_ValidationData_Selected = if(length(input$XGBoost_ValidationData) > 0L) input$XGBoost_ValidationData else NULL,
      XGBoost_TestData_Selected = if(length(input$XGBoost_TestData) > 0L) input$XGBoost_TestData else NULL,
      XGBoost_EncodeMethod_Selected = if(length(input$XGBoost_EncodeMethod) > 0L) input$XGBoost_EncodeMethod else 'credibility',
      XGBoost_Methods_Selected = if(length(input$XGBoost_Methods) > 0L) input$XGBoost_Methods else NULL,
      XGBoost_NThreads_Selected = if(length(input$XGBoost_NThreads) > 0L) input$XGBoost_NThreads else -1,
      XGBoost_TreeMethod_Selected = if(length(input$XGBoost_TreeMethod) > 0L) input$XGBoost_TreeMethod else "CPU",
      XGBoost_NumGPUs_Selected = if(length(input$XGBoost_NumGPUs) > 0L) input$XGBoost_NumGPUs else 1L,
      XGBoost_TrainOnFull_Selected = if(length(input$XGBoost_TrainOnFull) > 0L) input$XGBoost_TrainOnFull else FALSE,
      XGBoost_Trees_Selected = if(length(input$XGBoost_Trees) > 0L) input$XGBoost_Trees else 500,
      XGBoost_max_depth_Selected = if(length(input$XGBoost_max_depth) > 0L) input$XGBoost_max_depth else 6L,
      XGBoost_eta_Selected = if(length(input$XGBoost_eta) > 0L) input$XGBoost_eta else 0.10,
      XGBoost_min_child_weight_Selected = if(length(input$XGBoost_min_child_weight) > 0L) input$XGBoost_min_child_weight else 1L,
      XGBoost_subsample_Selected = if(length(input$XGBoost_subsample) > 0L) input$XGBoost_subsample else 1,
      XGBoost_colsample_bytree_Selected = if(length(input$XGBoost_colsample_bytree) > 0L) input$XGBoost_colsample_bytree else 1,
      XGBoost_alpha_Selected = if(length(input$XGBoost_alpha) > 0L) input$XGBoost_alpha else 0,
      XGBoost_lambda_Selected = if(length(input$XGBoost_lambda) > 0L) input$XGBoost_lambda else 1,
      XGBoost_PassInGrid_Selected = if(length(input$XGBoost_PassInGrid) > 0L) input$XGBoost_PassInGrid else NULL,
      XGBoost_GridTune_Selected = if(length(input$XGBoost_GridTune) > 0L) input$XGBoost_GridTune else FALSE,
      XGBoost_MaxModelsInGrid_Selected = if(length(input$XGBoost_MaxModelsInGrid) > 0L) input$XGBoost_MaxModelsInGrid else 30,
      XGBoost_MaxRunsWithoutNewWinner_Selected = if(length(input$XGBoost_MaxRunsWithoutNewWinner) > 0L) input$XGBoost_MaxRunsWithoutNewWinner else 20,
      XGBoost_MaxRunMinutes_Selected = if(length(input$XGBoost_MaxRunMinutes) > 0L) input$XGBoost_MaxRunMinutes else 30,
      XGBoost_BaselineComparison_Selected = if(length(input$XGBoost_BaselineComparison) > 0L) input$XGBoost_BaselineComparison else 'default',
      XGBoost_ClassWeights0_Selected = if(length(input$CatBoost_ClassWeights0) > 0L) input$XGBoost_ClassWeights0 else 1,
      XGBoost_ClassWeights1_Selected = if(length(input$CatBoost_ClassWeights1) > 0L) input$XGBoost_ClassWeights1 else 1)


    # Reactives
    XGBoost_dataReactive <- shiny::reactive({tryCatch({DataList[[shiny::req(input$XGBoost_data)]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(XGBoost_dataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(XGBoost_dataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(XGBoost_dataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='XGBoost_TargetColumnName', Label='Select Target', Choices = ChoiceList, SelectedDefault = if(length(input$XGBoost_TargetColumnName) > 0L) input$XGBoost_TargetColumnName else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='XGBoost_FeatureColNames', Label='Select Features', Choices = ChoiceList, SelectedDefault = if(length(input$XGBoost_FeatureColNames) > 0L) input$XGBoost_FeatureColNames else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='XGBoost_PrimaryDateColumn', Label='Select Date', Choices = ChoiceList, SelectedDefault = if(length(input$XGBoost_PrimaryDateColumn) > 0L) input$XGBoost_PrimaryDateColumn else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='XGBoost_WeightsColumnName', Label='Weights Column', Choices = ChoiceList, SelectedDefault = if(length(input$XGBoost_WeightsColumnName) > 0L) input$XGBoost_WeightsColumnName else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='XGBoost_TransformNumericColumns', Label='Transform Columns', Choices = ChoiceList, SelectedDefault = if(length(input$XGBoost_TransformNumericColumns) > 0L) input$XGBoost_TransformNumericColumns else NULL, Multiple = TRUE)
    })

    XGBoost_TargetTypeReactive <- shiny::reactive({input$XGBoost_TargetType})
    shiny::observeEvent(XGBoost_TargetTypeReactive(), {
      out <- Quantico:::Shiny.ML.CatBoost.GridEvalMetricsOptions(XGBoost_TargetTypeReactive())
      Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID='XGBoost_grid_eval_metric', Label='Grid Tuning Evaluation Metric', Choices = out$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = out$Default, CloseAfterSelect = FALSE)
      out <- Quantico:::Shiny.ML.XGBoost.LossFunctionOptions(XGBoost_TargetTypeReactive())
      Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID='XGBoost_LossFunction', Label='Loss Function', Choices = out$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = out$Default, CloseAfterSelect = FALSE)
      out <- Quantico:::Shiny.ML.XGBoost.EvalMetricOptions(XGBoost_TargetTypeReactive())
      Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID='XGBoost_EvalMetric', Label='Evaluation Metric', Choices = out$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = out$Default, CloseAfterSelect = FALSE)
    })
  })
  shiny::observeEvent(input$XGBoost_OK, {shiny::removeModal()})

  # LightGBM DropDown
  shiny::observeEvent(input$LightGBM_Modal, {
    Quantico:::LightGBM_Modal_Fun(
      id = 'LightGBMMLID',

      # Reactives
      LightGBM_TargetColumnName_Selected = if(length(input$LightGBM_TargetColumnName_Selected) > 0L) input$LightGBM_TargetColumnName_Selected else NULL,
      LightGBM_FeatureColNames_Selected = if(length(input$LightGBM_FeatureColNames_Selected) > 0L) input$LightGBM_FeatureColNames_Selected else NULL,
      LightGBM_WeightsColumnName_Selected = if(length(input$LightGBM_WeightsColumnName_Selected) > 0L) input$LightGBM_WeightsColumnName_Selected else NULL,
      LightGBM_TransformNumericColumns_Selected = if(length(input$LightGBM_TransformNumericColumns_Selected) > 0L) input$LightGBM_TransformNumericColumns_Selected else NULL,
      LightGBM_grid_eval_metric_Selected = if(length(input$LightGBM_grid_eval_metric_Selected) > 0L) input$LightGBM_grid_eval_metric_Selected else NULL,
      LightGBM_metric_Selected = if(length(input$LightGBM_metric_Selected) > 0L) input$LightGBM_metric_Selected else "rmse",

      # Statics
      LightGBM_Runs_Choices = 1L:30L,
      LightGBM_TargetType_Choices = c("Regression", "Binary Classification", "MultiClass"),
      LightGBM_data_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      LightGBM_ValidationData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      LightGBM_TestData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      LightGBM_EncodeMethod_Choices = c('meow','credibility','binary','m_estimator','woe','target_encoding','poly_encode','backward_difference','helmert'),
      LightGBM_Methods_Choices = c('BoxCox', 'Asinh', 'Log', 'LogPlus1', 'Sqrt', 'Asin', 'Logit'),
      LightGBM_NThreads_Choices = c(-1L, 1L:512L),
      LightGBM_device_type_Choices = c('cpu','gpu'),
      LightGBM_NumGPUs_Choices = 1L:12L,
      LightGBM_TrainOnFull_Choices = c(TRUE,FALSE),
      LightGBM_ModelID_Choices = NULL,
      LightGBM_Trees_Choices = c(5:50, seq(75,475,25), seq(500,9500,500), seq(10000, 25000, 1000)),
      LightGBM_max_depth_Choices = 4L:20L,
      LightGBM_eta_Choices = seq(0.01,0.50,0.002),
      LightGBM_min_data_in_leaf_Choices = c(1,seq(5,1000,5)),
      LightGBM_num_leaves_Choices = seq(5,500,5),
      LightGBM_bagging_fraction_Choices = seq(0.05,1,0.05),
      LightGBM_feature_fraction_Choices = seq(0.05,1,0.05),
      LightGBM_feature_fraction_bynode_Choices = seq(0.05,1,0.05),
      LightGBM_lambda_l1_Choices = 1L:50L,
      LightGBM_lambda_l2_Choices = 1L:50L,
      LightGBM_PassInGrid_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      LightGBM_GridTune_Choices = c(TRUE,FALSE),
      LightGBM_MaxModelsInGrid_Choices = c(seq(5,20,5),seq(25,2500,25)),
      LightGBM_MaxRunsWithoutNewWinner_Choices = c(seq(5,20,5),seq(25,2500,25)),
      LightGBM_MaxRunMinutes_Choices = c(seq(30,60*24*7,30)),
      LightGBM_BaselineComparison_Choices = c('default','best'),

      # Updaters
      LightGBM_ModelID_Selected = if(length(input$LightGBM_ModelID) > 0L) input$LightGBM_ModelID else "ML1",
      LightGBM_TargetType_Selected = if(length(input$LightGBM_TargetType) > 0L) input$LightGBM_TargetType else 'Regression',
      LightGBM_Runs_Selected = if(length(input$LightGBM_Runs) > 0L) input$LightGBM_Runs else 1,
      LightGBM_data_Selected = if(length(input$LightGBM_data) > 0L) input$LightGBM_data else NULL,
      LightGBM_ValidationData_Selected = if(length(input$LightGBM_ValidationData) > 0L) input$LightGBM_ValidationData else NULL,
      LightGBM_TestData_Selected = if(length(input$LightGBM_TestData) > 0L) input$LightGBM_TestData else NULL,
      LightGBM_EncodeMethod_Selected = if(length(input$LightGBM_EncodeMethod) > 0L) input$LightGBM_EncodeMethod else "credibility",
      LightGBM_Methods_Selected = if(length(input$LightGBM_Methods) > 0L) input$LightGBM_Methods else NULL,
      LightGBM_NThreads_Selected = if(length(input$LightGBM_NThreads) > 0L) input$LightGBM_NThreads else -1,
      LightGBM_device_type_Selected = if(length(input$LightGBM_device_type) > 0L) input$LightGBM_device_type else 'cpu',
      LightGBM_NumGPUs_Selected = if(length(input$LightGBM_NumGPUs) > 0L) input$LightGBM_NumGPUs else 1,
      LightGBM_TrainOnFull_Selected = if(length(input$LightGBM_TrainOnFull) > 0L) input$LightGBM_TrainOnFull else FALSE,
      LightGBM_Trees_Selected = if(length(input$LightGBM_Trees) > 0L) input$LightGBM_Trees else 500,
      LightGBM_max_depth_Selected = if(length(input$LightGBM_max_depth) > 0L) input$LightGBM_max_depth else 6,
      LightGBM_eta_Selected = if(length(input$LightGBM_eta) > 0L) input$LightGBM_eta else 0.10,
      LightGBM_min_data_in_leaf_Selected = if(length(input$LightGBM_min_data_in_leaf) > 0L) input$LightGBM_min_data_in_leaf else 1,
      LightGBM_num_leaves_Selected = if(length(input$LightGBM_num_leaves) > 0L) input$LightGBM_num_leaves else 30,
      LightGBM_bagging_fraction_Selected = if(length(input$LightGBM_bagging_fraction) > 0L) input$LightGBM_bagging_fraction else 1,
      LightGBM_feature_fraction_Selected = if(length(input$LightGBM_feature_fraction) > 0L) input$LightGBM_feature_fraction else 1,
      LightGBM_feature_fraction_bynode_Selected = if(length(input$LightGBM_feature_fraction_bynode) > 0L) input$LightGBM_feature_fraction_bynode else 1,
      LightGBM_lambda_l1_Selected = if(length(input$LightGBM_lambda_l1) > 0L) input$LightGBM_lambda_l1 else 1,
      LightGBM_lambda_l2_Selected = if(length(input$LightGBM_lambda_l2) > 0L) input$LightGBM_lambda_l2 else 1,
      LightGBM_PassInGrid_Selected = if(length(input$LightGBM_PassInGrid) > 0L) input$LightGBM_PassInGrid else NULL,
      LightGBM_GridTune_Selected = if(length(input$LightGBM_GridTune) > 0L) input$LightGBM_GridTune else FALSE,
      LightGBM_MaxModelsInGrid_Selected = if(length(input$LightGBM_MaxModelsInGrid) > 0L) input$LightGBM_MaxModelsInGrid else 25,
      LightGBM_MaxRunsWithoutNewWinner_Selected = if(length(input$LightGBM_MaxRunsWithoutNewWinner) > 0L) input$LightGBM_MaxRunsWithoutNewWinner else 20,
      LightGBM_MaxRunMinutes_Selected = if(length(input$LightGBM_MaxRunMinutes) > 0L) input$LightGBM_MaxRunMinutes else 30,
      LightGBM_BaselineComparison_Selected = if(length(input$LightGBM_BaselineComparison) > 0L) input$LightGBM_BaselineComparison else 'default',
      LightGBM_ClassWeights0_Selected = if(length(input$LightGBM_ClassWeights0) > 0L) input$LightGBM_ClassWeights0 else 1,
      LightGBM_ClassWeights1_Selected = if(length(input$LightGBM_ClassWeights1) > 0L) input$LightGBM_ClassWeights1 else 1)

    # Reactives
    LightGBM_dataReactive <- shiny::reactive({tryCatch({DataList[[shiny::req(input$LightGBM_data)]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(LightGBM_dataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(LightGBM_dataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(LightGBM_dataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='LightGBM_TargetColumnName', Label='Select Target', Choices = ChoiceList, SelectedDefault = if(length(input$LightGBM_TargetColumnName) > 0L) input$LightGBM_TargetColumnName else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='LightGBM_FeatureColNames', Label='Select Features', Choices = ChoiceList, SelectedDefault = if(length(input$LightGBM_FeatureColNames) > 0L) input$LightGBM_FeatureColNames else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='LightGBM_PrimaryDateColumn', Label='Select Date', Choices = ChoiceList, SelectedDefault = if(length(input$LightGBM_PrimaryDateColumn) > 0L) input$LightGBM_PrimaryDateColumn else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='LightGBM_WeightsColumnName', Label='Weights Column', Choices = ChoiceList, SelectedDefault = if(length(input$LightGBM_WeightsColumnName) > 0L) input$LightGBM_WeightsColumnName else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='LightGBM_TransformNumericColumns', Label='Transform Columns', Choices = ChoiceList, SelectedDefault = if(length(input$LightGBM_TransformNumericColumns) > 0L) input$LightGBM_TransformNumericColumns else NULL, Multiple = TRUE)
    })

    LightGBM_TargetTypeReactive <- shiny::reactive({input$LightGBM_TargetType})
    shiny::observeEvent(LightGBM_TargetTypeReactive(), {
      out <- Quantico:::Shiny.ML.CatBoost.GridEvalMetricsOptions(LightGBM_TargetTypeReactive())
      Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID='LightGBM_grid_eval_metric', Label='Grid Tuning Evaluation Metric', Choices = tryCatch({out$Choices}, error = function(x) NULL), Multiple = TRUE, MaxVars = 1, SelectedDefault = out$Default, CloseAfterSelect = FALSE)
      out <- Quantico:::Shiny.ML.LightGBM.LossFunctionOptions(LightGBM_TargetTypeReactive())
      Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID='LightGBM_metric', Label='Loss Function', Choices = tryCatch({out$Choices}, error = function(x) NULL), Multiple = TRUE, MaxVars = 1, SelectedDefault = out$Default, CloseAfterSelect = FALSE)
    })
  })
  shiny::observeEvent(input$LightGBM_OK, {shiny::removeModal()})

  # H2O_DRF DropDown
  shiny::observeEvent(input$H2O_DRF_Modal, {
    Quantico:::H2O_DRF_Modal_Fun(
      id = 'H2O_DRFMLID',

      # Reactives
      H2O_DRF_eval_metric_Selected = if(length(input$H2O_DRF_eval_metric_Selected) > 0L) input$H2O_DRF_eval_metric_Selected else NULL,
      H2O_DRF_TargetColumnName_Selected = if(length(input$H2O_DRF_TargetColumnName) > 0L) input$H2O_DRF_TargetColumnName else NULL,
      H2O_DRF_FeatureColNames_Selected = if(length(input$H2O_DRF_FeatureColNames) > 0L) input$H2O_DRF_FeatureColNames else NULL,
      H2O_DRF_TransformNumericColumns_Selected = if(length(input$H2O_DRF_TransformNumericColumns) > 0L) input$H2O_DRF_TransformNumericColumns else NULL,
      H2O_DRF_WeightsColumn_Selected = if(length(input$H2O_DRF_WeightsColumn) > 0L) input$H2O_DRF_WeightsColumn else NULL,

      # Statics
      H2O_DRF_Runs_Choices = 1L:30L,
      H2O_DRF_TargetType_Choices = c('Regression','Binary Classification','MultiClass'),
      H2O_DRF_data_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      H2O_DRF_ValidationData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      H2O_DRF_TestData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      H2O_DRF_EncodeMethod_Choices = c('meow','credibility','binary','m_estimator','woe','target_encoding','poly_encode','backward_difference','helmert'),
      H2O_DRF_Methods_Choices = c('BoxCox', 'Asinh', 'Log', 'LogPlus1', 'Sqrt', 'Asin', 'Logit'),
      H2O_DRF_NThreads_Choices = c(-1L, 1L:512L),
      H2O_DRF_MaxMem_Choices = 4L:512L,
      H2O_DRF_TrainOnFull_Choices = c(FALSE, TRUE),
      H2O_DRF_Trees_Choices = c(5:50, seq(75,475,25), seq(500,9500,500), seq(10000, 25000, 1000)),
      H2O_DRF_MaxDepth_Choices = 4L:20L,
      H2O_DRF_SampleRate_Choices = seq(0.01,1,0.01),
      H2O_DRF_MTries_Choices = seq(0.01,1,0.01),
      H2O_DRF_ColSampleRatePerTree_Choices = seq(0.01,1,0.01),
      H2O_DRF_MinRows_Choices = 1L:500L,
      H2O_DRF_NBinsCats_Choices = 2^(1:18),
      H2O_DRF_NBinsTopLevel_Choices = 2^(1:18),
      H2O_DRF_HistogramType_Choices = c("AUTO","UniformAdaptive","Random","QuantilesGlobal","RoundRobin"),
      H2O_DRF_CategoricalEncoding_Choices = c("AUTO","Enum","OneHotInternal","OneHotExplicit","Binary","Eigen","LabelEncoder","SortByResponse","EnumLimite"),
      H2O_DRF_StoppingRounds_Choices = 1L:50L,
      H2O_DRF_GridTune_Choices = c(FALSE, TRUE),
      H2O_DRF_MaxModelsInGrid_Choices = c(seq(5,20,5),seq(25,2500,25)),

      # Updaters
      H2O_DRF_ModelID_Selected = if(length(input$H2O_DRF_ModelID) > 0L) input$H2O_DRF_ModelID else "ML1",
      H2O_DRF_TargetType_Selected = if(length(input$H2O_DRF_TargetType) > 0L) input$H2O_DRF_TargetType else 'Regression',
      H2O_DRF_Runs_Selected = if(length(input$H2O_DRF_Runs) > 0L) input$H2O_DRF_Runs else 1,
      H2O_DRF_data_Selected = if(length(input$H2O_DRF_data) > 0L) input$H2O_DRF_data else NULL,
      H2O_DRF_ValidationData_Selected = if(length(input$H2O_DRF_ValidationData) > 0L) input$H2O_DRF_ValidationData else NULL,
      H2O_DRF_TestData_Selected = if(length(input$H2O_DRF_TestData) > 0L) input$H2O_DRF_TestData else NULL,
      H2O_DRF_EncodeMethod_Selected = if(length(input$H2O_DRF_EncodeMethod) > 0L) input$H2O_DRF_EncodeMethod else 'credibility',
      H2O_DRF_Methods_Selected = if(length(input$H2O_DRF_Methods) > 0L) input$H2O_DRF_Methods else NULL,
      H2O_DRF_NThreads_Selected = if(length(input$H2O_DRF_NThreads) > 0L) input$H2O_DRF_NThreads else -1L,
      H2O_DRF_MaxMem_Selected = if(length(input$H2O_DRF_MaxMem) > 0L) input$H2O_DRF_MaxMem else 4L,
      H2O_DRF_TrainOnFull_Selected = if(length(input$H2O_DRF_TrainOnFull) > 0L) input$H2O_DRF_TrainOnFull else FALSE,
      H2O_DRF_Trees_Selected = if(length(input$H2O_DRF_Trees) > 0L) input$H2O_DRF_Trees else 20L,
      H2O_DRF_MaxDepth_Selected = if(length(input$H2O_DRF_MaxDepth) > 0L) input$H2O_DRF_MaxDepth else 20,
      H2O_DRF_SampleRate_Selected = if(length(input$H2O_DRF_SampleRate) > 0L) input$H2O_DRF_SampleRate else 0.61,
      H2O_DRF_MTries_Selected = if(length(input$H2O_DRF_MTries) > 0L) input$H2O_DRF_MTries else 1,
      H2O_DRF_ColSampleRatePerTree_Selected = if(length(input$H2O_DRF_ColSampleRatePerTree) > 0L) input$H2O_DRF_ColSampleRatePerTree else 1,
      H2O_DRF_MinRows_Selected = if(length(input$H2O_DRF_MinRows) > 0L) input$H2O_DRF_MinRows else 1,
      H2O_DRF_NBinsCats_Selected = if(length(input$H2O_DRF_NBinsCats) > 0L) input$H2O_DRF_NBinsCats else 1024,
      H2O_DRF_NBinsTopLevel_Selected = if(length(input$H2O_DRF_NBinsTopLevel) > 0L) input$H2O_DRF_NBinsTopLevel else 1024,
      H2O_DRF_HistogramType_Selected = if(length(input$H2O_DRF_HistogramType) > 0L) input$H2O_DRF_HistogramType else "AUTO",
      H2O_DRF_CategoricalEncoding_Selected = if(length(input$H2O_DRF_CategoricalEncoding) > 0L) input$H2O_DRF_CategoricalEncoding else "AUTO",
      H2O_DRF_StoppingRounds_Selected = if(length(input$H2O_DRF_StoppingRounds) > 0L) input$H2O_DRF_StoppingRounds else 1,
      H2O_DRF_GridTune_Selected = if(length(input$H2O_DRF_GridTune) > 0L) input$H2O_DRF_GridTune else FALSE,
      H2O_DRF_MaxModelsInGrid_Selected = if(length(input$H2O_DRF_MaxModelsInGrid) > 0L) input$H2O_DRF_MaxModelsInGrid else 25,
      H2O_DRF_ClassWeights0_Selected = if(length(input$H2O_DRF_ClassWeights0) > 0L) input$H2O_DRF_ClassWeights0 else 1,
      H2O_DRF_ClassWeights1_Selected = if(length(input$H2O_DRF_ClassWeights1) > 0L) input$H2O_DRF_ClassWeights1 else 1)

    # Reactives
    H2O_DRF_dataReactive <- shiny::reactive({tryCatch({DataList[[shiny::req(input$H2O_DRF_data)]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(H2O_DRF_dataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(H2O_DRF_dataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(H2O_DRF_dataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = 'H2O_DRF_TargetColumnName', Label='Select Target', Choices = ChoiceList, SelectedDefault = if(length(input$H2O_DRF_TargetColumnName) > 0L) input$H2O_DRF_TargetColumnName else NULL,  Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = 'H2O_DRF_FeatureColNames', Label='Select Features', Choices = ChoiceList, SelectedDefault = if(length(input$H2O_DRF_FeatureColNames) > 0L) input$H2O_DRF_FeatureColNames else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = 'H2O_DRF_PrimaryDateColumn', Label='Select Date', Choices = ChoiceList, SelectedDefault = if(length(input$H2O_DRF_PrimaryDateColumn) > 0L) input$H2O_DRF_PrimaryDateColumn else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = 'H2O_DRF_TransformNumericColumns', Label='Transform Columns', Choices = ChoiceList, SelectedDefault = if(length(input$H2O_DRF_TransformNumericColumns) > 0L) input$H2O_DRF_TransformNumericColumns else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = 'H2O_DRF_WeightsColumn', Label='Weights Column', Choices = ChoiceList, SelectedDefault = if(length(input$H2O_DRF_WeightsColumn) > 0L) input$H2O_DRF_WeightsColumn else NULL, Multiple = TRUE, MaxVars = 1L)
    })

    H2O_DRF_TargetTypeReactive <- shiny::reactive({input$H2O_DRF_TargetType})
    shiny::observeEvent(H2O_DRF_TargetTypeReactive(), {
      out <- Quantico:::Shiny.ML.H2O_DRF.EvalMetricOptions(shiny::req(H2O_DRF_TargetTypeReactive()))
      Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID='H2O_DRF_eval_metric', Label='Loss Function', Choices = out$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = out$Default, CloseAfterSelect = FALSE)
    })

  })
  shiny::observeEvent(input$H2O_DRF_OK, {shiny::removeModal()})

  # H2O_GBM DropDown
  shiny::observeEvent(input$H2O_GBM_Modal, {
    Quantico:::H2O_GBM_Modal_Fun(
      id = 'H2O_GBMMLID',

      # Reactives
      H2O_GBM_eval_metric_Selected = if(length(input$H2O_GBM_eval_metric_Selected) > 0L) input$H2O_GBM_eval_metric_Selected else NULL,
      H2O_GBM_TargetColumnName_Selected = if(length(input$H2O_GBM_TargetColumnName) > 0L) input$H2O_GBM_TargetColumnName else NULL,
      H2O_GBM_FeatureColNames_Selected = if(length(input$H2O_GBM_FeatureColNames) > 0L) input$H2O_GBM_FeatureColNames else NULL,
      H2O_GBM_TransformNumericColumns_Selected = if(length(input$H2O_GBM_TransformNumericColumns) > 0L) input$H2O_GBM_TransformNumericColumns else NULL,
      H2O_GBM_WeightsColumn_Selected = if(length(input$H2O_GBM_WeightsColumn) > 0L) input$H2O_GBM_WeightsColumn else NULL,
      H2O_GBM_Distribution_Selected = if(length(input$H2O_GBM_Distribution) > 0L) input$H2O_GBM_Distribution else NULL,

      # Statics
      H2O_GBM_Runs_Choices = 1L:30L,
      H2O_GBM_TargetType_Choices = c('Regression','Binary Classification','MultiClass'),
      H2O_GBM_data_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      H2O_GBM_ValidationData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      H2O_GBM_TestData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      H2O_GBM_EncodeMethod_Choices = c('meow','credibility','binary','m_estimator','woe','target_encoding','poly_encode','backward_difference','helmert'),
      H2O_GBM_Methods_Choices = c('BoxCox', 'Asinh', 'Log', 'LogPlus1', 'Sqrt', 'Asin', 'Logit'),
      H2O_GBM_NThreads_Choices = c(-1L, 1L:512L),
      H2O_GBM_MaxMem_Choices = 4L:512L,
      H2O_GBM_TrainOnFull_Choices = c(FALSE, TRUE),
      H2O_GBM_Trees_Choices = c(5:50, seq(75,475,25), seq(500,9500,500), seq(10000, 25000, 1000)),
      H2O_GBM_MaxDepth_Choices = 4L:20L,
      H2O_GBM_SampleRate_Choices = seq(0.01,1,0.01),
      H2O_GBM_ColSampleRatePerTree_Choices = seq(0.01,1,0.01),
      H2O_GBM_LearnRate_Choices = seq(0.01,1,0.01),
      H2O_GBM_LearnRateAnnealing_Choices = seq(0.01,1,0.01),
      H2O_GBM_MinRows_Choices = 1L:500L,
      H2O_GBM_NBinsCats_Choices = 2^(1:18),
      H2O_GBM_NBinsTopLevel_Choices = 2^(1:18),
      H2O_GBM_HistogramType_Choices = c("AUTO","UniformAdaptive","Random","QuantilesGlobal","RoundRobin"),
      H2O_GBM_CategoricalEncoding_Choices = c("AUTO","Enum","OneHotInternal","OneHotExplicit","Binary","Eigen","LabelEncoder","SortByResponse","EnumLimite"),
      H2O_GBM_StoppingRounds_Choices = 1L:50L,
      H2O_GBM_GridTune_Choices = c(FALSE, TRUE),
      H2O_GBM_MaxModelsInGrid_Choices = c(seq(5,20,5),seq(25,2500,25)),

      # Updaters
      H2O_GBM_ModelID_Selected = if(length(input$H2O_GBM_ModelID) > 0L) input$H2O_GBM_ModelID else "ML1",
      H2O_GBM_TargetType_Selected = if(length(input$H2O_GBM_TargetType) > 0L) input$H2O_GBM_TargetType else "Regression",
      H2O_GBM_Runs_Selected = if(length(input$H2O_GBM_Runs) > 0L) input$H2O_GBM_Runs else 1,
      H2O_GBM_data_Selected = if(length(input$H2O_GBM_data) > 0L) input$H2O_GBM_data else NULL,
      H2O_GBM_ValidationData_Selected = if(length(input$H2O_GBM_ValidationData) > 0L) input$H2O_GBM_ValidationData else NULL,
      H2O_GBM_TestData_Selected = if(length(input$H2O_GBM_TestData) > 0L) input$H2O_GBM_TestData else NULL,
      H2O_GBM_EncodeMethod_Selected = if(length(input$H2O_GBM_EncodeMethod) > 0L) input$H2O_GBM_EncodeMethod else 'credibility',
      H2O_GBM_Methods_Selected = if(length(input$H2O_GBM_Methods) > 0L) input$H2O_GBM_Methods else NULL,
      H2O_GBM_NThreads_Selected = if(length(input$H2O_GBM_NThreads) > 0L) input$H2O_GBM_NThreads else -1L,
      H2O_GBM_MaxMem_Selected = if(length(input$H2O_GBM_MaxMem) > 0L) input$H2O_GBM_MaxMem else 4L,
      H2O_GBM_TrainOnFull_Selected = if(length(input$H2O_GBM_TrainOnFull) > 0L) input$H2O_GBM_TrainOnFull else FALSE,
      H2O_GBM_Trees_Selected = if(length(input$H2O_GBM_Trees) > 0L) input$H2O_GBM_Trees else 20L,
      H2O_GBM_MaxDepth_Selected = if(length(input$H2O_GBM_MaxDepth) > 0L) input$H2O_GBM_MaxDepth else 20,
      H2O_GBM_SampleRate_Selected = if(length(input$H2O_GBM_SampleRate) > 0L) input$H2O_GBM_SampleRate else 0.61,
      H2O_GBM_LearnRate_Selected = if(length(input$H2O_GBM_SampleRate) > 0L) input$H2O_GBM_LearnRate else 0.1,
      H2O_GBM_LearnRateAnnealing_Selected = if(length(input$H2O_GBM_SampleRate) > 0L) input$H2O_GBM_LearnRateAnnealing else 0.99,
      H2O_GBM_MTries_Selected = if(length(input$H2O_GBM_MTries) > 0L) input$H2O_GBM_MTries else 1,
      H2O_GBM_ColSampleRatePerTree_Selected = if(length(input$H2O_GBM_ColSampleRatePerTree) > 0L) input$H2O_GBM_ColSampleRatePerTree else 1,
      H2O_GBM_MinRows_Selected = if(length(input$H2O_GBM_MinRows) > 0L) input$H2O_GBM_MinRows else 1,
      H2O_GBM_NBinsCats_Selected = if(length(input$H2O_GBM_NBinsCats) > 0L) input$H2O_GBM_NBinsCats else 1024,
      H2O_GBM_NBinsTopLevel_Selected = if(length(input$H2O_GBM_NBinsTopLevel) > 0L) input$H2O_GBM_NBinsTopLevel else 1024,
      H2O_GBM_HistogramType_Selected = if(length(input$H2O_GBM_HistogramType) > 0L) input$H2O_GBM_HistogramType else "AUTO",
      H2O_GBM_CategoricalEncoding_Selected = if(length(input$H2O_GBM_CategoricalEncoding) > 0L) input$H2O_GBM_CategoricalEncoding else "AUTO",
      H2O_GBM_StoppingRounds_Selected = if(length(input$H2O_GBM_StoppingRounds) > 0L) input$H2O_GBM_StoppingRounds else 1,
      H2O_GBM_GridTune_Selected = if(length(input$H2O_GBM_GridTune) > 0L) input$H2O_GBM_GridTune else FALSE,
      H2O_GBM_MaxModelsInGrid_Selected = if(length(input$H2O_GBM_MaxModelsInGrid) > 0L) input$H2O_GBM_MaxModelsInGrid else 25,
      H2O_GBM_ClassWeights0_Selected = if(length(input$H2O_GBM_ClassWeights0) > 0L) input$H2O_GBM_ClassWeights0 else 1,
      H2O_GBM_ClassWeights1_Selected = if(length(input$H2O_GBM_ClassWeights1) > 0L) input$H2O_GBM_ClassWeights1 else 1)

    # Reactives
    H2O_GBM_dataReactive <- shiny::reactive({tryCatch({DataList[[shiny::req(input$H2O_GBM_data)]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(H2O_GBM_dataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(H2O_GBM_dataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(H2O_GBM_dataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = 'H2O_GBM_TargetColumnName', Label='Select Target', Choices = ChoiceList, SelectedDefault = if(length(input$H2O_GBM_TargetColumnName) > 0L) input$H2O_GBM_TargetColumnName else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = 'H2O_GBM_FeatureColNames', Label='Select Features', Choices = ChoiceList, SelectedDefault = if(length(input$H2O_GBM_FeatureColNames) > 0L) input$H2O_GBM_FeatureColNames else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = 'H2O_GBM_PrimaryDateColumn', Label='Select Date', Choices = ChoiceList, SelectedDefault = if(length(input$H2O_GBM_PrimaryDateColumn) > 0L) input$H2O_GBM_PrimaryDateColumn else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = 'H2O_GBM_TransformNumericColumns', Label='Transform Columns', Choices = ChoiceList, SelectedDefault = if(length(input$H2O_GBM_TransformNumericColumns) > 0L) input$H2O_GBM_TransformNumericColumns else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = 'H2O_GBM_WeightsColumn', Label='Weights Column', Choices = ChoiceList, SelectedDefault = if(length(input$H2O_GBM_WeightsColumn) > 0L) input$H2O_GBM_WeightsColumn else NULL, Multiple = TRUE, MaxVars = 1L)
    })

    H2O_GBM_TargetTypeReactive <- shiny::reactive({input$H2O_GBM_TargetType})
    shiny::observeEvent(H2O_GBM_TargetTypeReactive(), {
      out <- Quantico:::Shiny.ML.H2O.DistributionOptions(shiny::req(H2O_GBM_TargetTypeReactive()))
      Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID='H2O_GBM_Distribution', Label='Loss Function', Choices = out$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = out$Default, CloseAfterSelect = FALSE)
    })


  })
  shiny::observeEvent(input$H2O_GBM_OK, {shiny::removeModal()})

  # H2O_GLM DropDown
  shiny::observeEvent(input$H2O_GLM_Modal, {
    Quantico:::H2O_GLM_Modal_Fun(
      id = 'H2O_GLMMLID',

      # Reactives
      H2O_GLM_TargetColumnName_Selected = if(length(input$H2O_GLM_TargetColumnName) > 0L) input$H2O_GLM_TargetColumnName else NULL,
      H2O_GLM_FeatureColNames_Selected = if(length(input$H2O_GLM_FeatureColNames) > 0L) input$H2O_GLM_FeatureColNames else NULL,
      H2O_GLM_TransformNumericColumns_Selected = if(length(input$H2O_GLM_TransformNumericColumns) > 0L) input$H2O_GLM_TransformNumericColumns else NULL,
      H2O_GLM_WeightsColumn_Selected = if(length(input$H2O_GLM_WeightsColumn) > 0L) input$H2O_GLM_WeightsColumn else NULL,
      H2O_GLM_Distribution_Selected = if(length(input$H2O_GLM_Distribution) > 0L) input$H2O_GLM_Distribution else NULL,
      H2O_GLM_Link_Selected = if(length(input$H2O_GLM_Link) > 0L) input$H2O_GLM_Link else NULL,
      H2O_GLM_eval_metric_Selected = if(length(input$H2O_GLM_eval_metric) > 0L) input$H2O_GLM_eval_metric else NULL,

      # Statics
      H2O_GLM_Runs_Choices = 1L:30L,
      H2O_GLM_TargetType_Choices = c('Regression','Binary Classification','MultiClass'),
      H2O_GLM_data_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      H2O_GLM_ValidationData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      H2O_GLM_TestData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      H2O_GLM_EncodeMethod_Choices = c('meow','credibility','binary','m_estimator','woe','target_encoding','poly_encode','backward_difference','helmert'),
      H2O_GLM_Methods_Choices = c('BoxCox', 'Asinh', 'Log', 'LogPlus1', 'Sqrt', 'Asin', 'Logit'),
      H2O_GLM_NThreads_Choices = c(-1L, 1L:512L),
      H2O_GLM_MaxMem_Choices = 4L:512L,
      H2O_GLM_TrainOnFull_Choices = c(FALSE, TRUE),
      H2O_GLM_Solver_Choices = c("AUTO","IRLSM","L_BFGS","COORDINATE_DESCENT_NAIVE","COORDINATE_DESCENT","GRADIENT_DESCENT_LH","GRADIENT_DESCENT_SQERR"),
      H2O_GLM_Alpha_Choices = seq(0,1,0.01),
      H2O_GLM_Lambda_Choices = seq(0,5,0.01),
      H2O_GLM_LambdaSearch_Choices = c(TRUE,FALSE),
      H2O_GLM_NLambdas_Choices = seq(5,1000,5),
      H2O_GLM_Standardize_Choices = c(TRUE,FALSE),
      H2O_GLM_RemoveCollinearColumns_Choices = c(TRUE,FALSE),
      H2O_GLM_InterceptInclude_Choices = c(TRUE,FALSE),
      H2O_GLM_NonNegativeCoefficients_Choices = c(TRUE,FALSE),
      H2O_GLM_TweedieLinkPower_Choices = seq(1.01,1.99,0.01),
      H2O_GLM_TweedieVariancePower_Choices = seq(1.01,5,0.01),
      H2O_GLM_GridTune_Choices = c(FALSE, TRUE),
      H2O_GLM_MaxModelsInGrid_Choices = c(seq(5,20,5),seq(25,2500,25)),

      # Updaters
      H2O_GLM_ModelID_Selected = if(length(input$H2O_GLM_ModelID) > 0L) input$H2O_GLM_ModelID else "ML1",
      H2O_GLM_TargetType_Selected = if(length(input$H2O_GLM_TargetType) > 0L) input$H2O_GLM_TargetType else "Regression",
      H2O_GLM_Runs_Selected = if(length(input$H2O_GLM_Runs) > 0L) input$H2O_GLM_Runs else 1,
      H2O_GLM_data_Selected = if(length(input$H2O_GLM_data) > 0L) input$H2O_GLM_data else NULL,
      H2O_GLM_ValidationData_Selected = if(length(input$H2O_GLM_ValidationData) > 0L) input$H2O_GLM_ValidationData else NULL,
      H2O_GLM_TestData_Selected = if(length(input$H2O_GLM_TestData) > 0L) input$H2O_GLM_TestData else NULL,
      H2O_GLM_EncodeMethod_Selected = if(length(input$H2O_GLM_EncodeMethod) > 0L) input$H2O_GLM_EncodeMethod else 'credibility',
      H2O_GLM_Methods_Selected = if(length(input$H2O_GLM_Methods) > 0L) input$H2O_GLM_Methods else NULL,
      H2O_GLM_NThreads_Selected = if(length(input$H2O_GLM_NThreads) > 0L) input$H2O_GLM_NThreads else -1L,
      H2O_GLM_MaxMem_Selected = if(length(input$H2O_GLM_MaxMem) > 0L) input$H2O_GLM_MaxMem else 4L,
      H2O_GLM_TrainOnFull_Selected = if(length(input$H2O_GLM_TrainOnFull) > 0L) input$H2O_GLM_TrainOnFull else FALSE,

      H2O_GLM_Solver_Selected = if(length(input$H2O_GLM_Solver) > 0L) input$H2O_GLM_Solver else "IRSLM",
      H2O_GLM_Alpha_Selected = if(length(input$H2O_GLM_Alpha) > 0L) input$H2O_GLM_Alpha else 0,
      H2O_GLM_Lambda_Selected = if(length(input$H2O_GLM_Lambda) > 0L) input$H2O_GLM_Lambda else 0,
      H2O_GLM_LambdaSearch_Selected = if(length(input$H2O_GLM_LambdaSearch) > 0L) input$H2O_GLM_LambdaSearch else FALSE,
      H2O_GLM_NLambdas_Selected = if(length(input$H2O_GLM_NLambdas) > 0L) input$H2O_GLM_NLambdas else 5,
      H2O_GLM_Standardize_Selected = if(length(input$H2O_GLM_Standardize) > 0L) input$H2O_GLM_Standardize else FALSE,
      H2O_GLM_RemoveCollinearColumns_Selected = if(length(input$H2O_GLM_RemoveCollinearColumns) > 0L) input$H2O_GLM_RemoveCollinearColumns else TRUE,
      H2O_GLM_InterceptInclude_Selected = if(length(input$H2O_GLM_InterceptInclude) > 0L) input$H2O_GLM_InterceptInclude else TRUE,
      H2O_GLM_NonNegativeCoefficients_Selected = if(length(input$H2O_GLM_NonNegativeCoefficients) > 0L) input$H2O_GLM_NonNegativeCoefficients else FALSE,
      H2O_GLM_TweedieLinkPower_Selected = if(length(input$H2O_GLM_TweedieLinkPower) > 0L) input$H2O_GLM_TweedieLinkPower else 1.2,
      H2O_GLM_TweedieVariancePower_Selected = if(length(input$H2O_GLM_TweedieVariancePower) > 0L) input$H2O_GLM_TweedieVariancePower else 1.2,

      H2O_GLM_GridTune_Selected = if(length(input$H2O_GLM_GridTune) > 0L) input$H2O_GLM_GridTune else FALSE,
      H2O_GLM_MaxModelsInGrid_Selected = if(length(input$H2O_GLM_MaxModelsInGrid) > 0L) input$H2O_GLM_MaxModelsInGrid else 25,
      H2O_GLM_ClassWeights0_Selected = if(length(input$H2O_GLM_ClassWeights0) > 0L) input$H2O_GLM_ClassWeights0 else 1,
      H2O_GLM_ClassWeights1_Selected = if(length(input$H2O_GLM_ClassWeights1) > 0L) input$H2O_GLM_ClassWeights1 else 1)

    # Reactives
    H2O_GLM_dataReactive <- shiny::reactive({tryCatch({DataList[[shiny::req(input$H2O_GLM_data)]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(H2O_GLM_dataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(H2O_GLM_dataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(H2O_GLM_dataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='H2O_GLM_TargetColumnName', Label='Select Target', Choices = ChoiceList, SelectedDefault = if(length(input$H2O_GLM_TargetColumnName) > 0L) input$H2O_GLM_TargetColumnName else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='H2O_GLM_FeatureColNames', Label='Select Features', Choices = ChoiceList, SelectedDefault = if(length(input$H2O_GLM_FeatureColNames) > 0L) input$H2O_GLM_FeatureColNames else NULL, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='H2O_GLM_PrimaryDateColumn', Label='Select Date', Choices = ChoiceList, SelectedDefault = if(length(input$H2O_GLM_PrimaryDateColumn) > 0L) input$H2O_GLM_PrimaryDateColumn else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='H2O_GLM_WeightsColumn', Label='Weights Column', Choices = ChoiceList, SelectedDefault = if(length(input$H2O_GLM_WeightsColumn) > 0L) input$H2O_GLM_WeightsColumn else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='H2O_GLM_TransformNumericColumns', Label='Transform Columns', Choices = ChoiceList, SelectedDefault = if(length(input$H2O_GLM_TransformNumericColumns) > 0L) input$H2O_GLM_TransformNumericColumns else NULL, Multiple = TRUE)
    })

    H2O_GLM_TargetTypeReactive <- shiny::reactive({input$H2O_GLM_TargetType})
    shiny::observeEvent(H2O_GLM_TargetTypeReactive(), {
      out <- Quantico:::Shiny.ML.H2O.GLM.DistributionOptions(H2O_GLM_TargetTypeReactive())
      Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID='H2O_GLM_Distribution', Label='Distribution', Choices = out$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = 'AUTO', CloseAfterSelect = FALSE)
      out <- Quantico:::Shiny.ML.H2O.LinkOptions(H2O_GLM_TargetTypeReactive())
      Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID='H2O_GLM_Link', Label='Link', Choices = out$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = 'AUTO', CloseAfterSelect = FALSE)
      out <- Quantico:::Shiny.ML.H2O_GLM.EvalMetricOptions(H2O_GLM_TargetTypeReactive())
      Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID='H2O_GLM_eval_metric', Label='Eval Metric', Choices = out$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = out$Default, CloseAfterSelect = FALSE)
    })
  })
  shiny::observeEvent(input$H2O_GLM_OK, {shiny::removeModal()})

  # H2O_HGLM DropDown
  shiny::observeEvent(input$H2O_HGLM_Modal, {
    Quantico:::H2O_HGLM_Modal_Fun(
      id = 'H2O_HGLMMLID',

      # Reactives
      H2O_HGLM_TargetColumnName_Selected = if(length(input$H2O_HGLM_TargetColumnName) > 0L) input$H2O_HGLM_TargetColumnName else NULL,
      H2O_HGLM_FeatureColNames_Selected = if(length(input$H2O_HGLM_FeatureColNames) > 0L) input$H2O_HGLM_FeatureColNames else NULL,
      H2O_HGLM_RandomColNumbers_Selected = if(length(input$H2O_HGLM_RandomColNumbers) > 0L) input$H2O_HGLM_RandomColNumbers else NULL,
      H2O_HGLM_TransformNumericColumns_Selected = if(length(input$H2O_HGLM_TransformNumericColumns) > 0L) input$H2O_HGLM_TransformNumericColumns else NULL,
      H2O_HGLM_WeightsColumn_Selected = if(length(input$H2O_HGLM_WeightsColumn) > 0L) input$H2O_HGLM_WeightsColumn else NULL,
      H2O_HGLM_Distribution_Selected = if(length(input$H2O_HGLM_Distribution) > 0L) input$H2O_HGLM_Distribution else NULL,
      H2O_HGLM_Link_Selected = if(length(input$H2O_HGLM_Link) > 0L) input$H2O_HGLM_Link else NULL,
      H2O_HGLM_eval_metric_Selected = if(length(input$H2O_HGLM_eval_metric) > 0L) input$H2O_HGLM_eval_metric else NULL,

      # Statics
      H2O_HGLM_Runs_Choices = 1L:30L,
      H2O_HGLM_TargetType_Choices = c('Regression'),
      H2O_HGLM_data_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      H2O_HGLM_EncodeMethod_Choices = c('meow','credibility','binary','m_estimator','woe','target_encoding','poly_encode','backward_difference','helmert'),
      H2O_HGLM_Methods_Choices = c('BoxCox', 'Asinh', 'Log', 'LogPlus1', 'Sqrt', 'Asin', 'Logit'),
      H2O_HGLM_NThreads_Choices = c(-1L, 1L:512L),
      H2O_HGLM_MaxMem_Choices = 4L:512L,
      H2O_HGLM_TrainOnFull_Choices = c(FALSE, TRUE),
      H2O_HGLM_Solver_Choices = c("AUTO","IRLSM","L_BFGS","COORDINATE_DESCENT_NAIVE","COORDINATE_DESCENT","GRADIENT_DESCENT_LH","GRADIENT_DESCENT_SQERR"),
      H2O_HGLM_Alpha_Choices = seq(0,1,0.01),
      H2O_HGLM_Lambda_Choices = seq(0,5,0.01),
      H2O_HGLM_LambdaSearch_Choices = c(TRUE,FALSE),
      H2O_HGLM_NLambdas_Choices = seq(5,1000,5),
      H2O_HGLM_Standardize_Choices = c(TRUE,FALSE),
      H2O_HGLM_RemoveCollinearColumns_Choices = c(TRUE,FALSE),
      H2O_HGLM_InterceptInclude_Choices = c(TRUE,FALSE),
      H2O_HGLM_NonNegativeCoefficients_Choices = c(TRUE,FALSE),
      H2O_HGLM_TweedieLinkPower_Choices = seq(1.01,1.99,0.01),
      H2O_HGLM_TweedieVariancePower_Choices = seq(1.01,5,0.01),
      H2O_HGLM_GridTune_Choices = c(FALSE, TRUE),
      H2O_HGLM_MaxModelsInGrid_Choices = c(seq(5,20,5),seq(25,2500,25)),
      H2O_HGLM_RandomDistribution_Choices = 'Gaussian',
      H2O_HGLM_RandomLink_Choices = "identity",

      # Updaters
      H2O_HGLM_ModelID_Selected = if(length(input$H2O_HGLM_ModelID) > 0L) input$H2O_HGLM_ModelID else "ML1",
      H2O_HGLM_TargetType_Selected = if(length(input$H2O_HGLM_TargetType) > 0L) input$H2O_HGLM_TargetType else "Regression",
      H2O_HGLM_Runs_Selected = if(length(input$H2O_HGLM_Runs) > 0L) input$H2O_HGLM_Runs else 1,
      H2O_HGLM_data_Selected = if(length(input$H2O_HGLM_data) > 0L) input$H2O_HGLM_data else NULL,
      H2O_HGLM_ValidationData_Selected = if(length(input$H2O_HGLM_ValidationData) > 0L) input$H2O_HGLM_ValidationData else NULL,
      H2O_HGLM_TestData_Selected = if(length(input$H2O_HGLM_TestData) > 0L) input$H2O_HGLM_TestData else NULL,
      H2O_HGLM_EncodeMethod_Selected = if(length(input$H2O_HGLM_EncodeMethod) > 0L) input$H2O_HGLM_EncodeMethod else 'credibility',
      H2O_HGLM_Methods_Selected = if(length(input$H2O_HGLM_Methods) > 0L) input$H2O_HGLM_Methods else NULL,
      H2O_HGLM_NThreads_Selected = if(length(input$H2O_HGLM_NThreads) > 0L) input$H2O_HGLM_NThreads else -1L,
      H2O_HGLM_MaxMem_Selected = if(length(input$H2O_HGLM_MaxMem) > 0L) input$H2O_HGLM_MaxMem else 4L,
      H2O_HGLM_TrainOnFull_Selected = if(length(input$H2O_HGLM_TrainOnFull) > 0L) input$H2O_HGLM_TrainOnFull else FALSE,
      H2O_HGLM_RandomDistribution_Selected = 'Gaussian',
      H2O_HGLM_RandomLink_Selected = "identity",
      H2O_HGLM_Solver_Selected = if(length(input$H2O_HGLM_Solver) > 0L) input$H2O_HGLM_Solver else "AUTO",
      H2O_HGLM_Alpha_Selected = if(length(input$H2O_HGLM_Alpha) > 0L) input$H2O_HGLM_Alpha else 0,
      H2O_HGLM_Lambda_Selected = if(length(input$H2O_HGLM_Lambda) > 0L) input$H2O_HGLM_Lambda else 0,
      H2O_HGLM_LambdaSearch_Selected = if(length(input$H2O_HGLM_LambdaSearch) > 0L) input$H2O_HGLM_LambdaSearch else FALSE,
      H2O_HGLM_NLambdas_Selected = if(length(input$H2O_HGLM_NLambdas) > 0L) input$H2O_HGLM_NLambdas else 5,
      H2O_HGLM_Standardize_Selected = if(length(input$H2O_HGLM_Standardize) > 0L) input$H2O_HGLM_Standardize else FALSE,
      H2O_HGLM_RemoveCollinearColumns_Selected = if(length(input$H2O_HGLM_RemoveCollinearColumns) > 0L) input$H2O_HGLM_RemoveCollinearColumns else FALSE,
      H2O_HGLM_InterceptInclude_Selected = if(length(input$H2O_HGLM_InterceptInclude) > 0L) input$H2O_HGLM_InterceptInclude else TRUE,
      H2O_HGLM_NonNegativeCoefficients_Selected = if(length(input$H2O_HGLM_NonNegativeCoefficients) > 0L) input$H2O_HGLM_NonNegativeCoefficients else FALSE,
      H2O_HGLM_TweedieLinkPower_Selected = if(length(input$H2O_HGLM_TweedieLinkPower) > 0L) input$H2O_HGLM_TweedieLinkPower else 1.2,
      H2O_HGLM_TweedieVariancePower_Selected = if(length(input$H2O_HGLM_TweedieVariancePower) > 0L) input$H2O_HGLM_TweedieVariancePower else 1.2,
      H2O_HGLM_GridTune_Selected = if(length(input$H2O_HGLM_GridTune) > 0L) input$H2O_HGLM_GridTune else FALSE,
      H2O_HGLM_MaxModelsInGrid_Selected = if(length(input$H2O_HGLM_MaxModelsInGrid) > 0L) input$H2O_HGLM_MaxModelsInGrid else 25,
      H2O_HGLM_ClassWeights0_Selected = if(length(input$H2O_HGLM_ClassWeights0) > 0L) input$H2O_HGLM_ClassWeights0 else 1,
      H2O_HGLM_ClassWeights1_Selected = if(length(input$H2O_HGLM_ClassWeights1) > 0L) input$H2O_HGLM_ClassWeights1 else 1)

    H2O_HGLM_dataReactive <- shiny::reactive({tryCatch({DataList[[shiny::req(input$H2O_HGLM_data)]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(H2O_HGLM_dataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(H2O_HGLM_dataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(H2O_HGLM_dataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='H2O_HGLM_TargetColumnName', Label='Select Target', Choices = ChoiceList, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='H2O_HGLM_FeatureColNames', Label='Select Features', Choices = ChoiceList, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='H2O_HGLM_RandomColNumbers', Label='Random Effects', Choices = ChoiceList, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='H2O_HGLM_WeightsColumn', Label='Weights Column', Choices = ChoiceList, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='H2O_HGLM_TransformNumericColumns', Label='Transform Columns', Choices = ChoiceList, Multiple = TRUE)
    })

    H2O_HGLM_TargetTypeReactive <- shiny::reactive({input$H2O_GLM_TargetType})
    shiny::observeEvent(H2O_HGLM_TargetTypeReactive(), {
      out <- Quantico:::Shiny.ML.H2O_GLM.EvalMetricOptions(H2O_HGLM_TargetTypeReactive())
      if(Debug) {print(out); print(H2O_HGLM_TargetTypeReactive())}
      Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID='H2O_HGLM_eval_metric', Label='Eval Metric', Choices = out$Choices, Multiple = TRUE, MaxVars = 1, SelectedDefault = out$Default, CloseAfterSelect = FALSE)
    })

  })
  shiny::observeEvent(input$H2O_HGLM_OK, {shiny::removeModal()})

  # CausalMediation DropDown
  shiny::observeEvent(input$CausalMediation_Modal, {
    Quantico:::CausalMediation_Modal_Fun(
      id = 'CausalMediationMLID',

      # Reactives
      CausalMediation_OutcomeTargetVariable_Selected = if(length(input$CausalMediation_OutcomeTargetVariable) > 0L) input$CausalMediation_OutcomeTargetVariable else NULL,
      CausalMediation_TreatmentVariable_Selected = if(length(input$CausalMediation_TreatmentVariable) > 0L) input$CausalMediation_TreatmentVariable else NULL,
      CausalMediation_MediatorVariable_Selected = if(length(input$CausalMediation_MediatorVariable) > 0L) input$CausalMediation_MediatorVariable else NULL,
      CausalMediation_SurvivalEventVariable_Selected = if(length(input$CausalMediation_SurvivalEventVariable) > 0L) input$CausalMediation_SurvivalEventVariable else NULL,
      CausalMediation_Covariates_Selected = if(length(input$CausalMediation_Covariates) > 0L) input$CausalMediation_Covariates else NULL,
      CausalMediation_OM_MediatorCovariates_Selected = if(length(input$CausalMediation_OM_MediatorCovariates) > 0L) input$CausalMediation_OM_MediatorCovariates else NULL,
      CausalMediation_OM_TreatmentCovariates_Selected = if(length(input$CausalMediation_OM_TreatmentCovariates) > 0L) input$CausalMediation_OM_TreatmentCovariates else NULL,
      CausalMediation_MM_TreatmentCovariates_Selected = if(length(input$CausalMediation_MM_TreatmentCovariates) > 0L) input$CausalMediation_MM_TreatmentCovariates else NULL,

      # Statics
      CausalMediation_data_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      CausalMediation_OutcomeTargetType_Choices = c("linear","logistic","loglinear","poisson","negbin","survCox","survAFT_exp","survAFT_weibull"),
      CausalMediation_MediatorTargetType_Choices = c("linear","logistic"),
      CausalMediation_TreatmentMediatorInteraction_Choices = c(FALSE,TRUE),
      CausalMediation_RemoveNA_Choices = c(FALSE,TRUE),
      CausalMediation_CaseControlSourceData_Choices = c(FALSE,TRUE),
      CausalMediation_Treated_ReferenceIndicator_Choices = 0L:100L,
      CausalMediation_UnTreated_ReferenceIndicator_Choices = 0L:1L,
      CausalMediation_Mediator_ControlDirectEffectLevel_Choices = 0L:100L,
      CausalMediation_Covariate_NaturalDirectIndirect_Choices = 0L:100L,

      CausalMediation_data_Selected = if(length(input$CausalMediation_data) > 0L) input$CausalMediation_data else NULL,
      CausalMediation_OutcomeTargetType_Selected = if(length(input$CausalMediation_OutcomeTargetType) > 0L) input$CausalMediation_OutcomeTargetType else 'linear',
      CausalMediation_MediatorTargetType_Selected = if(length(input$CausalMediation_MediatorTargetType) > 0L) input$CausalMediation_MediatorTargetType else 'linear',
      CausalMediation_TreatmentMediatorInteraction_Selected = if(length(input$CausalMediation_TreatmentMediatorInteraction) > 0L) input$CausalMediation_TreatmentMediatorInteraction else FALSE,
      CausalMediation_RemoveNA_Selected = if(length(input$CausalMediation_RemoveNA) > 0L) input$CausalMediation_RemoveNA else FALSE,
      CausalMediation_CaseControlSourceData_Selected = if(length(input$CausalMediation_CaseControlSourceData) > 0L) input$CausalMediation_CaseControlSourceData else FALSE,
      CausalMediation_Treated_ReferenceIndicator_Selected = if(length(input$CausalMediation_Treated_ReferenceIndicator) > 0L) input$CausalMediation_Treated_ReferenceIndicator else NULL,
      CausalMediation_UnTreated_ReferenceIndicator_Selected = if(length(input$CausalMediation_UnTreated_ReferenceIndicator) > 0L) input$CausalMediation_UnTreated_ReferenceIndicator else NULL,
      CausalMediation_Mediator_ControlDirectEffectLevel_Selected = if(length(input$CausalMediation_Mediator_ControlDirectEffectLevel) > 0L) input$CausalMediation_Mediator_ControlDirectEffectLevel else NULL,
      CausalMediation_Covariate_NaturalDirectIndirect_Selected = if(length(input$CausalMediation_Covariate_NaturalDirectIndirect) > 0L) input$CausalMediation_Covariate_NaturalDirectIndirect else NULL)

    # Reactives
    CausalMediation_dataReactive <- shiny::reactive({tryCatch({DataList[[input$CausalMediation_data]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(CausalMediation_dataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(CausalMediation_dataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(CausalMediation_dataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='CausalMediation_OutcomeTargetVariable', Label='Outcome Target', Choices = ChoiceList, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='CausalMediation_TreatmentVariable', Label='Treatment Variable', Choices = ChoiceList, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='CausalMediation_MediatorVariable', Label='Mediator Variable', Choices = ChoiceList, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='CausalMediationList_MediatorVariable', Label='Mediator Variable', Choices = ChoiceList, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='CausalMediation_SurvivalEventVariable', Label='Survival Event Variable', Choices = ChoiceList, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='CausalMediation_Covariates', Label='Outcome Covariates', Choices = ChoiceList, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='CausalMediation_OM_MediatorCovariates', Label='Outcome Mediator Covariates', Choices = ChoiceList, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='CausalMediation_OM_TreatmentCovariates', Label='Outcome Treatment Covariates', Choices = ChoiceList, Multiple = TRUE)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='CausalMediation_MM_TreatmentCovariates', Label='Mediation Treatment Covariates', Choices = ChoiceList, Multiple = TRUE)
    }, ignoreInit = TRUE)

  })
  shiny::observeEvent(input$CausalMediation_OK, {shiny::removeModal()})

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Inputs ::  ML Scoring             ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(input$ScoreML_Modal, {
    Quantico:::MLScoring_Modal_Fun(
      id = 'ScoreMLID',

      # Statics
      ScoreML_Model_Choices = tryCatch({names(ModelOutputList)}, error = function(x) NULL),
      ScoreML_Data_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      ScoreML_ReturnFeatures_Choices = c(TRUE,FALSE),
      ScoreML_ReturnShapVals_Choices = c(TRUE,FALSE),

      # Updaters
      ScoreML_Model_Selected = if(length(input$ScoreML_Model) > 0L) input$ScoreML_Model else NULL,
      ScoreML_Data_Selected = if(length(input$ScoreML_Data) > 0L) input$ScoreML_Data else NULL,
      ScoreML_ReturnFeatures_Selected = if(length(input$ScoreML_ReturnFeatures) > 0L) input$ScoreML_ReturnFeatures else TRUE,
      ScoreML_ReturnShapVals_Selected = if(length(input$ScoreML_ReturnShapVals) > 0L) input$ScoreML_Data else FALSE)
  })
  shiny::observeEvent(input$ScoreML_OK, {shiny::removeModal()})

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Inputs ::  Inference              ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Normality Testing
  shiny::observeEvent(input$Inference_Normality, {
    Quantico:::Normality_Modal_Fun(
      id = "Inference_NormalityID",
      AppWidth=12L,

      # Reactives
      Normality_YVars_Selected = if(length(input$Normality_YVars) > 0L) input$Normality_YVars else NULL,

      # Statics
      Normality_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),

      # Updaters
      Normality_SelectData_Selected = if(length(input$Normality_SelectData) > 0L) input$Normality_SelectData else NULL,
      Normality_InferenceID_Selected = if(length(input$Normality_InferenceID) > 0L) input$Normality_InferenceID else "INF_Normality",
      SampleSize.ADT_Selected = if(length(input$SampleSize.ADT) > 0L) input$SampleSize.ADT else NULL,
      Samples.ADT_Selected = if(length(input$Samples.ADT) > 0L) input$Samples.ADT else NULL,
      SampleSize.CVMT_Selected = if(length(input$SampleSize.CVMT) > 0L) input$SampleSize.CVMT else NULL,
      Samples.CVMT_Selected = if(length(input$Samples.CVMT) > 0L) input$Samples.CVMT else NULL,
      SampleSize.KST_Selected = if(length(input$SampleSize.KST) > 0L) input$SampleSize.KST else NULL,
      Samples.KST_Selected = if(length(input$Samples.KST) > 0L) input$Samples.KST else NULL,
      SampleSize.ST_Selected = if(length(input$SampleSize.ST) > 0L) input$SampleSize.ST else NULL,
      Samples.ST_Selected = if(length(input$Samples.ST) > 0L) input$Samples.ST else NULL,
      SampleSize.JBT_Selected = if(length(input$SampleSize.JBT) > 0L) input$SampleSize.JBT else NULL,
      Samples.JBT_Selected = if(length(input$Samples.JBT) > 0L) input$Samples.JBT else NULL,
      SampleSize.AT_Selected = if(length(input$SampleSize.AT) > 0L) input$SampleSize.AT else NULL,
      Samples.AT_Selected = if(length(input$Samples.AT) > 0L) input$Samples.AT else NULL)

    # Reactives
    Normality_dataReactive <- shiny::reactive({tryCatch({DataList[[shiny::req(input$Normality_SelectData)]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(shiny::req(Normality_dataReactive()), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(Normality_dataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(Normality_dataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='Normality_YVars', Label='Variables', Choices = ChoiceList, SelectedDefault = if(length(input$Normality_YVars) > 0L) input$Normality_YVars else NULL, Multiple = TRUE, MaxVars = 100L)
    })
  })
  shiny::observeEvent(input$Normality_OK, {shiny::removeModal()})

  # Correlation Testing
  shiny::observeEvent(input$Inference_Correlation, {
    Quantico:::Correlation_Modal_Fun(
      id = "Inference_CorrelationID",
      AppWidth=12L,

      # Reactives
      Correlation_CorrVars_Selected = if(length(input$Correlation_CorrVars) > 0L) input$Correlation_CorrVars else NULL,
      Correlation_DateVar_Selected = if(length(input$Correlation_DateVar) > 0L) input$Correlation_DateVar else NULL,

      # Statics
      Correlation_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      Correlation_P_Adjust_Choices = c("holm","hochberg","hommel","bonferroni","BH","BY","fdr","somers","none"),
      Correlation_Bayesian_Choices = c(TRUE,FALSE),
      Correlation_Bayesian_Prior_Choices = c("medium", "medium.narrow","medium","wide","ultrawide"),
      Correlation_MultiLevel_Choices = c(TRUE,FALSE),
      Correlation_Include_Factors_Choices = c(TRUE,FALSE),
      Correlation_Partial_Choices = c(TRUE,FALSE),
      Correlation_Partial_Bayesian_Choices = c(TRUE,FALSE),

      # Updaters
      Correlation_SelectData_Selected = if(length(input$Correlation_SelectData) > 0L) input$Correlation_SelectData else NULL,
      Correlation_InferenceID_Selected = if(length(input$Correlation_InferenceID) > 0L) input$Correlation_InferenceID else "INF_Correlation",
      Correlation_SampleSize_Selected = if(length(input$Correlation_SampleSize) > 0L) input$Correlation_SampleSize else 10000,
      Correlation_P_Adjust_Selected = if(length(input$Correlation_P_Adjust) > 0L) input$Correlation_P_Adjust else "holm",
      Correlation_Bayesian_Selected = if(length(input$Correlation_Bayesian) > 0L) input$Correlation_Bayesian else FALSE,
      Correlation_Bayesian_Prior_Selected = if(length(input$Correlation_Bayesian_Prior) > 0L) input$Correlation_Bayesian_Prior else NULL,
      Correlation_MultiLevel_Selected = if(length(input$Correlation_MultiLevel) > 0L) input$Correlation_MultiLevel else FALSE,
      Correlation_Include_Factors_Selected = if(length(input$Correlation_Include_Factors) > 0L) input$Correlation_Include_Factors else FALSE,
      Correlation_Partial_Selected = if(length(input$Correlation_Partial) > 0L) input$Correlation_Partial else FALSE,
      Correlation_Partial_Bayesian_Selected = if(length(input$Correlation_Partial_Bayesian) > 0L) input$Correlation_Partial_Bayesian else FALSE)

    # Reactives
    Correlation_dataReactive <- shiny::reactive({tryCatch({DataList[[shiny::req(input$Correlation_SelectData)]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(shiny::req(Correlation_dataReactive()), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(Correlation_dataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(Correlation_dataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='Correlation_CorrVars', Label='Variables', Choices = ChoiceList, SelectedDefault = if(length(input$Correlation_CorrVars) > 0L) input$Correlation_CorrVars else NULL, Multiple = TRUE, MaxVars = 100L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='Correlation_DateVar', Label='Date Variable', Choices = ChoiceList, SelectedDefault = if(length(input$Correlation_DateVar) > 0L) input$Correlation_DateVar else NULL, Multiple = TRUE, MaxVars = 100L)
    })
  })
  shiny::observeEvent(input$Correlation_OK, {shiny::removeModal()})

  # One Sample TTest
  shiny::observeEvent(input$Inference_1STTest, {
    Quantico::OneSampleTTest_Modal_Fun(
      id = "OneSampleTTest_Modal_FunID",
      AppWidth=12L,

      # Reactives
      OneSampleTTest_Variable_Selected = if(length(input$OneSampleTTest_Variable) > 0L) input$OneSampleTTest_Variable else NULL,

      # Statics
      OneSampleTTest_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      OneSampleTTest_Alternative_Choices = c("two.sided","less","greater"),

      # Updaters
      OneSampleTTest_SelectData_Selected = if(length(input$OneSampleTTest_SelectData) > 0L) input$OneSampleTTest_SelectData else NULL,
      OneSampleTTest_InferenceID_Selected = if(length(input$OneSampleTTest_InferenceID) > 0L) input$OneSampleTTest_InferenceID else "INF_OneSampleTTest",
      OneSampleTTest_NullValue_Selected = if(length(input$OneSampleTTest_NullValue) > 0L) input$OneSampleTTest_NullValue else 0,
      OneSampleTTest_Alternative_Selected = if(length(input$OneSampleTTest_Alternative) > 0L) input$OneSampleTTest_Alternative else "two.sided",
      OneSampleTTest_ConfidenceLevel_Selected = if(length(input$OneSampleTTest_ConfidenceLevel) > 0L) input$OneSampleTTest_ConfidenceLevel else 0.95,
      OneSampleTTest_Samples_Selected = if(length(input$OneSampleTTest_Samples) > 0L) input$OneSampleTTest_Samples else 1,
      OneSampleTTest_SampleSize_Selected = if(length(input$OneSampleTTest_SampleSize) > 0L) input$OneSampleTTest_SampleSize else 100000)

    # Reactives
    OneSampleTTest_dataReactive <- shiny::reactive({tryCatch({DataList[[shiny::req(input$OneSampleTTest_SelectData)]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(shiny::req(OneSampleTTest_dataReactive()), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(OneSampleTTest_dataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(OneSampleTTest_dataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='OneSampleTTest_Variable', Label='Variable', Choices = ChoiceList, SelectedDefault = if(length(input$OneSampleTTest_Variable) > 0L) input$OneSampleTTest_Variable else NULL, Multiple = TRUE, MaxVars = 1L)
    })
  })
  shiny::observeEvent(input$OneSampleTTest_OK, {shiny::removeModal()})

  # Two Sample TTest
  shiny::observeEvent(input$Inference_2STTest, {
    Quantico::TwoSampleTTest_Modal_Fun(
      id = "TwoSampleTTest_ModalID",
      AppWidth=12L,

      # Reactives
      TwoSampleTTest_Variable1_Selected = NULL,
      TwoSampleTTest_Variable2_Selected = NULL,

      # Statics
      TwoSampleTTest_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      TwoSampleTTest_Alternative_Choices = c("two.sided","less","greater"),
      TwoSampleTTest_Paired_Choices = c(TRUE, FALSE),
      TwoSampleTTest_EqualVariance_Choices = c(TRUE, FALSE),

      # Updaters
      TwoSampleTTest_SelectData_Selected = if(length(input$TwoSampleTTest_SelectData) > 0L) input$TwoSampleTTest_SelectData else NULL,
      TwoSampleTTest_InferenceID_Selected = if(length(input$TwoSampleTTest_InferenceID) > 0L) input$TwoSampleTTest_InferenceID else "INF_TwoSampleTTest",
      TwoSampleTTest_Paired_Selected = if(length(input$TwoSampleTTest_Paired) > 0L) input$TwoSampleTTest_Paired else FALSE,
      TwoSampleTTest_EqualVariance_Selected = if(length(input$TwoSampleTTest_EqualVariance) > 0L) input$TwoSampleTTest_EqualVariance else FALSE,
      TwoSampleTTest_NullValue_Selected = if(length(input$TwoSampleTTest_NullValue) > 0L) input$TwoSampleTTest_NullValue else 0,
      TwoSampleTTest_Alternative_Selected = if(length(input$TwoSampleTTest_Alternative) > 0L) input$TwoSampleTTest_Alternative else "two.sided",
      TwoSampleTTest_ConfidenceLevel_Selected = if(length(input$TwoSampleTTest_ConfidenceLevel) > 0L) input$TwoSampleTTest_ConfidenceLevel else 0.95,
      TwoSampleTTest_Samples_Selected = if(length(input$TwoSampleTTest_Samples) > 0L) input$TwoSampleTTest_Samples else 1,
      TwoSampleTTest_SampleSize_Selected = if(length(input$TwoSampleTTest_SampleSize) > 0L) input$TwoSampleTTest_SampleSize else 100000)

    # Reactives
    TwoSampleTTest_dataReactive <- shiny::reactive({tryCatch({DataList[[shiny::req(input$TwoSampleTTest_SelectData)]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(shiny::req(TwoSampleTTest_dataReactive()), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(TwoSampleTTest_dataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(TwoSampleTTest_dataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='TwoSampleTTest_Variable1', Label='Variable1', Choices = ChoiceList, SelectedDefault = if(length(input$TwoSampleTTest_Variable1) > 0L) input$TwoSampleTTest_Variable1 else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='TwoSampleTTest_Variable2', Label='Variable2', Choices = ChoiceList, SelectedDefault = if(length(input$TwoSampleTTest_Variable2) > 0L) input$TwoSampleTTest_Variable2 else NULL, Multiple = TRUE, MaxVars = 1L)
    })
  })
  shiny::observeEvent(input$TwoSampleTTest_OK, {shiny::removeModal()})

  # F-Test
  shiny::observeEvent(input$Inference_FTest, {
    Quantico::FTest_Modal_Fun(
      id = "FTest_ModalID",
      AppWidth=12L,

      # Reactives
      FTest_Variable1_Selected = if(length(input$FTest_Variable1) > 0) input$FTest_Variable1 else NULL,
      FTest_Variable2_Selected = if(length(input$FTest_Variable2) > 0) input$FTest_Variable2 else NULL,

      # Statics
      FTest_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      FTest_Alternative_Choices = c("two.sided","less","greater"),

      # Updaters
      FTest_SelectData_Selected = if(length(input$FTest_SelectData) > 0L) input$FTest_SelectData else NULL,
      FTest_InferenceID_Selected = if(length(input$FTest_InferenceID) > 0L) input$FTest_InferenceID else "INF_FTest",
      FTest_RatioVariances_Selected = if(length(input$FTest_RatioVariances) > 0L) input$FTest_RatioVariances else 1,
      FTest_Alternative_Selected = if(length(input$FTest_Alternative) > 0L) input$FTest_Alternative else "two.sided",
      FTest_ConfidenceLevel_Selected = if(length(input$FTest_ConfidenceLevel) > 0L) input$FTest_ConfidenceLevel else 0.95,
      FTest_Samples_Selected = if(length(input$FTest_Samples) > 0L) input$FTest_Samples else 1,
      FTest_SampleSize_Selected = if(length(input$FTest_SampleSize) > 0L) input$FTest_SampleSize else 100000)

    # Reactives
    FTest_dataReactive <- shiny::reactive({tryCatch({DataList[[shiny::req(input$FTest_SelectData)]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(shiny::req(FTest_dataReactive()), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(FTest_dataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(FTest_dataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='FTest_Variable1', Label='Variable1', Choices = ChoiceList, SelectedDefault = if(length(input$FTest_Variable1) > 0L) input$FTest_Variable1 else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='FTest_Variable2', Label='Variable2', Choices = ChoiceList, SelectedDefault = if(length(input$FTest_Variable2) > 0L) input$FTest_Variable2 else NULL, Multiple = TRUE, MaxVars = 1L)
    })
  })
  shiny::observeEvent(input$FTest_OK, {shiny::removeModal()})

  # Chi-Square Test
  shiny::observeEvent(input$Inference_ChiSq, {
    Quantico::ChiSquareTest_Modal_Fun(
      id = "ChiSquareModalID",
      AppWidth=12L,

      # Reactives
      ChiSquareTest_Variable1_Selected = if(length(input$ChiSquareTest_Variable1) > 0L) input$ChiSquareTest_Variable1 else NULL,
      ChiSquareTest_Variable2_Selected = if(length(input$ChiSquareTest_Variable2) > 0L) input$ChiSquareTest_Variable2 else NULL,

      # Statics
      ChiSquareTest_SelectData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      ChiSquareTest_Alternative_Choices = c("two.sided","less","greater"),

      # Updaters
      ChiSquareTest_SelectData_Selected = if(length(input$ChiSquareTest_SelectData) > 0L) input$ChiSquareTest_SelectData else NULL,
      ChiSquareTest_InferenceID_Selected = if(length(input$ChiSquareTest_InferenceID) > 0L) input$ChiSquareTest_InferenceID else "INF_ChiSquareTest",
      ChiSquareTest_Alternative_Selected = if(length(input$ChiSquareTest_Alternative) > 0L) input$ChiSquareTest_Alternative else "two.sided",
      ChiSquareTest_ConfidenceLevel_Selected = if(length(input$ChiSquareTest_ConfidenceLevel) > 0L) input$ChiSquareTest_ConfidenceLevel else 0.95,
      ChiSquareTest_Samples_Selected = if(length(input$ChiSquareTest_Samples) > 0L) input$ChiSquareTest_Samples else 1,
      ChiSquareTest_SampleSize_Selected = if(length(input$ChiSquareTest_SampleSize) > 0L) input$ChiSquareTest_SampleSize else 100000)

    # Reactives
    ChiSquareTest_dataReactive <- shiny::reactive({tryCatch({DataList[[shiny::req(input$ChiSquareTest_SelectData)]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(shiny::req(ChiSquareTest_dataReactive()), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(ChiSquareTest_dataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(ChiSquareTest_dataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='ChiSquareTest_Variable1', Label='Variable1', Choices = ChoiceList, SelectedDefault = if(length(input$ChiSquareTest_Variable1) > 0L) input$ChiSquareTest_Variable1 else NULL, Multiple = TRUE, MaxVars = 1L)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID='ChiSquareTest_Variable2', Label='Variable2', Choices = ChoiceList, SelectedDefault = if(length(input$ChiSquareTest_Variable2) > 0L) input$ChiSquareTest_Variable2 else NULL, Multiple = TRUE, MaxVars = 1L)
    })
  })
  shiny::observeEvent(input$ChiSquareTest_OK, {shiny::removeModal()})

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Inputs :: Forecasting             ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  if(Debug) print("Inputs :: Forecasting")

  # TBATS DropDown
  shiny::observeEvent(input$TBATS_Modal, {
    Quantico:::TBATS_Modal_Fun(
      id = "TBATS_Modal_ID",
      AppWidth=12L,

      # Reactives
      ModelID_Selected = if(length(input$TBATS_ModelID) > 0L) input$TBATS_ModelID else "TBATS_01",
      TargetColumnName_Selected = if(length(input$TBATS_TargetColumnName) > 0L) input$TBATS_TargetColumnName else NULL,
      DateColumnName_Selected = if(length(input$TBATS_DateColumnName) > 0L) input$TBATS_DateColumnName else NULL,

      # Statics
      RunMode_Choices = c("Grid Tune", "Forecast"),
      ArgsList_Choices = tryCatch({names(ModelOutputList)}, error = function(x) NULL),
      data_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      TimeUnit_Choices = c("1Min", "5Min", "10Min", "15Min", "30Min", "hour", "day", "week", "month", "quarter", "year"),
      FCPeriods_Choices = 1L:1000L,
      EvaluationMetric_Choices = c("MSE", "MAE", "MAPE"),
      MaxLags_Choices = 0L:500L,
      MaxMovingAverages_Choices = 0L:500L,
      TrainWeighting_Choices = seq(0.0,1,0.01),

      # Updaters
      RunMode_Selected = if(length(input$TBATS_RunMode) > 0L) input$TBATS_RunMode else "Grid Tune",
      ArgsList_Selected = if(length(input$TBATS_ArgsList) > 0L) input$TBATS_ArgsList else NULL,
      data_Selected = if(length(input$TBATS_Data) > 0L) input$TBATS_Data else NULL,
      TimeUnit_Selected = if(length(input$TBATS_TimeUnit) > 0L) input$TBATS_TimeUnit else "day",
      FCPeriods_Selected = if(length(input$TBATS_FCPeriods) > 0L) input$TBATS_FCPeriods else 5,
      EvaluationMetric_Selected = if(length(input$TBATS_EvaluationMetric) > 0L) input$TBATS_EvaluationMetric else "MSE",
      MaxLags_Selected = if(length(input$TBATS_MaxLags) > 0L) input$TBATS_MaxLags else 0,
      MaxMovingAverages_Selected = if(length(input$TBATS_MaxMovingAverages) > 0L) input$TBATS_MaxMovingAverages else 0,
      TrainWeighting_Selected = if(length(input$TBATS_TrainWeighting) > 0L) input$TBATS_TrainWeighting else 0.50,
      DebugMode_Selected = Debug)

    # Reactives
    TBATS_DataReactive <- shiny::reactive({tryCatch({DataList[[input$TBATS_Data]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(TBATS_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(TBATS_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(TBATS_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = "TBATS_TargetColumnName", Label = "Target Variable", Choices = ChoiceList, SelectedDefault = if(length(input$TBATS_TargetColumnName) > 0L) input$TBATS_TargetColumnName else NULL, Multiple = TRUE, MaxVars = 1)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = "TBATS_DateColumnName", Label = "Date Variable", Choices = ChoiceList, SelectedDefault = if(length(input$TBATS_DateColumnName) > 0L) input$TBATS_DateColumnName else NULL, Multiple = TRUE, MaxVars = 1)
    }, ignoreInit = FALSE)

  })
  shiny::observeEvent(input$TBATS_OK, {shiny::removeModal()})
  shiny::observeEvent(input$TBATS_RefreshInputs, {shinyjs::click(id = 'TBATS_Modal', asis = TRUE)})

  # SARIMA DropDown
  shiny::observeEvent(input$SARIMA_Modal, {
    Quantico:::Sarima_Modal_Fun(
      id = "SARIMA_Modal_ID",
      AppWidth=12L,

      # Reactives
      ModelID_Selected = if(length(input$NNET_ModelID) > 0L) input$NNET_ModelID else "SARIMA_01",
      TargetColumnName_Selected = if(length(input$SARIMA_TargetColumnName) > 0L) input$SARIMA_TargetColumnName else NULL,
      DateColumnName_Selected = if(length(input$SARIMA_DateColumnName) > 0L) input$SARIMA_DateColumnName else NULL,

      # Statics
      RunMode_Choices = c("Grid Tune", "Forecast"),
      ArgsList_Choices = tryCatch({names(ModelOutputList)}, error = function(x) NULL),
      data_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      TimeUnit_Choices = c("1Min", "5Min", "10Min", "15Min", "30Min", "hour", "day", "week", "month", "quarter", "year"),
      FCPeriods_Choices = 1L:500L,
      EvaluationMetric_Choices = c("MSE", "MAE", "MAPE"),
      MaxLags_Choices = 0L:500L,
      MaxSeasonalLags_Choices = 0L:10L,
      MaxMovingAverages_Choices = 0L:500L,
      MaxSeasonalMovingAverages_Choices = 0L:50L,
      MaxFourierPairs_Choices = 0L:2L,
      TrainWeighting_Choices = seq(0.0,1,0.01),
      MaxConsecutiveFails_Choices = 1L:250L,
      MaxNumberModels_Choices = seq(10L,1000L,10L),
      MaxRunTimeMinutes_Choices = 1L:360L,

      # Updaters
      RunMode_Selected = if(length(input$SARIMA_RunMode) > 0L) input$SARIMA_RunMode else "Grid Tune",
      ArgsList_Selected = if(length(input$SARIMA_ArgsList) > 0L) input$SARIMA_ArgsList else NULL,
      data_Selected = if(length(input$SARIMA_Data) > 0L) input$SARIMA_Data else NULL,
      TimeUnit_Selected = if(length(input$SARIMA_TimeUnit) > 0L) input$SARIMA_TimeUnit else "day",
      FCPeriods_Selected = if(length(input$SARIMA_FCPeriods) > 0L) input$SARIMA_FCPeriods else 5,
      EvaluationMetric_Selected = if(length(input$SARIMA_EvaluationMetric) > 0L) input$SARIMA_EvaluationMetric else "MSE",
      MaxLags_Selected = if(length(input$SARIMA_MaxLags) > 0L) input$SARIMA_MaxLags else 0,
      MaxSeasonalLags_Selected = if(length(input$SARIMA_MaxSeasonalLags) > 0L) input$SARIMA_MaxSeasonalLags else 0,
      MaxMovingAverages_Selected = if(length(input$SARIMA_MaxMovingAverages) > 0L) input$SARIMA_MaxMovingAverages else 0,
      MaxSeasonalMovingAverages_Selected = if(length(input$SARIMA_MaxSeasonalMovingAverages) > 0L) input$SARIMA_MaxSeasonalMovingAverages else 0,
      MaxFourierPairs_Selected = if(length(input$SARIMA_MaxFourierPairs) > 0L) input$SARIMA_MaxFourierPairs else 0L,
      TrainWeighting_Selected = if(length(input$SARIMA_TrainWeighting) > 0L) input$SARIMA_TrainWeighting else 0.50,
      MaxConsecutiveFails_Selected = if(length(input$SARIMA_MaxConsecutiveFails) > 0L) input$SARIMA_MaxConsecutiveFails else 20,
      MaxNumberModels_Selected = if(length(input$SARIMA_MaxNumberModels) > 0L) input$SARIMA_MaxNumberModels else 30,
      MaxRunTimeMinutes_Selected = if(length(input$SARIMA_MaxRunTimeMinutes) > 0L) input$SARIMA_MaxRunTimeMinutes else 10,
      DebugMode_Selected = Debug)

    # Reactives
    SARIMA_DataReactive <- shiny::reactive({tryCatch({DataList[[input$SARIMA_Data]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(SARIMA_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(SARIMA_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(SARIMA_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = "SARIMA_TargetColumnName", Label = "Target Variable", Choices = ChoiceList, SelectedDefault = if(length(input$SARIMA_TargetColumnName) > 0L) input$SARIMA_TargetColumnName else NULL, Multiple = TRUE, MaxVars = 1)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = "SARIMA_DateColumnName", Label = "Date Variable", Choices = ChoiceList, SelectedDefault = if(length(input$SARIMA_DateColumnName) > 0L) input$SARIMA_DateColumnName else NULL, Multiple = TRUE, MaxVars = 1)
    }, ignoreInit = FALSE)

  })
  shiny::observeEvent(input$SARIMA_OK, {shiny::removeModal()})
  shiny::observeEvent(input$SARIMA_RefreshInputs, {shinyjs::click(id = 'SARIMA_Modal', asis = TRUE)})

  # ETS DropDown
  shiny::observeEvent(input$ETS_Modal, {
    Quantico:::ETS_Modal_Fun(
      id = "ETS_Modal_ID",
      AppWidth=12L,

      # Reactives
      TargetColumnName_Selected = if(length(input$ETS_TargetColumnName) > 0L) input$ETS_TargetColumnName else NULL,
      DateColumnName_Selected = if(length(input$ETS_DateColumnName) > 0L) input$ETS_DateColumnName else NULL,

      # Statics
      RunMode_Choices = c("Grid Tune", "Forecast"),
      ArgsList_Choices = tryCatch({names(ModelOutputList)}, error = function(x) NULL),
      data_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      TimeUnit_Choices = c("1Min", "5Min", "10Min", "15Min", "30Min", "hour", "day", "week", "month", "quarter", "year"),
      FCPeriods_Choices = 1L:1000L,
      EvaluationMetric_Choices = c("MSE", "MAE", "MAPE"),
      TrainWeighting_Choices = seq(0.0,1,0.01),

      # Updaters
      ModelID_Selected = if(length(input$ETS_ModelID) > 0L) input$ETS_ModelID else "ETS_01",
      RunMode_Selected = if(length(input$ETS_RunMode) > 0L) input$ETS_RunMode else "Grid Tune",
      ArgsList_Selected = if(length(input$ETS_ArgsList) > 0L) input$ETS_ArgsList else NULL,
      data_Selected = if(length(input$ETS_Data) > 0L) input$ETS_Data else NULL,
      TimeUnit_Selected = if(length(input$ETS_TimeUnit) > 0L) input$ETS_TimeUnit else "day",
      FCPeriods_Selected = if(length(input$ETS_FCPeriods) > 0L) input$ETS_FCPeriods else 5,
      EvaluationMetric_Selected = if(length(input$ETS_EvaluationMetric) > 0L) input$ETS_EvaluationMetric else "MSE",
      TrainWeighting_Selected = if(length(input$ETS_TrainWeighting) > 0L) input$ETS_TrainWeighting else 0.50,
      DebugMode_Selected = Debug)

    # Reactives
    ETS_DataReactive <- shiny::reactive({tryCatch({DataList[[input$ETS_Data]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(ETS_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(ETS_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(ETS_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = "ETS_TargetColumnName", Label = "Target Variable", Choices = ChoiceList, SelectedDefault = if(length(input$ETS_TargetColumnName) > 0L) input$ETS_TargetColumnName else NULL, Multiple = TRUE, MaxVars = 1)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = "ETS_DateColumnName", Label = "Date Variable", Choices = ChoiceList, SelectedDefault = if(length(input$ETS_DateColumnName) > 0L) input$ETS_DateColumnName else NULL, Multiple = TRUE, MaxVars = 1)
    }, ignoreInit = FALSE)

  })
  shiny::observeEvent(input$ETS_OK, {shiny::removeModal()})
  shiny::observeEvent(input$ETS_RefreshInputs, {shinyjs::click(id = 'ETS_Modal', asis = TRUE)})

  # ARFIMA DropDown
  shiny::observeEvent(input$ARFIMA_Modal, {
    Quantico:::ARFIMA_Modal_Fun(
      id = "ARFIMA_Modal_ID",
      AppWidth=12L,

      # Reactives
      TargetColumnName_Selected = if(length(input$ARFIMA_TargetColumnName) > 0L) input$ARFIMA_TargetColumnName else NULL,
      DateColumnName_Selected = if(length(input$ARFIMA_DateColumnName) > 0L) input$ARFIMA_DateColumnName else NULL,

      # Statics
      RunMode_Choices = c("Grid Tune", "Forecast"),
      ArgsList_Choices = tryCatch({names(ModelOutputList)}, error = function(x) NULL),
      data_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      TimeUnit_Choices = c("1Min", "5Min", "10Min", "15Min", "30Min", "hour", "day", "week", "month", "quarter", "year"),
      FCPeriods_Choices = 1L:1000L,
      EvaluationMetric_Choices = c("MSE", "MAE", "MAPE"),
      TrainWeighting_Choices = seq(0.0,1,0.01),

      # Updaters
      ModelID_Selected = if(length(input$ARFIMA_ModelID) > 0L) input$ARFIMA_ModelID else "ARFIMA_01",
      RunMode_Selected = if(length(input$ARFIMA_RunMode) > 0L) input$ARFIMA_RunMode else "Grid Tune",
      ArgsList_Selected = if(length(input$ARFIMA_ArgsList) > 0L) input$ARFIMA_ArgsList else NULL,
      data_Selected = if(length(input$ARFIMA_Data) > 0L) input$ARFIMA_Data else NULL,
      TimeUnit_Selected = if(length(input$ARFIMA_TimeUnit) > 0L) input$ARFIMA_TimeUnit else "day",
      FCPeriods_Selected = if(length(input$ARFIMA_FCPeriods) > 0L) input$ARFIMA_FCPeriods else 5,
      EvaluationMetric_Selected = if(length(input$ARFIMA_EvaluationMetric) > 0L) input$ARFIMA_EvaluationMetric else "MSE",
      MaxLags_Selected = if(length(input$ARFIMA_MaxLags) > 0L) input$ARFIMA_MaxLags else 0,
      MaxMovingAverages_Selected = if(length(input$ARFIMA_MaxMovingAverages) > 0L) input$ARFIMA_MaxMovingAverages else 0,
      TrainWeighting_Selected = if(length(input$ARFIMA_TrainWeighting) > 0L) input$ARFIMA_TrainWeighting else 0.50,
      DebugMode_Selected = Debug)

    # Reactives
    ARFIMA_DataReactive <- shiny::reactive({tryCatch({DataList[[input$ARFIMA_Data]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(ARFIMA_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(ARFIMA_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(ARFIMA_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = "ARFIMA_TargetColumnName", Label = "Target Variable", Choices = ChoiceList, SelectedDefault = if(length(input$ARFIMA_TargetColumnName) > 0L) input$ARFIMA_TargetColumnName else NULL, Multiple = TRUE, MaxVars = 1)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = "ARFIMA_DateColumnName", Label = "Date Variable", Choices = ChoiceList, SelectedDefault = if(length(input$ARFIMA_DateColumnName) > 0L) input$ARFIMA_DateColumnName else NULL, Multiple = TRUE, MaxVars = 1)
    }, ignoreInit = FALSE)

  })
  shiny::observeEvent(input$ARFIMA_OK, {shiny::removeModal()})
  shiny::observeEvent(input$ARFIMA_RefreshInputs, {shinyjs::click(id = 'ARFIMA_Modal', asis = TRUE)})

  # NNetar DropDown
  shiny::observeEvent(input$NNET_Modal, {
    Quantico:::NNET_Modal_Fun(
      id = "NNET_Modal_ID",
      AppWidth=12L,

      # Reactives
      TargetColumnName_Selected = if(length(input$NNET_TargetColumnName) > 0L) input$NNET_TargetColumnName else NULL,
      DateColumnName_Selected = if(length(input$NNET_DateColumnName) > 0L) input$NNET_DateColumnName else NULL,

      # Statics
      RunMode_Choices = c("Grid Tune", "Forecast"),
      ArgsList_Choices = tryCatch({names(ModelOutputList)}, error = function(x) NULL),
      data_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      TimeUnit_Choices = c("1Min", "5Min", "10Min", "15Min", "30Min", "hour", "day", "week", "month", "quarter", "year"),
      FCPeriods_Choices = 1L:1000L,
      EvaluationMetric_Choices = c("MSE", "MAE", "MAPE"),
      MaxLags_Choices = 0L:500L,
      MaxSeasonalLags_Choices = 0L:10L,
      MaxFourierPairs_Choices = 0L:2L,
      TrainWeighting_Choices = seq(0.0,1,0.01),
      MaxConsecutiveFails_Choices = 1L:250L,
      MaxNumberModels_Choices = seq(10L,1000L,10L),
      MaxRunTimeMinutes_Choices = 1L:360L,

      # Updaters
      ModelID_Selected = if(length(input$NNET_ModelID) > 0L) input$NNET_ModelID else "NNET_01",
      RunMode_Selected = if(length(input$NNET_RunMode) > 0L) input$NNET_RunMode else "Grid Tune",
      ArgsList_Selected = if(length(input$NNET_ArgsList) > 0L) input$NNET_ArgsList else NULL,
      data_Selected = if(length(input$NNET_Data) > 0L) input$NNET_Data else NULL,
      TimeUnit_Selected = if(length(input$NNET_TimeUnit) > 0L) input$NNET_TimeUnit else "day",
      FCPeriods_Selected = if(length(input$NNET_FCPeriods) > 0L) input$NNET_FCPeriods else 5,
      EvaluationMetric_Selected = if(length(input$NNET_EvaluationMetric) > 0L) input$NNET_EvaluationMetric else "MSE",
      MaxLags_Selected = if(length(input$NNET_MaxLags) > 0L) input$NNET_MaxLags else 0,
      MaxSeasonalLags_Selected = if(length(input$NNET_MaxSeasonalLags) > 0L) input$NNET_MaxSeasonalLags else 0,
      MaxFourierPairs_Selected = if(length(input$NNET_MaxFourierPairs) > 0L) input$NNET_MaxFourierPairs else 0L,
      TrainWeighting_Selected = if(length(input$NNET_TrainWeighting) > 0L) input$NNET_TrainWeighting else 0.50,
      MaxConsecutiveFails_Selected = if(length(input$NNET_MaxConsecutiveFails) > 0L) input$NNET_MaxConsecutiveFails else 20,
      MaxNumberModels_Selected = if(length(input$NNET_MaxNumberModels) > 0L) input$NNET_MaxNumberModels else 30,
      MaxRunTimeMinutes_Selected = if(length(input$NNET_MaxRunTimeMinutes) > 0L) input$NNET_MaxRunTimeMinutes else 10,
      DebugMode_Selected = Debug)

    # Reactives
    NNET_DataReactive <- shiny::reactive({tryCatch({DataList[[input$NNET_Data]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(NNET_DataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(NNET_DataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(NNET_DataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = "NNET_TargetColumnName", Label = "Target Variable", Choices = ChoiceList, SelectedDefault = if(length(input$NNET_TargetColumnName) > 0L) input$NNET_TargetColumnName else NULL, Multiple = TRUE, MaxVars = 1)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = "NNET_DateColumnName", Label = "Date Variable", Choices = ChoiceList, SelectedDefault = if(length(input$NNET_DateColumnName) > 0L) input$NNET_DateColumnName else NULL, Multiple = TRUE, MaxVars = 1)
    }, ignoreInit = FALSE)

  })
  shiny::observeEvent(input$NNET_OK, {shiny::removeModal()})
  shiny::observeEvent(input$NNET_RefreshInputs, {shinyjs::click(id = 'NNET_Modal', asis = TRUE)})

  # CatBoost CARMA DropDown
  shiny::observeEvent(input$CatBoostCARMA_Modal, {
    Quantico:::CatBoostCARMA_Modal_Fun(
      id = 'CatBoostCARMAMLID',

      # Reactives
      CatBoostCARMA_TargetColumnName_Selected = if(length(input$CatBoostCARMA_TargetColumnName) > 0L) input$CatBoostCARMA_TargetColumnName else NULL,
      CatBoostCARMA_DateColumnName_Selected = if(length(input$CatBoostCARMA_DateColumnName) > 0L) input$CatBoostCARMA_DateColumnName else NULL,
      CatBoostCARMA_GroupVariables_Selected = if(length(input$CatBoostCARMA_GroupVariables) > 0L) input$CatBoostCARMA_GroupVariables else NULL,

      # Statics
      CatBoostCARMA_TrainData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      CatBoostCARMA_ValidationData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      CatBoostCARMA_XREGS_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      CatBoostCARMA_RunMode_Choices = c('Train New Model','Retrain Existing Model','Backtest','Feature Engineering Test','Backtest Cross Eval','Forecast','Forecast+Retrain'),
      CatBoostCARMA_ArgsList_Choices = tryCatch({names(ModelOutputList)}, error = function(x) NULL),
      CatBoostCARMA_TimeUnit_Choices = c("1-Minute","5-Minutes","10-Minutes","15-Minutes","30-Minutes","Hourly","Daily","Weekly","Monthly","Quarterly","Yearly"),
      CatBoostCARMA_FCPeriods_Choices = c(1L:60L, seq(65L,1000L,5L)),
      CatBoostCARMA_EncodingMethod_Choices = c('meow','credibility','target_encoding','binary','woe','poly_encode','backward_difference','helmert'),
      CatBoostCARMA_ZeroPadSeries_Choices = c('dynamic:meow','dynamic:target_encoding','dynamic:credibility','maxmax','maxmin','minmax','minmin'),
      CatBoostCARMA_Methods_Choices = c("Standardize", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit"),
      CatBoostCARMA_Difference_Choices = c(FALSE,TRUE),
      CatBoostCARMA_TimeWeights_Choices = c(1,0.9999,0.9995,0.999,0.995,0.99,0.975,0.95),
      CatBoostCARMA_NonNegativePred_Choices = c(TRUE,FALSE),
      CatBoostCARMA_RoundPreds_Choices = c(TRUE,FALSE),
      CatBoostCARMA_TaskType_Choices = c("GPU","CPU"),
      CatBoostCARMA_NumGPU_Choices = c(1,2,3,4,5,6,7,8),
      CatBoostCARMA_SaveModel_Choices = c(FALSE,TRUE),
      CatBoostCARMA_CalendarVariables_Choices = list("Minute of Hour" = "minute","Hour of Day" = "hour","Day of Week" = "wday", "Day of Month" = "mday","Day of Year" = "yday","Week of Year" = "week", "Isoweek of Year" = "isoweek", "Week of Month" = "wom", "Month of Year" = "month", "Quarter of Year" = "quarter"),
      CatBoostCARMA_HolidayVariables_Choices = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
      CatBoostCARMA_Lookback_Choices = 1L:60L,
      CatBoostCARMA_TimeTrend_Choices = c(FALSE,TRUE),
      CatBoostCARMA_AnomalyDetection_HighThreshold_Choices = c(0,2,3,4,5,6,7,8,9,10),
      CatBoostCARMA_AnomalyDetection_LowThreshold_Choices = c(0,-2,-3,-4,-5,-6,-7,-8,-9,-10),
      CatBoostCARMA_Lags_Choices = c(1L:60L, seq(65L,1000L,5L)),
      CatBoostCARMA_MovingAverages_Choices = c(2L:60L, seq(65L,1000L,5L)),
      CatBoostCARMA_MovingSD_Choices = c(2L:60L, seq(65L,1000L,5L)),
      CatBoostCARMA_MovingSkew_Choices = c(3L:60L, seq(65L,1000L,5L)),
      CatBoostCARMA_MovingKurt_Choices = c(4L:60L, seq(65L,1000L,5L)),
      CatBoostCARMA_MovingQuantiles_Choices = c(5L:60L, seq(65L,1000L,5L)),
      CatBoostCARMA_Quantiles_Selected_Choices = c("q5","q10","q15","q20","q25","q30","q35","q40","q45","q50","q55","q60","q65","q70","q75","q80","q85","q90","q95"),
      CatBoostCARMA_DataTruncate_Choices = c(FALSE,TRUE),
      CatBoostCARMA_NTrees_Choices = seq(50,5000,50),
      CatBoostCARMA_Depth_Choices = 2L:16L,
      CatBoostCARMA_Langevin_Choices = c(TRUE,FALSE),
      CatBoostCARMA_DiffusionTemperature_Choices = seq(2500,25000,2500),
      CatBoostCARMA_SubSample_Choices = seq(0.50,1.0,0.01),
      CatBoostCARMA_BootStrapType_Choices = c("Bernoulli","MVS","Bayesian","Poisson","No"),
      CatBoostCARMA_L2_Leaf_Reg_Choices = seq(0,20,0.25),
      CatBoostCARMA_LearningRate_Choices = seq(0.0,0.30,0.01),
      CatBoostCARMA_ModelSizeReg_Choices = seq(0.0,5.0,0.05),
      CatBoostCARMA_MinDataInLeaf_Choices = 1L:100L,
      CatBoostCARMA_RandomStrength_Choices = seq(0.05,1.0,0.05),
      CatBoostCARMA_RSM_Choices = seq(0.05,1.0,0.05),
      CatBoostCARMA_BorderCount_Choices = seq(32,256,32),
      CatBoostCARMA_GrowPolicy_Choices = c("SymmetricTree","Depthwise","Lossguide"),
      CatBoostCARMA_FeatureBorderType_Choices = c("GreedyLogSum","Median","Uniform","UniformAndQuantiles","MaxLogSum","MinEntropy"),
      CatBoostCARMA_ScoreFunction_Choices = c("Cosine","L2","NewtonL2","NewtonCosine"),
      CatBoostCARMA_CrossEvalModelUpdate_Choices = c(TRUE,FALSE),
      CatBoostCARMA_CrossEvalValuesUpdate_Choices = c(TRUE,FALSE),
      CatBoostCARMA_CrossEvalModelUpdateFreq_Choices = 1L:365L,
      CatBoostCARMA_CrossEvalValuesUpdateFreq_Choices = 1L:365L,
      CatBoostCARMA_EvalMetric_Choices = c("RMSE","MAE","MAPE","Poisson","Quantile","LogLinQuantile","Lq","NumErrors","SMAPE","R2","MSLE","MedianAbsoluteError"),
      CatBoostCARMA_EvalMetricValue_Choices = seq(1,2,0.01),
      CatBoostCARMA_LossFunction_Choices = c('RMSE','MAE','Quantile','LogLinQuantile','MAPE','Poisson','PairLogitPairwise','Tweedie','QueryRMSE'),
      CatBoostCARMA_LossFunctionValue_Choices = seq(1,2,0.01),

      # Updaters
      CatBoostCARMA_ModelID_Selected = if(length(input$CatBoostCARMA_ModelID) > 0L) input$CatBoostCARMA_ModelID else "FC1",
      CatBoostCARMA_TrainData_Selected = if(length(input$CatBoostCARMA_TrainData) > 0L) input$CatBoostCARMA_TrainData else NULL,
      CatBoostCARMA_ValidationData_Selected = if(length(input$CatBoostCARMA_ValidationData) > 0L) input$CatBoostCARMA_ValidationData else NULL,
      CatBoostCARMA_XREGS_Selected = if(length(input$CatBoostCARMA_XREGS) > 0L) input$CatBoostCARMA_XREGS else NULL,
      CatBoostCARMA_RunMode_Selected = if(length(input$CatBoostCARMA_RunMode) > 0L) input$CatBoostCARMA_RunMode else 'Train New Model',
      CatBoostCARMA_ArgsList_Selected = if(length(input$CatBoostCARMA_ArgsList) > 0L) input$CatBoostCARMA_ArgsList else NULL,
      CatBoostCARMA_TimeUnit_Selected = if(length(input$CatBoostCARMA_TimeUnit) > 0L) input$CatBoostCARMA_TimeUnit else "Daily",
      CatBoostCARMA_FCPeriods_Selected = if(length(input$CatBoostCARMA_FCPeriods) > 0L) input$CatBoostCARMA_FCPeriods else 5L,
      CatBoostCARMA_EncodingMethod_Selected = if(length(input$CatBoostCARMA_EncodingMethod) > 0L) input$CatBoostCARMA_EncodingMethod else 'credibility',
      CatBoostCARMA_ZeroPadSeries_Selected = if(length(input$CatBoostCARMA_ZeroPadSeries) > 0L) input$CatBoostCARMA_ZeroPadSeries else 'maxmax',
      CatBoostCARMA_Methods_Selected = if(length(input$CatBoostCARMA_Methods) > 0L) input$CatBoostCARMA_Methods else NULL,
      CatBoostCARMA_Difference_Selected = if(length(input$CatBoostCARMA_Difference) > 0L) input$CatBoostCARMA_Difference else FALSE,
      CatBoostCARMA_TimeWeights_Selected = if(length(input$CatBoostCARMA_TimeWeights) > 0L) input$CatBoostCARMA_TimeWeights else 1,
      CatBoostCARMA_NonNegativePred_Selected = if(length(input$CatBoostCARMA_NonNegativePred) > 0L) input$CatBoostCARMA_NonNegativePred else FALSE,
      CatBoostCARMA_RoundPreds_Selected = if(length(input$CatBoostCARMA_RoundPreds) > 0L) input$CatBoostCARMA_RoundPreds else FALSE,
      CatBoostCARMA_TaskType_Selected = if(length(input$CatBoostCARMA_TaskType) > 0L) input$CatBoostCARMA_TaskType else "CPU",
      CatBoostCARMA_NumGPU_Selected = if(length(input$CatBoostCARMA_NumGPU) > 0L) input$CatBoostCARMA_NumGPU else 1,
      CatBoostCARMA_SaveModel_Selected = if(length(input$CatBoostCARMA_SaveModel) > 0L) input$CatBoostCARMA_SaveModel else FALSE,
      CatBoostCARMA_CalendarVariables_Selected = if(length(input$CatBoostCARMA_CalendarVariables) > 0L) input$CatBoostCARMA_CalendarVariables else NULL,
      CatBoostCARMA_HolidayVariables_Selected = if(length(input$CatBoostCARMA_HolidayVariables) > 0L) input$CatBoostCARMA_HolidayVariables else NULL,
      CatBoostCARMA_Lookback_Selected = if(length(input$CatBoostCARMA_Lookback) > 0L) input$CatBoostCARMA_Lookback else NULL,
      CatBoostCARMA_TimeTrend_Selected = if(length(input$CatBoostCARMA_TimeTrend) > 0L) input$CatBoostCARMA_TimeTrend else FALSE,
      CatBoostCARMA_AnomalyDetection_HighThreshold_Selected = if(length(input$CatBoostCARMA_AnomalyDetection_HighThreshold) > 0L) input$CatBoostCARMA_AnomalyDetection_HighThreshold else NULL,
      CatBoostCARMA_AnomalyDetection_LowThreshold_Selected = if(length(input$CatBoostCARMA_AnomalyDetection_LowThreshold) > 0L) input$CatBoostCARMA_AnomalyDetection_LowThreshold else NULL,
      CatBoostCARMA_Lags_Selected = if(length(input$CatBoostCARMA_Lags) > 0L) input$CatBoostCARMA_Lags else NULL,
      CatBoostCARMA_MovingAverages_Selected = if(length(input$CatBoostCARMA_MovingAverages) > 0L) input$CatBoostCARMA_MovingAverages else NULL,
      CatBoostCARMA_MovingSD_Selected = if(length(input$CatBoostCARMA_MovingSD) > 0L) input$CatBoostCARMA_MovingSD else NULL,
      CatBoostCARMA_MovingSkew_Selected = if(length(input$CatBoostCARMA_MovingSkew) > 0L) input$CatBoostCARMA_MovingSkew else NULL,
      CatBoostCARMA_MovingKurt_Selected = if(length(input$CatBoostCARMA_MovingKurt) > 0L) input$CatBoostCARMA_MovingKurt else NULL,
      CatBoostCARMA_MovingQuantiles_Selected = if(length(input$CatBoostCARMA_MovingQuantiles) > 0L) input$CatBoostCARMA_MovingQuantiles else NULL,
      CatBoostCARMA_Quantiles_Selected_Selected = if(length(input$CatBoostCARMA_Quantiles_Selected) > 0L) input$CatBoostCARMA_Quantiles_Selected else NULL,
      CatBoostCARMA_DataTruncate_Selected = if(length(input$CatBoostCARMA_DataTruncate) > 0L) input$CatBoostCARMA_DataTruncate else FALSE,
      CatBoostCARMA_NTrees_Selected = if(length(input$CatBoostCARMA_NTrees) > 0L) input$CatBoostCARMA_NTrees else 500,
      CatBoostCARMA_Depth_Selected = if(length(input$CatBoostCARMA_Depth) > 0L) input$CatBoostCARMA_Depth else 6L,
      CatBoostCARMA_Langevin_Selected = if(length(input$CatBoostCARMA_Langevin) > 0L) input$CatBoostCARMA_Langevin else TRUE,
      CatBoostCARMA_DiffusionTemperature_Selected = if(length(input$CatBoostCARMA_DiffusionTemperature) > 0L) input$CatBoostCARMA_DiffusionTemperature else 10000L,
      CatBoostCARMA_SubSample_Selected = if(length(input$CatBoostCARMA_SubSample) > 0L) input$CatBoostCARMA_SubSample else 1.0,
      CatBoostCARMA_BootStrapType_Selected = if(length(input$CatBoostCARMA_BootStrapType) > 0L) input$CatBoostCARMA_BootStrapType else "No",
      CatBoostCARMA_L2_Leaf_Reg_Selected = if(length(input$CatBoostCARMA_L2_Leaf_Reg) > 0L) input$CatBoostCARMA_L2_Leaf_Reg else NULL,
      CatBoostCARMA_LearningRate_Selected = if(length(input$CatBoostCARMA_LearningRate) > 0L) input$CatBoostCARMA_LearningRate else NULL,
      CatBoostCARMA_ModelSizeReg_Selected = if(length(input$CatBoostCARMA_ModelSizeReg) > 0L) input$CatBoostCARMA_ModelSizeReg else 0.5,
      CatBoostCARMA_MinDataInLeaf_Selected = if(length(input$CatBoostCARMA_MinDataInLeaf) > 0L) input$CatBoostCARMA_MinDataInLeaf else 1L,
      CatBoostCARMA_RandomStrength_Selected = if(length(input$CatBoostCARMA_RandomStrength) > 0L) input$CatBoostCARMA_RandomStrength else 1.0,
      CatBoostCARMA_RSM_Selected = if(length(input$CatBoostCARMA_RSM) > 0L) input$CatBoostCARMA_RSM else 1.0,
      CatBoostCARMA_BorderCount_Selected = if(length(input$CatBoostCARMA_BorderCount) > 0L) input$CatBoostCARMA_BorderCount else 256,
      CatBoostCARMA_GrowPolicy_Selected = if(length(input$CatBoostCARMA_GrowPolicy) > 0L) input$CatBoostCARMA_GrowPolicy else "SymmetricTree",
      CatBoostCARMA_FeatureBorderType_Selected = if(length(input$CatBoostCARMA_FeatureBorderType) > 0L) input$CatBoostCARMA_FeatureBorderType else "GreedyLogSum",
      CatBoostCARMA_ScoreFunction_Selected = if(length(input$CatBoostCARMA_ScoreFunction) > 0L) input$CatBoostCARMA_ScoreFunction else "Cosine",
      CatBoostCARMA_CrossEvalModelUpdate_Selected = if(length(input$CatBoostCARMA_CrossEvalModelUpdate) > 0L) input$CatBoostCARMA_CrossEvalModelUpdate else FALSE,
      CatBoostCARMA_CrossEvalValuesUpdate_Selected = if(length(input$CatBoostCARMA_CrossEvalValuesUpdate) > 0L) input$CatBoostCARMA_CrossEvalValuesUpdate else FALSE,
      CatBoostCARMA_CrossEvalModelUpdateFreq_Selected = if(length(input$CatBoostCARMA_CrossEvalModelUpdateFreq) > 0L) input$CatBoostCARMA_CrossEvalModelUpdateFreq else 1L,
      CatBoostCARMA_CrossEvalValuesUpdateFreq_Selected = if(length(input$CatBoostCARMA_CrossEvalValuesUpdateFreq) > 0L) input$CatBoostCARMA_CrossEvalValuesUpdateFreq else 1L,
      CatBoostCARMA_EvalMetric_Selected = if(length(input$CatBoostCARMA_EvalMetric) > 0L) input$CatBoostCARMA_EvalMetric else "RMSE",
      CatBoostCARMA_EvalMetricValue_Selected = if(length(input$CatBoostCARMA_EvalMetricValue) > 0L) input$CatBoostCARMA_EvalMetricValue else 1,
      CatBoostCARMA_LossFunction_Selected = if(length(input$CatBoostCARMA_LossFunction) > 0L) input$CatBoostCARMA_LossFunction else 'RMSE',
      CatBoostCARMA_LossFunctionValue_Selected = if(length(input$CatBoostCARMA_LossFunctionValue) > 0L) input$CatBoostCARMA_LossFunctionValue else 1)

    # Reactives
    CatBoostCARMA_TrainDataReactive <- shiny::reactive({tryCatch({DataList[[input$CatBoostCARMA_TrainData]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(CatBoostCARMA_TrainDataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(CatBoostCARMA_TrainDataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(CatBoostCARMA_TrainDataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = "CatBoostCARMA_TargetColumnName", Label = "Target Variable", Choices = ChoiceList, SelectedDefault = if(length(input$CatBoostCARMA_TargetColumnName) > 0L) input$CatBoostCARMA_TargetColumnName else NULL, Multiple = TRUE, MaxVars = 1)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = "CatBoostCARMA_DateColumnName", Label = "Date Variable", Choices = ChoiceList, SelectedDefault = if(length(input$CatBoostCARMA_DateColumnName) > 0L) input$CatBoostCARMA_DateColumnName else NULL, Multiple = TRUE, MaxVars = 1)
      Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = "CatBoostCARMA_GroupVariables", Label = "Group Variables", Choices = ChoiceList, SelectedDefault = if(length(input$CatBoostCARMA_GroupVariables) > 0L) input$CatBoostCARMA_GroupVariables else NULL, Multiple = TRUE)
    }, ignoreInit = FALSE)

  })
  shiny::observeEvent(input$CatBoostCARMA_OK, {shiny::removeModal()})
  shiny::observeEvent(input$CatBoostCARMA_RefreshInputs, {shinyjs::click(id = 'CatBoostCARMA_Modal', asis = TRUE)})

  # XGBoost CARMA DropDown
  shiny::observeEvent(input$XGBoostCARMA_Modal, {
    Quantico:::XGBoostCARMA_Modal_Fun(
      id = 'XGBoostCARMAMLID',

      # Reactives
      XGBoostCARMA_TargetColumnName_Selected = if(length(input$XGBoostCARMA_TargetColumnName) > 0L) input$XGBoostCARMA_TargetColumnName else NULL,
      XGBoostCARMA_DateColumnName_Selected = if(length(input$XGBoostCARMA_DateColumnName) > 0L) input$XGBoostCARMA_DateColumnName else NULL,
      XGBoostCARMA_GroupVariables_Selected = if(length(input$XGBoostCARMA_GroupVariables) > 0L) input$XGBoostCARMA_GroupVariables else NULL,

      # Statics
      XGBoostCARMA_TrainData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      XGBoostCARMA_ValidationData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      XGBoostCARMA_XREGS_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      XGBoostCARMA_RunMode_Choices = c('Train New Model','Retrain Existing Model','Backtest','Feature Engineering Test','Backtest Cross Eval','Forecast','Forecast+Retrain'),
      XGBoostCARMA_TimeUnit_Choices = c("1-Minute","5-Minutes","10-Minutes","15-Minutes","30-Minutes","Hourly","Daily","Weekly","Monthly","Quarterly","Yearly"),
      XGBoostCARMA_FCPeriods_Choices = c(1L:60L, seq(65L,1000L,5L)),
      XGBoostCARMA_TaskType_Choices = c("GPU","CPU"),
      XGBoostCARMA_NumGPU_Choices = c(1,2,3,4,5,6,7,8),
      XGBoostCARMA_SaveModel_Choices = c(FALSE,TRUE),
      XGBoostCARMA_ArgsList_Choices = tryCatch({names(ModelOutputList)}, error = function(x) NULL),
      XGBoostCARMA_TimeWeights_Choices = c(1,0.9999,0.9995,0.999,0.995,0.99,0.975,0.95),
      XGBoostCARMA_Methods_Choices = c("Standardize", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit"),
      XGBoostCARMA_Difference_Choices = c(FALSE,TRUE),
      XGBoostCARMA_NonNegativePred_Choices = c(TRUE,FALSE),
      XGBoostCARMA_RoundPreds_Choices = c(TRUE,FALSE),
      XGBoostCARMA_CalendarVariables_Choices = c("minute","hour","wday","mday","yday","week","isoweek","wom","month","quarter"),
      XGBoostCARMA_HolidayVariables_Choices = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
      XGBoostCARMA_Lookback_Choices = 1L:60L,
      XGBoostCARMA_Lags_Choices = c(1L:60L, seq(65L,1000L,5L)),
      XGBoostCARMA_MovingAverages_Choices = c(2L:60L, seq(65L,1000L,5L)),
      XGBoostCARMA_MovingSD_Choices = c(2L:60L, seq(65L,1000L,5L)),
      XGBoostCARMA_MovingSkew_Choices = c(3L:60L, seq(65L,1000L,5L)),
      XGBoostCARMA_MovingKurt_Choices = c(4L:60L, seq(65L,1000L,5L)),
      XGBoostCARMA_MovingQuantiles_Choices = c(5L:60L, seq(65L,1000L,5L)),
      XGBoostCARMA_Quantiles_Selected_Choices = c("q5","q10","q15","q20","q25","q30","q35","q40","q45","q50","q55","q60","q65","q70","q75","q80","q85","q90","q95"),
      XGBoostCARMA_AnomalyDetection_HighThreshold_Choices = c(0,2,3,4,5,6,7,8,9,10),
      XGBoostCARMA_AnomalyDetection_LowThreshold_Choices = c(0,-2,-3,-4,-5,-6,-7,-8,-9,-10),
      XGBoostCARMA_TimeTrend_Choices = c(FALSE,TRUE),
      XGBoostCARMA_DataTruncate_Choices = c(FALSE,TRUE),
      XGBoostCARMA_EncodingMethod_Choices = c('meow','credibility','target_encoding','binary','woe','poly_encode','backward_difference','helmert'),
      XGBoostCARMA_ZeroPadSeries_Choices = c('dynamic:meow','dynamic:target_encoding','dynamic:credibility','maxmax','maxmin','minmax','minmin'),
      XGBoostCARMA_CrossEvalModelUpdate_Choices = c(TRUE,FALSE),
      XGBoostCARMA_CrossEvalValuesUpdate_Choices = c(TRUE,FALSE),
      XGBoostCARMA_CrossEvalModelUpdateFreq_Choices = 1L:365L,
      XGBoostCARMA_CrossEvalValuesUpdateFreq_Choices = 1L:365L,
      XGBoostCARMA_EvalMetric_Choices = c("RMSE","MAE","R2","MAPE"),
      XGBoostCARMA_LossFunction_Choices = c('reg:squarederror','reg:squaredlogerror','reg:pseudohubererror','count:poisson','reg:gamma','reg:tweedie'),
      XGBoostCARMA_NTrees_Choices = seq(50,5000,50),
      XGBoostCARMA_MaxDepth_Choices = 2L:16L,
      XGBoostCARMA_LearningRate_Choices = seq(0.01,0.99,0.01),
      XGBoostCARMA_MinChildWeight_Choices = 1L:100L,
      XGBoostCARMA_SubSample_Choices = seq(0.01,1.0,0.01),
      XGBoostCARMA_ColSampleByTree_Choices = seq(0.01,1.0,0.01),
      XGBoostCARMA_lambda_Choices = seq(1.0,50.0,0.5),
      XGBoostCARMA_alpha_Choices = seq(1.0,50.0,0.5),

      # Updaters
      XGBoostCARMA_ModelID_Selected = if(length(input$XGBoostCARMA_ModelID) > 0L) input$XGBoostCARMA_ModelID else "FC1",
      XGBoostCARMA_TrainData_Selected = if(length(input$XGBoostCARMA_TrainData) > 0L) input$XGBoostCARMA_TrainData else NULL,
      XGBoostCARMA_ValidationData_Selected = if(length(input$XGBoostCARMA_ValidationData) > 0L) input$XGBoostCARMA_ValidationData else NULL,
      XGBoostCARMA_XREGS_Selected = if(length(input$XGBoostCARMA_XREGS) > 0L) input$XGBoostCARMA_XREGS else NULL,
      XGBoostCARMA_RunMode_Selected = if(length(input$XGBoostCARMA_RunMode) > 0L) input$XGBoostCARMA_RunMode else 'Train New Model',
      XGBoostCARMA_TimeUnit_Selected = if(length(input$XGBoostCARMA_TimeUnit) > 0L) input$XGBoostCARMA_TimeUnit else "Daily",
      XGBoostCARMA_FCPeriods_Selected = if(length(input$XGBoostCARMA_FCPeriods) > 0L) input$XGBoostCARMA_FCPeriods else 5L,
      XGBoostCARMA_TaskType_Selected = if(length(input$XGBoostCARMA_TaskType) > 0L) input$XGBoostCARMA_TaskType else "CPU",
      XGBoostCARMA_NumGPU_Selected = if(length(input$XGBoostCARMA_NumGPU) > 0L) input$XGBoostCARMA_NumGPU else 1,
      XGBoostCARMA_SaveModel_Selected = if(length(input$XGBoostCARMA_SaveModel) > 0L) input$XGBoostCARMA_SaveModel else FALSE,
      XGBoostCARMA_ArgsList_Selected = if(length(input$XGBoostCARMA_ArgsList) > 0L) input$XGBoostCARMA_ArgsList else tryCatch({names(ModelOutputList)}, error = function(x) NULL),
      XGBoostCARMA_TimeWeights_Selected = if(length(input$XGBoostCARMA_TimeWeights) > 0L) input$XGBoostCARMA_TimeWeights else 1,
      XGBoostCARMA_Methods_Selected = if(length(input$XGBoostCARMA_Methods) > 0L) input$XGBoostCARMA_Methods else NULL,
      XGBoostCARMA_Difference_Selected = if(length(input$XGBoostCARMA_Difference) > 0L) input$XGBoostCARMA_Difference else FALSE,
      XGBoostCARMA_NonNegativePred_Selected = if(length(input$XGBoostCARMA_NonNegativePred) > 0L) input$XGBoostCARMA_NonNegativePred else FALSE,
      XGBoostCARMA_RoundPreds_Selected = if(length(input$XGBoostCARMA_RoundPreds) > 0L) input$XGBoostCARMA_RoundPreds else FALSE,
      XGBoostCARMA_CalendarVariables_Selected = if(length(input$XGBoostCARMA_CalendarVariables) > 0L) input$XGBoostCARMA_CalendarVariables else NULL,
      XGBoostCARMA_HolidayVariables_Selected = if(length(input$XGBoostCARMA_HolidayVariables) > 0L) input$XGBoostCARMA_HolidayVariables else NULL,
      XGBoostCARMA_Lookback_Selected = if(length(input$XGBoostCARMA_Lookback) > 0L) input$XGBoostCARMA_Lookback else NULL,
      XGBoostCARMA_Lags_Selected = if(length(input$XGBoostCARMA_Lags) > 0L) input$XGBoostCARMA_Lags else NULL,
      XGBoostCARMA_MovingAverages_Selected = if(length(input$XGBoostCARMA_MovingAverages) > 0L) input$XGBoostCARMA_MovingAverages else NULL,
      XGBoostCARMA_MovingSD_Selected = if(length(input$XGBoostCARMA_MovingSD) > 0L) input$XGBoostCARMA_MovingSD else NULL,
      XGBoostCARMA_MovingSkew_Selected = if(length(input$XGBoostCARMA_MovingSkew) > 0L) input$XGBoostCARMA_MovingSkew else NULL,
      XGBoostCARMA_MovingKurt_Selected = if(length(input$XGBoostCARMA_MovingKurt) > 0L) input$XGBoostCARMA_MovingKurt else NULL,
      XGBoostCARMA_MovingQuantiles_Selected = if(length(input$XGBoostCARMA_MovingQuantiles) > 0L) input$XGBoostCARMA_MovingQuantiles else NULL,
      XGBoostCARMA_Quantiles_Selected_Selected = if(length(input$XGBoostCARMA_Quantiles_Selected) > 0L) input$XGBoostCARMA_Quantiles_Selected else NULL,
      XGBoostCARMA_AnomalyDetection_HighThreshold_Selected = if(length(input$XGBoostCARMA_AnomalyDetection_HighThreshold) > 0L) input$XGBoostCARMA_AnomalyDetection_HighThreshold else NULL,
      XGBoostCARMA_AnomalyDetection_LowThreshold_Selected = if(length(input$XGBoostCARMA_AnomalyDetection_LowThreshold) > 0L) input$XGBoostCARMA_AnomalyDetection_LowThreshold else NULL,
      XGBoostCARMA_TimeTrend_Selected = if(length(input$XGBoostCARMA_TimeTrend) > 0L) input$XGBoostCARMA_TimeTrend else FALSE,
      XGBoostCARMA_DataTruncate_Selected = if(length(input$XGBoostCARMA_DataTruncate) > 0L) input$XGBoostCARMA_DataTruncate else FALSE,
      XGBoostCARMA_EncodingMethod_Selected = if(length(input$XGBoostCARMA_EncodingMethod) > 0L) input$XGBoostCARMA_EncodingMethod else 'credibility',
      XGBoostCARMA_ZeroPadSeries_Selected = if(length(input$XGBoostCARMA_ZeroPadSeries) > 0L) input$XGBoostCARMA_ZeroPadSeries else 'maxmax',
      XGBoostCARMA_CrossEvalModelUpdate_Selected = if(length(input$XGBoostCARMA_CrossEvalModelUpdate) > 0L) input$XGBoostCARMA_CrossEvalModelUpdate else FALSE,
      XGBoostCARMA_CrossEvalValuesUpdate_Selected = if(length(input$XGBoostCARMA_CrossEvalValuesUpdate) > 0L) input$XGBoostCARMA_CrossEvalValuesUpdate else FALSE,
      XGBoostCARMA_CrossEvalModelUpdateFreq_Selected = if(length(input$XGBoostCARMA_CrossEvalModelUpdateFreq) > 0L) input$XGBoostCARMA_CrossEvalModelUpdateFreq else 1L,
      XGBoostCARMA_CrossEvalValuesUpdateFreq_Selected = if(length(input$XGBoostCARMA_CrossEvalValuesUpdateFreq) > 0L) input$XGBoostCARMA_CrossEvalValuesUpdateFreq else 1L,
      XGBoostCARMA_EvalMetric_Selected = if(length(input$XGBoostCARMA_EvalMetric) > 0L) input$XGBoostCARMA_EvalMetric else "RMSE",
      XGBoostCARMA_LossFunction_Selected = if(length(input$XGBoostCARMA_LossFunction) > 0L) input$XGBoostCARMA_LossFunction else 'reg:squarederror',
      XGBoostCARMA_NTrees_Selected = if(length(input$XGBoostCARMA_NTrees) > 0L) input$XGBoostCARMA_NTrees else 500,
      XGBoostCARMA_MaxDepth_Selected = if(length(input$XGBoostCARMA_MaxDepth) > 0L) input$XGBoostCARMA_MaxDepth else 6L,
      XGBoostCARMA_LearningRate_Selected = if(length(input$XGBoostCARMA_LearningRate) > 0L) input$XGBoostCARMA_LearningRate else 0.10,
      XGBoostCARMA_MinChildWeight_Selected = if(length(input$XGBoostCARMA_MinChildWeight) > 0L) input$XGBoostCARMA_MinChildWeight else 1L,
      XGBoostCARMA_SubSample_Selected = if(length(input$XGBoostCARMA_SubSample) > 0L) input$XGBoostCARMA_SubSample else 1.0,
      XGBoostCARMA_ColSampleByTree_Selected = if(length(input$XGBoostCARMA_ColSampleByTree) > 0L) input$XGBoostCARMA_ColSampleByTree else 1.0,
      XGBoostCARMA_lambda_Selected = if(length(input$XGBoostCARMA_lambda) > 0L) input$XGBoostCARMA_lambda else 4,
      XGBoostCARMA_alpha_Selected = if(length(input$XGBoostCARMA_alpha) > 0L) input$XGBoostCARMA_alpha else 4)

    # Reactives
    XGBoostCARMA_TrainDataReactive <- shiny::reactive({tryCatch({DataList[[input$XGBoostCARMA_TrainData]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(XGBoostCARMA_TrainDataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(XGBoostCARMA_TrainDataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(XGBoostCARMA_TrainDataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = "XGBoostCARMA_TargetColumnName", Label = "Target Variable", Choices = ChoiceList, SelectedDefault = if(length(input$XGBoostCARMA_TargetColumnName) > 0L) input$XGBoostCARMA_TargetColumnName else NULL, Multiple = TRUE, MaxVars = 1)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = "XGBoostCARMA_DateColumnName", Label = "Date Variable", Choices = ChoiceList, SelectedDefault = if(length(input$XGBoostCARMA_DateColumnName) > 0L) input$XGBoostCARMA_DateColumnName else NULL, Multiple = TRUE, MaxVars = 1)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = "XGBoostCARMA_GroupVariables", Label = "Group Variables", Choices = ChoiceList, SelectedDefault = if(length(input$XGBoostCARMA_GroupVariables) > 0L) input$XGBoostCARMA_GroupVariables else NULL, Multiple = TRUE)
    }, ignoreInit = FALSE)
  })
  shiny::observeEvent(input$XGBoostCARMA_OK, {shiny::removeModal()})
  shiny::observeEvent(input$XGBoostCARMA_RefreshInputs, {shinyjs::click(id = 'XGBoostCARMA_Modal', asis = TRUE)})

  # LightGBM CARMA DropDown
  shiny::observeEvent(input$LightGBMCARMA_Modal, {
    Quantico:::LightGBMCARMA_Modal_Fun(
      id = 'LightGBMCARMAMLID',

      # Reactives
      LightGBMCARMA_TargetColumnName_Selected = if(length(input$LightGBMCARMA_TargetColumnName) > 0L) input$LightGBMCARMA_TargetColumnName else NULL,
      LightGBMCARMA_DateColumnName_Selected = if(length(input$LightGBMCARMA_DateColumnName) > 0L) input$LightGBMCARMA_DateColumnName else NULL,
      LightGBMCARMA_GroupVariables_Selected = if(length(input$LightGBMCARMA_GroupVariables) > 0L) input$LightGBMCARMA_GroupVariables else NULL,

      # Statics
      LightGBMCARMA_TrainData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      LightGBMCARMA_ValidationData_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      LightGBMCARMA_XREGS_Choices = tryCatch({names(DataList)}, error = function(x) NULL),
      LightGBMCARMA_RunMode_Choices = c('Train New Model','Retrain Existing Model','Backtest','Feature Engineering Test','Backtest Cross Eval','Forecast','Forecast+Retrain'),
      LightGBMCARMA_TimeUnit_Choices = c("1-Minute","5-Minutes","10-Minutes","15-Minutes","30-Minutes","Hourly","Daily","Weekly","Monthly","Quarterly","Yearly"),
      LightGBMCARMA_FCPeriods_Choices = c(1L:60L, seq(65L,1000L,5L)),
      LightGBMCARMA_TaskType_Choices = c("GPU","CPU"),
      LightGBMCARMA_NumGPU_Choices = c(1,2,3,4,5,6,7,8),
      LightGBMCARMA_SaveModel_Choices = c(FALSE,TRUE),
      LightGBMCARMA_ArgsList_Choices = tryCatch({names(ModelOutputList)}, error = function(x) NULL),
      LightGBMCARMA_TimeWeights_Choices = c(1,0.9999,0.9995,0.999,0.995,0.99,0.975,0.95),
      LightGBMCARMA_Methods_Choices = c("Standardize", "Asinh", "Log", "LogPlus1", "Sqrt", "Asin", "Logit"),
      LightGBMCARMA_Difference_Choices = c(FALSE,TRUE),
      LightGBMCARMA_NonNegativePred_Choices = c(TRUE,FALSE),
      LightGBMCARMA_RoundPreds_Choices = c(TRUE,FALSE),
      LightGBMCARMA_CalendarVariables_Choices = c("minute","hour","wday","mday","yday","week","isoweek","wom","month","quarter"),
      LightGBMCARMA_HolidayVariables_Choices = c("USPublicHolidays","EasterGroup","ChristmasGroup","OtherEcclesticalFeasts"),
      LightGBMCARMA_Lookback_Choices = 1L:60L,
      LightGBMCARMA_Lags_Choices = c(1L:60L, seq(65L,1000L,5L)),
      LightGBMCARMA_MovingAverages_Choices = c(2L:60L, seq(65L,1000L,5L)),
      LightGBMCARMA_MovingSD_Choices = c(2L:60L, seq(65L,1000L,5L)),
      LightGBMCARMA_MovingSkew_Choices = c(3L:60L, seq(65L,1000L,5L)),
      LightGBMCARMA_MovingKurt_Choices = c(4L:60L, seq(65L,1000L,5L)),
      LightGBMCARMA_MovingQuantiles_Choices = c(5L:60L, seq(65L,1000L,5L)),
      LightGBMCARMA_Quantiles_Selected_Choices = c("q5","q10","q15","q20","q25","q30","q35","q40","q45","q50","q55","q60","q65","q70","q75","q80","q85","q90","q95"),
      LightGBMCARMA_AnomalyDetection_HighThreshold_Choices = c(0,2,3,4,5,6,7,8,9,10),
      LightGBMCARMA_AnomalyDetection_LowThreshold_Choices = c(0,-2,-3,-4,-5,-6,-7,-8,-9,-10),
      LightGBMCARMA_TimeTrend_Choices = c(FALSE,TRUE),
      LightGBMCARMA_DataTruncate_Choices = c(FALSE,TRUE),
      LightGBMCARMA_EncodingMethod_Choices = c('meow','credibility','target_encoding','binary','woe','poly_encode','backward_difference','helmert'),
      LightGBMCARMA_ZeroPadSeries_Choices = c('dynamic:meow','dynamic:target_encoding','dynamic:credibility','maxmax','maxmin','minmax','minmin'),
      LightGBMCARMA_CrossEvalModelUpdate_Choices = c(TRUE,FALSE),
      LightGBMCARMA_CrossEvalValuesUpdate_Choices = c(TRUE,FALSE),
      LightGBMCARMA_CrossEvalModelUpdateFreq_Choices = 1L:365L,
      LightGBMCARMA_CrossEvalValuesUpdateFreq_Choices = 1L:365L,
      LightGBMCARMA_EvalMetric_Choices = c("RMSE","MAE","MAPE","Poisson","Quantile","LogLinQuantile","Lq","NumErrors","SMAPE","R2","MSLE","MedianAbsoluteError"),
      LightGBMCARMA_LossFunction_Choices = c('reg:squarederror','reg:squaredlogerror','reg:pseudohubererror','count:poisson','reg:gamma','reg:tweedie'),
      LightGBMCARMA_Trees_Choices = seq(50,5000,50),
      LightGBMCARMA_Max_Depth_Choices = 2L:16L,
      LightGBMCARMA_ETA_Choices = seq(0.0,0.99,0.01),
      LightGBMCARMA_Min_Data_In_Leaf_Choices = 1L:100L,
      LightGBMCARMA_Num_Leaves_Choices = 1L:100L,
      LightGBMCARMA_Bagging_Fraction_Choices = seq(0.0,1.0,0.01),
      LightGBMCARMA_Feature_Fraction_Choices = seq(0.0,1.0,0.01),
      LightGBMCARMA_Feature_Fraction_Bynode_Choices = seq(0.0,1.0,0.01),
      LightGBMCARMA_Lambda_L1_Choices = seq(0,50,0.5),
      LightGBMCARMA_Lambda_L2_Choices = seq(0,50,0.5),

      # Reactives
      LightGBMCARMA_ModelID_Selected = if(length(input$LightGBMCARMA_ModelID) > 0L) input$LightGBMCARMA_ModelID else "FC1",
      LightGBMCARMA_TrainData_Selected = if(length(input$LightGBMCARMA_TrainData) > 0L) input$LightGBMCARMA_TrainData else NULL,
      LightGBMCARMA_ValidationData_Selected = if(length(input$LightGBMCARMA_ValidationData) > 0L) input$LightGBMCARMA_ValidationData else NULL,
      LightGBMCARMA_XREGS_Selected = if(length(input$LightGBMCARMA_XREGS) > 0L) input$LightGBMCARMA_XREGS else NULL,
      LightGBMCARMA_RunMode_Selected = if(length(input$LightGBMCARMA_RunMode) > 0L) input$LightGBMCARMA_RunMode else 'Train New Model',
      LightGBMCARMA_TimeUnit_Selected = if(length(input$LightGBMCARMA_TimeUnit) > 0L) input$LightGBMCARMA_TimeUnit else "Daily",
      LightGBMCARMA_FCPeriods_Selected = if(length(input$LightGBMCARMA_FCPeriods) > 0L) input$LightGBMCARMA_FCPeriods else 5L,
      LightGBMCARMA_TaskType_Selected = if(length(input$LightGBMCARMA_TaskType) > 0L) input$LightGBMCARMA_TaskType else "CPU",
      LightGBMCARMA_NumGPU_Selected = if(length(input$LightGBMCARMA_NumGPU) > 0L) input$LightGBMCARMA_NumGPU else 1,
      LightGBMCARMA_SaveModel_Selected = if(length(input$LightGBMCARMA_SaveModel) > 0L) input$LightGBMCARMA_SaveModel else FALSE,
      LightGBMCARMA_ArgsList_Selected = if(length(input$LightGBMCARMA_ArgsList) > 0L) input$LightGBMCARMA_ArgsList else tryCatch({names(ModelOutputList)}, error = function(x) NULL),
      LightGBMCARMA_TimeWeights_Selected = if(length(input$LightGBMCARMA_TimeWeights) > 0L) input$LightGBMCARMA_TimeWeights else 1,
      LightGBMCARMA_Methods_Selected = if(length(input$LightGBMCARMA_Methods) > 0L) input$LightGBMCARMA_Methods else NULL,
      LightGBMCARMA_Difference_Selected = if(length(input$LightGBMCARMA_Difference) > 0L) input$LightGBMCARMA_Difference else FALSE,
      LightGBMCARMA_NonNegativePred_Selected = if(length(input$LightGBMCARMA_NonNegativePred) > 0L) input$LightGBMCARMA_NonNegativePred else FALSE,
      LightGBMCARMA_RoundPreds_Selected = if(length(input$LightGBMCARMA_RoundPreds) > 0L) input$LightGBMCARMA_RoundPreds else FALSE,
      LightGBMCARMA_CalendarVariables_Selected = if(length(input$LightGBMCARMA_CalendarVariables) > 0L) input$LightGBMCARMA_CalendarVariables else NULL,
      LightGBMCARMA_HolidayVariables_Selected = if(length(input$LightGBMCARMA_HolidayVariables) > 0L) input$LightGBMCARMA_HolidayVariables else NULL,
      LightGBMCARMA_Lookback_Selected = if(length(input$LightGBMCARMA_Lookback) > 0L) input$LightGBMCARMA_Lookback else NULL,
      LightGBMCARMA_Lags_Selected = if(length(input$LightGBMCARMA_Lags) > 0L) input$LightGBMCARMA_Lags else NULL,
      LightGBMCARMA_MovingAverages_Selected = if(length(input$LightGBMCARMA_MovingAverages) > 0L) input$LightGBMCARMA_MovingAverages else NULL,
      LightGBMCARMA_MovingSD_Selected = if(length(input$LightGBMCARMA_MovingSD) > 0L) input$LightGBMCARMA_MovingSD else NULL,
      LightGBMCARMA_MovingSkew_Selected = if(length(input$LightGBMCARMA_MovingSkew) > 0L) input$LightGBMCARMA_MovingSkew else NULL,
      LightGBMCARMA_MovingKurt_Selected = if(length(input$LightGBMCARMA_MovingKurt) > 0L) input$LightGBMCARMA_MovingKurt else NULL,
      LightGBMCARMA_MovingQuantiles_Selected = if(length(input$LightGBMCARMA_MovingQuantiles) > 0L) input$LightGBMCARMA_MovingQuantiles else NULL,
      LightGBMCARMA_Quantiles_Selected_Selected = if(length(input$LightGBMCARMA_Quantiles_Selected) > 0L) input$LightGBMCARMA_Quantiles_Selected else NULL,
      LightGBMCARMA_AnomalyDetection_HighThreshold_Selected = if(length(input$LightGBMCARMA_AnomalyDetection_HighThreshold) > 0L) input$LightGBMCARMA_AnomalyDetection_HighThreshold else NULL,
      LightGBMCARMA_AnomalyDetection_LowThreshold_Selected = if(length(input$LightGBMCARMA_AnomalyDetection_LowThreshold) > 0L) input$LightGBMCARMA_AnomalyDetection_LowThreshold else NULL,
      LightGBMCARMA_TimeTrend_Selected = if(length(input$LightGBMCARMA_TimeTrend) > 0L) input$LightGBMCARMA_TimeTrend else FALSE,
      LightGBMCARMA_DataTruncate_Selected = if(length(input$LightGBMCARMA_DataTruncate) > 0L) input$LightGBMCARMA_DataTruncate else FALSE,
      LightGBMCARMA_EncodingMethod_Selected = if(length(input$LightGBMCARMA_EncodingMethod) > 0L) input$LightGBMCARMA_EncodingMethod else 'credibility',
      LightGBMCARMA_ZeroPadSeries_Selected = if(length(input$LightGBMCARMA_ZeroPadSeries) > 0L) input$LightGBMCARMA_ZeroPadSeries else 'maxmax',
      LightGBMCARMA_CrossEvalModelUpdate_Selected = if(length(input$LightGBMCARMA_CrossEvalModelUpdate) > 0L) input$LightGBMCARMA_CrossEvalModelUpdate else FALSE,
      LightGBMCARMA_CrossEvalValuesUpdate_Selected = if(length(input$LightGBMCARMA_CrossEvalValuesUpdate) > 0L) input$LightGBMCARMA_CrossEvalValuesUpdate else FALSE,
      LightGBMCARMA_CrossEvalModelUpdateFreq_Selected = if(length(input$LightGBMCARMA_CrossEvalModelUpdateFreq) > 0L) input$LightGBMCARMA_CrossEvalModelUpdateFreq else 1L,
      LightGBMCARMA_CrossEvalValuesUpdateFreq_Selected = if(length(input$LightGBMCARMA_CrossEvalValuesUpdateFreq) > 0L) input$LightGBMCARMA_CrossEvalValuesUpdateFreq else 1L,
      LightGBMCARMA_EvalMetric_Selected = if(length(input$LightGBMCARMA_EvalMetric) > 0L) input$LightGBMCARMA_EvalMetric else 'RMSE',
      LightGBMCARMA_LossFunction_Selected = 'reg:squarederror',
      LightGBMCARMA_Trees_Selected = if(length(input$LightGBMCARMA_Trees) > 0L) input$LightGBMCARMA_Trees else 500L,
      LightGBMCARMA_Max_Depth_Selected = if(length(input$LightGBMCARMA_Max_Depth) > 0L) input$LightGBMCARMA_Max_Depth else 6L,
      LightGBMCARMA_ETA_Selected = if(length(input$LightGBMCARMA_ETA) > 0L) input$LightGBMCARMA_ETA else 0.10,
      LightGBMCARMA_Min_Data_In_Leaf_Selected = if(length(input$LightGBMCARMA_Min_Data_In_Leaf) > 0L) input$LightGBMCARMA_Min_Data_In_Leaf else 1,
      LightGBMCARMA_Num_Leaves_Selected = if(length(input$LightGBMCARMA_Num_Leaves) > 0L) input$LightGBMCARMA_Num_Leaves else 31,
      LightGBMCARMA_Bagging_Fraction_Selected = if(length(input$LightGBMCARMA_Bagging_Fraction) > 0L) input$LightGBMCARMA_Bagging_Fraction else 1.0,
      LightGBMCARMA_Feature_Fraction_Selected = if(length(input$LightGBMCARMA_Feature_Fraction) > 0L) input$LightGBMCARMA_Feature_Fraction else 1.0,
      LightGBMCARMA_Feature_Fraction_Bynode_Selected = if(length(input$LightGBMCARMA_Feature_Fraction_Bynode) > 0L) input$LightGBMCARMA_Feature_Fraction_Bynode else 1.0,
      LightGBMCARMA_Lambda_L1_Selected = if(length(input$LightGBMCARMA_Lambda_L1) > 0L) input$LightGBMCARMA_Lambda_L1 else 4,
      LightGBMCARMA_Lambda_L2_Selected = if(length(input$LightGBMCARMA_Lambda_L2) > 0L) input$LightGBMCARMA_Lambda_L2 else 4)


    # Reactive
    LightGBMCARMA_TrainDataReactive <- shiny::reactive({tryCatch({DataList[[input$LightGBMCARMA_TrainData]][['data']]}, error = function(x) NULL)})
    shiny::observeEvent(LightGBMCARMA_TrainDataReactive(), {
      ChoiceList <- list()
      ColTypes <- unique(Quantico:::ColTypes(LightGBMCARMA_TrainDataReactive()))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(LightGBMCARMA_TrainDataReactive(), Types = ColTypes[i])
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = "LightGBMCARMA_TargetColumnName", Label = "Target Variable", Choices = ChoiceList, SelectedDefault = if(length(input$LightGBMCARMA_TargetColumnName) > 0L) input$LightGBMCARMA_TargetColumnName else NULL, Multiple = TRUE, Debug = Debug)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = "LightGBMCARMA_DateColumnName", Label = "Date Variable", Choices = ChoiceList, SelectedDefault = if(length(input$LightGBMCARMA_DateColumnName) > 0L) input$LightGBMCARMA_DateColumnName else NULL, Multiple = TRUE, Debug = Debug)
      Quantico:::PickerInput(session = session, Update = TRUE, InputID = "LightGBMCARMA_GroupVariables", Label = "Group Variables", Choices = ChoiceList, SelectedDefault = if(length(input$LightGBMCARMA_GroupVariables) > 0L) input$LightGBMCARMA_GroupVariables else NULL, Multiple = TRUE, Debug = Debug)
    }, ignoreInit = FALSE)

  })
  shiny::observeEvent(input$LightGBMCARMA_OK, {shiny::removeModal()})
  shiny::observeEvent(input$LightGBMCARMA_RefreshInputs, {shinyjs::click(id = 'LightGBMCARMA_Modal', asis = TRUE)})

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Inputs :: Plotting Text Labels    ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Dragula Updates
  shiny::observeEvent({eval(DragulaExpression)}, {

    # Update for all available tabs
    for(i in seq_len(NumTabs)) {

      # Exit if no Plot Boxes are Available
      ChoiceBoxes_NOT_Available <- length(input[[paste0('PlotTypeDragula',i)]][['source']]) == 0L
      SelectedBoxes_NOT_Available <- length(input[[paste0('PlotTypeDragula',i)]][['target']][['View']]) == 0L
      if(ChoiceBoxes_NOT_Available && SelectedBoxes_NOT_Available) {
        break
      }

      Choices <- input[[paste0('PlotTypeDragula',i)]][['source']]
      Selected <- input[[paste0('PlotTypeDragula',i)]][['target']][['View']]
      for(j in seq_len(NumPlotsAvailable)) {
        if(length(Choices) > 0L) {
          for(c in Choices) data.table::set(PlotMap, i = which(PlotMap$PlotName == eval(c)), j = "Selected", value = FALSE)
        }
      }

      for(j in seq_len(NumPlotsAvailable)) {
        if(length(Selected) > 0L) {
          for(c in Selected) data.table::set(PlotMap, i = which(PlotMap$PlotName == eval(c)), j = "Selected", value = TRUE)
        }
      }
    }

    PlotMap <<- PlotMap
  }, ignoreInit = TRUE) # investigate ignoreNULL = FALSE; I deal with this on the ObsEvent

  # Text updates
  shiny::observeEvent(input$PlotID_1, {
    NewName <- input$PlotID_1
    if(length(NewName) > 0L) {
      if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
        data.table::set(PlotMap, i = 1L, j = "PlotName", value = NewName)
      } else {
        data.table::set(PlotMap, i = 1L, j = "PlotName", value = paste0(NewName, " 1"))
        data.table::set(PlotMap, i = 1L, j = "PlotType", value = NewName)
      }
    } else {
      print("NewName 1b")
      data.table::set(PlotMap, i = 1L, j = "PlotName", value = "Plot1")
    }
    PlotMap <<- PlotMap
    DragulaChoices <- as.list(PlotMap[TabNumber == 1 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
    DragulaSelected <- list()
    DragulaSelected[['View']] <- PlotMap[TabNumber == 1 & Selected == TRUE][["PlotName"]]
    esquisse::updateDragulaInput(
      session = session,
      inputId = "PlotTypeDragula1",
      choices = DragulaChoices,
      selected = DragulaSelected)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  shiny::observeEvent(input$PlotID_2, {
    NewName <- input$PlotID_2
    if(length(NewName) > 0L) {
      if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
        data.table::set(PlotMap, i = 2L, j = "PlotName", value = NewName)
      } else {
        data.table::set(PlotMap, i = 2L, j = "PlotName", value = paste0(NewName, " 2"))
        data.table::set(PlotMap, i = 2L, j = "PlotType", value = NewName)
      }
    } else {
      data.table::set(PlotMap, i = 2L, j = "PlotName", value = "Plot2")
    }
    PlotMap <<- PlotMap
    DragulaChoices <- as.list(PlotMap[TabNumber == 1 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
    DragulaSelected <- list()
    DragulaSelected[['View']] <- PlotMap[TabNumber == 1 & Selected == TRUE][["PlotName"]]
    esquisse::updateDragulaInput(
      session = session,
      inputId = "PlotTypeDragula1",
      choices = DragulaChoices,
      selected = DragulaSelected)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  shiny::observeEvent(input$PlotID_3, {
    NewName <- input$PlotID_3
    if(length(NewName) > 0L) {
      if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
        data.table::set(PlotMap, i = 3L, j = "PlotName", value = NewName)
      } else {
        data.table::set(PlotMap, i = 3L, j = "PlotName", value = paste0(NewName, " 3"))
        data.table::set(PlotMap, i = 3L, j = "PlotType", value = NewName)
      }
    } else {
      data.table::set(PlotMap, i = 3L, j = "PlotName", value = "Plot3")
    }
    PlotMap <<- PlotMap
    DragulaChoices <- as.list(PlotMap[TabNumber == 1 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
    DragulaSelected <- list()
    DragulaSelected[['View']] <- PlotMap[TabNumber == 1 & Selected == TRUE][["PlotName"]]
    esquisse::updateDragulaInput(
      session = session,
      inputId = "PlotTypeDragula1",
      choices = DragulaChoices,
      selected = DragulaSelected)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  shiny::observeEvent(input$PlotID_4, {
    NewName <- input$PlotID_4
    if(length(NewName) > 0L) {
      if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
        data.table::set(PlotMap, i = 4L, j = "PlotName", value = NewName)
      } else {
        data.table::set(PlotMap, i = 4L, j = "PlotName", value = paste0(NewName, " 4"))
        data.table::set(PlotMap, i = 4L, j = "PlotType", value = NewName)
      }
    } else {
      data.table::set(PlotMap, i = 4L, j = "PlotName", value = "Plot4")
    }
    PlotMap <<- PlotMap
    DragulaChoices <- as.list(PlotMap[TabNumber == 1 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
    DragulaSelected <- list()
    DragulaSelected[['View']] <- PlotMap[TabNumber == 1 & Selected == TRUE][["PlotName"]]
    esquisse::updateDragulaInput(
      session = session,
      inputId = "PlotTypeDragula1",
      choices = DragulaChoices,
      selected = DragulaSelected)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  shiny::observeEvent(input$PlotID_5, {
    NewName <- input$PlotID_5
    if(length(NewName) > 0L) {
      if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
        data.table::set(PlotMap, i = 5L, j = "PlotName", value = NewName)
      } else {
        data.table::set(PlotMap, i = 5L, j = "PlotName", value = paste0(NewName, " 5"))
        data.table::set(PlotMap, i = 5L, j = "PlotType", value = NewName)
      }
    } else {
      data.table::set(PlotMap, i = 5L, j = "PlotName", value = "Plot5")
    }
    PlotMap <<- PlotMap
    DragulaChoices <- as.list(PlotMap[TabNumber == 1 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
    DragulaSelected <- list()
    DragulaSelected[['View']] <- PlotMap[TabNumber == 1 & Selected == TRUE][["PlotName"]]
    esquisse::updateDragulaInput(
      session = session,
      inputId = "PlotTypeDragula1",
      choices = DragulaChoices,
      selected = DragulaSelected)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  shiny::observeEvent(input$PlotID_6, {
    NewName <- input$PlotID_6
    if(length(NewName) > 0L) {
      if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
        data.table::set(PlotMap, i = 6L, j = "PlotName", value = NewName)
      } else {
        data.table::set(PlotMap, i = 6L, j = "PlotName", value = paste0(NewName, " 6"))
        data.table::set(PlotMap, i = 6L, j = "PlotType", value = NewName)
      }
    } else {
      data.table::set(PlotMap, i = 6L, j = "PlotName", value = "Plot6")
    }
    PlotMap <<- PlotMap
    DragulaChoices <- as.list(PlotMap[TabNumber == 1 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
    DragulaSelected <- list()
    DragulaSelected[['View']] <- PlotMap[TabNumber == 1 & Selected == TRUE][["PlotName"]]
    esquisse::updateDragulaInput(
      session = session,
      inputId = "PlotTypeDragula1",
      choices = DragulaChoices,
      selected = DragulaSelected)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  shiny::observeEvent(input$PlotID_7, {
    NewName <- input$PlotID_7
    if(length(NewName) > 0L) {
      if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
        data.table::set(PlotMap, i = 7L, j = "PlotName", value = NewName)
      } else {
        data.table::set(PlotMap, i = 7L, j = "PlotName", value = paste0(NewName, " 7"))
        data.table::set(PlotMap, i = 7L, j = "PlotType", value = NewName)
      }
    } else {
      data.table::set(PlotMap, i = 7L, j = "PlotName", value = "Plot7")
    }
    PlotMap <<- PlotMap
    DragulaChoices <- as.list(PlotMap[TabNumber == 1 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
    DragulaSelected <- list()
    DragulaSelected[['View']] <- PlotMap[TabNumber == 1 & Selected == TRUE][["PlotName"]]
    esquisse::updateDragulaInput(
      session = session,
      inputId = "PlotTypeDragula1",
      choices = DragulaChoices,
      selected = DragulaSelected)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  shiny::observeEvent(input$PlotID_8, {
    NewName <- input$PlotID_8
    if(length(NewName) > 0L) {
      if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
        data.table::set(PlotMap, i = 8L, j = "PlotName", value = NewName)
      } else {
        data.table::set(PlotMap, i = 8L, j = "PlotName", value = paste0(NewName, " 8"))
        data.table::set(PlotMap, i = 8L, j = "PlotType", value = NewName)
      }
    } else {
      data.table::set(PlotMap, i = 8L, j = "PlotName", value = "Plot8")
    }
    PlotMap <<- PlotMap
    DragulaChoices <- as.list(PlotMap[TabNumber == 1 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
    DragulaSelected <- list()
    DragulaSelected[['View']] <- PlotMap[TabNumber == 1 & Selected == TRUE][["PlotName"]]
    esquisse::updateDragulaInput(
      session = session,
      inputId = "PlotTypeDragula1",
      choices = DragulaChoices,
      selected = DragulaSelected)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  shiny::observeEvent(input$PlotID_9, {
    NewName <- input$PlotID_9
    if(length(NewName) > 0L) {
      if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
        data.table::set(PlotMap, i = 9L, j = "PlotName", value = NewName)
      } else {
        data.table::set(PlotMap, i = 9L, j = "PlotName", value = paste0(NewName, " 9"))
        data.table::set(PlotMap, i = 9L, j = "PlotType", value = NewName)
      }
    } else {
      data.table::set(PlotMap, i = 9L, j = "PlotName", value = "Plot9")
    }
    PlotMap <<- PlotMap
    DragulaChoices <- as.list(PlotMap[TabNumber == 1 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
    DragulaSelected <- list()
    DragulaSelected[['View']] <- PlotMap[TabNumber == 1 & Selected == TRUE][["PlotName"]]
    esquisse::updateDragulaInput(
      session = session,
      inputId = "PlotTypeDragula1",
      choices = DragulaChoices,
      selected = DragulaSelected)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  shiny::observeEvent(input$PlotID_10, {
    NewName <- input$PlotID_10
    if(length(NewName) > 0L) {
      if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
        data.table::set(PlotMap, i = 10L, j = "PlotName", value = NewName)
      } else {
        data.table::set(PlotMap, i = 10L, j = "PlotName", value = paste0(NewName, " 10"))
        data.table::set(PlotMap, i = 10L, j = "PlotType", value = NewName)
      }
    } else {
      data.table::set(PlotMap, i = 10L, j = "PlotName", value = "Plot10")
    }
    PlotMap <<- PlotMap
    DragulaChoices <- as.list(PlotMap[TabNumber == 1 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
    DragulaSelected <- list()
    DragulaSelected[['View']] <- PlotMap[TabNumber == 1 & Selected == TRUE][["PlotName"]]
    esquisse::updateDragulaInput(
      session = session,
      inputId = "PlotTypeDragula1",
      choices = DragulaChoices,
      selected = DragulaSelected)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  shiny::observeEvent(input$PlotID_11, {
    NewName <- input$PlotID_11
    if(length(NewName) > 0L) {
      if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
        data.table::set(PlotMap, i = 11L, j = "PlotName", value = NewName)
      } else {
        data.table::set(PlotMap, i = 11L, j = "PlotName", value = paste0(NewName, " 11"))
        data.table::set(PlotMap, i = 11L, j = "PlotType", value = NewName)
      }
    } else {
      data.table::set(PlotMap, i = 11L, j = "PlotName", value = "Plot11")
    }
    PlotMap <<- PlotMap
    DragulaChoices <- as.list(PlotMap[TabNumber == 1 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
    DragulaSelected <- list()
    DragulaSelected[['View']] <- PlotMap[TabNumber == 1 & Selected == TRUE][["PlotName"]]
    esquisse::updateDragulaInput(
      session = session,
      inputId = "PlotTypeDragula1",
      choices = DragulaChoices,
      selected = DragulaSelected)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  shiny::observeEvent(input$PlotID_12, {
    NewName <- input$PlotID_12
    if(length(NewName) > 0L) {
      if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
        data.table::set(PlotMap, i = 12L, j = "PlotName", value = NewName)
      } else {
        data.table::set(PlotMap, i = 12L, j = "PlotName", value = paste0(NewName, " 12"))
        data.table::set(PlotMap, i = 12L, j = "PlotType", value = NewName)
      }
    } else {
      data.table::set(PlotMap, i = 12L, j = "PlotName", value = "Plot12")
    }
    PlotMap <<- PlotMap
    DragulaChoices <- as.list(PlotMap[TabNumber == 1 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
    DragulaSelected <- list()
    DragulaSelected[['View']] <- PlotMap[TabNumber == 1 & Selected == TRUE][["PlotName"]]
    esquisse::updateDragulaInput(
      session = session,
      inputId = "PlotTypeDragula1",
      choices = DragulaChoices,
      selected = DragulaSelected)
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  if(NumPlotsAvailable > 12L) {

    if(Debug) print("NumPlotsAvailable > 12L")

    shiny::observeEvent(input$PlotID_13, {
      NewName <- input$PlotID_13
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 13L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 13L, j = "PlotName", value = paste0(NewName, " 13"))
          data.table::set(PlotMap, i = 13L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 13L, j = "PlotName", value = "Plot13")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 2 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 2 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula2",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_14, {
      NewName <- input$PlotID_14
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 14L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 14L, j = "PlotName", value = paste0(NewName, " 14"))
          data.table::set(PlotMap, i = 14L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 14L, j = "PlotName", value = "Plot12")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 2 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 2 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula2",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_15, {
      NewName <- input$PlotID_15
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 15L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 15L, j = "PlotName", value = paste0(NewName, " 15"))
          data.table::set(PlotMap, i = 15L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 15L, j = "PlotName", value = "Plot12")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 2 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 2 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula2",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_16, {
      NewName <- input$PlotID_16
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 16L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 16L, j = "PlotName", value = paste0(NewName, " 16"))
          data.table::set(PlotMap, i = 16L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 16L, j = "PlotName", value = "Plot12")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 2 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 2 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula2",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_17, {
      NewName <- input$PlotID_17
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 17L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 17L, j = "PlotName", value = paste0(NewName, " 17"))
          data.table::set(PlotMap, i = 17L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 17L, j = "PlotName", value = "Plot12")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 2 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 2 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula2",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_18, {
      NewName <- input$PlotID_18
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 18L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 18L, j = "PlotName", value = paste0(NewName, " 18"))
          data.table::set(PlotMap, i = 18L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 18L, j = "PlotName", value = "Plot12")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 2 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 2 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula2",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_19, {
      NewName <- input$PlotID_19
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 19L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 19L, j = "PlotName", value = paste0(NewName, " 19"))
          data.table::set(PlotMap, i = 19L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 19L, j = "PlotName", value = "Plot12")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 2 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 2 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula2",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_20, {
      NewName <- input$PlotID_20
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 20L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 20L, j = "PlotName", value = paste0(NewName, " 20"))
          data.table::set(PlotMap, i = 20L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 20L, j = "PlotName", value = "Plot12")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 2 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 2 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula2",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_21, {
      NewName <- input$PlotID_21
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 21L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 21L, j = "PlotName", value = paste0(NewName, " 21"))
          data.table::set(PlotMap, i = 21L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 21L, j = "PlotName", value = "Plot12")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 2 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 2 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula2",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_22, {
      NewName <- input$PlotID_22
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 22L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 22L, j = "PlotName", value = paste0(NewName, " 22"))
          data.table::set(PlotMap, i = 22L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 22L, j = "PlotName", value = "Plot12")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 2 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 2 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula2",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_23, {
      NewName <- input$PlotID_23
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 23L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 23L, j = "PlotName", value = paste0(NewName, " 23"))
          data.table::set(PlotMap, i = 23L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 23L, j = "PlotName", value = "Plot12")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 2 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 2 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula2",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_24, {
      NewName <- input$PlotID_24
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 24L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 24L, j = "PlotName", value = paste0(NewName, " 24"))
          data.table::set(PlotMap, i = 24L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 24L, j = "PlotName", value = "Plot12")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 2 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 2 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula2",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
  }

  if(NumPlotsAvailable > 24L) {

    if(Debug) print("NumPlotsAvailable > 24L")

    shiny::observeEvent(input$PlotID_25, {
      NewName <- input$PlotID_25
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 25L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 25L, j = "PlotName", value = paste0(NewName, " 25"))
          data.table::set(PlotMap, i = 25L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 25L, j = "PlotName", value = "Plot25")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 3 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:36L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 3 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula3",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_26, {
      NewName <- input$PlotID_26
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 26L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 26L, j = "PlotName", value = paste0(NewName, " 26"))
          data.table::set(PlotMap, i = 26L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 26L, j = "PlotName", value = "Plot26")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 3 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:36L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 3 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula3",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_27, {
      NewName <- input$PlotID_27
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 27L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 27L, j = "PlotName", value = paste0(NewName, " 27"))
          data.table::set(PlotMap, i = 27L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 27L, j = "PlotName", value = "Plot27")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 3 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:36L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 3 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula3",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_28, {
      NewName <- input$PlotID_28
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 28L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 28L, j = "PlotName", value = paste0(NewName, " 28"))
          data.table::set(PlotMap, i = 28L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 28L, j = "PlotName", value = "Plot28")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 3 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:36L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 3 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula3",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_29, {
      NewName <- input$PlotID_29
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 29L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 29L, j = "PlotName", value = paste0(NewName, " 29"))
          data.table::set(PlotMap, i = 29L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 29L, j = "PlotName", value = "Plot29")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 3 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:36L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 3 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula3",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_30, {
      NewName <- input$PlotID_30
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 30L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 30L, j = "PlotName", value = paste0(NewName, " 30"))
          data.table::set(PlotMap, i = 30L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 30L, j = "PlotName", value = "Plot30")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 3 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:36L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 3 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula3",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_31, {
      NewName <- input$PlotID_31
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 31L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 31L, j = "PlotName", value = paste0(NewName, " 31"))
          data.table::set(PlotMap, i = 31L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 31L, j = "PlotName", value = "Plot31")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 3 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:36L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 3 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula3",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_32, {
      NewName <- input$PlotID_32
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 32L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 32L, j = "PlotName", value = paste0(NewName, " 32"))
          data.table::set(PlotMap, i = 32L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 32L, j = "PlotName", value = "Plot32")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 3 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:36L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 3 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula3",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_33, {
      NewName <- input$PlotID_33
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 33L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 33L, j = "PlotName", value = paste0(NewName, " 33"))
          data.table::set(PlotMap, i = 33L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 33L, j = "PlotName", value = "Plot33")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 3 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:36L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 3 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula3",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_34, {
      NewName <- input$PlotID_34
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 34L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 34L, j = "PlotName", value = paste0(NewName, " 34"))
          data.table::set(PlotMap, i = 34L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 34L, j = "PlotName", value = "Plot34")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 3 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:36L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 3 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula3",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_35, {
      NewName <- input$PlotID_35
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 35L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 35L, j = "PlotName", value = paste0(NewName, " 35"))
          data.table::set(PlotMap, i = 35L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 35L, j = "PlotName", value = "Plot35")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 3 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:36L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 3 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula3",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    shiny::observeEvent(input$PlotID_36, {
      NewName <- input$PlotID_36
      if(length(NewName) > 0L) {
        if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
          data.table::set(PlotMap, i = 36L, j = "PlotName", value = NewName)
        } else {
          data.table::set(PlotMap, i = 36L, j = "PlotName", value = paste0(NewName, " 36"))
          data.table::set(PlotMap, i = 36L, j = "PlotType", value = NewName)
        }
      } else {
        data.table::set(PlotMap, i = 36L, j = "PlotName", value = "Plot36")
      }
      PlotMap <<- PlotMap
      DragulaChoices <- as.list(PlotMap[TabNumber == 3 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:36L),"Nothing Selected")][["PlotName"]])
      DragulaSelected <- list()
      DragulaSelected[['View']] <- PlotMap[TabNumber == 3 & Selected == TRUE][["PlotName"]]
      esquisse::updateDragulaInput(
        session = session,
        inputId = "PlotTypeDragula3",
        choices = DragulaChoices,
        selected = DragulaSelected)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

  }

  # shiny::observeEvent(input$PlotID_25, {
  #   NewName <- input$PlotID_24
  #   if(length(NewName) > 0L) {
  #     if(NewName == "" || grepl(pattern = "Plot", x = substr(x = NewName, 1,4))) {
  #       data.table::set(PlotMap, i = 25L, j = "PlotName", value = NewName)
  #     } else {
  #       data.table::set(PlotMap, i = 25L, j = "PlotName", value = paste0(NewName, " ", (PlotMap[PlotType == NewName, .N] + 1L)))
  #       data.table::set(PlotMap, i = 25L, j = "PlotType", value = NewName)
  #     }
  #   } else {
  #     data.table::set(PlotMap, i = 25L, j = "PlotName", value = "Plot12")
  #   }
  #   PlotMap <<- PlotMap
  #   DragulaChoices <- as.list(PlotMap[TabNumber == 2 & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
  #   DragulaSelected <- list()
  #   DragulaSelected[['View']] <- PlotMap[TabNumber == 2 & Selected == TRUE][["PlotName"]]
  #   esquisse::updateDragulaInput(
  #     session = session,
  #     inputId = "PlotTypeDragula3",
  #     choices = DragulaChoices,
  #     selected = DragulaSelected)
  # }, ignoreInit = TRUE, ignoreNULL = FALSE)

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Inputs :: Plotting Tabs           ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(input$tabss, {
    if(DebugAddPlotTab) print("shiny::observeEvent(input$tabss 1")
    PMisc <- tryCatch({as.integer(gsub("[^\\d]+", "", input$tabss, perl=TRUE))}, error = function(x) 0L)
    if(!is.na(PMisc) && PMisc > 0L) {
      if(tryCatch({input$tabss}, error = function(x) "aa") %in% paste0('Plots ', PMisc) && PMisc > 0L) {
        if(!exists("DragulaChoicesList")) {DragulaChoicesList <- list();DragulaChoicesList <<- DragulaChoicesList}
        if(!exists("plot_output_list")) {plot_output_list <- list(); plot_output_list <<- plot_output_list}
        PlotPanelInputsList[[paste0("PlotWidth",PMisc)]] <- input[[paste0("PlotWidth",PMisc)]]
        PlotPanelInputsList[[paste0("PlotHeight",PMisc)]] <- input[[paste0("PlotHeight",PMisc)]]
        PlotPanelInputsList[[paste0("GridCols",PMisc)]] <- input[[paste0("GridCols",PMisc)]]
        PlotPanelInputsList[[paste0("Number_of_Bins",PMisc)]] <- input[[paste0("Number_of_Bins",PMisc)]]
        PlotPanelInputsList[[paste0("Number_of_Levels",PMisc)]] <- input[[paste0("Number_of_Levels",PMisc)]]
        PlotPanelInputsList[[paste0("FontSize",PMisc)]] <- input[[paste0("FontSize",PMisc)]]
        PlotPanelInputsList <<- PlotPanelInputsList
        if(!exists("ChoiceUpdate")) {
          ChoiceUpdate <- c()
          for(j in seq_len(NumPlotsAvailable)) ChoiceUpdate[j] <- paste0("Plot",j)
          ChoiceUpdate <<- ChoiceUpdate
          ChoiceUpdate_old <- ChoiceUpdate
          ChoiceUpdate_old <<- ChoiceUpdate_old
        }

        Choices <- input[[paste0('PlotTypeDragula',PMisc)]][['source']]
        sel <- input[[paste0('PlotTypeDragula',PMisc)]][['target']][['View']]
        selList <- list()
        selList[['View']] <- sel
        DragulaSelectedList[[paste0("PlotPanel",PMisc)]] <- selList
        DragulaSelectedList <<- DragulaSelectedList
        DragulaChoicesList[[paste0("PlotPanel",PMisc)]] <- DragulaChoices <- as.list(PlotMap[TabNumber == eval(PMisc) & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
        DragulaChoicesList <<- DragulaChoicesList
        if(length(DragulaChoicesList[[paste0("PlotPanel",PMisc)]]) > 0L || length(selList) > 0L) {
          esquisse::updateDragulaInput(
            session = session,
            inputId = paste0("PlotTypeDragula", PMisc),
            choices = DragulaChoicesList[[paste0("PlotPanel",PMisc)]],
            selected = selList)
        } else {

          DragulaChoicesList[[paste0("PlotPanel",PMisc)]] <- as.list(PlotMap[TabNumber == eval(PMisc) & Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
          DragulaChoicesList <<- DragulaChoicesList
          selList <- list()
          esquisse::updateDragulaInput(
            session = session,
            inputId = paste0("PlotTypeDragula", PMisc),
            choices = DragulaChoicesList[[paste0("PlotPanel",PMisc)]],
            selected = selList)
        }
        out <- Quantico:::PlotSize_(input, PMisc, 1, 1)
        if(length(out) == 0L) {out <- list(); out[[paste0("PlotHeight",PMisc)]] <- '860px'; out[[paste0("PlotWidth",PMisc)]] <- '1450px'}
        if(PlotOutput_Start1) {
          PlotOutput_Start1 <<- FALSE
          plot_output_list[[PMisc]] <- echarts4r::renderEcharts4r({
            Quantico:::BlankEchart(Type = "Poincare", Theme = tryCatch({EchartsTheme}, error = function(x) "macarons"))
          })
          if(is.list(plot_output_list[[PMisc]])) {
            attr(plot_output_list[[PMisc]], 'outputArgs') <- list(height = '600px', width = '1135px')
            plot_output_list[[PMisc]]
            output[[paste0("PlottingExecute", PMisc)]] <- shiny::renderUI({plot_output_list[[PMisc]]})
          }
        }
      }
    }
  })

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Inputs :: Output Tabs             ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # ML Reports Inputs Updates
  shiny::observeEvent({eval(MLModelSelectionExpression)}, {
    Page <- tryCatch({as.integer(gsub("[^\\d]+", "", input$tabss, perl=TRUE))}, error = function(x) 0L)
    MLModelSelection <- Quantico::ReturnParam(xx = tryCatch({input[[paste0("MLReportsModelSelection", Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
    print(MLModelSelection)
    ChoiceList <- list()
    d <- tryCatch({ModelOutputList[[MLModelSelection]]$TestData}, error = function(x) NULL)
    TargetVar <- ModelOutputList[[MLModelSelection]]$ArgsList$TargetColumnName
    PredictVar <- ModelOutputList[[MLModelSelection]]$ArgsList$PredictionColumnName
    if(data.table::is.data.table(d)) {
      ColTypes <- unique(Quantico:::ColTypes(d))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(d, Types = ColTypes[i])

      # Remove target variable from ChoiceList
      if(length(ChoiceList$numeric) > 0L) {
        if(TargetVar %in% ChoiceList$numeric) ChoiceList$numeric <- ChoiceList$numeric[!ChoiceList$numeric %in% TargetVar]
        if(TargetVar %in% ChoiceList$numeric) ChoiceList$numeric <- ChoiceList$numeric[!ChoiceList$numeric %in% PredictVar]
      }
      if(length(ChoiceList$character) > 0L) {
        if(TargetVar %in% ChoiceList$character) ChoiceList$character <- ChoiceList$character[!ChoiceList$character %in% TargetVar]
        if(TargetVar %in% ChoiceList$character) ChoiceList$character <- ChoiceList$character[!ChoiceList$character %in% PredictVar]
      }
      if(length(ChoiceList$factor) > 0L) {
        if(TargetVar %in% ChoiceList$factor) ChoiceList$factor <- ChoiceList$factor[!ChoiceList$factor %in% TargetVar]
        if(TargetVar %in% ChoiceList$factor) ChoiceList$factor <- ChoiceList$factor[!ChoiceList$factor %in% PredictVar]
      }

      Quantico::PickerInput(
        session = session,
        input = input,
        Update = TRUE,
        InputID = paste0('PDPVariables',Page),
        Label = 'PDP Variables',
        Choices = ChoiceList,
        Multiple = TRUE,
        MaxVars = 100L)
      Quantico::PickerInput(
        session = session,
        input = input,
        Update = TRUE,
        InputID = paste0('GroupVariableInclude',Page),
        Label = 'By-Variable Include?',
        Choices = ChoiceList,
        Multiple = TRUE,
        MaxVars = 100L)
    }
  })

  # EDA Reports Inputs Updates
  shiny::observeEvent({eval(EDASelectionExpression)}, {
    Page <- tryCatch({as.integer(gsub("[^\\d]+", "", input$tabss, perl=TRUE))}, error = function(x) 0L)
    EDAData <- Quantico::ReturnParam(xx = tryCatch({input[[paste0("EDAData", Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
    ChoiceList <- list()
    dd <- tryCatch({DataList[[EDAData]][['data']]}, error = function(x) NULL)
    if(data.table::is.data.table(dd)) {
      ColTypes <- unique(Quantico:::ColTypes(dd))
      for(i in seq_along(ColTypes)) ChoiceList[[ColTypes[i]]] <- Quantico:::ColNameFilter(dd, Types = ColTypes[i])
      Quantico::PickerInput(
        session = session, input = input, Update = TRUE,
        InputID = paste0('EDAUnivariateVars',Page),
        Label = 'Univariate Vars', Choices = ChoiceList, Multiple = TRUE, MaxVars = 100L)
      Quantico::PickerInput(
        session = session, input = input, Update = TRUE,
        InputID = paste0('EDACorrVars',Page),
        Label = 'Corr Vars', Choices = ChoiceList, Multiple = TRUE, MaxVars = 100L)
      Quantico::PickerInput(
        session = session, input = input, Update = TRUE,
        InputID = paste0('EDATrendVars',Page),
        Label = 'Trend Vars', Choices = ChoiceList, Multiple = TRUE, MaxVars = 100L)
      Quantico::PickerInput(
        session = session, input = input, Update = TRUE,
        InputID = paste0('EDADateVar',Page),
        Label = 'Trend Date Var', Choices = ChoiceList, Multiple = TRUE, MaxVars = 100L)
      Quantico::PickerInput(
        session = session, input = input, Update = TRUE,
        InputID = paste0('EDAGroupVar',Page),
        Label = 'Trend By-Variable', Choices = ChoiceList, Multiple = TRUE, MaxVars = 100L)
    }
  })

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Inputs :: Ok Buttons in Modals    ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  OKModalsButtonsForPlots <- paste0(AvailablePlots, "OK")
  OKModalsButtonsForPlots <- expand.grid(OKModalsButtonsForPlots, seq_len(NumPlotsAvailable))
  OKModalsButtons <- paste0(OKModalsButtonsForPlots$Var1, OKModalsButtonsForPlots$Var2)
  lapply(OKModalsButtons, FUN = function(x) {
    shiny::observeEvent(input[[x]], {

      PlotNums <- as.integer(gsub("[^\\d]+", "", tryCatch(x, error = function(x) 1), perl=TRUE))
      PlotType <- PlotMap[PlotNumber == eval(PlotNums)][["PlotType"]]#Quantico::ReturnParam(xx = input[[paste0('PlotID_',PlotNums)]], Type = "character", Default = NULL)#tryCatch({input[[paste0('Plot',shiny::req(PN()))]]}, error = function(x) NULL)
      PlotType <<- PlotType
      key <- paste0(PlotType, PlotNums)

      print("OK BUTTON MODAL 1")
      print(PlotNums)

      SelectedData_Restore <- shiny::reactive({shiny::isolate(input[[paste0("Plot",PlotNums,"_SelectData")]])})
      SelectedPlot_Restore <- shiny::reactive({shiny::isolate(input[[paste0('Plot',PlotNums)]])})
      SampleSize_Restore <- shiny::reactive({shiny::isolate(input[[paste0('SampleSize',PlotNums)]])})
      SamplingSize_Restore <- shiny::reactive({shiny::isolate(input[[paste0('SamplingSize',PlotNums)]])})
      Symbols_Restore <- shiny::reactive({shiny::isolate(input[[paste0('Symbols', PlotNums)]])})
      StockMetric_Restore <- shiny::reactive({shiny::isolate(input[[paste0('StockMetric', PlotNums)]])})
      StockDateRange_Restore <- shiny::reactive({shiny::isolate(input[[paste0('StockDateRange',PlotNums)]])})
      StockTimeAgg_Restore <- shiny::reactive({shiny::isolate(input[[paste0('StockTimeAgg',PlotNums)]])})
      YVar_Restore <- shiny::reactive({shiny::isolate(input[[paste0('YVar',PlotNums)]])})
      DualYVar_Restore <- shiny::reactive({shiny::isolate(input[[paste0('DualYVar',PlotNums)]])})
      PlottingDateVar_Restore <- shiny::reactive({shiny::isolate(input[[paste0('PlottingDateVar',PlotNums)]])})
      PlotDateAgg_Restore <- shiny::reactive({shiny::isolate(input[[paste0('PlotDateAgg',PlotNums)]])})
      XVar_Restore <- shiny::reactive({shiny::isolate(input[[paste0('XVar',PlotNums)]])})
      ZVar_Restore <- shiny::reactive({shiny::isolate(input[[paste0('ZVar',PlotNums)]])})

      PlottingMaxLags_Restore <- shiny::reactive({shiny::isolate(input[[paste0('PlottingMaxLags',PlotNums)]])})
      PlottingTimeUnit_Restore <- shiny::reactive({shiny::isolate(input[[paste0('PlottingTimeUnit',PlotNums)]])})

      Title_Restore <- shiny::reactive({shiny::isolate(input[[paste0('Title',PlotNums)]])})
      ShowLabels_Restore <- shiny::reactive({shiny::isolate(input[[paste0('ShowLabels',PlotNums)]])})
      YAxisTitle_Restore <- shiny::reactive({shiny::isolate(input[[paste0('YAxisTitle',PlotNums)]])})
      XAxisTitle_Restore <- shiny::reactive({shiny::isolate(input[[paste0('XAxisTitle',PlotNums)]])})
      GroupVars_Restore <- shiny::reactive({shiny::isolate(input[[paste0('GroupVars',PlotNums)]])})
      AggMethod_Restore <- shiny::reactive({shiny::isolate(input[[paste0('AggMethod',PlotNums)]])})
      FacetRows_Restore <- shiny::reactive({shiny::isolate(input[[paste0('FacetRows',PlotNums)]])})
      FacetCols_Restore <- shiny::reactive({shiny::isolate(input[[paste0('FacetCols',PlotNums)]])})
      YVarTrans_Restore <- shiny::reactive({shiny::isolate(input[[paste0('YVarTrans',PlotNums)]])})
      DualYVarTrans_Restore <- shiny::reactive({shiny::isolate(input[[paste0('DualYVarTrans',PlotNums)]])})
      XVarTrans_Restore <- shiny::reactive({shiny::isolate(input[[paste0('XVarTrans',PlotNums)]])})
      ZVarTrans_Restore <- shiny::reactive({shiny::isolate(input[[paste0('ZVarTrans',PlotNums)]])})
      Levels_Restore1 <- shiny::reactive({shiny::isolate(input[[paste0('Levels_',PlotNums,'_1')]])})
      Levels_Restore2 <- shiny::reactive({shiny::isolate(input[[paste0('Levels_',PlotNums,'_2')]])})
      Levels_Restore3 <- shiny::reactive({shiny::isolate(input[[paste0('Levels_',PlotNums,'_3')]])})
      FilterVariable_Restore1 <- shiny::reactive({shiny::isolate(input[[paste0('FilterVariable_',PlotNums,'_1')]])})
      FilterVariable_Restore2 <- shiny::reactive({shiny::isolate(input[[paste0('FilterVariable_',PlotNums,'_2')]])})
      FilterVariable_Restore3 <- shiny::reactive({shiny::isolate(input[[paste0('FilterVariable_',PlotNums,'_3')]])})
      FilterVariable_Restore4 <- shiny::reactive({shiny::isolate(input[[paste0('FilterVariable_',PlotNums,'_4')]])})
      FilterLogic_Restore1 <- shiny::reactive({shiny::isolate(input[[paste0('FilterLogic_',PlotNums,'_1')]])})
      FilterLogic_Restore2 <- shiny::reactive({shiny::isolate(input[[paste0('FilterLogic_',PlotNums,'_2')]])})
      FilterLogic_Restore3 <- shiny::reactive({shiny::isolate(input[[paste0('FilterLogic_',PlotNums,'_3')]])})
      FilterLogic_Restore4 <- shiny::reactive({shiny::isolate(input[[paste0('FilterLogic_',PlotNums,'_4')]])})
      FilterValue_Restore1 <- shiny::reactive({shiny::isolate(input[[paste0('FilterValue_',PlotNums,'_1_1')]])})
      FilterValue_Restore2 <- shiny::reactive({shiny::isolate(input[[paste0('FilterValue_',PlotNums,'_1_2')]])})
      FilterValue_Restore3 <- shiny::reactive({shiny::isolate(input[[paste0('FilterValue_',PlotNums,'_2_1')]])})
      FilterValue_Restore4 <- shiny::reactive({shiny::isolate(input[[paste0('FilterValue_',PlotNums,'_2_2')]])})
      FilterValue_Restore5 <- shiny::reactive({shiny::isolate(input[[paste0('FilterValue_',PlotNums,'_3_1')]])})
      FilterValue_Restore6 <- shiny::reactive({shiny::isolate(input[[paste0('FilterValue_',PlotNums,'_3_2')]])})
      FilterValue_Restore7 <- shiny::reactive({shiny::isolate(input[[paste0('FilterValue_',PlotNums,'_4_1')]])})
      FilterValue_Restore8 <- shiny::reactive({shiny::isolate(input[[paste0('FilterValue_',PlotNums,'_4_2')]])})
      shiny::observeEvent(SelectedData_Restore(), {print("Adrian is checking selected data"); PlotDropDown[[key]][[paste0("Plot",PlotNums,"_SelectData")]] <- input[[paste0("Plot",PlotNums,"_SelectData")]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(SelectedPlot_Restore(), {
        PlotDropDown[[key]][[paste0('Plot',PlotNums)]] <- input[[paste0('Plot',PlotNums)]]; print(paste0("PlotType == ", PlotDropDown[[key]][[paste0('Plot',PlotNums)]])); PlotDropDown <<- PlotDropDown
      })

      shiny::observeEvent(PlottingMaxLags_Restore(), {PlotDropDown[[key]][[paste0('PlottingMaxLags',PlotNums)]] <- input[[paste0('PlottingMaxLags',PlotNums)]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(PlottingTimeUnit_Restore(), {PlotDropDown[[key]][[paste0('PlottingTimeUnit',PlotNums)]] <- input[[paste0('PlottingTimeUnit',PlotNums)]]; PlotDropDown <<- PlotDropDown})

      shiny::observeEvent(SampleSize_Restore(), {PlotDropDown[[key]][[paste0('SampleSize',PlotNums)]] <- input[[paste0('SampleSize',PlotNums)]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(SamplingSize_Restore(), {PlotDropDown[[key]][[paste0('SamplingSize',PlotNums)]] <- input[[paste0('SamplingSize',PlotNums)]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(YVar_Restore(), {PlotDropDown[[key]][[paste0('YVar',PlotNums)]] <- input[[paste0('YVar',PlotNums)]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(DualYVar_Restore(), {PlotDropDown[[key]][[paste0('DualYVar',PlotNums)]] <- input[[paste0('DualYVar',PlotNums)]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(PlottingDateVar_Restore(), {PlotDropDown[[key]][[paste0('PlottingDateVar',PlotNums)]] <- input[[paste0('PlottingDateVar',PlotNums)]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(XVar_Restore(), {PlotDropDown[[key]][[paste0('XVar',PlotNums)]] <- input[[paste0('XVar',PlotNums)]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(ZVar_Restore(), {PlotDropDown[[key]][[paste0('ZVar',PlotNums)]] <- input[[paste0('ZVar',PlotNums)]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(GroupVars_Restore(), {PlotDropDown[[key]][[paste0('GroupVars',PlotNums)]] <- input[[paste0('GroupVars',PlotNums)]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(AggMethod_Restore(), {PlotDropDown[[key]][[paste0('AggMethod',PlotNums)]] <- input[[paste0('AggMethod',PlotNums)]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(FacetRows_Restore(), {PlotDropDown[[key]][[paste0('FacetRows',PlotNums)]] <- input[[paste0('FacetRows',PlotNums)]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(FacetCols_Restore(), {PlotDropDown[[key]][[paste0('FacetCols',PlotNums)]] <- input[[paste0('FacetCols',PlotNums)]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(YVarTrans_Restore(), {PlotDropDown[[key]][[paste0('YVarTrans',PlotNums)]] <- input[[paste0('YVarTrans',PlotNums)]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(DualYVarTrans_Restore(), {PlotDropDown[[key]][[paste0('DualYVarTrans',PlotNums)]] <- input[[paste0('DualYVarTrans',PlotNums)]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(XVarTrans_Restore(), {PlotDropDown[[key]][[paste0('XVarTrans',PlotNums)]] <- input[[paste0('XVarTrans',PlotNums)]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(ZVarTrans_Restore(), {PlotDropDown[[key]][[paste0('ZVarTrans',PlotNums)]] <- input[[paste0('ZVarTrans',PlotNums)]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(PlotDateAgg_Restore(), {PlotDropDown[[key]][[paste0('PlotDateAgg',PlotNums)]] <- input[[paste0('PlotDateAgg',PlotNums)]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(Levels_Restore1(), {PlotDropDown[[key]][[paste0('Levels_',PlotNums,'_1')]] <- input[[paste0('Levels_',PlotNums,'_1')]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(Levels_Restore2(), {PlotDropDown[[key]][[paste0('Levels_',PlotNums,'_2')]] <- input[[paste0('Levels_',PlotNums,'_2')]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(Levels_Restore3(), {PlotDropDown[[key]][[paste0('Levels_',PlotNums,'_3')]] <- input[[paste0('Levels_',PlotNums,'_3')]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(Title_Restore(), {PlotDropDown[[key]][[paste0('Title',PlotNums)]] <- input[[paste0('Title',PlotNums)]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(ShowLabels_Restore(), {PlotDropDown[[key]][[paste0('ShowLabels',PlotNums)]] <- input[[paste0('ShowLabels',PlotNums)]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(YAxisTitle_Restore(), {PlotDropDown[[key]][[paste0('YAxisTitle',PlotNums)]] <- input[[paste0('YAxisTitle',PlotNums)]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(XAxisTitle_Restore(), {PlotDropDown[[key]][[paste0('XAxisTitle',PlotNums)]] <- input[[paste0('XAxisTitle',PlotNums)]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(FilterVariable_Restore1(), {PlotDropDown[[key]][[paste0('FilterVariable_',PlotNums,'_1')]] <- input[[paste0('FilterVariable_',PlotNums,'_1')]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(FilterVariable_Restore2(), {PlotDropDown[[key]][[paste0('FilterVariable_',PlotNums,'_2')]] <- input[[paste0('FilterVariable_',PlotNums,'_2')]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(FilterVariable_Restore3(), {PlotDropDown[[key]][[paste0('FilterVariable_',PlotNums,'_3')]] <- input[[paste0('FilterVariable_',PlotNums,'_3')]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(FilterVariable_Restore4(), {PlotDropDown[[key]][[paste0('FilterVariable_',PlotNums,'_4')]] <- input[[paste0('FilterVariable_',PlotNums,'_4')]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(FilterLogic_Restore1(), {PlotDropDown[[key]][[paste0('FilterLogic_',PlotNums,'_1')]] <- input[[paste0('FilterLogic_',PlotNums,'_1')]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(FilterLogic_Restore2(), {PlotDropDown[[key]][[paste0('FilterLogic_',PlotNums,'_2')]] <- input[[paste0('FilterLogic_',PlotNums,'_2')]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(FilterLogic_Restore3(), {PlotDropDown[[key]][[paste0('FilterLogic_',PlotNums,'_3')]] <- input[[paste0('FilterLogic_',PlotNums,'_3')]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(FilterLogic_Restore4(), {PlotDropDown[[key]][[paste0('FilterLogic_',PlotNums,'_4')]] <- input[[paste0('FilterLogic_',PlotNums,'_4')]]; PlotDropDown <<- PlotDropDown})
      shiny::observeEvent(FilterValue_Restore1(), {PlotDropDown[[key]][[paste0('FilterValue_',PlotNums,'_1_1')]] <- input[[paste0('FilterValue_',PlotNums,'_1_1')]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(FilterValue_Restore2(), {PlotDropDown[[key]][[paste0('FilterValue_',PlotNums,'_1_2')]] <- input[[paste0('FilterValue_',PlotNums,'_1_2')]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(FilterValue_Restore3(), {PlotDropDown[[key]][[paste0('FilterValue_',PlotNums,'_2_1')]] <- input[[paste0('FilterValue_',PlotNums,'_2_1')]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(FilterValue_Restore4(), {PlotDropDown[[key]][[paste0('FilterValue_',PlotNums,'_2_2')]] <- input[[paste0('FilterValue_',PlotNums,'_2_2')]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(FilterValue_Restore5(), {PlotDropDown[[key]][[paste0('FilterValue_',PlotNums,'_3_1')]] <- input[[paste0('FilterValue_',PlotNums,'_3_1')]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(FilterValue_Restore6(), {PlotDropDown[[key]][[paste0('FilterValue_',PlotNums,'_3_2')]] <- input[[paste0('FilterValue_',PlotNums,'_3_2')]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(FilterValue_Restore7(), {PlotDropDown[[key]][[paste0('FilterValue_',PlotNums,'_4_1')]] <- input[[paste0('FilterValue_',PlotNums,'_4_1')]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)
      shiny::observeEvent(FilterValue_Restore8(), {PlotDropDown[[key]][[paste0('FilterValue_',PlotNums,'_4_2')]] <- input[[paste0('FilterValue_',PlotNums,'_4_2')]]; PlotDropDown <<- PlotDropDown}, ignoreNULL = FALSE)

      shiny::removeModal()
    }, ignoreInit = TRUE)
  })

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Inputs :: Plotting Modals         ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  PlotInitializeCheck <- TRUE
  PlotInitializeCheck <<- PlotInitializeCheck
  lapply(paste0("PlotDropDown", seq_len(NumPlotsAvailable)), FUN = function(x) {
    shiny::observeEvent(input[[x]], {
      PlotNums <- as.integer(gsub("[^\\d]+", "", tryCatch(x, error = function(x) 1), perl=TRUE))
      PlotType <- PlotMap[PlotNumber == eval(PlotNums)][["PlotType"]]#Quantico::ReturnParam(xx = input[[paste0('PlotID_',PlotNums)]], Type = "character", Default = NULL)#tryCatch({input[[paste0('Plot',shiny::req(PN()))]]}, error = function(x) NULL)
      PlotType <<- PlotType
      if(length(PlotType) > 0L && PlotType != "None") {

        data.table::set(PlotMap, i = PlotNums, j = "PlotType", value = eval(PlotType)); PlotMap <<- PlotMap

        # Kick off modal
        if(PlotType %in% "CorrelogramPlot") {
          Quantico:::Plots.CorrMatrix.Modal(id = paste0('Correlogram',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% "ParallelPlot") {
          Quantico:::Plots.ParallelPlot.Modal(id = paste0('ParallelPlot',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% c("BarPlot3D","HeatMapPlot")) {
          Quantico:::Plots.3D.NoGroup.Modal(id = paste0('ThreeDNG',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% c("PartialDependenceHeatMap")) {
          Quantico:::Plots.3D.Target.NoGroup.Modal(id = paste0('Eval.PDHeatMap',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% c("HistogramPlot","DensityPlot")) {
          Quantico:::Plots.Distribution.Modal(id = paste0('Distribution',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% c("PiePlot","DonutPlot","RosetypePlot")) {
          Quantico:::Plots.Distribution.Y.NoFacet.Modal(id = paste0('DistributionNoFacetY',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% c("BoxPlot","ViolinPlot","PolarPlot")) {
          Quantico:::Plots.Distribution.XY.NoFacet.Modal(id = paste0('DistributionNoFacetXY',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% c("WordCloud")) {
          Quantico:::Plots.WordCloud.Modal(id = paste0('WordCloudModal',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% c("ProbabilityPlot")) {
          Quantico:::Plots.ProbabilityPlot.Modal(id = paste0('ProbabilityPlotModal',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% c("Autocorrelation","PartialAutocorr")) {
          Quantico:::Plots.ACF.Modal(id = paste0('AutoCorr',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% c("StackedBarPlot","RiverPlot","ConfusionMatrix")) {
          Quantico:::Plots.AggRel.NoFacet.Modal(id = paste0('AggRelNF',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% c("BarPlot","AreaPlot","StepPlot","LinePlot","ScatterPlot","CopulaPlot")) {
          Quantico:::Plots.AggRel.Modal(id = paste0('AggRel',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% c("RadarPlot")) {
          Quantico:::Plots.Radar.Modal(id = paste0('RadarPlot',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% c("PartialDependenceLine")) {
          Quantico:::Plots.3D.Modal(id = paste0('ThreeD',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% c("ScatterPlot3D","CopulaPlot3D")) {
          Quantico:::Plots.3D.Modal.NoFacet(id = paste0('ThreeD',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% c("CalibrationLine","ConfusionMatrixHeatmap","ROCPlot","LiftPlot","GainsPlot","Residuals","ResidScatter")) {
          Quantico:::Plots.Eval.ZY.Modal(id = paste0('Eval.ZY',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% c("CalibrationBox")) {
          Quantico:::Plots.Eval.ZY.NoFacet.Modal(id = paste0('Eval.ZY.NoFacet',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% c("PartialDependenceBox")) {
          Quantico:::Plots.Eval.XYZ.NoGroup.Modal(id = paste0('Eval.XYZ.NoGroup',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% "VariableImportance") {
          Quantico:::Plots.Eval.VarImp.Modal(id = paste0('EvalVarImp',PlotNums,PlotType), PlotNumber = PlotNums)
        } else if(PlotType %in% "ShapleyImportance") {
          Quantico:::Plots.Eval.ShapImp.Modal(id = paste0('EvalShapImp',PlotNums,PlotType), PlotNumber = PlotNums)
        } else {
          Quantico:::Plots.Eval.Modal(id = paste0('Eval',PlotNums,PlotType), PlotNumber = PlotNums)
        }

        Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0('Plot',PlotNums,'_SelectData'), Label = 'Choose data set', Choices = names(DataList), Multiple = TRUE, MaxVars = 100, SelectedDefault = NULL, PlotDropDown = PlotDropDown, Key = key)

        # Experimenting with this code here as well as below
        if(length(PlotType) > 0L && !PlotType %in% paste0("Plot", 1L:36L)) {
          key <- paste0(PlotType, PlotNums)
          Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0('YVarTrans',PlotNums), Label = 'Y-Variable Transformation', Choices = c("Identity","Asinh","Log","LogPlus1","Sqrt","Asin","Logit","BoxCox","YeoJohnson"), Multiple = TRUE, MaxVars = 1, SelectedDefault = "Identity", PlotDropDown = PlotDropDown, Key = key)
          Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0('DualYVarTrans',PlotNums), Label = 'Dual Axis Y-Variable Transformation', Choices = c("Identity","Asinh","Log","LogPlus1","Sqrt","Asin","Logit","BoxCox","YeoJohnson"), Multiple = TRUE, MaxVars = 1, SelectedDefault = "Identity", PlotDropDown = PlotDropDown, Key = key)
          Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0('XVarTrans',PlotNums), Label = 'X-Variable Transformation', Choices = c("Identity","Asinh","Log","LogPlus1","Sqrt","Asin","Logit","BoxCox","YeoJohnson"), Multiple = TRUE, MaxVars = 1, SelectedDefault = "Identity", PlotDropDown = PlotDropDown, Key = key)
          Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0('ZVarTrans',PlotNums), Label = 'Z-Variable Transformation', Choices = c("Identity","Asinh","Log","LogPlus1","Sqrt","Asin","Logit","BoxCox","YeoJohnson"), Multiple = TRUE, MaxVars = 1, SelectedDefault = "Identity", PlotDropDown = PlotDropDown, Key = key)
          Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0('PlotDateAgg',PlotNums), Label = 'Date Aggregration Level', Choices = c("as-is","second","minute","hour","day","week","month","quarter","year"), Multiple = TRUE, MaxVars = 1, SelectedDefault = "as-is", PlotDropDown = PlotDropDown, Key = key)
          Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0('AggMethod',PlotNums), Choices = c('count','proportion','mean','meanabs','median','medianabs','sum','sumabs','sd','sdabs', 'skewness','skewnessabs', 'kurtosis','kurtosisabs','CoeffVar','CoeffVarabs'), SelectedDefault = 'mean', Label = "Aggregation Method", PlotDropDown = PlotDropDown, Key = key)
          Quantico:::SliderInput(session = session, Update = TRUE, input=input, InputID = paste0('FacetRows',PlotNums), Label = "Facet Rows", PlotDropDown = PlotDropDown, Key = key, Step = 1, Value = 1, Min = 1, Max = 12)
          Quantico:::SliderInput(session = session, Update = TRUE, input=input, InputID = paste0('FacetCols',PlotNums), Label = "Facet Columns", PlotDropDown = PlotDropDown, Key = key, Step = 1, Value = 1, Min = 1, Max = 12)
          Quantico:::TextInput(session = session, Update = TRUE, input=input, InputID = paste0('Title', PlotNums), Label='Rename Title', Value = NULL, PlotDropDown = PlotDropDown, Key = key)
          Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0('ShowLabels', PlotNums), Label='Show Labels', Choices = c(TRUE,FALSE), Multiple=TRUE, MaxVars = 1, SelectedDefault = FALSE, CloseAfterSelect = TRUE, PlotDropDown = PlotDropDown, Key = key)
          Quantico:::TextInput(session = session, Update = TRUE, input=input, InputID = paste0("YAxisTitle", PlotNums), Label = "Y-Axis Title ('None' for blank)", Value = NULL, PlotDropDown = PlotDropDown, Key = key)
          Quantico:::TextInput(session = session, Update = TRUE, input=input, InputID = paste0("XAxisTitle", PlotNums), Label = "X-Axis Title ('None' for blank)", Value = NULL, PlotDropDown = PlotDropDown, Key = key)
          Quantico:::NumericInput(session = session, Update = TRUE, input=input, InputID = paste0("PlottingMaxLags", PlotNums), Label = 'Max Lags', Value = 50, Min = 5, Max = 200, Step = 1)
          Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0("PlottingTimeUnit", PlotNums), Label = "Time Agg Level", Choices = c("hour", "day", "week", "month", "quarter", "year"), Multiple = TRUE, MaxVars = 1)

          # Reactive
          data <- shiny::reactive({tryCatch({DataList[[input[[paste0("Plot",shiny::req(PlotNums),"_SelectData")]]]][['data']]}, error = function(x) NULL)})
          shiny::observeEvent(data(), {
            ColNames <- tryCatch({DataList[[input[[paste0("Plot",PlotNums,"_SelectData")]]]]['colnames']}, error = function(x) {
              tryCatch({DataList[[1]]$colnames}, error = function(x) NULL)
            })
            ChoiceList <- list(); ChoiceListSelectize <- list()
            if(length(data()) > 0L) {
              Coltypes <- unique(Quantico:::ColTypes(data()))
              for(i in seq_along(Coltypes)) {
                ChoiceList[[Coltypes[i]]] <- Quantico:::ColNameFilter(data(), Types = Coltypes[i])
                ChoiceListSelectize[[Coltypes[i]]] <- as.list(Quantico:::ColNameFilter(data(), Types = Coltypes[i]))
              }
            }

            Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('YVar',shiny::isolate(PlotNums)), Label = 'Y-Variable', Choices = ChoiceList, Multiple = TRUE, SelectedDefault = if(length(input[[paste0('YVar',shiny::isolate(PlotNums))]]) > 0L) input[[paste0('YVar',shiny::isolate(PlotNums))]] else NULL)
            Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('DualYVar',shiny::isolate(PlotNums)), Label = 'Dual Axis Y-Variable', Choices = ChoiceList, Multiple = TRUE, SelectedDefault = if(length(input[[paste0('DualYVar',shiny::isolate(PlotNums))]]) > 0L) input[[paste0('DualYVar',shiny::isolate(PlotNums))]] else NULL)
            Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('XVar',shiny::isolate(PlotNums)), Label = 'X-Variable', Choices = ChoiceList, Multiple = TRUE, SelectedDefault = if(length(input[[paste0('XVar',shiny::isolate(PlotNums))]]) > 0L) input[[paste0('XVar',shiny::isolate(PlotNums))]] else NULL)
            Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('ZVar',shiny::isolate(PlotNums)), Label = 'Z-Variable', Choices = ChoiceList, Multiple = TRUE, SelectedDefault = if(length(input[[paste0('ZVar',shiny::isolate(PlotNums))]]) > 0L) input[[paste0('ZVar',shiny::isolate(PlotNums))]] else NULL)
            Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('PlottingDateVar',shiny::isolate(PlotNums)), Label = 'Date Variable', Choices = ChoiceList, Multiple = TRUE, SelectedDefault = if(length(input[[paste0('PlottingDateVar',shiny::isolate(PlotNums))]]) > 0L) input[[paste0('PlottingDateVar',shiny::isolate(PlotNums))]] else NULL)
            Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('GroupVars',shiny::isolate(PlotNums)), Label='Select Group Variables', Choices=ChoiceList, Multiple=TRUE, MaxVars = 3L, SelectedDefault = if(length(input[[paste0('GroupVars',shiny::isolate(PlotNums))]]) > 0L) input[[paste0('GroupVars',shiny::isolate(PlotNums))]] else NULL)
            Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterVariable_',shiny::isolate(PlotNums),'_1'), Label = 'Filter Variable 1', Choices = ChoiceList, SelectedDefault = if(length(input[[paste0('FilterVariable_',shiny::isolate(PlotNums),'_1')]]) > 0L) input[[paste0('FilterVariable_',shiny::isolate(PlotNums),'_1')]] else NULL, Multiple = TRUE, MaxVars = 2L)
            Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterVariable_',shiny::isolate(PlotNums),'_2'), Label = 'Filter Variable 2', Choices = ChoiceList, SelectedDefault = if(length(input[[paste0('FilterVariable_',shiny::isolate(PlotNums),'_2')]]) > 0L) input[[paste0('FilterVariable_',shiny::isolate(PlotNums),'_2')]] else NULL, Multiple = TRUE, MaxVars = 2L)
            Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterVariable_',shiny::isolate(PlotNums),'_3'), Label = 'Filter Variable 3', Choices = ChoiceList, SelectedDefault = if(length(input[[paste0('FilterVariable_',shiny::isolate(PlotNums),'_3')]]) > 0L) input[[paste0('FilterVariable_',shiny::isolate(PlotNums),'_3')]] else NULL, Multiple = TRUE, MaxVars = 2L)
            Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterVariable_',shiny::isolate(PlotNums),'_4'), Label = 'Filter Variable 4', Choices = ChoiceList, SelectedDefault = if(length(input[[paste0('FilterVariable_',shiny::isolate(PlotNums),'_4')]]) > 0L) input[[paste0('FilterVariable_',shiny::isolate(PlotNums),'_4')]] else NULL, Multiple = TRUE, MaxVars = 2L)
            Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterLogic_',shiny::isolate(PlotNums),'_1'), SelectedDefault = Quantico:::FL_Default(tryCatch({shiny::isolate(data())}, error = function(x) NULL), x=tryCatch({FilterVariable_1_1()}, error = function(x) NULL)), Label = 'Logical Operation', Choices = c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), Multiple = TRUE, MaxVars = 1, PlotDropDown = PlotDropDown, Key = key)
            Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterLogic_',shiny::isolate(PlotNums),'_2'), SelectedDefault = Quantico:::FL_Default(tryCatch({shiny::isolate(data())}, error = function(x) NULL), x=tryCatch({FilterVariable_1_2()}, error = function(x) NULL)), Label = 'Logical Operation', Choices = c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), Multiple = TRUE, MaxVars = 1, PlotDropDown = PlotDropDown, Key = key)
            Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterLogic_',shiny::isolate(PlotNums),'_3'), SelectedDefault = Quantico:::FL_Default(tryCatch({shiny::isolate(data())}, error = function(x) NULL), x=tryCatch({FilterVariable_1_3()}, error = function(x) NULL)), Label = 'Logical Operation', Choices = c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), Multiple = TRUE, MaxVars = 1, PlotDropDown = PlotDropDown, Key = key)
            Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterLogic_',shiny::isolate(PlotNums),'_4'), SelectedDefault = Quantico:::FL_Default(tryCatch({shiny::isolate(data())}, error = function(x) NULL), x=tryCatch({FilterVariable_1_4()}, error = function(x) NULL)), Label = 'Logical Operation', Choices = c('<','>','<=','>=','%in%','%like%','%between%','not %between%'), Multiple = TRUE, MaxVars = 1, PlotDropDown = PlotDropDown, Key = key)

            # Group Variables
            SelectedGroups1 <- shiny::reactive({input[[paste0('GroupVars',shiny::isolate(PlotNums))]]})
            shiny::observeEvent(SelectedGroups1(), {
              if(DebugPlottingOE) print(paste0("PlotDropDown ", shiny::isolate(PlotNums), " ObsEvent 3: Plot Variables: SelectedGroups Reactive ObsEvent a"))
              sgs <- tryCatch({sort(unique(DataList[[input[[paste0("Plot",shiny::isolate(PlotNums),"_SelectData")]]]][['data']][[shiny::isolate(SelectedGroups1())[1L]]]))}, error = function(x) NULL); if(Debug) {print('PickerInput_GetLevels 3.1')}
              if(Debug) {
                print("here 1")
                print(shiny::isolate(SelectedGroups1()))
                print(sgs)
              }
              if(length(shiny::isolate(SelectedGroups1())) == 0L) {
                Quantico:::PickerInput_GetLevels2(Update = TRUE, session = session, input=input, GroupVariable = NULL, InputID=paste0('Levels_',shiny::isolate(PlotNums),'_1'), Choices=NULL, SelectedDefault=NULL)
              } else {
                if(Debug) {
                  print("here 2.a")
                  print(input[[paste0('Levels_',shiny::isolate(PlotNums),'_1')]])
                  print("here 2.b")
                  print(tryCatch({PlotDropDown[[key]][[paste0('Levels_',shiny::isolate(PlotNums),'_1')]]}, error = function(x) print("Error in tryCatch")))
                }
                Quantico:::PickerInput_GetLevels2(Update = TRUE, session = session, input=input, GroupVariable = shiny::isolate(SelectedGroups1())[1L], InputID=paste0('Levels_',shiny::isolate(PlotNums),'_1'), Choices=sgs, SelectedDefault=NULL, PlotDropDown = PlotDropDown, Key = key)# if(length(input[[paste0('Levels_',shiny::isolate(PlotNums),'_1')]]) > 0L) input[[paste0('Levels_',shiny::isolate(PlotNums),'_1')]] else NULL)
              }

              if(Debug) {
                print("here 3")
                print(length(shiny::isolate(SelectedGroups1())) %in% c(0L,1L))
              }

              if(length(shiny::isolate(SelectedGroups1())) %in% c(0L,1L)) {
                Quantico:::PickerInput_GetLevels2(Update = TRUE, session = session, input=input, GroupVariable = NULL, InputID = paste0('Levels_',shiny::isolate(PlotNums),'_2'), Choices = NULL, SelectedDefault = NULL)
              } else {
                sgs <- tryCatch({sort(unique(DataList[[input[[paste0("Plot",shiny::isolate(PlotNums),"_SelectData")]]]][['data']][[shiny::isolate(SelectedGroups1())[2L]]]))}, error = function(x) NULL); if(Debug) {print('PickerInput_GetLevels 3.2')}
                Quantico:::PickerInput_GetLevels2(Update = TRUE, session = session, input=input, GroupVariable = shiny::isolate(SelectedGroups1())[2L], InputID=paste0('Levels_',shiny::isolate(PlotNums),'_2'), Choices=sgs, SelectedDefault=NULL, PlotDropDown = PlotDropDown, Key = key)#if(length(input[[paste0('Levels_',shiny::isolate(PlotNums),'_2')]]) > 0L) input[[paste0('Levels_',shiny::isolate(PlotNums),'_2')]] else NULL)
              }

              if(length(shiny::isolate(SelectedGroups1())) %in% c(0L:2L)) {
                Quantico:::PickerInput_GetLevels2(Update = TRUE, session = session, input=input, GroupVariable = NULL, InputID = paste0('Levels_',shiny::isolate(PlotNums),'_3'), Choices = NULL, SelectedDefault = NULL)
              } else {
                sgs <- tryCatch({sort(unique(DataList[[input[[paste0("Plot",shiny::isolate(PlotNums),"_SelectData")]]]][['data']][[shiny::isolate(SelectedGroups1())[3L]]]))}, error = function(x) NULL); if(Debug) {print('PickerInput_GetLevels 3.3')}
                Quantico:::PickerInput_GetLevels2(Update = TRUE, session = session, input=input, GroupVariable = shiny::isolate(SelectedGroups1())[3L], InputID=paste0('Levels_',shiny::isolate(PlotNums),'_3'), Choices=sgs, SelectedDefault=NULL, PlotDropDown = PlotDropDown, Key = key)# if(length(input[[paste0('Levels_',shiny::isolate(PlotNums),'_3')]]) > 0L) input[[paste0('Levels_',shiny::isolate(PlotNums),'_3')]] else NULL)
              }
            }, ignoreNULL = FALSE)

            # Filter Variables
            FilterVariable_1_1 <- shiny::reactive({tryCatch({input[[paste0('FilterVariable_',shiny::isolate(PlotNums),'_1')]]}, error = function(x) NULL)})
            shiny::observeEvent(FilterVariable_1_1(), {
              if(length(FilterVariable_1_1()) <= 1L) {
                if(Debug) print(paste0("PlotDropDown ", shiny::isolate(PlotNums), " ObsEvent 3: Filter Variables: FilterVariable_1_1() ObsEvent"))
                choices <- tryCatch({Quantico:::KeyVarsInit(tryCatch({shiny::isolate(data())}, error = function(x) NULL), VarName = shiny::isolate(FilterVariable_1_1()))$ChoiceInput}, error = function(x) NULL)
                Mult <- Quantico:::GetFilterValueMultiple(tryCatch({shiny::isolate(data())}, error = function(x) NULL), VarName = shiny::isolate(FilterVariable_1_1()))
                print("filter var 1")
                print(choices)
                if(length(choices) > 0L) {
                  print("here 1")
                  print(PlotDropDown[[key]][[paste0('FilterValue_',shiny::isolate(PlotNums),'_1_1')]])
                  Quantico:::PickerInput(session = session, Update = TRUE, input=input, Multiple = Mult, InputID = paste0('FilterValue_',shiny::isolate(PlotNums),'_1_1'), Label = 'Value 1', Choices = Quantico::CharNull(choices), SelectedDefault = NULL, PlotDropDown = PlotDropDown, Key = key)# if(length(input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_1_1')]]) > 0L) input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_1_1')]] else NULL)
                } else {
                  print("here 2")
                  Quantico:::PickerInput(session = session, Update = TRUE, input=input, Multiple = Mult, InputID = paste0('FilterValue_',shiny::isolate(PlotNums),'_1_1'), Label = 'Value 1', Choices = NULL, SelectedDefault = NULL)#if(length(input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_1_1')]]) > 0L) input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_1_1')]] else NULL)
                }
                choices <- tryCatch({Quantico:::KeyVarsInit(tryCatch({shiny::isolate(data())}, error = function(x) NULL), VarName = shiny::isolate(FilterVariable_1_1()))$ChoiceInput}, error = function(x) NULL)
                Mult <- Quantico:::GetFilterValueMultiple(tryCatch({shiny::isolate(data())}, error = function(x) NULL), VarName = shiny::isolate(FilterVariable_1_1()))
                if(length(choices) > 0L) {
                  Quantico:::PickerInput(session = session, Update = TRUE, input=input, Multiple=Mult, InputID = paste0('FilterValue_',shiny::isolate(PlotNums),'_1_2'), Label='Value 2', Choices=Quantico::CharNull(choices), SelectedDefault = NULL, PlotDropDown = PlotDropDown, Key = key)# if(length(input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_1_2')]]) > 0L) input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_1_2')]] else NULL)
                } else {
                  Quantico:::PickerInput(session = session, Update = TRUE, input=input, Multiple=Mult, InputID = paste0('FilterValue_',shiny::isolate(PlotNums),'_1_2'), Label='Value 2', Choices = NULL, SelectedDefault = NULL)#if(length(input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_1_2')]]) > 0L) input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_1_2')]] else NULL)
                }
              }
            }, ignoreNULL = FALSE)

            FilterVariable_1_2 <- shiny::reactive({tryCatch({input[[paste0('FilterVariable_',shiny::isolate(PlotNums),'_2')]]}, error = function(x) NULL)})
            shiny::observeEvent(FilterVariable_1_2(), {
              if(length(FilterVariable_1_2()) <= 1L) {
                if(Debug) print(paste0("PlotDropDown ", shiny::isolate(PlotNums), " ObsEvent 3: Filter Variables: FilterVariable_1_2() ObsEvent"))
                choices <- tryCatch({Quantico:::KeyVarsInit(tryCatch({shiny::isolate(data())}, error = function(x) NULL), VarName = shiny::isolate(FilterVariable_1_2()))$ChoiceInput}, error = function(x) NULL)
                Mult <- Quantico:::GetFilterValueMultiple(tryCatch({shiny::isolate(data())}, error = function(x) NULL), VarName = shiny::isolate(FilterVariable_1_2()))
                if(length(choices) > 0L) {
                  Quantico:::PickerInput(session = session, Update = TRUE, input=input, Multiple=Mult, InputID=paste0('FilterValue_',shiny::isolate(PlotNums),'_2_1'), Label='Value 1', Choices=Quantico::CharNull(choices), SelectedDefault = NULL, PlotDropDown = PlotDropDown, Key = key)# if(length(input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_2_1')]]) > 0L) input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_2_1')]] else NULL)
                } else {
                  Quantico:::PickerInput(session = session, Update = TRUE, input=input, Multiple=Mult, InputID=paste0('FilterValue_',shiny::isolate(PlotNums),'_2_1'), Label='Value 1', Choices=NULL, SelectedDefault = NULL) #if(length(input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_2_1')]]) > 0L) input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_2_1')]] else NULL)
                }
                choices <- tryCatch({Quantico:::KeyVarsInit(tryCatch({shiny::isolate(data())}, error = function(x) NULL), VarName = shiny::isolate(FilterVariable_1_2()))$ChoiceInput}, error = function(x) NULL)
                Mult <- Quantico:::GetFilterValueMultiple(tryCatch({dshiny::isolate(ata())}, error = function(x) NULL), VarName = shiny::isolate(FilterVariable_1_2()))
                if(length(choices) > 0L) {
                  Quantico:::PickerInput(session = session, Update = TRUE, input=input, Multiple=Mult, InputID=paste0('FilterValue_',shiny::isolate(PlotNums),'_2_2'), Label='Value 2', Choices=Quantico::CharNull(choices), SelectedDefault = NULL, PlotDropDown = PlotDropDown, Key = key) #if(length(input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_2_2')]]) > 0L) input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_2_2')]] else NULL)
                } else {
                  Quantico:::PickerInput(session = session, Update = TRUE, input=input, Multiple=Mult, InputID=paste0('FilterValue_',shiny::isolate(PlotNums),'_2_2'), Label='Value 2', Choices=NULL, SelectedDefault=NULL) #if(length(input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_2_2')]]) > 0L) input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_2_2')]] else NULL)
                }
              }
            }, ignoreNULL = FALSE)

            FilterVariable_1_3 <- shiny::reactive({tryCatch({input[[paste0('FilterVariable_',shiny::isolate(PlotNums),'_3')]]}, error = function(x) NULL)})
            shiny::observeEvent(FilterVariable_1_3(), {
              if(length(FilterVariable_1_3()) <= 1L) {
                if(Debug) print(paste0("PlotDropDown ", shiny::isolate(PlotNums), " ObsEvent 3: Filter Variables: FilterVariable_1_3() ObsEvent"))
                choices <- tryCatch({Quantico:::KeyVarsInit(tryCatch({shiny::isolate(data())}, error = function(x) NULL), VarName = shiny::isolate(FilterVariable_1_3()))$ChoiceInput}, error = function(x) NULL)
                Mult <- Quantico:::GetFilterValueMultiple(tryCatch({shiny::isolate(data())}, error = function(x) NULL), VarName = shiny::isolate(FilterVariable_1_3()))
                if(length(choices) > 0L) {
                  Quantico:::PickerInput(session = session, Update = TRUE, input=input, Multiple = Mult, InputID=paste0('FilterValue_',shiny::isolate(PlotNums),'_3_1'), Label='Value 1', Choices=Quantico::CharNull(choices), SelectedDefault = NULL, PlotDropDown = PlotDropDown, Key = key) #if(length(input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_3_1')]]) > 0L) input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_3_1')]] else NULL)
                } else {
                  Quantico:::PickerInput(session = session, Update = TRUE, input=input, Multiple = Mult, InputID=paste0('FilterValue_',shiny::isolate(PlotNums),'_3_1'), Label='Value 1', Choices = NULL, SelectedDefault = NULL) #if(length(input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_3_1')]]) > 0L) input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_3_1')]] else NULL)
                }
                choices <- tryCatch({Quantico:::KeyVarsInit(tryCatch({shiny::isolate(data())}, error = function(x) NULL), VarName = shiny::isolate(FilterVariable_1_3()))$ChoiceInput}, error = function(x) NULL)
                Mult <- Quantico:::GetFilterValueMultiple(tryCatch({shiny::isolate(data())}, error = function(x) NULL), VarName = shiny::isolate(FilterVariable_1_3()))
                if(length(choices) > 0L) {
                  Quantico:::PickerInput(session = session, Update = TRUE, input=input,Multiple = Mult, InputID=paste0('FilterValue_',shiny::isolate(PlotNums),'_3_2'), Label='Value 2', Choices=Quantico::CharNull(choices), SelectedDefault = NULL, PlotDropDown = PlotDropDown, Key = key)# if(length(input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_3_2')]]) > 0L) input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_3_2')]] else NULL)
                } else {
                  Quantico:::PickerInput(session = session, Update = TRUE, input=input,Multiple = Mult, InputID=paste0('FilterValue_',shiny::isolate(PlotNums),'_3_2'), Label='Value 2', Choices = NULL, SelectedDefault = NULL) #if(length(input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_3_2')]]) > 0L) input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_3_2')]] else NULL)
                }
              }
            }, ignoreNULL = FALSE)

            FilterVariable_1_4 <- shiny::reactive({tryCatch({input[[paste0('FilterVariable_',shiny::isolate(PlotNums),'_4')]]}, error = function(x) NULL)})
            shiny::observeEvent(FilterVariable_1_4(), {
              if(length(FilterVariable_1_4())) {
                if(Debug) print(paste0("PlotDropDown ", shiny::isolate(PlotNums), " ObsEvent 3: Filter Variables: FilterVariable_1_4() ObsEvent"))
                choices <- tryCatch({Quantico:::KeyVarsInit(tryCatch({shiny::isolate(data())}, error = function(x) NULL), VarName = shiny::isolate(FilterVariable_1_4()))$ChoiceInput}, error = function(x) NULL)
                Mult <- Quantico:::GetFilterValueMultiple(tryCatch({shiny::isolate(data())}, error = function(x) NULL), VarName = shiny::isolate(FilterVariable_1_4()))
                if(length(choices) > 0L) {
                  Quantico:::PickerInput(session = session, Update = TRUE, input=input, Multiple = Mult, MaxVars = 100, InputID=paste0('FilterValue_',shiny::isolate(PlotNums),'_4_1'), Label='Value 1', Choices=Quantico::CharNull(choices), SelectedDefault = NULL, PlotDropDown = PlotDropDown, Key = key)# if(length(input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_4_1')]]) > 0L) input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_4_1')]] else NULL)
                } else {
                  Quantico:::PickerInput(session = session, Update = TRUE, input=input, Multiple = Mult, MaxVars = 100, InputID=paste0('FilterValue_',shiny::isolate(PlotNums),'_4_1'), Label='Value 1', Choices = NULL, SelectedDefault = NULL) #if(length(input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_4_1')]]) > 0L) input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_4_1')]] else NULL)
                }
                choices <- tryCatch({Quantico:::KeyVarsInit(tryCatch({shiny::isolate(data())}, error = function(x) NULL), VarName = shiny::isolate(FilterVariable_1_4()))$ChoiceInput}, error = function(x) NULL)
                Mult <- Quantico:::GetFilterValueMultiple(tryCatch({shiny::isolate(data())}, error = function(x) NULL), VarName = shiny::isolate(FilterVariable_1_4()))
                if(length(choices) > 0L) {
                  Quantico:::PickerInput(session = session, Update = TRUE, input=input, Multiple = Mult, MaxVars = 100, InputID=paste0('FilterValue_',shiny::isolate(PlotNums),'_4_2'), Label='Value 2', Choices = Quantico::CharNull(choices), SelectedDefault = NULL, PlotDropDown = PlotDropDown, Key = key)#if(length(input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_4_2')]]) > 0L) input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_4_2')]] else NULL)
                } else {
                  Quantico:::PickerInput(session = session, Update = TRUE, input=input, Multiple = Mult, MaxVars = 100, InputID=paste0('FilterValue_',shiny::isolate(PlotNums),'_4_2'), Label='Value 2', Choices = NULL, SelectedDefault = NULL)#if(length(input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_4_2')]]) > 0L) input[[paste0('FilterValue_',shiny::isolate(PlotNums),'_4_2')]] else NULL)
                }
              }
            }, ignoreNULL = FALSE)
          })
        }
      }
    }, ignoreInit = TRUE)
  })
  PlotInitializeCheck <- FALSE
  PlotInitializeCheck <<- PlotInitializeCheck

  #                                      ----

  #               ----
  #                                      ----

  shiny::observe({shinyAce::updateAceEditor(session, editorId = "PrintCode", theme = input$editor_theme2)})

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event :: Data Wrangling       ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Sample Data
  shiny::observeEvent(input$DataWrangling_SampleData, {
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    Output <- tryCatch({Quantico:::Shiny.DW.SampleData(input,output,session,DataList,DataWranglingCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      DataList <- Output$DataList; DataList <<- DataList; DataWranglingCode <- Output$CodeList; DataWranglingCode <<- DataWranglingCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Subset Data
  shiny::observeEvent(input$DataWrangling_SubsetData, {
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    Output <- tryCatch({Quantico:::Shiny.DW.SubsetData(input,output,session,DataList,DataWranglingCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      DataList <- Output$DataList; DataList <<- DataList; DataWranglingCode <- Output$CodeList; DataWranglingCode <<- DataWranglingCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Aggregate Data
  shiny::observeEvent(input$DataWrangling_AggregateData, {
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    Output <- tryCatch({Quantico:::Shiny.DW.AggregateData(input,output,session,DataList,DataWranglingCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      DataList <- Output$DataList; DataList <<- DataList; DataWranglingCode <- Output$CodeList; DataWranglingCode <<- DataWranglingCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Join Data
  shiny::observeEvent(input$DataWrangling_JoinData, {
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    Output <- tryCatch({Quantico:::Shiny.DW.JoinData(input,output,session,DataList,DataWranglingCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      DataList <- Output$DataList; DataList <<- DataList; DataWranglingCode <- Output$CodeList; DataWranglingCode <<- DataWranglingCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Union Data
  shiny::observeEvent(input$DataWrangling_UnionData, {
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    Output <- tryCatch({Quantico:::Shiny.DW.UnionData(input,output,session,DataList,DataWranglingCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      DataList <- Output$DataList; DataList <<- DataList; DataWranglingCode <- Output$CodeList; DataWranglingCode <<- DataWranglingCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Melt Data
  shiny::observeEvent(input$DataWrangling_MeltData, {
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    Output <- tryCatch({Quantico:::Shiny.DW.MeltData(input,output,session,DataList,DataWranglingCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      DataList <- Output$DataList; DataList <<- DataList; DataWranglingCode <- Output$CodeList; DataWranglingCode <<- DataWranglingCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Cast Data
  shiny::observeEvent(input$DataWrangling_CastData, {
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    Output <- tryCatch({Quantico:::Shiny.DW.CastData(input,output,session,DataList,DataWranglingCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      DataList <- Output$DataList; DataList <<- DataList; DataWranglingCode <- Output$CodeList; DataWranglingCode <<- DataWranglingCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Remove Data
  shiny::observeEvent(input$DataWrangling_RemoveData, {
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    Output <- tryCatch({Quantico:::Shiny.DW.RemoveData(input,output,session,DataList,DataWranglingCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      DataList <- Output$DataList; DataList <<- DataList; DataWranglingCode <- Output$CodeList; DataWranglingCode <<- DataWranglingCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Time Series Fill
  shiny::observeEvent(input$DataWrangling_TimeSeriesFill, {
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    Output <- tryCatch({Quantico:::Shiny.DW.TimeSeriesFill(input,output,session,DataList,DataWranglingCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      DataList <- Output$DataList; DataList <<- DataList; DataWranglingCode <- Output$CodeList; DataWranglingCode <<- DataWranglingCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Time Series Roll
  shiny::observeEvent(input$DataWrangling_TimeSeriesRoll, {
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    Output <- tryCatch({Quantico:::Shiny.DW.TimeSeriesRoll(input,output,session,DataList,DataWranglingCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      DataList <- Output$DataList; DataList <<- DataList; DataWranglingCode <- Output$CodeList; DataWranglingCode <<- DataWranglingCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Delete Columns
  shiny::observeEvent(input$DataWranglingButton_DeleteColumns, {
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    Output <- tryCatch({Quantico:::Shiny.DW.DeleteColumns(input,output,session,DataList,DataWranglingCode,TabCount = NumTabs, CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      DataList <- Output$DataList; DataList <<- DataList; DataWranglingCode <- Output$CodeList; DataWranglingCode <<- DataWranglingCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Concat Columns
  shiny::observeEvent(input$DataWranglingButton_ConcatColumns, {
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    if(Debug) print('FeatureEngineeringButton_ConcatColumns')
    Output <- tryCatch({Quantico:::Shiny.DW.ConcatenateColumns(input,output,session,DataList,DataWranglingCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      DataList <- Output$DataList; DataList <<- DataList; DataWranglingCode <- Output$CodeList; DataWranglingCode <<- DataWranglingCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Meta Programming
  shiny::observeEvent(input$DataWranglingButton_MetaProgramming, {
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    if(Debug) print('DataWranglingButton_MetaProgramming')
    Output <- tryCatch({Quantico:::Shiny.DW.MetaProgramming(input,output,session,DataList,DataWranglingCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      DataList <- Output$DataList; DataList <<- DataList; DataWranglingCode <- Output$CodeList; DataWranglingCode <<- DataWranglingCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Sort Data
  shiny::observeEvent(input$DataWranglingButton_SortData, {
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    if(Debug) print('DataWranglingButton_SortData')
    Output <- tryCatch({Quantico:::Shiny.DW.SortData(input,output,session,DataList,DataWranglingCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      DataList <- Output$DataList; DataList <<- DataList; DataWranglingCode <- Output$CodeList; DataWranglingCode <<- DataWranglingCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Rename Columns
  shiny::observeEvent(input$DataWranglingButton_RenameColumns, {
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    if(Debug) print('DataWranglingButton_RenameColumns')
    Output <- tryCatch({Quantico:::Shiny.DW.RenameColumns(input,output,session,DataList,DataWranglingCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      DataList <- Output$DataList; DataList <<- DataList; DataWranglingCode <- Output$CodeList; DataWranglingCode <<- DataWranglingCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Time Trend Column
  shiny::observeEvent(input$DataWranglingButton_TimeTrendColumn, {
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    if(Debug) print('DataWranglingButton_TimeTrendColumn')
    Output <- tryCatch({Quantico:::Shiny.DW.TimeTrendColumn(input,output,session,DataList,DataWranglingCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      DataList <- Output$DataList; DataList <<- DataList; DataWranglingCode <- Output$CodeList; DataWranglingCode <<- DataWranglingCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Type Casting Column
  shiny::observeEvent(input$DataWranglingButton_TypeCasting, {
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    if(Debug) print('DataWranglingButton_TimeTrendColumn')
    Output <- tryCatch({Quantico:::Shiny.DW.TypeCast(input,output,session,DataList,DataWranglingCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      DataList <- Output$DataList; DataList <<- DataList; DataWranglingCode <- Output$CodeList; DataWranglingCode <<- DataWranglingCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # CalendarVariables()
  shiny::observeEvent(input$FeatureEngineeringButton_CalendarVariables, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    if(Debug) {
      print('FeatureEngineeringButton_CalendarVariables')
      Output <- Quantico:::Shiny.FE.Date.Calendar(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)
    } else {
      Output <- tryCatch({Quantico:::Shiny.FE.Date.Calendar(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    }

    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # HolidayVariables()
  shiny::observeEvent(input$FeatureEngineeringButton_HolidayVariables, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    if(Debug) print('FeatureEngineeringButton_HolidayVariables')
    Output <- tryCatch({Quantico:::Shiny.FE.Date.Holiday(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # PercentRank()
  shiny::observeEvent(input$FeatureEngineeringButton_PercRank, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    Output <- tryCatch({Quantico:::Shiny.FE.Numeric.PercentRank(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Standardize()
  shiny::observeEvent(input$FeatureEngineeringButton_Standardize, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    Output <- tryCatch({Quantico:::Shiny.FE.Numeric.Standardize(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # AutoInteraction()
  shiny::observeEvent(input$FeatureEngineeringButton_Interaction, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    Output <- tryCatch({Quantico:::Shiny.FE.Numeric.Interactions(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # AutoTransformationCreate()
  shiny::observeEvent(input$FeatureEngineeringButton_Transformations, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    Output <- tryCatch({Quantico:::Shiny.FE.Numeric.Transformations(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # DummifyDT()
  shiny::observeEvent(input$FeatureEngineeringButton_PartialDummies, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    Output <- tryCatch({Quantico:::Shiny.FE.Categorical.Dummify(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # CategoricalEncoding()
  shiny::observeEvent(input$FeatureEngineeringButton_CategoricalEncoding, {
    Output <- tryCatch({Quantico:::Shiny.FE.CrossRow.CategoricalEncoding(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # AutoLagRollMode()
  shiny::observeEvent(input$FeatureEngineeringButton_AutoLagRollMode, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    Output <- tryCatch({Quantico:::Shiny.FE.CrossRow.RollingMode(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # AutoLagRollStats()
  shiny::observeEvent(input$FeatureEngineeringButton_AutoLagRollStats, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    Output <- tryCatch({Quantico:::Shiny.FE.CrossRow.RollingStats(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # AutoDiffLagN()
  shiny::observeEvent(input$FeatureEngineeringButton_AutoDiff, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    Output <- tryCatch({Quantico:::Shiny.FE.CrossRow.Differencing(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # ModelDataPrep()
  shiny::observeEvent(input$FeatureEngineeringButton_ModelDataPrep, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    Output <- tryCatch({Quantico:::Shiny.FE.ModelDataPrep(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # AutoDataPartition()
  shiny::observeEvent(input$FeatureEngineeringButton_AutoDataPartition, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    Output <- tryCatch({Quantico:::Shiny.FE.PartitionData(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # AutoEncoder_H2O()
  shiny::observeEvent(input$FeatureEngineeringButton_AutoEncoder_H2O, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    Output <- tryCatch({Quantico:::Shiny.FE.DimReduction.AutoEncoder.H2O(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # IsolationForest_H2O()
  shiny::observeEvent(input$FeatureEngineeringButton_IsolationForest_H2O, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    Output <- tryCatch({Quantico:::Shiny.FE.AnomalyDetection.IsolationForest.H2O(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Kmeans_H2O()
  shiny::observeEvent(input$FeatureEngineeringButton_Kmeans_H2O, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    Output <- tryCatch({Quantico:::Shiny.FE.Clustering.Kmeans.H2O(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Word2Vec_H2O()
  shiny::observeEvent(input$FeatureEngineeringButton_Word2Vec_H2O, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    Output <- tryCatch({Quantico:::Shiny.FE.Word2Vec.H2O(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # TextSummary()
  shiny::observeEvent(input$NLPButton_TextSummary, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    if(Debug) {
      Output <- Quantico:::Shiny.NLP.TextSummary(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)
    } else {
      Output <- tryCatch({Quantico:::Shiny.NLP.TextSummary(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    }
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Sentiment()
  shiny::observeEvent(input$NLPButton_Sentiment, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    if(Debug) {
      Output <- Quantico:::Shiny.NLP.Sentiment(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)
    } else {
      Output <- tryCatch({Quantico:::Shiny.NLP.Sentiment(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    }
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Readability()
  shiny::observeEvent(input$NLPButton_Readability, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    if(Debug) {
      Output <- Quantico:::Shiny.NLP.Readability(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)
    } else {
      Output <- tryCatch({Quantico:::Shiny.NLP.Readability(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    }
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # LexicalDiversity()
  shiny::observeEvent(input$NLPButton_LexicalDiversity, {
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    if(Debug) {
      Output <- Quantico:::Shiny.NLP.LexicalDiversity(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)
    } else {
      Output <- tryCatch({Quantico:::Shiny.NLP.LexicalDiversity(input,output,session,DataList,FeatureEngineeringCode,CacheDir=CacheDir,CacheName=CacheName,Debug=Debug)}, error = function(x) NULL)
    }
    if(length(Output) > 0L) {
      FeatureEngineeringCode <- Output$CodeList; FeatureEngineeringCode <<- FeatureEngineeringCode; DataList <- Output$DataList; DataList <<- DataList
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "NULL Result", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event :: Inference            ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Normality Execution
  shiny::observeEvent(input$Inference_Normality_Execute, {

    # Args
    temp <- Quantico::ReturnParam(xx = tryCatch({input$Normality_SelectData}, error = function(x) NULL), Type = "character", Default = NULL)
    if(length(temp) > 0L) {

      EchartsTheme <- Quantico:::ReturnParam(xx = input[["EchartsTheme"]], Type = "character", Default = "dark")
      FontColorData <- Quantico:::rgba2hex(Quantico:::ReturnParam(xx = input[["ColorFont"]], Type = "character", Default = "#e2e2e2"))

      PlotWidthINF <- Quantico:::ReturnParam(xx = input[["PlotWidth1"]], Type = "numeric", Default = 1450)
      PlotWidthINF <- paste0(PlotWidthINF, "px")
      PlotHeightINF <- Quantico:::ReturnParam(xx = input[["PlotHeight1"]], Type = "numeric", Default = 860)
      PlotHeightINF <- paste0(PlotHeightINF, "px")

      Normality_SelectData <- DataList[[temp]][['data']]
      Normality_YVars <- Quantico::ReturnParam(xx = tryCatch({input$Normality_YVars}, error = function(x) NULL), Type = "character", Default = NULL)
      Normality_InferenceID <- Quantico::ReturnParam(xx = tryCatch({input$Normality_InferenceID}, error = function(x) NULL), Type = "character", Default = "INF_Normality1")
      SampleSize.ADT <- Quantico::ReturnParam(xx = tryCatch({input$SampleSize.ADT}, error = function(x) NULL), Type = "character", Default = 1000000)
      Samples.ADT <- Quantico::ReturnParam(xx = tryCatch({input$Samples.ADT}, error = function(x) NULL), Type = "character", Default = 1)
      SampleSize.CVMT <- Quantico::ReturnParam(xx = tryCatch({input$SampleSize.CVMT}, error = function(x) NULL), Type = "character", Default = 1000000)
      Samples.CVMT <- Quantico::ReturnParam(xx = tryCatch({input$Samples.CVMT}, error = function(x) NULL), Type = "character", Default = 1)
      SampleSize.KST <- Quantico::ReturnParam(xx = tryCatch({input$SampleSize.KST}, error = function(x) NULL), Type = "character", Default = 1000000)
      Samples.KST <- Quantico::ReturnParam(xx = tryCatch({input$Samples.KST}, error = function(x) NULL), Type = "character", Default = 1)
      SampleSize.ST <- Quantico::ReturnParam(xx = tryCatch({input$SampleSize.ST}, error = function(x) NULL), Type = "character", Default = 5000)
      Samples.ST <- Quantico::ReturnParam(xx = tryCatch({input$Samples.ST}, error = function(x) NULL), Type = "character", Default = 1)
      SampleSize.JBT <- Quantico::ReturnParam(xx = tryCatch({input$SampleSize.JBT}, error = function(x) NULL), Type = "character", Default = 1000000)
      Samples.JBT <- Quantico::ReturnParam(xx = tryCatch({input$Samples.JBT}, error = function(x) NULL), Type = "character", Default = 1)
      SampleSize.AT <- Quantico::ReturnParam(xx = tryCatch({input$SampleSize.AT}, error = function(x) NULL), Type = "character", Default = 46340)
      Samples.AT <- Quantico::ReturnParam(xx = tryCatch({input$Samples.AT}, error = function(x) NULL), Type = "character", Default = 1)

      # Run function
      if(Debug) print("inference 0")
      if(!exists("InferenceOutputList")) InferenceOutputList <- list()
      InferenceOutputList[[Normality_InferenceID]] <- Quantico::Normality.Analysis(
        dt = Normality_SelectData,
        YVars = Normality_YVars,
        EchartsTheme = EchartsTheme,
        TextColor = FontColorData$flv,
        PlotHeight = PlotHeightINF,
        PlotWidth = PlotWidthINF,
        SampleSize.ADT = SampleSize.ADT,
        Samples.ADT = Samples.ADT,
        SampleSize.CVMT = SampleSize.CVMT,
        Samples.CVMT = Samples.CVMT,
        SampleSize.KST = SampleSize.KST,
        Samples.KST = Samples.KST,
        SampleSize.ST = SampleSize.ST,
        Samples.ST = Samples.ST,
        SampleSize.JBT = SampleSize.JBT,
        Samples.JBT = Samples.JBT,
        SampleSize.AT = SampleSize.AT,
        Samples.AT = Samples.AT)

      if(Debug) print("inference 1")

      MachineLearningCode <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = MachineLearningCode, x = paste0(
        "\n",
        "# Normality Testing\n",
        "Normality_SelectData <- DataList[[", Quantico:::CEP(temp), "]][['data']]\n",
        "Output <- Quantico::Normality.Analysis(, \n  ",
        "dt = Normality_SelectData, \n  ",
        "YVars = ", Quantico:::ExpandText(Normality_YVars), ",\n  ",
        "EchartsTheme = ", Quantico:::CEP(EchartsTheme), ",\n  ",
        "TextColor = ", Quantico:::CEP(FontColorData$flv), ",\n  ",
        "PlotHeight = ", Quantico:::CEP(PlotHeightINF), ",\n  ",
        "PlotWidth = ", Quantico:::CEP(PlotWidthINF), ",\n  ",
        "SampleSize.ADT = ", Quantico:::CEPP(SampleSize.ADT), ",\n  ",
        "Samples.ADT = ", Quantico:::CEPP(Samples.ADT), ",\n  ",
        "SampleSize.CVMT = ", Quantico:::CEPP(SampleSize.CVMT), ",\n  ",
        "Samples.CVMT = ", Quantico:::CEPP(Samples.CVMT), ",\n  ",
        "SampleSize.KST = ", Quantico:::CEPP(SampleSize.KST), ",\n  ",
        "Samples.KST = ", Quantico:::CEPP(Samples.KST), ",\n  ",
        "SampleSize.ST = ", Quantico:::CEPP(SampleSize.ST), ",\n  ",
        "Samples.ST = ", Quantico:::CEPP(Samples.ST), ",\n  ",
        "SampleSize.JBT = ", Quantico:::CEPP(SampleSize.JBT), ",\n  ",
        "Samples.JBT = ", Quantico:::CEPP(Samples.JBT), ",\n  ",
        "SampleSize.AT = ", Quantico:::CEPP(SampleSize.AT), ",\n  ",
        "Samples.AT = Samples.AT)\n"))}, error = function(x) MachineLearningCode)

      # Update Available Outputs for Inference Tab
      if(Debug) print("inference 2")
      for(i in seq_len(NumTabs)) Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0('InferenceReportsModelSelection',i), Label = 'Testing Output', Choices = tryCatch({names(InferenceOutputList)}, error = function(x) NULL), Multiple = FALSE, MaxVars = 1L)
      InferenceOutputList <<- InferenceOutputList
      MachineLearningCode <<- MachineLearningCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = "", type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = "Data not available", type = NULL, btn_labels = "Error", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }

  }, ignoreInit = TRUE)

  # Correlation Execution
  shiny::observeEvent(input$Inference_Correlation_Execute, {

    # Args
    temp <- Quantico::ReturnParam(xx = tryCatch({input$Correlation_SelectData}, error = function(x) NULL), Type = "character", Default = NULL)
    if(length(temp) > 0L) {

      Correlation_EchartsTheme <- Quantico:::ReturnParam(xx = input[["EchartsTheme"]], Type = "character", Default = "dark")
      Correlation_FontColorData <- Quantico:::rgba2hex(Quantico:::ReturnParam(xx = input[["ColorFont"]], Type = "character", Default = "#e2e2e2"))

      Correlation_PlotWidthINF <- Quantico:::ReturnParam(xx = input[["PlotWidthinf"]], Type = "numeric", Default = 1450)
      Correlation_PlotWidthINF <- paste0(Correlation_PlotWidthINF, "px")
      Correlation_PlotHeightINF <- Quantico:::ReturnParam(xx = input[["PlotHeightinf"]], Type = "numeric", Default = 860)
      Correlation_PlotHeightINF <- paste0(Correlation_PlotHeightINF, "px")

      Correlation_SelectData <- DataList[[temp]][['data']]
      Correlation_CorrVars <- Quantico::ReturnParam(xx = tryCatch({input$Correlation_CorrVars}, error = function(x) NULL), Type = "character", Default = NULL)
      Correlation_InferenceID <- Quantico::ReturnParam(xx = tryCatch({input$Correlation_InferenceID}, error = function(x) NULL), Type = "character", Default = "INF_Corr1")
      Correlation_DateVar <- Quantico::ReturnParam(xx = tryCatch({input$Correlation_DateVar}, error = function(x) NULL), Type = "character", Default = NULL)
      Correlation_SampleSize <- Quantico::ReturnParam(xx = tryCatch({input$Correlation_SampleSize}, error = function(x) NULL), Type = "numeric", Default = 10000)
      Correlation_EchartsTheme <- Quantico::ReturnParam(xx = tryCatch({input$EchartsTheme}, error = function(x) NULL), Type = "character", Default = "macarons")
      Correlation_P_Adjust <- Quantico::ReturnParam(xx = tryCatch({input$Correlation_P_Adjust}, error = function(x) NULL), Type = "character", Default = NULL)
      Correlation_Bayesian <- Quantico::ReturnParam(xx = tryCatch({input$Correlation_Bayesian}, error = function(x) NULL), Type = "logical", Default = FALSE)
      Correlation_Bayesian_Prior <- Quantico::ReturnParam(xx = tryCatch({input$Correlation_Bayesian_Prior}, error = function(x) NULL), Type = "character", Default = NULL)
      Correlation_MultiLevel <- Quantico::ReturnParam(xx = tryCatch({input$Correlation_MultiLevel}, error = function(x) NULL), Type = "logical", Default = FALSE)
      Correlation_Include_Factors <- Quantico::ReturnParam(xx = tryCatch({input$Correlation_Include_Factors}, error = function(x) NULL), Type = "logical", Default = FALSE)
      Correlation_Partial <- Quantico::ReturnParam(xx = tryCatch({input$Correlation_Partial}, error = function(x) NULL), Type = "logical", Default = FALSE)
      Correlation_Partial_Bayesian <- Quantico::ReturnParam(xx = tryCatch({input$Correlation_Partial_Bayesian}, error = function(x) NULL), Type = "logical", Default = FALSE)

      # Run function
      if(Debug) print("inference 0")
      if(!exists("InferenceOutputList")) InferenceOutputList <- list()
      InferenceOutputList[[Correlation_InferenceID]] <- Quantico::Correlation.Analysis(
        dt = Correlation_SelectData,
        CorrVars = Correlation_CorrVars,
        DateVar = Correlation_DateVar,
        EchartsTheme = Correlation_EchartsTheme,
        TextColor = Correlation_FontColorData$flv,
        PlotHeight = Correlation_PlotHeightINF,
        PlotWidth = Correlation_PlotWidthINF,
        P_Adjust = Correlation_P_Adjust,
        Bayesian = Correlation_Bayesian,
        Bayesian_Prior = Correlation_Bayesian_Prior,
        MultiLevel = Correlation_MultiLevel,
        Include_Factors = Correlation_Include_Factors,
        Partial = Correlation_Partial,
        Partial_Bayesian = Correlation_Partial_Bayesian)

      if(Debug) print("inference 1")

      MachineLearningCode <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = MachineLearningCode, x = paste0(
        "\n",
        "# Normality Testing\n",
        "Normality_SelectData <- DataList[[", Quantico:::CEP(temp), "]][['data']]\n",
        "Output <- Quantico::Correlation.Analysis(, \n  ",
        "dt = Normality_SelectData, \n  ",
        "CorrVars = ", Quantico:::ExpandText(Correlation_CorrVars), ",\n  ",
        "DateVar = ", Quantico:::ExpandText(Correlation_DateVar), ",\n  ",
        "EchartsTheme = ", Quantico:::CEP(Correlation_EchartsTheme), ",\n  ",
        "TextColor = ", Quantico:::CEP(Correlation_FontColorData$flv), ",\n  ",
        "PlotHeight = ", Quantico:::CEP(Correlation_PlotHeightINF), ",\n  ",
        "PlotWidth = ", Quantico:::CEP(Correlation_PlotWidthINF), ",\n  ",
        "P_Adjust = ", Quantico:::CEP(Correlation_P_Adjust), ",\n  ",
        "Bayesian = ", Quantico:::CEP(Correlation_Bayesian), ",\n  ",
        "Bayesian_Prior = ", Quantico:::CEP(Correlation_Bayesian_Prior), ",\n  ",
        "MultiLevel = ", Quantico:::CEP(Correlation_MultiLevel), ",\n  ",
        "Include_Factors = ", Quantico:::CEP(Correlation_Include_Factors), ",\n  ",
        "Partial = ", Quantico:::CEP(Correlation_Partial), ",\n  ",
        "Partial_Bayesian = ", Quantico:::CEP(Correlation_Partial_Bayesian), ")\n"))}, error = function(x) MachineLearningCode)

      # Update Available Outputs for Inference Tab
      if(Debug) print("inference 2")
      for(i in seq_len(NumTabs)) Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0('InferenceReportsModelSelection',i), Label = 'Testing Output', Choices = tryCatch({names(InferenceOutputList)}, error = function(x) NULL), Multiple = FALSE, MaxVars = 1L)
      InferenceOutputList <<- InferenceOutputList
      MachineLearningCode <<- MachineLearningCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = "", type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = "Data not available", type = NULL, btn_labels = "Error", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }

  }, ignoreInit = TRUE)

  # One Sample TTest Execution
  shiny::observeEvent(input$Inference_1STTest_Execute, {

    # Args
    temp <- Quantico::ReturnParam(xx = tryCatch({input$OneSampleTTest_SelectData}, error = function(x) NULL), Type = "character", Default = NULL)
    if(length(temp) > 0L) {

      OneSampleTTest_EchartsTheme <- Quantico::ReturnParam(xx = tryCatch({input$EchartsTheme}, error = function(x) NULL), Type = "character", Default = "macarons")
      OneSampleTTest_FontColorData <- Quantico:::rgba2hex(Quantico:::ReturnParam(xx = input[["ColorFont"]], Type = "character", Default = "#e2e2e2"))
      OneSampleTTest_PlotWidthINF <- Quantico:::ReturnParam(xx = input[["PlotWidthinf"]], Type = "numeric", Default = 1450)
      OneSampleTTest_PlotWidthINF <- paste0(OneSampleTTest_PlotWidthINF, "px")
      OneSampleTTest_PlotHeightINF <- Quantico:::ReturnParam(xx = input[["PlotHeightinf"]], Type = "numeric", Default = 860)
      OneSampleTTest_PlotHeightINF <- paste0(OneSampleTTest_PlotHeightINF, "px")

      OneSampleTTest_SelectData <- DataList[[temp]][['data']]
      OneSampleTTest_Variable <- Quantico::ReturnParam(xx = tryCatch({input$OneSampleTTest_Variable}, error = function(x) NULL), Type = "character", Default = NULL)
      OneSampleTTest_InferenceID <- Quantico::ReturnParam(xx = tryCatch({input$OneSampleTTest_InferenceID}, error = function(x) NULL), Type = "character", Default = "INF_1STTest1")

      OneSampleTTest_SampleSize <- Quantico::ReturnParam(xx = tryCatch({input$OneSampleTTest_SampleSize}, error = function(x) NULL), Type = "numeric", Default = 100000)
      OneSampleTTest_Samples <- Quantico::ReturnParam(xx = tryCatch({input$OneSampleTTest_Samples}, error = function(x) NULL), Type = "numeric", Default = 1)
      OneSampleTTest_NullValue <- Quantico::ReturnParam(xx = tryCatch({input$OneSampleTTest_NullValue}, error = function(x) NULL), Type = "numeric", Default = 0)
      OneSampleTTest_Alternative <- Quantico::ReturnParam(xx = tryCatch({input$OneSampleTTest_Alternative}, error = function(x) NULL), Type = "character", Default = 1)
      OneSampleTTest_ConfidenceLevel <- Quantico::ReturnParam(xx = tryCatch({input$OneSampleTTest_ConfidenceLevel}, error = function(x) NULL), Type = "numeric", Default = 0.95)


      # Run function
      if(Debug) print("inference 0")
      if(!exists("InferenceOutputList")) InferenceOutputList <- list()
      InferenceOutputList[[OneSampleTTest_InferenceID]] <- Quantico::One.Sample.TTest(
        dt = OneSampleTTest_SelectData,
        Variable = OneSampleTTest_Variable,
        NullValue = OneSampleTTest_NullValue,
        Alternative = OneSampleTTest_Alternative,
        ConfidenceLevel = OneSampleTTest_ConfidenceLevel,
        SampleSize = OneSampleTTest_SampleSize,
        Samples = OneSampleTTest_Samples,
        EchartsTheme = OneSampleTTest_EchartsTheme,
        TextColor = OneSampleTTest_FontColorData$flv,
        PlotHeight = OneSampleTTest_PlotHeightINF,
        PlotWidth = OneSampleTTest_PlotWidthINF)

      if(Debug) print("inference 1")

      MachineLearningCode <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = MachineLearningCode, x = paste0(
        "\n",
        "# Normality Testing\n",
        "OneSampleTTest_SelectData <- DataList[[", Quantico:::CEP(temp), "]][['data']]\n",
        "Output <- Quantico::One.Sample.TTest(, \n  ",
        "dt = OneSampleTTest_SelectData, \n  ",
        "Variable = ", Quantico:::ExpandText(OneSampleTTest_Variable), ",\n  ",
        "EchartsTheme = ", Quantico:::CEP(OneSampleTTest_EchartsTheme), ",\n  ",
        "TextColor = ", Quantico:::CEP(OneSampleTTest_FontColorData$flv), ",\n  ",
        "PlotHeight = ", Quantico:::CEP(OneSampleTTest_PlotHeightINF), ",\n  ",
        "PlotWidth = ", Quantico:::CEP(OneSampleTTest_PlotWidthINF), ",\n  ",
        "NullValue = ", Quantico:::CEPP(OneSampleTTest_NullValue), ",\n  ",
        "Alternative = ", Quantico:::CEP(OneSampleTTest_Alternative), ",\n  ",
        "ConfidenceLevel = ", Quantico:::CEPP(OneSampleTTest_ConfidenceLevel), ",\n  ",
        "SampleSize = ", Quantico:::CEP(OneSampleTTest_SampleSize), ",\n  ",
        "Samples = ", Quantico:::CEP(OneSampleTTest_Samples), ")\n"))}, error = function(x) MachineLearningCode)

      # Update Available Outputs for Inference Tab
      if(Debug) print("inference 2")
      for(i in seq_len(NumTabs)) Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0('InferenceReportsModelSelection',i), Label = 'Testing Output', Choices = tryCatch({names(InferenceOutputList)}, error = function(x) NULL), Multiple = FALSE, MaxVars = 1L)
      InferenceOutputList <<- InferenceOutputList
      MachineLearningCode <<- MachineLearningCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = "", type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = "Data not available", type = NULL, btn_labels = "Error", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }

  }, ignoreInit = TRUE)

  # Two Sample TTest Execution
  shiny::observeEvent(input$Inference_2STTest_Execute, {

    # Args
    temp <- Quantico::ReturnParam(xx = tryCatch({input$TwoSampleTTest_SelectData}, error = function(x) NULL), Type = "character", Default = NULL)
    if(length(temp) > 0L) {

      TwoSampleTTest_EchartsTheme <- Quantico::ReturnParam(xx = tryCatch({input$EchartsTheme}, error = function(x) NULL), Type = "character", Default = "macarons")
      TwoSampleTTest_FontColorData <- Quantico:::rgba2hex(Quantico:::ReturnParam(xx = input[["ColorFont"]], Type = "character", Default = "#e2e2e2"))
      TwoSampleTTest_PlotWidthINF <- Quantico:::ReturnParam(xx = input[["PlotWidthinf"]], Type = "numeric", Default = 1450)
      TwoSampleTTest_PlotWidthINF <- paste0(TwoSampleTTest_PlotWidthINF, "px")
      TwoSampleTTest_PlotHeightINF <- Quantico:::ReturnParam(xx = input[["PlotHeightinf"]], Type = "numeric", Default = 860)
      TwoSampleTTest_PlotHeightINF <- paste0(TwoSampleTTest_PlotHeightINF, "px")

      TwoSampleTTest_SelectData <- DataList[[temp]][['data']]
      TwoSampleTTest_Variable1 <- Quantico::ReturnParam(xx = tryCatch({input$TwoSampleTTest_Variable1}, error = function(x) NULL), Type = "character", Default = NULL)
      TwoSampleTTest_Variable2 <- Quantico::ReturnParam(xx = tryCatch({input$TwoSampleTTest_Variable2}, error = function(x) NULL), Type = "character", Default = NULL)
      TwoSampleTTest_InferenceID <- Quantico::ReturnParam(xx = tryCatch({input$TwoSampleTTest_InferenceID}, error = function(x) NULL), Type = "character", Default = "INF_1STTest1")

      TwoSampleTTest_SampleSize <- Quantico::ReturnParam(xx = tryCatch({input$TwoSampleTTest_SampleSize}, error = function(x) NULL), Type = "numeric", Default = 100000)
      TwoSampleTTest_Samples <- Quantico::ReturnParam(xx = tryCatch({input$TwoSampleTTest_Samples}, error = function(x) NULL), Type = "numeric", Default = 1)
      TwoSampleTTest_NullValue <- Quantico::ReturnParam(xx = tryCatch({input$TwoSampleTTest_NullValue}, error = function(x) NULL), Type = "numeric", Default = 0)
      TwoSampleTTest_Paired <- Quantico::ReturnParam(xx = tryCatch({input$TwoSampleTTest_Paired}, error = function(x) NULL), Type = "logical", Default = FALSE)
      TwoSampleTTest_EqualVariance <- Quantico::ReturnParam(xx = tryCatch({input$TwoSampleTTest_EqualVariance}, error = function(x) NULL), Type = "logical", Default = FALSE)
      TwoSampleTTest_Alternative <- Quantico::ReturnParam(xx = tryCatch({input$TwoSampleTTest_Alternative}, error = function(x) NULL), Type = "character", Default = 1)
      TwoSampleTTest_ConfidenceLevel <- Quantico::ReturnParam(xx = tryCatch({input$TwoSampleTTest_ConfidenceLevel}, error = function(x) NULL), Type = "numeric", Default = 0.95)


      # Run function
      if(Debug) print("inference 0")
      if(!exists("InferenceOutputList")) InferenceOutputList <- list()
      InferenceOutputList[[TwoSampleTTest_InferenceID]] <- Quantico::Two.Sample.TTest(
        dt = TwoSampleTTest_SelectData,
        Variable1 = TwoSampleTTest_Variable1,
        Variable2 = TwoSampleTTest_Variable2,
        MeanDifference = TwoSampleTTest_NullValue,
        Paired = TwoSampleTTest_Paired,
        EqualVariance = TwoSampleTTest_EqualVariance,
        Alternative = TwoSampleTTest_Alternative,
        ConfidenceLevel = TwoSampleTTest_ConfidenceLevel,
        SampleSize = TwoSampleTTest_SampleSize,
        Samples = TwoSampleTTest_Samples,
        EchartsTheme = TwoSampleTTest_EchartsTheme,
        TextColor = TwoSampleTTest_FontColorData$flv,
        PlotHeight = TwoSampleTTest_PlotHeightINF,
        PlotWidth = TwoSampleTTest_PlotWidthINF)

      if(Debug) print("inference 1")

      MachineLearningCode <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = MachineLearningCode, x = paste0(
        "\n",
        "# Normality Testing\n",
        "TwoSampleTTest_SelectData <- DataList[[", Quantico:::CEP(temp), "]][['data']]\n",
        "Output <- Quantico::Two.Sample.TTest(, \n  ",
        "dt = TwoSampleTTest_SelectData, \n  ",
        "Variable1 = ", Quantico:::ExpandText(TwoSampleTTest_Variable1), ",\n  ",
        "Variable2 = ", Quantico:::ExpandText(TwoSampleTTest_Variable2), ",\n  ",
        "EchartsTheme = ", Quantico:::CEP(TwoSampleTTest_EchartsTheme), ",\n  ",
        "TextColor = ", Quantico:::CEP(TwoSampleTTest_FontColorData$flv), ",\n  ",
        "PlotHeight = ", Quantico:::CEP(TwoSampleTTest_PlotHeightINF), ",\n  ",
        "PlotWidth = ", Quantico:::CEP(TwoSampleTTest_PlotWidthINF), ",\n  ",
        "MeanDifference = ", Quantico:::CEPP(TwoSampleTTest_NullValue), ",\n  ",
        "EqualVariance = ", Quantico:::CEPP(TwoSampleTTest_EqualVariance), ",\n  ",
        "Paired = ", Quantico:::CEPP(TwoSampleTTest_Paired), ",\n  ",
        "Alternative = ", Quantico:::CEP(TwoSampleTTest_Alternative), ",\n  ",
        "ConfidenceLevel = ", Quantico:::CEPP(TwoSampleTTest_ConfidenceLevel), ",\n  ",
        "SampleSize = ", Quantico:::CEP(TwoSampleTTest_SampleSize), ",\n  ",
        "Samples = ", Quantico:::CEP(TwoSampleTTest_Samples), ")\n"))}, error = function(x) MachineLearningCode)

      # Update Available Outputs for Inference Tab
      if(Debug) print("inference 2")
      for(i in seq_len(NumTabs)) Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0('InferenceReportsModelSelection',i), Label = 'Testing Output', Choices = tryCatch({names(InferenceOutputList)}, error = function(x) NULL), Multiple = FALSE, MaxVars = 1L)
      InferenceOutputList <<- InferenceOutputList
      MachineLearningCode <<- MachineLearningCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = "", type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = "Data not available", type = NULL, btn_labels = "Error", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }

  }, ignoreInit = TRUE)

  # FTest Execution
  shiny::observeEvent(input$Inference_FTest_Execute, {

    # Args
    temp <- Quantico::ReturnParam(xx = tryCatch({input$FTest_SelectData}, error = function(x) NULL), Type = "character", Default = NULL)
    if(length(temp) > 0L) {

      FTest_EchartsTheme <- Quantico::ReturnParam(xx = tryCatch({input$EchartsTheme}, error = function(x) NULL), Type = "character", Default = "macarons")
      FTest_FontColorData <- Quantico:::rgba2hex(Quantico:::ReturnParam(xx = input[["ColorFont"]], Type = "character", Default = "#e2e2e2"))
      FTest_PlotWidthINF <- Quantico:::ReturnParam(xx = input[["PlotWidthinf"]], Type = "numeric", Default = 1450)
      FTest_PlotWidthINF <- paste0(FTest_PlotWidthINF, "px")
      FTest_PlotHeightINF <- Quantico:::ReturnParam(xx = input[["PlotHeightinf"]], Type = "numeric", Default = 860)
      FTest_PlotHeightINF <- paste0(FTest_PlotHeightINF, "px")

      FTest_SelectData <- DataList[[temp]][['data']]
      FTest_Variable1 <- Quantico::ReturnParam(xx = tryCatch({input$FTest_Variable1}, error = function(x) NULL), Type = "character", Default = NULL)
      FTest_Variable2 <- Quantico::ReturnParam(xx = tryCatch({input$FTest_Variable2}, error = function(x) NULL), Type = "character", Default = NULL)
      FTest_InferenceID <- Quantico::ReturnParam(xx = tryCatch({input$FTest_InferenceID}, error = function(x) NULL), Type = "character", Default = "INF_1STTest1")

      FTest_SampleSize <- Quantico::ReturnParam(xx = tryCatch({input$FTest_SampleSize}, error = function(x) NULL), Type = "numeric", Default = 100000)
      FTest_Samples <- Quantico::ReturnParam(xx = tryCatch({input$FTest_Samples}, error = function(x) NULL), Type = "numeric", Default = 1)
      FTest_RatioVariances <- Quantico::ReturnParam(xx = tryCatch({input$FTest_RatioVariances}, error = function(x) NULL), Type = "numeric", Default = 1)
      FTest_Alternative <- Quantico::ReturnParam(xx = tryCatch({input$FTest_Alternative}, error = function(x) NULL), Type = "character", Default = 1)
      FTest_ConfidenceLevel <- Quantico::ReturnParam(xx = tryCatch({input$FTest_ConfidenceLevel}, error = function(x) NULL), Type = "numeric", Default = 0.95)


      # Run function
      if(Debug) print("inference 0")
      if(!exists("InferenceOutputList")) InferenceOutputList <- list()
      InferenceOutputList[[FTest_InferenceID]] <- Quantico::F.Test(
        dt = FTest_SelectData,
        Variable1 = FTest_Variable1,
        Variable2 = FTest_Variable2,
        RatioVariances = FTest_RatioVariances,
        Alternative = FTest_Alternative,
        ConfidenceLevel = FTest_ConfidenceLevel,
        SampleSize = FTest_SampleSize,
        Samples = FTest_Samples,
        EchartsTheme = FTest_EchartsTheme,
        TextColor = FTest_FontColorData$flv,
        PlotHeight = FTest_PlotHeightINF,
        PlotWidth = FTest_PlotWidthINF)

      if(Debug) print("inference 1")

      MachineLearningCode <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = MachineLearningCode, x = paste0(
        "\n",
        "# Normality Testing\n",
        "FTest_SelectData <- DataList[[", Quantico:::CEP(temp), "]][['data']]\n",
        "Output <- Quantico::F.Test(, \n  ",
        "dt = FTest_SelectData, \n  ",
        "Variable1 = ", Quantico:::ExpandText(FTest_Variable1), ",\n  ",
        "Variable2 = ", Quantico:::ExpandText(FTest_Variable2), ",\n  ",
        "EchartsTheme = ", Quantico:::CEP(FTest_EchartsTheme), ",\n  ",
        "TextColor = ", Quantico:::CEP(FTest_FontColorData$flv), ",\n  ",
        "PlotHeight = ", Quantico:::CEP(FTest_PlotHeightINF), ",\n  ",
        "PlotWidth = ", Quantico:::CEP(FTest_PlotWidthINF), ",\n  ",
        "RatioVariances = ", Quantico:::CEPP(FTest_RatioVariances), ",\n  ",
        "Alternative = ", Quantico:::CEP(FTest_Alternative), ",\n  ",
        "ConfidenceLevel = ", Quantico:::CEPP(FTest_ConfidenceLevel), ",\n  ",
        "SampleSize = ", Quantico:::CEP(FTest_SampleSize), ",\n  ",
        "Samples = ", Quantico:::CEP(FTest_Samples), ")\n"))}, error = function(x) MachineLearningCode)

      # Update Available Outputs for Inference Tab
      if(Debug) print("inference 2")
      for(i in seq_len(NumTabs)) Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0('InferenceReportsModelSelection',i), Label = 'Testing Output', Choices = tryCatch({names(InferenceOutputList)}, error = function(x) NULL), Multiple = FALSE, MaxVars = 1L)
      InferenceOutputList <<- InferenceOutputList
      MachineLearningCode <<- MachineLearningCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = "", type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = "Data not available", type = NULL, btn_labels = "Error", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }

  }, ignoreInit = TRUE)

  # ChiSquareTest Execution
  shiny::observeEvent(input$Inference_ChiSquaredTest_Execute, {

    # Args
    temp <- Quantico::ReturnParam(xx = tryCatch({input$ChiSquareTest_SelectData}, error = function(x) NULL), Type = "character", Default = NULL)
    if(length(temp) > 0L) {

      ChiSquareTest_EchartsTheme <- Quantico::ReturnParam(xx = tryCatch({input$EchartsTheme}, error = function(x) NULL), Type = "character", Default = "macarons")
      ChiSquareTest_FontColorData <- Quantico:::rgba2hex(Quantico:::ReturnParam(xx = input[["ColorFont"]], Type = "character", Default = "#e2e2e2"))
      ChiSquareTest_PlotWidthINF <- Quantico:::ReturnParam(xx = input[["PlotWidthinf"]], Type = "numeric", Default = 1450)
      ChiSquareTest_PlotWidthINF <- paste0(ChiSquareTest_PlotWidthINF, "px")
      ChiSquareTest_PlotHeightINF <- Quantico:::ReturnParam(xx = input[["PlotHeightinf"]], Type = "numeric", Default = 860)
      ChiSquareTest_PlotHeightINF <- paste0(ChiSquareTest_PlotHeightINF, "px")

      ChiSquareTest_SelectData <- DataList[[temp]][['data']]
      ChiSquareTest_Variable1 <- Quantico::ReturnParam(xx = tryCatch({input$ChiSquareTest_Variable1}, error = function(x) NULL), Type = "character", Default = NULL)
      ChiSquareTest_Variable2 <- Quantico::ReturnParam(xx = tryCatch({input$ChiSquareTest_Variable2}, error = function(x) NULL), Type = "character", Default = NULL)
      ChiSquareTest_InferenceID <- Quantico::ReturnParam(xx = tryCatch({input$ChiSquareTest_InferenceID}, error = function(x) NULL), Type = "character", Default = "INF_1STTest1")

      ChiSquareTest_SampleSize <- Quantico::ReturnParam(xx = tryCatch({input$ChiSquareTest_SampleSize}, error = function(x) NULL), Type = "numeric", Default = 100000)
      ChiSquareTest_Samples <- Quantico::ReturnParam(xx = tryCatch({input$ChiSquareTest_Samples}, error = function(x) NULL), Type = "numeric", Default = 1)
      ChiSquareTest_Alternative <- Quantico::ReturnParam(xx = tryCatch({input$ChiSquareTest_Alternative}, error = function(x) NULL), Type = "character", Default = 1)
      ChiSquareTest_ConfidenceLevel <- Quantico::ReturnParam(xx = tryCatch({input$ChiSquareTest_ConfidenceLevel}, error = function(x) NULL), Type = "numeric", Default = 0.95)


      # Run function
      if(Debug) print("inference 0")
      if(!exists("InferenceOutputList")) InferenceOutputList <- list()
      print(ChiSquareTest_InferenceID)
      InferenceOutputList[[ChiSquareTest_InferenceID]] <- Quantico::ChiSq.Test(
        dt = ChiSquareTest_SelectData,
        Variable1 = ChiSquareTest_Variable1,
        Variable2 = ChiSquareTest_Variable2,
        Alternative = ChiSquareTest_Alternative,
        ConfidenceLevel = ChiSquareTest_ConfidenceLevel,
        SampleSize = ChiSquareTest_SampleSize,
        Samples = ChiSquareTest_Samples,
        EchartsTheme = ChiSquareTest_EchartsTheme,
        TextColor = ChiSquareTest_FontColorData$flv,
        PlotHeight = ChiSquareTest_PlotHeightINF,
        PlotWidth = ChiSquareTest_PlotWidthINF)

      if(Debug) print("inference 1")

      MachineLearningCode <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = MachineLearningCode, x = paste0(
        "\n",
        "# Normality Testing\n",
        "ChiSquareTest_SelectData <- DataList[[", Quantico:::CEP(temp), "]][['data']]\n",
        "Output <- Quantico::ChiSq.Test(, \n  ",
        "dt = ChiSquareTest_SelectData, \n  ",
        "Variable1 = ", Quantico:::ExpandText(ChiSquareTest_Variable1), ",\n  ",
        "Variable2 = ", Quantico:::ExpandText(ChiSquareTest_Variable2), ",\n  ",
        "EchartsTheme = ", Quantico:::CEP(ChiSquareTest_EchartsTheme), ",\n  ",
        "TextColor = ", Quantico:::CEP(ChiSquareTest_FontColorData$flv), ",\n  ",
        "PlotHeight = ", Quantico:::CEP(ChiSquareTest_PlotHeightINF), ",\n  ",
        "PlotWidth = ", Quantico:::CEP(ChiSquareTest_PlotWidthINF), ",\n  ",
        "Alternative = ", Quantico:::CEP(ChiSquareTest_Alternative), ",\n  ",
        "ConfidenceLevel = ", Quantico:::CEPP(ChiSquareTest_ConfidenceLevel), ",\n  ",
        "SampleSize = ", Quantico:::CEP(ChiSquareTest_SampleSize), ",\n  ",
        "Samples = ", Quantico:::CEP(ChiSquareTest_Samples), ")\n"))}, error = function(x) MachineLearningCode)

      # Update Available Outputs for Inference Tab
      if(Debug) print("inference 2")
      for(i in seq_len(NumTabs)) Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0('InferenceReportsModelSelection',i), Label = 'Testing Output', Choices = tryCatch({names(InferenceOutputList)}, error = function(x) NULL), Multiple = FALSE, MaxVars = 1L)
      InferenceOutputList <<- InferenceOutputList
      MachineLearningCode <<- MachineLearningCode
      shinyWidgets::sendSweetAlert(session, title = NULL, text = "", type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = "Data not available", type = NULL, btn_labels = "Error", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }

  }, ignoreInit = TRUE)

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event :: ML                   ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # CatBoost
  shiny::observeEvent(input$BuildModels_CatBoost, {

    # Code Collection
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()

    # Loop through Cross Validation Runs: 1 for regular training
    shiny::withProgress(message = 'CatBoost ML has begun..', value = 0, {
      ArgsList <- list()
      CatBoost_TargetType <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_TargetType']]}, error=function(x) NULL), Type='character', Default='MultiClass')
      n <- Quantico:::ReturnParam(xx=tryCatch({input[['CatBoost_Runs']]}, error=function(x) NULL), Type='numeric', Default=1L)
      for(run in seq_len(n)) {

        # Build Model
        if(Debug) {
          if(CatBoost_TargetType == "Regression") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, CatBoost_TargetType, ML_RegressionTable, run, n, Debug, Algo = 'CatBoost', wd = WorkingDirectory)
          } else if(CatBoost_TargetType == "Binary Classification") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, CatBoost_TargetType, ML_ClassificationTable, run, n, Debug, Algo = 'CatBoost', wd = WorkingDirectory)
          } else if(CatBoost_TargetType == "MultiClass") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, CatBoost_TargetType, ML_MultiClassTable, run, n, Debug, Algo = 'CatBoost', wd = WorkingDirectory)
          }
        } else {
          if(CatBoost_TargetType == "Regression") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, CatBoost_TargetType, ML_RegressionTable, run, n, Debug, Algo = 'CatBoost', wd = WorkingDirectory)}, error = function(x) NULL)
          } else if(CatBoost_TargetType == "Binary Classification") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, CatBoost_TargetType, ML_ClassificationTable, run, n, Debug, Algo = 'CatBoost', wd = WorkingDirectory)}, error = function(x) NULL)
          } else if(CatBoost_TargetType == "MultiClass") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, CatBoost_TargetType, ML_MultiClassTable, run, n, Debug, Algo = 'CatBoost', wd = WorkingDirectory)}, error = function(x) NULL)
          }
        }

        if(length(Output) > 0L) {
          MachineLearningCode <- Output$CodeList; MachineLearningCode <<- MachineLearningCode
          ModelOutputList <- Output$ModelOutputList; ModelOutputList <<- ModelOutputList
          DataList <<- Output$DataList; ArgsList <<- Output$ArgsList; rm(Output); gc()

          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("MLReportsModelSelection", i), Label = 'Model Selection', Choices = names(ModelOutputList), Multiple = TRUE, MaxVars = 100L)
          Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Model', Label = 'Select Model', Choices = names(ModelOutputList), SelectedDefault = NULL)
          Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Data', Label = 'Select Data', Choices = names(DataList), SelectedDefault = NULL)

          if(CatBoost_TargetType == "Regression") {
            ML_RegressionTable <- DataList[["ML_Metrics"]][["sample"]]
            ML_RegressionTable <<- ML_RegressionTable
          } else if(CatBoost_TargetType == "Binary Classification") {
            ML_ClassificationTable <- DataList[["ML_Metrics"]][["sample"]]
            ML_ClassificationTable <<- ML_ClassificationTable
          } else if(CatBoost_TargetType == "MultiClass") {
            ML_MultiClassTable <- DataList[["ML_Metrics"]][["sample"]]
            ML_MultiClassTable <<- ML_MultiClassTable
          }

          # Increment the progress bar, and update the detail text.
          if(n > run) {
            shiny::incProgress(1/n, detail = paste("Run number ", run, " has finished. Moving on to run ", run + 1L))
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            shiny::incProgress(1/n, detail = paste("100% Complete"))
          }

        } else {
          if(n > run) {
            shiny::incProgress(1/n, detail = paste("Run number ", run, " failed to build. Moving on to run ", run + 1L))
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            shiny::incProgress(1/n, detail = paste("100% Complete"))
          }
        }
      }
    })
  }, ignoreInit = TRUE)

  # XGBoostMLParams
  shiny::observeEvent(input$BuildModels_XGBoost,  {

    # Code Collection
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()

    # Loop through Cross Validation Runs: 1 for regular training
    shiny::withProgress(message = 'XGBoost ML has begun..', value = 0, {
      ArgsList <- list()
      XGBoost_TargetType <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_TargetType']]}, error=function(x) NULL), Type='character', Default='MultiClass')
      n <- Quantico:::ReturnParam(xx=tryCatch({input[['XGBoost_Runs']]}, error=function(x) NULL), Type='numeric', Default=1L)
      for(run in seq_len(n)) {

        # Build Model
        if(Debug) {
          if(XGBoost_TargetType == "Regression") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, XGBoost_TargetType, ML_RegressionTable, run, n, Debug, Algo = 'XGBoost')
          } else if(XGBoost_TargetType == "Binary Classification") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, XGBoost_TargetType, ML_ClassificationTable, run, n, Debug, Algo = 'XGBoost')
          } else if(XGBoost_TargetType == "MultiClass") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, XGBoost_TargetType, ML_MultiClassTable, run, n, Debug, Algo = 'XGBoost')
          }
        } else {
          if(XGBoost_TargetType == "Regression") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, XGBoost_TargetType, ML_RegressionTable, run, n, Debug, Algo = 'XGBoost')}, error = function(x) NULL)
          } else if(XGBoost_TargetType == "Binary Classification") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, XGBoost_TargetType, ML_ClassificationTable, run, n, Debug, Algo = 'XGBoost')}, error = function(x) NULL)
          } else if(XGBoost_TargetType == "MultiClass") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, XGBoost_TargetType, ML_MultiClassTable, run, n, Debug, Algo = 'XGBoost')}, error = function(x) NULL)
          }
        }

        if(length(Output) > 0L) {
          MachineLearningCode <- Output$CodeList; MachineLearningCode <<- MachineLearningCode
          ModelOutputList <- Output$ModelOutputList; ModelOutputList <<- ModelOutputList
          DataList <<- Output$DataList; ArgsList <<- Output$ArgsList; rm(Output); gc()

          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("MLReportsModelSelection", i), Label = 'Model Selection', Choices = names(ModelOutputList), Multiple = TRUE, MaxVars = 100L)
          Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Model', Label = 'Select Model', Choices = names(ModelOutputList), SelectedDefault = NULL)
          Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Data', Label = 'Select Data', Choices = names(DataList), SelectedDefault = NULL)

          if(XGBoost_TargetType == "Regression") {
            ML_RegressionTable <- DataList[["ML_RegressionMetrics"]][["sample"]]
            ML_RegressionTable <<- ML_RegressionTable
          } else if(XGBoost_TargetType == "Binary Classification") {
            ML_ClassificationTable <- DataList[["ML_ClassificationMetrics"]][["sample"]]
            ML_ClassificationTable <<- ML_ClassificationTable
          } else if(XGBoost_TargetType == "MultiClass") {
            ML_MultiClassTable <- DataList[["ML_MultiClassMetrics"]][["sample"]]
            ML_MultiClassTable <<- ML_MultiClassTable
          }

          # Increment the progress bar, and update the detail text.
          if(n > run) {
            shiny::incProgress(1/n, detail = paste("Run number ", run, " has finished. Moving on to run ", run + 1L))
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            shiny::incProgress(1/n, detail = paste("100% Complete"))
          }
        } else {
          if(n > run) {
            shiny::incProgress(1/n, detail = paste("Run number ", run, " failed to build. Moving on to run ", run + 1L))
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            shiny::incProgress(1/n, detail = paste("100% Complete"))
          }
        }
      }
    })
  }, ignoreInit = TRUE)

  # LightGBM
  shiny::observeEvent(input$BuildModels_LightGBM, {

    # Code Collection
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()

    # Loop through Cross Validation Runs: 1 for regular training
    shiny::withProgress(message = 'LightGBM ML has begun..', value = 0, {
      ArgsList <- list()
      LightGBM_TargetType <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_TargetType']]}, error=function(x) NULL), Type='character', Default='MultiClass')
      n <- Quantico:::ReturnParam(xx=tryCatch({input[['LightGBM_Runs']]}, error=function(x) NULL), Type='numeric', Default=1L)
      for(run in seq_len(n)) {

        # Build Model
        if(Debug) {
          if(LightGBM_TargetType == "Regression") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, LightGBM_TargetType, ML_RegressionTable, run, n, Debug, Algo = 'LightGBM')
          } else if(LightGBM_TargetType == "Binary Classification") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, LightGBM_TargetType, ML_ClassificationTable, run, n, Debug, Algo = 'LightGBM')
          } else if(LightGBM_TargetType == "MultiClass") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, LightGBM_TargetType, ML_MultiClassTable, run, n, Debug, Algo = 'LightGBM')
          }
        } else {
          if(LightGBM_TargetType == "Regression") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, LightGBM_TargetType, ML_RegressionTable, run, n, Debug, Algo = 'LightGBM')}, error = function(x) NULL)
          } else if(LightGBM_TargetType == "Binary Classification") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, LightGBM_TargetType, ML_ClassificationTable, run, n, Debug, Algo = 'LightGBM')}, error = function(x) NULL)
          } else if(LightGBM_TargetType == "MultiClass") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, LightGBM_TargetType, ML_MultiClassTable, run, n, Debug, Algo = 'LightGBM')}, error = function(x) NULL)
          }
        }

        if(length(Output) > 0L) {
          MachineLearningCode <- Output$CodeList; MachineLearningCode <<- MachineLearningCode
          ModelOutputList <- Output$ModelOutputList; ModelOutputList <<- ModelOutputList
          DataList <<- Output$DataList; ArgsList <<- Output$ArgsList; rm(Output); gc()

          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("MLReportsModelSelection", i), Label = 'Model Selection', Choices = names(ModelOutputList), Multiple = TRUE, MaxVars = 100L)
          Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Model', Label = 'Select Model', Choices = names(ModelOutputList), SelectedDefault = NULL)
          Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Data', Label = 'Select Data', Choices = names(DataList), SelectedDefault = NULL)

          if(LightGBM_TargetType == "Regression") {
            ML_RegressionTable <- DataList[["ML_RegressionMetrics"]][["sample"]]
            ML_RegressionTable <<- ML_RegressionTable
          } else if(LightGBM_TargetType == "Binary Classification") {
            ML_ClassificationTable <- DataList[["ML_ClassificationMetrics"]][["sample"]]
            ML_ClassificationTable <<- ML_ClassificationTable
          } else if(LightGBM_TargetType == "MultiClass") {
            ML_MultiClassTable <- DataList[["ML_MultiClassMetrics"]][["sample"]]
            ML_MultiClassTable <<- ML_MultiClassTable
          }

          # Increment the progress bar, and update the detail text.
          if(n > run) {
            shiny::incProgress(1/n, detail = paste("Run number ", run, " has finished. Moving on to run ", run + 1L))
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            shiny::incProgress(1/n, detail = paste("100% Complete"))
          }
        } else {
          if(n > run) {
            shiny::incProgress(1/n, detail = paste("Run number ", run, " failed to build. Moving on to run ", run + 1L))
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            shiny::incProgress(1/n, detail = paste("100% Complete"))
          }
        }
      }
    })
  }, ignoreInit = TRUE)

  # H2O_DRF
  shiny::observeEvent(input$BuildModels_H2O_DRF, {

    # Code Collection
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()

    # Loop through Cross Validation Runs: 1 for regular training
    shiny::withProgress(message = 'H2O_DRF ML has begun..', value = 0, {
      ArgsList <- list()
      H2O_DRF_TargetType <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_TargetType']]}, error=function(x) NULL), Type='character', Default='MultiClass')
      n <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_DRF_Runs']]}, error=function(x) NULL), Type='numeric', Default=1L)
      for(run in seq_len(n)) {

        # Build Model
        if(Debug) {
          if(H2O_DRF_TargetType == "Regression") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_DRF_TargetType, ML_RegressionTable, run, n, Debug, Algo = 'H2O_DRF', wd = WorkingDirectory)
          } else if(H2O_DRF_TargetType == "Binary Classification") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_DRF_TargetType, ML_ClassificationTable, run, n, Debug, Algo = 'H2O_DRF', wd = WorkingDirectory)
          } else if(H2O_DRF_TargetType == "MultiClass") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_DRF_TargetType, ML_MultiClassTable, run, n, Debug, Algo = 'H2O_DRF', wd = WorkingDirectory)
          }
        } else {
          if(H2O_DRF_TargetType == "Regression") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_DRF_TargetType, ML_RegressionTable, run, n, Debug, Algo = 'H2O_DRF', wd = WorkingDirectory)}, error = function(x) NULL)
          } else if(H2O_DRF_TargetType == "Binary Classification") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_DRF_TargetType, ML_ClassificationTable, run, n, Debug, Algo = 'H2O_DRF', wd = WorkingDirectory)}, error = function(x) NULL)
          } else if(H2O_DRF_TargetType == "MultiClass") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_DRF_TargetType, ML_MultiClassTable, run, n, Debug, Algo = 'H2O_DRF', wd = WorkingDirectory)}, error = function(x) NULL)
          }
        }

        if(length(Output) > 0L) {
          MachineLearningCode <- Output$CodeList; MachineLearningCode <<- MachineLearningCode
          ModelOutputList <- Output$ModelOutputList; ModelOutputList <<- ModelOutputList
          DataList <<- Output$DataList; ArgsList <<- Output$ArgsList; rm(Output); gc()
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("MLReportsModelSelection", i), Label = 'Model Selection', Choices = names(ModelOutputList), Multiple = TRUE, MaxVars = 100L)
          Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Model', Label = 'Select Model', Choices = names(ModelOutputList), SelectedDefault = NULL)
          Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Data', Label = 'Select Data', Choices = names(DataList), SelectedDefault = NULL)

          if(H2O_DRF_TargetType == "Regression") {
            ML_RegressionTable <- DataList[["ML_RegressionMetrics"]][["sample"]]
            ML_RegressionTable <<- ML_RegressionTable
          } else if(H2O_DRF_TargetType == "Binary Classification") {
            ML_ClassificationTable <- DataList[["ML_ClassificationMetrics"]][["sample"]]
            ML_ClassificationTable <<- ML_ClassificationTable
          } else if(H2O_DRF_TargetType == "MultiClass") {
            ML_MultiClassTable <- DataList[["ML_MultiClassMetrics"]][["sample"]]
            ML_MultiClassTable <<- ML_MultiClassTable
          }

          # Increment the progress bar, and update the detail text.
          if(n > run) {
            shiny::incProgress(1/n, detail = paste("Run number ", run, " has finished. Moving on to run ", run + 1L))
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            shiny::incProgress(1/n, detail = paste("100% Complete"))
          }

        } else {
          if(n > run) {
            shiny::incProgress(1/n, detail = paste("Run number ", run, " failed to build. Moving on to run ", run + 1L))
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            shiny::incProgress(1/n, detail = paste("100% Complete"))
          }
        }
      }
    })
  }, ignoreInit = TRUE)

  # H2O_GBM
  shiny::observeEvent(input$BuildModels_H2O_GBM, {

    # Code Collection
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()

    # Loop through Cross Validation Runs: 1 for regular training
    shiny::withProgress(message = 'H2O_GBM ML has begun..', value = 0, {
      ArgsList <- list()
      H2O_GBM_TargetType <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_TargetType']]}, error=function(x) NULL), Type='character', Default='MultiClass')
      n <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GBM_Runs']]}, error=function(x) NULL), Type='numeric', Default=1L)
      for(run in seq_len(n)) {

        # Build Model
        if(Debug) {
          if(H2O_GBM_TargetType == "Regression") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_GBM_TargetType, ML_RegressionTable, run, n, Debug, Algo = 'H2O_GBM', wd = WorkingDirectory)
          } else if(H2O_GBM_TargetType == "Binary Classification") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_GBM_TargetType, ML_ClassificationTable, run, n, Debug, Algo = 'H2O_GBM', wd = WorkingDirectory)
          } else if(H2O_GBM_TargetType == "MultiClass") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_GBM_TargetType, ML_MultiClassTable, run, n, Debug, Algo = 'H2O_GBM', wd = WorkingDirectory)
          }
        } else {
          if(H2O_GBM_TargetType == "Regression") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_GBM_TargetType, ML_RegressionTable, run, n, Debug, Algo = 'H2O_GBM', wd = WorkingDirectory)}, error = function(x) NULL)
          } else if(H2O_GBM_TargetType == "Binary Classification") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_GBM_TargetType, ML_ClassificationTable, run, n, Debug, Algo = 'H2O_GBM', wd = WorkingDirectory)}, error = function(x) NULL)
          } else if(H2O_GBM_TargetType == "MultiClass") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_GBM_TargetType, ML_MultiClassTable, run, n, Debug, Algo = 'H2O_GBM', wd = WorkingDirectory)}, error = function(x) NULL)
          }
        }

        if(Debug) print(names(Output))
        if(length(Output) > 0L) {
          MachineLearningCode <- Output$CodeList; MachineLearningCode <<- MachineLearningCode
          ModelOutputList <- Output$ModelOutputList; ModelOutputList <<- ModelOutputList
          DataList <<- Output$DataList; ArgsList <<- Output$ArgsList; rm(Output); gc()

          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("MLReportsModelSelection", i), Label = 'Model Selection', Choices = names(ModelOutputList), Multiple = TRUE, MaxVars = 100L)
          Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Model', Label = 'Select Model', Choices = names(ModelOutputList), SelectedDefault = NULL)
          Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Data', Label = 'Select Data', Choices = names(DataList), SelectedDefault = NULL)

          if(H2O_GBM_TargetType == "Regression") {
            ML_RegressionTable <- DataList[["ML_RegressionMetrics"]][["sample"]]
            ML_RegressionTable <<- ML_RegressionTable
          } else if(H2O_GBM_TargetType == "Binary Classification") {
            ML_ClassificationTable <- DataList[["ML_ClassificationMetrics"]][["sample"]]
            ML_ClassificationTable <<- ML_ClassificationTable
          } else if(H2O_GBM_TargetType == "MultiClass") {
            ML_MultiClassTable <- DataList[["ML_MultiClassMetrics"]][["sample"]]
            ML_MultiClassTable <<- ML_MultiClassTable
          }

          # Increment the progress bar, and update the detail text.
          if(n > run) {
            shiny::incProgress(1/n, detail = paste("Run number ", run, " has finished. Moving on to run ", run + 1L))
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            shiny::incProgress(1/n, detail = paste("100% Complete"))
          }

        } else {
          if(n > run) {
            shiny::incProgress(1/n, detail = paste("Run number ", run, " failed to build. Moving on to run ", run + 1L))
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            shiny::incProgress(1/n, detail = paste("100% Complete"))
          }
        }
      }
    })
  }, ignoreInit = TRUE)

  # H2O_GLM
  shiny::observeEvent(input$BuildModels_H2O_GLM, {

    # Code Collection
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()

    # Loop through Cross Validation Runs: 1 for regular training
    shiny::withProgress(message = 'H2O_GLM ML has begun..', value = 0, {
      ArgsList <- list()
      H2O_GLM_TargetType <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_TargetType']]}, error=function(x) NULL), Type='character', Default='Regression')
      n <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_GLM_Runs']]}, error=function(x) NULL), Type='numeric', Default=1L)
      for(run in seq_len(n)) {

        # Build Model
        if(Debug) {
          if(H2O_GLM_TargetType == "Regression") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_GLM_TargetType, ML_RegressionTable, run, n, Debug, Algo = 'H2O_GLM', wd = WorkingDirectory)
          } else if(H2O_GLM_TargetType == "Binary Classification") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_GLM_TargetType, ML_ClassificationTable, run, n, Debug, Algo = 'H2O_GLM', wd = WorkingDirectory)
          } else if(H2O_GLM_TargetType == "MultiClass") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_GLM_TargetType, ML_MultiClassTable, run, n, Debug, Algo = 'H2O_GLM', wd = WorkingDirectory)
          }
        } else {
          if(H2O_GLM_TargetType == "Regression") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_GLM_TargetType, ML_RegressionTable, run, n, Debug, Algo = 'H2O_GLM', wd = WorkingDirectory)}, error = function(x) NULL)
          } else if(H2O_GLM_TargetType == "Binary Classification") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_GLM_TargetType, ML_ClassificationTable, run, n, Debug, Algo = 'H2O_GLM', wd = WorkingDirectory)}, error = function(x) NULL)
          } else if(H2O_GLM_TargetType == "MultiClass") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_GLM_TargetType, ML_MultiClassTable, run, n, Debug, Algo = 'H2O_GLM', wd = WorkingDirectory)}, error = function(x) NULL)
          }
        }

        if(Debug) print(names(Output))

        if(length(Output) > 0L) {
          MachineLearningCode <- Output$CodeList; MachineLearningCode <<- MachineLearningCode
          ModelOutputList <- Output$ModelOutputList; ModelOutputList <<- ModelOutputList
          DataList <<- Output$DataList; ArgsList <<- Output$ArgsList; rm(Output); gc()

          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("MLReportsModelSelection", i), Label = 'Model Selection', Choices = names(ModelOutputList), Multiple = TRUE, MaxVars = 100L)
          Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Model', Label = 'Select Model', Choices = names(ModelOutputList), SelectedDefault = NULL)
          Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Data', Label = 'Select Data', Choices = names(DataList), SelectedDefault = NULL)

          if(H2O_GLM_TargetType == "Regression") {
            ML_RegressionTable <- DataList[["ML_RegressionMetrics"]][["sample"]]
            ML_RegressionTable <<- ML_RegressionTable
          } else if(H2O_GLM_TargetType == "Binary Classification") {
            ML_ClassificationTable <- DataList[["ML_ClassificationMetrics"]][["sample"]]
            ML_ClassificationTable <<- ML_ClassificationTable
          } else if(H2O_GLM_TargetType == "MultiClass") {
            ML_MultiClassTable <- DataList[["ML_MultiClassMetrics"]][["sample"]]
            ML_MultiClassTable <<- ML_MultiClassTable
          }

          # Increment the progress bar, and update the detail text.
          if(n > run) {
            shiny::incProgress(1/n, detail = paste("Run number ", run, " has finished. Moving on to run ", run + 1L))
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            shiny::incProgress(1/n, detail = paste("100% Complete"))
          }

        } else {
          if(n > run) {
            shiny::incProgress(1/n, detail = paste("Run number ", run, " failed to build. Moving on to run ", run + 1L))
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            shiny::incProgress(1/n, detail = paste("100% Complete"))
          }
        }
      }
    })
  }, ignoreInit = TRUE)

  # H2O_HGLM
  shiny::observeEvent(input$BuildModels_H2O_HGLM, {

    # Code Collection
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()

    # Loop through Cross Validation Runs: 1 for regular training
    shiny::withProgress(message = 'H2O_HGLM ML has begun..', value = 0, {
      ArgsList <- list()
      H2O_HGLM_TargetType <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_TargetType']]}, error=function(x) NULL), Type='character', Default='Regression')
      n <- Quantico:::ReturnParam(xx=tryCatch({input[['H2O_HGLM_Runs']]}, error=function(x) NULL), Type='numeric', Default=1L)
      for(run in seq_len(n)) {

        # Build Model
        if(Debug) {
          if(H2O_HGLM_TargetType == "Regression") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_HGLM_TargetType, ML_RegressionTable, run, n, Debug, Algo = 'H2O_HGLM')
          } else if(H2O_HGLM_TargetType == "Binary Classification") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_HGLM_TargetType, ML_ClassificationTable, run, n, Debug, Algo = 'H2O_HGLM')
          } else if(H2O_HGLM_TargetType == "MultiClass") {
            Output <- Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_HGLM_TargetType, ML_MultiClassTable, run, n, Debug, Algo = 'H2O_HGLM')
          }
        } else {
          if(H2O_HGLM_TargetType == "Regression") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_HGLM_TargetType, ML_RegressionTable, run, n, Debug, Algo = 'H2O_HGLM')}, error = function(x) NULL)
          } else if(H2O_HGLM_TargetType == "Binary Classification") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_HGLM_TargetType, ML_ClassificationTable, run, n, Debug, Algo = 'H2O_HGLM')}, error = function(x) NULL)
          } else if(H2O_HGLM_TargetType == "MultiClass") {
            Output <- tryCatch({Quantico:::Shiny.ML.Trainer(input, output, DataList, ArgsList, MachineLearningCode, ModelOutputList, H2O_HGLM_TargetType, ML_MultiClassTable, run, n, Debug, Algo = 'H2O_HGLM')}, error = function(x) NULL)
          }
        }

        if(Debug) print(names(Output))

        if(length(Output) > 0L) {
          MachineLearningCode <- Output$CodeList; MachineLearningCode <<- MachineLearningCode
          ModelOutputList <- Output$ModelOutputList; ModelOutputList <<- ModelOutputList
          DataList <<- Output$DataList; ArgsList <<- Output$ArgsList; rm(Output); gc()

          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("MLReportsModelSelection", i), Label = 'Model Selection', Choices = names(ModelOutputList), Multiple = TRUE, MaxVars = 100L)
          Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Model', Label = 'Select Model', Choices = names(ModelOutputList), SelectedDefault = NULL)
          Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Data', Label = 'Select Data', Choices = names(DataList), SelectedDefault = NULL)

          if(H2O_HGLM_TargetType == "Regression") {
            ML_RegressionTable <- DataList[["ML_RegressionMetrics"]][["sample"]]
            ML_RegressionTable <<- ML_RegressionTable
          } else if(H2O_HGLM_TargetType == "Binary Classification") {
            ML_ClassificationTable <- DataList[["ML_ClassificationMetrics"]][["sample"]]
            ML_ClassificationTable <<- ML_ClassificationTable
          } else if(H2O_HGLM_TargetType == "MultiClass") {
            ML_MultiClassTable <- DataList[["ML_MultiClassMetrics"]][["sample"]]
            ML_MultiClassTable <<- ML_MultiClassTable
          }

          # Increment the progress bar, and update the detail text.
          if(n > run) {
            shiny::incProgress(1/n, detail = paste("Run number ", run, " has finished. Moving on to run ", run + 1L))
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            shiny::incProgress(1/n, detail = paste("100% Complete"))
          }

        } else {
          if(n > run) {
            shiny::incProgress(1/n, detail = paste("Run number ", run, " failed to build. Moving on to run ", run + 1L))
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            shiny::incProgress(1/n, detail = paste("100% Complete"))
          }
        }
      }
    })
  }, ignoreInit = TRUE)

  # Causal Mediation
  shiny::observeEvent(input$BuildModels_CausalMediation, {

    # Code Collection
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()

    # Loop through Cross Validation Runs: 1 for regular training
    shiny::withProgress(message = 'Causal Mediation has begun..', value = 0, {

      # ArgsList <- list()
      x <- Quantico:::ReturnParam(xx = tryCatch({input[['CausalMediation_data']]}, error=function(x) NULL), Type='character', Default=NULL)
      if(length(x) > 0L) {

        # Values
        data <- DataList[[x]]
        OutcomeTargetVariable <- Quantico:::ReturnParam(xx=tryCatch({input[['CausalMediation_OutcomeTargetVariable']]}, error=function(x) NULL), Type='character', Default=NULL)
        TreatmentVariable <- Quantico:::ReturnParam(xx=tryCatch({input[['CausalMediation_TreatmentVariable']]}, error=function(x) NULL), Type='character', Default=NULL)
        MediatorVariable <- Quantico:::ReturnParam(xx=tryCatch({input[['CausalMediation_MediatorVariable']]}, error=function(x) NULL), Type='character', Default=NULL)
        Covariates <- Quantico:::ReturnParam(xx=tryCatch({input[['CausalMediation_Covariates']]}, error=function(x) NULL), Type='character', Default=NULL)
        MM_TreatmentCovariates <- Quantico:::ReturnParam(xx=tryCatch({input[['CausalMediation_MM_TreatmentCovariates']]}, error=function(x) NULL), Type='character', Default=NULL)
        OM_TreatmentCovariates <- Quantico:::ReturnParam(xx=tryCatch({input[['CausalMediation_OM_TreatmentCovariates']]}, error=function(x) NULL), Type='character', Default=NULL)
        OM_MediatorCovariates <- Quantico:::ReturnParam(xx=tryCatch({input[['CausalMediation_OM_MediatorCovariates']]}, error=function(x) NULL), Type='character', Default=NULL)
        SurvivalEventVariable <- Quantico:::ReturnParam(xx=tryCatch({input[['CausalMediation_SurvivalEventVariable']]}, error=function(x) NULL), Type='character', Default=NULL)
        UnTreated_ReferenceIndicator <- Quantico:::ReturnParam(xx=tryCatch({input[['CausalMediation_UnTreated_ReferenceIndicator']]}, error=function(x) NULL), Type='numeric', Default=0)
        Treated_ReferenceIndicator <- Quantico:::ReturnParam(xx=tryCatch({input[['CausalMediation_Treated_ReferenceIndicator']]}, error=function(x) NULL), Type='numeric', Default=1)
        Mediator_ControlDirectEffectLevel <- Quantico:::ReturnParam(xx=tryCatch({input[['CausalMediation_Mediator_ControlDirectEffectLevel']]}, error=function(x) NULL), Type='numeric', Default=0)
        Covariate_NaturalDirectIndirect <- Quantico:::ReturnParam(xx=tryCatch({input[['CausalMediation_Covariate_NaturalDirectIndirect']]}, error=function(x) NULL), Type='numeric', Default=NULL)
        MediatorTargetType <- Quantico:::ReturnParam(xx=tryCatch({input[['CausalMediation_MediatorTargetType']]}, error=function(x) NULL), Type='character', Default='linear')
        OutcomeTargetType <- Quantico:::ReturnParam(xx=tryCatch({input[['CausalMediation_OutcomeTargetType']]}, error=function(x) NULL), Type='character', Default='linear')
        TreatmentMediatorInteraction <- Quantico:::ReturnParam(xx=tryCatch({input[['CausalMediation_TreatmentMediatorInteraction']]}, error=function(x) NULL), Type='logical', Default=TRUE)
        CaseControlSourceData <- Quantico:::ReturnParam(xx=tryCatch({input[['CausalMediation_CaseControlSourceData']]}, error=function(x) NULL), Type='logical', Default=FALSE)
        RemoveNA <- Quantico:::ReturnParam(xx=tryCatch({input[['CausalMediation_RemoveNA']]}, error=function(x) NULL), Type='logical', Default=FALSE)

        if(Debug) {
          print(data)
          print(OutcomeTargetVariable)
          print(TreatmentVariable)
          print(MediatorVariable)
          print(Covariates)
          print(MM_TreatmentCovariates)
          print(OM_TreatmentCovariates)
          print(OM_MediatorCovariates)
          print(SurvivalEventVariable)
          print(UnTreated_ReferenceIndicator)
          print(Treated_ReferenceIndicator)
          print(Mediator_ControlDirectEffectLevel)
          print(Covariate_NaturalDirectIndirect)
          print(MediatorTargetType)
          print(OutcomeTargetType)
          print(TreatmentMediatorInteraction)
          print(CaseControlSourceData)
          print(RemoveNA)
        }

        Output <- tryCatch({AutoQuant::CausalMediation(
          data = data,

          # Variables
          OutcomeTargetVariable = OutcomeTargetVariable,                          # yvar char length = 0
          TreatmentVariable = TreatmentVariable,                                  # avar char length = 0 (binary)
          MediatorVariable = MediatorVariable,                                    # mvar char length = 0 (binary)
          Covariates = Covariates,                                                # cvar char length > 0
          MM_TreatmentCovariates = MM_TreatmentCovariates,                        # emm_ac_mreg = NULL char length > 0
          OM_TreatmentCovariates = OM_TreatmentCovariates,                        # emm_ac_yreg = NULL char length > 0
          OM_MediatorCovariates = OM_MediatorCovariates,                          # emm_mc_yreg = NULL char length > 0
          SurvivalEventVariable = SurvivalEventVariable,                          # eventvar char length = 0

          # Evaluation Values
          UnTreated_ReferenceIndicator = UnTreated_ReferenceIndicator,            # ao num length = 1
          Treated_ReferenceIndicator = Treated_ReferenceIndicator,                # a1 num length = 1
          Mediator_ControlDirectEffectLevel = Mediator_ControlDirectEffectLevel,  # m_cde num length = 1
          Covariate_NaturalDirectIndirect = Covariate_NaturalDirectIndirect,      # c_cond; same length as Covariates num length = length(Covariates)

          # Model Types
          MediatorTargetType = MediatorTargetType,                                # mreg "linear" or "logistic",
          OutcomeTargetType = OutcomeTargetType,                                  # yreg "linear", "logistic", "loglinear", "poisson", "negbin", "survCox", "survAFT_exp", or "survAFT_weibull"

          # Additional Specifications
          TreatmentMediatorInteraction = TreatmentMediatorInteraction,            # interaction = TRUE,
          CaseControlSourceData = CaseControlSourceData,                          # casecontrol = FALSE,
          RemoveNA = RemoveNA)}, error = function(x) NULL)
        if(Debug) print(names(Output))

        if(length(Output) > 0L) {
          CausalMediationSummaryText <- Output$SummaryOutput
          ML_ExperimentTable <<- Output$Effects
          MainEffects <- Output$MainEffects
          if(Debug) print(ML_ExperimentTable)

          # Code Collection
          MachineLearningCode <- tryCatch({Quantico:::Shiny.CodePrint.Collect(y = MachineLearningCode, x = paste0(
            "\n",
            "# Build Model\n",
            "Output <- AutoQuant::CausalMediation(\n  ",
            "data = DataList[['data']],\n  ",
            "OutcomeTargetVariable = ", Quantico:::CEP(OutcomeTargetVariable),",\n  ",
            "TreatmentVariable = ", Quantico:::CEP(TreatmentVariable), ",\n  ",
            "MediatorVariable = ", Quantico:::CEP(MediatorVariable), ",\n  ",
            "Covariates = ", Quantico:::ExpandText(Covariates), ",\n  ",
            "MM_TreatmentCovariates = ", Quantico:::ExpandText(MM_TreatmentCovariates), ",\n  ",
            "OM_TreatmentCovariates = ", Quantico:::ExpandText(OM_TreatmentCovariates), ",\n  ",
            "OM_MediatorCovariates = ", Quantico:::ExpandText(OM_MediatorCovariates), ",\n  ",
            "SurvivalEventVariable = ", Quantico:::CEPP(SurvivalEventVariable), ",\n  ",
            "UnTreated_ReferenceIndicator = ", Quantico:::CEPP(UnTreated_ReferenceIndicator), ",\n  ",
            "Treated_ReferenceIndicator = ", Quantico:::CEPP(Treated_ReferenceIndicator), ",\n  ",
            "Mediator_ControlDirectEffectLevel = ", Quantico:::CEPP(Mediator_ControlDirectEffectLevel), ",\n  ",
            "Covariate_NaturalDirectIndirect = ", Quantico:::CEPP(Covariate_NaturalDirectIndirect), ",\n  ",
            "MediatorTargetType = ", Quantico:::CEP(MediatorTargetType), ",\n  ",
            "OutcomeTargetType = ", Quantico:::CEP(OutcomeTargetType), ",\n  ",
            "TreatmentMediatorInteraction = ", Quantico:::CEPP(TreatmentMediatorInteraction), ",\n  ",
            "CaseControlSourceData = ", Quantico:::CEPP(CaseControlSourceData), ",\n  ",
            "RemoveNA = ", Quantico:::CEPP(RemoveNA), ")\n\n"))}, error = function(x) MachineLearningCode)

          # Increment the progress bar, and update the detail text.
          DataList[['Causal_Mediation_Measures']][['data']] <- ML_ExperimentTable
          DataList[['Causal_Mediation_MainEffects']][['data']] <- MainEffects
          DataList <<- DataList

          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("MLReportsModelSelection", i), Label = 'Model Selection', Choices = names(ModelOutputList), Multiple = TRUE, MaxVars = 100L)
          Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Data', Label = 'Select Data', Choices = names(DataList), SelectedDefault = NULL)

          shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

        } else {
          shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Model Failed", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
        }

      } else {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Missing Data", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      }
    })
  }, ignoreInit = TRUE)

  # Mixed Effects
  shiny::observeEvent(input$BuildModels_MixedEffects, {

    # Code Collection
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()

    # Loop through Cross Validation Runs: 1 for regular training
    shiny::withProgress(message = 'Mixed Effects has begun..', value = 0, {

      # ArgsList <- list()
      x <- Quantico:::ReturnParam(xx = tryCatch({input[['MixedEffects_data']]}, error=function(x) NULL), Type='character', Default=NULL)
      if(length(x) > 0L) {

        # Values
        data <- DataList[[x]][['data']]
        x <- Quantico:::ReturnParam(xx=tryCatch({input[['MixedEffects_ValidationData']]}, error=function(x) NULL), Type='character', Default=NULL)
        if(length(x) > 0L) ValidationData <- DataList[[x]][['data']] else ValidationData <- NULL
        x <- Quantico:::ReturnParam(xx=tryCatch({input[['MixedEffects_TestData']]}, error=function(x) NULL), Type='character', Default=NULL)
        if(length(x) > 0L) TestData <- DataList[[x]][['data']] else TestData <- NULL

        MixedEffects_TargetColumnName <- Quantico:::ReturnParam(xx=tryCatch({input[['MixedEffects_TargetColumnName']]}, error=function(x) NULL), Type='character', Default=NULL)
        MixedEffects_TargetType <- Quantico:::ReturnParam(xx=tryCatch({input[['MixedEffects_TargetType']]}, error=function(x) NULL), Type='character', Default=NULL)
        MixedEffects_ModelID <- Quantico:::ReturnParam(xx=tryCatch({input[['MixedEffects_ModelID']]}, error=function(x) NULL), Type='character', Default=NULL)
        MixedEffects_FixedEffects <- Quantico:::ReturnParam(xx=tryCatch({input[['MixedEffects_FixedEffects']]}, error=function(x) NULL), Type='character', Default=NULL)
        MixedEffects_RandomEffects <- Quantico:::ReturnParam(xx=tryCatch({input[['MixedEffects_RandomEffects']]}, error=function(x) NULL), Type='character', Default=NULL)
        MixedEffects_PrimaryDateColumn <- Quantico:::ReturnParam(xx=tryCatch({input[['MixedEffects_PrimaryDateColumn']]}, error=function(x) NULL), Type='character', Default=NULL)
        MixedEffects_WeightsColumn <- Quantico:::ReturnParam(xx=tryCatch({input[['MixedEffects_WeightsColumn']]}, error=function(x) NULL), Type='character', Default=NULL)
        MixedEffects_InterceptInclude <- Quantico:::ReturnParam(xx=tryCatch({input[['MixedEffects_InterceptInclude']]}, error=function(x) NULL), Type='logical', Default=TRUE)

        MixedEffects_Nest <- Quantico:::ReturnParam(xx=tryCatch({input[['MixedEffects_Nest']]}, error=function(x) NULL), Type='character', Default=NULL)
        MixedEffects_CollapseEPV <- Quantico:::ReturnParam(xx=tryCatch({input[['MixedEffects_CollapseEPV']]}, error=function(x) NULL), Type='logical', Default=TRUE)
        MixedEffects_KeepSubStats <- Quantico:::ReturnParam(xx=tryCatch({input[['MixedEffects_Lmer']]}, error=function(x) NULL), Type='logical', Default=TRUE)
        MixedEffects_Lmer <- Quantico:::ReturnParam(xx=tryCatch({input[['MixedEffects_Lmer']]}, error=function(x) NULL), Type='logical', Default=TRUE)
        MixedEffects_LmerType <- Quantico:::ReturnParam(xx=tryCatch({input[['MixedEffects_LmerType']]}, error=function(x) NULL), Type='character', Default='add')
        MixedEffects_FastMixedEffects <- Quantico:::ReturnParam(xx=tryCatch({input[['MixedEffects_FastMixedEffects']]}, error=function(x) NULL), Type='logical', Default=TRUE)
        MixedEffects_PolyN <- Quantico:::ReturnParam(xx=tryCatch({input[['MixedEffects_PolyN']]}, error=function(x) NULL), Type='numeric', Default=NULL)

        if(Debug) {
          print(MixedEffects_TargetColumnName)
          print(MixedEffects_TargetType)
          print(MixedEffects_ModelID)
          print(MixedEffects_FixedEffects)
          print(MixedEffects_RandomEffects)
          print(MixedEffects_PrimaryDateColumn)
          print(MixedEffects_WeightsColumn)
          print(MixedEffects_InterceptInclude)
        }

        # Build Model
        Output <- Quantico:::MEOW(
          data = data,
          TargetType = MixedEffects_TargetType,
          TargetVariable = MixedEffects_TargetColumnName,
          RandomEffects = MixedEffects_RandomEffects,
          FixedEffects = MixedEffects_FixedEffects,
          Nest = MixedEffects_Nest,
          CollapseEPV = MixedEffects_CollapseEPV,
          KeepSubStats = MixedEffects_KeepSubStats,
          Lmer = MixedEffects_Lmer,
          LmerType = MixedEffects_LmerType,
          Fme = MixedEffects_FastMixedEffects,
          PolyN = MixedEffects_PolyN,
          Debug = Debug)

        if(length(Output) > 0L) {
          DataList[['data']] <- Output$data
          ML_ExperimentTable <<- DataList[['data']]

          if(length(MixedEffects_Lmer) > 0L && MixedEffects_Lmer) {
            DataList[['MixedEffects_Effects']][['data']] <- Output$Lmer$EffectsInterval()
            ML_ExperimentTable <- DataList[['MixedEffects_Effects']][['data']]
            DataList[['MixedEffects_PredictionInterval']][['data']] <- Output$Lmer$PredictionInterval()
          }

          DataList <<- DataList

          if(Debug) print(ML_ExperimentTable)

          # Code Collection
          MachineLearningCode <- Quantico:::Shiny.CodePrint.Collect(y = MachineLearningCode, x = paste0(
            "\n",
            "# Build Model\n",
            "Output <- AutoQuant:::MEOW(\n  ",
            "data = DataList[['data']],\n  ",
            "TargetType = ", Quantico:::CEP(MixedEffects_TargetType),",\n  ",
            "TargetVariable = ", Quantico:::CEP(MixedEffects_TargetColumnName),",\n  ",
            "RandomEffects = ", Quantico:::ExpandText(MixedEffects_RandomEffects), ",\n  ",
            "FixedEffects = ", Quantico:::ExpandText(MixedEffects_FixedEffects), ",\n  ",
            "Nest = ", Quantico:::ExpandText(MixedEffects_Nest), ",\n  ",
            "CollapseEPV = ", Quantico:::CEPP(MixedEffects_CollapseEPV), ",\n  ",
            "KeepSubStats = ", Quantico:::CEPP(MixedEffects_KeepSubStats), ",\n  ",
            "Lmer = ", Quantico:::CEPP(MixedEffects_Lmer), ",\n  ",
            "LmerType = ", Quantico:::CEP(MixedEffects_LmerType), ",\n  ",
            "Fme = ", Quantico:::CEPP(MixedEffects_FastMixedEffects), ",\n  ",
            "PolyN = ", Quantico:::CEPP(MixedEffects_PolyN), ",\n  ",
            "Debug = FALSE)\n\n"))

          shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

        } else {
          shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Model Failed", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
        }

      } else {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Missing Data", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      }
    })
  }, ignoreInit = TRUE)

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event :: ML Scoring           ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(input$ScoreML_Run, {
    if(Debug) {
      print("ML Scoring 1")
      print(length(DataList))
    }
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()
    if(length(DataList) > 0L) {
      if(Debug) {
        print("ML Scoring 2")
        Output <- Quantico::Shiny.ML.Scoring(
          input,
          output,
          DataList = DataList,
          CodeList = MachineLearningCode,
          MOL = ModelOutputList,
          Debug = Debug)
      } else {
        Output <- tryCatch({Quantico::Shiny.ML.Scoring(
          input,
          output,
          DataList = DataList,
          CodeList = MachineLearningCode,
          MOL = ModelOutputList,
          Debug = Debug)}, error = function(x) NULL)
      }

      if(Debug) {
        print("ML Scoring 3")
        print(length(Output) > 0L)
      }
      if(length(Output) > 0L) {
        DataList <- Output$DataList; DataList <<- DataList
        MachineLearningCode <- Output$CodeList; MachineLearningCode <<- MachineLearningCode
        for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
        Quantico::SelectizeInput(session, input, Update = TRUE, InputID = "ScoreML_Data", Label = 'Select Data', Choices = names(DataList))
        for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
        shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      }
    }
  }, ignoreInit = TRUE)

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event :: SS Forecasting       ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # TBATS
  shiny::observeEvent(input$BuildModels_TBATS, {

    if(DebugFC) print('TBATS FC 1')

    # Code Collection
    if(!exists('ForecastingCode')) ForecastingCode <- list()

    # Build Forecast
    shiny::withProgress(message = 'TBATS FC has begun..', value = 0, {

      # TBATSFC stores ArgsList from AutoTBATS()
      # ModelOutputList stores many ArgsList's across builds and sessions
      # TBATSFC will get recycled upon running in RunMode = Train
      #   Otherwise, TBATSFC should persist
      #   data gets removed from ArgsList after Train runs
      #   ArgsList$Model is NULLd out before Retrain, but it persists in ModelOutputList
      # Start off NULL. If not NULL define appropriately
      if(DebugFC) print('TBATS FC 2')
      if(!exists('TBATSFC')) TBATSFC <- NULL
      if(!exists('ModelOutputList')) ModelOutputList <- NULL

      # Grab TBATSFC if user requested via ArgsList input
      # If a user can request one it must be available. Error handling just in case
      alname <- Quantico::ReturnParam(xx = input[['TBATS_ArgsList']], Type = 'character', Debug = DebugFC)
      runm   <- Quantico::ReturnParam(xx = input[['TBATS_RunMode']], Type = 'character', Debug = DebugFC)
      if(length(alname) == 0L && runm %in% c("Forecast")) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Please supply a value to ArgsList for this RunMode", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else {

        # NULL out TBATS or set it to ArgsList from ModelOutputList
        if(DebugFC) print('TBATS FC 4')
        check <- length(ModelOutputList) > 0L && length(alname) > 0L && runm == "Forecast"
        Skipp <- FALSE
        if(check) {
          if(DebugFC) print('TBATS FC 5.a'); TBATSFC <- ModelOutputList[[alname]]; TBATSFC <<- TBATSFC
          if(length(TBATSFC) == 0L) Skipp <- TRUE
        } else {
          if(DebugFC) print('TBATS FC 5.b'); TBATSFC <- list(); TBATSFC <<- TBATSFC
        }

        if(!Skipp) {

          # Run FC System
          if(DebugFC) {
            Output <- Quantico::Shiny.FC.SS(input, output, DataList, TBATSFC, ForecastingCode, DebugFC, Algo = 'TBATS', wd = WorkingDirectory)
          } else {
            Output <- tryCatch({Quantico::Shiny.FC.SS(input, output, DataList, TBATSFC, ForecastingCode, DebugFC, Algo = 'TBATS', wd = WorkingDirectory)}, error = function(x) NULL)
          }

          # Collect Results and update Inputs
          if(length(Output) > 0L) {

            # Get remaining elements
            if(DebugFC) print('TBATS FC 6')
            ForecastingCode <- Output$CodeList; ForecastingCode <<- ForecastingCode; RunMode <- Output$RunMode;
            DataList <- Output$DataList; DataList <<- DataList; TBATSFC <- Output$ArgsList; TBATSFC <<- TBATSFC

            # TBATSFC will be NULL for Backtest Simple Loop.
            # Intent isn't to store a model in that case but to
            # see which parameter set is best to train a keeper
            if(DebugFC) print('TBATS FC 7 DONE')
            if(length(TBATSFC) > 0L) {
              ModelOutputList[[paste0("TBATS_", TBATSFC$ModelID)]] <- TBATSFC
              ModelOutputList <<- ModelOutputList; rm(Output); gc()
              for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection",i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
              Quantico:::SelectizeInput(session = session, input = input, Update = TRUE, InputID = "TBATS_ArgsList", Label = "Tuning Grid", Choices = names(ModelOutputList), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 100L)
              shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0('Model: ', TBATSFC$TBATS_ModelID, "\n", TBATSFC$TBATS_RunMode, " ran successfully"), type = NULL, btn_labels = paste0("Success"), btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            } else {
              shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("Forecasting task was unsuccessful"), type = NULL, btn_labels = paste0("Success"), btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            }

          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Forecasting task was unsuccessful", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
          }
        }

      }
    })
  }, ignoreInit = TRUE)

  # SARIMA
  shiny::observeEvent(input$BuildModels_SARIMA, {

    if(DebugFC) print('SARIMA FC 1')

    # Code Collection
    if(!exists('ForecastingCode')) ForecastingCode <- list()

    # Build Forecast
    shiny::withProgress(message = 'SARIMA FC has begun..', value = 0, {

      # SARIMAFC stores ArgsList from AutoSARIMA()
      # ModelOutputList stores many ArgsList's across builds and sessions
      # SARIMAFC will get recycled upon running in RunMode = Train
      #   Otherwise, SARIMAFC should persist
      #   data gets removed from ArgsList after Train runs
      #   ArgsList$Model is NULLd out before Retrain, but it persists in ModelOutputList
      # Start off NULL. If not NULL define appropriately
      if(DebugFC) print('SARIMA FC 2')
      if(!exists('SARIMAFC')) SARIMAFC <- NULL
      if(!exists('ModelOutputList')) ModelOutputList <- NULL

      # Grab SARIMAFC if user requested via ArgsList input
      # If a user can request one it must be available. Error handling just in case
      alname <- Quantico::ReturnParam(xx = input[['SARIMA_ArgsList']], Type = 'character', Debug = DebugFC)
      runm   <- Quantico::ReturnParam(xx = input[['SARIMA_RunMode']], Type = 'character', Debug = DebugFC)
      if(length(alname) == 0L && runm %in% c("Forecast")) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Please supply a value to ArgsList for this RunMode", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else {

        # NULL out SARIMAFC or set it to ArgsList from ModelOutputList
        if(DebugFC) print('SARIMA FC 4')
        print(alname)
        check <- length(ModelOutputList) > 0L && length(alname) > 0L && runm == "Forecast"
        Skipp <- FALSE
        if(check) {
          if(DebugFC) print('SARIMA FC 5.a'); SARIMAFC <- ModelOutputList[[alname]]; SARIMAFC <<- SARIMAFC
          if(length(SARIMAFC) == 0L) Skipp <- TRUE
        } else {
          if(DebugFC) print('SARIMA FC 5.b'); SARIMAFC <- list(); SARIMAFC <<- SARIMAFC
        }

        if(!Skipp) {

          # Run FC System
          if(DebugFC) {
            Output <- Quantico::Shiny.FC.SS(input, output, DataList, SARIMAFC, ForecastingCode, DebugFC, Algo = 'SARIMA', wd = WorkingDirectory)
          } else {
            Output <- tryCatch({Quantico::Shiny.FC.SS(input, output, DataList, SARIMAFC, ForecastingCode, DebugFC, Algo = 'SARIMA', wd = WorkingDirectory)}, error = function(x) NULL)
          }

          # Collect Results and update Inputs
          if(length(Output) > 0L) {

            # Get remaining elements
            if(DebugFC) print('SARIMA FC 6')
            ForecastingCode <- Output$CodeList; ForecastingCode <<- ForecastingCode; RunMode <- Output$RunMode;
            DataList <- Output$DataList; DataList <<- DataList; SARIMAFC <- Output$ArgsList; SARIMAFC <<- SARIMAFC

            # SARIMAFC will be NULL for Backtest Simple Loop.
            # Intent isn't to store a model in that case but to
            # see which parameter set is best to train a keeper
            if(DebugFC) print('SARIMA FC 7 DONE')
            if(length(SARIMAFC) > 0L) {
              ModelOutputList[[paste0("SARIMA_", SARIMAFC$ModelID)]] <- SARIMAFC
              ModelOutputList <<- ModelOutputList; rm(Output); gc()
              for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection",i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
              Quantico:::SelectizeInput(session = session, input = input, Update = TRUE, InputID = "SARIMA_ArgsList", Label = "Tuning Grid", Choices = names(ModelOutputList), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 100L)
              shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0('Model: ', SARIMAFC$SARIMA_ModelID, "\n", SARIMAFC$SARIMA_RunMode, " ran successfully"), type = NULL, btn_labels = paste0("Success"), btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            } else {
              shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("Forecasting task was unsuccessful"), type = NULL, btn_labels = paste0("Success"), btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            }

          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Forecasting task was unsuccessful", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
          }
        }

      }
    })
  }, ignoreInit = TRUE)

  # ETS
  shiny::observeEvent(input$BuildModels_ETS, {

    if(DebugFC) print('ETS FC 1')

    # Code Collection
    if(!exists('ForecastingCode')) ForecastingCode <- list()

    # Build Forecast
    shiny::withProgress(message = 'ETS FC has begun..', value = 0, {

      # ETSFC stores ArgsList from AutoETS()
      # ModelOutputList stores many ArgsList's across builds and sessions
      # ETSFC will get recycled upon running in RunMode = Train
      #   Otherwise, ETSFC should persist
      #   data gets removed from ArgsList after Train runs
      #   ArgsList$Model is NULLd out before Retrain, but it persists in ModelOutputList
      # Start off NULL. If not NULL define appropriately
      if(DebugFC) print('ETS FC 2')
      if(!exists('ETSFC')) ETSFC <- NULL
      if(!exists('ModelOutputList')) ModelOutputList <- NULL

      # Grab ETSFC if user requested via ArgsList input
      # If a user can request one it must be available. Error handling just in case
      alname <- Quantico::ReturnParam(xx = input[['ETS_ArgsList']], Type = 'character', Debug = DebugFC)
      runm   <- Quantico::ReturnParam(xx = input[['ETS_RunMode']], Type = 'character', Debug = DebugFC)
      if(length(alname) == 0L && runm %in% c("Forecast")) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Please supply a value to ArgsList for this RunMode", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else {

        # NULL out ETS or set it to ArgsList from ModelOutputList
        if(DebugFC) print('ETS FC 4')
        check <- length(ModelOutputList) > 0L && length(alname) > 0L && runm == "Forecast"
        Skipp <- FALSE
        if(check) {
          if(DebugFC) print('ETS FC 5.a'); ETSFC <- ModelOutputList[[alname]]; ETSFC <<- ETSFC
          if(length(ETSFC) == 0L) Skipp <- TRUE
        } else {
          if(DebugFC) print('ETS FC 5.b'); ETSFC <- list(); ETSFC <<- ETSFC
        }

        if(!Skipp) {

          # Run FC System
          if(DebugFC) {
            Output <- Quantico::Shiny.FC.SS(input, output, DataList, ETSFC, ForecastingCode, DebugFC, Algo = 'ETS', wd = WorkingDirectory)
          } else {
            Output <- tryCatch({Quantico::Shiny.FC.SS(input, output, DataList, ETSFC, ForecastingCode, DebugFC, Algo = 'ETS', wd = WorkingDirectory)}, error = function(x) NULL)
          }

          # Collect Results and update Inputs
          if(length(Output) > 0L) {

            # Get remaining elements
            if(DebugFC) print('ETS FC 6')
            ForecastingCode <- Output$CodeList; ForecastingCode <<- ForecastingCode; RunMode <- Output$RunMode;
            DataList <- Output$DataList; DataList <<- DataList; ETSFC <- Output$ArgsList; ETSFC <<- ETSFC

            # ETSFC will be NULL for Backtest Simple Loop.
            # Intent isn't to store a model in that case but to
            # see which parameter set is best to train a keeper
            if(DebugFC) print('ETS FC 7 DONE')
            if(length(ETSFC) > 0L) {
              ModelOutputList[[paste0("ETS_", ETSFC$ModelID)]] <- ETSFC
              ModelOutputList <<- ModelOutputList; rm(Output); gc()
              for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection",i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
              Quantico:::SelectizeInput(session = session, input = input, Update = TRUE, InputID = "ETS_ArgsList", Label = "Tuning Grid", Choices = names(ModelOutputList), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 100L)
              shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0('Model: ', ETSFC$ETS_ModelID, "\n", ETSFC$ETS_RunMode, " ran successfully"), type = NULL, btn_labels = paste0("Success"), btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            } else {
              shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("Forecasting task was unsuccessful"), type = NULL, btn_labels = paste0("Success"), btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            }

          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Forecasting task was unsuccessful", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
          }
        }

      }
    })
  }, ignoreInit = TRUE)

  # ARFIMA
  shiny::observeEvent(input$BuildModels_ARFIMA, {

    if(DebugFC) print('ARFIMA FC 1')

    # Code Collection
    if(!exists('ForecastingCode')) ForecastingCode <- list()

    # Build Forecast
    shiny::withProgress(message = 'ARFIMA FC has begun..', value = 0, {

      # ARFIMAFC stores ArgsList from AutoARFIMA()
      # ModelOutputList stores many ArgsList's across builds and sessions
      # ARFIMAFC will get recycled upon running in RunMode = Train
      #   Otherwise, ARFIMAFC should persist
      #   data gets removed from ArgsList after Train runs
      #   ArgsList$Model is NULLd out before Retrain, but it persists in ModelOutputList
      # Start off NULL. If not NULL define appropriately
      if(DebugFC) print('ARFIMA FC 2')
      if(!exists('ARFIMAFC')) ARFIMAFC <- NULL
      if(!exists('ModelOutputList')) ModelOutputList <- NULL

      # Grab ARFIMAFC if user requested via ArgsList input
      # If a user can request one it must be available. Error handling just in case
      alname <- Quantico::ReturnParam(xx = input[['ARFIMA_ArgsList']], Type = 'character', Debug = DebugFC)
      runm   <- Quantico::ReturnParam(xx = input[['ARFIMA_RunMode']], Type = 'character', Debug = DebugFC)
      if(length(alname) == 0L && runm %in% c("Forecast")) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Please supply a value to ArgsList for this RunMode", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else {

        # ArgsList Management:
        #   Forecast run mode requires a supplied ModelID in the ArgsList dropdown at the top of the UI page
        #     ARFIMAFC: named list for model parameters, data, and other metadata needed to encapsulate the related objects
        #     ModelOutputList: main list for storing models in Quantico. Can contain ML models, Panel Models, and Single Series Models
        #   Grid Tune run mode will reset the ARFIMAFC list every time (single use)
        #     Model artifacts will be stored in ModelDropdownList after each run
        if(DebugFC) {
          print('ARFIMA FC 4')
          print(paste0("length(ModelOutputList) > 0L: ", names(ModelOutputList)))
          print(paste0("length(alname) > 0L: ", alname))
          print(paste0("runm == 'Forecast': ", runm))
        }

        check <- length(ModelOutputList) > 0L && length(alname) > 0L && runm == "Forecast"
        Skipp <- FALSE
        if(check) {
          if(DebugFC) print('ARFIMA FC 5.a'); ARFIMAFC <- ModelOutputList[[alname]]; ARFIMAFC <<- ARFIMAFC
          if(length(ARFIMAFC) == 0L) Skipp <- TRUE
        } else {
          if(DebugFC) print('ARFIMA FC 5.b'); ARFIMAFC <- list(); ARFIMAFC <<- ARFIMAFC
        }

        if(!Skipp) {

          # Run FC System
          if(DebugFC) {
            Output <- Quantico::Shiny.FC.SS(input, output, DataList, ARFIMAFC, ForecastingCode, DebugFC, Algo = 'ARFIMA', wd = WorkingDirectory)
          } else {
            Output <- tryCatch({Quantico::Shiny.FC.SS(input, output, DataList, ARFIMAFC, ForecastingCode, DebugFC, Algo = 'ARFIMA', wd = WorkingDirectory)}, error = function(x) NULL)
          }

          # Collect Results and update Inputs
          if(length(Output) > 0L) {

            # Get remaining elements
            if(DebugFC) print('ARFIMA FC 6')
            ForecastingCode <- Output$CodeList; ForecastingCode <<- ForecastingCode; RunMode <- Output$RunMode;
            DataList <- Output$DataList; DataList <<- DataList; ARFIMAFC <- Output$ArgsList; ARFIMAFC <<- ARFIMAFC

            # ARFIMAFC will be NULL for Backtest Simple Loop.
            # Intent isn't to store a model in that case but to
            # see which parameter set is best to train a keeper
            if(DebugFC) print('ARFIMA FC 7 DONE')
            if(length(ARFIMAFC) > 0L) {
              if(DebugFC) print('ARFIMA FC 7 DONE 1')
              ModelOutputList[[paste0("ARFIMA_", ARFIMAFC$ModelID)]] <- ARFIMAFC
              ModelOutputList <<- ModelOutputList; rm(Output); gc()
              for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection",i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
              Quantico:::SelectizeInput(session = session, input = input, Update = TRUE, InputID = "ARFIMA_ArgsList", Label = "Tuning Grid", Choices = names(ModelOutputList), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 100L)
              shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0('Model: ', ARFIMAFC$ARFIMA_ModelID, "\n", ARFIMAFC$ARFIMA_RunMode, " ran successfully"), type = NULL, btn_labels = paste0("Success"), btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            } else {
              if(DebugFC) print('ARFIMA FC 7 DONE 0')
              shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("Forecasting task was unsuccessful"), type = NULL, btn_labels = paste0("Success"), btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            }

          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Forecasting task was unsuccessful", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
          }

        }
      }
    })
  }, ignoreInit = TRUE)

  # NNET
  shiny::observeEvent(input$BuildModels_NNET, {

    if(DebugFC) print('NNET FC 1')

    # Code Collection
    if(!exists('ForecastingCode')) ForecastingCode <- list()

    # Build Forecast
    shiny::withProgress(message = 'NNET FC has begun..', value = 0, {

      # NNETFC stores ArgsList from AutoNNET()
      # ModelOutputList stores many ArgsList's across builds and sessions
      # NNETFC will get recycled upon running in RunMode = Train
      #   Otherwise, NNETFC should persist
      #   data gets removed from ArgsList after Train runs
      #   ArgsList$Model is NULLd out before Retrain, but it persists in ModelOutputList
      # Start off NULL. If not NULL define appropriately
      if(DebugFC) print('NNET FC 2')
      if(!exists('NNETFC')) NNETFC <- NULL
      if(!exists('ModelOutputList')) ModelOutputList <- NULL

      # Grab NNETFC if user requested via ArgsList input
      # If a user can request one it must be available. Error handling just in case
      alname <- Quantico::ReturnParam(xx = input[['NNET_ArgsList']], Type = 'character', Debug = DebugFC)
      runm   <- Quantico::ReturnParam(xx = input[['NNET_RunMode']], Type = 'character', Debug = DebugFC)
      if(length(alname) == 0L && runm %in% c("Forecast")) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Please supply a value to ArgsList for this RunMode", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else {

        # NULL out NNETFC or set it to ArgsList from ModelOutputList
        if(DebugFC) {
          print('NNET FC 4')
          print(names(ModelOutputList))
          print(alname)
          print(runm)
        }

        check <- length(ModelOutputList) > 0L && length(alname) > 0L && runm == "Forecast"
        Skipp <- FALSE
        if(check) {
          if(DebugFC) print('NNET FC 5.a'); NNETFC <- ModelOutputList[[alname]]; NNETFC <<- NNETFC
          if(length(NNETFC) == 0L) Skipp <- TRUE
        } else {
          if(DebugFC) print('NNET FC 5.b'); NNETFC <- list(); NNETFC <<- NNETFC
        }

        if(!Skipp) {

          # Run FC System
          if(DebugFC) {
            Output <- Quantico::Shiny.FC.SS(input, output, DataList, NNETFC, ForecastingCode, DebugFC, Algo = 'NNET', wd = WorkingDirectory)
          } else {
            Output <- tryCatch({Quantico::Shiny.FC.SS(input, output, DataList, NNETFC, ForecastingCode, DebugFC, Algo = 'NNET', wd = WorkingDirectory)}, error = function(x) NULL)
          }

          # Collect Results and update Inputs
          if(length(Output) > 0L) {

            # Get remaining elements
            if(DebugFC) print('NNET FC 6')
            ForecastingCode <- Output$CodeList; ForecastingCode <<- ForecastingCode; RunMode <- Output$RunMode;
            DataList <- Output$DataList; DataList <<- DataList; NNETFC <- Output$ArgsList; NNETFC <<- NNETFC

            # NNETFC will be NULL for Backtest Simple Loop.
            # Intent isn't to store a model in that case but to
            # see which parameter set is best to train a keeper
            if(DebugFC) print('NNET FC 7 DONE')
            if(length(NNETFC) > 0L) {
              ModelOutputList[[paste0("NNET_", NNETFC$ModelID)]] <- NNETFC
              ModelOutputList <<- ModelOutputList; rm(Output); gc()
              print(names(ModelOutputList))
              for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection",i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
              Quantico:::SelectizeInput(session = session, input = input, Update = TRUE, InputID = "NNET_ArgsList", Label = "Tuning Grid", Choices = names(ModelOutputList), SelectedDefault = NULL, Multiple = TRUE, MaxVars = 100L)
              shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0('Model: ', NNETFC$NNET_ModelID, "\n", NNETFC$NNET_RunMode, " ran successfully"), type = NULL, btn_labels = paste0("Success"), btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            } else {
              shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("Forecasting task was unsuccessful"), type = NULL, btn_labels = paste0("Success"), btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
            }

          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Forecasting task was unsuccessful", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
          }
        }

      }
    })
  }, ignoreInit = TRUE)

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event :: Panel Forecasting    ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # CatBoost
  shiny::observeEvent(input$BuildModels_CatBoostCARMA, {

    if(DebugFC) print('CatBoost FC 1')

    # Code Collection
    if(!exists('ForecastingCode')) ForecastingCode <- list()

    # Build Forecast
    shiny::withProgress(message = 'CatBoost FC has begun..', value = 0, {

      # CatBoostFC stores ArgsList from AutoCatBoostCARMA()
      # ModelOutputList stores many ArgsList's across builds and sessions
      # CatBoostFC will get recycled upon running in RunMode = Train
      #   Otherwise, CatBoostFC should persist
      #   data gets removed from ArgsList after Train runs
      #   ArgsList$Model is NULLd out before Retrain, but it persists in ModelOutputList
      # Start off NULL. If not NULL define appropriately
      if(DebugFC) print('CatBoost FC 2')
      if(!exists('CatBoostFC')) CatBoostFC <- NULL
      if(!exists('ModelOutputList')) ModelOutputList <- NULL

      # Grab CatBoostFC if user requested via ArgsList input
      # If a user can request one it must be available. Error handling just in case
      alname <- Quantico::ReturnParam(xx = input[['CatBoostCARMA_ArgsList']], Type = 'character', Debug = DebugFC)
      runm   <- Quantico::ReturnParam(xx = input[['CatBoostCARMA_RunMode']], Type = 'character', Debug = DebugFC)
      if(length(alname) == 0L && runm %in% c("Retrain","Forecast","Forecast+Retrain")) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Please supply a value to ArgsList for this RunMode", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else {

        if(DebugFC) print('CatBoost FC 3')
        check <- length(ModelOutputList) > 0L && length(alname) > 0L
        if(check) {
          if(DebugFC) print('CatBoost FC 4.a'); CatBoostFC <- ModelOutputList[[alname]]; CatBoostFC <<- CatBoostFC
        } else {
          if(DebugFC) print('CatBoost FC 4.b'); CatBoostFC <- list(); CatBoostFC <<- CatBoostFC
        }

        # Run FC System
        if(DebugFC) {
          Output <- Quantico::Shiny.FC.CARMA(input, output, DataList, CatBoostFC, ForecastingCode, DebugFC, Algo = 'CatBoost', wd = WorkingDirectory)
        } else {
          Output <- tryCatch({Quantico::Shiny.FC.CARMA(input, output, DataList, CatBoostFC, ForecastingCode, DebugFC, Algo = 'CatBoost', wd = WorkingDirectory)}, error = function(x) NULL)
        }

        # Collect Results and update Inputs
        if(length(Output) > 0L) {

          # Get remaining elements
          if(DebugFC) print('CatBoost FC 5')
          ForecastingCode <- Output$CodeList; ForecastingCode <<- ForecastingCode; RunMode <- Output$RunMode; RunMode <<- RunMode
          DataList <- Output$DataList; DataList <<- DataList; CatBoostFC <- Output$ArgsList; CatBoostFC <<- CatBoostFC

          # CatBoostFC will be NULL for Backtest Simple Loop.
          # Intent isn't to store a model in that case but to
          # see which parameter set is best to train a keeper
          if(DebugFC) print('CatBoost FC 6 DONE')
          if(length(CatBoostFC) > 0L) {
            ModelOutputList[[CatBoostFC$ModelID]] <- CatBoostFC#; ModelOutputList[[paste0(CatBoostFC$ModelID, "_MLOutput")]] <- CatBoostFC[[paste0(CatBoostFC$ModelID, "_Meta")]]
            ModelOutputList <<- ModelOutputList; rm(Output); gc()
            for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection",i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
            for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0('FCReportsModelSelection',i), Label = 'FC Output', Choices = names(ModelOutputList), Multiple = TRUE, MaxVars = 100L)
            shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0('Model: ', CatBoostFC$CatBoostCARMA_ModelID, "\n", CatBoostFC$CatBoostCARMA_RunMode, " ran successfully"), type = NULL, btn_labels = paste0("Success"), btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("Forecasting task was unsuccessful"), type = NULL, btn_labels = paste0("No Success"), btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
          }

        } else {
          shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Forecasting task was unsuccessful", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
        }
      }
    })
  }, ignoreInit = TRUE)

  # XGBoost
  shiny::observeEvent(input$BuildModels_XGBoostCARMA, {

    if(DebugFC) print('XGBoost FC 1')

    # Code Collection
    if(!exists('ForecastingCode')) ForecastingCode <- list()

    # Build Forecast
    shiny::withProgress(message = 'XGBoost FC has begun..', value = 0, {

      # XGBoostFC stores ArgsList from AutoXGBoostCARMA()
      # ModelOutputList stores many ArgsList's across builds and sessions
      # XGBoostFC will get recycled upon running in RunMode = Train
      #   Otherwise, XGBoostFC should persist
      #   data gets removed from ArgsList after Train runs
      #   ArgsList$Model is NULLd out before Retrain, but it persists in ModelOutputList
      # Start off NULL. If not NULL define appropriately
      if(DebugFC) print('XGBoost FC 2')
      if(!exists('XGBoostFC')) XGBoostFC <- NULL
      if(!exists('ModelOutputList')) ModelOutputList <- NULL

      # Grab XGBoostFC if user requested via ArgsList input
      # If a user can request one it must be available. Error handling just in case
      alname <- Quantico::ReturnParam(xx = input[['XGBoostCARMA_ArgsList']], Type = 'character', Debug = DebugFC)
      runm   <- Quantico::ReturnParam(xx = input[['XGBoostCARMA_RunMode']], Type = 'character', Debug = DebugFC)
      if(length(alname) == 0L && runm %in% c("Retrain","Forecast","Forecast+Retrain")) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Please supply a value to ArgsList for this RunMode", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else {

        if(DebugFC) print('XGBoost FC 4')
        check <- length(ModelOutputList) > 0L && length(alname) > 0L
        if(check) {
          if(DebugFC) print('XGBoost FC 5.a'); XGBoostFC <- ModelOutputList[[alname]]; XGBoostFC <<- XGBoostFC
        } else {
          if(DebugFC) print('XGBoost FC 5.b'); XGBoostFC <- list(); XGBoostFC <<- XGBoostFC
        }

        # Run FC System
        if(DebugFC) {
          Output <- Quantico::Shiny.FC.CARMA(input, output, DataList, XGBoostFC, ForecastingCode, DebugFC, Algo = 'XGBoost', wd = WorkingDirectory)
        } else {
          Output <- tryCatch({Quantico::Shiny.FC.CARMA(input, output, DataList, XGBoostFC, ForecastingCode, DebugFC, Algo = 'XGBoost', wd = WorkingDirectory)}, error = function(x) NULL)
        }

        # Collect Results and update Inputs
        if(length(Output) > 0L) {

          # Get remaining elements
          if(DebugFC) print('XGBoost FC 6')
          ForecastingCode <- Output$CodeList; ForecastingCode <<- ForecastingCode; RunMode <- Output$RunMode; RunMode <<- RunMode
          DataList <- Output$DataList; DataList <<- DataList; XGBoostFC <- Output$ArgsList; XGBoostFC <<- XGBoostFC

          # XGBoostFC will be NULL for Backtest Simple Loop.
          # Intent isn't to store a model in that case but to
          # see which parameter set is best to train a keeper
          if(DebugFC) print('XGBoost FC 7 DONE')
          if(length(XGBoostFC) > 0L) {
            ModelOutputList[[XGBoostFC$ModelID]] <- XGBoostFC; ModelOutputList[[paste0(XGBoostFC$ModelID, "_MLOutput")]] <- XGBoostFC[[paste0(XGBoostFC$ModelID, "_Meta")]]
            ModelOutputList <<- ModelOutputList; rm(Output); gc()
            for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection",i) , Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
            for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0('FCReportsModelSelection',i), Label = 'FC Output', Choices = names(ModelOutputList), Multiple = TRUE, MaxVars = 100L)
            shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0('Model: ', XGBoostFC$XGBoostCARMA_ModelID, "\n", XGBoostFC$XGBoostCARMA_RunMode, " ran successfully"), type = NULL, btn_labels = paste0("Success"), btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("Process ran successfully"), type = NULL, btn_labels = paste0("Success"), btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
          }

        } else {
          shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Forecasting task was unsuccessful", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
        }
      }
    })
  }, ignoreInit = TRUE)

  # LightGBM
  shiny::observeEvent(input$BuildModels_LightGBMCARMA, {

    if(DebugFC) print('LightGBM FC 1')

    # Code Collection
    if(!exists('ForecastingCode')) ForecastingCode <- list()

    # Build Forecast
    shiny::withProgress(message = 'LightGBM FC has begun..', value = 0, {

      # LightGBMFC stores ArgsList from AutoLightGBMCARMA()
      # ModelOutputList stores many ArgsList's across builds and sessions
      # LightGBMFC will get recycled upon running in RunMode = Train
      #   Otherwise, LightGBMFC should persist
      #   data gets removed from ArgsList after Train runs
      #   ArgsList$Model is NULLd out before Retrain, but it persists in ModelOutputList
      # Start off NULL. If not NULL define appropriately
      if(DebugFC) print('LightGBM FC 2')
      if(!exists('LightGBMFC')) LightGBMFC <- NULL
      if(!exists('ModelOutputList')) ModelOutputList <- NULL

      # Grab LightGBMFC if user requested via ArgsList input
      # If a user can request one it must be available. Error handling just in case
      alname <- Quantico::ReturnParam(xx = input[['LightGBMCARMA_ArgsList']], Type = 'character', Debug = DebugFC)
      runm   <- Quantico::ReturnParam(xx = input[['LightGBMCARMA_RunMode']], Type = 'character', Debug = DebugFC)
      if(length(alname) == 0L && runm %in% c("Retrain","Forecast","Forecast+Retrain")) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Please supply a value to ArgsList for this RunMode", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      } else {

        if(DebugFC) print('LightGBM FC 4')
        check <- length(ModelOutputList) > 0L && length(alname) > 0L
        if(check) {
          if(DebugFC) print('LightGBM FC 5.a'); LightGBMFC <- ModelOutputList[[alname]]; LightGBMFC <<- LightGBMFC
        } else {
          if(DebugFC) print('LightGBM FC 5.b'); LightGBMFC <- list(); LightGBMFC <<- LightGBMFC
        }

        # Run FC System
        if(DebugFC) {
          Output <- Quantico::Shiny.FC.CARMA(input, output, DataList, LightGBMFC, ForecastingCode, DebugFC, Algo = 'LightGBM', wd = WorkingDirectory)
        } else {
          Output <- tryCatch({Quantico::Shiny.FC.CARMA(input, output, DataList, LightGBMFC, ForecastingCode, DebugFC, Algo = 'LightGBM', wd = WorkingDirectory)}, error = function(x) NULL)
        }

        # Collect Results and update Inputs
        if(length(Output) > 0L) {

          # Get remaining elements
          if(DebugFC) print('LightGBM FC 6')
          ForecastingCode <- Output$CodeList; ForecastingCode <<- ForecastingCode; RunMode <- Output$RunMode; RunMode <<- RunMode
          DataList <- Output$DataList; DataList <<- DataList; LightGBMFC <- Output$ArgsList; LightGBMFC <<- LightGBMFC

          # LightGBMFC will be NULL for Backtest Simple Loop.
          # Intent isn't to store a model in that case but to
          # see which parameter set is best to train a keeper
          if(DebugFC) print('LightGBM FC 7 DONE')
          if(length(LightGBMFC) > 0L) {
            ModelOutputList[[LightGBMFC$ModelID]] <- LightGBMFC; ModelOutputList[[paste0(LightGBMFC$ModelID, "_MLOutput")]] <- LightGBMFC[[paste0(LightGBMFC$ModelID, "_Meta")]]
            ModelOutputList <<- ModelOutputList; rm(Output); gc()
            for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection",i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
            for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0('FCReportsModelSelection',i), Label = 'FC Output', Choices = names(ModelOutputList), Multiple = TRUE, MaxVars = 100L)
            shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0('Model: ', LightGBMFC$LightGBMCARMA_ModelID, "\n", LightGBMFC$LightGBMCARMA_RunMode, " ran successfully"), type = NULL, btn_labels = paste0("Success"), btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
          } else {
            shinyWidgets::sendSweetAlert(session, title = NULL, text = paste0("Process ran successfully"), type = NULL, btn_labels = paste0("Success"), btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
          }

        } else {
          shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Forecasting task was unsuccessful", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
        }
      }
    })
  }, ignoreInit = TRUE)

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event :: Import | Export      ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----

  # Update PostGRE Credentials
  shiny::observeEvent(input$PostGRE_UpdateCredentials, {

    PostGRE_Host_Selected  <- Quantico::ReturnParam(xx = tryCatch({input$PostGRE_Host}, error = function(x) NULL), Type = 'character', Default = 'localhost')
    PostGRE_Port_Selected <- Quantico::ReturnParam(xx = tryCatch({input$PostGRE_Port}, error = function(x) NULL), Type = 'numeric', Default = NULL)
    PostGRE_User_Selected <- Quantico::ReturnParam(xx = tryCatch({input$PostGRE_User}, error = function(x) NULL), Type = 'character', Default = NULL)
    PostGRE_Password_Selected <- Quantico::ReturnParam(xx = tryCatch({input$PostGRE_Password}, error = function(x) NULL), Type = 'character', Default = NULL)

    if(length(PostGRE_Host_Selected) > 0L) {
      PostGRE_Host <- PostGRE_Host_Selected
      PostGRE_Host <<- PostGRE_Host
    }

    if(length(PostGRE_Port_Selected) > 0L) {
      PostGRE_Port <- PostGRE_Port_Selected
      PostGRE_Port <<- PostGRE_Port
    }

    if(length(PostGRE_User_Selected) > 0L) {
      PostGRE_User <- PostGRE_User_Selected
      PostGRE_User <<- PostGRE_User
    }

    if(length(PostGRE_Password_Selected) > 0L) {
      PostGRE_Password <- PostGRE_Password_Selected
      PostGRE_Password <<- PostGRE_Password
    }

    # Check if there is a database connection
    if(length(PostGRE_Host) > 0L && length(PostGRE_Port) > 0L && length(PostGRE_User) > 0L && length(PostGRE_Password) > 0L) {
      PostGRE_DBNames <- tryCatch({Quantico:::DM.pgListDatabases(Host = PostGRE_Host, Port = PostGRE_Port, User = PostGRE_User, Password = PostGRE_Password)}, error = function(x) NULL)
    } else {
      PostGRE_DBNames <- NULL
    }

    PostGRE_DBNames <<- PostGRE_DBNames
    Quantico:::SelectizeInput(Update = TRUE, input = input, session = session, InputID = 'PostGRE_PullTable_DB', Label = 'Select DB', Choices = PostGRE_DBNames, SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    Quantico:::SelectizeInput(Update = TRUE, input = input, session = session, InputID = 'SaveData_DataBaseName', Label='PostGRE Database Name', Choices = PostGRE_DBNames, SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    Quantico:::SelectizeInput(Update = TRUE, input = input, session = session, InputID = 'PostGRE_DropTable_DB', Label = 'Select DB', Choices = PostGRE_DBNames, SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)
    Quantico:::SelectizeInput(Update = TRUE, input = input, session = session, InputID = 'PostGRE_DropDB_DB', Label = 'Select DB', Choices = PostGRE_DBNames, SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1)

    if(length(PostGRE_DBNames) > 0L) {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "No dbnames found", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }

  })

  # Save Session
  shiny::observeEvent(input$SaveSessionButton, {

    # Get Session Name user wants to load # SaveSessionName <- 'fff'
    SaveSessionName <- Quantico:::ReturnParam(xx = tryCatch({input$SaveSessionName}, error = function(x) NULL), Type = 'character', Debug = Debug)
    if(length(SaveSessionName) > 0L) {

      # File Mgt
      if(!dir.exists(paths = file.path(WorkingDirectory, SaveSessionName))) {
        SessionSavePath <- file.path(WorkingDirectory, SaveSessionName)
        dir.create(path = file.path(SessionSavePath))
      } else {
        SessionSavePath <- file.path(WorkingDirectory, SaveSessionName)
      }
      if(Debug) print(paste0('SaveSessionName: ', SaveSessionName))
      dataCheck <- Quantico:::ReturnParam(xx = tryCatch({input$LoadSession_IncludeData}, error = function(x) TRUE), Type = 'logical', Debug = Debug)

      # Side bar Inputs
      SideBarElements <- c("EchartsTheme","EchartsTimeLine","ColorFont","BackgroundImageSelect","CssSelect","BlankPlotBackground")
      SideBarList <- list()
      for(sbe in SideBarElements) {
        SideBarList[[sbe]] <- tryCatch({input[[sbe]]}, error = function(x) NULL)
      }
      SideBarList[['NumPlotTabsCurrent']] <- NumPlotTabsCurrent
      SideBarList <<- SideBarList

      # Plot Button Inputs
      for(i in seq_len(NumPlotsAvailable)) {
        PlotType <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('Plot', i)]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
        if(length(PlotType) > 0L) {
          key <- paste0(PlotType, i)
          PlotDropDown[[key]][[paste0('Symbols', i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('Symbols', i)]]}, error=function(x) NULL), Type='character', Default=NULL)
          PlotDropDown[[key]][[paste0('StockMetric', i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('StockMetric', i)]]}, error=function(x) NULL), Type='character', Default=NULL)
          PlotDropDown[[key]][[paste0('StockTimeAgg', i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('StockTimeAgg', i)]]}, error=function(x) NULL), Type='character', Default='days')
          PlotDropDown[[key]][[paste0('StockDateRange', i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('StockDateRange', i)]]}, error=function(x) NULL), Type='date', Default=c(as.character(Sys.Date()-365), as.character(Sys.Date())))
          PlotDropDown[[key]][[paste0("Plot",i,"_SelectData")]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0("Plot",i,"_SelectData")]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0('Plot',i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('Plot',i)]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0('YVar',i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('YVar',i)]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0('DualYVar',i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('DualYVar',i)]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0('XVar',i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('XVar',i)]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0('ZVar',i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('ZVar',i)]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0('GroupVars',i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('GroupVars',i)]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0("YVarTrans",i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0("YVarTrans",i)]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0("DualYVarTrans",i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0("DualYVarTrans",i)]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0("XVarTrans",i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0("XVarTrans",i)]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0("ZVarTrans",i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0("ZVarTrans",i)]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0("AggMethod",i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0("AggMethod",i)]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0("FacetRows",i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0("FacetRows",i)]]}, error = function(x) NULL), Type='numeric', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0("FacetCols",i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0("FacetCols",i)]]}, error = function(x) NULL), Type='numeric', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0('Levels_',i,'_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('Levels_',i,'_1')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0('Levels_',i,'_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('Levels_',i,'_2')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0('Levels_',i,'_3')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('Levels_',i,'_3')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)

          PlotDropDown[[key]][[paste0("Title",i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0("Title",i)]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0("ShowLabels",i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0("ShowLabels",i)]]}, error = function(x) NULL), Type='logical', Default=FALSE, Debug = TRUE)
          PlotDropDown[[key]][[paste0("YAxisTitle",i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0("YAxisTitle",i)]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0("XAxisTitle",i)]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0("XAxisTitle",i)]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)

          # Filter vars
          PlotDropDown[[key]][[paste0('FilterVariable_',i,'_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterVariable_',i,'_1')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0('FilterLogic_',i,'_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',i,'_1')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          if(length(PlotDropDown[[key]][[paste0('FilterVariable_',i,'_1')]]) > 0L && PlotDropDown[[key]][[paste0('FilterVariable_',i,'_1')]] != "No Data Loaded") {
            FilterVariable1Class <- class(DataList[[PlotDropDown[[key]][[paste0("Plot",i,"_SelectData")]]]][["data"]][[PlotDropDown[[key]][[paste0('FilterVariable_',i,'_1')]]]])[1L]
          } else {
            FilterVariable1Class <- 'numeric'
          }
          if(FilterVariable1Class %in% c("numeric","integer")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_1_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_1_1')]]}, error = function(x) NULL), Type='numeric', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_1_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_1_2')]]}, error = function(x) NULL), Type='numeric', Default=NULL, Debug = TRUE)
          } else if(FilterVariable1Class %in% c("character","factor")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_1_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_1_1')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_1_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_1_2')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          } else if(FilterVariable1Class %in% c("Date","IDate")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_1_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_1_1')]]}, error = function(x) NULL), Type='date', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_1_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_1_2')]]}, error = function(x) NULL), Type='date', Default=NULL, Debug = TRUE)
          } else if(FilterVariable1Class %in% c("IDateTime","POSIXct","POSIXt")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_1_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_1_1')]]}, error = function(x) NULL), Type='posix', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_1_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_1_2')]]}, error = function(x) NULL), Type='posix', Default=NULL, Debug = TRUE)
          } else if(FilterVariable1Class %in% c("logical")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_1_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_1_1')]]}, error = function(x) NULL), Type='logical', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_1_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_1_2')]]}, error = function(x) NULL), Type='logical', Default=NULL, Debug = TRUE)
          }

          PlotDropDown[[key]][[paste0('FilterVariable_',i,'_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterVariable_',i,'_2')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0('FilterLogic_',i,'_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',i,'_2')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          if(length(PlotDropDown[[key]][[paste0('FilterVariable_',i,'_2')]]) > 0L && PlotDropDown[[key]][[paste0('FilterVariable_',i,'_2')]] != "No Data Loaded") {
            FilterVariable2Class <- class(DataList[[PlotDropDown[[key]][[paste0("Plot",i,"_SelectData")]]]][["data"]][[PlotDropDown[[key]][[paste0('FilterVariable_',i,'_2')]]]])[1L]
          } else {
            FilterVariable2Class <- 'numeric'
          }
          if(FilterVariable1Class %in% c("numeric","integer")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_2_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_2_1')]]}, error = function(x) NULL), Type='numeric', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_2_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_2_2')]]}, error = function(x) NULL), Type='numeric', Default=NULL, Debug = TRUE)
          } else if(FilterVariable1Class %in% c("character","factor")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_2_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_2_1')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_2_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_2_2')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          } else if(FilterVariable1Class %in% c("Date","IDate")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_2_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_2_1')]]}, error = function(x) NULL), Type='date', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_2_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_2_2')]]}, error = function(x) NULL), Type='date', Default=NULL, Debug = TRUE)
          } else if(FilterVariable1Class %in% c("IDateTime","POSIXct","POSIXt")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_2_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_2_1')]]}, error = function(x) NULL), Type='posix', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_2_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_2_2')]]}, error = function(x) NULL), Type='posix', Default=NULL, Debug = TRUE)
          } else if(FilterVariable1Class %in% c("logical")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_2_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_2_1')]]}, error = function(x) NULL), Type='logical', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_2_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_2_2')]]}, error = function(x) NULL), Type='logical', Default=NULL, Debug = TRUE)
          }

          PlotDropDown[[key]][[paste0('FilterVariable_',i,'_3')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterVariable_',i,'_3')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0('FilterLogic_',i,'_3')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',i,'_3')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          if(length(PlotDropDown[[key]][[paste0('FilterVariable_',i,'_3')]]) > 0L && PlotDropDown[[key]][[paste0('FilterVariable_',i,'_3')]] != "No Data Loaded") {
            FilterVariable3Class <- class(DataList[[PlotDropDown[[key]][[paste0("Plot",i,"_SelectData")]]]][["data"]][[PlotDropDown[[key]][[paste0('FilterVariable_',i,'_3')]]]])[1L]
          } else {
            FilterVariable3Class <- 'numeric'
          }
          if(FilterVariable1Class %in% c("numeric","integer")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_3_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_3_1')]]}, error = function(x) NULL), Type='numeric', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_3_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_3_2')]]}, error = function(x) NULL), Type='numeric', Default=NULL, Debug = TRUE)
          } else if(FilterVariable1Class %in% c("character","factor")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_3_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_3_1')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_3_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_3_2')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          } else if(FilterVariable1Class %in% c("Date","IDate")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_3_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_3_1')]]}, error = function(x) NULL), Type='date', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_3_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_3_2')]]}, error = function(x) NULL), Type='date', Default=NULL, Debug = TRUE)
          } else if(FilterVariable1Class %in% c("IDateTime","POSIXct","POSIXt")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_3_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_3_1')]]}, error = function(x) NULL), Type='posix', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_3_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_3_2')]]}, error = function(x) NULL), Type='posix', Default=NULL, Debug = TRUE)
          } else if(FilterVariable1Class %in% c("logical")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_3_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_3_1')]]}, error = function(x) NULL), Type='logical', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_3_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_3_2')]]}, error = function(x) NULL), Type='logical', Default=NULL, Debug = TRUE)
          }

          PlotDropDown[[key]][[paste0('FilterVariable_',i,'_4')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterVariable_',i,'_4')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          PlotDropDown[[key]][[paste0('FilterLogic_',i,'_4')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterLogic_',i,'_4')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          if(length(PlotDropDown[[key]][[paste0('FilterVariable_',i,'_4')]]) > 0L && PlotDropDown[[key]][[paste0('FilterVariable_',i,'_4')]] != "No Data Loaded") {
            FilterVariable4Class <- class(DataList[[PlotDropDown[[key]][[paste0("Plot",i,"_SelectData")]]]][["data"]][[PlotDropDown[[key]][[paste0('FilterVariable_',i,'_4')]]]])[1L]
          } else {
            FilterVariable4Class <- 'numeric'
          }
          if(FilterVariable1Class %in% c("numeric","integer")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_4_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_4_1')]]}, error = function(x) NULL), Type='numeric', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_4_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_4_2')]]}, error = function(x) NULL), Type='numeric', Default=NULL, Debug = TRUE)
          } else if(FilterVariable1Class %in% c("character","factor")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_4_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_4_1')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_4_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_4_2')]]}, error = function(x) NULL), Type='character', Default=NULL, Debug = TRUE)
          } else if(FilterVariable1Class %in% c("Date","IDate")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_4_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_4_1')]]}, error = function(x) NULL), Type='date', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_4_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_4_2')]]}, error = function(x) NULL), Type='date', Default=NULL, Debug = TRUE)
          } else if(FilterVariable1Class %in% c("IDateTime","POSIXct","POSIXt")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_4_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_4_1')]]}, error = function(x) NULL), Type='posix', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_4_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_4_2')]]}, error = function(x) NULL), Type='posix', Default=NULL, Debug = TRUE)
          } else if(FilterVariable1Class %in% c("logical")) {
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_4_1')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_4_1')]]}, error = function(x) NULL), Type='logical', Default=NULL, Debug = TRUE)
            PlotDropDown[[key]][[paste0('FilterValue_',i,'_4_2')]] <- Quantico:::ReturnParam(xx=tryCatch({input[[paste0('FilterValue_',i,'_4_2')]]}, error = function(x) NULL), Type='logical', Default=NULL, Debug = TRUE)
          }
        }
      }

      # Plot Panel Inupts
      if(!exists("DragulaChoicesList")) {DragulaChoicesList <- list();DragulaChoicesList <<- DragulaChoicesList}
      for(i in seq_len(NumPlotTabsCurrent)) {
        PlotPanelInputsList[[paste0("PlotWidth",i)]] <- input[[paste0("PlotWidth",i)]]
        PlotPanelInputsList[[paste0("PlotHeight",i)]] <- input[[paste0("PlotHeight",i)]]
        PlotPanelInputsList[[paste0("GridCols",i)]] <- input[[paste0("GridCols",i)]]
        PlotPanelInputsList[[paste0("Number_of_Bins",i)]] <- input[[paste0("Number_of_Bins",i)]]
        PlotPanelInputsList[[paste0("Number_of_Levels",i)]] <- input[[paste0("Number_of_Levels",i)]]
        PlotPanelInputsList[[paste0("FontSize",i)]] <- input[[paste0("FontSize",i)]]
        PlotPanelInputsList <<- PlotPanelInputsList
      }

      # Table Panel Inupts
      if(!exists("TableTabList")) {TableTabList <- list();TableTabList <<- TableTabList}
      TableTabList[["NumTableTabsCurrent"]] <- NumTableTabsCurrent
      for(i in seq_len(NumTableTabsCurrent)) {
        TableTabList[[paste0("DataOutputSelection",i)]] <- input[[paste0("DataOutputSelection",i)]]
        TableTabList[[paste0("GridColsData",i)]] <- input[[paste0("GridColsData",i)]]
        TableTabList[[paste0("MinRows",i)]] <- input[[paste0("MinRows",i)]]
        TableTabList[[paste0("Number_of_Records",i)]] <- input[[paste0("Number_of_Records",i)]]
        TableTabList[[paste0("Shuffle",i)]] <- input[[paste0("Shuffle",i)]]
        TableTabList[[paste0("WrapText",i)]] <- input[[paste0("WrapText",i)]]
        TableTabList[[paste0("Compact",i)]] <- input[[paste0("Compact",i)]]
        TableTabList <<- TableTabList
      }

      # ML Panel Inupts
      if(!exists("MLTabList")) {MLTabList <- list();MLTabList <<- MLTabList}
      MLTabList[["NumMLTabsCurrent"]] <- NumMLTabsCurrent
      for(i in seq_len(NumMLTabsCurrent)) {
        MLTabList[[paste0("MLReportsModelSelection",i)]] <- input[[paste0("MLReportsModelSelection",i)]]
        MLTabList[[paste0("MLOutputSelection",i)]] <- input[[paste0("MLOutputSelection",i)]]
        MLTabList[[paste0("SampleSize",i)]] <- input[[paste0("SampleSize",i)]]
        MLTabList[[paste0("PDPVariables",i)]] <- input[[paste0("PDPVariables",i)]]
        MLTabList[[paste0("TrainDataInclude",i)]] <- input[[paste0("TrainDataInclude",i)]]
        MLTabList[[paste0("GroupVariableInclude",i)]] <- input[[paste0("GroupVariableInclude",i)]]
        MLTabList[[paste0("PlotWidtha",i)]] <- input[[paste0("PlotWidtha",i)]]
        MLTabList[[paste0("PlotHeighta",i)]] <- input[[paste0("PlotHeighta",i)]]
        MLTabList <<- MLTabList
      }

      # EDA Panel Inupts
      if(!exists("EDATabList")) {EDATabList <- list();EDATabList <<- EDATabList}
      EDATabList[["NumEDATabsCurrent"]] <- NumEDATabsCurrent
      for(i in seq_len(NumEDATabsCurrent)) {
        EDATabList[[paste0("EDAData",i)]] <- input[[paste0("EDAData",i)]]
        EDATabList[[paste0("EDAUnivariateVars",i)]] <- input[[paste0("EDAUnivariateVars",i)]]
        EDATabList[[paste0("EDACorrVars",i)]] <- input[[paste0("EDACorrVars",i)]]
        EDATabList[[paste0("EDATrendVars",i)]] <- input[[paste0("EDATrendVars",i)]]
        EDATabList[[paste0("EDADateVar",i)]] <- input[[paste0("EDADateVar",i)]]
        EDATabList[[paste0("EDAGroupVar",i)]] <- input[[paste0("EDAGroupVar",i)]]
        EDATabList[[paste0("PlotWidtheda",i)]] <- input[[paste0("PlotWidtheda",i)]]
        EDATabList[[paste0("PlotHeighteda",i)]] <- input[[paste0("PlotHeighteda",i)]]
        EDATabList <<- EDATabList
      }

      # Inference Panel Inupts
      if(!exists("InferenceTabList")) {InferenceTabList <- list();InferenceTabList <<- InferenceTabList}
      InferenceTabList[["NumInferenceTabsCurrent"]] <- NumInferenceTabsCurrent
      for(i in seq_len(NumInferenceTabsCurrent)) {
        InferenceTabList[[paste0("InferenceReportsModelSelection",i)]] <- input[[paste0("InferenceReportsModelSelection",i)]]
        InferenceTabList[[paste0("Striped",i)]] <- input[[paste0("Striped",i)]]
        InferenceTabList[[paste0("MinRowsa",i)]] <- input[[paste0("MinRowsa",i)]]
        InferenceTabList[[paste0("Compact",i)]] <- input[[paste0("Compact",i)]]
        InferenceTabList[[paste0("Filterable",i)]] <- input[[paste0("Filterable",i)]]
        InferenceTabList[[paste0("PlotWidthinf",i)]] <- input[[paste0("PlotWidthinf",i)]]
        InferenceTabList[[paste0("PlotHeightinf",i)]] <- input[[paste0("PlotHeightinf",i)]]
        InferenceTabList <<- InferenceTabList
      }

      LoadedSessionNumPlots <- NumPlotsAvailable
      LoadedSessionNumPlots <<- LoadedSessionNumPlots

      PlotMap <<- PlotMap
      PlotDropDown <<- PlotDropDown
      NumPlotTabsCurrentList <- NumPlotTabsCurrent
      NumPlotTabsCurrentList <<- NumPlotTabsCurrentList
      QuanticoSession <- list()
      QuanticoSession[['InputListNames']] <- c(
        'SideBarList', # SideBar inputs
        'data', # do I still need this?
        'DataList', # All
        'TableTabList', # Table Tabs
        'MLTabList', # ML Tabs
        'EDATabList', # EDA Tabs
        'InferenceOutputList', # Inference Tabs
        'NumPlotTabsCurrentList', # PLOTTING
        'LoadedSessionNumPlots', # PLOTTING
        'FinanceDataList', # PLOTTING
        'PlotDropDown', # PLOTTING
        'DisplayPlots', # PLOTTING
        'PlotPanelInputsList', # PLOTTING
        'ModelOutputList', # ML
        'CatBoostFCParams', # ML
        'CatBoostMLParams', # ML
        'XGBoostMLParams', # ML
        'LightGBMMLParams', # ML
        'H2O_DRFMLParams', # ML
        'H2O_GBMMLParams', # ML
        'H2O_GLMMLParams', # ML
        'H2O_HGLMMLParams', # ML
        'MixedEffectsMLParams', # ML
        'CausalMediationMLParams', # ML
        'MasterSet', # Code Print
        'FeatureEngineeringCode', # Code Print
        'MachineLearningCode', # Code Print
        'ForecastingCode', # Code Print
        'PlotterCode', # Code Print
        'DataMgtCode', # Code Print
        'DataWranglingCode') # Code Print

      # SessionObjectFull: ensure you can operate on it by checking if it has zero length
      # SessionObjectName: the character string name of the object
      # SessionList: container list for all other list objects that gets saved to file (and loaded)
      SessionList <- list()
      DataListNames <- tryCatch({names(DataList)}, error = function(x) NULL)
      if(length(DataListNames) > 0L) {
        SessionList[['DataListNames']] <- DataListNames
        if(length(DataListNames) > 0L) {
          for(nam in DataListNames) {
            tryCatch({data.table::fwrite(x = DataList[[nam]][['data']], file = file.path(SessionSavePath, paste0(gsub(pattern = '.csv', replacement = '', x = nam), '.csv')))}, error = function(x) print(paste0(nam, " Failed to write")))
          }
        }
      }

      # Save Plotmap data.table
      data.table::fwrite(PlotMap, file = file.path(SessionSavePath, paste0(SaveSessionName, "_PlotMap.csv")))

      # Load up SessionList with all the non-null Tab parameter lists
      for(SessionObjectName in QuanticoSession[['InputListNames']]) {
        if(Debug) print(SessionObjectName)
        if(length(tryCatch({get(SessionObjectName)}, error = function(x) NULL)) > 0L) {
          if(Debug) print(paste0('For Loop iteration: ', SessionObjectName))
          SessionObjectFull <- length(get(SessionObjectName)) > 0L && SessionObjectName != "" && !is.na(SessionObjectName)
          if(Debug) {print(SessionObjectFull); print(length(SessionObjectName)); print(is.na(SessionObjectName))}
          if(SessionObjectFull) SessionList[[SessionObjectName]] <- get(SessionObjectName)
        } else {
          if(Debug) print(paste0('Object is missing: ', SessionObjectName))
        }
      }

      # Save SessionList
      if(length(SessionList) > 0L) {
        if(Debug) print("Save SessionList here")
        if(Debug) print(names(SessionList))
        qs::qsave(x = SessionList, file = file.path(SessionSavePath, SaveSessionName))
      }

      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

    } else {

      shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Please select a session name', type = NULL, btn_labels = "Error", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

    }
  }, ignoreInit = TRUE)

  # Load Session
  shiny::observeEvent(input$LoadSessionButton, {

    shiny::withProgress(message = 'Loading session...', value = 0, {

      # Code Collection
      if(!exists('DataMgtCode')) DataMgtCode <- list()
      if(!exists('DataWranglingCode')) DataWranglingCode <- list()
      if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
      if(!exists('MachineLearningCode')) MachineLearningCode <- list()
      if(!exists('PlotterCode')) PlotterCode <- list()

      # Initialize
      PlotterCode <<- PlotterCode; DataMgtCode <<- DataMgtCode; DataWranglingCode <<- DataWranglingCode; FeatureEngineeringCode <<- FeatureEngineeringCode; MachineLearningCode <<- MachineLearningCode

      # Get Session Name user wants to load
      LoadSessionName <- basename(Quantico:::ReturnParam(xx = tryCatch({input$LoadSessionName}, error = function(x) NULL), Type = 'character', Debug = DebugSessionLoad))
      SessionPath <- file.path(WorkingDirectory, LoadSessionName)
      data_check <- Quantico:::ReturnParam(xx = tryCatch({input$LoadSession_IncludeData}, error = function(x) NULL), Type = 'logical', Debug = DebugSessionLoad, Default = TRUE)

      # If not NULL, load up all the lists
      if(length(LoadSessionName) > 0L) {
        LoadSessionName <- LoadSessionName[1L]
        file_check <- file.exists(paths = SessionPath)
        if(file_check) {
          QuanticoSession <- list()
          QuanticoSession[['InputListNames']] <- c(
            'data', # do I still need this?
            'SideBarList', # Sidebar list
            'DataList', # All
            'TableTabList', # Table Tabs
            'MLTabList', # ML Tabs
            'EDATabList', # EDA Tabs
            'InferenceOutputList', # Inference Tabs
            'NumPlotTabsCurrentList', # PLOTTING
            'LoadedSessionNumPlots', # PLOTTING
            'FinanceDataList', # PLOTTING
            'PlotDropDown', # PLOTTING
            'DisplayPlots', # PLOTTING
            'DisplayRequests', # PLOTTING
            'DragulaSelectedList', # PLOTTING
            'DragulaChoicesList',  # PLOTTING
            'PlotPanelInputsList', # PLOTTING
            'ModelOutputList', # ML
            'CatBoostFCParams', # ML
            'CatBoostMLParams', # ML
            'XGBoostMLParams', # ML
            'LightGBMMLParams', # ML
            'H2O_DRFMLParams', # ML
            'H2O_GBMMLParams', # ML
            'H2O_GLMMLParams', # ML
            'H2O_HGLMMLParams', # ML
            'MixedEffectsMLParams', # ML
            'CausalMediationMLParams', # ML
            'MasterSet', # Code Print
            'FeatureEngineeringCode', # Code Print
            'MachineLearningCode', # Code Print
            'ForecastingCode', # Code Print
            'PlotterCode', # Code Print
            'DataMgtCode', # Code Print
            'DataWranglingCode') # Code Print

          # Flush current objects
          if(DebugSessionLoad) print('Load Session 1: Load up data')

          if(exists('LoadedSessionNumPlots') && length(LoadedSessionNumPlots) > 0L) rm(LoadedSessionNumPlots)
          if(exists('ChoiceUpdate') && length(ChoiceUpdate) > 0L) rm(ChoiceUpdate)

          if(exists('data') && length(data) > 0L) rm(data)
          if(exists('SideBarList') && length(SideBarList) > 0L) rm(SideBarList)
          if(exists('DataList') && length(DataList) > 0L) rm(DataList)
          if(exists('PlotMap') && length(PlotMap) > 0L) rm(PlotMap)
          if(exists('NumPlotTabsCurrentList') && length(NumPlotTabsCurrentList) > 0L) rm(NumPlotTabsCurrentList)
          if(exists('NumPlotsAvailable') && length(NumPlotsAvailable) > 0L) rm(NumPlotsAvailable)
          if(exists('FinanceDataList') && length(FinanceDataList) > 0L) rm(FinanceDataList)
          if(exists('PlotDropDown') && length(PlotDropDown) > 0L) rm(PlotDropDown)
          if(exists('DisplayPlots') && length(DisplayPlots) > 0L) rm(DisplayPlots)
          if(exists('DisplayRequests') && length(DisplayRequests) > 0L) rm(DisplayRequests)
          if(exists('DragulaSelectedList') && length(DragulaSelectedList) > 0L) rm(DragulaSelectedList)
          if(exists('DragulaChoicesList') && length(DragulaChoicesList) > 0L) rm(DragulaChoicesList)
          if(exists('PlotPanelInputsList') && length(PlotPanelInputsList) > 0L) rm(PlotPanelInputsList)
          if(exists('ModelOutputList') && length(ModelOutputList) > 0L) rm(ModelOutputList)
          if(exists('CatBoostFCParams') && length(CatBoostFCParams) > 0L) rm(CatBoostFCParams)
          if(exists('CatBoostMLParams') && length(CatBoostMLParams) > 0L) rm(CatBoostMLParams)
          if(exists('XGBoostMLParams') && length(XGBoostMLParams) > 0L) rm(XGBoostMLParams)
          if(exists('LightGBMMLParams') && length(LightGBMMLParams) > 0L) rm(LightGBMMLParams)
          if(exists('H2O_DRFMLParams') && length(H2O_DRFMLParams) > 0L) rm(H2O_DRFMLParams)
          if(exists('H2O_GBMMLParams') && length(H2O_GBMMLParams) > 0L) rm(H2O_GBMMLParams)
          if(exists('H2O_GLMMLParams') && length(H2O_GLMMLParams) > 0L) rm(H2O_GLMMLParams)
          if(exists('H2O_HGLMMLParams') && length(H2O_HGLMMLParams) > 0L) rm(H2O_HGLMMLParams)
          if(exists('MixedEffectsMLParams') && length(MixedEffectsMLParams) > 0L) rm(MixedEffectsMLParams)
          if(exists('CausalMediationMLParams') && length(CausalMediationMLParams) > 0L) rm(CausalMediationMLParams)
          if(exists('MasterSet') && length(MasterSet) > 0L) rm(MasterSet)
          if(exists('FeatureEngineeringCode') && length(FeatureEngineeringCode) > 0L) rm(FeatureEngineeringCode)
          if(exists('MachineLearningCode') && length(MachineLearningCode) > 0L) rm(MachineLearningCode)
          if(exists('ForecastingCode') && length(ForecastingCode) > 0L) rm(ForecastingCode)
          if(exists('PlotterCode') && length(PlotterCode) > 0L) rm(PlotterCode)
          if(exists('DataMgtCode') && length(DataMgtCode) > 0L) rm(DataMgtCode)
          if(exists('DataWranglingCode') && length(DataWranglingCode) > 0L) rm(DataWranglingCode)

          # Load Session Objections
          if(DebugSessionLoad) print("qs::qload(file = LoadName)")
          qs::qload(file = file.path(SessionPath, LoadSessionName))
          for(SessionObjectName in QuanticoSession[['InputListNames']]) if(exists(SessionObjectName) && length(SessionObjectName) > 0L) eval(parse(text = paste0(SessionObjectName, " <<- ", SessionObjectName)))
          if(!exists('DataList')) DataList <- list()
          if(length(DataListNames) > 0L) {
            if(DebugSessionLoad) {print('Load Data Sets'); print(DataListNames)}
            if(length(DataListNames) > 0L) {
              for(nam in DataListNames) {
                if(file.exists(file.path(SessionPath, paste0(gsub(pattern = '.csv', replacement = '', x = nam), '.csv')))) {
                  DataList[[nam]][['data']] <- data.table::fread(file = file.path(SessionPath, paste0(gsub(pattern = '.csv', replacement = '', x = nam), '.csv')))
                  DataList <- tryCatch({Quantico:::DM.DataListUpdate(DataList, nam)}, error = function(x) DataList)
                  DataList <<- DataList
                }
              }
              for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection",i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
            }
          }

          PlotMap <- data.table::fread(file = file.path(SessionPath, paste0(LoadSessionName, "_PlotMap.csv")))
          PlotMap <<- PlotMap

          # object <<- object: for set of objects
          # Push to <<- level environment via metaprogramming
          if(DebugSessionLoad) print('Load Session 2: Load up lists')
          qs::qload(file = file.path(SessionPath, LoadSessionName))
          for(SessionObjectName in QuanticoSession[['InputListNames']]) {
            if(exists(SessionObjectName) && length(SessionObjectName) > 0L) {
              eval(parse(text = paste0(SessionObjectName, " <<- ", SessionObjectName)))
            }
          }

          # Table Panels
          NumTableTabsCurrentLoad <- TableTabList[["NumTableTabsCurrent"]]
          NumTableTabsCurrentLoad <<- NumTableTabsCurrentLoad
          if(NumTableTabsCurrentLoad > NumTableTabsCurrent) {
            for(TableTabs in NumTableTabsCurrentLoad - NumTableTabsCurrent) {
              shinyjs::click(id = "NewDataTab")
            }
          }

          NumTableTabsCurrent <- NumTableTabsCurrentLoad
          NumTableTabsCurrent <<- NumTableTabsCurrent

          # Table Tabs Update
          for(i in seq_len(NumTableTabsCurrent)) {
            Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0('DataOutputSelection',i), Label = 'Display Data', Choices = NULL, SelectedDefault = TableTabList[[paste0("DataOutputSelection",i)]], Multiple = TRUE, MaxVars = 100L)
            shiny::updateNumericInput(session = session, inputId = paste0("GridColsData",i), label = "Columns", value = TableTabList[[paste0("GridColsData",i)]], min = 1L, max = 6L, step = 1L)
            shiny::updateNumericInput(session = session, inputId = paste0("MinRows",i), label = "Records per Page", value = TableTabList[[paste0("MinRows",i)]], min = 10L, max = 1000L, step = 10L)
            shiny::updateNumericInput(session = session, inputId = paste0("Number_of_Records",i), label = "Total Rows", value = TableTabList[[paste0("Number_of_Records",i)]], min = 1000L, max = 100000L, step = 1000L)
            shinyWidgets::updateMaterialSwitch(session = session, inputId = paste0("Shuffle",i), value = TableTabList[[paste0("Shuffle",i)]])
            shinyWidgets::updateMaterialSwitch(session = session, inputId = paste0("WrapText",i), value = TableTabList[[paste0("WrapText",i)]])
            shinyWidgets::updateMaterialSwitch(session = session, inputId = paste0("Compact",i), value = TableTabList[[paste0("Compact",i)]])
          }

          # ML Tabs Update
          for(i in seq_len(NumMLTabsCurrent)) {
            MLModelSelection <- MLTabList[[paste0("MLReportsModelSelection",i)]]
            print(MLModelSelection)
            ChoiceList <- list()
            d <- tryCatch({ModelOutputList[[MLModelSelection]]$TestData}, error = function(x) NULL)
            if(data.table::is.data.table(d)) {
              ColTypes <- unique(Quantico:::ColTypes(d))
              for(j in seq_along(ColTypes)) ChoiceList[[ColTypes[j]]] <- Quantico:::ColNameFilter(d, Types = ColTypes[j])
              Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0('MLReportsModelSelection',i), Label = 'Model Selection', Choices = ChoiceList, SelectedDefault = MLModelSelection, Multiple = FALSE, MaxVars = 1L)
              Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0("MLOutputSelection",i), Label = "Evaluation Objects", Choices = c("Model Comparison Metrics", "Evaluation Plots", "Partial Dependence Plots"), SelectedDefault = MLTabList[[paste0("MLOutputSelection",i)]], Multiple = TRUE, MaxVars = 100L)
              Quantico::NumericInput(session = session, input = input, Update = TRUE, InputID = paste0("SampleSize", i), Label = 'Sample Size', Value = MLTabList[[paste0("SampleSize",i)]], Min = 1, Max = 10000000, Step = 10000)
              Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0('PDPVariables',i), Label = 'PDP Variables', Choices = ChoiceList, SelectedDefault = MLTabList[[paste0("PDPVariables",i)]], Multiple = TRUE, MaxVars = 100L)
              Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0("TrainDataInclude", i), Label = 'Include Train Data?', Choices = c(TRUE,FALSE), SelectedDefault = MLTabList[[paste0("TrainDataInclude",i)]], Multiple = FALSE)
              Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0('GroupVariableInclude',i), Label = 'By-Variable Include?', Choices = ChoiceList, SelectedDefault = MLTabList[[paste0("GroupVariableInclude",i)]], Multiple = TRUE, MaxVars = 1L)
            }
            shiny::updateSliderInput(session = session, inputId = paste0("PlotWidtha",i), label = 'Plot Width', value = MLTabList[[paste0("PlotWidtha",i)]], min = 100, max = 2800)
            shiny::updateSliderInput(session = session, inputId = paste0("PlotHeighta",i), label = 'Plot Height', value = MLTabList[[paste0("PlotHeighta",i)]], min = 100, max = 2800)
          }

          # EDA Tabs Update
          for(i in seq_len(NumEDATabsCurrent)) {
            EDAData <- EDATabList[[paste0("EDAData",i)]]
            ChoiceList <- list()
            dd <- tryCatch({DataList[[EDAData]][['data']]}, error = function(x) NULL)
            if(data.table::is.data.table(dd)) {
              ColTypes <- unique(Quantico:::ColTypes(dd))
              for(j in seq_along(ColTypes)) ChoiceList[[ColTypes[j]]] <- Quantico:::ColNameFilter(dd, Types = ColTypes[j])
              Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0('EDAData',i), Label = 'Data Selection', Choices = tryCatch(names(DataList), error = function(x) NULL), SelectedDefault = EDATabList[[paste0("EDAData",i)]], Multiple = TRUE, MaxVars = 1L)
              Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0('EDAUnivariateVars',i), Label = 'Univariate Vars', Choices = ChoiceList, SelectedDefault = EDATabList[[paste0("EDAUnivariateVars",i)]], Multiple = TRUE, MaxVars = 100L)
              Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0("EDACorrVars", i), Label = 'Corr Vars', Choices = ChoiceList, SelectedDefault = EDATabList[[paste0("EDACorrVars",i)]], Multiple = TRUE, MaxVars = 100L)
              Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0('EDATrendVars',i), Label = 'Trend Vars', Choices = ChoiceList, SelectedDefault = EDATabList[[paste0("EDATrendVars",i)]], Multiple = TRUE, MaxVars = 100L)
              Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0("EDADateVar", i), Label = 'Trend Date Var', Choices = ChoiceList, SelectedDefault = EDATabList[[paste0("EDADateVar",i)]], Multiple = TRUE, MaxVars = 1L)
              Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0('EDAGroupVar',i), Label = 'Trend By-Variable', Choices = ChoiceList, SelectedDefault = EDATabList[[paste0("EDAGroupVar",i)]], Multiple = TRUE, MaxVars = 1L)
            }
            shiny::updateSliderInput(session = session, inputId = paste0("PlotWidtheda",i), label = 'Plot Width', value = EDATabList[[paste0("PlotWidtheda",i)]], min = 100, max = 2800)
            shiny::updateSliderInput(session = session, inputId = paste0("PlotHeighteda",i), label = 'Plot Height', value = EDATabList[[paste0("PlotHeighteda",i)]], min = 100, max = 2800)
          }

          # Inference Tabs Update
          for(i in seq_len(NumInferenceTabsCurrent)) {
            Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0('InferenceReportsModelSelection',Page), Label = 'Inference Output', Choices = tryCatch({names(InferenceOutputList)}, error = function(x) NULL), SelectedDefault = InferenceTabList[[paste0("InferenceReportsModelSelection",i)]], Multiple = FALSE, MaxVars = 1L)
            shinyWidgets::updateMaterialSwitch(inputId = paste0("Striped", Page), value = InferenceTabList[[paste0("Striped",i)]])
            shiny::updateNumericInput(session = session, inputId = paste0("MinRowsa", Page), label = 'Records per Page', value = InferenceTabList[[paste0("MinRowsa",i)]], min = 10, max = 1000, step = 10)
            shinyWidgets::updateMaterialSwitch(session = session, inputId = paste0("Compact", Page), value = InferenceTabList[[paste0("Compact",i)]])
            shinyWidgets::updateMaterialSwitch(session = session, inputId = paste0("Filterable", Page), value = InferenceTabList[[paste0("Filterable",i)]])
            shiny::updateSliderInput(session = session, inputId = paste0("PlotWidthinf",i), label = 'Plot Width', value = EDATabList[[paste0("PlotWidtheda",i)]], min = 100, max = 2800)
            shiny::updateSliderInput(session = session, inputId = paste0("PlotHeightinf",i), label = 'Plot Height', value = EDATabList[[paste0("PlotHeighteda",i)]], min = 100, max = 2800)
          }

          loadPlotPanelInputList <- PlotPanelInputsList
          loadPlotPanelInputList <<- loadPlotPanelInputList
          PlotDropDown <<- PlotDropDown

          # Plot Tab Creation
          if(DebugSessionLoad) print('Load Session 3: Plot Tabs Add')
          if(exists("SideBarList")) {
            gyg <- SideBarList[['NumPlotTabsCurrent']] - NumPlotTabsCurrent
            if(gyg > 0L) {
              for(i in seq_len(gyg)) {
                shinyjs::click(id = "NewPlotTab")
              }
            }
          }

          # SideBar Elements Updates
          if(DebugSessionLoad) print('Load Session 5: Update Sidebar elements')
          if(exists("SideBarList") && length(SideBarList) > 0L) {
            for(sbe in names(SideBarList)) {
              if(sbe %chin% c("EchartsTheme","EchartsTimeLine","BackgroundImageSelect","CssSelect","BlankPlotBackground")) {
                if(length(SideBarList) > 0L && sbe %in% names(SideBarList)) {
                  if(sbe == "EchartsTheme") {
                    Choices <- EchartThemes
                    Label <- "Theme"
                  } else if(sbe == "EchartsTimeLine") {
                    Choices <- c(TRUE,FALSE)
                    Label <- "Timeline"
                  } else if(sbe == "BackgroundImageSelect") {
                    Choices <- unique(c('None', sort(c('River','FoggyMountains','CatMoon','PalmTreesLake','MountainLake','FoggyTrees','GreenWoods','Liminal-Train','Dock-Mountains-Sunset','SailBoat','Europe','Vacation','BeachCave','Vespa','PerformanceReview','Lightning','Country','Getaway','Outrun','Havana'))))
                    Label <- NULL
                  } else if(sbe == "CssSelect") {
                    Choices <- c('day-light-blue','light-gray','medium-gray','dark-gray','piano-black','yellow','yellow-green','green','green-blue','light-blue','dodger-blue','blue','blue-purple','purple','pink','red')
                    Label <- NULL
                  } else if(sbe == "BlankPlotBackground") {
                    Choices <- c("Poincare","Divergence","Lips", "Space", "Tenet")
                    Label <- NULL
                  }
                  shiny::updateSelectInput(session = session, inputId = sbe, choices = Choices, selected = SideBarList[[sbe]])
                }
              }

              # Plotly Colors
              if(sbe %chin% c("ColorFont")) {
                if(length(SideBarList) > 0L && sbe %in% names(SideBarList)) {
                  if(DebugSessionLoad) print(SideBarList[[sbe]])
                  shiny.fluent::updateColorPicker.shinyInput(session = session, inputId = sbe, value = SideBarList[[sbe]])
                }
              }
            }
          }

          if(length(PlotDropDown) > 0L) {
            for(i in seq_along(PlotDropDown)[-1L]) { # Debug is first element; i = 2
              key <- as.integer(gsub("[^\\d]+", "", names(PlotDropDown)[i], perl=TRUE))
              if(DebugSessionLoad) print("plot 1 only check 2")
              selected_data <- tryCatch({DataList[[PlotDropDown[[i]][[paste0("Plot",key,"_SelectData")]]]][['data']]}, error = function(x) {
                a <- exists("DataList")
                b <- exists("DataList") && length(DataList) > 0L
                c <- exists("DataList") && length(DataList) > 0L && length(DataList[[key]])
                print(paste0("DataList exists", a))
                if(a) print(paste0("DataList length", length(DataList)))
                if(b) print(paste0("length(DataList[[key]]) = ", length(DataList[[key]])))
                if(c) print(paste0("length(DataList[[PlotDropDown[[key]][[paste0('Plot',i,'_SelectData')]]]]) = ", length(tryCatch({DataList[[PlotDropDown[[key]][[paste0("Plot",key,"_SelectData")]]]]}, error = function(x) NULL))))
              })
              if(length(selected_data) > 1L && data.table::is.data.table(selected_data)) {
                Choices <- names(selected_data)
                ChoiceList <- list();ChoiceListSelectize <- list()
                ColTypes <- unique(Quantico:::ColTypes(selected_data))
                for(j in seq_along(ColTypes)) {
                  if(length(ColTypes) >= j) {
                    ChoiceList[[ColTypes[j]]] <- Quantico:::ColNameFilter(selected_data, Types = ColTypes[j])
                    ChoiceListSelectize[[ColTypes[j]]] <- as.list(Quantico:::ColNameFilter(selected_data, Types = ColTypes[j]))
                  }
                }

              } else {
                if(DebugSessionLoad) print("plot 1 only check 9")
                Choices <- NULL
              }

              Key <- names(PlotDropDown)[i]

              Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0("Plot",i-1L,"_SelectData"), Label = "Select Data", Choices = names(DataList), Multiple = TRUE, MaxVars = 1, SelectedDefault = PlotDropDown[[i]][[paste0("Plot",key,"_SelectData")]], Debug = DebugSessionLoad, CloseAfterSelect = TRUE)
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('YVar',i-1), Label = 'Y-Variable', Choices = ChoiceList, Multiple = TRUE, SelectedDefault = PlotDropDown[[i]][[paste0('YVar',i-1)]])
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('DualYVar',i-1), Label = 'Dual Axis Y-Variable', Choices = ChoiceList, Multiple = TRUE, SelectedDefault = PlotDropDown[[i]][[paste0('DualYVar',i-1)]])
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('XVar',i-1), Label = 'X-Variable', Choices = ChoiceListSelectize, Multiple = TRUE, SelectedDefault = PlotDropDown[[i]][[paste0('XVar',i-1)]])
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('ZVar',i-1), Label = 'Z-Variable', Choices = ChoiceList, Multiple = TRUE, SelectedDefault = PlotDropDown[[i]][[paste0('ZVar',i-1)]])
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('PlottingDateVar',i-1), Label = 'Date Variable', Choices = ChoiceList, Multiple = TRUE, SelectedDefault = PlotDropDown[[i]][[paste0('PlottingDateVar',i-1)]])
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('GroupVars',i-1), Label = 'Grouping Variables', Choices = Choices, Multiple = TRUE, SelectedDefault = PlotDropDown[[i]][[paste0('GroupVars',i-1)]])
              Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0("YVarTrans",i-1), Label = "Y-Variable Transformation", Choices = NULL, Multiple = TRUE, MaxVars = 30, SelectedDefault = PlotDropDown[[i]][[paste0('YVarTrans',i-1)]])
              Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0("DualYVarTrans",i-1), Label = "Dual Axis Y-Variable Transformation", Choices = NULL, Multiple = TRUE, MaxVars = 30, SelectedDefault = PlotDropDown[[i]][[paste0('DualYVarTrans',i-1)]])
              Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0("XVarTrans",i-1), Label = "X-Variable Transformation", Choices = NULL, Multiple = TRUE, MaxVars = 30, SelectedDefault = PlotDropDown[[i]][[paste0('XVarTrans',i-1)]])
              Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0("ZVarTrans",i-1), Label = "Z-Variable Transformation", Choices = NULL, Multiple = TRUE, MaxVars = 30, SelectedDefault = PlotDropDown[[i]][[paste0('ZVarTrans',i-1)]])
              Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0("AggMethod",i-1), Label = "Aggregation Method", Choices = c('count','proportion','mean','meanabs','median','medianabs','sum','sumabs','sd','sdabs', 'skewness','skewnessabs', 'kurtosis','kurtosisabs','CoeffVar','CoeffVarabs'), SelectedDefault = PlotDropDown[[i]][[paste0('AggMethod',i-1)]])
              Quantico:::SliderInput(session = session, Update = TRUE, input=input, InputID = paste0("FacetRows",i-1), Step = 1, Value = PlotDropDown[[i]][[paste0('FacetRows',i-1)]], Min = 1, Max = 12)
              Quantico:::SliderInput(session = session, Update = TRUE, input=input, InputID = paste0("FacetCols",i-1), Step = 1, Value = PlotDropDown[[i]][[paste0('FacetCols',i-1)]], Min = 1, Max = 12)
              Quantico:::NumericInput(session = session, Update = TRUE, input=input, InputID = paste0("PlottingMaxLags", i-1), Label = 'Max Lags', Value = PlotDropDown[[i]][[paste0('PlottingMaxLags',i-1)]], Min = 5, Max = 200, Step = 1)
              Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0("PlottingTimeUnit", i-1), Label = "Time Agg Level", Choices = c("hour", "day", "week", "month", "quarter", "year"), SelectedDefault = PlotDropDown[[i]][[paste0('PlottingTimeUnit',i-1)]], Multiple = TRUE, MaxVars = 1)
              sgs <- PlotDropDown[[paste0("GroupVars",i-1)]]
              if(length(sgs) > 0L) {
                Quantico:::PickerInput_GetLevels2(Update = TRUE, session = session, input=input, GroupVariable = sgs[1L], InputID=paste0('Levels_',i-1,'_1'), Choices=Choices, SelectedDefault = PlotDropDown[[i]][[paste0('Levels_',i-1,'_1')]])
                if(length(sgs) > 1L) {
                  Quantico:::PickerInput_GetLevels2(Update = TRUE, session = session, input=input, GroupVariable = sgs[2L], InputID=paste0('Levels_',i-1,'_2'), Choices=Choices, SelectedDefault = PlotDropDown[[i]][[paste0('Levels_',i-1,'_2')]])
                  if(length(sgs) > 2L) {
                    Quantico:::PickerInput_GetLevels2(Update = TRUE, session = session, input=input, GroupVariable = sgs[3L], InputID=paste0('Levels_',i-1,'_3'), Choices=Choices, SelectedDefault = PlotDropDown[[i]][[paste0('Levels_',i-1,'_3')]])
                  }
                }
              }


              Quantico:::TextInput(session = session, Update = TRUE, input=input, InputID = paste0('Title', i-1), Label='Rename Title', Value = PlotDropDown[[i]][[paste0('Title',i-1)]])
              Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0('ShowLabels', i-1), Label='Show Labels', Choices = c(TRUE,FALSE), Multiple=TRUE, MaxVars = 1, SelectedDefault = PlotDropDown[[i]][[paste0('ShowLabels',i-1)]], CloseAfterSelect = TRUE)
              Quantico:::TextInput(session = session, Update = TRUE, input=input, InputID = paste0("YAxisTitle", i-1), Label = "Y-Axis Title ('None' for blank)", Value = PlotDropDown[[i]][[paste0('YAxisTitle',i-1)]])
              Quantico:::TextInput(session = session, Update = TRUE, input=input, InputID = paste0("XAxisTitle", i-1), Label = "X-Axis Title ('None' for blank)", Value = PlotDropDown[[i]][[paste0('XAxisTitle',i-1)]])
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterVariable_',i-1,'_1'), Label = "Filter Variable", Choices = ChoiceList, Multiple = TRUE, SelectedDefault = PlotDropDown[[i]][[paste0('FilterVariable_',i-1,'_1')]])
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterVariable_',i-1,'_2'), Label = "Filter Variable", Choices = ChoiceList, Multiple = TRUE, SelectedDefault = PlotDropDown[[i]][[paste0('FilterVariable_',i-1,'_2')]])
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterVariable_',i-1,'_3'), Label = "Filter Variable", Choices = ChoiceList, Multiple = TRUE, SelectedDefault = PlotDropDown[[i]][[paste0('FilterVariable_',i-1,'_3')]])
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterVariable_',i-1,'_4'), Label = "Filter Variable", Choices = ChoiceList, Multiple = TRUE, SelectedDefault = PlotDropDown[[i]][[paste0('FilterVariable_',i-1,'_4')]])
              Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterLogic_',i-1,'_1'), Label = "Logical Operation", Choices = Choices, Multiple = TRUE, MaxVars = 30, SelectedDefault = PlotDropDown[[i]][[paste0('FilterLogic_',i-1,'_1')]])
              Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterLogic_',i-1,'_2'), Label = "Logical Operation", Choices = Choices, Multiple = TRUE, MaxVars = 30, SelectedDefault = PlotDropDown[[i]][[paste0('FilterLogic_',i-1,'_2')]])
              Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterLogic_',i-1,'_3'), Label = "Logical Operation", Choices = Choices, Multiple = TRUE, MaxVars = 30, SelectedDefault = PlotDropDown[[i]][[paste0('FilterLogic_',i-1,'_3')]])
              Quantico:::SelectizeInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterLogic_',i-1,'_4'), Label = "Logical Operation", Choices = Choices, Multiple = TRUE, MaxVars = 30, SelectedDefault = PlotDropDown[[i]][[paste0('FilterLogic_',i-1,'_4')]])
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterValue_',i-1,'_1_1'), Label = "Value 1", Choices = Choices, Multiple = TRUE, MaxVars = 30, SelectedDefault = PlotDropDown[[i]][[paste0('FilterValue_',i-1,'_1_1')]])
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterValue_',i-1,'_1_2'), Label = "Value 1", Choices = Choices, Multiple = TRUE, MaxVars = 30, SelectedDefault = PlotDropDown[[i]][[paste0('FilterValue_',i-1,'_1_2')]])
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterValue_',i-1,'_2_1'), Label = "Value 1", Choices = Choices, Multiple = TRUE, MaxVars = 30, SelectedDefault = PlotDropDown[[i]][[paste0('FilterValue_',i-1,'_2_1')]])
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterValue_',i-1,'_2_2'), Label = "Value 1", Choices = Choices, Multiple = TRUE, MaxVars = 30, SelectedDefault = PlotDropDown[[i]][[paste0('FilterValue_',i-1,'_2_2')]])
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterValue_',i-1,'_3_1'), Label = "Value 1", Choices = Choices, Multiple = TRUE, MaxVars = 30, SelectedDefault = PlotDropDown[[i]][[paste0('FilterValue_',i-1,'_3_1')]])
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterValue_',i-1,'_3_2'), Label = "Value 1", Choices = Choices, Multiple = TRUE, MaxVars = 30, SelectedDefault = PlotDropDown[[i]][[paste0('FilterValue_',i-1,'_3_2')]])
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterValue_',i-1,'_4_1'), Label = "Value 1", Choices = Choices, Multiple = TRUE, MaxVars = 30, SelectedDefault = PlotDropDown[[i]][[paste0('FilterValue_',i-1,'_4_1')]])
              Quantico:::PickerInput(session = session, Update = TRUE, input=input, InputID = paste0('FilterValue_',i-1,'_4_2'), Label = "Value 1", Choices = Choices, Multiple = TRUE, MaxVars = 30, SelectedDefault = PlotDropDown[[i]][[paste0('FilterValue_',i-1,'_4_2')]])
            }
          }

          # Update Dragula and Plot Output Panel Inputs for all Tabs
          if(DebugSessionLoad) print('Load Session 4: Update Plot Output Panel')
          if(PlotMap[,.N] > 0L) {
            PlotList <- list()
            PlotList[["Standard Plots"]] <- c(
              "",
              'HistogramPlot','DensityPlot','BoxPlot','PiePlot','DonutPlot','RosetypePlot','WordCloud',
              'LinePlot','AreaPlot','StepPlot','RiverPlot',
              'BarPlot','StackedBarPlot','BarPlot3D',
              'HeatMapPlot','CorrelogramPlot','Autocorrelation','PartialAutocorr',
              'ScatterPlot','ScatterPlot3D','CopulaPlot','CopulaPlot3D')
            PlotList[["ML Evaluation"]] <- c(
              'Residuals','ResidScatter',
              'PartialDependenceLine','PartialDependenceHeatMap',
              'CalibrationLine','CalibrationBox',
              'VariableImportance','ShapleyImportance',
              'ROCPlot','ConfusionMatrixHeatmap',
              'GainsPlot','LiftPlot')
            for(OutputTabNumber in seq_len(NumTabs)) {
              if(OutputTabNumber == 1L) {
                DragulaChoices <- as.list(PlotMap[1L:12L][Selected == FALSE &  !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
                DragulaSelected <- list()
                DragulaSelected[["View"]] <- PlotMap[1L:12L][Selected == TRUE][["PlotName"]]
                PlotNamesText <- c(paste0("Plot",1L:12L))
                for(i in 1L:12L) {
                  if(PlotMap[i][["PlotName"]] != "") {
                    Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0("PlotID_", i), Choices = PlotList, SelectedDefault = PlotMap[i][["PlotType"]])
                  } else {
                    Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0("PlotID_", i), Choices = PlotList, SelectedDefault = NULL, PlotSelection = paste0("Plot",i))
                  }
                }

              } else if(OutputTabNumber == 2L) {
                DragulaChoices <- as.list(PlotMap[13L:24L][Selected == FALSE & !PlotName %in% c(paste0("Plot", 1L:24L),"Nothing Selected")][["PlotName"]])
                DragulaSelected <- list()
                DragulaSelected[["View"]] <- PlotMap[13L:24L][Selected == TRUE][["PlotName"]]
                for(i in 13L:24L) {
                  if(PlotMap[i][["PlotName"]] != "") {
                    Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0("PlotID_", i), Choices = PlotList, SelectedDefault = PlotMap[i][["PlotType"]])
                  } else {
                    Quantico::PickerInput(session = session, input = input, Update = TRUE, InputID = paste0("PlotID_", i), Choices = PlotList, SelectedDefault = NULL, PlotSelection = paste0("Plot",i))
                  }
                }
              }
              esquisse::updateDragulaInput(
                session = session,
                inputId = paste0("PlotTypeDragula", OutputTabNumber),
                choices = DragulaChoices,
                selected = DragulaSelected)
              sel <- DragulaSelectedList[[paste0("PlotPanel",1)]]
              sel <<- sel
              Quantico:::SliderInput(session = session, Update = TRUE, server = server, input = input, InputID = paste0("PlotWidth",OutputTabNumber), Value = tryCatch({loadPlotPanelInputList[[paste0("PlotWidth",OutputTabNumber)]]}, error = function(x) 1450), PlotDropDown = loadPlotPanelInputList, Key = paste0("PlotWidth",OutputTabNumber), MainPanel = "bla")
              Quantico:::SliderInput(session = session, Update = TRUE, server = server, input = input, InputID = paste0("PlotHeight",OutputTabNumber), Value = tryCatch({loadPlotPanelInputList[[paste0("PlotHeight",OutputTabNumber)]]}, error = function(x) 950), PlotDropDown = loadPlotPanelInputList, Key = paste0("PlotHeight",OutputTabNumber), MainPanel = "bla")
              Quantico:::SliderInput(session = session, Update = TRUE, server = server, input = input, InputID = paste0("GridCols",OutputTabNumber), Value = tryCatch({loadPlotPanelInputList[[paste0("GridCols",OutputTabNumber)]]}, error = function(x) 1), PlotDropDown = loadPlotPanelInputList, Key = paste0("GridCols",OutputTabNumber), MainPanel = "bla")
              Quantico:::SliderInput(session = session, Update = TRUE, server = server, input = input, InputID = paste0("Number_of_Bins",OutputTabNumber), Value = tryCatch({loadPlotPanelInputList[[paste0("Number_of_Bins",OutputTabNumber)]]}, error = function(x) 20), PlotDropDown = loadPlotPanelInputList, Key = paste0("Number_of_Bins",OutputTabNumber), MainPanel = "bla")
              Quantico:::SliderInput(session = session, Update = TRUE, server = server, input = input, InputID = paste0("Number_of_Levels",OutputTabNumber), Value = tryCatch({loadPlotPanelInputList[[paste0("Number_of_Levels",OutputTabNumber)]]}, error = function(x) 75), PlotDropDown = loadPlotPanelInputList, Key = paste0("Number_of_Levels",OutputTabNumber), MainPanel = "bla")
              Quantico:::SliderInput(session = session, Update = TRUE, server = server, input = input, InputID = paste0("FontSize",OutputTabNumber), Value = tryCatch({loadPlotPanelInputList[[paste0("FontSize",OutputTabNumber)]]}, error = function(x) 14), PlotDropDown = loadPlotPanelInputList, Key = paste0("FontSize",OutputTabNumber), MainPanel = "bla")
            }
          }

          # In order for this to work, on the OE side, you'll need to
          #   have the ReturnParams() setup to pull from a list, not the input
          #   I.e, the DisplayRequests come back NULL
          # shinyjs::click(id = 'TrendPlotExecute1')

          # All done
          shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Session Loaded!", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")

        } else {
          shinyWidgets::sendSweetAlert(session, title = NULL, text = 'File not found', type = NULL, btn_labels = 'Unable to load', btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, width = "40%")
        }

      } else {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = 'Please select a session', type = NULL, btn_labels = "Unable to load", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      }
    })
  }, ignoreInit = TRUE)

  # Import Local CSV
  shiny::observeEvent(input$ImportLocalCSV,    {

    # Code Collection
    if(!exists('DataMgtCode')) DataMgtCode <- list()
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()
    if(!exists('PlotterCode')) PlotterCode <- list()

    # Filepath <- tcltk::tk_choose.files() # desktop version
    Filepath <- tryCatch({input$ImportLocalCSV$datapath}, error = function(x) NULL)
    FilepathName <- tryCatch({input$ImportLocalCSV$name}, error = function(x) NULL)

    # Create an availability awaiting event
    shiny::req(Filepath)

    # See what file path and file name are returned
    if(Debug) print(input$ImportLocalCSV)

    # Load data
    if(length(Filepath) > 0L) {
      DataList[[basename(FilepathName)]][['data']] <- tryCatch({Quantico:::LoadCSV(Filepath)}, error = function(x) NULL)
      if(length(DataList[[basename(FilepathName)]][['data']]) > 0L) {
        DataList[[basename(FilepathName)]][['sample']] <- DataList[[basename(FilepathName)]][['data']][1:min(.N, 1000L)]
        DataList <<- DataList
        DataMgtCode <- Quantico::Shiny.CodePrint.Collect(y = DataMgtCode, x = paste0(
          "\n",
          "# Load CSV\n",
          "library(data.table)\n",
          "if(!exists('DataList')) DataList <- list()\n",
          "filename <- basename(", Quantico:::CEP(FilepathName), ")\n",
          "DataList[[filename]] <- Quantico:::LoadCSV(file.choose())\n"))
        DataMgtCode <<- DataMgtCode
      }
      if(length(DataList[[basename(FilepathName)]]) > 0L) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
        for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("EDAData", i), Label = 'Data Selection', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
        for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
        Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Data', Label = 'Select Data', Choices = names(DataList), SelectedDefault = NULL)
      } else {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Empty Data", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      }
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Empty Data", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Import Local Rdata or rds file
  shiny::observeEvent(input$ImportLocalRdata,  {

    # Code Collection
    if(!exists('DataMgtCode')) DataMgtCode <- list()
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()
    if(!exists('ForecastingCode')) ForecastingCode <- list()
    if(!exists('PlotterCode')) PlotterCode <- list()

    # Notify user that data is being loaded
    shiny::showNotification('Data loading has begun')

    # Filepath <- tcltk::tk_choose.files() # desktop version
    Filepath <- tryCatch({input$ImportLocalRdata$datapath}, error = function(x) NULL)
    FilepathName <- tryCatch({input$ImportLocalRdata$name}, error = function(x) NULL)

    if(Debug) {
      print(Filepath)
      print(FilepathName)
    }

    # Create an availability awaiting event
    shiny::req(Filepath)

    # Load Data: if Algo == lightgbm then model is separated from the modeloutputlist, because that's how lightgbm works
    # otherwise, they're all in the same list
    if(Debug) print('ModelObjectLoad')
    if(length(Filepath) > 0L) {
      filename <- tryCatch({basename(FilepathName)}, error = function(x) NULL) # Works for both expanded or truncated Paths args
      if(!grepl(pattern = ".csv", x = filename)) {
        if(length(filename) > 0L &&
           !grepl(pattern = "__lgbmModelObject__", x = filename) &&
           !grepl(pattern = ".zip", x = filename) &&
           grepl(pattern = "\\.", x = filename)) {
          filename <- gsub(pattern = ".rds", replacement = "", x = filename)
          ModelOutputList[[filename]] <- tryCatch({readRDS(Filepath)}, error = function(x) NULL)
          ModelOutputList <<- ModelOutputList
          import_check <- TRUE
          import_check <<- import_check
        } else {
          import_check <- FALSE
          import_check <<- import_check
        }
        if(import_check) {
          if(ModelOutputList[[filename]]$ArgsList$Algo == "LightGBM") {
            LightGBM_ModelID <- filename
            LightGBM_ModelID <<- LightGBM_ModelID
          } else if(ModelOutputList[[filename]]$ArgsList$Algo %in% c("H2ODRF","H2OGBM","H2OGLM")) {
            H2O_ModelID <- filename
            H2O_ModelID <<- H2O_ModelID
          }

          # Code Collect
          DataMgtCode <- tryCatch({Quantico::Shiny.CodePrint.Collect(y = DataMgtCode, x = paste0(
            "\n",
            "# Load Model\n",
            "if(!exists(ModelOutputList)) ModelOutputList <- list()\n",
            "ModelOutputList[[", Quantico:::CEP(filename), "]] <- readRDS(file.choose())\n"))}, error = function(x) DataMgtCode)

          # Add data to DataOutputSelection Page
          if(Debug) print("ML Reporting Tab Update")
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("MLReportsModelSelection", i), Label = 'Model Selection', Choices = names(ModelOutputList), Multiple = TRUE, MaxVars = 100L)
          for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
          Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Model', Label = 'Select Model', Choices = names(ModelOutputList), SelectedDefault = NULL, Multiple = FALSE)
          Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Data', Label = 'Select Data', Choices = names(DataList), SelectedDefault = NULL, Multiple = FALSE)

          if(Debug) print("Updating done")

          # Initialize
          PlotterCode <<- PlotterCode;DataMgtCode <<- DataMgtCode;DataWranglingCode <<- DataWranglingCode;FeatureEngineeringCode <<- FeatureEngineeringCode;MachineLearningCode <<- MachineLearningCode; ForecastingCode <<- ForecastingCode

          # Sweet Alert
          shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
        } else {
          shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Incorrect File Selected", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
        }
      }

    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Empty Data", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Import LightGBM Model
  shiny::observeEvent(input$ImportLightGBMModel,  {

    # Code Collection
    if(!exists('DataMgtCode')) DataMgtCode <- list()
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()
    if(!exists('ForecastingCode')) ForecastingCode <- list()
    if(!exists('PlotterCode')) PlotterCode <- list()

    # Notify user that data is being loaded
    shiny::showNotification('Data loading has begun')

    # Filepath <- tcltk::tk_choose.files() # desktop version
    Filepath <- tryCatch({input$ImportLightGBMModel$datapath}, error = function(x) NULL)
    FilepathName <- tryCatch({input$ImportLightGBMModel$name}, error = function(x) NULL)

    if(Debug) {
      print(Filepath)
      print(FilepathName)
    }

    # Create an availability awaiting event
    shiny::req(Filepath)

    # Load Data: if Algo == lightgbm then model is separated from the modeloutputlist, because that's how lightgbm works
    # otherwise, they're all in the same list
    if(Debug) {
      print(Filepath)
      print('ModelObjectLoad')
      print(LightGBM_ModelID)
      print(grepl(pattern = "__lgbmModelObject__", x = FilepathName))
    }
    if(length(Filepath) > 0L &&
       grepl(pattern = "__lgbmModelObject__", x = FilepathName)) {
      if(Debug) {
        print(LightGBM_ModelID)
        print(FilepathName)
      }
      ModelOutputList[[LightGBM_ModelID]]$Model <- tryCatch({lightgbm::readRDS.lgb.Booster(Filepath)}, error = function(x) NULL)
      ModelOutputList <<- ModelOutputList
      print(ModelOutputList[[LightGBM_ModelID]]$Model)
      if(Debug) {
        print(Filepath)
      }

      # Code Collect
      DataMgtCode <- tryCatch({Quantico::Shiny.CodePrint.Collect(y = DataMgtCode, x = paste0(
        "\n",
        "# Load Model\n",
        "ModelOutputList[[", Quantico:::CEP(LightGBM_ModelID), "]]$Model <- lightgbm::readRDS.lgb.Booster(file.choose())\n"))}, error = function(x) DataMgtCode)

      # Add data to DataOutputSelection Page
      if(Debug) print("ML Reporting Tab Update")
      for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("MLReportsModelSelection", i), Label = 'Model Selection', Choices = names(ModelOutputList), Multiple = TRUE, MaxVars = 100L)
      for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
      Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Model', Label = 'Select Model', Choices = names(ModelOutputList), SelectedDefault = NULL, Multiple = FALSE)
      Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Data', Label = 'Select Data', Choices = names(DataList), SelectedDefault = NULL, Multiple = FALSE)

      # Initialize
      PlotterCode <<- PlotterCode;DataMgtCode <<- DataMgtCode;DataWranglingCode <<- DataWranglingCode;FeatureEngineeringCode <<- FeatureEngineeringCode;MachineLearningCode <<- MachineLearningCode; ForecastingCode <<- ForecastingCode

      # Sweet Alert
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Incorrect File Selected", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Import H2O Model
  shiny::observeEvent(input$ImportH2OModel,    {

    # Code Collection
    if(!exists('DataMgtCode')) DataMgtCode <- list()
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()
    if(!exists('ForecastingCode')) ForecastingCode <- list()
    if(!exists('PlotterCode')) PlotterCode <- list()

    # Notify user that data is being loaded
    shiny::showNotification('Data loading has begun')

    # Filepath <- tcltk::tk_choose.files() # desktop version
    Filepath <- tryCatch({input$ImportH2OModel$datapath}, error = function(x) NULL)
    FilepathName <- tryCatch({input$ImportH2OModel$name}, error = function(x) NULL)

    if(Debug) {
      print(Filepath)
      print(FilepathName)
    }

    # Create an availability awaiting event
    shiny::req(Filepath)

    # Load Data: if Algo == lightgbm then model is separated from the modeloutputlist, because that's how lightgbm works
    # otherwise, they're all in the same list
    if(Debug) print('ModelObjectLoad')
    if(length(Filepath) > 0L) {
      #filename <- basename(FilepathName) # Works for both expanded or truncated Paths args
      if(Debug) print(LightGBM_ModelID)
      ModelOutputList[[LightGBM_ModelID]]$Model <- tryCatch({h2o::h2o.loadModel(path = Filepath)}, error = function(x) NULL)
      ModelOutputList <<- ModelOutputList
      if(Debug) {
        print(Filepath)
        print(tryCatch({readRDS(Filepath)}, error = function(x) NULL))
      }

      # Add data to DataOutputSelection Page
      if(Debug) print("ML Reporting Tab Update")
      # for(a in 1:10) print(names(ModelOutputList))
      for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("MLReportsModelSelection", i), Label = 'Model Selection', Choices = names(ModelOutputList), Multiple = TRUE, MaxVars = 100L)
      for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection", i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
      Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Model', Label = 'Select Model', Choices = names(ModelOutputList), SelectedDefault = NULL, Multiple = FALSE)
      Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Data', Label = 'Select Data', Choices = names(DataList), SelectedDefault = NULL, Multiple = FALSE)

      # Initialize
      PlotterCode <<- PlotterCode;DataMgtCode <<- DataMgtCode;DataWranglingCode <<- DataWranglingCode;FeatureEngineeringCode <<- FeatureEngineeringCode;MachineLearningCode <<- MachineLearningCode; ForecastingCode <<- ForecastingCode

      # Sweet Alert
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Empty Data", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # PostGRE Import data
  shiny::observeEvent(input$PostGRE_PullTable, {

    # Code Collection
    if(!exists('DataMgtCode')) DataMgtCode <- list()
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()
    if(!exists('ForecastingCode')) ForecastingCode <- list()
    if(!exists('PlotterCode')) PlotterCode <- list()

    # Notify user that data is being loaded
    shiny::showNotification('Data loading has begun')

    # Local PostGRE Data
    if(Debug) print("PostGRE Pull Table 1")
    sql <- Quantico:::ReturnParam(xx = tryCatch({input[['sql_code']]}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    PostGRE_PullTable_DB <- Quantico:::ReturnParam(xx = tryCatch({input[['PostGRE_PullTable_DB']]}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    PostGRE_PullTable_Table <- Quantico:::ReturnParam(xx = tryCatch({input[['PostGRE_PullTable_Table']]}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    PostGRE_SamplePercent <- Quantico:::ReturnParam(xx = tryCatch({input[['PostGRE_SamplePercent']]}, error = function(x) NULL), Type = 'numeric', Default = 1.0, Debug = Debug)
    if(length(sql) > 0L && nchar(sql) < 5) sql <- NULL

    # Query Builder
    if(Debug) print("PostGRE Pull Table 2")
    PostGRE_PullTable_Columns <- Quantico:::ReturnParam(xx = tryCatch({input[['PostGRE_PullTable_Columns']]}, error = function(x) NULL), Type = 'character', Default = '*', Debug = Debug)
    PostGRE_PullTable_GroupByColumns <- Quantico:::ReturnParam(xx = tryCatch({input[['PostGRE_PullTable_GroupByColumns']]}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    PostGRE_PullTable_AggStat <- Quantico:::ReturnParam(xx = tryCatch({input[['PostGRE_PullTable_AggStat']]}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)

    if(Debug) {print(PostGRE_PullTable_Table);print(PostGRE_PullTable_DB)}
    if(length(PostGRE_PullTable_DB) > 0L && length(PostGRE_PullTable_Table) > 0L) {
      print("PostGRE Pull Table 3")
      if(!exists('DataMgtCode')) DataMgtCode <- list()

      # Pull Data
      if(Debug) print(sql)
      DataList[[PostGRE_PullTable_Table]][['data']] <- Quantico:::DM.pgQuery(
        Query = sql,
        DataBase = PostGRE_PullTable_DB,
        SELECT = PostGRE_PullTable_Columns,
        AggStat = PostGRE_PullTable_AggStat,
        FROM = PostGRE_PullTable_Table,
        GroupBy = PostGRE_PullTable_GroupByColumns,
        SamplePercent = PostGRE_SamplePercent,
        Host = PostGRE_Host, User = PostGRE_User,
        Port = PostGRE_Port, Password = PostGRE_Password)

      # Update DataList
      DataList <- Quantico:::DM.DataListUpdate(DataList, PostGRE_PullTable_Table)
      DataList <<- DataList; CurrentData <<- PostGRE_PullTable_Table

      # Add data to DataOutputSelection Page
      for(i in seq_len(NumTabs)) Quantico::PickerInput(session, input, Update = TRUE, InputID = paste0("DataOutputSelection",i), Label = 'Display Data', Choices = names(DataList), Multiple = TRUE, MaxVars = 100L)
      Quantico::SelectizeInput(session, input, Update = TRUE, InputID = 'ScoreML_Data', Label = 'Select Data', Choices = names(DataList), SelectedDefault = NULL)

      # Code Fill
      DataMgtCode <- Quantico::Shiny.CodePrint.Collect(y = DataMgtCode, x = paste0(
        "\n",
        "# Query postgres\n",
        "library(data.table)\n",
        "DataList[[", Quantico:::CEP(PostGRE_PullTable_Table), "]][['data']] <- Quantico::DM.pgQuery(\n  ",
        "Query = ", sql, ",\n  ",
        "Host = ", Quantico:::CEP(PostGRE_Host), ",\n  ",
        "DataBase = ", Quantico:::CEP(PostGRE_PullTable_DB), ",\n  ",
        "SELECT = ", Quantico:::CEP(PostGRE_PullTable_Columns), ",\n  ",
        "AggStat = ", Quantico:::CEP(PostGRE_PullTable_AggStat), ",\n  ",
        "FROM = ", Quantico:::CEP(PostGRE_PullTable_Table), ",\n  ",
        "GroupBy = ", Quantico:::CEP(PostGRE_PullTable_GroupByColumns), ",\n  ",
        "SamplePercent = ", Quantico:::CEPP(PostGRE_SamplePercent), ",\n  ",
        "User = ", Quantico:::CEP(PostGRE_User), ",\n  ",
        "Port = ", Quantico:::CEPP(PostGRE_Port), ",\n  ",
        "Password = ", Quantico:::CEP(PostGRE_Password), ")\n"))

      # Initialize
      PlotterCode <<- PlotterCode;DataMgtCode <<- DataMgtCode;DataWranglingCode <<- DataWranglingCode;FeatureEngineeringCode <<- FeatureEngineeringCode;MachineLearningCode <<- MachineLearningCode; ForecastingCode <<- ForecastingCode

      # Sweet Alert
      if(length(PostGRE_PullTable_DB) > 0L && length(PostGRE_PullTable_Table) > 0L && length(DataList[[PostGRE_PullTable_Table]]) > 0L) {
        shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
      }
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Empty Data", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # PostGRE Create Database
  shiny::observeEvent(input$PostGRE_CreateDB,  {

    # Code Collection
    if(!exists('DataMgtCode')) DataMgtCode <- list()
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()
    if(!exists('ForecastingCode')) ForecastingCode <- list()
    if(!exists('PlotterCode')) PlotterCode <- list()

    # Pull Table
    PostGRE_NewDB_DB <- tryCatch({input[['PostGRE_NewDB_DB']]}, error = function(x) NULL)
    if(!exists('DataMgtCode')) DataMgtCode <- list()
    if(length(PostGRE_NewDB_DB) > 0L && PostGRE_NewDB_DB != "") {
      tryCatch({Quantico::DM.pgCreateDB(
        Host = PostGRE_Host,
        Database = PostGRE_NewDB_DB,
        User = PostGRE_User,
        Port = PostGRE_Port,
        Password = PostGRE_Password)}, error = function(x) NULL)

      # Code Fill
      DataMgtCode <- Quantico::Shiny.CodePrint.Collect(y = DataMgtCode, x = paste0(
        "\n",
        "# Create new PostGRE Database\n",
        "Quantico::DM.pgCreateDB(\n  ",
        "Host = ", Quantico:::CEP(PostGRE_Host), ",\n  ",
        "DataBase = ", Quantico:::CEP(PostGRE_NewDB_DB), ",\n  ",
        "User = ", Quantico:::CEP(PostGRE_User), ",\n  ",
        "Port = ", Quantico:::CEP(PostGRE_Port), ",\n  ",
        "Password = ", Quantico:::CEP(PostGRE_Password), ")\n"))

      # Initialize
      PlotterCode <<- PlotterCode; DataMgtCode <<- DataMgtCode; DataWranglingCode <<- DataWranglingCode; FeatureEngineeringCode <<- FeatureEngineeringCode;MachineLearningCode <<- MachineLearningCode; ForecastingCode <<- ForecastingCode

      # Used below to update db's available
      if(length(PostGRE_DBNames) == 0L) {
        PostGRE_DBNames <<- Quantico::DM.pgListDatabases(Host = PostGRE_Host, Port = PostGRE_Port, User = PostGRE_User, Password = PostGRE_Password)
      } else {
        PostGRE_DBNames <<- NULL
      }

      # Update available databases
      output$PostGRE_PullTable_DB <- shiny::renderUI({
        Quantico:::SelectizeInput(InputID = 'PostGRE_PullTable_DB', Label = NULL, Choices = PostGRE_DBNames, SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1L, CloseAfterSelect = FALSE, Debug = Debug)
      })
      output$PostGRE_NewDB_DB <- shiny::renderUI({
        Quantico:::SelectizeInput(InputID = 'PostGRE_NewDB_DB', Label = NULL, Choices = PostGRE_DBNames, SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1L, CloseAfterSelect = FALSE, Debug = Debug)
      })
      output$PostGRE_DropTable_DB <- shiny::renderUI({
        Quantico:::SelectizeInput(InputID = 'PostGRE_DropTable_DB', Label = NULL, Choices = PostGRE_DBNames, SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1L, CloseAfterSelect = FALSE, Debug = Debug)
      })
      output$PostGRE_DropDB_DB <- shiny::renderUI({
        Quantico:::SelectizeInput(InputID = 'PostGRE_DropDB_DB', Label = NULL, Choices = PostGRE_DBNames, SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1L, CloseAfterSelect = FALSE, Debug = Debug)
      })

      # Sweet Alert
      print("Data was loaded")
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Missing DB Name", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # PostGRE Drop Table
  shiny::observeEvent(input$PostGRE_DropTable, {

    # Code Collection
    if(!exists('DataMgtCode')) DataMgtCode <- list()
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()
    if(!exists('ForecastingCode')) ForecastingCode <- list()
    if(!exists('PlotterCode')) PlotterCode <- list()

    # Local PostGRE Data
    PostGRE_DropTable_DB <- Quantico:::ReturnParam(xx = tryCatch({input[['PostGRE_DropTable_DB']]}, error = function(x) NULL), Type = 'character', Default = NULL)
    PostGRE_DropTable_Table <- Quantico:::ReturnParam(xx = tryCatch({input[['PostGRE_DropTable_Table']]}, error = function(x) NULL), Type = 'character', Default = NULL)

    # Drop Table
    if(length(PostGRE_DropTable_Table) > 0L && length(PostGRE_DropTable_DB) > 0L) {
      for(i in PostGRE_DropTable_Table) {
        x <- tryCatch({Quantico::DM.pgRemovePlotTable(
          Table = i,
          Host = PostGRE_Host,
          DataBase = PostGRE_DropTable_DB,
          User = PostGRE_User,
          Port = PostGRE_Port,
          Password = PostGRE_Password)}, error = function(x) 'fail')
      }

      # Code Fill
      DataMgtCode <- Quantico::Shiny.CodePrint.Collect(y = DataMgtCode, x = paste0(
        "\n",
        "# Remove Table\n",
        "for(i in c(", Quantico:::ExpandText(PostGRE_DropTable_Table), ")) {\n  ",
        "Quantico::DM.pgRemovePlotTable(\n    ",
        "Table = i,\n    ",
        "Host = ", Quantico:::CEP(PostGRE_Host), ",\n    ",
        "DataBase = i,\n    ",
        "User = ", Quantico:::CEP(PostGRE_User), ",\n    ",
        "Port = ", Quantico:::CEP(PostGRE_Port), ",\n    ",
        "Password = ", Quantico:::CEP(PostGRE_Password), ")\n",
        "}\n"))

      # Sweet Alert
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Table Not Found", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # PostGRE Drop Database
  shiny::observeEvent(input$PostGRE_DropDB,    {

    # Code Collection
    if(!exists('DataMgtCode')) DataMgtCode <- list()
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()
    if(!exists('ForecastingCode')) ForecastingCode <- list()
    if(!exists('PlotterCode')) PlotterCode <- list()

    # Local PostGRE Data
    DropDatabaseName <- Quantico:::ReturnParam(xx = tryCatch({input[['PostGRE_DropTable_DB']]}, error = function(x) NULL), Type = 'character', Default = NULL)
    if(length(DropDatabaseName) > 0L) {
      for(i in DropDatabaseName) {
        tryCatch({
          Quantico::DM.pgDropDB(
            Host = PostGRE_Host,
            DataBase = DropDatabaseName,
            User = PostGRE_User,
            Port = PostGRE_Port,
            Password = PostGRE_Password)}, error = function(x) print('DB Not Found'))
      }

      # Code Fill
      DataMgtCode <- Quantico::Shiny.CodePrint.Collect(y = DataMgtCode, x = paste0(
        "\n",
        "# Create new PostGRE Database\n",
        "for(i in c(", Quantico:::ExpandText(DropDatabaseName), ")) {\n  ",
        "Quantico::DM.pgDropDB((\n    ",
        "Host = ", Quantico:::CEP(PostGRE_Host), ",\n    ",
        "DataBase = i,\n    ",
        "User = ", Quantico:::CEP(PostGRE_User), ",\n    ",
        "Port = ", Quantico:::CEP(PostGRE_Port), ",\n    ",
        "Password = ", Quantico:::CEP(PostGRE_Password), ")\n",
        "}\n"
      ))

      # Initialize
      PlotterCode <<- PlotterCode
      DataMgtCode <<- DataMgtCode
      DataWranglingCode <<- DataWranglingCode
      FeatureEngineeringCode <<- FeatureEngineeringCode
      MachineLearningCode <<- MachineLearningCode
      ForecastingCode <<- ForecastingCode

      # Update available databases
      if(length(PostGRE_DBNames) == 0L) {
        PostGRE_DBNames <- tryCatch({Quantico::DM.pgListDatabases(Host = PostGRE_Host, Port = PostGRE_Port, User = PostGRE_User, Password = PostGRE_Password)}, error = function(x) NULL)
      }
      Quantico:::SelectizeInput(session=session, Update = TRUE, input = input, InputID = 'PostGRE_PullTable_DB', Label = NULL, Choices = PostGRE_DBNames, SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1L, CloseAfterSelect = FALSE, Debug = Debug)
      Quantico:::TextInput(session=session, Update = TRUE, input = input, InputID = 'PostGRE_NewDB_DB', Label = NULL, Value = NULL)
      Quantico:::SelectizeInput(session=session, Update = TRUE, input = input, InputID = 'PostGRE_DropTable_DB', Label = NULL, Choices = input$LocalPostGRE_Database, SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1, CloseAfterSelect = FALSE, Debug = Debug)
      Quantico:::SelectizeInput(session=session, Update = TRUE, input = input, InputID = 'PostGRE_DropDB_DB', Label = NULL, Choices = input$LocalPostGRE_Database, SelectedDefault = NULL, Multiple = TRUE, MaxVars = 1, CloseAfterSelect = FALSE, Debug = Debug)

      # Sweet Alert
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "DB Not Found", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # Export Local CSV
  shiny::observeEvent(input$SaveLocalData,     {
    SaveData <- Quantico:::ReturnParam(xx = tryCatch({shiny::req(input$SaveData_SelectData)}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    SaveFileName <- Quantico:::ReturnParam(xx = tryCatch({shiny::req(input$SaveFileName)}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(Debug) print(SaveFileName)
    if(length(SaveFileName) > 0L && length(SaveData) > 0L) {
      tryCatch({data.table::fwrite(DataList[[SaveData]][['data']], file = file.path(WorkingDirectory, paste0(SaveFileName, '.csv')))}, error = function(x) NULL)
      if(!exists('DataMgtCode')) DataMgtCode <- list()
      DataMgtCode <- Quantico::Shiny.CodePrint.Collect(y = DataMgtCode, x = paste0(
        "\n",
        "# Save Data locally as .csv\n",
        "data.table::fwrite(DataList[[", Quantico:::CEP(SaveData), "]], file = file.path(", Quantico:::CEP(WorkingDirectory), ", paste0(", Quantico:::CEP(SaveFileName), ", '.csv')))\n"))
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'Success', btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "20%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'Missing File Name', btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "20%")
    }
  }, ignoreInit = TRUE)

  # Export Local Model
  shiny::observeEvent(input$SaveLocalDataRdata,{
    SaveData <- Quantico:::ReturnParam(xx = tryCatch({shiny::req(input$SaveData_SelectDataRdata)}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    SaveFileName <- SaveData # Quantico:::ReturnParam(xx = tryCatch({shiny::req(input$SaveFileNameRdata)}, error = function(x) NULL), Type = 'character', Default = NULL, Debug = Debug)
    if(Debug) {
      print(SaveData)
      print(SaveFileName)
    }

    if(!dir.exists(paths = file.path(WorkingDirectory, SaveFileName))) {
      LocalExportPath <- file.path(WorkingDirectory, SaveFileName)
      dir.create(path = file.path(LocalExportPath))
    } else {
      LocalExportPath <- file.path(WorkingDirectory, SaveFileName)
    }

    if(length(SaveFileName) > 0L && length(SaveData) > 0L) {
      if(Debug) {
        print(paste0("length(ModelOutputList) == ", length(ModelOutputList)))
        print(paste0("LocalExportPath == ", LocalExportPath))
        print(paste0("SaveFileName == ", SaveFileName))
        print(paste0("names(ModelOutputList) == ", names(ModelOutputList)))
        print(paste0("Filepath output == ", file.path(LocalExportPath, paste0(SaveFileName, '.rds'))))
      }
      temp_model_rdata <- ModelOutputList[[SaveData]]
      print(names(temp_model_rdata$ArgsList))
      algo <- temp_model_rdata$ArgsList$Algo
      if(Debug) print(algo)
      if(algo == "LightGBM") {
        tryCatch({
          lightgbm::saveRDS.lgb.Booster(temp_model_rdata$Model, file = file.path(LocalExportPath, paste0(SaveFileName, '__lgbmModelObject__.rds')))
          if(Debug) print("lgbm 1")
          temp_model_rdata$Model <- NULL
          if(Debug) print("lgbm 2")
          saveRDS(temp_model_rdata, file = file.path(LocalExportPath, paste0(SaveFileName, '.rds')))
        }, error = function(x) {
          print("Saving Failed")
          print(paste0("length(ModelOutputList) == ", length(ModelOutputList)))
          print(paste0("LocalExportPath == ", LocalExportPath))
          NULL
        })
      } else if(algo %in% c("H2ODRF","H2OGBM","H2OGLM")) {
        tryCatch({
          if(Debug) print("h2o 1")
          temp_model_rdata$Model <- NULL
          if(Debug) print("h2o 2")
          saveRDS(temp_model_rdata, file = file.path(LocalExportPath, paste0(SaveFileName, '.rds')))
        }, error = function(x) {
          print("Saving Failed")
          print(paste0("length(ModelOutputList) == ", length(ModelOutputList)))
          print(paste0("LocalExportPath == ", LocalExportPath))
          NULL
        })
      } else {
        tryCatch({
          saveRDS(temp_model_rdata, file = file.path(LocalExportPath, paste0(SaveFileName, '.rds')))
        }, error = function(x) {
          print("Saving Failed")
          print(paste0("length(ModelOutputList) == ", length(ModelOutputList)))
          print(paste0("LocalExportPath == ", LocalExportPath))
          NULL
        })
      }

      if(!exists('DataMgtCode')) DataMgtCode <- list()
      DataMgtCode <- Quantico::Shiny.CodePrint.Collect(y = DataMgtCode, x = paste0(
        "\n",
        "# Save Rdata locally as .Rdata\n",
        "save(ModelOutputList[[", Quantico:::CEP(SaveData), "]], file = file.path(", Quantico:::CEP(LocalExportPath), ", paste0(", Quantico:::CEP(SaveData), ", '.rds')))\n"
      ))
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'Success', btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "20%")
    } else {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'Missing File Name', btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "20%")
    }
  }, ignoreInit = TRUE)

  # PostGRE Export Data
  shiny::observeEvent(input$PostGRE_Push,      {

    # Data Name
    TableName <- Quantico:::ReturnParam(xx=tryCatch({input[['SaveData_TableName']]}, error=function(x) NULL), Type='character', Default=NULL)
    DataBaseName <- Quantico:::ReturnParam(xx=tryCatch({input[['SaveData_DataBaseName']]}, error=function(x) NULL), Type='character', Default=NULL)
    postgre_data <- tryCatch({shiny::req(DataList[[input[['SaveData_SelectDataPostGRE']]]][['data']])}, error = function(x) NULL)

    # Push to PostGRE
    if(length(TableName) > 0L && length(DataBaseName) > 0L && data.table::is.data.table(postgre_data)) {
      tryCatch({Quantico::DM.pgRemoveCreateAppend(
        data = postgre_data,
        DataBase = DataBaseName,
        Table = TableName,
        Host = PostGRE_Host,
        User = PostGRE_User,
        Port = PostGRE_Port,
        Password = PostGRE_Password)}, error = function(x) NULL)
      DataMgtCode <- Quantico::Shiny.CodePrint.Collect(y = DataMgtCode, x = paste0(
        "\n",
        "# Save Data to local PostGRE\n",
        "\n",
        "Quantico::DM.pgRemoveCreateAppend(\n  ",
        "data = ", Quantico:::CEP(postgre_data), ",\n  ",
        "DataBase = ", Quantico:::CEP(DataBaseName), ",\n  ",
        "Tabl = ", Quantico:::CEP(TableName), ",\n  ",
        "Host = ", Quantico:::CEP(PostGRE_Host), ",\n  ",
        "User = ", Quantico:::CEP(PostGRE_User), ",\n  ",
        "Port = ", Quantico:::CEP(PostGRE_Port), ",\n  ",
        "Password = ", Quantico:::CEP(PostGRE_Password), ")\n"))
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'Success', btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    } else {
      print('length(TableName) == 0L or length(DataBaseName) == 0L or postgre_data is not a data.table')
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = 'data not sent', btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }
  }, ignoreInit = TRUE)

  # # Azure Import CSV
  # shiny::observeEvent(input$ImportAzureCSV,    {
  #
  #   # Code Collection
  #   if(!exists('DataMgtCode')) DataMgtCode <- list()
  #   if(!exists('DataWranglingCode')) DataWranglingCode <- list()
  #   if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
  #   if(!exists('MachineLearningCode')) MachineLearningCode <- list()
  #   if(!exists('ForecastingCode')) ForecastingCode <- list()
  #   if(!exists('PlotterCode')) PlotterCode <- list()
  #
  #   # Notify user that data is being loaded
  #   shiny::showNotification('Data loading has begun')
  #
  #   # File Type .csv
  #   FileName <- tryCatch({input[['AzureBlobStorageTabular']]}, error = function(x) NULL)
  #   if(Debug) print(FileName)
  #   if(length(FileName) != 0 && FileName != "Load" && FileName != "") {
  #     AzureStor::download_blob(container = cont, src = input[['AzureBlobStorageTabular']], dest = file.path('/inputdata', input[['AzureBlobStorageTabular']]), overwrite=TRUE)
  #
  #     # Azure .csv
  #     x <- tryCatch({input[['AzureBlobStorageTabular']]}, error = function(x) NULL)
  #     if(Debug) print('AzureBlobStorageTabular')
  #     if(Debug) print(x)
  #     if(length(x) > 0L) {
  #       filename <- tryCatch({basename(input$AzureBlobStorageTabular)}, error = function(x) NULL)
  #       for(i in seq_len(24L)) {
  #         DataList[[filename]] <<- tryCatch({Quantico:::LoadCSV(Infile = file.path('/inputdata', input[['AzureBlobStorageTabular']]), Debug = Debug)}, error = function(x) NULL)
  #         if(length(DataList[[filename]]) > 0L) {
  #           xx <- tryCatch({DataList[[PostGRE_PullTable_Table]][['data']][,.N]}, error = function(x) NULL)
  #           if(length(xx) > 0L) {
  #             DataList <- tryCatch({Quantico:::DM.DataListUpdate(DataList, PostGRE_PullTable_Table)}, error = function(x) DataList)
  #           }
  #           break
  #         } else {
  #           Sys.sleep(5)
  #         }
  #       }
  #
  #       DataList <<- DataList; CurrentData <<- filename
  #       DataMgtCode <- Quantico::Shiny.CodePrint.Collect(y = DataMgtCode, x = paste0(
  #         "\n",
  #         "# Load ModelOutputList\n",
  #         "x <- ", Quantico:::CEP(x), "\n",
  #         "filename <<- basename(", Quantico:::CEP(filename), "\n",
  #         "ModelOutputList <- readRDS(file.path('/inputdata', x)),\n",
  #         "DataList[[filename]] <- Quantico:::LoadCSV(Infile = file.path('/inputdata', input[['AzureBlobStorageTabular']]))\n"))
  #
  #       # Sweet Alert
  #       shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  #     }
  #
  #     # Sweet Alert
  #     print("Data was loaded")
  #     shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Missing Name", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  #   } else {
  #     print("Data was loaded")
  #     shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Missing Name", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  #   }
  # }, ignoreInit = TRUE)
  #
  # # Azure Import Rdata or rds file
  # shiny::observeEvent(input$ImportAzureRdata,  {
  #
  #   # Notify user that data is being loaded
  #   shiny::showNotification('Data loading has begun')
  #
  #   # File Type .Rdata
  #   inFile2 <- tryCatch({input[['AzureBlobStorageRdata']]}, error = function(x) NULL)
  #   if(!is.null(inFile2)) print(inFile2)
  #   if(length(inFile2) != 0 && inFile2 != "") {
  #     if(Debug) {print('data check 3')}
  #
  #     # Download data
  #     tryCatch({AzureStor::download_blob(container = cont, src = input[['AzureBlobStorageRdata']], dest = file.path('/inputdata', input[['AzureBlobStorageRdata']]), overwrite=TRUE)}, error = function(x) NULL)
  #
  #     # Load ModelOutputList
  #     x <- tryCatch({input[['AzureBlobStorageRdata']]}, error = function(x) NULL)
  #     if(Debug) print('AzureBlobStorageRdata')
  #     if(length(x) > 0L && x != "" && !is.na(x)) {
  #       for(i in seq_len(24L)) {
  #         filename <- basename(x)
  #         ModelOutputList <<- tryCatch({readRDS(file.path('/inputdata', x))}, error = function(x) NULL)
  #
  #         # Grab or loop
  #         if(length(ModelOutputList) > 0L) {
  #           if(!is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
  #             DataList[[filename]][['data']] <<- data.table::rbindlist(list(ModelOutputList$TrainData, ModelOutputList$TestData), use.names = TRUE, fill = TRUE)
  #             xx <- tryCatch({DataList[[filename]][['data']][,.N]}, error = function(x) NULL)
  #             if(length(xx) > 0L) {
  #               DataList <- tryCatch({Quantico:::DM.DataListUpdate(DataList, filename)}, error = function(x) DataList)
  #             }
  #             DataList <<- DataList
  #             CurrentData <<- filename
  #             DataMgtCode <- Quantico::Shiny.CodePrint.Collect(y = DataMgtCode, x = paste0(
  #               "\n",
  #               "# Load ModelOutputList\n",
  #               "AzureStor::download_blob(container = ", Quantico:::CEP(cont),", src = ", Quantico:::CEP(input[['AzureBlobStorageRdata']]), ", dest = ", Quantico:::CEP(file.path('/inputdata', input[['AzureBlobStorageRdata']])), ", overwrite=TRUE)\n",
  #               "x <- ", Quantico:::CEP(x), "\n",
  #               "filename <- basename(x)\n",
  #               "ModelOutputList <- readRDS(file.path('/inputdata', x)),\n",
  #               "DataList[[filename]] <- data.table::rbindlist(list(ModelOutputList$TrainData, ModelOutputList$TestData), use.names = TRUE, fill = TRUE)\n"))
  #           } else if(is.null(ModelOutputList$TrainData) && !is.null(ModelOutputList$TestData)) {
  #             DataList[[filename]][['data']] <<- ModelOutputList$TestData
  #             xx <- tryCatch({DataList[[filename]][['data']][,.N]}, error = function(x) NULL)
  #             if(length(xx) > 0L) {
  #               DataList <- tryCatch({Quantico:::DM.DataListUpdate(DataList, filename)}, error = function(x) DataList)
  #             }
  #             DataList <<- DataList
  #             CurrentData <<- filename
  #             DataMgtCode <- Quantico::Shiny.CodePrint.Collect(y = DataMgtCode, x = paste0(
  #               "\n",
  #               "# Load ModelOutputList\n",
  #               "AzureStor::download_blob(container = ", Quantico:::CEP(cont),", src = ", Quantico:::CEP(input[['AzureBlobStorageRdata']]), ", dest = ", Quantico:::CEP(file.path('/inputdata', input[['AzureBlobStorageRdata']])), ", overwrite=TRUE)\n",
  #               "x <- ", Quantico:::CEP(x), "\n",
  #               "filename <- basename(x)\n",
  #               "ModelOutputList <- readRDS(file.path('/inputdata', x)),\n",
  #               "DataList[[filename]] <- ModelOutputList$TestData\n"))
  #           } else if(!is.null(ModelOutputList$TrainData) && is.null(ModelOutputList$TestData)) {
  #             DataList[[filename]][['data']] <<- ModelOutputList$TrainData
  #             xx <- tryCatch({DataList[[filename]][['data']][,.N]}, error = function(x) NULL)
  #             if(length(xx) > 0L) {
  #               DataList <- tryCatch({Quantico:::DM.DataListUpdate(DataList, filename)}, error = function(x) DataList)
  #             }
  #             DataList <<- DataList
  #             CurrentData <<- filename
  #             DataMgtCode <- Quantico::Shiny.CodePrint.Collect(y = DataMgtCode, x = paste0(
  #               "\n",
  #               "# Load ModelOutputList\n",
  #               "AzureStor::download_blob(container = ", Quantico:::CEP(cont),", src = ", Quantico:::CEP(input[['AzureBlobStorageRdata']]), ", dest = ", Quantico:::CEP(file.path('/inputdata', input[['AzureBlobStorageRdata']])), ", overwrite=TRUE)\n",
  #               "x <- ", Quantico:::CEP(x), "\n",
  #               "filename <- basename(x)\n",
  #               "ModelOutputList <- readRDS(file.path('/inputdata', x)),\n",
  #               "DataList[[filename]] <- ModelOutputList$TrainData\n"))
  #           }
  #           break
  #         } else {
  #           Sys.sleep(5L)
  #         }
  #       }
  #
  #       # Sweet Alert
  #       print("Data was loaded")
  #       shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Success", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  #     } else {
  #       shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Missing Info", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  #     }
  #   } else {
  #     shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Missing Info", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
  #   }
  # }, ignoreInit = TRUE)

  #                                      ----

  #               ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event :: Print Code Output    ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent(input$PrintCodeButton, {
    if(Debug) print(' :: Print Code Begins Here :: ')
    if(!exists('DataMgtCode')) DataMgtCode <- list()
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    if(!exists('MachineLearningCode')) {
      print("here 1")
      MachineLearningCode <- list()
    }

    print(MachineLearningCode)

    if(!exists('ForecastingCode')) ForecastingCode <- list()
    if(!exists('PlotterCode')) PlotterCode <- list()

    CodePrintFontSize <- Quantico::ReturnParam(xx = input$CodePrintFontSize, Type = "numeric", Default = 16)

    # Combine lists and turn into a data.table
    # Two columns in code: 'TimeStamp' and 'Code'
    MasterSet <- tryCatch({Quantico:::Shiny.CodePrint.OrganizeCode(DM = DataMgtCode, DW = DataWranglingCode, FE = FeatureEngineeringCode, ML = MachineLearningCode, FC = ForecastingCode, PL = PlotterCode)}, error = function(x) NULL)

    # Send to ui
    if(length(MasterSet) > 0L) {
      if(Debug) print('Code Print: length(MasterSet) > 0L')
      data.table::setorderv(MasterSet, cols = 'TimeStamp', order = 1L)
      shinyAce::updateAceEditor(
        session, editorId = 'PrintCode',
        value = paste0(MasterSet[['Code']], collapse = " "),
        theme = input$editor_theme2,
        fontSize = CodePrintFontSize, mode = "r")
    } else {
      if(Debug) print('Code Print: length(MasterSet) == 0L')
      shinyAce::updateAceEditor(
        session, editorId = 'PrintCode',
        value = NULL,
        theme = input$editor_theme2,
        fontSize = CodePrintFontSize, mode = "r")
    }

    MasterSet <- tryCatch({Quantico:::Shiny.CodePrint.OrganizeCode(DM = DataMgtCode, DW = DataWranglingCode, FE = FeatureEngineeringCode, ML = MachineLearningCode, FC = ForecastingCode, PL = PlotterCode)}, error = function(x) NULL)
    if(length(MasterSet) == 0L) {
      shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Code has not been collected", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
    }

  }, ignoreInit = TRUE)

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event :: EDA Tabs Output      ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent({eval(EDAOutputExpression)}, {

    if(Debug) print("EDA Panel Report 1")

    # Code Collection
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()
    if(!exists("EDAOutputList")) EDAOutputList <- list()

    # tabss refers to the entire tabsetPanel that houses the plotting, tables, and ml output panes
    # pane names are as such:
    #   Plots 1, Plots 2, ...
    #   Tables 1, Tables 2, ...
    #   ML 1, ML 2, ...
    Page <- tryCatch({as.integer(gsub("[^\\d]+", "", input$tabss, perl=TRUE))}, error = function(x) 0L)
    if(length(DataList) > 0L) {
      shiny::withProgress(message = 'EDA Reporting Has Begun..', value = 0, {

        # Inputs
        FontColorData <- Quantico:::rgba2hex(Quantico:::ReturnParam(xx = input[["ColorFont"]], Type = "character", Default = "#e2e2e2"))
        EchartsTheme <- Quantico:::ReturnParam(xx = input[["EchartsTheme"]], Type = "character", Default = "dark")

        if(Debug) {
          print("EDA Panel Report 2")
          print(names(ModelOutputList))
        }

        # Run Quantico:::Shiny.ML.ReportOutput
        if(Debug) {
          Output <- Quantico:::Shiny.EDA.ReportOutput(
            input, output, DataList, MachineLearningCode, Page,
            Debug = Debug,
            Theme = EchartsTheme,
            FontColor = FontColorData)
        } else {
          Output <- tryCatch({
            Quantico:::Shiny.EDA.ReportOutput(
              input, output, DataList, MachineLearningCode, Page,
              Debug = Debug,
              Theme = EchartsTheme,
              FontColor = FontColorData)
          }, error = function(x) NULL)
        }

        if(Debug) print("EDA Panel Report 3")

        # Collect output
        if(length(Output) > 0L) {
          EDAOutputList[[Page]] <- Output[["OutputList"]]; EDAOutputList <<- EDAOutputList
          DataList <- Output[["DataList"]]; DataList <<- DataList
          MachineLearningCode <- Output[["CodeList"]]; MachineLearningCode <<- MachineLearningCode

          if(Debug) {
            print("EDA Panel Report 4")
            print(names(EDAOutputList[[Page]]))
          }

          # Render Function
          if(length(EDAOutputList[[Page]]) > 0L) {
            Quantico:::Shiny.Display(
              input, output, session,
              EDAOutputList[[Page]], Debug,
              OutputId = paste0("EDAOutput",Page),
              Cols = 1,
              FontColor = FontColorData$flv,
              PM = names(EDAOutputList[[Page]]))
          }

          if(Debug) print("EDA Panel Report 5")

        }
      })
    }

  }, ignoreInit = TRUE)
  shiny::observeEvent({eval(EDAReportOutputExpression)}, {
    if(Debug) print("EDA Markdown 1")
    Page <- tryCatch({as.integer(gsub("[^\\d]+", "", input$tabss, perl=TRUE))}, error = function(x) 0L)
    if(length(DataList) > 0L) {
      shiny::withProgress(message = 'EDA Reporting Has Begun..', value = 0, {

        # Inputs
        EchartsTheme <- Quantico:::ReturnParam(xx = input[["EchartsTheme"]], Type = "character", Default = "dark")
        EDADataHTML <- DataList[[Quantico:::ReturnParam(xx = tryCatch({input[[paste0("EDAData", Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)]][['data']]
        UnivariateVars <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0("EDAUnivariateVars", Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
        CorrVars <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0("EDACorrVars", Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
        TrendVars <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0("EDATrendVars", Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
        TrendDateVar <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0("EDADateVar", Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
        TrendGroupVar <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0("EDAGroupVar", Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)

        if(Debug) {
          print("EDA Panel Report 2")
        }

        # Run Quantico:::Shiny.ML.ReportOutput
        if(Debug) {
          Quantico:::EDAReport(
            data = EDADataHTML,
            DataName = input[[paste0("EDAData", Page)]],
            UnivariateVars = UnivariateVars,
            CorrVars = CorrVars,
            TrendVars = TrendVars,
            TrendDateVar = TrendDateVar,
            TrendGroupVar = TrendGroupVar,
            OutputPath = WorkingDirectory)
        } else {
          tryCatch({
            Quantico:::EDAReport(
              data = EDADataHTML,
              DataName = input[[paste0("EDAData", Page)]],
              UnivariateVars = UnivariateVars,
              CorrVars = CorrVars,
              TrendVars = TrendVars,
              TrendDateVar = TrendDateVar,
              TrendGroupVar = TrendGroupVar,
              OutputPath = WorkingDirectory)
          }, error = function(x) NULL)
        }

        # Code Collect
        MachineLearningCode <- Quantico:::Shiny.CodePrint.Collect(y = MachineLearningCode, x = paste0(
          "\n",
          "# EDA RMarkdown Report\n",
          "Quantico:::EDAReport(\n\n  ",
          "DataName = ", Quantico:::CEP(input[[paste0("EDAData", Page)]]), ",\n  ",
          "UnivariateVars = ", Quantico:::ExpandText(UnivariateVars), ",\n  ",
          "CorrVars = ", Quantico:::ExpandText(CorrVars), ",\n  ",
          "TrendVars = ", Quantico:::ExpandText(TrendVars), ",\n  ",
          "TrendDateVar = ", Quantico:::CEP(TrendDateVar), ",\n  ",
          "TrendGroupVar = ", Quantico:::CEP(TrendGroupVar), ",\n  ",
          "OutputPath = ", Quantico:::CEP(WorkingDirectory), ")\n"))

        if(Debug) print("ML Markdown 2")
      })
    }

  }, ignoreInit = TRUE)

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event :: Inf Tabs Output      ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent({eval(InferenceOutputExpression)}, {

    if(Debug) print("Inference Panel Report 1")

    # Code Collection
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()

    # tabss refers to the entire tabsetPanel that houses the plotting, tables, and ml output panes
    # pane names are as such:
    #   Plots 1, Plots 2, ...
    #   Tables 1, Tables 2, ...
    #   ML 1, ML 2, ...
    Page <- tryCatch({as.integer(gsub("[^\\d]+", "", input$tabss, perl=TRUE))}, error = function(x) 0L)
    if(length(DataList) > 0L) {
      shiny::withProgress(message = 'Inference Reporting Has Begun..', value = 0, {

        if(Debug) {
          print("Inference Panel Report 2")
        }

        # Collect output
        if(length(InferenceOutputList) > 0L) {
          InfName <- Quantico::ReturnParam(xx = tryCatch({input[[paste0("InferenceReportsModelSelection", Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
          FontColorData <- Quantico:::rgba2hex(Quantico:::ReturnParam(xx = input[["ColorFont"]], Type = "character", Default = "#e2e2e2"))
          if(Debug) print("Inference Panel Report 4")

          # Update Plot Dims and Table Info
          EchartsTheme <- Quantico:::ReturnParam(xx = input[["EchartsTheme"]], Type = "character", Default = "dark")
          PlotWidthinf <- Quantico::ReturnParam(xx = tryCatch({input[[paste0("PlotWidthinf", Page)]]}, error = function(x) NULL), Type = "numeric", Default = 1450)
          PlotHeightinf <- Quantico::ReturnParam(xx = tryCatch({input[[paste0("PlotHeightinf", Page)]]}, error = function(x) NULL), Type = "numeric", Default = 850)
          PlotWidthinf <- paste0(PlotWidthinf, "px")
          PlotHeightinf <- paste0(PlotHeightinf, "px")

          if(Debug) print("Inference Panel Report 5")

          MinRowsa <- Quantico::ReturnParam(xx = tryCatch({input[[paste0("MinRowsa", Page)]]}, error = function(x) NULL), Type = "numeric", Default = 20)
          Compact <- Quantico::ReturnParam(xx = tryCatch({input[[paste0("Compact", Page)]]}, error = function(x) NULL), Type = "logical", Default = TRUE)
          Filterable <- Quantico::ReturnParam(xx = tryCatch({input[[paste0("Filterable", Page)]]}, error = function(x) NULL), Type = "logical", Default = TRUE)
          Striped <- Quantico::ReturnParam(xx = tryCatch({input[[paste0("Striped", Page)]]}, error = function(x) NULL), Type = "logical", Default = TRUE)

          if(Debug) print("Inference Panel Report 6")

          ioln <- names(InferenceOutputList[[InfName]])
          if(Debug) print(ioln)
          for(iolnn in ioln) {
            if(Debug) print("Inference Panel Report 7")
            if(class(InferenceOutputList[[InfName]][[iolnn]])[1L] == "echarts4r") {
              InferenceOutputList[[InfName]][[iolnn]]$width <- PlotWidthinf
              InferenceOutputList[[InfName]][[iolnn]]$height <- PlotHeightinf
              InferenceOutputList[[InfName]][[iolnn]]$x$theme <- EchartsTheme
            } else if(class(InferenceOutputList[[InfName]][[iolnn]])[1L] == "reactable") {
              InferenceOutputList[[InfName]][[iolnn]]$x$tag$attribs$defaultPageSize <- MinRowsa
              InferenceOutputList[[InfName]][[iolnn]]$x$tag$attribs$striped <- Striped
              InferenceOutputList[[InfName]][[iolnn]]$x$tag$attribs$filterable <- Filterable
              InferenceOutputList[[InfName]][[iolnn]]$x$tag$attribs$compact <- Compact
            }
          }

          if(Debug) print("Inference Panel Report 8")
          if(!exists("InfOutput")) InfOutput <- list()
          InfOutput[[Page]] <- InferenceOutputList[[InfName]]
          InfOutput <<- InfOutput

          # Render Function
          if(Debug) print("Inference Panel Report 9")
          if(length(InfOutput) > 0L) {
            if(Debug) print("Inference Panel Report 10")
            Quantico:::Shiny.Display(
              input, output, session,
              InfOutput[[Page]], Debug,
              OutputId = paste0("InferenceOutput",Page),
              Cols = 1,
              FontColor = FontColorData$flv,
              PM = names(InferenceOutputList[[InfName]]))
          }

          if(Debug) print("Inference Panel Report 5")

        }
      })
    }

  }, ignoreInit = TRUE)

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event :: ML Tabs Output       ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent({eval(MLOutputExpression)}, {

    if(Debug) print("ML Panel Report 1")

    # Code Collection
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()
    if(!exists("MLOutputList")) MLOutputList <- list()

    # tabss refers to the entire tabsetPanel that houses the plotting, tables, and ml output panes
    # pane names are as such:
    #   Plots 1, Plots 2, ...
    #   Tables 1, Tables 2, ...
    #   ML 1, ML 2, ...
    Page <- tryCatch({as.integer(gsub("[^\\d]+", "", input$tabss, perl=TRUE))}, error = function(x) 0L)
    if(length(ModelOutputList) > 0L) {
      shiny::withProgress(message = 'ML Reporting Has Begun..', value = 0, {

        # Inputs
        MLModelSelection <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0('MLReportsModelSelection',Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
        FontColorData <- Quantico:::rgba2hex(Quantico:::ReturnParam(xx = input[["ColorFont"]], Type = "character", Default = "#e2e2e2"))
        EchartsTheme <- Quantico:::ReturnParam(xx = input[["EchartsTheme"]], Type = "character", Default = "dark")

        if(Debug) {
          print("ML Panel Report 2")
          print(names(ModelOutputList))
        }

        # Run Quantico:::Shiny.ML.ReportOutput
        if(Debug) {
          MLOut <- Quantico:::Shiny.ML.ReportOutput(
            input, output, DataList, MachineLearningCode, Page,
            Debug = Debug,
            MOL = ModelOutputList,
            ModelID = MLModelSelection,
            Theme = EchartsTheme,
            FontColor = FontColorData)
        } else {
          MLOut <- tryCatch({
            Quantico:::Shiny.ML.ReportOutput(
              input, output, DataList, MachineLearningCode, Page,
              Debug = Debug,
              MOL = ModelOutputList,
              ModelID = MLModelSelection,
              Theme = EchartsTheme,
              FontColor = FontColorData)
          }, error = function(x) NULL)
        }

        if(Debug) print("ML Panel Report 3")

        # Collect output
        if(length(MLOut) > 0L) {
          MLOutputList[[Page]] <- MLOut[["OutputList"]]; MLOutputList <<- MLOutputList
          DataList <- MLOut[["DataList"]]; DataList <<- DataList
          MachineLearningCode <- MLOut[["CodeList"]]; MachineLearningCode <<- MachineLearningCode

          if(Debug) {
            print("ML Panel Report 4")
            print(names(MLOutputList[[Page]]))
          }

          # Render Function
          #if(length(MLOutputList) > 0L) {
            Quantico:::Shiny.Display(
              input, output, session,
              MLOutputList[[Page]], Debug,
              OutputId = paste0("MLOutput",Page),
              Cols = 1,
              FontColor = FontColorData$flv,
              PM = names(MLOutputList[[Page]]))
          #}

          if(Debug) print("ML Panel Report 5")

        }
      })
    }

  }, ignoreInit = TRUE)
  shiny::observeEvent({eval(MLReportOutputExpression)}, {
    if(Debug) print("ML Markdown 1")
    Page <- tryCatch({as.integer(gsub("[^\\d]+", "", input$tabss, perl=TRUE))}, error = function(x) 0L)
    if(length(ModelOutputList) > 0L) {
      shiny::withProgress(message = 'ML Report Exporting Has Begun..', value = 0, {

        # Inputs
        ModelID <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0('MLReportsModelSelection',Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
        FeatureColNames <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0('PDPVariables',Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
        SampleSize <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0('SampleSize',Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
        TrainDataInclude <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0('TrainDataInclude',Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)

        if(Debug) {
          print(ModelID)
          print(FeatureColNames)
          print(names(ModelOutputList))
        }

        # Build report
        Quantico::ModelInsightsReport(
          TrainDataInclude = TrainDataInclude,
          FeatureColumnNames = FeatureColNames,
          SampleSize = SampleSize,
          ModelID = ModelID,
          OutputPath = WorkingDirectory,
          ModelObject = ModelOutputList[[ModelID]])

        # Code Collect
        MachineLearningCode <- Quantico:::Shiny.CodePrint.Collect(y = MachineLearningCode, x = paste0(
          "\n",
          "# ML Build Model\n",
          "Quantico::ModelInsightsReport(\n\n  ",
          "FeatureColumnNames = ", Quantico:::ExpandText(FeatureColNames), ",\n  ",
          "ModelID = ", Quantico:::CEP(ModelID), ",\n  ",
          "OutputPath = ", Quantico:::CEP(WorkingDirectory), ",\n  ",
          "ModelObject = ModelOutputList)\n"))

        if(Debug) print("ML Markdown 2")
      })
    }

  }, ignoreInit = TRUE)

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event :: FC Tabs Output       ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent({eval(FCOutputExpression)}, {

    if(Debug) print("FC Panel Report 1")

    # Code Collection
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()
    if(!exists("FCOutputList")) FCOutputList <- list()

    # tabss refers to the entire tabsetPanel that houses the plotting, tables, and ml output panes
    # pane names are as such:
    #   Plots 1, Plots 2, ...
    #   Tables 1, Tables 2, ...
    #   FC 1, FC 2, ...
    Page <- tryCatch({as.integer(gsub("[^\\d]+", "", input$tabss, perl=TRUE))}, error = function(x) 0L)
    if(length(ModelOutputList) > 0L) {
      shiny::withProgress(message = 'FC Reporting Has Begun..', value = 0, {

        # Inputs
        FCModelSelection <- Quantico:::ReturnParam(xx = tryCatch({input[[paste0('FCReportsModelSelection',Page)]]}, error = function(x) NULL), Type = "character", Default = NULL)
        FontColorData <- Quantico:::rgba2hex(Quantico:::ReturnParam(xx = input[["ColorFont"]], Type = "character", Default = "#e2e2e2"))
        EchartsTheme <- Quantico:::ReturnParam(xx = input[["EchartsTheme"]], Type = "character", Default = "dark")

        if(Debug) {
          print("FC Panel Report 2")
          print(names(ModelOutputList))
        }

        saveRDS(object = DataList, file = file.path(WorkingDirectory, "DataList.rds"))

        # Run Quantico:::Shiny.FC.ReportOutput
        if(Debug) {
          FCOut <- Quantico:::Shiny.FC.ReportOutput(
            input, output, DataList, MachineLearningCode, Page,
            Debug = Debug,
            MOL = ModelOutputList,
            ModelID = FCModelSelection,
            RunMode = RunMode,
            Theme = EchartsTheme,
            FontColor = FontColorData)
        } else {
          FCOut <- tryCatch({
            Quantico:::Shiny.FC.ReportOutput(
              input, output, DataList, MachineLearningCode, Page,
              Debug = Debug,
              MOL = ModelOutputList,
              ModelID = FCModelSelection,
              RunMode = RunMode,
              Theme = EchartsTheme,
              FontColor = FontColorData)
          }, error = function(x) NULL)
        }

        if(Debug) print("FC Panel Report 3")

        # Collect output
        if(length(FCOut) > 0L) {
          FCOutputList[[Page]] <- FCOut[["OutputList"]]; FCOutputList <<- FCOutputList
          DataList <- FCOut[["DataList"]]; DataList <<- DataList
          MachineLearningCode <- FCOut[["CodeList"]]; MachineLearningCode <<- MachineLearningCode

          if(Debug) {
            print("FC Panel Report 4")
            print(names(FCOutputList[[Page]]))
          }

          # Render Function
          #if(length(FCOutputList) > 0L) {
          Quantico:::Shiny.Display(
            input, output, session,
            FCOutputList[[Page]], Debug,
            OutputId = paste0("FCOutput",Page),
            Cols = 1,
            FontColor = FontColorData$flv,
            PM = names(FCOutputList[[Page]]))
          #}

          if(Debug) print("FC Panel Report 5")

        }
      })
    }

  }, ignoreInit = TRUE)

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event :: Data Tabs Output     ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent({eval(DataOutputExpression)}, {

    # tabss refers to the entire tabsetPanel that houses the plotting, tables, and ml output panes
    # pane names are as such:
    #   Plots 1, Plots 2, ...
    #   Tables 1, Tables 2, ...
    #   ML 1, ML 2, ...
    Page <- tryCatch({as.integer(gsub("[^\\d]+", "", input$tabss, perl=TRUE))}, error = function(x) 0L)
    if(length(DataList) > 0L) {
      DataOuts <- tryCatch({input[[paste0("DataOutputSelection", Page)]]}, error = function(x) NULL)
      DataOuts <<- DataOuts
      if(length(DataOuts) > 0L && any(DataOuts != "No Data Loaded")) {

        # Reactable collection list
        DataOutputList <- list()

        # Adjustable Settings on Panel
        GridColsData <- Quantico::ReturnParam(xx = input[[paste0("GridColsData", Page)]], Type = "numeric", Default = 1)
        MinRows <- Quantico::ReturnParam(xx = input[[paste0("MinRows", Page)]], Type = "numeric", Default = 10)
        Number_of_Records <- Quantico::ReturnParam(xx = input[[paste0("Number_of_Records", Page)]], Type = "numeric", Default = 1000)
        Shuffle <- Quantico::ReturnParam(xx = input[[paste0("Shuffle", Page)]], Type = "logical", Default = FALSE)
        WrapText <- Quantico:::ReturnParam(xx = input[[paste0("WrapText", Page)]], Type = "logical", Default = FALSE)
        Compact <- Quantico:::ReturnParam(xx = input[[paste0("Compact", Page)]], Type = "logical", Default = FALSE)

        # Side panel settings
        FontColorData <- Quantico:::rgba2hex(Quantico:::ReturnParam(xx = input[["ColorFont"]], Type = "character", Default = "#e2e2e2"))

        # Prepare data, build reactable tables
        for(do in DataOuts) {

          # Ensure that ViewData is a data.table and that it's less than 1000k rows
          if(!Shuffle) {
            ViewData <- DataList[[do]]$data
            Number_of_Records <- min(Number_of_Records, ViewData[,.N])
            ViewData <- ViewData[seq_len(Number_of_Records)]
          } else {
            ViewData <- DataList[[do]]$data
            ViewData <- ViewData[order(runif(.N))][seq_len(min(.N, Number_of_Records))]
            Number_of_Records <- min(Number_of_Records, ViewData[,.N])
          }

          # Build Reactable Table
          DataOutputList[[do]] <- reactable::reactable(
            data = ViewData,
            compact = Compact,
            defaultPageSize = MinRows,
            wrap = WrapText,
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
              color = FontColorData$flv,
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

        # Render Function
        DataOutputList <<- DataOutputList
        Quantico:::Shiny.Display(
          input, output, session,
          DataOutputList, DebugPlottingOE,
          OutputId = paste0("DataOutput", Page), Cols = GridColsData, FontColor = FontColorData$flv, PM = DataOuts)
      }
    }
  }, ignoreInit = TRUE)

  #                                      ----

  #                                      ----

  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Obs Event :: Plot Tabs Output     ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::observeEvent({eval(PlotExecuteExpression)}, {

    # tabss refers to the entire tabsetPanel that houses the plotting, tables, and ml output panes
    # pane names are as such:
    #   Plots 1, Plots 2, ...
    #   Tables 1, Tables 2, ...
    #   ML 1, ML 2, ...
    TabNum <- tryCatch({as.integer(gsub("[^\\d]+", "", input$tabss, perl=TRUE))}, error = function(x) 0L)

    # Display Requests + Session Storage
    if(!exists("plot_output_list")) plot_output_list <- list()
    sel <- DisplayRequests <- input[[paste0('PlotTypeDragula',TabNum)]][['target']][['View']]
    if(length(DisplayRequests) > 0L) {

      ChoiceUpdate_old <- input[[paste0('PlotTypeDragula',TabNum)]][['source']]

      selList <- list()
      selList[['View']] <- DisplayRequests
      DragulaSelectedList[[TabNum]] <- selList
      DragulaSelectedList <<- DragulaSelectedList
      sel <<- sel
      ChoiceUpdate_old <<- ChoiceUpdate_old
      selList <<- selList

      for(j in seq_len(NumPlotsAvailable)) {
        if(length(ChoiceUpdate_old) > 0L) {
          for(c in ChoiceUpdate_old) data.table::set(PlotMap, i = which(PlotMap$PlotName == eval(c)), j = "Selected", value = FALSE)
        }
      }
      for(j in seq_len(NumPlotsAvailable)) {
        if(length(sel) > 0L) {
          for(c in sel) data.table::set(PlotMap, i = which(PlotMap$PlotName == eval(c)), j = "Selected", value = TRUE)
        }
      }
      PlotMap <<- PlotMap

      if(!exists("DisplayRequests_old")) DisplayRequests_old <- DisplayRequests

      DisplayRequests <<- DisplayRequests

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      # Display Request
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      if(DebugPlottingOE) print(paste0('# Dragula Name Matching'))
      PlotNumbers <- c()
      PMNs <- c()

      if(DebugPlottingOE) print(DisplayRequests)
      PMNs <- PlotMap[TabNumber == eval(TabNum)][["PlotNameOriginal"]]
      PlotNumbers <- c()
      for(s in sel) PlotNumbers <- c(PlotNumbers, PlotMap[TabNumber == eval(TabNum) & PlotName == eval(s)][["PlotNumber"]])

      # Plot dimensions
      if(!exists("PlotNumbers_old")) PlotNumbers_old <- PlotNumbers
      if(identical(PlotNumbers,PlotNumbers_old)) {
        PlotNumbers_old <<- PlotNumbers_old
        PlotNumbers <<- PlotNumbers
        PlotNumbersChange <- FALSE
      } else {
        PlotNumbers_old <- PlotNumbers
        PlotNumbers_old <<- PlotNumbers_old
        PlotNumbers <<- PlotNumbers
        PlotNumbersChange <- TRUE
      }

      # Plot dimensions
      nc <- input[[paste0("GridCols", TabNum)]]
      plotnumbers_temp <- seq_len(length(PlotNumbers))
      out <- Quantico:::PlotSize_(input, TabNum, nc, plotnumbers_temp)
      if(!exists("out_old")) out_old <- out
      if(identical(out,out_old)) {
        out_old <<- out_old
        out <<- out
        PlotDimsChange <- FALSE
      } else {
        out_old <- out
        out_old <<- out_old
        out <<- out
        PlotDimsChange <- TRUE
      }

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      # Global Themes
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      if(DebugPlottingOE) print("# Global Themes")
      gtl <- list()

      gtl[["PE"]] <- Quantico:::ReturnParam(xx = tryCatch({input[["PlottingFramework"]]}, error = function(x) "Echarts"), Type = "character", Default = "Echarts")
      cf3 <- Quantico:::rgba2hex(Quantico:::ReturnParam(xx = input[["ColorFont"]], Type = "character", Default = "#e2e2e2"))
      gtl[["ColorFont"]] <- cf3$flv
      gtl[["TL"]] <- Quantico:::ReturnParam(xx = input[["EchartsTimeLine"]],Type = "logical",Default = FALSE, Debug = FALSE)
      gtl[["EchartsTheme"]] <- Quantico:::ReturnParam(xx = input[["EchartsTheme"]], Type = "character", Default = "macarons")
      gtl[["NumLevelsDisplay"]] <- Quantico:::ReturnParam(xx = input[[paste0("Number_of_Levels", TabNum)]], Type = "numeric", Default = 20L, Debug = DebugPlottingOE)
      gtl[["FontSize"]] <- Quantico:::ReturnParam(xx = input[[paste0("FontSize", TabNum)]], Type = "numeric", Default = 14L, Debug = DebugPlottingOE)
      gtl[["Number_of_Bins"]] <- Quantico:::ReturnParam(xx = input[[paste0("Number_of_Bins", TabNum)]], Type = "numeric", Default = 21L, Debug = DebugPlottingOE)

      # Storage
      if(DebugPlottingOE) print("# gtl & gtl_old")
      if(!exists("gtl_old")) gtl_old <- gtl
      if(identical(gtl,gtl_old)) {
        gtl_old <<- gtl_old
        gtl <<- gtl
        GlobalChange <- FALSE
      } else {
        gtl_old <- gtl
        gtl_old <<- gtl_old
        gtl <<- gtl
        GlobalChange <- TRUE
      }

      # Total Formatting Change
      GlobalChange <- GlobalChange || PlotDimsChange || PlotNumbersChange

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      # Build Plots:
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      if(DebugPlottingOE) print(PlotNumbers)
      if(length(PlotNumbers) > 0L) {
        if(!exists("SubsetList")) {
          SubsetList <- list()
          SubsetList <<- SubsetList
        }

        # Build plots
        if(DebugPlottingOE) {print("Quantico:::Shiny.Plot.Build :::::::::::::::::::::::::::::::::");print(PlotNumbers);print(TabNum);print(NumPlotsAvailable); print(length(PlotDropDown))}
        PlotOutput <- tryCatch({Quantico:::Shiny.Plot.Build(
          input,output,session,
          PlotNumbers, DataList, DebugPlottingOE, PlotMap,
          PlotterCode, gtl[["EchartsTheme"]], OutputPage = TabNum,
          PlotEngine = gtl[["PE"]], Timeline = gtl[["TL"]],ColorFont = gtl[["ColorFont"]],
          NumLevelsDisplay = gtl[["NumLevelsDisplay"]], NumberOfBins = gtl[["Number_of_Bins"]],
          GlobalChange = GlobalChange, PlotHeight = out$PlotHeight, PlotWidth = out$PlotWidth,
          Args = PlotDropDown, SubsetList = SubsetList)}, error = function(x) NULL)

        if(length(PlotOutput) > 0L) {
          SubsetList <- PlotOutput$SubsetList; SubsetList <<- SubsetList
          PlotterCode <- PlotOutput$CodeList; PlotterCode <<- PlotterCode
          DisplayPlots <- PlotOutput$PlotList; DisplayPlots <<- DisplayPlots
          FinanceDataList <- PlotOutput$FinanceDataList; FinanceDataList <<- FinanceDataList
        } else {
          if(DebugPlottingOE) print("Plot ITERATION WAS NULL")
        }

      } else {
        if(DebugPlottingOE) print("SKIPPING THE BUILD PROCESS: nothing changed")
        PlotOutput <- NULL
      }

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      # Display Plots
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      if(DebugPlottingOE) print("Display Plots")
      Cols <- input[[paste0("GridCols", TabNum)]]
      if(length(PlotOutput) > 0L) {
        Quantico:::Shiny.Display(
          input, output, session,
          DisplayPlots, DebugPlottingOE,
          OutputId = paste0("PlottingExecute", TabNum),
          Cols = Cols, FontColor = gtl[["ColorFont"]], PM = PMNs)

      } else {

        if(DebugPlottingOE) print('RenderUI White Hole')
        if(!exists("plot_output_list")) plot_output_list <- list()
        plot_output_list[[TabNum]] <- echarts4r::renderEcharts4r({
          Quantico:::BlankEchart(Type = "Poincare", Theme = tryCatch({EchartsTheme}, error = function(x) "macarons"))
        })
        out <- Quantico:::PlotSize_(input, 1, 1, 1)
        if(length(out) == 0L) {
          out <- list(); out$PlotHeight <- '860px'; out$PlotWidth <- '1450px'
        }
        if(length(plot_output_list[[TabNum]]) > 0L) {
          attr(plot_output_list[[TabNum]], 'outputArgs') <- list(height = out$PlotHeight[1L], width = out$PlotWidth[1L])
          output[[paste0("PlottingExecute", TabNum)]] <- shiny::renderUI({plot_output_list[[TabNum]]})
          shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Data not defined", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
        } else {
          shinyWidgets::sendSweetAlert(session, title = NULL, text = NULL, type = NULL, btn_labels = "Internal Error", btn_colors = NULL, html = FALSE, closeOnClickOutside = TRUE, showCloseButton = TRUE, width = "40%")
        }
      }

    } else {

      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      # Blank Plots if Building Fails
      # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      if(DebugPlottingOE) print('RenderUI Super Nova')
      plot_output_list[[TabNum]] <- echarts4r::renderEcharts4r({
        Quantico:::BlankEchart(Type = "Poincare", Theme = tryCatch({EchartsTheme}, error = function(x) "macarons"))
      })
      out <- Quantico:::PlotSize_(input, 1, 1, 1)
      if(length(out) == 0L) {out <- list(); out$PlotHeight <- '860px'; out$PlotWidth <- '1450px'}
      if(length(plot_output_list[[TabNum]]) > 0L) {
        attr(plot_output_list[[TabNum]], 'outputArgs') <- list(height = out$PlotHeight[1L], width = out$PlotWidth[1L])
        output[[paste0("PlottingExecute", TabNum)]] <- shiny::renderUI({plot_output_list[[TabNum]]})
      }
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent({eval(PlottingReportOutputExpression)}, {
    if(Debug) print("EDA Markdown 1")
    Page <- tryCatch({as.integer(gsub("[^\\d]+", "", input$tabss, perl=TRUE))}, error = function(x) 0L)
    if(length(DataList) > 0L) {
      shiny::withProgress(message = 'Plots Reporting Has Begun..', value = 0, {

        if(Debug) {
          print(length(DisplayPlots))
        }

        if(length(DisplayPlots) > 0L) {

          # Run Quantico:::Shiny.ML.ReportOutput
          if(Debug) {
            Quantico:::PlottingReport(
              PlotOutputList = DisplayPlots,
              OutputPath = WorkingDirectory)
          } else {
            tryCatch({
              Quantico:::PlottingReport(
                PlotOutputList = DisplayPlots,
                OutputPath = WorkingDirectory)
            }, error = function(x) NULL)
          }

          # Code Collect
          MachineLearningCode <- Quantico:::Shiny.CodePrint.Collect(y = MachineLearningCode, x = paste0(
            "\n",
            "# Plotting RMarkdown Build\n",
            "Quantico:::EDAReport(\n\n  ",
            "PlotOutputList = ", Quantico:::CEP(input[[paste0("EDAData", Page)]]), ",\n  ",
            "OutputPath = ", Quantico:::CEP(WorkingDirectory), ")\n"))
        }
        if(Debug) print("ML Markdown 2")
      })
    }

  }, ignoreInit = TRUE)

  #                                      ----

  #               ----
  #                                      ----


  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  # :: Close app after closing browser   ----
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
  shiny::onStop(function() {
    cat("This will run on session stop\n")
    if(!exists('DataMgtCode')) DataMgtCode <- list()
    if(!exists('DataWranglingCode')) DataWranglingCode <- list()
    if(!exists('FeatureEngineeringCode')) FeatureEngineeringCode <- list()
    if(!exists('MachineLearningCode')) MachineLearningCode <- list()
    if(!exists('ForecastingCode')) ForecastingCode <- list()
    if(!exists('PlotterCode')) PlotterCode <- list()
    MasterSet <- tryCatch({Quantico:::Shiny.CodePrint.OrganizeCode(DM = DataMgtCode, DW = DataWranglingCode, FE = FeatureEngineeringCode, ML = MachineLearningCode, FC = ForecastingCode, PL = PlotterCode)}, error = function(x) NULL)
    if(Debug) {
      print(length(MasterSet))
      print(length(MasterSet$Code))
      print(MasterSet)
      print(class(MasterSet))
    }
    # if(is.data.table(MasterSet)) data.table::fwrite(MasterSet, file = file.path("C:/Users/Bizon/Documents/GitHub", paste0("Guest", "_", format(Sys.time(), "%Y%m%d_%H%M%S_"), ".csv")))
  })

  # For App Initialization, to get those tabs up and available
  shinyjs::click(id = 'NewEDATab')
  shinyjs::click(id = 'NewInferenceTab')
  shinyjs::click(id = 'NewMLTab')
  shinyjs::click(id = 'NewFCTab')
  shinyjs::click(id = 'NewDataTab')
  shinyjs::click(id = 'NewPlotTab')
}

# ----

# ----

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
#    Run App                           ----
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
shiny::shinyApp(
  ui = ui,
  server = server,
  enableBookmarking = "disable")

# ----

# ----

