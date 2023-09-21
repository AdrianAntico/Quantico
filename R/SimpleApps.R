#' @title Muse
#'
#' @description Shiny web app with capabilities in data managment, data wrangling, feature engineering, machine learning, statistical modeling, forecasting, visualization, and code print outs in multiple languages.
#'
#' @author Adrian Antico
#' @family GUI
#'
#' @param MaxPlots 12L
#' @param MaxPlotTabs 10L
#' @param AzureCredsFile Path to your credentials file
#' @param WorkingDirectory getwd() or your preferred location. Saving local data will output to this directory
#' @param UserName_Password_DT NULL. In order to enforce authentication, supply a data.table with columns 'UserName' which contains the names of your users and 'Password' which contains the acceptable passwords. E.g. data.table::data.table(UserName = c('Adrian Antico', 'Guest'), Password = c('Password1', 'Password2')). Case sensitivity applies.
#' @param PostGRE_DBNames Names of warehouses you want available from your postgres database
#' @param PostGRE_Host Host for your postgres database
#' @param PostGRE_Port Port for your postgres database
#' @param PostGRE_User Username for your postgres database
#' @param PostGRE_Password Password for your postgres database
#' @param Debug FALSE
#' @param DebugSessionLoad FALSE
#' @param DebugAddPlotTab FALSE
#' @param DebugPlotButtonTextUpdate FALSE
#' @param DebugAddPlotButton FALSE
#' @param DebugPlottingOE FALSE
#' @param DebugFC Debugging for Forecasting
#'
#' @examples
#' \dontrun{
#' # Pull Data
#' # data <- data.table::fread(system.file('tests/QA_DataSets/ThreeGroup-FC-Walmart-XREG3.csv', package = "AutoQuant"))
#' # data[, Date := as.Date(Date)]
#'
#' # Creds file should look like this in data.table form. Two columns, Account and Values.
#'
#' # Account                Values
#' # StorageAccount         rshinyapps
#' # Container              blobstorageaccess
#' # Key                    ...
#' # PostGRE_User           postgres
#' # PostGRE_Password       ....
#' # PostGRE_Host           local
#' # PostGRE_Port           5432
#'
#' # Run App
#' Quantico::rapp()
#'
#' }
#'
#' @export
Muse <- function(UserName_Password_DT = NULL,

                 # Plotting
                 MaxPlots = 24L,
                 MaxPlotTabs = 2L,

                 # Data Management
                 AzureCredsFile = NULL,
                 WorkingDirectory = getwd(),
                 PostGRE_DBNames = NULL,
                 PostGRE_Host = NULL,
                 PostGRE_Port = NULL,
                 PostGRE_User = NULL,
                 PostGRE_Password = NULL,

                 # QA
                 Debug = FALSE,
                 DebugSessionLoad = FALSE,
                 DebugAddPlotTab = FALSE,
                 DebugPlotButtonTextUpdate = FALSE,
                 DebugAddPlotButton = FALSE,
                 DebugPlottingOE = FALSE,
                 DebugFC = FALSE) {

  CacheDir = NULL
  CacheName = 'data.csv'

  # Pass args to shiny app
  shiny::shinyOptions(
    MaxPlots = MaxPlotTabs * 12,
    MaxPlotTabs = MaxPlotTabs,
    AzureCredsFile = AzureCredsFile,
    WorkingDirectory = WorkingDirectory,
    PostGRE_DBNames = PostGRE_DBNames,
    PostGRE_Host = PostGRE_Host,
    PostGRE_Port = PostGRE_Port,
    PostGRE_User = PostGRE_User,
    PostGRE_Password = PostGRE_Password,
    CacheDir = CacheDir,
    CacheName = CacheName,
    UserName_Password_DT = UserName_Password_DT,
    Debug = Debug,
    DebugSessionLoad = DebugSessionLoad,
    DebugAddPlotTab = DebugAddPlotTab,
    DebugPlotButtonTextUpdate = DebugPlotButtonTextUpdate,
    DebugAddPlotButton = DebugAddPlotButton,
    DebugPlottingOE = DebugPlottingOE,
    DebugFC = DebugFC)

  # Run shiny app
  # shiny::runApp(appDir = file.path("C:/Users/Bizon/Documents/GitHub/Quantico/inst/shiny-apps/Quantico"), display.mode = "normal", launch.browser = TRUE)
  shiny::runApp(appDir = system.file("shiny-apps","Quantico", package = "Quantico"), display.mode = "normal", launch.browser = TRUE)
}
