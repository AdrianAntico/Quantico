TextLabelStyle = "background-color: #0000;border: none;padding: 1px;color: white;text-align: center;font-size: 17px;"
ButtonStyle <- "color:#FFFFFF; border-color:#979797; border-style:solid; border-width: 1px; border-radius:30px; font-size:14px;"
LogoutButtonStyle <- "width = 100%; color: #f3f3f3; font: bold; background-color: #5b6888; border-style:solid; border-width:1px; border-radius:3%; font-size:14px;"
AnimationTypes <- c(
  "bounce",
  "flash",
  "pulse",
  "rubberBand",
  "shake",
  "headShake",
  "swing",
  "tada",
  "wobble",
  "jello",
  "bounceIn",
  "bounceInDown",
  "bounceInLeft",
  "bounceInRight",
  "bounceInUp",
  "bounceOut",
  "bounceOutDown",
  "bounceOutLeft",
  "bounceOutRight",
  "bounceOutUp",
  "fadeIn",
  "fadeInDown",
  "fadeInDownBig",
  "fadeInLeft",
  "fadeInLeftBig",
  "fadeInRight",
  "fadeInRightBig",
  "fadeInUp",
  "fadeInUpBig",
  "fadeOut",
  "fadeOutDown",
  "fadeOutDownBig",
  "fadeOutLeft",
  "fadeOutLeftBig",
  "fadeOutRight",
  "fadeOutRightBig",
  "fadeOutUp",
  "fadeOutUpBig",
  "flip",
  "flipInX",
  "flipInY",
  "flipOutX",
  "flipOutY",
  "lightSpeedOut",
  "rotateIn",
  "rotateInDownLeft",
  "rotateInDownRight",
  "rotateInUpLeft",
  "rotateInUpRight",
  "rotateOut",
  "rotateOutDownLeft",
  "rotateOutDownRight",
  "rotateOutUpLeft",
  "rotateOutUpRight",
  "hinge",
  "jackInTheBox",
  "rollIn",
  "rollOut",
  "zoomIn",
  "zoomInDown",
  "zoomInLeft",
  "zoomInRight",
  "zoomInUp",
  "zoomOut",
  "zoomOutDown",
  "zoomOutLeft",
  "zoomOutRight",
  "zoomOutUp",
  "slideInDown",
  "slideInLeft",
  "slideInRight",
  "slideInUp",
  "slideOutDown",
  "slideOutLeft",
  "slideOutRight",
  "slideOutUp")

#' @title LoginInputs
#'
#' @description Box with UserName and PassWord Inputs. Username inputId is 'UserName'. PassWord inputId is 'Password'
#'
#' @author Adrian Antico
#' @family Login
#'
#' @param id inputId
#' @param BoxStatus = 'danger'
#' @param UserNameLabel = "UserName"
#' @param PassWordLabel = 'Input Password'
#' @param UserNameChoices = Credentials[['UserName']]
#' @param BoxTitle = NULL
#' @param SolidHeader = TRUE
#' @param Collapsible = FALSE
#'
#' @export
LoginInputs <- function(id = 'LoginBox',
                        BoxStatus = 'danger',
                        UserNameLabel = "Select from Names",
                        PassWordLabel = 'Input Password',
                        UserNameChoices = Credentials[['UserName']],
                        BoxTitle = NULL,
                        SolidHeader = TRUE,
                        Collapsible = FALSE) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DataMuse:::BlankRow(12L),
    shinydashboard::box(
      title = BoxTitle,
      solidHeader = SolidHeader,
      collapsible = Collapsible,
      status = BoxStatus,
      width = 12L,
      shiny::uiOutput('UserName'),
      shiny::uiOutput('Password')))
}

#' @title LoginButton
#'
#' @description Action Button for Checking Credentials
#'
#' @author Adrian Antico
#' @family Login
#'
#' @param id 'Check_Credentials'
#' @param Label = 'Check Credentials',
#' @param width = 1L
#' @param Icon = shiny::icon('chevron-right', lib = 'font-awesome')
#' @param Style = 'gradient'
#' @param Color = 'royal'
#'
#' @export
LoginButton <- function(id = 'LoginButton',
                        Label = 'Check Credentials',
                        width = 1L,
                        Icon = shiny::icon('chevron-right', lib = 'font-awesome'),
                        Style = 'gradient',
                        Color = 'royal') {
  ns <- shiny::NS(id)
  shiny::tagList(
    DataMuse:::BlankRow(12L),
    shiny::fluidRow(
      shiny::column(
        width = 2,
        align = 'center',
        shiny::actionButton("Check_Credentials", label = NULL, icon = shiny::icon("share-from-square"), style = ButtonStyle)
  )))
}

#' @title LogoutButton
#'
#' @description Action Button for Checking Credentials
#'
#' @author Adrian Antico
#' @family Login
#'
#' @param id 'Check_Credentials'
#' @param Label = 'Check Credentials',
#' @param width = 1L
#' @param Icon = shiny::icon('chevron-right', lib = 'font-awesome')
#' @param Style = 'gradient'
#' @param Color = 'royal'
#'
#' @export
LogoutButton <- function(id = 'LogoutButton',
                         Label = 'Reset Session',
                         width = 1L,
                         Icon = NULL,
                         Style = 'gradient',
                         Color = 'royal') {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align = 'center',
      shiny::actionButton(
        "Logout",
        label = Label,
        icon = shiny::icon("person-walking-dashed-line-arrow-right"),
        style = LogoutButtonStyle,
        width = "100%")
  ))
}

#' @title DM_Session
#'
#' @description Import and export session states from your local environment or azure blob storage
#'
#' @author Adrian Antico
#' @family DM
#'
#' @param id = input
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
DM_Session <- function(id = 'DM_Session',
                     Style = 'gradient',
                     Color = 'royal',
                     Align='center',
                     DropDownRight=FALSE,
                     Animate=TRUE,
                     Status='custom',
                     H3Color = 'white')  {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('Sessions')),
      shiny::actionLink("Session_Modal", label = "Sessions", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title DM_Local
#'
#' @description Import and export from your local environment
#'
#' @author Adrian Antico
#' @family DM
#'
#' @param id = input
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
DM_Local <- function(id = 'DM_Local',
                     Style = 'gradient',
                     Color = 'royal',
                     Align='center',
                     DropDownRight=FALSE,
                     Animate=TRUE,
                     Status='custom',
                     H3Color = 'white')  {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('Local')),
      shiny::actionLink("Local_Modal", label = "Local", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title DM_PostGRE
#'
#' @description DM_PostGRE Button
#'
#' @author Adrian Antico
#' @family DM
#'
#' @param id = input
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
DM_PostGRE <- function(id = 'DM_PostGRE',
                       Style = 'gradient',
                       Color = 'royal',
                       Align='center',
                       DropDownRight=FALSE,
                       Animate=TRUE,
                       Status='custom',
                       H3Color = 'white')  {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('PostGRE')),
      shiny::actionLink("PostGRE_Modal", label = "PostGRE", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title DM_AzureBlob
#'
#' @description DM_AzureBlob Button
#'
#' @author Adrian Antico
#' @family DM
#'
#' @param id = input
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
DM_AzureBlob <- function(id = 'DM_AzureBlob',
                         Style = 'gradient',
                         Color = 'royal',
                         Align='center',
                         DropDownRight=FALSE,
                         Animate=TRUE,
                         Status='custom',
                         H3Color = 'white')  {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('Azure')),
      shiny::actionLink("AzureBlob_Modal", label = "Azure Blob", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title DW_ShrinkData
#'
#' @description Subset, Aggregate, and Sample data
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param id = 'DW_ShrinkData'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
DW_ShrinkData <- function(id='DW_ShrinkData',
                          Style = 'gradient',
                          Color = 'royal',
                          Align='center',
                          DropDownRight=FALSE,
                          Animate=TRUE,
                          Status='custom',
                          H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 11L,
      align=Align,
      # tags$h4(tags$b('Shrink')),
      shiny::actionLink(inputId = "Shrink_Modal", label = "Shink", icon = shiny::icon("right-from-bracket"))
  ))
}

#' @title DW_GrowData
#'
#' @description Join or Union data
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param id = 'DW_GrowData'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
DW_GrowData <- function(id='DW_GrowData',
                          Style = 'gradient',
                          Color = 'royal',
                          Align='center',
                          DropDownRight=FALSE,
                          Animate=TRUE,
                          Status='custom',
                          H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      # tags$h4(tags$b('Grow')),
      shiny::actionLink("Grow_Modal", label = "Grow", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title DW_DataSets
#'
#' @description Data set operations
#'
#' @author Adrian Antico
#' @family FE
#'
#' @param id = 'DW_DataSets'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
DW_DataSets <- function(id='DW_DataSets',
                        Style = 'gradient',
                        Color = 'royal',
                        Align='center',
                        DropDownRight=FALSE,
                        Animate=TRUE,
                        Status='custom',
                        H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('Dataset')),
      shiny::actionLink("DataSets_Modal", label = "DataSet", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title DW_Pivot
#'
#' @description Time Series Fill Missing Dates
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param id = 'DW_Pivot'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
DW_Pivot <- function(id='DW_Pivot',
                     Style = 'gradient',
                     Color = 'royal',
                     Align='center',
                     DropDownRight=FALSE,
                     Animate=TRUE,
                     Status='custom',
                     H3Color = 'white') {

  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('Pivot')),
      shiny::actionLink("Pivot_Modal", label = "Pivot", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title DW_Misc
#'
#' @description Time Series Fill Missing Dates
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param id = 'DW_Misc'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
DW_Misc <- function(id='DW_Misc',
                      Style = 'gradient',
                      Color = 'royal',
                      Align='center',
                      DropDownRight=FALSE,
                      Animate=TRUE,
                      Status='custom',
                      H3Color = 'white') {

  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('Misc')),
      shiny::actionLink("Misc_Modal", label = "Miscellaneous", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title DW_Columns
#'
#' @description Delete columns from a data.table
#'
#' @author Adrian Antico
#' @family DW
#'
#' @param id = 'DW_Columns'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
DW_Columns <- function(id='DW_Columns',
                    Style = 'gradient',
                    Color = 'royal',
                    Align='center',
                    DropDownRight=FALSE,
                    Animate=TRUE,
                    Status='custom',
                    H3Color = 'white') {

  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('Columns')),
      shiny::actionLink("Columns_Modal", label = "Columns", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title FE_Windowing
#'
#' @description Windowing args
#'
#' @author Adrian Antico
#' @family FE
#'
#' @param id = 'FE_Windowing'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FE_Windowing <- function(id='FE_Windowing',
                        Style = 'gradient',
                        Color = 'royal',
                        Align='center',
                        DropDownRight=FALSE,
                        Animate=TRUE,
                        Status='custom',
                        H3Color = 'white') {

  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('Rolling')),
      shiny::actionLink("Windowing_Modal", label = "Windowing", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title FE_DateVariables
#'
#' @description Calendar Variables Dropdown
#'
#' @author Adrian Antico
#' @family FE
#'
#' @param id = 'FE_DateVariables'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FE_DateVariables <- function(id='FE_DateVariables',
                             Style = 'gradient',
                             Color = 'royal',
                             Align='center',
                             DropDownRight=FALSE,
                             Animate=TRUE,
                             Status='custom',
                             H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('Calendar')),
      shiny::actionLink("Calendar_Modal", label = "Calendar", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title FE_CategoricalVariables
#'
#' @description Categorical Variables Dropdown
#'
#' @author Adrian Antico
#' @family FE
#'
#' @param id = 'CategoricalVariables'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FE_CategoricalVariables <- function(id='CategoricalVariables',
                                    Style = 'gradient',
                                    Color = 'royal',
                                    Align='center',
                                    DropDownRight=TRUE,
                                    Animate=TRUE,
                                    Status='custom',
                                    H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('Categorical')),
      shiny::actionLink("Categorical_Modal", label = "Categorical", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title FE_NumericVariables
#'
#' @description Numeric Variables Dropdown
#'
#' @author Adrian Antico
#' @family FE
#'
#' @param id = 'NumericVariables'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FE_NumericVariables <- function(id='NumericVariables',
                                Style = 'gradient',
                                Color = 'royal',
                                Align='center',
                                DropDownRight=FALSE,
                                Animate=TRUE,
                                Status='custom',
                                H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('Numeric')),
      shiny::actionLink("Numeric_Modal", label = "Numeric", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title FE_AnomalyDetection
#'
#' @description Unsupervised learning
#'
#' @author Adrian Antico
#' @family FE
#'
#' @param id = 'ModelBased_Modal'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FE_AnomalyDetection <- function(id='AnomalyDetection_Modal',
                          Style = 'gradient',
                          Color = 'royal',
                          Align='center',
                          DropDownRight=FALSE,
                          Animate=TRUE,
                          Status='custom',
                          H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('Anomaly')),
      shiny::actionLink("AnomalyDetection_Modal", label = "Anomaly Detection", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title FE_DimensionalityReduction
#'
#' @description Unsupervised learning
#'
#' @author Adrian Antico
#' @family FE
#'
#' @param id = 'ModelBased_Modal'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FE_DimensionalityReduction <- function(id='DimensionalityReduction_Modal',
                                       Style = 'gradient',
                                       Color = 'royal',
                                       Align='center',
                                       DropDownRight=FALSE,
                                       Animate=TRUE,
                                       Status='custom',
                                       H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('DimReduction')),
      shiny::actionLink("DimensionalityReduction_Modal", label = "Dim Reduction", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title FE_Clustering
#'
#' @description Unsupervised learning
#'
#' @author Adrian Antico
#' @family FE
#'
#' @param id = 'ModelBased_Modal'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FE_Clustering <- function(id='ClusteringID',
                          Style = 'gradient',
                          Color = 'royal',
                          Align='center',
                          DropDownRight=FALSE,
                          Animate=TRUE,
                          Status='custom',
                          H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('Clustering')),
      shiny::actionLink("Clustering_Modal", label = "Clustering", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title FE_NLP
#'
#' @description NLP
#'
#' @author Adrian Antico
#' @family FE
#'
#' @param id = 'NLP_Modal'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FE_NLP <- function(id='NLP_Modal',
                   Style = 'gradient',
                   Color = 'royal',
                   Align='center',
                   DropDownRight=FALSE,
                   Animate=TRUE,
                   Status='custom',
                   H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('NLP')),
      shiny::actionLink("NLP_Modal", label = "NLP", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title ML_CatBoostReport
#'
#' @description Catboost report
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'CatBoost'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
ML_CatBoostReport <- function(id='ML_CatBoostReport',
                              Style = 'gradient',
                              Color = 'royal',
                              Align='center',
                              DropDownRight=FALSE,
                              Animate=TRUE,
                              Status='custom',
                              H3Color = 'white') {

  Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12L,
        align = 'center',
        tags$h4(tags$b('Report')),
        shiny::actionButton("CatBoostReport", label = NULL, icon = shiny::icon("share-from-square"), style = ButtonStyle)
      )))
}

#' @title ML_XGBoostReport
#'
#' @description XGboost report
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'XGBoost'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
ML_XGBoostReport <- function(id='ML_XGBoostReport',
                             Style = 'gradient',
                             Color = 'royal',
                             Align='center',
                             DropDownRight=FALSE,
                             Animate=TRUE,
                             Status='custom',
                             H3Color = 'white') {

  Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12L,
        align = 'center',
        shiny::actionButton("XGBoostReport", label = NULL, icon = shiny::icon("share-from-square"), style = ButtonStyle)
      )))
}

#' @title ML_LightGBMReport
#'
#' @description LightGBM report
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'XGBoost'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
ML_LightGBMReport <- function(id='ML_LightGBMReport',
                              Style = 'gradient',
                              Color = 'royal',
                              Align='center',
                              DropDownRight=FALSE,
                              Animate=TRUE,
                              Status='custom',
                              H3Color = 'white') {

  Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12L,
        align = 'center',
        tags$h4(tags$b('Report')),
        shiny::actionButton("LightGBMReport", label = NULL, icon = shiny::icon("share-from-square"), style = ButtonStyle)
      )))
}

#' @title ML_Scoring
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'CatBoost'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
ML_Scoring <- function(id='ML_Scoring_link',
                       Style = 'gradient',
                       Color = 'royal',
                       Align='center',
                       DropDownRight=FALSE,
                       Animate=TRUE,
                       Status='custom',
                       H3Color = 'white') {

  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      shiny::actionLink("ScoreML_Modal", label = "ML Scoring", icon = shiny::icon("right-from-bracket"))
    ))
}


#' @title ML_CatBoost
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'CatBoost'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
ML_CatBoost <- function(id='CatBoostML',
                        Style = 'gradient',
                        Color = 'royal',
                        Align='center',
                        DropDownRight=FALSE,
                        Animate=TRUE,
                        Status='custom',
                        H3Color = 'white') {

  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('CatBoost')),
      shiny::actionLink("CatBoost_Modal", label = "CatBoost", icon = shiny::icon("right-from-bracket"))
  ))
}

#' @title ML_XGBoost
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'XGBoost'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
ML_XGBoost <- function(id='XGBoostML',
                       Style = 'gradient',
                       Color = 'royal',
                       Align='center',
                       DropDownRight=FALSE,
                       Animate=TRUE,
                       Status='custom',
                       H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('XGBoost')),
      shiny::actionLink("XGBoost_Modal", label = "XGBoost", icon = shiny::icon("right-from-bracket"))# , style = ButtonStyle)
  ))
}

#' @title ML_LightGBM
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'LightGBM'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
ML_LightGBM <- function(id='LightGBMML',
                        Style = 'gradient',
                        Color = 'royal',
                        Align='center',
                        DropDownRight=TRUE,
                        Animate=TRUE,
                        Status='custom',
                        H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('LightGBM')),
      shiny::actionLink("LightGBM_Modal", label = "LightGBM", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title ML_H2O_DRF
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'H2O_DRF'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
ML_H2O_DRF <- function(id='H2O_DRFML',
                        Style = 'gradient',
                        Color = 'royal',
                        Align='center',
                        DropDownRight=TRUE,
                        Animate=TRUE,
                        Status='custom',
                        H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('H2O.DRF')),
      shiny::actionLink("H2O_DRF_Modal", label = "H2O DRF", icon = shiny::icon("right-from-bracket"))# , style = ButtonStyle)
  ))
}

#' @title ML_H2O_GBM
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'H2O_GBM'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
ML_H2O_GBM <- function(id='H2O_GBMML',
                       Style = 'gradient',
                       Color = 'royal',
                       Align='center',
                       DropDownRight=TRUE,
                       Animate=TRUE,
                       Status='custom',
                       H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      # tags$h4(tags$b('H2O.GBM')),
      shiny::actionLink("H2O_GBM_Modal", label = "H2O GBM", icon = shiny::icon("right-from-bracket"))# , style = ButtonStyle)
  ))
}

#' @title ML_H2O_GLM
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'H2O_GLM'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
ML_H2O_GLM <- function(id='H2O_GLMML',
                       Style = 'gradient',
                       Color = 'royal',
                       Align='center',
                       DropDownRight=TRUE,
                       Animate=TRUE,
                       Status='custom',
                       H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('H2O.GLM')),
      shiny::actionLink("H2O_GLM_Modal", label = "H2O GLM", icon = shiny::icon("right-from-bracket"))# , style = ButtonStyle)
  ))
}

#' @title ML_H2O_HGLM
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'H2O_HGLM'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
ML_H2O_HGLM <- function(id='H2O_HGLMML',
                       Style = 'gradient',
                       Color = 'royal',
                       Align='center',
                       DropDownRight=TRUE,
                       Animate=TRUE,
                       Status='custom',
                       H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('H2O.HGLM')),
      shiny::actionLink("H2O_HGLM_Modal", label = "H2O HGLM", icon = shiny::icon("right-from-bracket"))# , style = ButtonStyle)
  ))
}

#' @title ML_MixedEffects
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'ML_MixedEffects'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
ML_MixedEffects <- function(id='ML_MixedEffects',
                            Style = 'gradient',
                            Color = 'royal',
                            Align='center',
                            DropDownRight=TRUE,
                            Animate=TRUE,
                            Status='custom',
                            H3Color = 'white') {
  Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 1L,
      align=Align,
      tags$h4(tags$b('MixedEffects')),
      shiny::actionButton("MixedEffects_Modal", label = NULL, icon = shiny::icon("share-from-square"), style = ButtonStyle)
  ))
}

#' @title ML_GEE
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'ML_GEE'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
ML_GEE <- function(id='ML_GEE',
                   Style = 'gradient',
                   Color = 'royal',
                   Align='center',
                   DropDownRight=TRUE,
                   Animate=TRUE,
                   Status='custom',
                   H3Color = 'white') {
  Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 1L,
      align=Align,
      tags$h4(tags$b('GEE')),
      shiny::actionButton("GEE_Modal", label = NULL, icon = shiny::icon("share-from-square"), style = ButtonStyle)
  ))
}

#' @title ML_CausalMediation
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'ML_CausalMediation'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
ML_CausalMediation <- function(id='ML_CausalMediationML',
                               Style = 'gradient',
                               Color = 'royal',
                               Align='center',
                               DropDownRight=TRUE,
                               Animate=TRUE,
                               Status='custom',
                               H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('Mediation')),
      shiny::actionLink("CausalMediation_Modal", label = "Causal Mediation", icon = shiny::icon("right-from-bracket"))# , style = ButtonStyle)
  ))
}

#' @title FC_SARIMA
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'FC_SARIMA_Args'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FC_SARIMA <- function(id='FC_SARIMA',
                      Style = 'gradient',
                      Color = 'royal',
                      Align='center',
                      DropDownRight=TRUE,
                      Animate=TRUE,
                      Status='custom',
                      H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      shiny::actionLink("SARIMA_Modal", label = "SARIMA", icon = shiny::icon("right-from-bracket"))
    ))
}

#' @title FC_NNet
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'FC_NNet'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FC_NNet <- function(id='FC_NNet',
                    Style = 'gradient',
                    Color = 'royal',
                    Align='center',
                    DropDownRight=TRUE,
                    Animate=TRUE,
                    Status='custom',
                    H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      shiny::actionLink("NNET_Modal", label = "NNET", icon = shiny::icon("right-from-bracket"))
    ))
}

#' @title FC_TBATS
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'FC_TBATS'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FC_TBATS <- function(id='FC_TBATS',
                     Style = 'gradient',
                     Color = 'royal',
                     Align='center',
                     DropDownRight=TRUE,
                     Animate=TRUE,
                     Status='custom',
                     H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      shiny::actionLink("TBATS_Modal", label = "TBATS", icon = shiny::icon("right-from-bracket"))
    ))
}

#' @title FC_ETS
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'FC_ETS'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FC_ETS <- function(id='FC_ETS',
                   Style = 'gradient',
                   Color = 'royal',
                   Align='center',
                   DropDownRight=TRUE,
                   Animate=TRUE,
                   Status='custom',
                   H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      shiny::actionLink("ETS_Modal", label = "ETS", icon = shiny::icon("right-from-bracket"))
    ))
}

#' @title FC_Arfima
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'FC_Arfima'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FC_Arfima <- function(id='FC_Arfima',
                      Style = 'gradient',
                      Color = 'royal',
                      Align='center',
                      DropDownRight=TRUE,
                      Animate=TRUE,
                      Status='custom',
                      H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      shiny::actionLink("ARFIMA_Modal", label = "ARFIMA", icon = shiny::icon("right-from-bracket"))
    ))
}

#' @title FC_CatBoostCARMA
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'FC_CatBoostCARMA'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FC_CatBoostCARMA <- function(id='FC_CatBoostCARMA',
                             Style = 'gradient',
                             Color = 'royal',
                             Align='center',
                             DropDownRight=TRUE,
                             Animate=TRUE,
                             Status='custom',
                             H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('CatBoost Panel')),
      shiny::actionLink("CatBoostCARMA_Modal", label = "CatBoost", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title FC_XGBoostCARMA
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'FC_XGBoostCARMA'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FC_XGBoostCARMA <- function(id='FC_XGBoostCARMA',
                             Style = 'gradient',
                             Color = 'royal',
                             Align='center',
                             DropDownRight=TRUE,
                             Animate=TRUE,
                             Status='custom',
                             H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('XGBoost Panel')),
      shiny::actionLink("XGBoostCARMA_Modal", label = "XGBoost", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title FC_LightGBMCARMA
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'FC_LightGBMCARMA'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FC_LightGBMCARMA <- function(id='FC_LightGBMCARMA',
                            Style = 'gradient',
                            Color = 'royal',
                            Align='center',
                            DropDownRight=TRUE,
                            Animate=TRUE,
                            Status='custom',
                            H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('LightGBM Panel')),
      shiny::actionLink("LightGBMCARMA_Modal", label = "LightGBM", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title FC_CatBoostCARMAID
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'FC_CatBoostCARMA'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FC_CatBoostCARMAID <- function(id='FC_CatBoostCARMAID',
                             Style = 'gradient',
                             Color = 'royal',
                             Align='center',
                             DropDownRight=TRUE,
                             Animate=TRUE,
                             Status='custom',
                             H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('CatBoost IDemand')),
      shiny::actionLink("CatBoostCARMA_Modal", label = "CatBoost", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title FC_XGBoostCARMAID
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'FC_XGBoostCARMA'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FC_XGBoostCARMAID <- function(id='FC_XGBoostCARMAID',
                            Style = 'gradient',
                            Color = 'royal',
                            Align='center',
                            DropDownRight=TRUE,
                            Animate=TRUE,
                            Status='custom',
                            H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('XGBoost IDemand')),
      shiny::actionLink("XGBoostCARMA_Modal", label = "XGBoost", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title FC_LightGBMCARMAID
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'FC_LightGBMCARMA'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FC_LightGBMCARMAID <- function(id='FC_LightGBMCARMAID',
                             Style = 'gradient',
                             Color = 'royal',
                             Align='center',
                             DropDownRight=TRUE,
                             Animate=TRUE,
                             Status='custom',
                             H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('LightGBM IDemand')),
      shiny::actionLink("LightGBMCARMA_Modal", label = "LightGBM", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title FC_CatBoostCARMAFUN
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'FC_CatBoostCARMA'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FC_CatBoostCARMAFUN <- function(id='FC_CatBoostCARMAFUN',
                               Style = 'gradient',
                               Color = 'royal',
                               Align='center',
                               DropDownRight=TRUE,
                               Animate=TRUE,
                               Status='custom',
                               H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('CatBoost Funnel')),
      shiny::actionLink("CatBoostCARMA_Modal", label = "CatBoost", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title FC_XGBoostCARMAFUN
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'FC_XGBoostCARMA'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FC_XGBoostCARMAFUN <- function(id='FC_XGBoostCARMAFUN',
                              Style = 'gradient',
                              Color = 'royal',
                              Align='center',
                              DropDownRight=TRUE,
                              Animate=TRUE,
                              Status='custom',
                              H3Color = 'white') {
 #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('XGBoost Funnel')),
      shiny::actionLink("XGBoostCARMA_Modal", label = "XGBoost", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title FC_LightGBMCARMAID
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'FC_LightGBMCARMA'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
FC_LightGBMCARMAFUN <- function(id='FC_LightGBMCARMAFUN',
                               Style = 'gradient',
                               Color = 'royal',
                               Align='center',
                               DropDownRight=TRUE,
                               Animate=TRUE,
                               Status='custom',
                               H3Color = 'white') {
  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('LightGBM Funnel')),
      shiny::actionLink("LightGBMCARMA_Modal", label = "LightGBM", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
  ))
}

#' @title BuildModelsButton
#'
#' @author Adrian Antico
#' @family Modeling
#'
#' @param id = 'MLBuildButton'
#' @param Algo = 'CatBoost'
#' @param Style = 'gradient'
#' @param Color = 'royal'
#'
#' @export
BuildModelsButton <- function(id = 'MLBuildButton',
                              Algo = 'CatBoost',
                              Style = 'gradient',
                              Color = 'royal')  {
  ns <- shiny::NS(id)
  shiny::tagList(
    tags$h4(tags$b('Build')),
    shiny::column(
      width = 1L,
      shinyjs::useShinyjs(),
      align = 'center',
      shiny::actionButton(paste0('BuildModels_', Algo), label = NULL, icon = shiny::icon("share-from-square"), style = ButtonStyle)
  ))
}


#' @title Inference_Normality
#'
#' @author Adrian Antico
#' @family Inference
#'
#' @param id = 'Inference_Normality'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
Inference_Normality <- function(id='Inference_Normality_link',
                                Style = 'gradient',
                                Color = 'royal',
                                Align='center',
                                DropDownRight=FALSE,
                                Animate=TRUE,
                                Status='custom',
                                H3Color = 'white') {

  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('Pivot')),
      shiny::actionLink("Inference_Normality", label = "Normality", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
    ))
}

#' @title Inference_Correlation
#'
#' @author Adrian Antico
#' @family Inference
#'
#' @param id = 'Inference_Correlation'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
Inference_Correlation <- function(id='Inference_Correlation_link',
                                  Style = 'gradient',
                                  Color = 'royal',
                                  Align='center',
                                  DropDownRight=FALSE,
                                  Animate=TRUE,
                                  Status='custom',
                                  H3Color = 'white') {

  #Height <- '43px'
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      #tags$h4(tags$b('Pivot')),
      shiny::actionLink("Inference_Correlation", label = "Correlation", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
    ))
}

#' @title Inference_1STTest
#'
#' @author Adrian Antico
#' @family Inference
#'
#' @param id = 'Inference_1STTest'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
Inference_1STTest <- function(id='Inference_1STTest_link',
                              Style = 'gradient',
                              Color = 'royal',
                              Align='center',
                              DropDownRight=FALSE,
                              Animate=TRUE,
                              Status='custom',
                              H3Color = 'white') {

  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      shiny::actionLink("Inference_1STTest", label = "1-Sample T-Test", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
    ))
}

#' @title Inference_2STTest
#'
#' @author Adrian Antico
#' @family Inference
#'
#' @param id = 'Inference_2STTest'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
Inference_2STTest <- function(id='Inference_2STTest_link',
                              Style = 'gradient',
                              Color = 'royal',
                              Align='center',
                              DropDownRight=FALSE,
                              Animate=TRUE,
                              Status='custom',
                              H3Color = 'white') {

  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      shiny::actionLink("Inference_2STTest", label = "2-Sample T-Test", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
    ))
}

#' @title Inference_FTest
#'
#' @author Adrian Antico
#' @family Inference
#'
#' @param id = 'Inference_FTest'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
Inference_FTest <- function(id='Inference_FTest_link',
                            Style = 'gradient',
                            Color = 'royal',
                            Align='center',
                            DropDownRight=FALSE,
                            Animate=TRUE,
                            Status='custom',
                            H3Color = 'white') {

  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      shiny::actionLink("Inference_FTest", label = "F-Test", icon = shiny::icon("right-from-bracket"))#, style = ButtonStyle)
    ))
}

#' @title Inference_ChiSq
#'
#' @author Adrian Antico
#' @family Inference
#'
#' @param id = 'Inference_ChiSq'
#' @param Style = 'gradient',
#' @param Color = 'royal',
#' @param Align = 'center'
#' @param DropDownRight = FALSE
#' @param Animate = TRUE
#' @param Status = 'custom'
#' @param H3Color = 'white'
#'
#' @export
Inference_ChiSq <- function(id='Inference_ChiSq_link',
                                  Style = 'gradient',
                                  Color = 'royal',
                                  Align='center',
                                  DropDownRight=FALSE,
                                  Animate=TRUE,
                                  Status='custom',
                                  H3Color = 'white') {

  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::column(
      width = 12L,
      align=Align,
      shiny::actionLink("Inference_ChiSq", label = "Chi-Square", icon = shiny::icon("right-from-bracket"))
    ))
}
