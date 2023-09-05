#' @title HomePage
#'
#' @description Panels
#'
#' @author Adrian Antico
#' @family Plotting
#'
#' @param id = 'PlotPanels'
#' @param AppWidth = 12L
#' @param Page numeric
#' @param NumPlotsAvailable numeric
#' @param DragulaChoices character
#'
#' @export
HomePage <- function(id, Page, AppWidth=12L) {
  ns <- shiny::NS(id)
  shiny::tabsetPanel(
    id = 'tabs', selected = 'View', type = 'tabs',

    #  ----

    #  ----

    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    # Home Tab                             ----
    # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
    shiny::tabPanel(
      title = "Documentation",
      icon = shiny::icon("book"),
      shiny::fluidRow(
        width = 12L,
        style = "padding-left: 15px",
        id = "DocsID",
        DataMuse::BlankRow(12),
        DataMuse::BlankRow(12),

        shiny::fluidRow(
          width=12L,
          style="padding-left: 15px",
          id = "PlotAddButtonsUILocation",
          #DataMuse:::BlankRow(12),

          # ----

          # ----

          # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
          # Side Bar                             ----
          # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
          shiny::column(
            2L, style = "padding-right: 5px;",
            shinydashboard::box(
              title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'info', width = 12L,
              style = "background-color: #0000; min-height: 1440px; max-height: 1440px; overflow-x: clip; overflow-y:auto;",
              shiny::fluidRow(
                style = "padding-left: 15px; padding-right: 20px; background-color: #0000; overflow-y: auto; overflow-x: clip;",
                DataMuse:::BlankRow(12L),

                #  ----

                #  ----

                # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
                # Plotting                             ----
                # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
                shiny::tags$details(
                  style = "padding-left: 5px",
                  shiny::tags$summary(shiny::tags$span(shiny::tags$b('Plotting'))),
                  DataMuse::BlankLine(12L),
                  shiny::a(shiny::icon("newspaper"), href="#visualization", "Plotting"),

                  DataMuse::BlankRow(12L),
                  shiny::tags$details(
                    style = "padding-left: 5px",
                    shiny::tags$summary(shiny::tags$span(shiny::tags$b('Standard Plots'))),
                    DataMuse::BlankLine(12L),

                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#standardplots", "Standard Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#histogramplots", "Histogram")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#densityplots", "Density Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#boxplots", "Box Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#pieplots", "Pie Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#probabilityplots", "Probability Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#donutplots", "Donut Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#rosetypeplots", "Rosetype Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#wordcloudplots", "Word Cloud Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#areaplots", "Area Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#lineplots", "Line Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#riverplots", "River Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#stepplots", "Step Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#autocorrplots", "Autocorr Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#partialautocorrplots", "PartialAutocorr Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#barplots", "Bar Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#stackedbarplots", "Stacked Bar Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#3dbarplots", "3D Bar Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#radarplots", "Radar Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#scatterplots", "Scatter Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#3dscatterplots", "3D Scatter Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#copulaplots", "Copula Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#3dcopulaplots", "3D Copula Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#heatmapplots", "Heatmap Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#correlogramplots", "Correlogram Plots"))),

                  DataMuse::BlankRow(12L),
                  shiny::tags$details(
                    style = "padding-left: 5px",
                    shiny::tags$summary(shiny::tags$span(shiny::tags$b('Model Evaluation'))),
                    DataMuse::BlankLine(12L),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#modelevaluationplots", "Model Evaluation Plots")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#residualhistogram", "Residual Histogram")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#residualscatterplot", "Residual Scatter")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#calibrationline", "Calibration Line")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#calibrationbox", "Calibration Box")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#partialdependenceline", "Partial Dependence Line")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#partialdependenceheatmap", "Partial Dependence Heatmap")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#variableimportance", "Variable Importance")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#shapleyimportance", "Shapley Importance")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#roc", "ROC Plot")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#confusionmatrix", "Confusion Matrix")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#lift", "Lift Plot")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#gains", "Gains Plot"))),

                ),

                # ----

                # ----

                # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
                # Data Wrangling Tab                   ----
                # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
                DataMuse::BlankRow(12L),
                shiny::tags$details(
                  style = "padding-left: 5px",
                  shiny::tags$summary(shiny::tags$span(shiny::tags$b('Data Wrangling'))),
                  DataMuse::BlankLine(12L),
                  shiny::a(shiny::icon("newspaper"), href="#datawrangling", "Data Wrangling"),

                  DataMuse::BlankRow(12L),
                  shiny::tags$details(
                    style = "padding-left: 5px",
                    shiny::tags$summary(shiny::tags$span(shiny::tags$b('Shink'))),
                    DataMuse::BlankLine(12L),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#shrink", "Shrink")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#aggregate", "Aggregate Data")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#aggregate", "Subset Rows")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#aggregate", "Subset Columns")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#aggregate", "Sample Data"))
                  ),

                  DataMuse::BlankRow(12L),
                  shiny::tags$details(
                    style = "padding-left: 5px",
                    shiny::tags$summary(shiny::tags$span(shiny::tags$b('Grow'))),
                    DataMuse::BlankLine(12L),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#grow", "Grow")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#join", "Join Data")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#union", "Union Rows"))
                  ),

                  DataMuse::BlankRow(12L),
                  shiny::tags$details(
                    style = "padding-left: 5px",
                    shiny::tags$summary(shiny::tags$span(shiny::tags$b('Dataset'))),
                    DataMuse::BlankLine(12L),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#dataset", "Dataset")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#partition", "Partition Data")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#sort", "Sort Data")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#modeldataprep", "Model Data Prep")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#remove", "Remove Data"))
                  ),

                  DataMuse::BlankRow(12L),
                  shiny::tags$details(
                    style = "padding-left: 5px",
                    shiny::tags$summary(shiny::tags$span(shiny::tags$b('Pivot'))),
                    DataMuse::BlankLine(12L),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#pivot", "Pivot")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#melt", "Melt Data")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#cast", "Cast Data"))
                  ),

                  DataMuse::BlankRow(12L),
                  shiny::tags$details(
                    style = "padding-left: 5px",
                    shiny::tags$summary(shiny::tags$span(shiny::tags$b('Columns'))),
                    DataMuse::BlankLine(12L),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#columns", "Columns")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#typecasting", "Type Cast Columns")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#rename", "Rename Columns")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#timetrend", "Time Trend Column")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#concat", "Concat Colunms"))
                  ),

                  DataMuse::BlankRow(12L),
                  shiny::tags$details(
                    style = "padding-left: 5px",
                    shiny::tags$summary(shiny::tags$span(shiny::tags$b('Misc'))),
                    DataMuse::BlankLine(12L),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#misc", "Misc")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#metaprogramming", "Meta Programming")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#timeseriesfill", "Time Series Fill")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#timeseriesfillroll", "Time Series Fill Roll"))
                  )
                ),

                #  ----

                #  ----

                # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
                # Feature Engineering Tab              ----
                # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
                DataMuse::BlankRow(12L),
                shiny::tags$details(
                  style = "padding-left: 5px",
                  shiny::tags$summary(shiny::tags$span(shiny::tags$b('Feature Engineering'))),
                  DataMuse::BlankLine(12L),
                  shiny::a(shiny::icon("newspaper"), href="#featureengineering", "Feature Engineering"),

                  DataMuse::BlankRow(12L),
                  shiny::tags$details(
                    style = "padding-left: 5px",
                    shiny::tags$summary(shiny::tags$span(shiny::tags$b('Anomaly Detection'))),
                    DataMuse::BlankLine(12L),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#anomalydetection", "Anomaly Detection"))
                  ),

                  DataMuse::BlankRow(12L),
                  shiny::tags$details(
                    style = "padding-left: 5px",
                    shiny::tags$summary(shiny::tags$span(shiny::tags$b('Dim Reduction'))),
                    DataMuse::BlankLine(12L),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#dimensionalityreduction", "Dim Reduction")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#h2oautoencoder", "H2O Autoencoder"))
                  ),

                  DataMuse::BlankRow(12L),
                  shiny::tags$details(
                    style = "padding-left: 5px",
                    shiny::tags$summary(shiny::tags$span(shiny::tags$b('Clustering'))),
                    DataMuse::BlankLine(12L),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#clustering", "Clustering")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#h2oclustering", "H2O Clustering"))
                  ),

                  DataMuse::BlankRow(12L),
                  shiny::tags$details(
                    style = "padding-left: 5px",
                    shiny::tags$summary(shiny::tags$span(shiny::tags$b('NLP'))),
                    DataMuse::BlankLine(12L),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#nlp", "NLP")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#h2oword2vec", "H2O Word2Vec")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#textsummary", "Text Summary")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#sentiment", "Sentiment")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#readability", "Readability")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#lexicaldiversity", "Lexical Diversity")),
                  ),

                  DataMuse::BlankRow(12L),
                  shiny::tags$details(
                    style = "padding-left: 5px",
                    shiny::tags$summary(shiny::tags$span(shiny::tags$b('Rolling'))),
                    DataMuse::BlankLine(12L),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#rolling", "Rolling")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#numericvariables", "Rolling Numeric")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#diffvariables", "Differencing Variables")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#categoricalvariables", "Rolling Categorical"))
                  ),

                  DataMuse::BlankRow(12L),
                  shiny::tags$details(
                    style = "padding-left: 5px",
                    shiny::tags$summary(shiny::tags$span(shiny::tags$b('Categorical'))),
                    DataMuse::BlankLine(12L),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#categorical", "Categorical")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#categoricalencoding", "Categorical Encoding")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#dummyvariables", "Dummy Variables"))
                  ),

                  DataMuse::BlankRow(12L),
                  shiny::tags$details(
                    style = "padding-left: 5px",
                    shiny::tags$summary(shiny::tags$span(shiny::tags$b('Calendar'))),
                    DataMuse::BlankLine(12L),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#calendar", "Calendar")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#calendarvariables", "Calendar Variables")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#dummyvariables", "Dummy Variables"))
                  ),

                  DataMuse::BlankRow(12L),
                  shiny::tags$details(
                    style = "padding-left: 5px",
                    shiny::tags$summary(shiny::tags$span(shiny::tags$b('Numeric'))),
                    DataMuse::BlankLine(12L),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#numeric", "Numeric")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#percentrank", "Percent Rank Variables")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#standardize", "Standardize Variables")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#transform", "Transform Variables")),
                    shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#interaction", "Interaction Variables"))
                  )
                ),

                #  ----

                #  ----

                # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
                # ML + Inference Tab                   ----
                # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
                DataMuse::BlankRow(12L),
                shiny::tags$details(
                  style = "padding-left: 5px",
                  shiny::tags$summary(shiny::tags$span(shiny::tags$b('ML + Inference'))),
                  DataMuse::BlankLine(12L),
                  shiny::a(shiny::icon("newspaper"), href="#mlinference", "ML + Inference"),
                  shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#catboost", "CatBoost")),
                  shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#xgboost", "XGBoost")),
                  shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#lightgbm", "LightGBM")),
                  shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#h2odrf", "H2O-DRF")),
                  shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#h2ogbm", "H2O-GBM")),
                  shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#h2oglm", "H2O-GLM")),
                  shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#h2ohglm", "H2O-HGLM")),
                  shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#causalmediation", "Causal Mediation"))
                ),

                #  ----

                #  ----

                # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
                # Forecasting Tab                      ----
                # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
                DataMuse::BlankRow(12L),
                shiny::tags$details(
                  style = "padding-left: 5px",
                  shiny::tags$summary(shiny::tags$span(shiny::tags$b('Forecasting'))),
                  DataMuse::BlankLine(12L),
                  shiny::a(shiny::icon("newspaper"), href="#forecasting", "Forecasting")
                  # shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#catboostpanel", "CatBoost Panel")),
                  # shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#xgboostpanel", "XGBoost Panel")),
                  # shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#lightgbmpanel", "LightGBM Panel")),
                  # shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#catboostidemand", "CatBoost IDemand")),
                  # shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#xgboostidemand", "XGBoost IDemand")),
                  # shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#lightgbmidemand", "LightGBM IDemand")),
                  # shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#catboostfunnel", "CatBoost Funnel")),
                  # shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#xgboostfunnel", "XGBoost Funnel")),
                  # shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#lightgbmfunnel", "LightGBM Funnel"))
                ),

                #  ----

                #  ----

                # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
                # Settings                             ----
                # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ ----
                DataMuse::BlankRow(12L),
                shiny::tags$details(
                  style = "padding-left: 5px",
                  shiny::tags$summary(shiny::tags$span(shiny::tags$b('Settings'))),
                  DataMuse::BlankLine(12L),
                  shiny::a(shiny::icon("newspaper"), href="#settings", "Settings"),
                  shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#appthemes", "App Themes")),
                  shiny::fluidRow(style="padding-left: 5px", shiny::a(shiny::icon("newspaper"), href="#plotthemes", "Plot Themes"))
                )
              )
            )
          ),

          # ----

          # ----

          ################################################# ----
          # Content Portion of Docs                         ----
          ################################################# ----
          shiny::column(
            10L, style = "padding-right: 47px;",
            shinydashboard::box(
              title = NULL, solidHeader = TRUE, collapsible = FALSE, status = 'info', width = 12L,
              style = "background-color: #0000; min-height: 1440px; max-height: 1440px; overflow-x: clip; overflow-y:auto;",

              # ----

              # ----

              ################################################# ----
              # Plotting                                        ----
              ################################################# ----
              DataMuse::BlankRow(12L),
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "visualization"),
                    shiny::h2(tags$b("Plotting")),
                    shiny::p("Plotting is a vitally important aspect of this software. It's important that you know how to
                        utilize the functionality as intended. One of the goals is to make plotting as easy as possible.
                        You don't have to pre-aggrgate or prepare your data for plotting. Just pass it in and utilize the
                        inputs to tell the software what you want."),
                    shiny::p("Several plot buttons appear on app startup. If you add an additional Plotting Tab another set will appear.
                             These buttons store the plotting metadata needed to generate each plot and they correspond
                             to the plot boxes that appear in the dragula pane one the Plot output pages. You can change the name above the
                             plot buttons to help keep track of what you've put together already."),
                    shiny::p("Below are all of the plot types with descriptions about how to use each and every one for each of
                        their intended uses. While there are far too many combinations of each type I will be focusing on
                        the core aspects and let the user decide which combination makes the most sense for their use case.
                        The given info will be sufficient for users to generate any one of those combinations."),
                    DataMuse::BlankRow(12L),
                    shiny::h1(shiny::tags$b("Basic Usage:")),
                    shiny::p("For the Create Plots buttons, you will go through a few steps to define your plot information"),
                    shiny::markdown(
                      "
                      * First, you'll be presented with a menu of plot type buttons to press. Press the one you want built.
                      * Next, a modal will appear with up to five tabs for inputs:
                        * Data Selection Tab
                        * Axis Variables Tab
                        * Grouping Variables Tab (in most cases but not all)
                        * Filter Variables Tab
                        * Formatting Tab
                      * Data Selection screen is where you'll choose your data and number of display records
                      * Axis Variables screen is where you'll define your axis variables and any transformations you'd like applied
                        * Each variable input accepts a single variable (except for a river plots and CorrMatrix) but because the few that accept more than one the inputs accept multiple values. If multiple values are selected on a plot that does not support them, the subsequent variables will be ignored.
                      * Group Variables screen is where you'll optionally define any group variables and faceting (if applicable)
                      * Filter Variables screen is where you can optionally filter data before having the plot displayed
                      * Formatting screen is where you can enable values to be shown and rename the title and axis titles
                      "
                    ),
                    DataMuse::BlankRow(12L),
                    shiny::h1(shiny::tags$b("Important Features:")),
                    shiny::h3(shiny::tags$b("Numeric Transformations")),
                    shiny::markdown(
                      "
                      If you'd like to transform a numeric variable the options set is below:
                      * 'Asinh': inverse hyperbolic sine
                      * 'Log': natural logarithm
                      * 'LogPlus1' (natural log(x + absolute value of minimum value if min value is negative))
                      * 'Sqrt': square root
                      * 'Asin': inverse sine
                      * 'Logit'
                      * 'PercRank': Percentile Rank of a value
                      * 'Standardize'
                      * 'BoxCox'
                      * 'YeoJohnson'
                      "),

                    DataMuse::BlankRow(12L),
                    shiny::h3(shiny::tags$b("Group Variables and Aggregations")),
                    shiny::markdown(
                      "
                      * Date Time Aggregation: e.g. if you data is on a daily grain you can have it aggregated it to a weekly grain
                      * Group Variables: you can choose up to three group variables and the columns will be concatenated for the final plot
                      * Group Variables: you can filter for any group levels you want to be displayed
                      * Timeline: there is a option
                      * Grouping Variables Aggregation Method: you don't have to pre-aggregate your data for plotting. Aggreation stats include:
                        * 'count' Counts of values by group. Here, you need to select any of the numeric YVars available in your data just so it doesn't create an error for a missing YVar
                        * 'proportion' Proportion of total by group. Here, you need to select any of the numeric YVars available in your data just so it doesn't create an error for a missing YVar
                        * 'mean'
                        * 'meanabs' (absolute values are taken first, then the measure)
                        * 'median'
                        * 'medianabs' (absolute values are taken first, then the measure)
                        * 'sum'
                        * 'sumabs' (absolute values are taken first, then the measure)
                        * 'sd' (standard deviation)
                        * 'sdabs' (absolute values are taken first, then the measure)
                        * 'skewness'
                        * 'skewnessabs' (absolute values are taken first, then the measure)
                        * 'kurtosis'
                        * 'kurtosisabs' (absolute values are taken first, then the measure)
                        * 'CoeffVar' (coefficient of variation)
                        * 'CoeffVarabs' (absolute values are taken first, then the measure)
                      * Faceting: for plots with by groups you can select to have the output faceted into a grid of plots
                      "),

                    DataMuse::BlankRow(12L),
                    shiny::h3(shiny::tags$b("Filters and Formatting")),
                    shiny::markdown(
                      "
                      * Filter Variables: you can have your data filtered before being plotted, for several variables
                      * Theme customizations: there are 41 different plot themes to choose from
                      * Plot Formatting: you can modify Titles, axes titles, and show labels
                      * Plot Output: hover values, filter selection for Y & X-Variable, Y or X-Variable along, lasso filtering, and image saving
                      * Horizontal scrollers (and sometimes vertical scrollers)
                      "
                    ),
                    DataMuse::BlankRow(12L),
                    shiny::h3(shiny::tags$b("Plotting Color Themes")),
                    shiny::markdown(
                      "
                      * On the side bar, within Settings, you'll find an option to change up the plotting theme which modifies the colors used on the plot
                      * Here is the full list of available themes (you can view each in the Settings section of this Documentation Page):
                      * 'auritus'
                      * 'azul'
                      * 'bee-inspired'
                      * 'blue'
                      * 'caravan'
                      * 'carp'
                      * 'chalk'
                      * 'cool'
                      * 'dark-bold'
                      * 'dark'
                      * 'eduardo'
                      * 'essos'
                      * 'forest'
                      * 'fresh-cut'
                      * 'fruit'
                      * 'gray'
                      * 'green'
                      * 'halloween'
                      * 'helianthus'
                      * 'infographic'
                      * 'inspired'
                      * 'jazz'
                      * 'london'
                      * 'macarons'
                      * 'macarons2'
                      * 'mint'
                      * 'purple-passion'
                      * 'red-velvet'
                      * 'red'
                      * 'roma'
                      * 'royal'
                      * 'sakura'
                      * 'shine'
                      * 'tech-blue'
                      * 'vintage'
                      * 'walden'
                      * 'wef'
                      * 'weforum'
                      * 'westeros'
                      * 'wonderland'
                      "
                    )
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ----

              # Standard Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "standardplots"),
                    shiny::h1(tags$b("Standard Plots")),
                    shiny::p("Standard Plot types are those that we encounter in a general setting.
                           These plots get used the most frequently and apply to most data and
                           contexts."),
                    DataMuse::BlankRow(12L),
                    shiny::h3(tags$b("Standard Plots Types")),
                    shiny::markdown(
                      "
                      * Histogram
                      * Density
                      * Box
                      * Pie
                      * Donut
                      * Rosetype
                      * Wordcloud
                      * Area
                      * Line
                      * River
                      * Step
                      * Autocorrelation
                      * PartialAutocorr
                      * Bar
                      * Stacked Bar
                      * 3D Bar
                      * Scatter
                      * 3D Scatter
                      * Copula
                      * 3D Copula
                      * Heatmap
                      * Correlogram
                      "
                    )
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Histograms ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "histogramplots"),
                    shiny::h3("Histogram Plots"),
                    shiny::p("If you interesting in seeing where the peak(s) of your distribution lie,
                  or whether the distribution is skewed or symmetric, or if there are any outliers,
                  a histogram plot is a great choice. Also consider Density Plots.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "Histogram.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/Histogram.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Histogram Plots Usage"))),
                shiny::markdown(
                  "
                * Histogram plots only require a numeric Y-Variable to be defined
                * Group Variables are optional but I would recommend Faceting in you choose to supply one
                * If all the data rest close to the 0-value of the X-Variable, a variable transformation
                  can come in handy to normalize the data. I tend to use LogPlus1 in those situations
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Density ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "densityplots"),
                    shiny::h3("Density Plots"),
                    shiny::p("Density plots are a variation of Histograms. It charts continuous values from a
                         data set as equally binned distributions. It uses kernel smoothing to smoothen out the noise.
                         Thus, the plots are smooth across bins and are not affected by the number of bins created,
                         which helps create a more defined distribution shape. If you interesting in seeing where the peak(s) of your distribution lie,
                         or whether the distribution is skewed or symmetric, or if there are any outliers,
                         a density plot is a great choice. Also consider Histogram Plots.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "Density.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/Density.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Density Plots Usage"))),
                shiny::markdown(
                  "
                * Density plots only require a numeric Y-Variable to be defined
                * Group Variables are optional but I would recommend Faceting in you choose to supply one
                * If all the data rest close to the 0-value of the X-Variable, a variable transformation
                  can come in handy to normalize the data. I tend to use LogPlus1 in those situations
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Box Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "boxplots"),
                    shiny::h3("Box Plots"),
                    shiny::p("A Box Plot is for viewing the locality, spread, and skewness groups of a numeric variable
                  through their quartiles. Outliers that differ significantly from the rest of the column will
                  be plotted as individual points beyond the whiskers on the Box Plot.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "BoxPlot.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/BoxPlot.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Box Plots Usage"))),
                shiny::markdown(
                  "
                * Box plots only require a numeric Y-Variable but also accepts and a categorical X-Variable
                * Group Variables are not optional with these as the X-Variable takes care of this task
                * If all the data rest close to the 0-value of the Y-Variable, a variable transformation
                  can come in handy to normalize the data. I tend to use LogPlus1 in those situations
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Pie Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "pieplots"),
                    shiny::h3("Pie Charts"),
                    shiny::p("Pie Charts types are used when you are looking to understand
                           the frequency and spread of your data. Data is visualized through
                           a circular reqion that represents a statistic about each group.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "PiePlot.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/PiePlot.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Pie Plots Usage"))),
                shiny::markdown(
                  "
                * Pie plots requires a numeric Y-Variable and a categorical Group Variable
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),


              # Donut Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "donutplots"),
                    shiny::h3("Donut Charts"),
                    shiny::p("Donut Charts types are used when you are looking to understand
                           the frequency and spread of your data. Data is visualized through
                           a circular reqion that represents a statistic about each group.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "DonutPlot.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/PiePlot.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Donut Plots Usage"))),
                shiny::markdown(
                  "
                * Donut plots requires a numeric Y-Variable and a categorical Group Variable
                "
                )
              ),

              # Rosetype Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "rosetypeplots"),
                    shiny::h3("Rosetype Charts"),
                    shiny::p("Rosetype Charts types are used when you are looking to understand
                           the frequency and spread of your data. Data is visualized through
                           a circular reqion that represents a statistic about each group.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "RosetypePlot.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/PiePlot.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Rosetype Plots Usage"))),
                shiny::markdown(
                  "
                * Rosetype plots requires a numeric Y-Variable and a categorical Group Variable
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Probability ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "probabilityplots"),
                    shiny::h3("Probability Plots"),
                    shiny::p("Probability plots are visualizations that can help you determine if your data is normally distributed.
                             You can test out different transformations to see which one gets you closer to normality.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "ProbabilityPlot.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/Density.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Probability Plots Usage"))),
                shiny::markdown(
                  "
                * Probability plots only require a numeric Y-Variable to be defined
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Word Cloud Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "wordcloudplots"),
                    shiny::h3("Word Cloud Charts"),
                    shiny::p("Word Cloud types are used when you are looking to understand
                           the frequency of words utilized in a text column.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "WordCloud.PNG",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Rosetype Plots Usage"))),
                shiny::markdown(
                  "
                * Word Cloud plots requires a numeric Y-Variable that is text
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Area Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "areaplots"),
                    shiny::h3("Area Plots"),
                    shiny::p("Area Plot types are used when you are looking to understand
                           time series data. The visual shades the region below the respective
                           line... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "AreaPlot.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/AreaPlot.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Area Plots Usage"))),
                shiny::markdown(
                  "
                * Area plots requires a numeric Y-Variable and a date X-Variable
                * Group variables are optional and look good with or without faceting
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),


              # Line Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "lineplots"),
                    shiny::h3("Line Plots"),
                    shiny::p("Line Plot types are used when you are looking to understand
                           time series data. The visual does not shade the region below
                           the respective line.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "LinePlot.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/LinePlot.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Line Plots Usage"))),
                shiny::markdown(
                  "
                * Line plots requires a numeric Y-Variable and a date X-Variable
                * Group variables are optional and look good with or without faceting
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # River Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "riverplots"),
                    shiny::h3("River Plots"),
                    shiny::p("River Plot types are used when you are looking to understand
                           time series data for multiple series. The visual shades the regions
                           in between the lines and are centered in the graph.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "RiverPlot.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/RiverPlot.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("River Plots Usage"))),
                shiny::markdown(
                  "
                * River plots requires AT LEAST one numeric Y-Variable and a date X-Variable
                * Group variables are NOT optional
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Step Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "stepplots"),
                    shiny::h3("Step Plots"),
                    shiny::p("Step Plot types are used when you are looking to understand
                           time series data. The visual does not shade the region below
                           the respective line but the X-Variable area between points
                           are flat.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "StepPlot.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/StepPlot.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Step Plots Usage"))),
                shiny::markdown(
                  "
                * Step plots requires a numeric Y-Variable and a date X-Variable
                * Group variables are optional and look good with or without faceting
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # AutoCorr Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "autocorrplots"),
                    shiny::h3("Autocorrelation Plots"),
                    shiny::p("Autocorrelation Plot types are used when you are looking to understand
                           time series data. The output can guide you for determining how big of a window
                           to utilize for moving averages in a time series model.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "AutocorrelationPlot.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/StepPlot.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Step Plots Usage"))),
                shiny::markdown(
                  "
                * Autocorrelation plots requires a numeric Y-Variable and a date X-Variable
                * Define the time aggregation for your data
                * Define the max lags to test
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # PartialAutoCorr Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "partialautocorrplots"),
                    shiny::h3("Partial AutoCorrelation Plots"),
                    shiny::p("Partial AutoCorrelation Plot types are used when you are looking to understand
                           time series data. The output can guide you for determining how many lags to utilize
                           in a time series model.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "PartialAutocorrelationPlot.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/StepPlot.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Step Plots Usage"))),
                shiny::markdown(
                  "
                * Partial Autocorrelation plots requires a numeric Y-Variable and a date X-Variable
                * Define the time aggregation for your data
                * Define the max lags to test
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Bar Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "barplots"),
                    shiny::h3("Bar Plots"),
                    shiny::p("Bar Plot types are used when you are looking to understand
                           aggregated data. The visual representation are bars that
                           indicate the statical metric used and height (or width)
                           represents the magnitude of the value.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "BarPlot.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/BarPlot.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Bar Plots Usage"))),
                shiny::markdown(
                  "
                * Bar plots require a numeric Y-Variable and an X-Variable of categorical or date type
                * Group variables are optional but look better with faceting
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Stacked Bar Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "stackedbarplots"),
                    shiny::h3("Stacked Bar Plots"),
                    shiny::p("Stacked Bar Plot types are used when you are looking to understand
                           aggregated data. The visual representation are bars that
                           indicate the statical metric used and height (or width)
                           represents the magnitude of the value but in this case
                           there are multiple groups of values in that height (or width).")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "StackedBarPlot.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/StackedBarPlot.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Stacked Bar Plots Usage"))),
                shiny::markdown(
                  "
                * Stacked Bar plots require a numeric Y-Variable an X-Variable of categorical or date type
                * The X-Variable should be less granular than the Group Variable. Another way of saying that, the Group Variable should be nested within the X-Variable.
                * If the groupings look off for some reason, try switching the X-Variable and the Group Variable for each other
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Bar Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "radarplots"),
                    shiny::h3("Radar Plots"),
                    shiny::p("Radar Plot types are used when you are looking to understand
                           aggregated data. The visual representation is circular with a line connected end points.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "RadarPlot.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/BarPlot.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Radar Plots Usage"))),
                shiny::markdown(
                  "
                * Radar plots require a numeric Y-Variable and a Group Variable
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # 3D Bar Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "3dbarplots"),
                    shiny::h3("3D Bar Plots"),
                    shiny::p("3D Bar Plot types are used when you are looking to understand
                           aggregated data on an addition dimension. The visual representation
                           are bars that indicate the statical metric used and height (or width)
                           represents the magnitude of the value but in this case there are
                           two dimensions used in the graph.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "BarPlot3D.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/BarPlot3D.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("3D Bar Plots Usage"))),
                shiny::markdown(
                  "
                * 3D Bar plots require a non-numeric X-Variable and Y-Variable and a numeric Z-Variable
                * No Group Variables
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Scatter Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "scatterplots"),
                    shiny::h3("Scatter Plots"),
                    shiny::p("Scatter Plot types are used when you are looking to understand
                           the relationship between two numeric variables. The visual representation
                           are dots that indicate the paired coordinate in the graph.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "ScatterPlot.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/ScatterPlot.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Scatter Plots Usage"))),
                shiny::markdown(
                  "
                * Scatter plots require a numeric X-Variable and Y-Variable
                * Group Variables will likely look better with faceting
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # 3D Scatter Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "3dscatterplots"),
                    shiny::h3("3D Scatter Plots"),
                    shiny::p("3D Scatter Plot types are used when you are looking to understand
                           the relationship between three numeric variables. The visual representation
                           are dots that indicate the triple coordinate in the graph.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "ScatterPlot3D.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/ScatterPlot3D.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("3D Scatter Plots Usage"))),
                shiny::markdown(
                  "
                * 3D Scatter plots require a numeric X-Variable, Y-Variable, and Z-Variable
                * Group Variables are optional but faceting is not available
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Copula Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "copulaplots"),
                    shiny::h3("Copula Plots"),
                    shiny::p("Copula Plot types are used when you are looking to understand
                           the relationship between two numeric variables. The visual representation
                           are dots that indicate the paired coordinate in the graph. The distinction
                           between copula and scatter plots is that copula plots first transform
                           both variables to their respective percent rank so that each are
                           uniformly distributed over [0,1]. While pearson correlation would
                           correspond to a scatter plot, the spearman correlation corresponds
                           to copula plots.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "CopulaPlot.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/CopulaPlot.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Copula Plots Usage"))),
                shiny::markdown(
                  "
                * Copula plots require a numeric X-Variable and Y-Variable
                * Group Variables will likely look better with faceting
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # 3D Copula Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "3dcopulaplots"),
                    shiny::h3("3D Copula Plots"),
                    shiny::p("Copula Plot types are used when you are looking to understand
                           the relationship between two numeric variables. The visual representation
                           are dots that indicate the triple coordinate in the graph. The distinction
                           between 3D copula and 3D scatter plots is that copula plots first transform
                           all three variables to their respective percent rank so that each are
                           uniformly distributed over [0,1]. While pearson correlation would
                           correspond to a scatter plot, the spearman correlation corresponds
                           to copula plots.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "CopulaPlot3D.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/CopulaPlot3D.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("3D Copula Plots Usage"))),
                shiny::markdown(
                  "
                * 3D Copula plots require a numeric X-Variable, Y-Variable, and Z-Variable
                * Group Variables are optional but faceting is not available
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Heatmap Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "heatmapplots"),
                    shiny::h3("Heatmap Plots"),
                    shiny::p("Heatmap Plot types are used when you are looking to understand
                           the relationship between two categorical variables with respect
                           to a numeric variable. The visual representation are colored cells
                           that indicate the magnitude of the statistic used to relate both
                           categorical variables.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "Heatmap.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/Heatmap.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Heatmap Plots Usage"))),
                shiny::markdown(
                  "
                * Heatmap plots require a non-numeric X-Variable and Y-Variable, and a numeric Z-Variable
                * Group Variables are not optional
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Correlogram Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "correlogramplots"),
                    shiny::h3("Correlogram Plots"),
                    shiny::p("Correlogram Plot types are used when you are looking to understand
                           the relationships between many numeric variables. For all variables
                           provided, pairwise correlations are computed and displayed like a
                           heatmap.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "CorrelogramPlot.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/CorrelogramPlot.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Correlogram Plots Usage"))),
                shiny::markdown(
                  "
                * Correlogram plots require AT LEAST one numeric Y-Variable
                * Group Variables are not optional
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ----

              # Model Evaluation Plots ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "modelevaluationplots"),
                    shiny::h1(tags$b("Model Evalulation")),
                    shiny::p("Model Evaluation Plot types are those that we encounter in machine
                           learning contexts.")
                  )
                ),
                DataMuse::BlankRow(12L),
                shiny::h3(tags$b("Standard Plots Types")),
                shiny::markdown(
                  "
                  * Residual Histogram
                  * Residual Scatter
                  * Calibration Line
                  * Calibration Box
                  * Partial Dependence Line
                  * Partial Dependence Box
                  * Variable Importance
                  * Shapley Importance
                  * ROC
                  * Confusion Matrix
                  * Lift
                  * Gains
                  "
                )
              ),

              DataMuse::BlankRow(12),
              shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Model Evaluation Plots Usage"))),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Residual Histogram ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "residualhistogram"),
                    shiny::h3("Residual Histogram"),
                    shiny::p("Residual Histograms are used when you are looking to evaluate the
                           the residuals of a regression model.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "zz_ResidualHistogram.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/zz_ResidualHistogram.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Residual Histogram Plots Usage"))),
                shiny::markdown(
                  "
                * Residual Histogram plots requires the Y-Variable to be your target variable and the Z-Variable to be your predicted values variable
                * Group Variables are optional and tend look better with faceting
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Residual Scatter Plot ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "residualscatterplot"),
                    shiny::h3("Residual Scatter"),
                    shiny::p("Residual Scatter plots are used when you are looking to evaluate the
                           the residuals of a regression model over the range of your predicted
                           values that are expressed as deciles on the X-Variable.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "zz_ResidualScatter.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/zz_ResidualScatter.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Residual Scatter Plots Usage"))),
                shiny::markdown(
                  "
                * Residual Scatter plots requires the Y-Variable to be your target variable and the Z-Variable to be your predicted values variable
                * Group Variables are optional and tend look better with faceting
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Calibration Line Plot ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "calibrationline"),
                    shiny::h3("Calibration Line"),
                    shiny::p("Calibration Line plots are used when you are looking to evaluate the
                           the residuals of a regression, classification, or multiclass model,
                           aggregated over the range of your predicted values that are expressed as deciles
                           on the X-Variable.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "zz_CalibrationLine.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/zz_CalibrationLine.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Calibration Line Plots Usage"))),
                shiny::markdown(
                  "
                * Calibration Line plots requires the Y-Variable to be your target variable and the Z-Variable to be your predicted values variable
                * Group Variables are optional and tend look better with faceting
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Calibration Box Plot ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "calibrationbox"),
                    shiny::h3("Calibration Box"),
                    shiny::p("Calibration Box plots are used when you are looking to evaluate the
                           the residuals of a regression, classification, or multiclass model,
                           aggregated via box plots over the range of your predicted values that are
                           expressed as deciles on the X-Variable.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "zz_CalibrationBox.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/zz_CalibrationBox.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Calibration Box Plots Usage"))),
                shiny::markdown(
                  "
                * Calibration Box plots requires the Y-Variable to be your target variable and the Z-Variable to be your predicted values variable
                * Group Variables are NOT optional
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Partial Dependence Line Plot ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "partialdependenceline"),
                    shiny::h3("Partial Dependence Line"),
                    shiny::p("Partial Dependence Line plots are used when you are looking to evaluate the
                           the residuals of a regression, classification, or multiclass model,
                           aggregated over the range of an independence variable that are expressed as
                           deciles on the X-Variable.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "zz_PartialDependenceLine.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/zz_PartialDependenceLine.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Partial Dependence Line Plots Usage"))),
                shiny::markdown(
                  "
                * Partial Dependence Line plots requires the Y-Variable to be your target variable and the Z-Variable to be your predicted values variable
                * The X-Variable is for an independent variable
                * Group Variables are optional and look good with faceting. When Group Variables are used the Y-Variable becomes Target - Predicted
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Partial Dependence Box Plot ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "partialdependenceheatmap"),
                    shiny::h3("Partial Dependence Box"),
                    shiny::p("Partial Dependence Box plots are used when you are looking to evaluate the
                           the residuals of a regression, classification, or multiclass model,
                           aggregated via box plots over the range of an independence variable that
                           are expressed as deciles on the X-Variable.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "zz_PartialDependenceHeatmap.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/zz_PartialDependenceHeatmap.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Partial Dependence Box Plots Usage"))),
                shiny::markdown(
                  "
                * Partial Dependence Heatmap plots requires the Y-Variable to be your target variable and the Z-Variable to be your predicted values variable
                * The X-Variable is for ONE OR TWO independent variables. If two are selected, a heatmap is built, if only one the a bar plot is built
                * Group Variables are optional and look good with faceting. When Group Variables are used the Y-Variable becomes Target - Predicted
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Variable Importance Plot ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "variableimportance"),
                    shiny::h3("Variable Importance"),
                    shiny::p("Variable Importance plots visualize the importance stats
                           of you model via a Bar Plot.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "zz_VariableImportance.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/zz_VariableImportance.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Variable Importance Plots Usage"))),
                shiny::markdown(
                  "
                * Variable Importance plots requires the Y-Variable to be your Variable column and the X-Variable is for the Importance measure
                * Group Variables are NOT optional
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Shapley Importance Plot ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "shapleyimportance"),
                    shiny::h3("Shapley Importance"),
                    shiny::p("Shapley Importance plots visualize the importance stats
                           of you model via a Bar Plot. The distinction between this
                           and regular Variable Importance is that Shapley values
                           are used and you can choose various aggregation statistics
                           and subset your data to see importances across, perhaps, more
                           relevant parts of your data.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "zz_ShapleyImportance.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/zz_ShapleyImportance.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Shapley Importance Plots Usage"))),
                shiny::markdown(
                  "
                * Shapley Importance plots requires a model built from the app
                * Group Variables are optional but the Agg Method is mandatory.
                * You can choose to aggregate the shapley value by:
                  * 'mean'
                  * 'meanabs' (absolute values are taken first, then the measure)
                  * 'median'
                  * 'medianabs' (absolute values are taken first, then the measure)
                  * 'sum'
                  * 'sumabs' (absolute values are taken first, then the measure)
                  * 'sd' (standard deviation)
                  * 'sdabs' (absolute values are taken first, then the measure)
                  * 'skewness'
                  * 'skewnessabs' (absolute values are taken first, then the measure)
                  * 'kurtosis'
                  * 'kurtosisabs' (absolute values are taken first, then the measure)
                  * 'CoeffVar' (coefficient of variation)
                  * 'CoeffVarabs' (absolute values are taken first, then the measure)
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ROC Plot ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "roc"),
                    shiny::h3("ROC Plots"),
                    shiny::p("ROC plots are used to evaluate classification and multiclass models where
                           the model performance is measured over the range of possible cutoff values
                           used to classify.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "zz_ROC.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/zz_ROC.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("ROC Plots Usage"))),
                shiny::markdown(
                  "
                * ROC plots requires the Y-Variable to be your target variable and the Z-Variable to be your predicted values variable
                * Group Variables are optional and look good with faceting
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Confusion Matrix Plot ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "confusionmatrix"),
                    shiny::h3("Confusion Matrix Plots"),
                    shiny::p("Confusion Matrix plots are used to evaluate classification and multiclass models
                           via heatmap of possible combinations of model outputs.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "zz_ConfusionMatrix.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/zz_ConfusionMatrix.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Confusion Matrix Plots Usage"))),
                shiny::markdown(
                  "
                * Confusion Matrix plots requires the Y-Variable to be your target variable and the Z-Variable to be your predicted values variable
                * Group Variables are NOT optional and look good with faceting
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Lift Plot ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "lift"),
                    shiny::h3("Lift Plots"),
                    shiny::p("Lift plots are used to evaluate classification and multiclass models
                           via line plots as the cutoff for predictions increases. The
                           values displayed are incremental lift for each successive
                           decile.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "zz_Lift.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/zz_Lift.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Lift Plots Usage"))),
                shiny::markdown(
                  "
                * Lift plots requires the Y-Variable to be your target variable and the Z-Variable to be your predicted values variable
                * Group Variables are optional and look good with faceting
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Gains Plot ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "gains"),
                    shiny::h3("Gains Plots"),
                    shiny::p("Gains plots are used to evaluate classification and multiclass models
                           via line plots as the cutoff for predictions increases. The
                           values displayed are cumulative lift over each successive
                           decile.")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::tags$img(
                  src = "zz_Gains.PNG", # "https://github.com/AdrianAntico/AutoPlots/blob/main/Images/zz_Gains.PNG?raw=true",
                  alt = "DataMuse",
                  `data-view-component` = "true",
                  height="100%",
                  width="100%"),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Gains Plots Usage"))),
                shiny::markdown(
                  "
                * Gains plots requires the Y-Variable to be your target variable and the Z-Variable to be your predicted values variable
                * Group Variables are optional and look good with faceting
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ----

              # ----

              ################################################# ----
              # Data Wrangling                                  ----
              ################################################# ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "datawrangling"),
                    shiny::h2(tags$b("Data Wrangling")),
                    shiny::p("Data wrangling is a vitally important aspect of this software. It's important that you know how to
                        utilize the functionality as intended. Below are all of the plot types with descriptions about
                        how to use each and every one for each of their intended uses. While there are far too many
                        combinations of each type I will be focusing on the core aspects and let the user decide which
                        combination makes the most sense for their use case. The given info will be sufficient for users
                        to generate any one of those combinations. ")
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Shrink Button ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "shrink"),
                    shiny::h1(tags$b("Shrink")),
                    shiny::p("The Shrink button consists of tasks related to shrinking data.
                           These include aggregating data, subsetting rows, subsetting
                           columns, and sampling data... ")
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Aggregate Data ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "aggregate"),
                    shiny::h3("Aggregate Data"),
                    shiny::p("ROC plots are used to evaluate classification and multiclass models where
                           the model performance is measured over the range of possible cutoff values
                           used to classify... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Aggregate Data Usage"))),
                shiny::markdown(
                  "
                Aggregate Data operation contains five inputs:
                * Data Selection: choose the data set you want to operate on
                * New Name: if you want to apply results to the existing dataset, leave the input blank. If you fill out a name a new dataset will be created with the new name and the operation conducted on it
                * Aggregate Columns: these columns are numeric and will be aggregated via a statistics method, listed below
                * Aggregate Date Column: this is for when you also want to aggregate up your date column, for example, going from daily to weekly. If you don't want to change your time scale you should still include date in the Aggregate Columns list. If you accidentally put your date column in both Aggregate Columns and Aggregate Date Column, the operation will still work as intended.
                * Aggregate By-Columns: these columns are categorical
                * Aggregation Statistic: method for aggregating the numeric columns, which include:
                  * count
                  * mean
                  * standard deviation
                  * max
                  * min
                  * first value
                  * last value
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Subset Rows ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "subsetrows"),
                    shiny::h3("Subset Rows"),
                    shiny::p("The subset rows operation filters data per the users specifications... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Subset Rows Usage"))),
                shiny::markdown(
                  "
                Subset Rows operation allows you to filter by up to four variables by various logic conditions
                * Logic Condidtions: <, >, >=, <=, %in%, %between%, not %Between%, and %like%
                * Value 1: for all logic conditions
                * Value 2: only for logic conditions %between%, and not %Between%
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Subset Cols ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "subsetcols"),
                    shiny::h3("Subset Cols"),
                    shiny::p("The subset cols operation removes columns based on user specifications... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Subset Cols Usage"))),
                shiny::markdown(
                  "
                Subset Cols operation allows you to remove columns from your dataset
                * Select Data: choose your data set
                * New Data Name: choose a name if you want a new dataset created. Otherwise leave blank
                * Select Columns: Columns to remove from your data
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Sample Data ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "sampledata"),
                    shiny::h3("Sample Data"),
                    shiny::p("The Sample Data operation enables the user to create a sampled dataset,
                           by either random or stratified sampling... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Sample Data Usage"))),
                shiny::markdown(
                  "
                Sample Data operation allows you to remove columns from your dataset
                * Select Data: choose your data set
                * Stratify Columns: optional, can use to stratify sample
                * New Data Name: supply a name to create a new dataset. Otherwise, you existing data will be returned with sampled rows
                * Rate: the % of data to keep after sampling
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ----

              # Grow Data Button ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "grow"),
                    shiny::h1(tags$b("Grow")),
                    shiny::p("The Grow button consists of tasks related to growing data.
                           These include joining data and unioning data... ")
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Join Data ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "join"),
                    shiny::h1(tags$b("Join")),
                    shiny::p("The join operation consists of many joing types,
                           including:"),
                    shiny::markdown(
                      "
                    * inner
                    * left
                    * right
                    * full
                    * anti: includes anything in the left table that is not in the right table
                    * semi: left join except no columns from the right table are returned
                    * cross: Cross join is different from the rest in that it takes any number of vectors and creates a cartesian join of all of them to create a data.table
                    * RollForward: used to match up two tables with a time stamp where the records are off due to differences in timing
                    * RollBackward: just like the RollForward except is rolls data backwards instead of fowards
                    ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Join Data Usage"))),
                shiny::markdown(
                  "
                Join Data operation allows you to horizontally combine two data sets with common keys
                * Left Table: choose your first data set, typically the data set you want additional data added on to
                * Right Table: choose your second data set, typically the data set you want take columns from and add to the left table
                * Join Type: select the appropriate join type for your needs. See join types above for guidance
                * New Data Name: Choose a name for a new data set to be built, otherwise the left table will be updated
                * Left Table By-Vars: Include all by-vars (keys) for joining
                * Right Table By-Vars: Include all by-vars (keys) for joining
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Union Data ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "union"),
                    shiny::h1(tags$b("Union Data")),
                    shiny::p("The Union data operation consists of stacking multiple tables
                           on top of each other... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Union Data Usage"))),
                shiny::markdown(
                  "
                Union Data operation allows you to vertically stack two data sets
                * Choose data set: This is the dataset you want to utilize for the operation
                * Append table: this table gets stacked onto the Base data set
                * New Data Name: Choose a name for a new data set to be built, otherwise the left table will be updated
                * Fill:
                * Right Table By-Vars: Include all by-vars (keys) for joining
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ----

              # Dataset Button ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "dataset"),
                    shiny::h1(tags$b("Dataset")),
                    shiny::p("The Dataset button includes operations that work on
                           the entire dataset at once, such as data partitioning,
                           sorting data, removing data, and model data prep (for
                           ensuring your data is ready for ML)... ")
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Partition Data ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "partitiondata"),
                    shiny::h1(tags$b("Partition Data")),
                    shiny::p("The partition data operation is for generating
                           datasets for machine learning, such as a train,
                           validation, and test data set. The user can choose
                           between random data spitting or time based splitting,
                           and they can add a stratification variable as well... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Partition Data Usage"))),
                shiny::markdown(
                  "
                Partition Data operation allows you to split up your primary dataset into many datasets. Your supplied dataset will remain untouched but new datasets will be created
                * Choose data set: This is the dataset you want to utilize for the operation
                * Partition Type: choose 'random' for a purely random split amongst the data. Choose 'time' to have a time based split where training will consist of the earliest in time records, followed by the validation data and then test data
                * Train %: this is the percentage of data you want allocated to the train data set. Defaults to 70%
                * Validation %: this is the percentage of data you want allocated to the train data set. Defaults to 20%
                * Test %: this is the percentage of data you want allocated to the train data set. Defaults to 10%
                * Stratify variable: you can supply a stratification variable to ensure an even split amongst the levels in the variable supplied
                * Date Variable: required for a 'time' based partition, unused otherwise
                * Number of datasets: 3 is the standard but you can go with 2. Just ensure that the %'s add up to 100%
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Sort Data ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "sort"),
                    shiny::h1(tags$b("Sort Data")),
                    shiny::p("The Sort data operation is for sorting data based
                           a set of selected columns and selected orders... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Sort Data Usage"))),
                shiny::markdown(
                  "
                Sort Data operation allows you to split up your primary dataset into many datasets. Your supplied dataset will remain untouched but new datasets will be created
                * Choose data set: This is the dataset you want to utilize for the operation
                * Sort Columns: choose the columns to drive the sort operation
                * Sort Order: Ascending or Decending
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Remove Data ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "remove"),
                    shiny::h1(tags$b("Remove Data")),
                    shiny::p("The Remove data operation is for removing data
                           from memory to free up space... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Remove Data Usage"))),
                shiny::markdown(
                  "
                Remove Data operation allows you to remove datasets from the app session
                * Choose data set: This is the dataset you want to utilize for the operation
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Model Data Prep ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "modeldataprep"),
                    shiny::h1(tags$b("Model Data Prep")),
                    shiny::p("The Model Data Prep operation is for
                           preparing your data for machine learning.
                           Some ML algorithms requires numeric variables
                           to be converted to integers, for example... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Model Data Prep Usage"))),
                shiny::markdown(
                  "
                Model Data Prep applies type conversions across an entire dataset that are suitable for modeling purposes
                * Choose data set: This is the dataset you want to utilize for the operation
                * Skip Columns: Choose columns that you want to be ignored throughout this operation
                * Char to Factor: convert all character columns to factor columns
                * Factor to Char: convert all factor columns to character columns
                * Date to Char: convert date columns to character columns
                * IDate to Date: convert all IDate types columns to Date columns
                * Remove Date: remove Date columns from data
                * Integer to Numeric: convert all integer columns to numeric columns
                * Logical to Binary: convert all logical columns to binary columns
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ----

              # Pivot Button ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "pivot"),
                    shiny::h1(tags$b("Pivot")),
                    shiny::p("The Pivot button includes melting and casting
                           data, otherwise known as going from wide to long,
                           or long to wide... ")
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Melt data ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "melt"),
                    shiny::h1(tags$b("Melt Data")),
                    shiny::p("The Melt Data operation is for turning your
                           data from wide to long... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Melt Data Usage"))),
                shiny::markdown(
                  "
                Melt Data operation allows you to convert a wide data set to a long data set
                * Choose data set: This is the dataset you want to utilize for the operation
                * New Data Name: Choose a name for a new data set to be built, otherwise the left table will be updated
                * ID Columns: This variables remain in the dataset but are not transformed
                * Measure columns: These columns will be turned into a single column (melted)
                * RemoveNA: should NA values result after melting the data, select TRUE to have them removed
                * Variable Column Name: This will be the name of the new column that references which rows the values correspond to
                * Value Column Name: This will be the name of the column that contains the melted values
                * Variable to Factor: The variable column is of character type and you can set this to TRUE to convert it to a factor column
                * Value to Factor: If the melted value column is of character type you can set this to TRUE to convert it to a factor column
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Cast data ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "cast"),
                    shiny::h1(tags$b("Cast Data")),
                    shiny::p("The Cast Data operation is for turning your
                           data from long to wide... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Cast Data Usage"))),
                shiny::markdown(
                  "
                Cast Data operation allows you to convert a wide data set to a long data set
                * Choose data set: This is the dataset you want to utilize for the operation
                * New Data Name: Choose a name for a new data set to be built, otherwise the left table will be updated
                * Agg Stat: when casting the measure columns go through an aggregation step. Select a metric from the list:
                  * length
                  * sum
                  * median
                  * sd (standard deviation)
                  * max
                  * min
                  * data.table::first
                  * data.table::last
                * Impute Value: for missing values supply a value to fill in the blanks
                * LHS Columns: Left hand side columns are columns that are not transformed but remain in the data set
                * RHS Columns: Right hand side columns are columns that will be casted into column names
                * Values Variable: This column will be casted into the RHS columns as values for those columns
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ----

              # Columns Button ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "columns"),
                    shiny::h1(tags$b("Columns")),
                    shiny::p("The Columns button includes type casting,
                           renaming of columns, adding a time trend column,
                           and concatenating columns... ")
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Type Cast data ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "typecast"),
                    shiny::h1(tags$b("Type Cast Data")),
                    shiny::p("The Type Cast operation is for changing
                           the variable types of your columns... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Type Cast Data Usage"))),
                shiny::markdown(
                  "
                Type Cast Data operation to change the variable types of your columns
                * Choose data set: This is the dataset you want to utilize for the operation
                * Convert Numeric: Select columns you want converted to numeric type
                * Convert Date: Select columns to convert to date type
                * Convert Logical: Select columns to convert to logical
                * Convert Integer: Select columns to convert to integer type
                * Convert Factor: Select columns to convert to factor type
                * Excel Date to R Date: Select columns to convert to R Date types
                * Convert Posix: Select columns to convert to Posix type (date time)
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Renaming Columns ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "rename"),
                    shiny::h1(tags$b("Renaming Columns")),
                    shiny::p("The Rename Columns operation is for changing
                           the names of columns in your data... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Renaming Columns Usage"))),
                shiny::markdown(
                  "
                Rename Columns operation to change the variable names in your data
                * Choose data set: This is the dataset you want to utilize for the operation
                * New Data Name: Choose a name for a new data set to be built, otherwise the left table will be updated
                * Select Column: Choose a column whose name you'd like to change
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Time Trend Columns ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "timetrend"),
                    shiny::h1(tags$b("Time Trend")),
                    shiny::p("The Time Trend operation is for adding a
                           column to your data. This can be done by
                           groups as well... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Time Trend Usage"))),
                shiny::markdown(
                  "
                Time Trend operation creates a time trend column in your dataset
                * Choose data set: This is the dataset you want to utilize for the operation
                * New Data Name: Choose a name for a new data set to be built, otherwise the left table will be updated
                * Trend Order: Ascending or Decending
                * Date Column: This is the date column used to create the time trend variable
                * Group Variables: If you have group variables in your data you'll want to inclue those so that you end up with time trends for each group level
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Concatenating Columns ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "concat"),
                    shiny::h1(tags$b("Concat Columns")),
                    shiny::p("The Concat Columns operation is for combining
                           multiple columns into a single column... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Concat Columns Usage"))),
                shiny::markdown(
                  "
                Concat Columns operation creates one new column this is the combination of selected columns
                * Choose data set: This is the dataset you want to utilize for the operation
                * Concat Columns: select columns that you want combined
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),


              # ----

              # Misc Button ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "misc"),
                    shiny::h1(tags$b("Misc")),
                    shiny::p("The Misc button includes meta programming,
                           time series filling and time series rolling... ")
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # MetaProgramming ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "metaprogramming"),
                    shiny::h1(tags$b("Meta Programming")),
                    shiny::p("The meta programming operation is designed to allow
                           one to supply their own code to do things data wrangling
                           related... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Meta Programming Usage"))),
                shiny::markdown(
                  "
                Meta Programming allows you to write your own code to be evaluated
                * Concat Columns: select columns that you want combined
                * In the meta programming code panel, to refer to a data set you'll need to access the DataList
                  * DataList[[<Name of Data>]][['data']] is the data set
                  * You can pull out the data if you want: data <- DataList[[<Name of Data>]][['data']]
                  * If you pull out the data and want the changes to update, you need to put it back when you're done (or create a new data object)
                  * DataList[[<Name of data>]][['data']] <- data
                  * You can write as much code as you'd like in between, just make sure that DataList contains the updates when you're done
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Time Series Fill ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "timeseriesfill"),
                    shiny::h1(tags$b("Time Series Fill")),
                    shiny::p("The Time Series Fill operation is for preparing
                           time series data that has a few gaps in the data... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Time Series Fill Usage"))),
                shiny::markdown(
                  "
                Time Series Fill operation will ensure that your time series data is not missing any dates
                * Choose data set: This is the dataset you want to utilize for the operation
                * New Data Name: select a new name to have a new dataset create, otherwise leave blank
                * Time Unit: Select a time unit that describes your data's time granularity
                * Max Miss %: Any group levels that are missing more than the max will be dropped from your dataset
                * Date Column: This is the primary date column that determines your times series data
                * Grouping Columns: If you have panel data, supply your group variables here
                * Impute value: for missing dates that get created, what value do you want supplied? Zero is a natural options but there are others to select from
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Time Series Fill Roll ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "timeseriesfillroll"),
                    shiny::h1(tags$b("Time Series Fill Roll")),
                    shiny::p("The Time Series Fill Roll operation is for preparing
                           intermittent demand time series data that has a lot
                           of gaps in the data... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Time Series Fill Roll Usage"))),
                shiny::markdown(
                  "
                Time Series Roll operation is for converting intermittent demand data into regular panel data but spreading the demand spike across time
                * Choose data set: This is the dataset you want to utilize for the operation
                * New Data Name: select a new name to have a new dataset create, otherwise leave blank
                * Time Unit: Select a time unit that describes your data's time granularity
                * Date Column: This is the primary date column that determines your times series data
                * Grouping Columns: If you have panel data, supply your group variables here
                * Impute value: for missing dates that get created, what value do you want supplied? Zero is a natural options but there are others to select from
                * Roll Vars: These are the columns that need there values 'spread' across time
                * Non Roll Vars: These columns are not spread out over time
                * Roll Direction: Do you want your demand spikes spread forward in time or backwards in time?
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ----

              # ----

              ################################################# ----
              # Feature Engineering                             ----
              ################################################# ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "featureengineering"),
                    shiny::h2(tags$b("Feature Engineering")),
                    shiny::p("Feature Engineering is a vitally important aspect of this software. It's important that
                             you know how to utilize the functionality as intended. Below are all of the available methods
                             with descriptions about how to use each and every one for each of their intended uses. I find that
                             feature engineering is fastest way to generate a sufficiently good model, when combined with one of
                             the ML methods in the ML+Inference Tab.")
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Anomaly Detection Button ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "anomalydetection"),
                    shiny::h1(tags$b("Anomaly Detection")),
                    shiny::p("The Anomaly Detection button includes an Isolation Forest
                           algorithm... ")
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Isolation Forest ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "isolationforest"),
                    shiny::h1(tags$b("Isolation Forest")),
                    shiny::p("The Isolation Forest algorithm is provided by H2O. Per
                           their documentation, 'Isolation Forest is similar in
                           principle to Random Forest and is built on the basis of
                           decision trees. Isolation Forest, however, identifies anomalies
                           or outliers rather than profiling normal data points. Isolation
                           Forest isolates observations by randomly selecting a feature and
                           then randomly selecting a split value between the maximum and minimum
                           values of that selected feature. This split depends on how long it
                           takes to separate the points.'"),
                    shiny::p("Random partitioning produces noticeably shorter paths for anomalies.
                           When a forest of random trees collectively produces shorter path
                           lengths for particular samples, they are highly likely to be anomalies.'... ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Isolation Forest Usage"))),
                shiny::markdown(
                  "
                Isolation Forest operation is for running anomaly detection on your data
                * Train Data: This is your training dataset. This one is required
                * Validation Data: Optional. If you have a validation dataset supply that here
                * Test Data: Optional. If you have a test dataset supply that here
                * Features: Variables you want included in your anomaly detection operation
                * Number of Trees: this is for the Random Forest that runs under the hood
                * Anomaly Detection Threshold: this is to classify records as anomalies
                * Max Depth: For the Random Forest
                * Minimum Rows: For the Random Forest
                * Row Sample Rate: For the Random Forest
                * Column Sample Rate: For the Random Forest
                * Column Sample Rate per Level: For the Random Forest
                * Column Sample Rate per Tree: For the Random Forest
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ----

              # Dimensionality Reduction Button ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "dimensionalityreduction"),
                    shiny::h1(tags$b("Dimensionality Reduction")),
                    shiny::p("The Dimensionality Reduction button includes an Autoencoder
                           algorithm... ")
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Dimensionality Reduction ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "dimensionalityreduction"),
                    shiny::h1(tags$b("Dimensionality Reduction")),
                    shiny::p("The Dimensionality Reduction algorithm is provided by H2O. It
                           utilizes their feed forward neural network multilayer perceptron model.
                           You set one up to train without a target variable and extract one of the
                           layers after a sufficient number of ephocs have ran...")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Dimensionality Reduction Usage"))),
                shiny::markdown(
                  "
                Dimensionality Reduction operation is for reducing a series of columns into a smaller set of columns
                * Train Data: This is your training dataset. This one is required
                * Validation Data: Optional. If you have a validation dataset supply that here
                * Test Data: Optional. If you have a test dataset supply that here
                * Anomaly Detection: Set to TRUE to return anomaly detection values
                * Features: Variables you want included in your anomaly detection operation
                * Remove Base Columns: If you want to retain the source Features set this to FALSE
                * Anom Per-Feature: Set this to TRUE to have anomaly detection ran on all features individually as well as on the full set in aggregate
                * Layer Returned: Pick the neural network layer you'd like returned
                * Layer Count: How many layers do you want in your Neural Network?
                * Shrink Base: each layer will reduce the number of nodes (dim reduction). E.g. if you have 100 features and choose a shrink rate of 0.50, then layer 2 will contain 50 features, layer 3 will contain 25 features, and so on.
                * Epochs: number of iterations in training the neural network
                * L2 Regularization: prevents overfitting. The higher the value the more protection (sometimes at a loss of accuracy)
                * Elastic Averaging: Specify whether to enable elastic averaging between computing nodes, which can improve distributed model convergence
                * Elastic Avg Rate: Only applicable if Elastic Averaging is set to TRUE
                * Elastic Avg L2: Overfitting protection
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ----

              # Clustering Button ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "clustering"),
                    shiny::h1(tags$b("Clustering")),
                    shiny::p("The Clustering button inclues a k-means clustering algorithm...")
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Clustering ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "clustering"),
                    shiny::h1(tags$b("Clustering")),
                    shiny::p("The k-means algorithm is provided by H2O...")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Clustering Usage"))),
                shiny::markdown(
                  "
                Clustering operation is for grouping rows together by a common key (clusters)
                * Train Data: This is your training dataset. This one is required
                * Validation Data: Optional. If you have a validation dataset supply that here
                * Test Data: Optional. If you have a test dataset supply that here
                * Features: Variables you want included in your anomaly detection operation
                * Clustering Loss Metric: choose from:
                  * totss: total sum of squares
                  * betweenss: between sum of squares
                  * withinss: within sum of squares
                * Max Number of Clusters: set a ceiling for the potential number of clusters generated
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ----

              # NLP Button ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "nlp"),
                    shiny::h1(tags$b("NLP")),
                    shiny::p("The NLP button includes a word2vec model, and functions for
                           text summary, sentiment, readability, and lexical diversity... ")
                  )
                )
              ),

              # Word2Vec ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "word2vec"),
                    shiny::h1(tags$b("Word2Vec")),
                    shiny::p("The word2vec model is provided by H2O...")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Word2Vec Usage"))),
                shiny::markdown(
                  "
                H2O Word2Vec operation is for converting text columns into numeric vectors suitable for modeling purposes
                * Train Data: This is your training dataset. This one is required
                * Validation Data: Optional. If you have a validation dataset supply that here
                * Test Data: Optional. If you have a test dataset supply that here
                * Text Variables: Text columns you want included in your word2vec operation
                * Build Type:
                  * individual: Generate a separate model for each text column
                  * combined: Combine all text columns and build a single model
                * Keep Text Columns: Select TRUE to retain the source text columns after the word2vec operation
                * Number of vectors: For each text column, how many word2vec columns do you want returned?
                * Min Word Count: For text data to be considered, it must have a minimum word count supplied by the user
                * Window Size: For the word2vec operation, it analyzes surrounding words and that window is defined here by the user
                * Number of Epochs: Number of iterations for training the word2vec model
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Text Summary ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "textsummary"),
                    shiny::h1(tags$b("Text Summary")),
                    shiny::p("The Text Summary function provided a number of new columns
                           to your data set which include counts of: 'document', 'chars',
                           'sents', 'tokens', 'types', 'puncts', 'numbers', 'symbols',
                           'urls', 'tags', 'emojis'")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Text Summary Usage"))),
                shiny::markdown(
                  "
                Text Summary operation is for summarizing text columns for useful information
                * Choose Data Set: This is your source dataset with text columns to analyze
                * Text Columns: Select the text columns you want analyzed
                * Remove Stats: Choose which stats to remove from the text summary output
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Sentiment ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "sentiment"),
                    shiny::h1(tags$b("Sentiment")),
                    shiny::p("The Sentiment function provided a sentiment score or a
                           sentiment set of classifiers. One set is simply 'positive' or
                           'negative' while the other also includes 'neutral'...")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Sentiment Usage"))),
                shiny::markdown(
                  "
                Sentiment operation is for analyzing text for positive, negative, or neutral expression
                * Choose Data Set: This is your source dataset with text columns to analyze
                * Text Columns: Select the text columns you want analyzed
                * Output Type: Choose which stats to remove from the text summary output
                * Remove Stopwords: Select TRUE to have the Sentiment operation ignore stopwords
                * Apply Stemming: Select TRUE to apply stemming
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Readability ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "readability"),
                    shiny::h1(tags$b("Readability")),
                    shiny::p("The Readability function provides a readability score for a variety
                           of readability metrics. You can choose to have a single verion returned or
                           as many as you'd like and they're be returned as new features in your dataset.
                           The definitions can be found in the link below..."),
                    tags$a(target="_blank", shiny::icon("newspaper"), href="https://cran.r-project.org/web/packages/quanteda.textstats/quanteda.textstats.pdf", "Definitions Link"),
                    shiny::p("The Readability measures include: "),
                    shiny::markdown(
                      "
                  * 'Flesch'
                  * 'ARI'
                  * 'Bormuth.MC'
                  * 'Bormuth.GP'
                  * 'Coleman'
                  * 'Coleman.C2'
                  * 'Coleman.Liau.ECP'
                  * 'Coleman.Liau.grade'
                  * 'Coleman.Liau.short'
                  * 'Dale.Chall'
                  * 'Danielson.Bryan'
                  * 'Dickes.Steiwer'
                  * 'DRP'
                  * 'ELF'
                  * 'Farr.Jenkins.Paterson'
                  * 'Flesch.PSK'
                  * 'Flesch.Kincaid'
                  * 'FOG'
                  * 'FOG.PSK'
                  * 'FOG.NRI'
                  * 'FORCAST'
                  * 'Fucks'
                  * 'Linsear.Write'
                  * 'LIW'
                  * 'nWS'
                  * 'nWS.2'
                  * 'nWS.3'
                  * 'nWS4'
                  * 'RIX'
                  * 'Scrabble'
                  * 'SMOG'
                  * 'SMOG.C'
                  * 'SMOG.simple'
                  * 'SMOG.de'
                  * 'Spache'
                  * 'Spache.old'
                  * 'Strain'
                  * 'Traenkle.Bailer'
                  * 'Wheeler.Smith'
                  * 'meanSentenceLength'
                  * 'meanWordSyllables'
                  ")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Readability Usage"))),
                shiny::markdown(
                  "
                Readability operation is for analyzing text for readability, based on the metrics selected
                * Choose Data Set: This is your source dataset with text columns to analyze
                * Text Columns: Select the text columns you want analyzed
                * Readability Measures: Select at least one readability measure. Each readability measure will produce its own column of scores based on the measure
                * Remove Hyphens: Text cleaning task
                * Max Sentence Length: Text cleaning task
                * Include Intermediate Values: Set to TRUE to have the variables that go into the readability measure returned in your data
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Lexical Diversity ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "lexicaldiversity"),
                    shiny::h1(tags$b("Lexical Diversity")),
                    shiny::p("The Lexical Diversity function provides a lexical diversity score for a variety
                           of lexical diversity metrics. You can choose to have a single verion returned or
                           as many as you'd like and they're be returned as new features in your dataset.
                           Definitions for the below can be found here:"),
                    tags$a(target="_blank", shiny::icon("newspaper"), href="https://cran.r-project.org/web/packages/quanteda.textstats/quanteda.textstats.pdf", "Definitions Link"),
                    shiny::p("The Lexical Diversity measures include: "),
                    shiny::markdown(
                      "
                    * 'TTR'
                    * 'C'
                    * 'R'
                    * 'CTTR'
                    * 'U'
                    * 'S'
                    * 'K'
                    * 'I'
                    * 'D'
                    * 'Vm'
                    * 'Maas'
                    * 'MATTR'
                    * 'MSTTR'
                    "
                    )
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Lexical Diversity Usage"))),
                shiny::markdown(
                  "
                Lexical Diversity operation is for analyzing text for complexity, based on the metrics selected
                * Choose Data Set: This is your source dataset with text columns to analyze
                * Text Columns: Select the text columns you want analyzed
                * Lexical Measures: Select at least one lexical diversity measure. Each measure will produce its own column of scores based on the measure
                * Remove Hyphens: Text cleaning task
                * Remove Symbols: Text cleaning task
                * Remove Punctuation: Text cleaning task
                * Remove Numbers: Text cleaning task
                * Log Base: Some of the measures utilize logs in their calculation. Default is natural log but you can select a different base
                * Moving Avg Window for MATTR: Only applies if you select the MATTR measure
                * Segment size for MSTTR: Only applies if you select MSTTR measure
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ----

              # Rolling Button ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "rolling"),
                    shiny::h1(tags$b("Rolling")),
                    shiny::p("The Rolling operations include functions that generate rolling or
                           differencing stats for numeric, date, and categorical variables... ")
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # AutoLagRollStats ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "numericvariables"),
                    shiny::h1(tags$b("Numeric Variables")),
                    shiny::p("The Numeric Variable menu provides operations to generate a variety
                           of lags and rolling statistics. The rolling stats include: 'mean',
                           'sd', skewness', 'kurtosis', and quantiles, which can all be
                           generated by group variables...")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Numeric Variables Usage"))),
                shiny::markdown(
                  "
                Numeric Variables operation is for generating lags and rolling statistics
                * Choose Data Set: This is your source dataset
                * Target Variables: Select at least one target variable which are used to create the lags and rolling stats
                * By-Variables: Select all the group variables that when combined, create unique time series sets
                * Date Variable: This date variable is used to define the time series' and windowing for lags and rolling stats
                * Window Lag: Choose 0 to utilize the current target variable and 1 to utilize the lag-1 version of the target variable. This is important because if you utilize the current Target Variable you can introduce forward leak into your data if you plan on using the lags and rolling stats as features in a model
                * Time Aggregation: This is the time-grain of your data. If you data is on a daily cadence then select 'day' for example
                * Lags: Select lags from the list. You can select any values you'd like. For example, you can select lag 4 without selecting lags 1-3
                * Rolling Mean: rolling average window sizes to select. Select as many as you'd like
                * Rolling StDev: rolling standard deviation window sizes to select. Select as many as you'd like
                * Rolling Skew: rolling skew window sizes to select. Select as many as you'd like
                * Rolling Kurtosis: rolling kurtosis window sizes to select. Select as many as you'd like
                * Rolling Percentile: rolling percentile window sizes to select. Select as many as you'd like
                * Selected Percentiles: select which percentiles you want to run for all the Rolling Percentile window sizes
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Diff Variables ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "diffvariables"),
                    shiny::h1(tags$b("Difference Variables")),
                    shiny::p("The Difference Variable menu provides operations to generate a variety
                           of differencing features. First off, you can generate difference
                           variables for numeric, date, and categorical variables, all by
                           grouping variables if requested. Second, you can choose the
                           start and end periods. For example, you can get the difference
                           between current and a day ago, or 10 days age vs 27 days ago...")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Differencing Variables Usage"))),
                shiny::markdown(
                  "
                Differencing Variables operation is for generating differences (period to period differences) on your data
                * Choose Data Set: This is your source dataset
                * Date Variable: This date variable is used to define the time series' and differencing periods
                * By-Variables: Select all the group variables that when combined, create unique time series sets
                * Numeric Diff Variables: Select at least one diff variable which are used to create the differenced variables
                * Date Diff Variables: Select at least one date difference variable which are used to create the days between periods
                * Group Diff Variables: Select at least one group difference variable which are used to create the difference in group levels over periods
                * Base Period: 0 means current, 1 means 1 period back, 2 means 2 periods back, etc. Periods are based on time grain in your data. For example, with daily data, 1 period means 1 day.
                * Lookback Period: this is the period in the past prior to the Base Period. Same logic as the Base Period for what each value means
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Categorical Variables ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "categoricalvariables"),
                    shiny::h1(tags$b("Categorical Variables")),
                    shiny::p("The Categorical Variable menu provides operations to generate lags
                           and rolling modes, all by grouping variables if requested...")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Categorical Variables Usage"))),
                shiny::markdown(
                  "
                Categorical Variables operation is for generating rolling modes on categorical data
                * Choose Data Set: This is your source dataset
                * Target Variables: Select at least one categorical variable which are used to create the rolling modes
                * By-Variables: Select all the group variables that when combined, create unique time series sets
                * Date Variable: This date variable is used to define the time series' and rolling windows
                * Window Lag: Choose 0 to utilize the current target variable and 1 to utilize the lag-1 version of the target variable. This is important because if you utilize the current Target Variable you can introduce forward leak into your data if you plan on using the lags and rolling stats as features in a model
                * Lags: Select lags from the list. You can select any values you'd like. For example, you can select lag 4 without selecting lags 1-3
                * Window Sizes: rolling mode window sizes to select. Select as many as you'd like
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ----

              # Categorical Button ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "categorical"),
                    shiny::h1(tags$b("Categorical")),
                    shiny::p("The Categorical operations include two functions, one for categorical
                           encoding, and one for producing dummy variables... ")
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Categorical Encoding ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "categoricalencoding"),
                    shiny::h1(tags$b("Categorical Encoding")),
                    shiny::p("The Categorical Encoding menu provides operations to generate
                           encoding variables. These encodings include: ..."),
                    shiny::markdown(
                      "
                    * Nested Mixed Effects: blends data across levels if levels have low sample sizes, with respect to the target variable; example: customer -> sku -> brand
                    * Credibility (James-Stein): blends data across the levels of the variable and overal mean if the group levels have low sample sizes, with respect to the target variable
                    * Target Encoding: average by group level with respect to the target variable
                    * Weight of Evidence (woe)
                    * Poly Encode
                    * Backward Difference
                    * Helmert
                    "
                    )
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Categorical Encoding Usage"))),
                shiny::markdown(
                  "
                Categorical Variables operation is for generating rolling modes on categorical data
                * Train Data: This is your training dataset. This one is required
                * Validation Data: Optional. If you have a validation dataset supply that here
                * Test Data: Optional. If you have a test dataset supply that here
                * Group Variables: Select all the group variables that when combined, create unique time series sets
                * Target Variables: Select at least one categorical variable which are used to create the encodings
                * Encoding Method: Select one method to encode your variables
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Partial Dummies ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "dummyvariables"),
                    shiny::h1(tags$b("Dummy Variables")),
                    shiny::p("The Dummy Variables menu provides operations to generate
                           dummy variables for some or all of your categorical variable
                           levels... "),
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Dummy Variables Usage"))),
                shiny::markdown(
                  "
                Dummy Variables operation is for generating dummy variables from categorical variables
                * Choose Data Set: This is your source dataset
                * Target Columns: Select at least one categorical variable which are used to create the dummy variables
                * TopN: You can have a partial set of dummy variables created instead of having all group levels turned into dummy variables. If TopN = 5 then the five most frequent levels will be converted to dummy variables
                * Keep Base Columns: Once your dummy variables are created you can have the source Target Columns removed from your data by setting this to FALSE
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ----

              # Calendar Button ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "calendar"),
                    shiny::h1(tags$b("Calendar")),
                    shiny::p("The Calendar operations include two functions, one for calendar
                           variables and one for holiday variables... ")
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Calendar Variables ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "calendarvariables"),
                    shiny::h1(tags$b("Calendar Variables")),
                    shiny::p("The Calendar Variables operation generates
                           a columns for all time granularities, including: "),
                    shiny::markdown(
                      "
                    * second
                    * minute
                    * hour
                    * day
                    * week
                    * iso week
                    * week of month
                    * month
                    * quarter
                    * year
                    "
                    )
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Calendar Variables Usage"))),
                shiny::markdown(
                  "
                Calendar Variables operation is for generating variables that represent respective time categories of meaning
                * Choose Data Set: This is your source dataset
                * Date Columns: Select at least one date column which are used to create the calendar variables
                * Time Units: select the time units that you want to have converted to calendar variables
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Holiday Variables ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "holidayvariables"),
                    shiny::h1(tags$b("Holiday Variables")),
                    shiny::p("The Holiday Variables operation generates
                           a columns for many holiday types, including: "),
                    shiny::markdown(
                      "
                    * US Public Holidays
                    * Easter Group Holiday
                    * Christmas Group Holidays
                    * Other Ecclestical Feasts Holidays
                    "
                    )
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Holiday Variables Usage"))),
                shiny::markdown(
                  "
                Holiday Variables operation is for generating variables that represent respective holiday categories
                * Choose Data Set: This is your source dataset
                * Date Columns: Select at least one date column which are used to create the calendar variables
                * Holiday Selection: select the holiday categories of interest
                * Lookback Days: for each current row in your dataset, the lookback days will inspect that time period back in time to evaluate if a holiday fell within that window. This is useful for holidays that have an effect that lasts more than just the day the holiday landed on
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ----

              # Numeric Button ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "numeric"),
                    shiny::h1(tags$b("Numeric")),
                    shiny::p("The Numeric operations include several functions. Percent Rank,
                           Standardize, Transform Variables, and Interaction Variables... ")
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Percent Rank Variables ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "percentrank"),
                    shiny::h1(tags$b("Percent Rank")),
                    shiny::p("The Percent Rank operation converts numeric variables to a
                           uniform distribution via percentile ranking. Each value is
                           transformed to its percentile in light of all other values...")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Percent Rank Usage"))),
                shiny::markdown(
                  "
                Percent Rank Variables operation is for converting a numeric column onto the uniform distribution via converting the numeric values in each row to their respective percentiles
                * Train Data: This is your training dataset. This one is required
                * Validation Data: Optional. If you have a validation dataset supply that here
                * Test Data: Optional. If you have a test dataset supply that here
                * Target Columns: Select at least one date column which are used to create the percent rank variables
                * By-Variables: If you'd like to get percentiles by group variables (unique percentiles for each group level) supply group variables here
                * Precision: How many decimals do you want associated with the resuling percentiles. Less precision means there will be likely be duplicated percentiles in the column
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Standardize Variables ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "standardize"),
                    shiny::h1(tags$b("Standardize")),
                    shiny::p("The Standardize operation can let a user choose between
                           centering, scaling, or both...")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Standardize Usage"))),
                shiny::markdown(
                  "
                Percent Rank Variables operation is for converting a numeric column onto the uniform distribution via converting the numeric values in each row to their respective percentiles
                * Train Data: This is your training dataset. This one is required
                * Validation Data: Optional. If you have a validation dataset supply that here
                * Test Data: Optional. If you have a test dataset supply that here
                * Target Columns: Select at least one date column which are used to create the percent rank variables
                * By-Variables: If you'd like to get percentiles by group variables (unique percentiles for each group level) supply group variables here
                * Center: This is to center your data within the standardization process. Either Centering or Scaling must be chosen or nothing will change. Both can be selected
                * Scale: This is to scale your data withing the standardization process. Either Centering or Scaling must be chosen or nothing will change. Both can be selected
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Transform Variables ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "transform"),
                    shiny::h1(tags$b("Transform")),
                    shiny::p("The Transform operation converts your numeric variable into
                           another one by applying either a:"),
                    shiny::markdown(
                      "
                    * LogPlus1 (natual log + minimum value in set of values)
                    * Log (standard natural log transformation)
                    * Square Root
                    * Asinh
                    * BoxCox
                    * YeoJohnson
                    * Logit
                    * Asin
                    "
                    )
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Transform Usage"))),
                shiny::markdown(
                  "
                Transform Variables operation is for converting a numeric column onto the uniform distribution via converting the numeric values in each row to their respective percentiles
                * Choose Data Set: This is your source dataset
                * Target Columns: Select at least one date column which are used to create the percent rank variables. New transformation columns will be created
                * Transformation Methods: Select at least one transformation method. The operation will check to see which one normalizes your data best and select that method
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Interaction Variables ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "interaction"),
                    shiny::h1(tags$b("Interaction")),
                    shiny::p("The Interaction operation multiplies numeric variables together.
                           You can choose to center and scale them first to prevent numeric
                           overflow...")
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Interaction Usage"))),
                shiny::markdown(
                  "
                Interaction operation is for create interaction variables based on numeric variables
                * Choose Data Set: This is your source dataset
                * Target Columns: Select at least one date column which are used to create the percent rank variables. New transformation columns will be created
                * Interaction Depth: select 2 for pairwise interactions, 3 for three-way interactions, and so on
                * Center Data: Center and Scaling are useful to prevent numeric overflow from multiplication of large numbers
                * Scale Data: Center and Scaling are useful to prevent numeric overflow from multiplication of large numbers
                "
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ----

              # ----

              ################################################# ----
              # ML                                              ----
              ################################################# ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "mlinference"),
                    shiny::h2(tags$b("ML + Inference")),
                    shiny::p("ML is a vitally important aspect of this software. It's important that you know how to
                        utilize the functionality as intended. Below are all of the plot types with descriptions about
                        how to use each and every one for each of their intended uses. While there are far too many
                        combinations of each type I will be focusing on the core aspects and let the user decide which
                        combination makes the most sense for their use case. The given info will be sufficient for users
                        to generate any one of those combinations. "),
                    shiny::p("The ML algorithms here will provide a number of services for you automatically. These include, "),
                    shiny::markdown(
                      "
                      * If requested, Transformation of the target variable and backtransformation after scoring for evaluation purposes
                      * Data partitioning for train, validation, and test data sets if the user only supplies a training data set
                      * Categorical variable encoding and backtransform if the user supplies categorical variables as features
                      * Computation of model metrics for evaluation
                      * Data conversion to the structure appropriate for the given algorithm selected
                      * Multi-arm bandit grid tuning and best model selection if grid tuning is utilized with intermediate metrics and parameters returned
                      "
                    )
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # CatBoost ML ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "catboost"),
                    shiny::h1(tags$b("CatBoost")),
                    shiny::p("The CatBoost ML algorithm is for running regression, classification,
                           and multiclass models."),
                    tags$a(target="_blank", shiny::icon("newspaper"), href="https://catboost.ai/", "CatBoost Background Link"),
                  )
                ),

                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("CatBoost Usage"))),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Data:
                This is where you define your datasets and variables of interest
                * Train Data: This is your training dataset. This one is required
                * Validation Data: Optional. If you have a validation dataset supply that here. If not, one will be created for you and 20% of your data will be allocated to Validation Data unless you specify 'TrainOnFull = TRUE' in the Meta Parameters tab.
                * Test Data: Optional. If you have a validation dataset supply that here. If not, one will be created for you and 10% of your data will be allocated to Test Data unless you specify 'TrainOnFull = TRUE' in the Meta Parameters tab.
                * Select Target: This is your dependent variable that you are looking to predict / model
                * Select Features: These are your independent variables used to help predict your target variable
                * TrainOnFull: Select TRUE to train on the full dataset instead of utilizing training, validation, and test datasets. This is typically done when you've found a final set of parameters and are ready to build a final model for production.
                * Character Encoding: If you have character features in your feature set they will be converted to numeric encodings. You can choose which type in this dropdown
                * Weights Column: if you want your rows weighted for modeling purposes you'll need to have a column with the weights within them to supply here. A common reason to use weighting is for when you have group variables and some of the levels have lower counts than others. The ML models will tend to emphasize the higher record count when building so if you find bias is your outcome predictions, weighting can help to resolve that
                * Transform Variables: This is to transform your target variable. There is no need to transform your features (independent variables) as monotonic transformations on features with tree based methods make no difference in outcome
                * Transform Methods: Choose at least one method. The algorithms will determine which transformation among selected will best normalize your target variable and move forward with that one
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Hardware:
                This is where you define your datasets and variables of interest
                * GPU or CPU: If you're running a machine with GPU's available you can select to run with GPU. Note that outcome performance can vary by utilizing GPU vs CPU. You will typically need more Trees when utilizing the GPU selection
                * Number of GPU's: If you've selected GPU and have access to more than one you can utilize them by increasing this input to the number of GPU's of your choice
                * Create ModelID: This will attach all model artifacts to a name. If you plan on running multiple models for comparison then set a name to each model to distinguish between then and prevent overwriting an already built model
                * CPU Threads: This will default to total available threads minus 2. CPU is still utilized even when you select GPU. If you have other processes running at the same time as modeling you will want to lower the CPU Threads count to give all processes a chance to keep running and prevent a queue pool from forming (which can slow down all processes)
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Model:
                This is where you define the CatBoost parameters
                * Number of Trees: Select the number of trees for training the model
                * Max Tree Depth: Determines the max number of splits each tree can make
                * Learning Rate: Leave blank for CatBoost to determine the value for you. Otherwise supply a value. Used for reducing the gradient step
                * L2 Leaf Regularization: L2 Regularization for tree based methods. Higher the value the less likely for the model to overfit, at the expense of potential accuracy
                * Model Size Regularization: The model size regularization coefficient. The larger the value, the smaller the model size. Another overfitting protection parameter
                * Langevin Boosting: Enables the Stochastic Gradient Langevin Boosting mode
                * Diffusion Temperature: The diffusion temperature of the Stochastic Gradient Langevin Boosting mode. Utilize this with Langevin Boosting, otherwise the parameter is ignored
                * Random Strength: The amount of randomness to use for scoring splits when the tree structure is selected. Use this parameter to avoid overfitting the model
                * Border Count: The number of splits for numerical features
                * Random Subspace: Random subspace method. The percentage of features to use at each split selection, when features are selected over again at random
                * Bootstrap Type: Defines the method for sampling the weights of objects. Used for regularization and training speedup. Link below for details
                * Grow Policy: 'SymmetricTree' is the only option to use that will return variable importance data. If you're not concerned with that, you can also choose from 'LossGuide' or 'DepthWise'
                * Min Data in Leaf: The minimum number of training samples in a leaf. CatBoost does not search for new splits in leaves with samples count less than the specified value
                * Bagging Rate: Sample rate for selected subsets of the rows of the training data for each tree (like a random forest)
                * Feature Border Type: The quantization mode for numerical features. Link below for details
                * Score Function: The score type used to select the next split during the tree construction. Links below for details
                "
                ),
                DataMuse::BlankRow(12L),
                shiny::fluidRow(shiny::column(width = 12L,tags$a(target="_blank", shiny::icon("newspaper"), href="https://catboost.ai/en/docs/concepts/algorithm-main-stages_bootstrap-options", "Bootstrap Type Details"))),
                shiny::fluidRow(shiny::column(width = 12L,tags$a(target="_blank", shiny::icon("newspaper"), href="https://catboost.ai/en/docs/concepts/quantization", "Feature Border Type Details"))),
                shiny::fluidRow(shiny::column(width = 12L,tags$a(target="_blank", shiny::icon("newspaper"), href="https://catboost.ai/en/docs/concepts/algorithm-score-functions", "Score Function Details"))),

                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Grid Tuning:
                This is where you define parameters for a hyperparameter search
                * Grid Tune: Set to TRUE to run grid tuning
                * Baseline Comparison of Models: 'default' will compare all models to the catboost default settings (with the user supplied number of trees). 'best' will compare all models to the current best performer among all models built in the grid tuning process
                * Max Run Time (minutes): Set an upper boundary for run time of grid tuning
                * Runs without a new Winner: Set a value to terminate grid tuning due to lack of new winning models
                * Max Models: Cap the number of models to test in grid tuning mode
                "
                ),

                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Evaluation:
                This is where you define parameters for a hyperparameter search
                * Loss Function: The metric to use in training. The specified value also determines the machine learning problem to solve. Some metrics support optional parameters (see the Objectives and metrics section for details on each metric)
                * Evaluation Metric: The metric used for overfitting detection (if enabled) and best model selection (if enabled). Some metrics support optional parameters (see the Objectives and metrics section for details on each metric)
                * Trees between Evaluations: Number of trees to build between evaluations for overfitting detection
                * Class Weight for 0 Class: For Binary Classification models, you can up- or down-weight each class. The default under the hood is 1 for both
                * Class Weight for 1 Class: For Binary Classification models, you can up- or down-weight each class. The default under the hood is 1 for both
                * Grid Tuning Evaluation Metric:
                "
                ),

                DataMuse::BlankRow(12L),
                shiny::fluidRow(shiny::column(width = 12L,tags$a(target="_blank", shiny::icon("newspaper"), href="https://catboost.ai/en/docs/concepts/loss-functions", "Loss Functions and Eval Metrics Details")))
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # XGBoost ML ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "xgboost"),
                    shiny::h1(tags$b("XGBoost")),
                    shiny::p("The XGBoost ML algorithm is for running regression, classification,
                           and multiclass models."),
                    tags$a(target="_blank", shiny::icon("newspaper"), href="https://xgboost.ai/", "XGBoost Background Link"),
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("XGBoost Usage"))),

                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("CatBoost Usage"))),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Data:
                This is where you define your datasets and variables of interest
                * Train Data: This is your training dataset. This one is required
                * Validation Data: Optional. If you have a validation dataset supply that here. If not, one will be created for you and 20% of your data will be allocated to Validation Data unless you specify 'TrainOnFull = TRUE' in the Meta Parameters tab.
                * Test Data: Optional. If you have a validation dataset supply that here. If not, one will be created for you and 10% of your data will be allocated to Test Data unless you specify 'TrainOnFull = TRUE' in the Meta Parameters tab.
                * Select Target: This is your dependent variable that you are looking to predict / model
                * Select Features: These are your independent variables used to help predict your target variable
                * TrainOnFull: Select TRUE to train on the full dataset instead of utilizing training, validation, and test datasets. This is typically done when you've found a final set of parameters and are ready to build a final model for production.
                * Character Encoding: If you have character features in your feature set they will be converted to numeric encodings. You can choose which type in this dropdown
                * Weights Column: if you want your rows weighted for modeling purposes you'll need to have a column with the weights within them to supply here. A common reason to use weighting is for when you have group variables and some of the levels have lower counts than others. The ML models will tend to emphasize the higher record count when building so if you find bias is your outcome predictions, weighting can help to resolve that
                * Transform Variables: This is to transform your target variable. There is no need to transform your features (independent variables) as monotonic transformations on features with tree based methods make no difference in outcome
                * Transform Methods: Choose at least one method. The algorithms will determine which transformation among selected will best normalize your target variable and move forward with that one
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Hardware:
                This is where you define your datasets and variables of interest
                * GPU or CPU: If you're running a machine with GPU's available you can select to run with GPU. Note that outcome performance can vary by utilizing GPU vs CPU. You will typically need more Trees when utilizing the GPU selection
                * Number of GPU's: If you've selected GPU and have access to more than one you can utilize them by increasing this input to the number of GPU's of your choice
                * Create ModelID: This will attach all model artifacts to a name. If you plan on running multiple models for comparison then set a name to each model to distinguish between then and prevent overwriting an already built model
                * CPU Threads: This will default to total available threads minus 2. CPU is still utilized even when you select GPU. If you have other processes running at the same time as modeling you will want to lower the CPU Threads count to give all processes a chance to keep running and prevent a queue pool from forming (which can slow down all processes)
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Model:
                This is where you define the XGBoost parameters
                * Number of Trees: Select the number of trees for training the model
                * Max Tree Depth: Determines the max number of splits each tree can make
                * L1 Regularization: L1 Regularization for tree based methods. Higher the value the less likely for the model to overfit, at the expense of potential accuracy
                * L2 Leaf Regularization: L2 Regularization for tree based methods. Higher the value the less likely for the model to overfit, at the expense of potential accuracy
                * Row Sample Rate: % of rows to utilize in training per tree
                * Column Sample Rate: % of columns to utilize in training per tree
                * Learning Rate: Default is 0.10. Used for reducing the gradient step
                * Min Child Weight: Minimum number of records in leaves for continued splitting
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Grid Tuning:
                This is where you define parameters for a hyperparameter search
                * Grid Tune: Set to TRUE to run grid tuning
                * Baseline Comparison of Models: 'default' will compare all models to the catboost default settings (with the user supplied number of trees). 'best' will compare all models to the current best performer among all models built in the grid tuning process
                * Max Run Time (minutes): Set an upper boundary for run time of grid tuning
                * Runs without a new Winner: Set a value to terminate grid tuning due to lack of new winning models
                * Max Models: Cap the number of models to test in grid tuning mode
                "
                ),

                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Evaluation:
                This is where you define parameters for a hyperparameter search
                * Loss Function: The metric to use in training. The specified value also determines the machine learning problem to solve. Some metrics support optional parameters (see the Objectives and metrics section for details on each metric)
                * Evaluation Metric: The metric used for overfitting detection (if enabled) and best model selection (if enabled). Some metrics support optional parameters (see the Objectives and metrics section for details on each metric)
                * Grid Tuning Evaluation Metric: Metric used to evaluate individual model builds
                * Class Weight for 0 Class: For Binary Classification models, you can up- or down-weight each class. The default under the hood is 1 for both
                * Class Weight for 1 Class: For Binary Classification models, you can up- or down-weight each class. The default under the hood is 1 for both
                "
                ),

                DataMuse::BlankRow(12L),
                shiny::fluidRow(shiny::column(width = 12L,tags$a(target="_blank", shiny::icon("newspaper"), href="https://xgboost.readthedocs.io/en/stable/parameter.html", "XGBoost Parameter Docs")))
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # LightGBM ML ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "lightgbm"),
                    shiny::h1(tags$b("LightGBM")),
                    shiny::p("The LightGBM ML algorithm is for running regression, classification,
                           and multiclass models."),
                    tags$a(target="_blank", shiny::icon("newspaper"), href="https://lightgbm.readthedocs.io/en/latest/", "LightGBM Background Link"),
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("LightGBM Usage"))),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Data:
                This is where you define your datasets and variables of interest
                * Train Data: This is your training dataset. This one is required
                * Validation Data: Optional. If you have a validation dataset supply that here. If not, one will be created for you and 20% of your data will be allocated to Validation Data unless you specify 'TrainOnFull = TRUE' in the Meta Parameters tab.
                * Test Data: Optional. If you have a validation dataset supply that here. If not, one will be created for you and 10% of your data will be allocated to Test Data unless you specify 'TrainOnFull = TRUE' in the Meta Parameters tab.
                * Select Target: This is your dependent variable that you are looking to predict / model
                * Select Features: These are your independent variables used to help predict your target variable
                * TrainOnFull: Select TRUE to train on the full dataset instead of utilizing training, validation, and test datasets. This is typically done when you've found a final set of parameters and are ready to build a final model for production.
                * Character Encoding: If you have character features in your feature set they will be converted to numeric encodings. You can choose which type in this dropdown
                * Weights Column: if you want your rows weighted for modeling purposes you'll need to have a column with the weights within them to supply here. A common reason to use weighting is for when you have group variables and some of the levels have lower counts than others. The ML models will tend to emphasize the higher record count when building so if you find bias is your outcome predictions, weighting can help to resolve that
                * Transform Variables: This is to transform your target variable. There is no need to transform your features (independent variables) as monotonic transformations on features with tree based methods make no difference in outcome
                * Transform Methods: Choose at least one method. The algorithms will determine which transformation among selected will best normalize your target variable and move forward with that one
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Hardware:
                This is where you define your datasets and variables of interest
                * GPU or CPU: If you're running a machine with GPU's available you can select to run with GPU. Note that outcome performance can vary by utilizing GPU vs CPU. You will typically need more Trees when utilizing the GPU selection
                * Number of GPU's: If you've selected GPU and have access to more than one you can utilize them by increasing this input to the number of GPU's of your choice
                * Create ModelID: This will attach all model artifacts to a name. If you plan on running multiple models for comparison then set a name to each model to distinguish between then and prevent overwriting an already built model
                * CPU Threads: This will default to total available threads minus 2. CPU is still utilized even when you select GPU. If you have other processes running at the same time as modeling you will want to lower the CPU Threads count to give all processes a chance to keep running and prevent a queue pool from forming (which can slow down all processes)
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Model:
                This is where you define the LightGBM parameters
                * Number of Trees: Select the number of trees for training the model
                * Max Tree Depth: Determines the max number of splits each tree can make
                * Row Sample Rate: % of rows to utilize in training per tree
                * L1 Regularization: L1 Regularization for tree based methods. Higher the value the less likely for the model to overfit, at the expense of potential accuracy
                * L2 Leaf Regularization: L2 Regularization for tree based methods. Higher the value the less likely for the model to overfit, at the expense of potential accuracy
                * Min Child Weight: Minimum number of records in leaves for continued splitting
                * Learning Rate: Default is 0.10. Used for reducing the gradient step
                * Column Sample Rate by Tree: % of columns to utilize in training per tree
                * Column Sample Rate by Node: % of columns to utilize in training per split
                * Number of Leaves: You can set the maximum number of leaves for a given tree
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Grid Tuning:
                This is where you define parameters for a hyperparameter search
                * Grid Tune: Set to TRUE to run grid tuning
                * Baseline Comparison of Models: 'default' will compare all models to the catboost default settings (with the user supplied number of trees). 'best' will compare all models to the current best performer among all models built in the grid tuning process
                * Max Run Time (minutes): Set an upper boundary for run time of grid tuning
                * Runs without a new Winner: Set a value to terminate grid tuning due to lack of new winning models
                * Max Models: Cap the number of models to test in grid tuning mode
                "
                ),

                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Evaluation:
                This is where you define parameters for a hyperparameter search
                * Loss Function: The metric to use in training. The specified value also determines the machine learning problem to solve. Some metrics support optional parameters (see the Objectives and metrics section for details on each metric)
                * Class Weight for 0 Class: For Binary Classification models, you can up- or down-weight each class. The default under the hood is 1 for both
                * Class Weight for 1 Class: For Binary Classification models, you can up- or down-weight each class. The default under the hood is 1 for both
                "
                ),

                DataMuse::BlankRow(12L),
                shiny::fluidRow(shiny::column(width = 12L,tags$a(target="_blank", shiny::icon("newspaper"), href="https://lightgbm.readthedocs.io/en/v3.3.2/Parameters.html", "LightGBM Parameter Docs")))
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # H2O DRF ML ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "h2odrf"),
                    shiny::h1(tags$b("H2O-DRF")),
                    shiny::p("The H2O-DRF ML algorithm is for running regression, classification,
                           and multiclass models."),
                    tags$a(target="_blank", shiny::icon("newspaper"), href="https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/drf.html", "H2O-DRF Background Link"),
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("H2O-DRF Usage"))),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Data:
                This is where you define your datasets and variables of interest
                * Train Data: This is your training dataset. This one is required
                * Validation Data: Optional. If you have a validation dataset supply that here. If not, one will be created for you and 20% of your data will be allocated to Validation Data unless you specify 'TrainOnFull = TRUE' in the Meta Parameters tab.
                * Test Data: Optional. If you have a validation dataset supply that here. If not, one will be created for you and 10% of your data will be allocated to Test Data unless you specify 'TrainOnFull = TRUE' in the Meta Parameters tab.
                * Select Target: This is your dependent variable that you are looking to predict / model
                * Select Features: These are your independent variables used to help predict your target variable
                * TrainOnFull: Select TRUE to train on the full dataset instead of utilizing training, validation, and test datasets. This is typically done when you've found a final set of parameters and are ready to build a final model for production.
                * Character Encoding: If you have character features in your feature set they will be converted to numeric encodings. You can choose which type in this dropdown
                * Weights Column: if you want your rows weighted for modeling purposes you'll need to have a column with the weights within them to supply here. A common reason to use weighting is for when you have group variables and some of the levels have lower counts than others. The ML models will tend to emphasize the higher record count when building so if you find bias is your outcome predictions, weighting can help to resolve that
                * Transform Variables: This is to transform your target variable. There is no need to transform your features (independent variables) as monotonic transformations on features with tree based methods make no difference in outcome
                * Transform Methods: Choose at least one method. The algorithms will determine which transformation among selected will best normalize your target variable and move forward with that one
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Hardware:
                This is where you define your datasets and variables of interest
                * CPU Threads: Number of CPU threads to utilize for training
                * Create ModelID: This will attach all model artifacts to a name. If you plan on running multiple models for comparison then set a name to each model to distinguish between then and prevent overwriting an already built model
                * Max Memory: This will default to total available threads minus 2. CPU is still utilized even when you select GPU. If you have other processes running at the same time as modeling you will want to lower the CPU Threads count to give all processes a chance to keep running and prevent a queue pool from forming (which can slow down all processes)
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Model:
                This is where you define the H2O-DRF parameters
                * Number of Trees: Select the number of trees for training the model
                * Max Tree Depth: Determines the max number of splits each tree can make
                * Row Sample Rate: % of rows to utilize in training per tree
                * Column Sample Rate: % of columns to utilize in training per split
                * Column Sample Rate per Tree: % of columns to utilize in training per tree
                * Min Rows per Leaf: Minimum number of records in leaves for continued splitting
                * Bins for Categorical Variables: Number of bins for group variable levels. Fewer bins means that more group levels will be combined
                * Bins Top Level Categorical Variables: Number of bins for group variable levels at the top level of each tree. Fewer bins means that more group levels will be combined
                * Histogram Type: Methods for creating splits on numeric variables
                * Categorical Encoding: H2O's encoding methods for categorical variables
                * Stopping Rounds: Stops training when model performance doesn’t improve for the specified number of training rounds, based on a simple moving average
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Grid Tuning:
                This is where you define parameters for a hyperparameter search
                * Grid Tune: Set to TRUE to run grid tuning
                * Max Models: Cap the number of models to test in grid tuning mode
                "
                ),

                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Evaluation:
                This is where you define parameters for a hyperparameter search
                * Loss Function: The metric to use in training. The specified value also determines the machine learning problem to solve. Some metrics support optional parameters (see the Objectives and metrics section for details on each metric)
                * Class Weight for 0 Class: For Binary Classification models, you can up- or down-weight each class. The default under the hood is 1 for both
                * Class Weight for 1 Class: For Binary Classification models, you can up- or down-weight each class. The default under the hood is 1 for both
                "
                ),

                DataMuse::BlankRow(12L),
                shiny::fluidRow(shiny::column(width = 12L,tags$a(target="_blank", shiny::icon("newspaper"), href="https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/drf.html", "H2O DRF Parameter Docs")))
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # H2O GBM ML ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "h2ogbm"),
                    shiny::h1(tags$b("H2O-GBM")),
                    shiny::p("The H2O-GBM ML algorithm is for running regression, classification,
                           and multiclass models."),
                    tags$a(target="_blank", shiny::icon("newspaper"), href="https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/gbm.html", "H2O-GBM Background Link"),
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("H2O-GBM Usage"))),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Data:
                This is where you define your datasets and variables of interest
                * Train Data: This is your training dataset. This one is required
                * Validation Data: Optional. If you have a validation dataset supply that here. If not, one will be created for you and 20% of your data will be allocated to Validation Data unless you specify 'TrainOnFull = TRUE' in the Meta Parameters tab.
                * Test Data: Optional. If you have a validation dataset supply that here. If not, one will be created for you and 10% of your data will be allocated to Test Data unless you specify 'TrainOnFull = TRUE' in the Meta Parameters tab.
                * Select Target: This is your dependent variable that you are looking to predict / model
                * Select Features: These are your independent variables used to help predict your target variable
                * TrainOnFull: Select TRUE to train on the full dataset instead of utilizing training, validation, and test datasets. This is typically done when you've found a final set of parameters and are ready to build a final model for production.
                * Character Encoding: If you have character features in your feature set they will be converted to numeric encodings. You can choose which type in this dropdown
                * Weights Column: if you want your rows weighted for modeling purposes you'll need to have a column with the weights within them to supply here. A common reason to use weighting is for when you have group variables and some of the levels have lower counts than others. The ML models will tend to emphasize the higher record count when building so if you find bias is your outcome predictions, weighting can help to resolve that
                * Transform Variables: This is to transform your target variable. There is no need to transform your features (independent variables) as monotonic transformations on features with tree based methods make no difference in outcome
                * Transform Methods: Choose at least one method. The algorithms will determine which transformation among selected will best normalize your target variable and move forward with that one
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Hardware:
                This is where you define your datasets and variables of interest
                * CPU Threads: Number of CPU threads to utilize for training
                * Create ModelID: This will attach all model artifacts to a name. If you plan on running multiple models for comparison then set a name to each model to distinguish between then and prevent overwriting an already built model
                * Max Memory: This will default to total available threads minus 2. CPU is still utilized even when you select GPU. If you have other processes running at the same time as modeling you will want to lower the CPU Threads count to give all processes a chance to keep running and prevent a queue pool from forming (which can slow down all processes)
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Model:
                This is where you define the H2O-GBM parameters
                * Number of Trees: Select the number of trees for training the model
                * Max Tree Depth: Determines the max number of splits each tree can make
                * Row Sample Rate: % of rows to utilize in training per tree
                * Column Sample Rate: % of columns to utilize in training per split
                * Learning Rate: Used for reducing the gradient step
                * Learning Rate Annealing: Specifies to reduce the Learning Rate by this factor after every tree
                * Column Sample Rate per Tree: % of columns to utilize in training per tree
                * Bins for Categorical Variables: Number of bins for group variable levels. Fewer bins means that more group levels will be combined
                * Min Rows per Leaf: Minimum number of records in leaves for continued splitting
                * Histogram Type: Methods for creating splits on numeric variables
                * Categorical Encoding: H2O's encoding methods for categorical variables
                * Stopping Rounds: Stops training when model performance doesn’t improve for the specified number of training rounds, based on a simple moving average
                * Bins Top Level Categorical Variables: Number of bins for group variable levels at the top level of each tree. Fewer bins means that more group levels will be combined
                "
                ),
                DataMuse::BlankRow(12L),
                shiny::fluidRow(shiny::column(width = 12L,tags$a(target="_blank", shiny::icon("newspaper"), href="https://catboost.ai/en/docs/concepts/algorithm-main-stages_bootstrap-options", "Bootstrap Type Details"))),
                shiny::fluidRow(shiny::column(width = 12L,tags$a(target="_blank", shiny::icon("newspaper"), href="https://catboost.ai/en/docs/concepts/quantization", "Feature Border Type Details"))),
                shiny::fluidRow(shiny::column(width = 12L,tags$a(target="_blank", shiny::icon("newspaper"), href="https://catboost.ai/en/docs/concepts/algorithm-score-functions", "Score Function Details"))),

                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Grid Tuning:
                This is where you define parameters for a hyperparameter search
                * Grid Tune: Set to TRUE to run grid tuning
                * Max Models: Cap the number of models to test in grid tuning mode
                "
                ),

                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Evaluation:
                This is where you define parameters for a hyperparameter search
                * Loss Function: The metric to use in training. The specified value also determines the machine learning problem to solve. Some metrics support optional parameters (see the Objectives and metrics section for details on each metric)
                * Class Weight for 0 Class: For Binary Classification models, you can up- or down-weight each class. The default under the hood is 1 for both
                * Class Weight for 1 Class: For Binary Classification models, you can up- or down-weight each class. The default under the hood is 1 for both
                "
                ),

                DataMuse::BlankRow(12L),
                shiny::fluidRow(shiny::column(width = 12L,tags$a(target="_blank", shiny::icon("newspaper"), href="https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/gbm.html", "H2O GBM Parameter Docs")))
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # H2O GLM ML ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "h2oglm"),
                    shiny::h1(tags$b("H2O-GLM")),
                    shiny::p("The H2O-GLM ML algorithm is for running heirarchical regression models"),
                    tags$a(shiny::icon("newspaper"), href="https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/glm.html", "H2O-GLM Background Link"),
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("H2O-GLM Usage"))),
                shiny::markdown(
                  "
                ### Data:
                This is where you define your datasets and variables of interest
                * Train Data: This is your training dataset. This one is required
                * Select Target: This is your dependent variable that you are looking to predict / model
                * Select Features: These are your independent variables used to help predict your target variable
                * Select Random Effect: You can utilize a single random effects variable
                * TrainOnFull: Select TRUE to train on the full dataset instead of utilizing training, validation, and test datasets. This is typically done when you've found a final set of parameters and are ready to build a final model for production.
                * Character Encoding: If you have character features in your feature set they will be converted to numeric encodings. You can choose which type in this dropdown
                * Weights Column: if you want your rows weighted for modeling purposes you'll need to have a column with the weights within them to supply here. A common reason to use weighting is for when you have group variables and some of the levels have lower counts than others. The ML models will tend to emphasize the higher record count when building so if you find bias is your outcome predictions, weighting can help to resolve that
                * Transform Variables: This is to transform your target variable. There is no need to transform your features (independent variables) as monotonic transformations on features with tree based methods make no difference in outcome
                * Transform Methods: Choose at least one method. The algorithms will determine which transformation among selected will best normalize your target variable and move forward with that one
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Hardware:
                This is where you define your datasets and variables of interest
                * CPU Threads: Number of CPU threads to utilize for training
                * Create ModelID: This will attach all model artifacts to a name. If you plan on running multiple models for comparison then set a name to each model to distinguish between then and prevent overwriting an already built model
                * Max Memory: This will default to total available threads minus 2. CPU is still utilized even when you select GPU. If you have other processes running at the same time as modeling you will want to lower the CPU Threads count to give all processes a chance to keep running and prevent a queue pool from forming (which can slow down all processes)
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Model:
                This is where you define the H2O-GLM parameters
                * Solver: 'IRSLM' is the default for model interpretation purposes
                * Alpha: L1 Regularization; defaults to 0 for model interpretation purposes
                * Lambda: L2 Regularization; defaults to 0 for model interpretation purposes
                * Random Dist: Gaussian only
                * Standardize: defaults to FALSE for model interpretation purposes
                * Remove Collinear Columns: defaults to FALSE for model interpretation purposes
                * Include Intercept: defaults to TRUE for model interpretation purposes
                * Non Negative Coefficients: defaults to FALSE for model interpretation purposes
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Grid Tuning:
                This is where you define parameters for a hyperparameter search
                * Grid Tune: Set to TRUE to run grid tuning
                * Max Models: Cap the number of models to test in grid tuning mode
                "
                ),

                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Evaluation:
                This is where you define parameters for a hyperparameter search
                * Distribution: Choose the appropriate probability distribution that aligns with your target variable (dependent variable)
                * Link: Choose a link function to go with your Distribution choice
                * Tweedie Link Power: Only use with a Tweedie Distribution & Link choices
                * Tweedie Variance Power: Only use with a Tweedie Distribution & Link choices
                * Evalulation Metric: Choose a metric for evaluation
                * Class Weight for 0 Class: For Binary Classification models, you can up- or down-weight each class. The default under the hood is 1 for both
                * Class Weight for 1 Class: For Binary Classification models, you can up- or down-weight each class. The default under the hood is 1 for both
                "
                ),

                DataMuse::BlankRow(12L),
                shiny::fluidRow(shiny::column(width = 12L,tags$a(target="_blank", shiny::icon("newspaper"), href="https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/glm.html", "H2O GLM Parameter Docs")))
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # H2O HGLM ML ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "h2ohglm"),
                    shiny::h1(tags$b("H2O-HGLM")),
                    shiny::p("The H2O-HGLM ML algorithm is for running regression, classification,
                           and multiclass models."),
                    tags$a(target="_blank", shiny::icon("newspaper"), href="https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/glm.html#hierarchical-glm", "H2O-HGLM Background Link"),
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("H2O-HGLM Usage"))),
                shiny::markdown(
                  "
                ### Data:
                This is where you define your datasets and variables of interest
                * Train Data: Only training data for this model type, which is used for regression inference
                * Select Target: This is your dependent variable that you are looking to predict / model
                * Select Features: These are your independent variables used to help predict your target variable
                * TrainOnFull: Select TRUE to train on the full dataset instead of utilizing training, validation, and test datasets. This is typically done when you've found a final set of parameters and are ready to build a final model for production.
                * Character Encoding: If you have character features in your feature set they will be converted to numeric encodings. You can choose which type in this dropdown
                * Weights Column: if you want your rows weighted for modeling purposes you'll need to have a column with the weights within them to supply here. A common reason to use weighting is for when you have group variables and some of the levels have lower counts than others. The ML models will tend to emphasize the higher record count when building so if you find bias is your outcome predictions, weighting can help to resolve that
                * Transform Variables: This is to transform your target variable. There is no need to transform your features (independent variables) as monotonic transformations on features with tree based methods make no difference in outcome
                * Transform Methods: Choose at least one method. The algorithms will determine which transformation among selected will best normalize your target variable and move forward with that one
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Hardware:
                This is where you define your datasets and variables of interest
                * CPU Threads: Number of CPU threads to utilize for training
                * Create ModelID: This will attach all model artifacts to a name. If you plan on running multiple models for comparison then set a name to each model to distinguish between then and prevent overwriting an already built model
                * Max Memory: This will default to total available threads minus 2. CPU is still utilized even when you select GPU. If you have other processes running at the same time as modeling you will want to lower the CPU Threads count to give all processes a chance to keep running and prevent a queue pool from forming (which can slow down all processes)
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Model:
                This is where you define the H2O-HGLM parameters
                * Solver: 'IRSLM' is the default for model interpretation purposes
                * Alpha: L1 Regularization; defaults to 0 for model interpretation purposes
                * Lambda: L2 Regularization; defaults to 0 for model interpretation purposes
                * Standardize: defaults to FALSE for model interpretation purposes
                * Remove Collinear Columns: defaults to FALSE for model interpretation purposes
                * Include Intercept: defaults to TRUE for model interpretation purposes
                * Non Negative Coefficients: defaults to FALSE for model interpretation purposes
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Grid Tuning:
                This is where you define parameters for a hyperparameter search
                * Grid Tune: Set to TRUE to run grid tuning
                * Max Models: Cap the number of models to test in grid tuning mode
                "
                ),

                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Evaluation:
                This is where you define parameters for a hyperparameter search
                * Distribution: Choose the appropriate probability distribution that aligns with your target variable (dependent variable)
                * Link: Choose a link function to go with your Distribution choice
                * Tweedie Link Power: Only use with a Tweedie Distribution & Link choices
                * Tweedie Variance Power: Only use with a Tweedie Distribution & Link choices
                * Evalulation Metric: Choose a metric for evaluation
                * Class Weight for 0 Class: For Binary Classification models, you can up- or down-weight each class. The default under the hood is 1 for both
                * Class Weight for 1 Class: For Binary Classification models, you can up- or down-weight each class. The default under the hood is 1 for both
                "
                ),

                DataMuse::BlankRow(12L),
                shiny::fluidRow(shiny::column(width = 12L,tags$a(target="_blank", shiny::icon("newspaper"), href="https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/glm.html#hierarchical-glm", "H2O HGLM Parameter Docs")))
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Causal Mediation ML ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "causalmediation"),
                    shiny::h1(tags$b("Causal Mediation")),
                    shiny::p("The Causal Mediation algorithm is for generating causing inference for
                           regression, classification, and survival models."),
                    tags$a(target="_blank", shiny::icon("newspaper"), href="https://cran.r-project.org/web/packages/regmedint/index.html", "Causal Mediation Background Link"),
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Causal Mediation Usage"))),
                DataMuse::BlankRow(12L),
                shiny::markdown(
                  "
                ### Variables:
                This is where you define your variables for causal meditation
                * Choose data set: This is your training dataset
                * Outcome Target: regmedint yvar; Outcome variable name. If you're utilizing a survival model this should be the time variable for the survival outcome models
                * Treatment Variable: regmedint avar; Treatment variable name
                * Mediator Variable: regmedint mvar; Mediator variable name
                * Survival Event Variable: regmedint eventvar; Only required for survival outcome regression models. Note that the coding is 1 for event and 0 for censoring, following the R survival package convention
                * Outcome Target Covariates: regmedint cvar; Covariate names (features / independent variables / confounders)
                * Outcome Treatment Covariates: Treatment & Covariates interaction variables for the treatment model
                * Mediation Treatment Covariates: Treatment & Covariates interaction variables for the mediation model
                * Outcome Mediator Covariates: Treatment & Covariates interaction variables for the outcome model
                * Treatment Mediator Interaction: regmedint interaction; logical indicating the presense of a treatment & mediator interaction term in the outcome model
                * Case Control Data: If the data comes from a case-control study then set this to TRUE
                "
                ),
                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Evaluation Values:
                This is where you define your covariate values that you want evaluated
                * Mediation Untreated Value: a0; The reference level of treatment variable that is considered 'untreated' or 'unexposed'
                * Remove NA's
                * Mediation Treated Value: a1; The reference level of treatment variable that is considered 'treated' or 'exposed'
                * Mediator Control Direct Level: m_cde; Mediator level at which controlled direct effect is evaluated at
                * Natural Direct Effect: c_cond; One value for each cvar. Covariate levels at which natural direct and indirect effects are evaluated at
                "
                ),

                DataMuse::BlankRow(12),
                shiny::markdown(
                  "
                ### Model Types:
                This is where you define your model types
                * Outcome Target Type: regmedint yreg; Choose from
                  * 'linear'
                  * 'logistic'
                  * 'loglinear'
                  * 'poisson'
                  * 'negbin'
                  * 'survCox'
                  * 'survAFT_exp'
                  * 'survAFT_weibull'
                * Mediator Target Type: regmedint mreg; Choose from
                  * 'linear'
                  * 'logistic'
                "
                ),

                DataMuse::BlankRow(12L),
                shiny::tags$img(src = "https://github.com/AdrianAntico/prettydoc/blob/main/Images/CausalMediationData.PNG?raw=true", alt = "DataMuse", `data-view-component` = "true", height="100%", width="100%"),
                DataMuse::BlankRow(12L),
                shiny::markdown(
                  "
                ## Example Data Columns:

                ### Variables:
                * Outcome Target = 'y'
                * Treatment Variable = 'x'
                * Mediator Variable = 'm'
                * Outcome Target Covariates = 'c'
                * Survival Event Variable = 'event'
                * Treatment Mediator Interaction = TRUE
                * Case Control Data = FALSE

                ### Evaluation Values:
                * Mediation Untreated Value = 0
                * Mediation Treated Value = 1
                * Mediator Control Direct Level = 1
                * Natural Direct Effect = 0.5

                ### Model Types:
                * Mediator Target Type = 'logistic'
                * Outcome Target Type = 'survAFT_weibull'"
                ),

                DataMuse::BlankRow(12L),
                shiny::fluidRow(shiny::column(width = 12L,tags$a(target="_blank", shiny::icon("newspaper"), href="https://www.hsph.harvard.edu/tyler-vanderweele/tools-and-tutorials/", "Causal Mediation Background Learning")))
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # ----

              # ----

              ################################################# ----
              # Forecasting                                     ----
              ################################################# ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "forecasting"),
                    shiny::h2(tags$b("Forecasting")),
                    shiny::p("Forecasting is a vitally important aspect of this software. It's important that you know how to
                             utilize the functionality as intended. Below are all of the algorithms with descriptions about
                             how to use each and every one for each of their intended uses. If you've done forecasting in the
                             past you'll find that the methods provided are as feature rich as you've encountered, if at all.
                             There are serveral forecasting use cases covered in this software, including:"),
                    DataMuse::BlankRow(12),
                    shiny::markdown(
                      "
                      * Single Series
                      * Panel Series
                      * Irregular Panel Series (irregular start / end of panel levels)
                      * Intermittent Demand (large time gaps between positive instances of demand)
                      * Funnel / Cohort Series
                      "
                    ),
                    DataMuse::BlankRow(12),
                    shiny::h3("Algorithm Selection:"),
                    shiny::markdown(
                      "
                      Single Series Models
                      * TBATS (Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal components)
                      * SARIMA (Seasonal Autoregressive Integrated Moving Average)
                      * ETS (Exponential Smoothing State Space Model)
                      * ARFIMA (Autoregressive Fractionally Integrated Moving Average)
                      * NNET (Single layer neural network)

                      Panel models (can also be used on single series data)
                      * CatBoost Panel
                      * XGBoost Panel
                      * LightGBM Panel

                      Intermittent Demand Models

                      * CatBoost ID (Intermittent Demand)
                      * XGBoost ID (Intermittent Demand)
                      * LightGBM ID (Intermittent Demand)

                      Funnel Models (calendar date + cohort date)
                      * CatBoost Funnel
                      * XGBoost Funnel
                      * LightGBM Funnel
                      "
                    ),
                    DataMuse::BlankRow(12),
                    shiny::h3("Run Modes:"),
                    shiny::markdown(
                      "
                      There are various Run modes to training, backtesting, and forecasting:
                      * Train Model: This is equivalent to building an ML model
                      * Retrain Existing Model: This is for retraining a model that's already been built before. Perhaps you simply want an updated model but not a new forecast at the moment
                      * Backtest: This task will train a new model (if FC ArgsList is not supplied) and generate an N-Period ahead forecast that will be evaluated using Validation Data supplied by the user. If you don't have a Validation dataset, go to Data Wrangling and subset rows based on a time variable. The subset data will be your Training Data and your original dataset will be the Validation Data
                      * Feature Engineering Test: This task will loop through various builds starting from the most simple up to a moderately sophisticated models. An evaluation table is generated that you can view in the Tables tab when the procedure is complete. Evaluation metrics are based on the Backtest method. Features tested are below and are in order. If a feature is beneficial it will remain in the models trained thereafter:
                        1. LogPlus1 vs None: this will test whether a target variable transformation is beneficial
                        2. Series Difference vs None: this will test whether utilizing Differencing your series is useful
                        3. Calendar Variables vs None: this will test whether utilizing Calendar Variables is useful
                        4. Holiday Variable vs None: this will test whether utilizing Holiday Variables is useful
                        5. Credibility vs Target Encoding : this will test whether a target encoding is better than a credibility encoding
                        6. Time Weights vs None: this will test whether utilizing Time Weighting is useful
                        7. Anomaly Detection vs None: this will test whether utilizing Anomaly Detection is useful
                        8. Time Trend Variable vs None: this will test whether utilizing a Time Trend Variable is useful
                        9. Lag 1 vs None: this will test whether utilizing Lags are useful (user supplies the lags and if none are set then a lag1 is tested)
                      * Backtest Cross Evaluation: Once you have a good model designed you can mock production by running this procedure. Here, you'll set the data refresh rate and the model update rate. Performance measure are returned in a data.table once the procedure is finished.
                      * Forecast: if you have a trained model you can call it to generate a forecast for you
                      * Retrain + Forecast: if you have a model you can refresh it and have it generate a forecast for you
                      "
                    ),
                    DataMuse::BlankRow(12),
                    shiny::h3("Features Engineering:"),
                    shiny::markdown(
                      "
                      * Single shot backtesting over a defined horizon
                      * Roll forward backtesting over a defined horizon
                      * Series filling for any missing gaps
                      * Lags
                      * Moving averages
                      * Calendar variables
                      * Holiday variables
                      * Holiday lookback window for extended effects
                      * Target transformation
                      * Categorical variable encoding
                      * Differencing
                      * Exongenous regressors (user supplied)
                      * Time trend variable
                      * Anomaly detection as variables
                      * Data truncation testing for NA produced by rolling stats
                      * All the ML hyperparameters that come with the underlying ML algorithms
                      "
                    )
                  )
                ),

                DataMuse::BlankRow(12),
                shiny::fluidRow(shiny::column(width = 12L, shiny::h3("Forecasting Usage"))),
              ),

              DataMuse::BlankRow(12L),
              DataMuse::BlankRow(12L),
              DataMuse::BlankRow(12L),
              DataMuse::BlankRow(12L),
              DataMuse::BlankRow(12L),
              DataMuse::BlankRow(12L),
              DataMuse::BlankRow(12L),
              DataMuse::BlankRow(12L),
              DataMuse::BlankRow(12L),

              # ----

              # ----

              ################################################# ----
              # Settings                                        ----
              ################################################# ----

              # Settings
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "settings"),
                    shiny::h3("Settings"),
                    shiny::p("Settings contains some plotting enhancements, app enchancements, and some other useful metadata.")
                  )
                )
              ),

              DataMuse::BlankRow(12L),
              DataMuse::BlankRow(12L),
              DataMuse::BlankRow(12L),

              # App Themes ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "appthemes"),
                    shiny::h3("App Themes"),
                    shiny::p("There are a variety of app themes to select from. Below is a list of each one of them.
                             Associated with each are their default plot themes as well. For a full list of plot theme
                             images, scroll to the next section.")
                  )
                ),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("App Theme: day-light-blue; Plot Theme: wef"),
                    shiny::tags$img(
                      src = "day-light-blue_wef.PNG",# "https://github.com/AdrianAntico/prettydoc/blob/main/Images/day-light-blue_wef.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("App Theme: light-gray; Plot Theme: wef"),
                    shiny::tags$img(
                      src = "light-gray_wef.PNG",# "https://github.com/AdrianAntico/prettydoc/blob/main/Images/light-gray_wef.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("App Theme: medium-gray; Plot Theme: wef"),
                    shiny::tags$img(
                      src = "medium-gray_wef.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/medium-gray_wef.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("App Theme: dark-gray; Plot Theme: wef"),
                    shiny::tags$img(
                      src = "dark-gray_wef.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/dark-gray_wef.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("App Theme: piano-black; Plot Theme: wef"),
                    shiny::tags$img(
                      src = "piano-black_wef.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/piano-black_wef.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("App Theme: yellow; Plot Theme: bee-inspired"),
                    shiny::tags$img(
                      src = "yellow_bee-inspired.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/yellow_bee-inspired.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("App Theme: yellow-green; Plot Theme: forest"),
                    shiny::tags$img(
                      src = "yellow-green_forest.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/yellow-green_forest.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("App Theme: green; Plot Theme: green"),
                    shiny::tags$img(
                      src = "green_green.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/green_green.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("App Theme: green-blue; Plot Theme: green"),
                    shiny::tags$img(
                      src = "green-blue_green.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/green-blue_green.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("App Theme: light-blue; Plot Theme: walden"),
                    shiny::tags$img(
                      src = "light-blue_walden.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/light-blue_walden.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("App Theme: dodger-blue; Plot Theme: walden"),
                    shiny::tags$img(
                      src = "dodger-blue_walden.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/dodger-blue_walden.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("App Theme: blue; Plot Theme: walden"),
                    shiny::tags$img(
                      src = "blue_walden.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/blue_walden.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("App Theme: blue-purple; Plot Theme: macarons"),
                    shiny::tags$img(
                      src = "blue-purple_macarons.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/blue-purple_macarons.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("App Theme: purple; Plot Theme: walden"),
                    shiny::tags$img(
                      src = "purple_walden.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/purple_walden.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("App Theme: pink; Plot Theme: wonderland"),
                    shiny::tags$img(
                      src = "pink_wonderland.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/pink_wonderland.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("App Theme: red; Plot Theme: red"),
                    shiny::tags$img(
                      src = "red_red.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/red_red.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),

              # Plot Themes ----
              shinydashboard::box(
                title = NULL, solidHeader = TRUE, collapsible = FALSE, status = "danger", width = 12L,
                shiny::fluidRow(
                  shiny::column(
                    width = 12L,
                    shiny::a(name = "plotthemes"),
                    shiny::h3("App Themes"),
                    shiny::p("There are a variety of app themes to select from. Below is a list of each one of them.
                             Associated with each are their default plot themes as well. For a full list of plot theme
                             images, scroll to the next section.")
                  )
                ),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: auritus"),
                    shiny::tags$img(
                      src = "auritus.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/auritus.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: azul"),
                    shiny::tags$img(
                      src = "azul.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/azul.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: bee-inspired"),
                    shiny::tags$img(
                      src = "bee-inspired.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/bee-inspired.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: blue"),
                    shiny::tags$img(
                      src = "blue.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/blue.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: caravan"),
                    shiny::tags$img(
                      src = "caravan.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/caravan.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: carp"),
                    shiny::tags$img(
                      src = "carp.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/carp.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: chalk"),
                    shiny::tags$img(
                      src = "chalk.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/chalk.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: cool"),
                    shiny::tags$img(
                      src = "cool.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/cool.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: dark-bold"),
                    shiny::tags$img(
                      src = "dark-bold.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/dark-bold.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: dark"),
                    shiny::tags$img(
                      src = "dark.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/dark.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: eduardo"),
                    shiny::tags$img(
                      src = "eduardo.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/eduardo.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: essos"),
                    shiny::tags$img(
                      src = "essos.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/essos.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: forest"),
                    shiny::tags$img(
                      src = "forest.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/forest.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: fresh-cut"),
                    shiny::tags$img(
                      src = "fresh-cut.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/fresh-cut.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: fruit"),
                    shiny::tags$img(
                      src = "fruit.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/fruit.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: gray"),
                    shiny::tags$img(
                      src = "gray.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/gray.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: green"),
                    shiny::tags$img(
                      src = "green.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/green.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: halloween"),
                    shiny::tags$img(
                      src = "halloween.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/halloween.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: helianthus"),
                    shiny::tags$img(
                      src = "helianthus.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/helianthus.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: infographic"),
                    shiny::tags$img(
                      src = "infographic.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/infographic.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: inspired"),
                    shiny::tags$img(
                      src = "inspired.PNG", # https://github.com/AdrianAntico/prettydoc/blob/main/Images/inspired.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: jazz"),
                    shiny::tags$img(
                      src = "jazz.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/jazz.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: london"),
                    shiny::tags$img(
                      src = "london.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/london.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: macarons"),
                    shiny::tags$img(
                      src = "macarons.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/macarons.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: macarons2"),
                    shiny::tags$img(
                      src = "macarons2.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/macarons2.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: mint"),
                    shiny::tags$img(
                      src = "mint.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/mint.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: purple-passion"),
                    shiny::tags$img(
                      src = "purple-passion.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/purple-passion.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: red-velvet"),
                    shiny::tags$img(
                      src = "red-velvet.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/red-velvet.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: red"),
                    shiny::tags$img(
                      src = "red.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/red.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: roma"),
                    shiny::tags$img(
                      src = "roma.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/roma.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: royal"),
                    shiny::tags$img(
                      src = "royal.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/royal.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: sakura"),
                    shiny::tags$img(
                      src = "sakura.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/sakura.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: shine"),
                    shiny::tags$img(
                      src = "shine.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/shine.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: tech-blue"),
                    shiny::tags$img(
                      src = "tech-blue.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/tech-blue.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: vintage"),
                    shiny::tags$img(
                      src = "vintage.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/vintage.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: walden"),
                    shiny::tags$img(
                      src = "walden.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/walden.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: wef"),
                    shiny::tags$img(
                      src = "wef.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/wef.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: weforum"),
                    shiny::tags$img(
                      src = "weforum.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/weforum.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  ),
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: westeros"),
                    shiny::tags$img(
                      src = "westeros.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/westeros.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                ),
                DataMuse::BlankRow(12),
                DataMuse::BlankRow(12),
                shiny::fluidRow(
                  shiny::column(
                    w = 4,
                    shiny::h3("Plot Theme: wonderland"),
                    shiny::tags$img(
                      src = "wonderland.PNG", # "https://github.com/AdrianAntico/prettydoc/blob/main/Images/wonderland.PNG?raw=true",
                      alt = "DataMuse",
                      `data-view-component` = "true",
                      height="100%",
                      width="100%")
                  )
                )
              ),

              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12),
              DataMuse::BlankRow(12)
            )
          )
        )
      )
    ),

    ################################################# ----
    # Home                                            ----
    ################################################# ----
    # shiny::tabPanel(
    #   title = "Showcase",
    #   icon = shiny::icon("mountain-sun"),
    #   shiny::fluidRow(
    #     width=12L,
    #     style="padding-left: 15px",
    #     id = "PlotAddButtonsUILocation",
    #     DataMuse::BlankRow(12),
    #     DataMuse::BlankRow(12),
    #     shiny::fluidRow(
    #       Swiper4r::Swiper4r(
    #         Images, height = "600px", width = "1100px",effect = "cube",
    #         autoplay = list(delay = 2500, disableOnInteraction = FALSE),
    #         id = "CAROUSEL"
    #       )
    #     )
    #   )
    # ),

    ################################################# ----
    # View                                            ----
    ################################################# ----
    shiny::tabPanel(
      title = "View",
      icon = shiny::icon("mountain-sun"),
      shiny::fluidRow(
        width=12L,
        style="padding-left: 15px",
        id = "PlotAddButtonsUILocation",
        DataMuse::BlankRow(12),
        DataMuse::BlankRow(12)#,
        #shiny::fluidRow(
        #  shiny::h3("Enjoy the view", style = "padding-left: 45px")
        #)
      )
    )
  ) # closes box
}
