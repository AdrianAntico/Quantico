![Version:1.0.0](https://img.shields.io/static/v1?label=Version&message=1.0.0&color=blue&?style=plastic)
![Build:Passing](https://img.shields.io/static/v1?label=Build&message=passing&color=brightgreen)
[![PRsWelcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=default)](http://makeapullrequest.com)
[![GitHubStars](https://img.shields.io/github/stars/AdrianAntico/ShinyDS.svg?style=social)](https://github.com/AdrianAntico/ShinyDS)

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/LogoBlue1.png" align="center" width="600" />

Quantico is a Shiny App for data science, analytics, and business intelligence. The app is non-reactive where big data can cause a poor user experience. All data operations utilize data.table and collapse for fast processing and low memory utilization. Visualizations are based on the echarts4r library and the best machine learning and forecasting models are available. Data can be accessed in several ways and session saving and restoration is available. 15 app themes are available with a large variety of background images as well. 

Note: For the best user experience I recommend using Chrome and having the zoom level set to 75%

At a high level the app enables:
* Data Management
* Session Saving / Loading
* Code Generation
* Plotting
* Tables Viewer
* Data Wrangling
* Feature Engineering
* Machine Learning
* Inference
* Forecasting

Automated output reports are included for:
1. Plotting
2. Exploratory Data Analysis
3. Inference
4. Machine Learning
5. Forecasting

<br>

## App Usage
- [Installation](#Installation)
- [Quick Start](#Quickstart)
- [App Interface](#app-interface)
- [Documentation](#Documentation)
- [Data Management](#data-management)
- [Code Generation](#code-generation)
- [Visualization](#Visualization)
- [Table Viewing](#tables-viewer)
- [Exploratory Data Analysis](#exploratory-data-analysis)
- [Data Wrangling](#data-wrangling)
- [Feature Engineering](#feature-engineering)
- [Inference](#Inference)
- [Inference Reporting](#inference-reporting)
- [Machine Learning](#machine-learning)
- [Machine Learning Reporting](#machine-learning-reporting)
- [Forecasting](#Forecasting)
- [Forecasting Reporting](#forecasting-reporting)


<br>

## Installation

Note: if you're only looking to update Quantico, you only have to reinstall the Quantico package below in Step 5.

If you are setting up R for the first time run Steps 1-3

<b>Step 1:</b> install the "R-release" version of **rtools** and have it placed in your C:\ drive https://cran.r-project.org/bin/windows/Rtools/

<b>Step 2:</b> install **R** https://cran.r-project.org/bin/windows/base/

<b>Step 3:</b> install **RStudio Desktop** https://posit.co/download/rstudio-desktop/

<b>Step 4:</b> Install dependencies and Quantico two ways:

1. For Windows users, download DataMuse.rar, open it up, go into the DataMuse folder, and then run the installer, "Datamuse_installer_Win10x64"

2. Open up an RStudio session and run the following installation commands from the list of package below. It's best to go one by one in case there is an issue installing any one of them

<b>Step 5:</b> Here is a list of the dependencies to install

<details><summary> Click to expand </summary>

```r
options(install.packages.compile.from.source = "always")

# CRAN Packages
install.packages("devtools")
install.packages("data.table")
install.packages("collapse")
install.packages("bit64")
install.packages("doParallel")
install.packages("foreach")
install.packages("lubridate")
install.packages("timeDate")
install.packages("combinat")
install.packages("DBI")
install.packages("e1071")
install.packages("fBasics")
install.packages("itertools")
install.packages("MLmetrics")
install.packages("nortest")
install.packages("pROC")
install.packages("RColorBrewer")
install.packages("RPostgres")
install.packages("Rfast")
install.packages("stringr")
install.packages("xgboost")
install.packages("lightgbm")
install.packages("regmedint")
install.packages("RCurl")
install.packages("jsonlite")
install.packages("h2o")
install.packages("AzureStor")
install.packages("gitlink")
install.packages("arrow")
install.packages("reactable")
install.packages("DT")
install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinyWidgets")
install.packages("shiny.fluent")
install.packages("shinyjs")
install.packages("shinyjqui")
install.packages("shinyAce")
install.packages("shinybusy")
install.packages("gyro")
install.packages("arrangements")
install.packages("echarts4r")
install.packages('tidytext')
install.packages('tibble')
install.packages('stopwords')
install.packages('SentimentAnalysis')
install.packages('quanteda')
install.packages('quanteda.textstats')
install.packages('datamods')
install.packages('phosphoricons')
install.packages('correlation')

# GitHub Packages
devtools::install_url('https://github.com/catboost/catboost/releases/download/v1.2/catboost-R-Windows-1.2.tgz', INSTALL_opts = c("--no-multiarch", "--no-test-load"))
devtools::install_github("AdrianAntico/prettydoc", upgrade = FALSE, dependencies = FALSE, force = TRUE)
devtools::install_github("AdrianAntico/AutoNLP", upgrade = FALSE, dependencies = FALSE, force = TRUE)
devtools::install_github("AdrianAntico/AutoPlots", upgrade = FALSE, dependencies = FALSE, force = TRUE)
devtools::install_github("AdrianAntico/Rodeo", upgrade = FALSE, dependencies = FALSE, force = TRUE)
devtools::install_github("AdrianAntico/AutoQuant", upgrade = FALSE, dependencies = FALSE, force = TRUE)
devtools::install_github("AdrianAntico/esquisse", upgrade = FALSE, dependencies = FALSE, force = TRUE)
devtools::install_github("AdrianAntico/Quantico", upgrade = FALSE, dependencies = FALSE, force = TRUE, auth_token = "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")
```

</details>

<br>

## Quickstart

In your RStudio session, run the Quantico::Muse() function to kick off a Quantico session

Easy start

```r
# Optionally, you can change up the WorkingDirectory argument for your desired file path location
# Note: For the best user experience I recommend using Chrome and having the zoom level set to 75%
Quantico::runQuantico(WorkingDirectory = getwd())
```

If you have a PostGRE installation you can add in the PostGRE parameters (or just pass them in while in session)

```r
# Optionally, you can change up the WorkingDirectory argument for your desired file path location (don't forget to use these "/" instead of these "\" in your path)
# Note: For the best user experience I recommend using Chrome and having the zoom level set to 75%
Quantico::runQuantico(
  MaxTabs = 2L,
  WorkingDirectory = getwd(),
  PostGRE_DBNames = NULL, # list of database names you want connected
  PostGRE_Host = 'localhost',
  PostGRE_Port = 54321,
  PostGRE_User = '...',
  PostGRE_Password = '...')
```

<br>

## App Interface

src="https://github.com/AdrianAntico/Quantico/blob/main/inst/AppMain6.PNG" align="center" width="800" />

<br>

## Documentation
The documentation is located in the Home Tab in the Documentation tab. There is a side bar full of hyperlinks to speed up navigation. You simply click the topic of choice (and perhaps again if there are sub-categories) and the app will navigate to that location.

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Documentation1.PNG" align="center" width="800" /> 

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Documentation2.PNG" align="center" width="800" />

<br>

## Data Management
On the side bar, under Load / Save, you have a few options:
1. Local
2. Sessions
3. PostGRE

##### Local
With the local modal you can load and save:
1. csv data
2. parquet data
3. machine learning models

##### Sessions
You can save your session state and reload this at a later time. You can have a pre-configured plot output setup that you don't want to have to recreate every time you run the app. This would be similar to having saved reports. Further, all output panels will re-populate with what was previously setup at the time of the last save. 

##### PostGRE
1. Query data
2. Create tables
3. Create databases
4. Remove tables
5. Remove databases

<br>

## Code Generation
The Code generation tab returns the code that was used to execute the various tasks and generate output. You can select from a variety of code themes as well. This can be really helpful to those who are looking to kickstart a project and then convert to a coding environment later. Some output can simply be generated much more quickly utilizing the app so this should be a time saver even to the most seasoned programmers. 

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/CodeGeneration1.PNG" align="center" width="800" />

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/CodeGeneration2.PNG" align="center" width="800" />

<br>

## Visualization

#### Plotting Basics
Plotting is a vitally important aspect of this software. It's important that you know how to utilize the functionality as intended. One of the goals is to make plotting as easy as possible. You don't have to pre-aggrgate your data for plotting purposes since those steps will be carried out for you (although it can be). Just pass in your data and utilize the inputs to tell the software what you want.

### Plot Types

| Distribution     | Aggregate       | Time Series      | Relationship | Model Evaluation           |
| ---------------- | --------------- | ---------------- | ------------ | -------------------------- |
| Histogram        | Barplot         | Line             | Correlogram  | Residuals                  |
| Density          | Stacked Barplot | Area             | Parallel     | Residulas Scatter          |
| Boxplot          | 3D Barplot      | Step             | Scatter      | Partial Dependence Line    |
| Word Cloud       | Heatmap         | River            | 3D Scatter   | Partial Dependence Heatmap |
| Probability Plot | Radar           | Autocorrelation  | Copula       | Calibration Line           |
|                  | Piechart        | Partial Autocorr | 3D Copula    | Calibration Boxplot        |
|                  | Donut           |                  |              | Variable Importance        |
|                  | Rosetype        |                  |              | Shapley Importance         |
|                  |                 |                  |              | ROC Plot                   |
|                  |                 |                  |              | Confusion Matrix           |
|                  |                 |                  |              | Gains                      |
|                  |                 |                  |              | Lift                       |

### Faceting
For the plots that enable faceting you only have to select the number of columns and rows and the app will take care of the rest. Note that, if your group variable contains more levels than the total allotted facet grid and you didn't subset the group levels to match that count, in the case that there are more levels than grid elements then the levels with the most records will be displayed first. Ties go to ABC order.

### Aggregation Methods
Since the software will automatically aggregate your data (for some of the plot types) you can specify how you'd like your data aggregated. Below is a list of options:
1. `count` Counts of values by group. Here, you need to select any of the numeric YVars available in your data just so it doesn`t create an error for a missing YVar
2. `proportion` Proportion of total by group. Here, you need to select any of the numeric YVars available in your data just so it doesn`t create an error for a missing YVar
3. `mean`
4. `meanabs` (absolute values are taken first, then the measure)
5. `median`
6. `medianabs` (absolute values are taken first, then the measure)
7. `sum`
8. `sumabs` (absolute values are taken first, then the measure)
9. `sd` (standard deviation)
10. `sdabs` (absolute values are taken first, then the measure)
11. `skewness`
12. `skewnessabs` (absolute values are taken first, then the measure)
13. `kurtosis`
14. `kurtosisabs` (absolute values are taken first, then the measure)
15. `CoeffVar` (coefficient of variation)
16. `CoeffVarabs` (absolute values are taken first, then the measure)

### Datetime Aggregation
If you have a numeric X-Variable you can choose to display your plot on a higher grain datetime. For example, if you have daily data and you are looking to build a barplot time series, you can switch the default date aggregate parameter from "as-is" to "month" to display monthly aggregated time series.

### Variable Transformation Methods
For numeric variables you can choose to have them transformed automatically
1. `Asinh`: inverse hyperbolic sine
2. `Log`: natural logarithm
3. `LogPlus1` (natural log(x + absolute value of minimum value if min value is negative))
4. `Sqrt`: square root
5. `Asin`: inverse sine
6. `Logit`
7. `BoxCox`
8. `YeoJohnson`

### Plot Inputs
In the plotting panel you simply click on the top buttons (e.g. Plot 1, Plot 2, ...) and select a plot type from the dropdown menu. Then you click the button below to fill out the necessary parameters for your plot. Lastly, drop the newly created box in the dragula pane and move it to the bottom row in order for it to display.

When you click the button below the plot type dropdown, a modal will appear with up to five tabs for inputs and selections:
1. Data Selection Tab
2. Axis Variables Tab
3. Grouping Variables Tab (in most cases but not all)
4. Filter Variables Tab
5. Formatting Tab

##### Data Selection
The Data Selection tab is where you'll choose your dataset and number of records to display. The display record count is the number of records used for display purposes. For plots that require data aggregation display records won't typically matter but for non-aggregated data plots the records displayed are randomly sampled from your data right before the plot build occurs; not before any data preparation steps. 

##### Axis Variables

Axis variables:
The Axis Variables tab is where you'll define your axis variables and any transformations you'd like applied. The modals are designed to only supply inputs that are actually used for the given plot type. For example, histogram plots only required variables to be defined across a single dimension (you can select more than one variable however), whereas with line plots, you'll need to defined an X-Axis variable (a date variable) and Y-Axis variables.

Transformations:
Automatic transformations can be selected and generated for numeric variables during the data preparation process while the software builds the plots.

##### Group Variables
The Group Variables tab is where you'll <i>optionally</i> define up to 3 group variables and faceting selection (if applicable). Since multiple group variables are allowed for the plotting engine the group variables will be concatenated and the combined levels will be displayed. For each group variable you can select the levels you wish to have displayed. For faceting, you simply select the number of rows and columns desired to form the grid of your choice.

##### Filter Variables
The Filter Variables tab is where you can optionally define filters for your data before having the plot displayed. You can select up to 4 filter variables, you'll define the logical operation you want conducted, and associated values based on the logical operation you selected. 

##### Formatting
The Formatting tab is where you can rename the plot title and axis titles. You can also select to have data values shown on the plots.

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Plotting1.PNG" align="center" width="400" /> <img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Plotting2.PNG" align="center" width="400" />

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Plotting3.PNG" align="center" width="400" /> <img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Plotting4.PNG" align="center" width="400" />

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Plotting5.PNG" align="center" width="400" /> <img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Plotting6.PNG" align="center" width="400" />


### Plotting Report Export
You can save you plotting setup to an html file. Just click the Save button after you've setup your plots. While you can setup a grid of output in the app the plots will be stacked on top of each other in the html file due to limited space. The only time this doesn't occur is for faceted plots, which are themselves a grid within a grid. 

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/PlottingReport.PNG" align="center" width="800" />

<br>

## Tables Viewer
The Tables Viewer output tab allows you to views multiple tables stacked on top of each other. You can alter the number of records displayed, total records brought into the table, randomly sampled or not, and a few other formatting options. This can be useful for inspecting data after running some of the various tasks when you want to view new data or altered data.

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/TablesViewer1.PNG" align="center" width="800" />

## Exploratory Data Analysis
The Exploratory Data Analysis Report can display a variety of data insights, by a group variable if desired, including:
1. Data dictionary information
2. Univariate statistics
3. Univariate box plots
4. Correlogram
5. Trend line plots

#### EDA Collapsed Output View
<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/EDA1.PNG" align="center" width="800" /> 

#### EDA Expanded Output View
<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/EDA2.PNG" align="center" width="800" />

### EDA Report Export
The EDA Report can be generated by clicking the Save button on the EDA Output Panel either before or after generating the EDA info in app. 

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/EDAReport1.PNG" align="center" width="800" />

<br>

## Data Wrangling

#### Data Wrangling Basics
Data wrangling is a vitally important aspect of this software. It's important that you know how to utilize the functionality as intended. Below are all of the available methods with descriptions about how to use each and every one for each of their intended uses.

### Data Wrangling Methods:

| Category       | Method                |
| ---------------|-----------------------|
| Shrink         | Aggregate             |
|                | Subset Rows           |
|                | Subset Columns        |
|                | Sampling              |
| Grow           | Join                  |
|                | Union                 |
| Dataset        | Partition Data        |
|                | Sort Data             |
|                | Remove Data           |
|                | Model Data Prep       |
| Pivot          | Melt Data             |
|                | Cast Data             |
| Columns        | Type Casting          |
|                | Time Trend            |
|                | Rename Columns        |
|                | Concatenate Columns   |
| Misc           | Meta Programming      |
|                | Time Series Fill      |
|                | Time Series Roll Fill |

<br>

## Feature Engineering

#### Feature Engineering Basics
Feature Engineering is a vitally important aspect of this software. It's important that you know how to utilize the functionality as intended. Below are all of the available methods with descriptions about how to use each and every one for each of their intended uses.

### Feature Engineering Methods:

| Category     | Method                   |
| -------------| -------------------------|
| Numeric      | Percent Rank             |
|              | Standardize              |
|              | Transformations          |
|              | Interaction              |
| Categorical  | Character Encoding       |
|              | Partial Dummies          |
| Calendar     | Calendar Variables       |
|              | Holiday Variables        |
| Windowing    | Rolling Numeric          |
|              | Differencing             |
|              | Rolling Categorical      |
| Text         | Word2Vec                 |
|              | Text Summary             |
|              | Sentiment                |
|              | Readability              |
|              | Lexical Diversity        |
| Unsupervised | Clustering               |
|              | Anomaly Detection        |
|              | Dimensionality Reduction |


<br>

## Inference

#### Inference Basics
Inference is a vitally important aspect of this software. It's important that you know how to utilize the functionality as intended. Below are all of the available methods with descriptions about how to use each and every one for each of their intended uses.

### Inference Methods:
1. Normality Testing
2. Correlation Testing
3. One-Sample T-Test
4. Two-Sample T-Test
5. F-Test
6. Chi-Square Test

## Inference Reporting
The Inference Reports are dependent upon the inference method chosen. They all return summary statistics and visuals to help assess effects and assumptions. 

##### Normality Report
<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Inference_Normality1.PNG" align="center" width="400" /> <img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Inference_Normality2.PNG" align="center" width="400" />

<br>

##### Correlation Report
<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Inference_Correlation1.PNG" align="center" width="400" /> <img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Inference_Correlation2.PNG" align="center" width="400" />

<br>

##### One Sample T-Test Report
<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Inference_OneSampleTTest1.PNG" align="center" width="400" /> <img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Inference_OneSampleTTest2.PNG" align="center" width="400" />

<br>

##### Two Sample T-Test Report
<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Inference_TwoSampleTTest1.PNG" align="center" width="400" /> <img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Inference_TwoSampleTTest2.PNG" align="center" width="400" />

<br>

##### F-Test Report
<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Inference_FTest1.PNG" align="center" width="400" /> <img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Inference_FTest2.PNG" align="center" width="400" />

<br>

##### Chi-Square-Test Report
<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Inference_ChiSquareTest1.PNG" align="center" width="400" /> <img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/Inference_ChiSquareTest2.PNG" align="center" width="400" />

<br>

## Machine Learning

ML is a vitally important aspect of this software. It's important that you know how to utilize the functionality as intended. The documentation in-app contains information on each of the ML Algo types.

Currently available algorithms include:
1. CatBoost
2. XGBoost
3. LightGBM
4. H2O-DRF
5. H2O-GBM
6. H2O-GLM
7. H2O-HGLM
8. Causal Mediation

Some of the built-in features include:
* Automatic Transformations and backtransformations if user requests
* Data partitioning for train, validation, and test data sets if the user only supplies a training data set
* Categorical variable encoding and backtransform if the user supplies categorical variables as features
* Computation of model metrics for evaluation
* Data conversion to the structure appropriate for the given algorithm selected
* Multi-arm bandit grid tuning

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/ML1.PNG" align="center" width="400" /> <img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/ML2.PNG" align="center" width="400" />

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/ML3.PNG" align="center" width="400" /> <img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/ML4.PNG" align="center" width="400" />

## Machine Learning Reports
The ML Evaluation Report can be generated by clicking the Save button on the ML Output Panel either before or after generating the ML info in app. 

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/MLReport.PNG" align="center" width="800" />

<br> 

## Forecasting

Forecasting is a vitally important aspect of this software. It's important that you know how to utilize the functionality as intended. The documentation in-app contains information on each of the Forecasting Algo types.

Currently available algorithms can be split into Single Series and Panel Series:

### Single Series Forecasting
1. TBATS
2. SARIMA
3. ETS
4. ARFIMA
5. NNET

#### Single Series Run Modes
1. Grid Tuning
2. Forecasting

### Panel Series Forecasting
1. CatBoost
2. XGBoost
3. LightGBM

#### Panel Forecasting Run Modes
There are various Run modes to train, backtest, and forecast:

##### Training Options
1. <b>Train Model:</b> This is equivalent to building an ML model
2. <b>Retrain Existing Model:</b> This is for retraining a model that's already been built before. Perhaps you simply want an updated model but not a new forecast at the moment

##### Backtesting Options
1. <b>Backtest:</b> This task will train a new model (if FC ArgsList is not supplied) and generate an N-Period ahead forecast that will be evaluated using Validation Data supplied by the user. If you don't have a Validation dataset, go to Data Wrangling and subset rows based on a time variable. The subset data will be your Training Data and your original dataset will be the Validation Data
2. <b>Backtest Cross Evaluation:</b> Once you have a good model designed you can mock production by running this procedure. Here, you'll set the data refresh rate and the model update rate. Performance measure are returned in a data.table once the procedure is finished.
3. <b>Feature Engineering Test:</b> This task will loop through various builds starting from the most simple up to a moderately sophisticated model. An evaluation table is generated that you can view in the Tables tab when the procedure is complete. Evaluation metrics are based on the Backtest method. Features tested are below and are in order. If a feature is beneficial it will remain in the models trained thereafter:
> LogPlus1 vs None: this will test whether a target variable transformation is beneficial

> Series Difference vs None: this will test whether utilizing Differencing your series is useful

> Calendar Variables vs None: this will test whether utilizing Calendar Variables is useful

> Holiday Variable vs None: this will test whether utilizing Holiday Variables is useful

> Credibility vs Target Encoding : this will test whether a target encoding is better than a credibility encoding

> Time Weights vs None: this will test whether utilizing Time Weighting is useful

> Anomaly Detection vs None: this will test whether utilizing Anomaly Detection is useful

> Time Trend Variable vs None: this will test whether utilizing a Time Trend Variable is useful

> Lag 1 vs None: this will test whether utilizing Lags are useful

##### Forecasting Options
1. <b>Forecast:</b> if you have a trained model you can call it to generate a forecast for you
2. <b>Retrain + Forecast:</b> if you have a model you can refresh it and have it generate a forecast for you

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/PanelFC_Report1.PNG" align="center" width="800" />

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/PanelFC_Report2.PNG" align="center" width="800" />

## Forecast Reports
The FC Evaluation Report can be generated by clicking the Save button on the FC Output Panel either before or after generating the FC info in app. 

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/FCReport1.PNG" align="center" width="800" />

<br> 
