# Quantico

![Version:1.0.0](https://img.shields.io/static/v1?label=Version&message=1.0.0&color=blue&?style=plastic)
![Build:Passing](https://img.shields.io/static/v1?label=Build&message=passing&color=brightgreen)
[![PRsWelcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=default)](http://makeapullrequest.com)
[![GitHubStars](https://img.shields.io/github/stars/AdrianAntico/ShinyDS.svg?style=social)](https://github.com/AdrianAntico/ShinyDS)

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/LogoWhite.png" align="center" width="800" />

Quantico is a GUI for Visualization, Data Wrangling, Feature Engineering, NLP, Machine Learning, Statistical Modeling, Forecasting, Reporting, and Code Generation. 

Automated output reports are included for:
1. EDA
2. Inference
3. Machine Learning
4. Forecasting

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


## Quickstart

In your RStudio session, run the Quantico::Muse() function to kick off a Quantico session

Easy start

```r
# Optionally, you can change up the WorkingDirectory argument for your desired file path location
Quantico::runQuantico(WorkingDirectory = getwd())
```

If you have a PostGRE installation you can add in the PostGRE parameters (or just pass them in while in session)

```r
# Optionally, you can change up the WorkingDirectory argument for your desired file path location (don't forget to use these "/" instead of these "\" in your path)
Quantico::runQuantico(
  MaxTabs = 2L,
  WorkingDirectory = getwd(),
  PostGRE_DBNames = NULL, # list of database names you want connected
  PostGRE_Host = 'localhost',
  PostGRE_Port = 54321,
  PostGRE_User = '...',
  PostGRE_Password = '...')
```

## App Interface

Below is a subset of the various themes to choose from

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/AppMain.PNG" align="center" width="400" /> <img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/AppMain2.PNG" align="center" width="400" /> 

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/AppMain3.PNG" align="center" width="400" /> <img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/AppMain4.PNG" align="center" width="400" />

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/AppMain5.PNG" align="center" width="400" /> <img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/AppMain6.PNG" align="center" width="400" />

## Plotting Page

#### Plotting Basics
Plotting is a vitally important aspect of this software. It's important that you know how to utilize the functionality as intended. One of the goals is to make plotting as easy as possible. You don't have to pre-aggrgate or prepare your data for plotting (although it can be). Just pass it in and utilize the inputs to tell the software what you want.

### Plot types:

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

#### Build and display a plot:
In the plotting panel you simply click on the top buttons (e.g. Plot 1, Plot 2, ...) and select a plot type from the dropdown menu. Then you click the button below to fill out the necessary parameters for your plot. Lastly, drop the newly created box in the dragula pane and move it to the bottom row in order for it to display.

When you click the button below the plot type dropdown, a modal will appear with up to five tabs for inputs and selections:
1 Data Selection Tab
2 Axis Variables Tab
3 Grouping Variables Tab (in most cases but not all)
4 Filter Variables Tab
5 Formatting Tab

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
