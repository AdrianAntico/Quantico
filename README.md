# Quantico

![Version:0.1.0](https://img.shields.io/static/v1?label=Version&message=0.1.0&color=blue&?style=plastic)
![Build:Passing](https://img.shields.io/static/v1?label=Build&message=passing&color=brightgreen)
[![Maintenance](https://img.shields.io/badge/Maintained%253F-yes-green.svg)](https://GitHub.com/Naereen/StrapDown.js/graphs/commit-activity)
[![PRsWelcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg?style=default)](http://makeapullrequest.com)
[![GitHubStars](https://img.shields.io/github/stars/AdrianAntico/ShinyDS.svg?style=social)](https://github.com/AdrianAntico/ShinyDS)

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/LogoWhite.png" align="center" width="800" />

Quantico is a GUI for Visualization, Data Wrangling, Feature Engineering, NLP, Machine Learning, Statistical Modeling, Forecasting, and Code Generation.

## Installation

Note: if you're only looking to update Quantico, you only have to reinstall the Quantico package below.

Step 1: install the "R-release" version of **rtools** and have it placed in your C:\ drive https://cran.r-project.org/bin/windows/Rtools/

Step 2: install **R** https://cran.r-project.org/bin/windows/base/

Step 3: install **RStudio Desktop** https://posit.co/download/rstudio-desktop/

Step 4: Install dependencies and Quantico two ways:

1. Download DataMuse.rar, open it up, go into the DataMuse folder, and then run the installer, "Datamuse_installer_Win10x64"

2. Open up an RStudio session and run the following installation commands from the list of package below. It's best to go one by one in case there is an issue installing any one of them

<details><summary> List of packages to install by hand </summary>

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

Easy start (no PostGRE, no Azure Blob access)

```r
# Optionally, you can change up the WorkingDirectory argument for your desired file path location
Quantico::runQuantico(
  MaxTabs = 2L, 
  WorkingDirectory = getwd())
```

If you have a PostGRE installation you can add in the PostGRE parameters

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

## Plotting Page

In the plotting panel you simply click on the top buttons (e.g. Plot 1) and select a plot from the dropdown menu. Then you click the button below to fill out the necessary parameters for your plot. Lastly, drop the newly created box in the dragula pane and move it to the bottom row in order for it to display.

<img src="https://github.com/AdrianAntico/Quantico/blob/main/inst/AppMain.PNG" align="center" width="800" />
