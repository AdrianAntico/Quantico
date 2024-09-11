FROM rocker/shiny:4.4.1

# Install system requirements for index.R as needed
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    libgl1-mesa-dev \
    libglu1-mesa \
    libglfw3 \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV _R_SHLIB_STRIP_=true
RUN echo 'Its free real estate'
RUN install2.r --error --skipinstalled \
    AutoPlots \
    devtools \
    data.table \
    collapse \
    bit64 \
    doParallel \
    foreach \
    lubridate \
    timeDate \
    combinat \
    DBI \
    e1071 \
    fBasics \
    itertools \
    MLmetrics \
    nortest \
    pROC \
    RColorBrewer \
    RPostgres \
    Rfast \
    stringr \
    xgboost \
    lightgbm \
    regmedint \
    RCurl \
    jsonlite \
    h2o \
    AzureStor \
    gitlink \
    arrow \
    reactable \
    DT \
    shiny \
    shinydashboard \
    shinyWidgets \
    shiny.fluent \
    shinyjs \
    shinyjqui \
    shinyAce \
    shinybusy \
    gyro \
    arrangements \
    echarts4r \
    tidytext \
    tibble \
    stopwords \
    SentimentAnalysis \
    quanteda \
    quanteda.textstats \
    datamods \
    phosphoricons \
    correlation

RUN R -e "devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')"
RUN R -e "devtools::install_github('AdrianAntico/prettydoc', upgrade = FALSE, dependencies = FALSE, force = TRUE)"
RUN R -e "devtools::install_github('AdrianAntico/esquisse', upgrade = FALSE, dependencies = FALSE, force = TRUE)"
RUN R -e "devtools::install_github('AdrianAntico/AutoNLP', upgrade = FALSE, dependencies = FALSE, force = TRUE)"
RUN R -e "devtools::install_github('AdrianAntico/Rodeo', upgrade = FALSE, dependencies = FALSE, force = TRUE)"
RUN R -e "devtools::install_github('AdrianAntico/RemixAutoML', upgrade = FALSE, dependencies = FALSE, force = TRUE)"
RUN R -e "devtools::install_github('AdrianAntico/Quantico', upgrade = FALSE, dependencies = FALSE, force = TRUE)"

#WORKDIR /RemixAutoAI-Dockerfile
#COPY /ShinyTesting.R $HOME
#COPY /- $HOME

CMD /bin/bash \
# && R -e "options('shiny.port'=3838,shiny.host='127.0.0.1'); library(Quantico); Quantico::runQuantico(MaxTabs = 2L, WorkingDirectory = getwd())"
&& R -e "options('shiny.port'=3838,shiny.host='0.0.0.0'); library(Quantico); Quantico::runQuantico(MaxTabs = 2L, WorkingDirectory = getwd())"
EXPOSE 3838
