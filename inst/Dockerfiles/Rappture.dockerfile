FROM rocker/shiny:4.3

# Get linux up to date
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Something
ENV _R_SHLIB_STRIP_=true

# It's free real estate!
RUN echo 'Its free real estate'

# Install packages
RUN install2.r --error --skipinstalled \
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
    shiny \
    shinydashboard \
    shinydashboardPlus \
    AzureStor \
    shinyWidgets \
    gitlink \
    arrow \
    reactable \
    DT \
    shiny \
    shinyjs \
    shinyjqui \
    shinyAce \
    prompter \
    gyro \
    arrangements \
    echarts4r \
    tidytext \
    tibble \
    stopwords \
    SentimentAnalysis \
    quanteda \
    datamods \
    phosphoricons

# Install github packages
RUN R -e "devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')"
RUN R -e "devtools::install_github('JohnCoene/echarts4r.assets', upgrade = FALSE, dependencies = FALSE, force = TRUE)"
RUN R -e "devtools::install_github('AdrianAntico/AutoNLP', upgrade = FALSE, dependencies = FALSE, force = TRUE)"
RUN R -e "devtools::install_github('AdrianAntico/AutoPlots', upgrade = FALSE, dependencies = FALSE, force = TRUE)"
RUN R -e "devtools::install_github('AdrianAntico/Rodeo', upgrade = FALSE, dependencies = FALSE, force = TRUE)"
RUN R -e "devtools::install_github('AdrianAntico/AutoQuant', upgrade = FALSE, dependencies = FALSE, force = TRUE)"
RUN R -e "devtools::install_github('AdrianAntico/esquisse', upgrade = FALSE, dependencies = FALSE, force = TRUE)"
RUN R -e "devtools::install_github('AdrianAntico/DataMuse', upgrade = FALSE, dependencies = FALSE, force = TRUE, auth_token = 'ghp_eHRZjqviiYfD4r0uTdSQs9913qD7kG2VNO6e')"

# Copy in credentials for azure
COPY AutoPlotterCreds.csv .

# Run app
CMD /bin/bash R -e "options('shiny.port'=3838,shiny.host='0.0.0.0'); library(AutoQuant); DataMuse::Muse(MaxPlots = 24L,2L)"
