FROM openanalytics/r-base

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0

# system library dependency for the app
RUN apt-get update && apt-get install -y \
    libxml2-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny','shinydashboard','tidyverse','data.table','openxlsx','RCurl','readr','readxl','highcharter','shinycssloaders'), repos='https://cloud.r-project.org/')"


# copy the app to the image
RUN mkdir /root/corona
COPY app /root/corona

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 4000

CMD ["R", "-e", "shiny::runApp('/root/corona', port = 4000, host = "0.0.0.0)"]