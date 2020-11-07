## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(openxlsx)
library(RCurl)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)
library(gganimate)
library(highcharter)
library(magrittr)

# Import data
# Confirmed
cases_global_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv" # auf Raw klicken
confirmed <- data.table::fread(cases_global_link)

confirmed <- confirmed %>%
  select(-c(`Province/State`, Lat, Long)) %>%
  group_by(`Country/Region`) %>%
  summarise_all(list(sum)) 

confirmed$`Country/Region` <- countrycode::countrycode(df$`Country/Region`, origin = "country.name", destination = "iso3c")
confirmed <- df %>%
  filter(!is.na(`Country/Region`))
colnames(confirmed)[1] <- "iso3"


confirmed_lag <- confirmed[2:ncol(confirmed)] %>%
  t() %>%
  as.data.frame() %>%
  mutate_if(~is.numeric(.x), ~(.x - lag(.x, default = 0))) %>%
  t() %>%
  as.data.frame() %>%
  setNames(colnames(confirmed)[2:ncol(confirmed)])

confirmed_lag$iso3 <- confirmed$iso3
confirmed_lag <- confirmed_lag %>%
  select(iso3, everything())

# Deaths
deaths_global_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv" # auf Raw klicken
deaths <- data.table::fread(deaths_global_link)

deaths <- deaths %>%
  select(-c(`Province/State`, Lat, Long)) %>%
  group_by(`Country/Region`) %>%
  summarise_all(list(sum)) 

deaths$`Country/Region` <- countrycode::countrycode(df$`Country/Region`, origin = "country.name", destination = "iso3c")
deaths <- df %>%
  filter(!is.na(`Country/Region`))
colnames(deaths)[1] <- "iso3"


deaths_lag <- deaths[2:ncol(deaths)] %>%
  t() %>%
  as.data.frame() %>%
  mutate_if(~is.numeric(.x), ~(.x - lag(.x, default = 0))) %>%
  t() %>%
  as.data.frame() %>%
  setNames(colnames(deaths)[2:ncol(deaths)])

deaths_lag$iso3 <- deaths$iso3
deaths_lag <- deaths_lag %>%
  select(iso3, everything())

### User Interface

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)