library(tidyverse)
library(data.table)
library(openxlsx)
library(RCurl)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)

# Read in all data

cases_global_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv" # auf Raw klicken
weather_data_link <- "http://databank.worldbank.org/data/download/catalog/cckp_historical_data_0.xls"

tf = tempfile(fileext = ".xls")
curl::curl_download(weather_data_link, tf)

df <- data.table::fread(cases_global_link)
weather_df <- readxl::read_excel(tf, sheet = 4)

## EDA

globalcolnames <- colnames(df)[5:length(colnames(df))]

df <- df %>%
  select(-c(`Province/State`, Lat, Long)) %>%
  group_by(`Country/Region`) %>%
  summarise_all(list(sum)) %>%
  t() %>%
  as.data.frame()

colnames(df) <- df[1, ]
df <- df[-1, ]

dates <- rownames(df)

df <- data.frame(lapply(df, as.numeric))
df <- df %>%
  mutate_if(~is.numeric(.x), ~(.x - lag(.x, default = 0)))
df$dates <- as.Date(dates, "%m/%d/%Y")


#### EDA und Vis

ggplot(data=df, aes(x=dates, y=Germany, group=1)) +
  geom_line(color="red")


by_month <- df %>%
  group_by(month = floor_date(dates, "month")) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)
by_month

w_df <- weather_df %>%
  select(-Annual_temp) %>%
  t() %>%
  as.data.frame()
colnames(w_df) <- w_df[1, ]
w_df <- w_df[-1, ]


by_month$month[4:10]



comparison <- data.frame(by_month$month[4:10], c(scale(as.numeric(w_df$DEU)[4:10])), c(scale(by_month$Germany[4:10]))) %>%
  setnames(c("Date", "Temperature", "Cases"))

cor.test(comparison$Temperature,comparison$Cases,method="spearman")

comparison <- comparison %>%
  melt(id="Date")


ggplot(data=comparison, aes(x=Date, y=value, colour=variable)) + geom_line()

