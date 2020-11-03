library(tidyverse)
library(data.table)
library(openxlsx)
library(RCurl)
library(readr)
library(readxl)
library(lubridate)
library(ggplot2)
library(countrycode)


# Read in all data

cases_global_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv" # auf Raw klicken
weather_data_link <- "http://databank.worldbank.org/data/download/catalog/cckp_historical_data_0.xls"

tf = tempfile(fileext = ".xls")
curl::curl_download(weather_data_link, tf)

df <- data.table::fread(cases_global_link)
df <- df %>%
  select(-c(`Province/State`, Lat, Long)) %>%
  group_by(`Country/Region`) %>%
  summarise_all(list(sum)) 
colnames(df)[1] <- "country"

weather_df <- readxl::read_excel(tf, sheet = 4)

weather_df$country <- countrycode::countrycode(weather_df$ISO_3DIGIT, origin = "iso3c", destination = "country.name")
weather_df <- weather_df %>%
  select(-ISO_3DIGIT, -Annual_temp) %>%
  select("country", everything())

both <- intersect(df$country, weather_df$country)

df <- df %>%
  filter(country %in% both)

weather_df <- weather_df %>%
  filter(country %in% both)




## EDA

df <- df %>%
  select(-country) %>%
  t() %>%
  as.data.frame() %>%
  setNames(df$country) %>%
  mutate_all(as.numeric) %>%
  mutate(date = as.Date(colnames(df)[2:ncol(df)], "%m/%d/%Y")) %>%
  mutate_if(~is.numeric(.x), ~(.x - lag(.x, default = 0))) %>%
  mutate_if(is.numeric, scale)
  

weather_df <- weather_df %>%
  select(-country) %>%
  t() %>%
  as.data.frame() %>%
  setNames(weather_df$country) %>%
  mutate_all(as.numeric) %>%
  mutate(date = as.Date(colnames(weather_df)[2:ncol(weather_df )], "%m/%d/%Y")) %>%
  mutate_if(is.numeric, scale)

by_month <- df %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE)
by_month <- by_month[4:11,]

weather_df <- weather_df[4:11, ]

resultlist = list()

for(col in seq_along(df)) {
  resultlist[[col]] <- cor.test(by_month[[col]], weather_df[[col]], method = "pearson")
}

by_month$month

cor.test(df$Afghanistan, weather_df$Afghanistan)

#### EDA und Vis

ggplot(data=df, aes(x=dates, y=Germany, group=1)) +
  geom_line(color="red")




comparison <- comparison %>%
  melt(id="Date")

ggplot(data=comparison, aes(x=Date, y=value, colour=variable)) + geom_line()

