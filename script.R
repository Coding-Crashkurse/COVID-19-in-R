library(tidyverse)
library(data.table)
library(RCurl)
library(readxl)
library(ggplot2)
library(lubridate)
library(countrycode)


# Read in all data

cases_global_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv" # auf Raw klicken
weather_data_link <- "http://databank.worldbank.org/data/download/catalog/cckp_historical_data_0.xls"

tf = tempfile(fileext = ".xls")
curl::curl_download(weather_data_link, tf)

df <- data.table::fread(cases_global_link)


latitude_df <- df %>%
  select(`Country/Region`, Lat) %>%
  filter(!duplicated(`Country/Region`)) %>%
  setNames(c("country", "lat"))
  

df <- df %>%
  select(-c(`Province/State`, Lat, Long)) %>%
  group_by(`Country/Region`) %>%
  summarise_all(sum) 
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
  mutate_if(~is.numeric(.x), ~(.x - lag(.x, default = 0)))
  

weather_df <- weather_df %>%
  select(-country) %>%
  t() %>%
  as.data.frame() %>%
  setNames(weather_df$country) %>%
  mutate_all(as.numeric) %>%
  mutate(date = as.Date(colnames(weather_df)[2:ncol(weather_df )], "%m/%d/%Y"))

by_month <- df %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise_if(is.numeric, sum, na.rm = TRUE) %>%
  mutate_if(is.numeric, scale)
by_month <- by_month[4:10,] %>%
  select(-month) %>%
  select(sort(current_vars()))

weather_df <- weather_df[4:10, ] %>%
  select(-date) %>%
  select(sort(current_vars()))


all(colnames(by_month) == colnames(weather_df))

ggplot(df, aes(x =date)) + 
  geom_line(aes(y = Germany), color="steelblue", size = 1.5) +
  geom_line(aes(y = India), color="red", size = 1.5)

ggplot(df, aes(x =date)) + 
  geom_line(aes(y = scale(Germany)), color="steelblue", size = 1.5) +
  geom_line(aes(y = scale(India)), color="red", size = 1.5)


resultlist = list()
for(col in seq_along(by_month)) {
  resultlist[[col]] <- as.numeric(cor.test(by_month[[col]], weather_df[[col]], method = "pearson")$estimate)
  names(resultlist)[[col]] <- colnames(by_month)[[col]]
}



corr_df <- data.frame(unlist(resultlist), colnames(by_month)) %>%
  setNames(c("corr", "country"))
head(corr_df)
head(latitude_df)

merged_df <- merge(corr_df, latitude_df, by="country")
merged_df$category <- ifelse(merged_df$lat > 23.5, "North", ifelse(merged_df$lat <= 23.5 & merged_df$lat >=-23.5, "Equator", "South"))


merged_df %>%
  group_by(category) %>%
  summarise(mean_corr = mean(corr, na.rm = TRUE))

## Visualizing data
library(ggplot2)

ggplot(merged_df, aes(x = reorder(country, -corr), y = corr, color=category)) +
  geom_bar(stat="identity")

library(highcharter)


cor.test(by_month$Afghanistan, weather_df$Afghanistan , method = "pearson")$estimate

cor.test(by_month$Germany, weather_df$Germany, method = "pearson")$estimate

library(ggplot2)

head(merged_df)

ggplot(data = merged_df, aes(x = Lat , y = corr)) + 
  geom_point(color='red') +
  geom_smooth(method = "lm", se = FALSE, formula = y~x)

cor.test(merged_df$corr, merged_df$Lat)

