library(tidyverse)
library(data.table)
library(RCurl)
library(readxl)
library(ggplot2)
library(lubridate)
library(countrycode)

# Import

cases_global_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
weather_data_link <- "http://databank.worldbank.org/data/download/catalog/cckp_historical_data_0.xls"


df <- fread(cases_global_link)

tf <- tempfile(fileext = ".xls")
curl::curl_download(weather_data_link, tf)
weather_df <- readxl::read_excel(tf, sheet = 4)

latitude_df <- df %>%
  select(`Country/Region`, Lat)


df <- df %>%
  select(-c(`Province/State`, Long, Lat)) %>%
  group_by(`Country/Region`) %>%
  summarise_all(sum)
colnames(df)[1] <- "country"


weather_df$country <- countrycode::countrycode(weather_df$ISO_3DIGIT, origin = "iso3c", destination = "country.name")
weather_df <- weather_df %>%
  select(-c(ISO_3DIGIT, Annual_temp))
weather_df <- weather_df %>%
  select("country", everything())
weather_df

both <- intersect(df$country, weather_df$country)

df <- df %>%
  filter(country %in% both)

weather_df <- weather_df %>%
  filter(country %in% both)


# EDA

df <- df %>%
  select(-country) %>%
  t() %>%
  data.frame() %>%
  setNames(df$country) %>%
  mutate(date = as.Date(colnames(df)[2:ncol(df)], format = "%m/%d/%Y")) %>%
  select(date, everything())

df <- df %>%
  mutate_if(~is.numeric(.x), ~(.x - lag(.x, default = 0)))

#df$China - lag(df$China, default = 0)

ggplot(df, aes(x = date)) +
  geom_line(aes(y = Germany), color="steelblue", size = 1.5) +
  geom_line(aes(y = India), color="darkgreen", size = 1.5)


ggplot(df, aes(x = date)) +
  geom_line(aes(y = scale(Germany)), color="steelblue", size = 1.5) +
  geom_line(aes(y = scale(India)), color="darkgreen", size = 1.5)


weather_df <- weather_df %>%
  select(-country) %>%
  t() %>%
  as.data.frame() %>%
  setNames(weather_df$country) %>%
  mutate(date = colnames(weather_df)[2:ncol(weather_df)]) %>%
  select(date, everything())



df <- df %>%
  group_by(month = floor_date(date, "month")) %>%
  summarise_if(is.numeric, sum)
df <- df[4:10, ] %>%
  select(-month)


weather_df <- weather_df[4:10, ] %>%
  select(-date)

colnames(df) == colnames(weather_df)

df <- df %>%
  select(sort(current_vars()))

weather_df <- weather_df %>%
  select(sort(current_vars()))

all(colnames(df) == colnames(weather_df))


result <- cor.test(df$Germany, weather_df$Germany)$estimate
result

resultlist <- list()
for(col in seq_along(df)) {
  resultlist[[col]] <-  cor.test(df[[col]], weather_df[[col]])$estimate
}

resultlist

corr_result <- as.data.frame(cbind(unlist(resultlist), colnames(df))) %>%
  setNames(c("correlation", "country")) %>%
  filter(!is.na(correlation))

colnames(latitude_df)[1] <- "country"


latitude_df <- latitude_df %>%
  filter(!duplicated(country))

merged_df <- merge(corr_result, latitude_df, by="country")
merged_df$correlation <- as.numeric(merged_df$correlation)

all(!is.na(merged_df$correlation))

class(merged_df$correlation)

mean(merged_df$correlation)

ifelse(merged_df$Lat >23.5, "large", "small")

merged_df$category <- ifelse(merged_df$Lat > 23.5, "North", ifelse(merged_df$Lat <= 23.5 & merged_df$Lat >= -23.5, "Equator", "South"))

merged_df %>%
  group_by(category) %>%
  summarise(mean = mean(correlation, na.rm = TRUE))

ggplot(data = merged_df, aes(x = reorder(country, correlation), correlation, fill=category)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  xlab(label = "Country") + 
  ylab(label = "Correlation")

