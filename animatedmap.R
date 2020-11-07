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

cases_global_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv" # auf Raw klicken
df <- data.table::fread(cases_global_link)

df <- df %>%
  select(-c(`Province/State`, Lat, Long)) %>%
  group_by(`Country/Region`) %>%
  summarise_all(list(sum)) 

df$`Country/Region` <- countrycode::countrycode(df$`Country/Region`, origin = "country.name", destination = "iso3c")
df <- df %>%
  filter(!is.na(`Country/Region`))
colnames(df)[1] <- "iso3"


res <- df[2:ncol(df)] %>%
  t() %>%
  as.data.frame() %>%
  mutate_if(~is.numeric(.x), ~(.x - lag(.x, default = 0))) %>%
  t() %>%
  as.data.frame() %>%
  setNames(colnames(df)[2:ncol(df)])
res$iso3 <- df$iso3
res <- res %>%
  select(iso3, everything())




  
### Populationdata

pop_link <- "http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=excel"

tf = tempfile(fileext = ".xls")
curl::curl_download(pop_link, tf)

pop <- readxl::read_excel(tf, sheet = 1, skip = 2)  
pop <- pop %>%
  select(`Country Code`, `2019`) %>%
  setNames(c("iso3", "pop"))
pop

merged <- merge(res, pop, by="iso3")
releativepop <- merged %>%
  mutate_if(~is.numeric(.x), ~round((.x / pop) * 100000),4) %>%
  select(-pop)

releativepop

df_long <- pivot_longer(releativepop, cols = colnames(releativepop)[2:ncol(releativepop)], names_to = "sm", values_to = "value") 
df_long

my_ds <- df_long %>% 
  group_by(`iso3`) %>% 
  do(item = list(
    `iso3` = first(.$`iso3`),
    sequence = .$value,
    value = first(.$value))) %>% 
  .$item


dput(my_ds)

highchart(type = "map") %>% 
  hc_add_series(data = my_ds,
                mapData = worldgeojson,
                joinBy = "iso3",
                borderWidth = 0.01) %>% 
  #hc_colorAxis(stops = color_stops(n = 3, colors = c("darkgreen", "yellow", "darkred")), max = 50) %>%
  hc_colorAxis(dataClasses =  color_classes(breaks = c(0, 2, 4, 7, 14, 1000), colors = c("darkgreen", "yellow", "red", "darkred"))) %>%
  hc_title(text = "Covid 19 per Day") %>% 
  hc_legend(layout = "vertical", reversed = TRUE,
            floating = TRUE, align = "right") %>% 
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_motion(
    enabled = TRUE,
    axisLabel = "year",
    labels = unique(as.character(df_long$sm)),
    series = 0,
    updateIterval = 50,
    magnet = list(
      round = "floor",
      step = 0.1
    )
  )


