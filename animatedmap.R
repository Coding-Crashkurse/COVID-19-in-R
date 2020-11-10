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

res


  
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
releativepop_cases <- merged %>%
  mutate_if(~is.numeric(.x), ~(.x / pop) * 100000) %>%
  select(-pop)


df_long_cases <- pivot_longer(releativepop_cases, cols = colnames(releativepop_cases)[2:ncol(releativepop_cases)], names_to = "sm", values_to = "value") %>%
  mutate(value = round(value, 4))

my_ds <- df_long_cases %>% 
  group_by(`iso3`) %>% 
  do(item = list(
    `iso3` = first(.$`iso3`),
    sequence = .$value,
    value = first(.$value))) %>% 
  .$item


highchart(type = "map") %>% 
  hc_add_series(data = my_ds,
                mapData = worldgeojson,
                joinBy = "iso3",
                borderWidth = 0.01) %>% 
  hc_colorAxis(dataClasses =  color_classes(breaks = c(0, 2, 4, 7, 14, 1000), colors = c("darkgreen", "yellow", "red", "darkred"))) %>%
  hc_title(text = "Covid 19 per Day") %>% 
  hc_legend(layout = "vertical", reversed = TRUE,
            floating = TRUE, align = "right") %>% 
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_tooltip(useHTML = TRUE, headerFormat = "",
             pointFormat = "Infections in {point.name}: {point.value}") %>%
  hc_motion(
    enabled = TRUE,
    axisLabel = "year",
    labels = unique(as.character(colnames(df))),
    series = 0,
    updateIterval = 5,
    magnet = list(
      round = "floor",
      step = 0.05
    )
  )

### Death cases

deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
df <- data.table::fread(deaths)

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
  mutate_if(~is.numeric(.x), ~(ifelse(.x) < 0, 0, .x)) %>%
  t() %>%
  as.data.frame() %>%
  setNames(colnames(df)[2:ncol(df)])
res$iso3 <- df$iso3
res <- res %>%
  select(iso3, everything())
res

### Populationdata

pop_link <- "http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=excel"

tf = tempfile(fileext = ".xls")
curl::curl_download(pop_link, tf)

pop <- readxl::read_excel(tf, sheet = 1, skip = 2)  
pop <- pop %>%
  select(`Country Code`, `2019`) %>%
  setNames(c("iso3", "pop"))


merged <- merge(res, pop, by="iso3")
merged

releativepop_deaths <- merged %>%
  mutate_if(~is.numeric(.x), ~(.x / pop) * 1000000) %>%
  mutate_if(~is.numeric(.x), ~(ifelse(.x) < 0, 0, .x)) %>%
  select(-pop)
releativepop_deaths

df_death_long <- pivot_longer(releativepop_deaths, cols = colnames(releativepop_deaths)[2:ncol(releativepop_deaths)], names_to = "sm", values_to = "value") %>%
  mutate(value = round(value, 4))


my_ds <- df_death_long  %>% 
  group_by(`iso3`) %>% 
  do(item = list(
    `iso3` = first(.$`iso3`),
    sequence = .$value,
    value = first(.$value))) %>% 
  .$item


highchart(type = "map") %>% 
  hc_add_series(data = my_ds,
                mapData = worldgeojson,
                joinBy = "iso3",
                borderWidth = 0.01) %>% 
  hc_colorAxis(dataClasses =  color_classes(breaks = c(0, 1, 3, 5, 10, 1000), colors = c("darkgreen", "yellow", "red", "darkred"))) %>%
  hc_title(text = "Covid 19 per Day") %>% 
  hc_legend(layout = "vertical", reversed = TRUE,
            floating = TRUE, align = "right") %>% 
  hc_add_theme(hc_theme_smpl()) %>% 
  hc_tooltip(useHTML = TRUE, headerFormat = "",
             pointFormat = "Death by million in {point.name}: {point.value}") %>%
  hc_motion(
    enabled = TRUE,
    axisLabel = "year",
    labels = unique(as.character(df_death_long$sm)),
    series = 0,
    updateIterval = 5,
    magnet = list(
      round = "floor",
      step = 0.05
    )
  )


### Scatterplot
head(df_death_long)

df_ges <- cbind(df_death_long, df_long_cases$value) %>%
  setNames(c("iso3", "date", "deaths", "cases"))
df_ges %>%
  arrange(cases) %>%
  head()

range(df_ges$cases, na.rm = TRUE)
range(df_ges$deaths, na.rm = TRUE)


data_seqc <- df_ges %>%
  arrange(iso3, date) %>%
  group_by(iso3) %>%
  do(sequence = list_parse(select(., x = cases, y = deaths)))


highchart() %>% 
  hc_add_series(data = data_seqc, type = "scatter") %>%
  hc_motion(enabled = TRUE, series = 0, labels = unique(df_ges$date),
            loop = TRUE, autoPlay = TRUE, 
            updateInterval = 1000, magnet = list(step =  1)) %>% 
  hc_plotOptions(series = list(showInLegend = FALSE)) %>% 
  hc_yAxis(min = 0, max = 30) %>% 
  hc_xAxis(min = 0, max = 30) %>% 
  hc_add_theme(hc_theme_smpl())
