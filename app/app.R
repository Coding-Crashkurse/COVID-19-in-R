## app.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(openxlsx)
library(RCurl)
library(readr)
library(readxl)
library(highcharter)
library(shinycssloaders)

# Import data
# Confirmed
cases_global_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv" # auf Raw klicken
confirmed <- data.table::fread(cases_global_link)

confirmed <- confirmed %>%
  select(-c(`Province/State`, Lat, Long)) %>%
  group_by(`Country/Region`) %>%
  summarise_all(list(sum)) 

confirmed$`Country/Region` <- countrycode::countrycode(confirmed$`Country/Region`, origin = "country.name", destination = "iso3c")
confirmed <- confirmed %>%
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


deaths$`Country/Region` <- countrycode::countrycode(deaths$`Country/Region`, origin = "country.name", destination = "iso3c")
deaths <- deaths %>%
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

# Population
pop_link <- "http://api.worldbank.org/v2/en/indicator/SP.POP.TOTL?downloadformat=excel"

tf = tempfile(fileext = ".xls")
curl::curl_download(pop_link, tf)

pop <- readxl::read_excel(tf, sheet = 1, skip = 2)  
pop <- pop %>%
  select(`Country Code`, `2019`) %>%
  setNames(c("iso3", "pop"))


### User Interface

ui <- dashboardPage(
  dashboardHeader(title = "COVID-19 Dashboard"),
  dashboardSidebar(width = 0),
  dashboardBody(includeCSS("www/style.css"),
    fluidRow(
      box(width = 6,
        selectizeInput("variable", "Infections/Deaths", choices = c("Infections", "Deaths"), selected = "Infections", multiple = FALSE)
      ),
      box(width = 12,
        shinycssloaders::withSpinner  (highchartOutput("chart", height = "800px"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  relative_df <- reactive({
    
    if(input$variable == "Infections") {
      
      merged <- merge(confirmed_lag, pop, by="iso3")
      relativedf <- merged %>%
        mutate_if(~is.numeric(.x), ~(.x / pop) * 100000) %>%
        select(-pop)
  
    } else {
      
      merged <- merge(deaths_lag, pop, by="iso3")
      relativedf <- merged %>%
        mutate_if(~is.numeric(.x), ~(.x / pop) * 1000000) %>%
        select(-pop)
      
    }
    
    df_long <- pivot_longer(relativedf, cols = colnames(relativedf)[2:ncol(relativedf)], names_to = "sm", values_to = "value") %>%
      mutate(value = round(value, 4))
    
    my_ds <- df_long %>% 
      group_by(`iso3`) %>% 
      do(item = list(
        `iso3` = first(.$`iso3`),
        sequence = .$value,
        value = first(.$value))) %>% 
      .$item
    
    return(list(my_ds, df_long))
    
  })
  
  output$chart <- renderHighchart({
    
    # assign("relative", relative_df(), envir = globalenv())
    
    # relative[2]
    
    hc <- highchart(type = "map") %>% 
      hc_add_series(data = relative_df()[[1]],
                    mapData = worldgeojson,
                    joinBy = "iso3",
                    borderWidth = 0.01) %>%
      hc_legend(layout = "vertical", reversed = TRUE, floating = TRUE, align = "right")
    
    if(input$variable == "Infections") {
      hc <- hc %>%
        hc_colorAxis(dataClasses =  color_classes(breaks = c(0, 2, 4, 7, 14, 1000), colors = c("darkgreen", "yellow", "red", "darkred"))) %>%
        hc_title(text = "Covid 19 infections per day relative to population in 100k") %>% 
        hc_add_theme(hc_theme_smpl()) %>% 
        hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = "Infections in {point.name}: {point.value}")
    } else {
      hc <- hc %>%
        hc_colorAxis(dataClasses =  color_classes(breaks = c(0, 2, 4, 7, 14, 1000), colors = c("darkgreen", "yellow", "red", "darkred"))) %>%
        hc_title(text = "Covid 19 Deaths per day relative to population in Mio.") %>% 
        hc_add_theme(hc_theme_smpl()) %>% 
        hc_tooltip(useHTML = TRUE, headerFormat = "", pointFormat = "Deaths in {point.name}: {point.value}")
    }

    hc %>%
      hc_motion(
        enabled = TRUE,
        axisLabel = "year",
        labels = unique(as.character(relative_df()[[2]]$sm)),
        series = 0,
        updateIterval = 5,
        magnet = list(
          round = "floor",
          step = 0.05
        )
      )
    
  })
  
}

shinyApp(ui, server)