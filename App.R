

library(shiny)
library(dplyr)
library(tidyr)
library(lubridate)
library(DT)
library(ggplot2)

ConfirmedPath = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
DeathPath = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

ui <- fluidPage(

  titlePanel("Corona Confirmed Cases"),
  sidebarLayout(
    sidebarPanel(
      helpText("Choose Confirmed Cases or Number of Deaths."),
      selectInput("var", 
                  label = "Choose:",
                  choices = c("Confirmed Count", "Death Count"),
                  selected = "Confirmed Count"),
      uiOutput("country_selector")
    ),
 
   mainPanel(   
      dataTableOutput(head("table1")),
      plotOutput("plot1")
   )
   

)
)

server <- function(input, output, session) {

  fileData <- reactiveFileReader(1000, NULL, 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
, read.csv)
  
  deathData <- reactiveFileReader(1000, NULL, 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
                                  , read.csv)
  
    cleandata <- reactive({
      data <- switch(input$var, 
                     "Confirmed Count" = fileData,
                     "Death Count" = deathData)
      df <- data()

      for ( col in 1:ncol(df)){
        colnames(df)[col] <-  sub("X*", "", colnames(df)[col])
      }
      c_data <- df %>% rename(Province = "Province.State", 
                                          Country = "Country.Region") %>% 
                          pivot_longer(-c(Province, Country, Lat, Long), 
                                       names_to = "Date", values_to = "cumulative_cases") %>% 
                          mutate(Date= mdy(Date)) %>% 
                          arrange(Country,Province, Date) %>% 
                          group_by(Country) 
    })
    output$country_selector = renderUI({ #creates State select box object called in ui
      selectInput(inputId = "ctry", #name of input
                  label = "Country:", #label displayed in ui
                  choices = as.character(unique(cleandata()$Country)),
                  # calls unique values from the State column in the previously created table
                  selected = "All") #default choice (not required)
    })
  
   cum_data <- reactive({d <- cleandata()   
     c <- d %>% select(Country,cumulative_cases,Date) %>%
       group_by(Country) %>%
       filter(Date==max(Date)) %>%
       summarize(Confirmed=sum(cumulative_cases)) %>%
       arrange(desc(Confirmed))})
# 
    plot_country<- reactive({d <- cleandata()   
    c <- d %>% 
      filter(Country==input$ctry) %>% 
      arrange(Province, Date) 
    })
  
    output$table1 <- renderDataTable(head(cum_data()))
    output$plot1 <- renderPlot( ggplot(plot_country(),aes(x=Date ,y=cumulative_cases))+ geom_bar(stat="identity"))
#    

  }
 shinyApp(ui, server)