
options(shiny.maxRequestSize = 30*1024^2)

### load packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(psych)
library(glue)
library(scales)

### load data (NOTE: the full path is needed)

#setwd("C:/Users/LucaK/Desktop/Uni Luzern/Master/R_RStudio_UniLU")

# europe <- read_rds("C:/Users/LucaK/Desktop/Uni Luzern/Master/R_RStudio_UniLU/Web_Applications_unsing_Shiny/01_data/europe.rds") %>% 
#   # create Month and AvgTemperatureC
#   mutate(Month = month(Month,label = TRUE),
#          AvgTemperatureC = (AvgTemperatureF - 32) / 1.8)


# User Interface (Front End) ----------------------------------------------

# Define user interface for application
# => lay out the appearance of your app

ui <- fluidPage(
  
  
  ########## select a theme ########## 
  theme = shinytheme("cosmo"),
  
  
  ########## application title ########## 
  titlePanel(strong("Demo Shiny App")),
  
  
  
  ########## application text ########## 
  tabPanel(
    
    title = "",
    hr(),
    p("This is a", strong("demo shiny app"), ". It is created during the course demonstartions and 
      explanations using the demo data: temperature in Europe cities."),
    hr()
  ),
  
  
  ########## SIDEBAR ########## 
  sidebarLayout(
    
    
    # SLIDERBAR PANEL
    # define the sidebar panel 
    sidebarPanel(
      
      p(strong("Please upload a file, choose your options and hit GO!")),
      
      
      fileInput(inputId = "file",
                label = "Upload a file (RDS",
                multiple = FALSE,
                accept = (".rds")),
      
      
      # add a drop down
      selectInput(inputId = "countries",
                  label = "Countries",
                  choices = NULL,
                  multiple = TRUE,
                  # pre-selection of an option
                  selected = NULL),
      
      
      # add another drop down
      selectInput(inputId = "cities",
                  label = "Cities",
                  choices = NULL,
                  multiple = TRUE,
                  selected = NULL),
      
      
      # add a slider input
      sliderInput(inputId = "year",
                  label = "Year",
                  min = 2000, 
                  max = 2019,
                  step = 1,
                  sep = "",
                  # pre-selectuon of a value
                  value = c(2000, 2019)),
      
      
      # input (radio button)
      radioButtons(inputId = "temperature",
                   label = "Temperature",
                   choices = list("Degrees Celsius" = "AvgTemperatureC",
                                  "Degrees Fahrenheit" = "AvgTemperatureF"),
                   # pre-selection of an option
                   selected = NULL),
      
      # action button
      actionButton(inputId = "action_button",
                   label = "GO!"),
      
      br(),
      br(),
      br(),
      
      # download button
      p("You can download the data here"),
      downloadButton(outputId = "download", 
                     label = "Download")
    ),
    
    
    
    # MAIN PANEL
    # add a main panel
    mainPanel(
      
      # add some text at the top
      # "You can add text here...",
      
      # add tabs
      tabsetPanel(type = "pills",
                  
                  # Plots
                  tabPanel(title = "Plots",
                           br(),
                           # create fluidRows to split outputs
                           # fluidRows can be applied to ever element...
                           fluidRow(column(plotOutput("plot_1"), width = 5),
                                    column(plotOutput("plot_2"), width = 7)),
                           hr(),
                           fluidRow(column(plotOutput("plot_3"), width = 12)),
                           hr()),
                  
                  # Data
                  tabPanel(title = "Data",
                           br(),
                           p(strong("Take a look at the data...")),
                           br(),
                           dataTableOutput("data_table")),
                  
                  
                  # Info
                  tabPanel(title = "Info",
                           br(),
                           p(strong("summary()")),
                           verbatimTextOutput("data_summary_1"),
                           hr(),
                           p(strong("describe()")),
                           verbatimTextOutput("data_summary_2"))
      )
    )
  )
)





# Server (Back End) -------------------------------------------------------

# instructions needed to build the app
# NOTE: output is a list created by shiny. You can add stuff to this list.
server <- function(input, output, session){
  
  
  
  # reactive expressions --------------------------------------------------
  
  
  europe <- eventReactive(input$file, {
    read_rds(input$file$datapath)%>% 
      # create Month and AvgTemperatureC
      mutate(Month = month(Month,label = TRUE),
             AvgTemperatureC = (AvgTemperatureF - 32) / 1.8)
  })
  
  
  europe_df <- eventReactive(input$action_button, {
    
    europe() %>% filter(Country %in% c(input$countries),
                        City %in% c(input$cities),
                        between(Year, input$year[1], input$year[2]))
  })
  
  
  
  ### plot_1
  plot_1_df <- reactive({
    europe_df() %>% 
      group_by(City, Year) %>% 
      summarise(AvgTemp = mean(get(input$temperature))) %>% 
      ungroup() %>% 
      mutate(City = fct_reorder(City, AvgTemp,
                                .desc = TRUE))
  })
  
  ### plot_2
  plot_2_df <- reactive({
    europe_df() %>% 
      group_by(City, Month) %>%
      # get function is needed to turn the string into the object!
      summarise(AvgTemp = mean(get(input$temperature))) %>% 
      ungroup() %>% 
      mutate(City = fct_reorder(City, AvgTemp,
                                .desc = TRUE))
  })
  
  ### plot_3
  plot_3_df <- reactive({
    europe_df() %>% 
      group_by(City, Month, Day) %>% 
      # get function is needed to turn the string into the object!
      summarise(AvgTemp = mean(get(input$temperature))) 
    
  })
  
  
  
  
  # output ----------------------------------------------------------------
  
  
  # 1. output
  output$data_summary_1 <- renderPrint({
    europe_df() %>% 
      summary()
  })
  
  
  # 2. output
  output$data_summary_2 <- renderPrint({
    europe_df() %>% 
      describe()
  })
  
  
  # 3. output
  output$data_table <- renderDataTable({
    europe_df()
  })
  
  
  # 4. output
  output$plot_1 <- renderPlot({
    plot_1_df() %>% 
      ggplot(aes(Year, AvgTemp)) +
      geom_line(aes(color = City),
                size = ifelse(length(input$countries) <= 3,
                              1,
                              0.5),
                show.legend = FALSE) + 
      geom_point(aes(color = City),
                 size = ifelse(length(input$countries) <= 3,
                               2,
                               1),
                 show.legend = FALSE) +
      scale_x_continuous(breaks = pretty_breaks()) +
      theme_light() + 
      labs(title = glue("Temperature Changes over Years"),
           x = "",
           y = "Temperature")
  })
  
  
  # 5. output
  output$plot_2 <- renderPlot({
    plot_2_df() %>%
      ggplot(aes(Month, AvgTemp)) +
      geom_line(aes(color = City,
                    group = City),
                size = ifelse(length(input$countries) <= 3,
                              1,
                              0.5)) + 
      geom_point(aes(color = City),
                 size = ifelse(length(input$countries) <= 3,
                               2,
                               1)) +
      theme_light() + 
      labs(title = glue("Average Temperature during a Year"),
           x = "",
           y = "Temperature ",
           fill = glue("Average Temperature")) 
  })
  
  
  # 6. output
  output$plot_3 <- renderPlot({
    plot_3_df() %>% 
      ggplot(aes(Day, Month)) + 
      geom_tile(aes(fill = AvgTemp)) + 
      scale_fill_gradient2(low = "blue",
                           mid = "orange",
                           high = "red",
                           midpoint = ifelse(input$temperature == "AvgTemperatureF",
                                             60, 15)) + 
      theme_light() + 
      labs(title = glue("Average Temperature"),
           x = "Day",
           y = "",
           fill = glue("Average Temperature")) + 
      facet_wrap(~ City) 
  })
  
  
  # download
  
  output$download <- downloadHandler(
    filename = "temperature.csv",
    content = function(file) {
      write_csv(europe(),
                file)
    }
  )
  
  
  
  # observers ---------------------------------------------------------------
  
  # observer 1
  observe({
    
    new_country_choices <- europe() %>%
      pull(Country) %>% 
      unique()
    
    updateSelectInput(session, 
                      inputId = "countries",
                      choices = new_country_choices)
    
  })
  
  
  # observer 2
  observe({
    
    new_city_choices <- europe() %>% 
      filter(Country %in% c(input$countries)) %>% 
      pull(City) %>% 
      unique()
    
    
    updateSelectInput(session, 
                      inputId = "cities",
                      choices = new_city_choices)
    
  })
  
}




# combine ui and server to launch the app ---------------------------------

shinyApp(ui = ui,
         server = server)