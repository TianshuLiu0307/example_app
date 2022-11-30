library(flexdashboard)
library(tidyverse)
library(plotly)
library(p8105.datasets)
library(viridis)

library(shiny)
library(rsconnect)

data(nyc_airbnb)

nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(stars = review_scores_location / 2) %>% 
  rename(borough = neighbourhood_group) %>% 
  select(borough, neighbourhood, stars, price, room_type, lat, long) %>% 
  drop_na(stars)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  h1("This is h1 title:)"),
  h2("h2 title..."),
  
  # App title ----
  titlePanel("Hello Shiny!!!"),
  
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # # Input: Slider for the number of bins ----
      # sliderInput(inputId = "bins",
      #             label = "Number of bins:",
      #             min = 1,
      #             max = 50,
      #             value = 30)
      
      # selectInput widget
      selectInput(
        inputId = "boro_choice",
        label = h3("Select boro"),
        choices = nyc_airbnb %>% distinct(borough) %>% pull(),
        selected = "Manhattan"),
      
      
      # sliderInput widget
      sliderInput(
        "price_range", 
        label = h3("Choose price range"), 
        min = 0, max = 1000, value = c(100, 400)), 
      
      radioButtons(
        "room_choice", 
        label = h3("Choose room type"),
        choices = c("Private room", "Entire home/apt", "Shared room"), 
        selected = "Entire home/apt"), 
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output
      plotlyOutput(outputId = "scatterPlot")
      # textOutput(outputId = "text_test")
      
    )
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

  # output$distPlot <- renderPlot({
  #   
  #   x    <- faithful$waiting
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   hist(x, breaks = bins, col = "#007bc2", border = "white",
  #        xlab = "Waiting time to next eruption (in mins)",
  #        main = "Histogram of waiting times")
  #   
  # })
  
  output$scatterPlot <- renderPlotly({ 
    nyc_airbnb %>%
      filter(
        borough == input[["boro_choice"]], 
        price %in% input[["price_range"]][1]:input[["price_range"]][2],
        room_type == input[["room_choice"]] )%>%
      mutate(text_label = str_c("Price: $", price, '\nRating: ', stars)) %>% 
      plot_ly(
        x = ~lat, y = ~long, type = "scatter", mode = "markers",
        alpha = 0.5, color = ~price, text = ~text_label)
  })
  
  # output$text_test <- renderPrint({ 
  #   input[["room_choice"]]
  # })
  
}

shinyApp(ui = ui, server = server)