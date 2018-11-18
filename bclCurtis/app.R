library(shiny)
library(tidyverse)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)


ui <- fluidPage(
  titlePanel("BC Liquor price app", 
             windowTitle = "BCL app"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Select your desired price range.",
                  min = 0, max = 100, value = c(15, 30), pre="$"),
      radioButtons("typeInput", "Select your alcoholic beverage type.", 
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      checkboxInput("sortInput", "Check to sort data by price.",
                    value = FALSE),
      colourInput("colourInput", "Pick colour of plot.", value = "#4D4D4D",
                  showColour = "background", palette = "limited")
    ),
    mainPanel(
      plotOutput("price_hist"),
      DT::dataTableOutput("bcl_data")
    )
  )
)

server <- function(input, output) {
  bcl_edited <- reactive({
    if (input$sortInput) {
      bcl %>% 
        filter(Price < input$priceInput[2],
               Price > input$priceInput[1],
               Type == input$typeInput)  %>%
        arrange(Price)
    } else {
      bcl %>% 
        filter(Price < input$priceInput[2],
               Price > input$priceInput[1],
               Type == input$typeInput)       
    }
  })
  output$price_hist <- renderPlot({
    bcl_edited() %>% 
      ggplot(aes(Price)) +
      geom_histogram(fill = input$colourInput)
  })
  output$bcl_data <- DT::renderDataTable({
    bcl_edited()
  })
}

shinyApp(ui = ui, server = server)