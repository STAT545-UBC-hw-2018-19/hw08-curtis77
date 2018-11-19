library(shiny)
library(tidyverse)

bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)


ui <- fluidPage(
  titlePanel("BC Liquor price app", 
             windowTitle = "BCL app"),
  downloadButton("downloadData", label = "Download"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Select your desired price range.",
                  min = 0, max = 100, value = c(15, 30), pre="$"),
      radioButtons("typeInput", "Select your alcoholic beverage type.", 
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      checkboxInput("sortInput", "Check to sort data by price.",
                    value = FALSE),
      conditionalPanel(condition = "input.typeInput == 'WINE'",
                       sliderInput("wineInput", "Select sweetness level.",
                                   min = 0, max = 10, value = c(0, 10))
      )
    ),
    mainPanel(
      plotOutput("price_hist"),
      DT::dataTableOutput("bcl_data")
    )
  )
)

server <- function(input, output) {
  bcl_price <- reactive({
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
  bcl_edited <- reactive({
    if (input$typeInput == 'WINE') {
      bcl_price() %>% 
        filter(Type == 'WINE',
               Sweetness <= input$wineInput[2],
               Sweetness >= input$wineInput[1])
    } else {
      bcl_price()
    }
  })
  output$price_hist <- renderPlot({
    bcl_edited() %>% 
      ggplot(aes(Price)) +
      geom_histogram()
  })
  output$bcl_data <- DT::renderDataTable({
    bcl_edited()
  })
  output$downloadData <- downloadHandler(
    filename = "BCLData",
    content = function(file) {
      write.csv(bcl_edited(), file)
    }
  )
}

shinyApp(ui = ui, server = server)