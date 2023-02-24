library(shiny)

# Define UI for app ----
ui = fluidPage(
  titlePanel("Sw33pr : Configuration"),
  mainPanel(sliderInput("length","choose a length",10,40,20),
            sliderInput("heigth","choose a heigth",10,40,20),
            sliderInput("ratio","choose a mine-ratio",5,50,10)
            )
)

# Define server logic  ----
server = function(input, output){
  output$text = renderText(input$text)
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

