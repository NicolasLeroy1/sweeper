library(shiny)
library(shinyWidgets)
ratio = 10

launch_game = function(height=10,length=10,ratio=10){
  grid = array(sample(c(0,1), size = length*height,replace=TRUE,prob=c((100-ratio)/100,ratio/100)),dim=c(length,height))
  ui = fluidpage()
  server = function(input,output){}
  game = shinyApp(ui,server)
  runApp(game)
}


ui = navbarPage("MineSw33per",
    actionButton(
      inputId = "start_button",
      label = "Start game!"
    ),
    textOutput(
      outputId = "grid",
      placeholder = TRUE
    ),
    setBackgroundColor(
    color = c("#b4d3b2", "#FFFFFF"),
    gradient = "linear",
    direction = "bottom"
    ),
    navbarMenu("Difficulty",
               tabPanel("Beginner"),
               tabPanel("Intermediate"),
               tabPanel("Expert")),
    mainPanel(sliderInput("length","Length: ",10,40,20),
              sliderInput("height","Height: ",10,40,20),
              sliderInput("ratio","Choose a mine-ratio",5,50,10)))
server = function(input, output){
    start_game <- eventReactive({
      input$start_button
    },
    {
      firstGrid(length,height,ratio)
    })
  }


shinyApp(ui, server)
