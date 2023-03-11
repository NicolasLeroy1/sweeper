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
      label = "Play game!"
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
    sidebarPanel(numericInput("length","Length: ",value = 50),
              numericInput("height","Height: ", value=50),
              numericInput("ratio","Choose a mine-ratio",value=49)),
    mainPanel(dataTableOutput(
      outputId = "grid"),
      ))
server = function(input, output,session){
    start_game <- eventReactive({
      input$start_button
    },
    {
      firstGrid(length,height,ratio)
    })
    
    output$grid <- renderDataTable(
      start_game()
    )
  }

shinyApp(ui, server)
