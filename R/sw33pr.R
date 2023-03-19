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
    sidebarPanel(numericInput("width","Width: ",value = 50),
              numericInput("height","Height: ", value=50),
              numericInput("ratio","Number of mines: ",value=49),
              actionButton(
                inputId = "start_button",
                label = "Play game!"
              ), width = 2),
    mainPanel(
      minesweeperUI("MineSweeper"),
      width=10
    ))
server = function(input, output, session){
  reactiveL = reactiveVal()
  reactiveW= reactiveVal()
  reactiveNbMines = reactiveVal()
  
  observeEvent(input$gameGrid,{
    enforceLimit <- function(session, inputId, val, min, max, default) {
      val = if (!is.integer(val)) {
        default
      } else if (val < min) {
        min
      } else if (val > max) {
        max
      } else {
        val
      }
      
      updateNumericInput(session, inputId, val = val)
      val
    }
    
    reactiveNrow(
      enforceLimit(session, "height", input$height, 1, 50, 10)
    )
    
    reactiveNcol(
      enforceLimit(session, "width", input$width, 1, 50, 10)
    )
    
    maxMines = reactiveL() * reactiveW() - 1
    reactiveNbMines(
      enforceLimit(session, "ratio", input$ratio, 1, maxMines, 14)
    )
  })

minesweeperServer(
  "MineSweeper",
  reactive(input$firstGrid),
  reactiveNrow = reactiveL,
  reactiveNcol = reactiveW,
  reactiveNmines = reactiveNbMines
)
}

shinyApp(ui, server)

