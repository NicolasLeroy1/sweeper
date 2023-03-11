library(shiny)
library(shinyWidgets)
ratio = 10

launch_game = function(heigth=10,length=10,ratio=10){
  grid = array(sample(c(0,1), size = length*heigth,replace=TRUE,prob=c((100-ratio)/100,ratio/100)),dim=c(length,heigth))
  ui = fluidpage()
  server = function(input,output){}
  game = shinyApp(ui,server)
  runApp(game)
}



config = shinyApp(
  ui = fluidPage(
  setBackgroundColor(
    color = c("#b4d3b2", "#FFFFFF"),
    gradient = "linear",
    direction = "bottom"
  ),
  titlePanel("Sw33pr: Configuration"),
  mainPanel(sliderInput("length","Choose a length",10,40,20),
            sliderInput("heigth","Choose a height",10,40,20),
            sliderInput("ratio","Choose a mine-ratio",5,50,10),
            selectInput("DifficultyLevel","Difficulty",
                        choices = c("Beginner", "Intermediate", "Expert")))),
  server = function(input, output){}
  )
