library(shiny)
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
    titlePanel("Sw33pr : Configuration"),
    mainPanel(sliderInput("length","choose a length",10,40,20),
              sliderInput("heigth","choose a heigth",10,40,20),
              sliderInput("ratio","choose a mine-ratio",5,50,10))),
  server = function(input, output){}
  )
