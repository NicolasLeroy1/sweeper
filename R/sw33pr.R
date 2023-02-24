library(shiny)

config = shinyApp(

  ui = fluidPage(
    titlePanel("Sw33pr : Configuration"),

    mainPanel(sliderInput("length","choose a length",10,40,20),
              sliderInput("heigth","choose a heigth",10,40,20),
              sliderInput("ratio","choose a mine-ratio",5,50,10)
              )
    ),

  server = function(input, output){}
  )

game = shinyApp(

  ui = fluidpage(),

  server = function(input,output){}
  )
