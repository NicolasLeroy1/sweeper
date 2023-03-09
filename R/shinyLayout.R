library(shiny)
library(shinyWidgets)

ui <- basicPage(
  setBackgroundColor(
     color = c("#b4d3b2", "#FFFFFF"),
     gradient = "linear",
     direction = "bottom"
   ),
  titlePanel(h1(id="title","MineSweeper")),
   tags$h1(tags$style(HTML("#title{color: #FFFFFF;
                   font-style: bold;
                   font-family: monospace;}"))),
   sidebarPanel(sidebarLayout(selectInput("DifficultyLevel","Difficulty",
                                          choices = c("Beginner", "Intermediate", "Expert")),
   mainPanel(textOutput("Play Game!", inline=TRUE)))
 ))

shinyApp(ui, server)
