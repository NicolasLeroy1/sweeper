library(shiny)
library(shinyjs)

ui = fluidPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      numericInput("rows", "Number of rows:", 10, min = 3, max = 30),
      numericInput("cols", "Number of columns:", 10, min = 3, max = 30),
      numericInput("mines", "Number of mines:", 10, min = 1),
      actionButton("start", "Start Game")
    ),
    mainPanel(
      )
    )
  )



mineSweeperUI = function(rows,cols,state) {
  board = lapply(1:rows, function(r){
    gridrow = lapply(1:cols, function(c){
      actionButton(paste0("r",r,"c",c),if(state$hidden[r,c]==TRUE){return(" ")}else{return(state$board[r,c])})
    })
    tags$tr(do.call(tagList,gridrow))
  })
  tags$tr(do.call(tagList,board))
}
server = function(input,output,session){}




shinyApp(ui,server)

