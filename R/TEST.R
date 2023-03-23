library(shiny)
library(shinyjs)
generateBoard <- function(rows, cols, mines) {
  # Create empty board
  board <- matrix(0, nrow = rows, ncol = cols)

  # Place mines randomly on the board
  mine_positions <- sample(1:(rows * cols), size = mines)
  board[mine_positions] <- -1

  # Calculate adjacent mine counts for each cell
  for (r in 1:rows) {
    for (c in 1:cols) {
      if (board[r, c] != -1) {
        adjacent_cells <- board[max(1, r - 1):min(rows, r + 1), max(1, c - 1):min(cols, c + 1)]
        board[r, c] <- sum(adjacent_cells == -1)
      }
    }
  }

  return(board)
}
open = function(row,col,rows,cols,board,hidden){
  if(board[row,col]==0){
    hidden[row,col]=FALSE
    for(r in max((row-1),1):min((row+1),rows)){
      for(c in max((col-1),1):min((col+1),cols)){
        if(hidden[r,c]==TRUE){
          hidden = open(r,c,rows,cols,board,hidden)}
      }
    }
  }
  else{hidden[row,col]=FALSE}
return(hidden)
}

ui = fluidPage(
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      numericInput("rows", "Number of rows:", 10, min = 3, max = 30),
      numericInput("cols", "Number of columns:", 10, min = 3, max = 30),
      numericInput("mines", "Number of mines:", 10, min = 1),
      actionButton("start", "Start Game")
    ),
    mainPanel(uiOutput("gameboard")
      )
    )
  )


server = function(input,output,session){
  #Initialize state
  state=reactiveValues(
    rows = 10,
    cols = 10,
    mines = 10,
    hidden = matrix(TRUE,nrow=10,ncol=10),
    board = generateBoard(10,10,10),
    gameover = FALSE
  )

  #Reset the game board with new values
  observeEvent(input$start,{
    state$rows = input$rows
    state$cols = input$cols
    state$mines = input$mines
    state$hidden = matrix(TRUE,nrow=input$rows,ncol=input$cols)
    board=generateBoard(input$rows,input$cols,input$mines)
  })

  #Render the game board
  output$gameBoard = renderUI({
    board = lapply(1:rows, function(r){
      gridrow = lapply(1:cols, function(c){
        actionButton(paste0("r",r,"c",c),if(state$hidden[r,c]==TRUE){return("h")}else{return(state$board[r,c])})
      })
      tags$tr(do.call(tagList,gridrow))
    })
    tags$tr(do.call(tagList,board))
  })

  #ClickHandling
  observe({
    for(r in 1:(state$rows)){
      for(c in 1:(state$cols)){
        button = paste0("r",r,"c",c)
        observeEvent(button,{
          if(state$hidden[r,c]==FALSE){return()}
          else if(state$board[r,c]==-1){
            state$hidden[r,c]=FALSE
            state$gameover=TRUE}
          else {state$hidden = open(r,c,state$rows,state$cols,state$board,state$hidden)}
        }

        )
      }
    }
  })
}




shinyApp(ui,server)




