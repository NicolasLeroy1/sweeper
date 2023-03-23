library(shiny)

# Generate a matrix of a minesweeper grid
generateBoard = function(nrows, ncols, nmines) {
  # Create empty board
  board = matrix(0, nrow = nrows, ncol = ncols)

  # Place mines randomly on the board
  mine_positions = sample(1:(nrows * ncols), size = nmines)
  board[mine_positions] = -1

  # Calculate adjacent mine counts for each cell
  for (r in 1:nrows) {
    for (c in 1:ncols) {
      if (board[r, c] != -1) {
        adjacent_cells <- board[max(1, r - 1):min(nrows, r + 1), max(1, c - 1):min(ncols, c + 1)]
        board[r, c] <- sum(adjacent_cells == -1)
      }
    }
  }
  return(board)
}


ui=fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("ncols","Number of columns",5,20,10),
      sliderInput("nrows","Number of rows",5,20,10),
      sliderInput("nmines","Number of mines",1,100,10),
      actionButton("start","Start")
    ),
    mainPanel(
      verbatimTextOutput("mytext"),
      uiOutput("boardUI")
    )
  )
)

server = function(input,output,session){
  # Initialize gameState
  gameState = {reactiveValues(
    ncols = 10,
    nrows = 10,
    nmines = 10,
    board = generateBoard(nrows=10,ncols=10,nmines=10),
    opened = matrix(FALSE,ncol=10,nrow=10),
    gameOver = FALSE,
    success = FALSE,
    flags= matrix(FALSE,ncol=10,nrow=10)
  )}
  # StartButton handling
  observeEvent(input$start,{
    gameState$ncols = input$ncols
    gameState$nrows = input$nrows
    gameState$nmines = input$nmines
    gameState$board = generateBoard(ncols=gameState$ncols,nrows=gameState$nrows,nmines=gameState$nmines)
    gameState$opened = matrix(FALSE,ncol=gameState$ncols,nrow=gameState$nrows)
    gameState$flags = matrix(FALSE,ncol=gameState$ncols,nrow=gameState$nrows)
    gameOver = FALSE
    gameState$success = FALSE
  })
  #boardUI rendering using nrows and ncols
  output$boardUI = renderUI({
    grid = lapply(1:(gameState$nrows), function(r){
      gridrow = lapply(1:(gameState$ncols), function(c){
        actionButton(paste0("r",r,"c",c),
                     if(gameState$opened[r,c]==FALSE){"_"}
                     else if(gameState$flags[r,c]){"f"}
                     else{paste0(gameState$board[r,c])}
        )
      })
      tags$tr(tagList(gridrow))
    })
    tags$table(tagList(grid))
  })
  # Success test
  observe({
    if((sum(gameState$opened&(gameState$board!=-1)))==(gameState$ncols*gameState$nrows-gameState$nmines)){gameState$success=TRUE}
  })
  # mytext initialization
  output$mytext=renderText("try your luck")
  # gameOver handling
  observeEvent(gameState$gameOver,{
    if(gameState$gameOver){output$mytext=renderText(tags$text("U lost !!"))}
    else{output$mytext = renderText(tags$text("go gogogo !"))}
  })
  # Success handling
  observeEvent(gameState$success,{
    if(gameState$success){output$mytext=renderText("U won , start again with moooore mines!")}
    else{output$mytext=renderText("Yeeaaeaeeaeeaae gogogogogo !")}
  })
  # Clicks handling
observe({
  for(r in 1:gameState$nrows){
    for(c in 1:gameState$ncols){
      observeEvent(input[[paste0("r",r,"c",c)]],{
        if(gameState$flags[r,c]==TRUE){}
        else if(gameState$opened[r,c]==TRUE){}
        else if(gameState$board[r,c]==-1){
          gameState$opened[r,c]=TRUE
          gameState$gameOver = TRUE
        }
        else {gameState$opened[r,c]=TRUE}
      })
    }
  }
})



}

shinyApp(ui,server)

