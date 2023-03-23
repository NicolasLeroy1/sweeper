library(shiny)

#generateBoard
generateBoard = function(rows, cols, mines) {
  # Create empty board
  board = matrix(0, nrow = rows, ncol = cols)

  # Place mines randomly on the board
  mine_positions = sample(1:(rows * cols), size = mines)
  board[mine_positions] = -1

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


ui=fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("ncols","Number of columns",5,20,10),
      sliderInput("nrows","Number of rows",5,20,10),
      sliderInput("nmines","Number of mines",1,100,10),
      actionButton("start","Start")
    ),
    mainPanel(
      textOutput("mytext"),
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
    board = generateBoard(rows=10,cols=10,mines=10),
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
    gameState$board = generateBoard(ncols=gameState$ncols,nrows=gameState$nrows,mines=gameState$nmines)
    gameState$opened = matrix(FALSE,ncol=gameState$ncols,nrow=gameState$nrows)
    gameState$flags = matrix(FALSE,ncol=gameState$ncols,nrow=gameState$nrows)
    gameOver = FALSE
    gameState$success = FALSE
  })
  #boardUI rendering using rows and cols
  output$boardUI = renderUI({
    lapply(1:gameState$rows, function(r){
      fluidRow(lapply(1:gameState$cols, function(c){
          actionButton(paste0("r",r,"c",c),
                       if(gameState$opened[r,c]==FALSE){" "}
                       else if(gameState$flags[r,c]){"f"}
                       else{paste0(gameState$board[r,c])}
          )
        }))
    })
  })
  # Success test
  observe({
    if(sum(gameState$opened&(gameState$board!=-1))==gameState$ncols*gameState$nrows-gameState$mines){gameState$success=TRUE}
  })
  # mytext initialization
  output$mytext=renderUI(tags$text("Try your luck !"))
  # gameOver handling
  observeEvent(gameState$gameOver,{
    if(gameState$gameOver){output$mytext=renderUI(tags$text("U lost !!"))}
    else{output$mytext = renderUI(tags$text("go gogogo !"))}
  })
  # Success handling
  observeEvent(gameState$success,{
    if(gameState$success){output$mytext=renderUI(tags$text("U won , start again with moooore mines!"))}
    else{output$mytext=renderUI(tags$text("Yeeaaeaeeaeeaae gogogogogo !"))}
  })
  # Clicks handling
  observe({
    for(r in 1:gameState$nrows){
      for(c in 1:gameState$ncols){
        observeEvent(input[[paste0("r",r,"c",c)]],{
          if(gameState$flags[r,c]==TRUE){return()}
          else if(gameState$opened[r,c]==TRUE){return()}
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



