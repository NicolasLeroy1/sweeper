library(shiny)

# Flood open adjacent cells
openCell <- function(r, c, gameState) {
  if(gameState$opened[r,c]==TRUE){return(gameState)}
  gameState$opened[r,c]=TRUE
  if(gameState$board[r,c]==-1){gameState$gameOver=TRUE}
  if(gameState$board[r,c]==0){
    for(i in max(r-1,1):min(r+1,gameState$nrows)){
      for(j in max(c-1,1):min(c+1,gameState$ncols)){
        openCell(i,j,gameState)
      }
    }
  }
  return(gameState)
}
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
  tags$head(tags$style(HTML("
  table {
        width: 30%;
        border-collapse: collapse;
        background-color=black
      }
      td {
        padding: 0px;
        text-align: center;
      }
  "))),
  sidebarLayout(
    sidebarPanel(
      tags$audio(src="Chill_afternoon.mp3",type="audio/mp3",controls=TRUE),
      sliderInput("ncols","Number of columns",5,20,10),
      sliderInput("nrows","Number of rows",5,20,10),
      sliderInput("nmines","Number of mines",1,100,10),
      radioButtons("flagbox", label ="Select action : ",
                   choices = list("Open" = 1, "Flag" = 2),
                   selected = 1),
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
  observeEvent(input$start,ignoreNULL = FALSE,{
    #Reset gameState
    gameState$ncols = input$ncols
    gameState$nrows = input$nrows
    gameState$nmines = input$nmines
    gameState$board = generateBoard(ncols=gameState$ncols,nrows=gameState$nrows,nmines=gameState$nmines)
    gameState$opened = matrix(FALSE,ncol=gameState$ncols,nrow=gameState$nrows)
    gameState$flags = matrix(FALSE,ncol=gameState$ncols,nrow=gameState$nrows)
    gameState$gameOver = FALSE
    gameState$success = FALSE

    #Creating the buttons ids
    button_id = matrix(FALSE,gameState$nrows,gameState$ncols)
    for(r in 1:gameState$nrows){
      for(c in 1:gameState$ncols){
        button_id[r,c]=paste0("r",r,"c",c)

      }
    }
    #Creating
    lapply(button_id,function(id){
      observeEvent(input[[id]],ignoreInit = TRUE,{
        r = as.integer(unlist(strsplit(x=id,split="[[:alpha:]]")))[2]
        c = as.integer(unlist(strsplit(x=id,split="[[:alpha:]]")))[3]
        if(input$flagbox==1){openCell(r,c,gameState)}
        else{
          if(gameState$flags[r,c]){gameState$flags[r,c]=FALSE}
          else{gameState$flags[r,c]=TRUE}
        }
      })
    })
  })
  # boardUI rendering using nrows and ncols
  output$boardUI = renderUI({
    grid = lapply(1:(gameState$nrows), function(r){
      gridrow = lapply(1:(gameState$ncols), function(c){
        tags$td({
          actionButton(paste0("r",r,"c",c),
                       if(gameState$opened[r,c]==FALSE){
                         if(gameState$flags[r,c]){icon("flag")}
                         else{icon("square")}
                       }
                       else if(gameState$board[r,c]==-1){icon("bomb")}
                       else{paste0(gameState$board[r,c])}
          )
        })
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
  output$mytext=renderText("TRY YOUR LUCK !")
  # gameOver handling
  observeEvent(gameState$gameOver,ignoreInit = TRUE,{
    if(gameState$gameOver){output$mytext=renderText("!!!! YOU LOST !!!! SAD")}
    else{output$mytext = renderText("YEA ! START AGAIN !!!")}
  })
  # Success handling
  observeEvent(gameState$success,ignoreInit = TRUE,{
    if(gameState$success){output$mytext=renderText("YOU WON WITH EASE AND STYLE !!!
                                                   START AGAIN NOW !
                                                   YEA !")}
    else{output$mytext=renderText("GOOD LUCK AGAIN!")}
  })
}
shinyApp(ui,server)

