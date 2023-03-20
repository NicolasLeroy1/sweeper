library(shiny)

hiddenSquareUI <- function(i, j) {
  tags$image(
    href = sprintf("logo/icons8-data-grid-24.png", "hidden"),
    class = "hiddenSquare",
    x = j,
    y = i,
    width = 1,
    height = 1
  )
}

openedSquareUI <- function(i, j, n) {
  tags$image(
    href = sprintf(paste0("logo/icons8-",n,"-24.png"), "played"),
    class = "playedSquare",
    x = j,
    y = i,
    width = 1,
    height = 1
  )
}

flagLogoUI <- function(i, j) {
  tags$image(
    href = sprintf("logo/icons8-empty-flag-30.png", "flagged"),
    class = "flagged",
    x = j,
    y = i,
    width = 1,
    height = 1
  )
}

mineLogoUI <- function(i, j) {
  tags$image(
    href = sprintf("logo/icons8-naval-mine-30.png", "mines"),
    class = "mines",
    x = j,
    y = i,
    width = 1,
    height = 1
  )
}


mineSweeperUI = function(rows,cols,state) {
  board = lapply(1:rows, function(r){
     gridrow = lapply(1:cols, function(c){tags$td({
       if (state$hidden[r,c]){hiddenSquareUI(r,c)}
       else if(state$gameGrid[r,c]==-1){mineLogoUI(r,c)}
       else {openedSquareUI(r,c,state$gameGrid[r,c])}
       },
       id=paste0("cellr",r,"c",c),
       class="cell",
       onclick = paste0("Shiny.setInputValue(cellr",r,"c",c,", TRUE)")
       )
      })
     tags$tr(do.call(tagList,gridrow))
  })
  tags$tr(do.call(tagList,board))
}

clickHandler = function(rows,cols){
  for(r in 1:rows){
    for(c in 1:cols){
      cellid=paste("cellr",r,"c",c)
      observeEvent(input[cellid],{

      }

      )
    }
  }
}

#TEST
#rows=10
#cols=10
#state=list(hidden=matrix(TRUE,rows*cols,ncol=cols),gameGrid=matrix(FALSE,rows*cols,ncol=cols))
#mineSweeperUI(2,2,state)



!
