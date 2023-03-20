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
  table = lapply(1:rows, function(r){
     row = lapply(1:cols, function(c){
       if (state$hidden[r,c]){hiddenSquareUI(r,c)}
       else if(state$gameGrid[r,c]==-1){mineLogoUI(r,c)}
       else {openedSquareUI(r,c,state$gameGrid[r,c])}
    })
     tags$tr(do.call())
  })
}
