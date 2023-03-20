library(shiny)

hiddenSquareUI <- function(i, j) {
  tags$img(
    src = "/logo/icons8-data-grid-24.png",
    alt = "hiddenSquare",
    x = j,
    y = i,
    width = 1,
    height = 1
  )
}

openedSquareUI <- function(i, j) {
  tags$img(
    src = "/logo/icons8-data-grid-24 (1).png",
    alt = "playedSquare",
    x = j,
    y = i,
    width = 1,
    height = 1
  )
}

flagLogoUI <- function(i, j) {
  tags$img(
    src = "/logo/icons8-empty-flag-30.png",
    alt = "flagged",
    x = j,
    y = i,
    width = 1,
    height = 1
  )
}

mineLogoUI <- function(i, j) {
  tags$img(
    src = "/logo/icons8-naval-mine-30.png",
    alt = "mines",
    x = j,
    y = i,
    width = 1,
    height = 1
  )
}

drawGridUI <- function(inputId, l, w, status,fun) {
  cells = apply(expand.grid(1:l, 1:w), 1, function(index) {
    i = index[1]
    j = index[2]
    
    fun(i,j)
  })
  
  tagList(
    singleton(tags$head(tags$script(jscode))),
    tags$svg(
      `data-input-id` = inputId,
      class = sprintf("minesweeper-grid %s", status),
      width = 25 * w,
      viewBox = sprintf("1 1 %d %d", w, l),
      tagList(cells)
    )
  )
}


ongoingGridUI <- function(inputId, grid, state) {
  drawGridUI(
    inputId = inputId,
    l = grid$l,
    w = grid$w,
    status = "ongoing",
    function(i, j) {
      if (state$played[i, j]) {
        openedSquareUI(i, j, grid$neighbors[i, j])
      } else if (state$hidden[i, j]) {
        flagLogoUI(i, j)
      } else {
        hiddenSquareUI(i, j)
      }
    }
  )
}


youLoseUI <- function(grid, state) {
  drawGridUI(
    inputId = NULL,
    l = grid$l,
    w = grid$w,
    status = "You lose !",
    function(i, j) {
      if (state$revealed[i, j] && game$mines[i, j]) {
        flagLogoUI(i, j)
      } else if (state$played[i, j]) {
        openedSquareUI(i, j, grid$neighbors[i, j])
      } else if (state$hidden[i, j] && !grid$mines[i, j]) {
        mineLogoUI(i, j)
      } else if (state$hidden[i, j]) {
        flagLogoUI(i, j)
      } else if (grid$mines[i, j]) {
        mineLogoUI(i, j)
      } else {
        hiddenSquareUI(i, j)
      }
    }
  )
}


youWinUI <- function(grid) {
  drawGridUI(
    inputId = NULL,
    l = grid$l,
    w = grid$w,
    status = "You win !",
    function(i, j) {
      if (grid$mines[i, j]) {
        flagLogoUI(i, j)
      } else {
        openedSquareUI(i, j, grid$neighbors[i, j])
      }
    }
  )
}


