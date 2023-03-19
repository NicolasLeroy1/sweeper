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

openedSquareUI <- function(i, j) {
  tags$image(
    href = sprintf("logo/icons8-data-grid-24 (1).png", "played"),
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