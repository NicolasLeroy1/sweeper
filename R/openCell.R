#' Open a cell and flood-fill adjacent cells in a Minesweeper game
#'
#' This function recursively opens a cell and its adjacent cells in a Minesweeper game state.
#' If the cell contains a mine, the game is marked as over. If the cell is empty (has no adjacent mines),
#' the function will recursively open all its adjacent cells.
#'
#' @param r The row index of the cell to open.
#' @param c The column index of the cell to open.
#' @param gameState A list representing the current game state, containing the following elements:
#' * board: A matrix representing the minesweeper board. -1 indicates a mine, other values indicate the number of adjacent mines.
#' * opened: A matrix representing whether a cell has been opened (TRUE) or not (FALSE).
#' * nrows: The number of rows in the game board.
#' * ncols: The number of columns in the game board.
#' * gameOver: A boolean indicating whether the game is over (TRUE) or not (FALSE).
#' @return A modified gameState list with the specified cell and its adjacent cells opened.
#' @examples
#' gameState <- list(board = matrix(0, 3, 3),
#' opened = matrix(FALSE, 3, 3),
#' nrows = 3,
#' ncols = 3,
#' gameOver = FALSE)
#' newGameState <- openCell(1, 1, gameState)
#' @export
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
