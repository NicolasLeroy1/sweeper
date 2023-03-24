#' Generate a Minesweeper game board matrix
#'
#' This function generates a Minesweeper game board matrix with a specified number of rows, columns, and mines.
#' The generated matrix contains -1 for cells with mines and the number of adjacent mines for other cells.
#'
#' @param nrows The number of rows in the game board.
#' @param ncols The number of columns in the game board.
#' @param nmines The number of mines to be placed on the game board.
#' @return A nrows x ncols matrix representing the Minesweeper game board.
#' @examples
#' board <- generateBoard(5, 5, 10)
#' @export
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
