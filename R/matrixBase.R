getNeighbors <- function(row, col, rows, cols){
  neighbors <- list()
  if (row > 0){  #UP
    neighbors <- append(neighbors, c(row-1,col))
  }
  if (row < rows-1){   #DOWN
    neighbors <- append(neighbors, c(row+1,col))
  }
  if (col > 0){    #LEFT
    neighbors <- append(neighbors, c(row,col-1))
  }
  if (col < cols-1){    #RIGHT
    neighbors <- append(neighbors, c(row,col+1))
  } 
  #DIAGONALS 
  if (row > 0 & col > 0){
    neighbors <- append(neighbors, c(row-1,col-1))
  }
  if (row < rows-1 & col < cols-1){
    neighbors <- append(neighbors, c(row+1,col+1))
  }
  if (row < rows-1 & col > 0){
    neighbors <- append(neighbors, c(row+1,col-1))
  }
  if (row > 0 & col < cols-1){
    neighbors <- append(neighbors, c(row-1,col+1))
  }
}

firstGrid <- function(w, l){
  grid <- matrix(data = NA, nrow = l, ncol = w)
  randomMines <- sample(nrow(grid)*ncol(grid), runif(1,min=1,max=15))
  grid[randomMines] <- -1
  for (i in 1:l){
    for (j in 1:w){
      if (!is.na(grid[i][j])){
        neighbors <- getNeighbors(i,j,l,w)
      }
      }
  }
  for (k in neighbors){
    grid[k] <- +1
  }
grid
}

firstGrid(10,10)
  
