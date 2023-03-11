#allows us to add a list inside another list
#each list will serve as positions in the future matrix
lappend <- function (lst, ...){
  lst <- c(lst, list(...))
  return(lst)
}

#identifies all the neighboring squares in order to number out the mines later on
getNeighbors <- function(i, j, rows, cols){
  neighbors <- list()
  if (i > 1){  #UP
    neighbors <- lappend(neighbors, c(i-1,j))
  }
  if (i < rows){   #DOWN
    neighbors <- lappend(neighbors, c(i+1,j))
  }
  if (j > 1){    #LEFT
    neighbors<- lappend(neighbors, c(i,j-1))
  }
  if (j < cols){    #RIGHT
    neighbors<- lappend(neighbors, c(i,j+1))
  } 
  #DIAGONALS 
  if (i > 1 & j > 1){
    neighbors<- lappend(neighbors, c(i-1,j-1))
  }
  if (i < rows & j < cols){
    neighbors<- lappend(neighbors, c(i+1,j+1))
  }
  if (i < rows & j > 1){
    neighbors<- lappend(neighbors, c(i+1,j-1))
  }
  if (i > 1 & j < cols){
    neighbors<- lappend(neighbors, c(i-1,j+1))
  }
  neighbors
}

#randomly places mines with corresponding number around the mine
firstGrid <- function(w, l, nbofMines){
  grid <- matrix(data = 0, nrow = l, ncol = w)
  randomMines <- sample(nrow(grid)*ncol(grid), runif(1,min=1,max=nbofMines))
  grid[randomMines] <- -1
  for (i in 1:l){
    for (j in 1:w){
      if (isTRUE(grid[i,j] == -1)) {
        neighbors <- getNeighbors(i,j,l,w)
      }
      for (k in neighbors){
        grid[k[1],k[2]] <- 1
      }
    }
  }
  grid
}


