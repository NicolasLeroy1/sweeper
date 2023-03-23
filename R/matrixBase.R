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
baseGrid <- function(w, l, nbofMines){
  
  mines = rep(TRUE, nbofMines)
  empty = rep(FALSE, w * l - nbofMines)
  randomMines = sample(c(nbofMines, empty))
  
  gameGrid = matrix(randomMines, nrow=w, ncol=l)
  
  for (i in 1:l){
    for (j in 1:w){
      if (isTRUE(gameGrid[i,j] == -1)) {
        neighbors <- getNeighbors(i,j,l,w)
        for (k in neighbors){
          gameGrid[k[1],k[2]] <- gameGrid[k[1],k[2]]+1
        }
      }
    }
  }
  as.data.frame(gameGrid)
}

hiddenGrid <- function(l, w) {
  matrix(rep(FALSE, l * w), nrow=l, ncol=w)
}

playedGrid <- function(l, w) {
  matrix(rep(FALSE, l * w), nrow=l, ncol=w)
}

initialGrid <- function(grid) {
  list(
    hidden = hiddenGrid(ncol=grid$w, nrow=grid$l),
    played = playedGrid(ncol=grid$w, nrow=grid$l)
  )
}

addFlag <- function(state, i, j) {
  state$hidden[i, j] = TRUE
  state
}

playerTurn <- function(grid, state, i, j) {
  remaining_coords = as.stack(c(i, j))
  
  while (length(remainingCoords) > 0) {
    coords = pop(remainingCoords)
    
    i = coords[1]
    j = coords[2]
    
    if (state$played[i, j]) {
      next
    }
    
    state$played[i, j] = TRUE
    
    if (grid$neighbors[i, j] == 0) {
      apply(
        nearbyCoords(i, j, nrow=grid$l, ncol=grid$w),
        1,
        function(coords) {
          i = coords[1]
          j = coords[2]
          
          if (!state$played[i, j]) {
            push(remainingCoords, c(i, j))
          }
        }
      )
    }
  }
  
  state
}

gameStatus <- function(grid, state) {
  if (any(grid$mines & state$played)) {
    return("defeat")
  }
  
  if (all(xor(grid$mines, state$played))) {
    return("victory")
  }
  
  "ongoing"
}

