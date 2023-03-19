
library(shiny)

# Function to Generate the observeEvent linked to a cell
create_cell_click_handler <- function(cell_id, r, c, game_state, input,session) {
  observeEvent(input[[cell_id]], {
    if (!game_state$game_over && !game_state$opened_cells[r, c]) {
      game_state$opened_cells[r, c] <- TRUE

      cell_content <- if (game_state$game_board[r, c] == -1) "X" else game_state$game_board[r, c]
      session$sendCustomMessage("updateInnerHTML", list(id = cell_id, content = cell_content))
    }
  })
}
# Function to Generate game board
generateBoard <- function(rows, cols, mines) {
  # Create empty board
  board <- matrix(0, nrow = rows, ncol = cols)

  # Place mines randomly on the board
  mine_positions <- sample(1:(rows * cols), size = mines)
  board[mine_positions] <- -1

  # Calculate adjacent mine counts for each cell
  for (r in 1:rows) {
    for (c in 1:cols) {
      if (board[r, c] != -1) {
        adjacent_cells <- board[max(1, r - 1):min(rows, r + 1), max(1, c - 1):min(cols, c + 1)]
        board[r, c] <- sum(adjacent_cells == -1)
      }
    }
  }

  return(board)
}
# Recursive function to open cells
openCells <- function(row, col, board, opened) {
  if (opened[row, col]) return(opened)

  opened[row, col] <- TRUE
  if (board[row, col] == 0) {
    for (r in max(1, row - 1):min(nrow(board), row + 1)) {
      for (c in max(1, col - 1):min(ncol(board), col + 1)) {
        if (!opened[r, c]) {
          opened <- openCells(r, c, board, opened)
        }
      }
    }
  }

  return(opened)
}

# Define the user interface
ui <- fluidPage(
  titlePanel("Minesweeper Game"),
  sidebarLayout(
    sidebarPanel(
      numericInput("rows", "Number of rows:", 10, min = 3, max = 30),
      numericInput("cols", "Number of columns:", 10, min = 3, max = 30),
      numericInput("mines", "Number of mines:", 10, min = 1),
      actionButton("start", "Start Game")
    ),
    mainPanel(
      tags$style(type = "text/css", "
                .cell {
                  width: 30px;
                  height: 30px;
                  text-align: center;
                  border: 1px solid #999;
                  display: table-cell;
                  font-family: 'Courier New', monospace;
                  font-weight: bold;
                  line-height: 30px;
                  user-select: none;
                  vertical-align: middle;
                }
                "),
      uiOutput("gameBoard"),
      tags$script("
  Shiny.addCustomMessageHandler('updateInnerHTML', function(message) {
    var element = document.getElementById(message.id);
    if (element) {
      element.innerHTML = message.content;
    }
  });
")
    )
  )
)
# Define server-side logic
server <- function(input, output, session) {

  # Set initial values for game_state
  game_state <- reactiveValues(
    rows = 10,
    cols = 10,
    mines = 10,
    game_board = matrix(0, nrow = 10, ncol = 10),
    opened_cells = matrix(FALSE, nrow = 10, ncol = 10),
    game_over = FALSE
  )
  # Generate the game board based on input parameters
  observeEvent(input$start, {
    game_state$rows <- input$rows
    game_state$cols <- input$cols
    game_state$mines <- min(input$mines, game_state$rows * game_state$cols - 1)

    game_state$game_board <<- generateBoard(game_state$rows, game_state$cols, game_state$mines)
    game_state$opened_cells <<- matrix(FALSE, nrow = game_state$rows, ncol = game_state$cols)
    game_state$game_over <<- FALSE
  })
  # Update the game board UI
  output$gameBoard <- renderUI({
    rows <- game_state$rows
    cols <- game_state$cols

    board_ui <- lapply(1:rows, function(r) {
      game_row <- lapply(1:cols, function(c) {
        cell_id <- paste("cell", r, c, sep = "-")

        tags$td(
          id = cell_id,
          class = "cell",
          onclick = if (!game_state$game_over) paste0("Shiny.setInputValue('", cell_id, "', 1, {priority: 'event'})")
        )
      })
      tags$tr(do.call(tagList, game_row))
    })

    tags$table(do.call(tagList, board_ui))
  })
  # Handle cell clicks and update cell content
  observe({
  for (r in 1:game_state$rows) {
    for (c in 1:game_state$cols) {
      cell_id <- paste("cell", r, c, sep = "-")
      create_cell_click_handler(cell_id, r, c, game_state, input,session)
    }
  }
})

}




# Run the Shiny app
shinyApp(ui = ui, server = server)

