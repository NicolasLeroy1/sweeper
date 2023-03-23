#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyr)
library(dplyr)
library(stringr)

grid_size <- 8
n_mine <- 10

genarate_minesweeper_grid <- function(grid_size, n_mine) {
    mineland <- tidyr::expand_grid(row = 1:grid_size, col = 1:grid_size) %>%
        mutate(
            id = str_c("id", row, col), 
            mine = sample(c(rep(1, n_mine), rep(0, grid_size^2 - n_mine)))
        )
}

mineland_grid <- genarate_minesweeper_grid(grid_size, n_mine)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    tags$h1("Minesweeper"),

    # Sidebar with a slider input for number of bins 
    uiOutput("mineland")
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mineland <- renderUI({
        do.call(
            tagList,
            lapply(1:grid_size, function(ind) {
                tmp <- mineland_grid %>% dplyr::filter(row == ind)
                fluidRow(
                    actionGroupButtons(
                       inputIds = tmp$id,
                       labels = as.character(tmp$mine)
                    )
                )
            })
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
