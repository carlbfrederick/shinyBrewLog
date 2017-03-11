#' Title
#'
#' @param dbName
#' @param xmlRecipe
#'
#' @return
#' @export
#'
#' @import shiny
brewlog <- function(dbName, xmlRecipe = NULL) {
  #Prep Function
  require(shiny)

  #Preload Info from recipe if exists
  shinyApp(
    ui = fluidPage(
      titlePanel("Shiny Brewlog"),
      fluidRow(
        column(width = 2, h3("Recipe Info")),
        column(width = 2, textInput("rec_title", "Title", value = "Title")),
        column(width = 3, textInput("rec_link", "URL", value = "Recipe URL")),
        column(width = 3, numericInput("rec_assumedEff", "Efficiency (%)", min = 0, max = 100, value = 75, width = 100))
      )
    ),
    server = function(input, output) {

    }
  )
}
