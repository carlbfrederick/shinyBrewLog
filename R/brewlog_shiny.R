#' Run the Shiny Brew Log
#'
#' @param dbName A SQLite "brewlog" database (path included if not present in
#'               home directory)
#' @param xmlRecipe A beerXML file to pull the recipe stats
#'
#' @return
#' @export
#'
#' @import shiny
#' @import shinyTime
#' @import dplyr
#'
brewlog <- function(dbName, xmlRecipe = NULL) {
  #Prep Function
  require(shiny)
  require(shinyTime)

  #Check to see if database exists for batch_id
  if (!file.exists(dbName)) {
    batchID <-1
    oldBrews <- NULL
  } else {
    oldBrews <- readRDS(dbName)
    batchID <- max(oldBrews$batch_id, na.rm=TRUE) + 1
  }

  #initialize new row
  newBrew <- newBrewLog_db()

  #RadioButton inputs
  brewSalts <- c("", "Gypsum", "Calcium Chloride", "Epsom Salt", "Table Salt", "Magnesium Chloride", "Chalk", "Baking Soda", "Slaked Lime", "Lye")
  pHAgents <- c("", "Lactic Acid 88%", "Phosphoric Acid 10%")

  #Preload Info from recipe if exists
  shinyApp(
    ui = fluidPage(
      fluidRow(
        uiOutput("rec_title")
      ),
      sidebarLayout(
        sidebarPanel(width = 3,
          h4("Global Recipe Settings:"),
          fileInput("rec_xml", "Upload BeerXML file"),
          radioButtons("units_vol", "Volume Units", choices = c("Gallons", "Quarts", "Liters")),
          radioButtons("units_salts", "Salt Addition Units", choices = c("Grams", "Ounces")),
          radioButtons("units_grist", "Fermentables Units", choices = c("Pounds", "Ounces", "Kilograms")),
          radioButtons("units_hops", "Hops Units", choices = c("Grams", "Ounces")),
          actionButton("write_data", "Save Data", icon = icon("floppy-o"))
        ),
        mainPanel(
          tabsetPanel(
            tabPanel("Recipe",
              fluidRow(
                column(width = 3, dateInput("date_brew", "Brew Date", value = Sys.Date())),
                column(width = 2, timeInput("brew_start", "Start Time (24hr)", seconds = FALSE)),
                column(width = 2, timeInput("brew_stop", "Stop Time (24hr)", seconds = FALSE)),
                column(width = 5, textInput("rec_link", "Recipe URL", value = "Recipe URL"))
              ),
              fluidRow(
                column(width = 3, numericInput("rec_assumedEff", "Assumed Efficiency (%)", min = 0, max = 100, value = 75, width = 200)),
                column(width = 3, numericInput("rec_targetVol", "Target Volume in Fermentor", min = 0, value = 3))
              ),
              fluidRow(
                textAreaInput("rec_notes", "", value = "General Notes", width = 700, rows = 12)
              )
            ),
            tabPanel("Water Profile",
              fluidRow(
                withMathJax(
                  column(width = 2, h4("Target Profile:")),
                  column(width = 1, numericInput("h2o_target_ca", "$$Ca^{+2}$$", min = 0, value = 0)),
                  column(width = 1, numericInput("h2o_target_ma", "$$Mg^{+2}$$", min = 0, value = 0)),
                  column(width = 1, numericInput("h2o_target_na", "$$Na^+$$", min = 0, value = 0)),
                  column(width = 1, numericInput("h2o_target_cl", "$$Cl^-$$", min = 0, value = 0)),
                  column(width = 1, numericInput("h2o_target_so", "$$SO_4^{-2}$$", min = 0, value = 0)),
                  column(width = 1, numericInput("h2o_target_hco", "$$HCO_3^-$$", min = 0, value = 0))
                )
              ),
              fluidRow(
                column(width = 2, h4("Salts Added:")),
                column(width = 2, selectizeInput("salt1", "Select", choices = brewSalts)),
                column(width = 2, numericInput("salt1_amt", "Amount (g)", min = 0, value = 0)),
                column(width = 2, checkboxInput("salt1_add", "Add Another Salt", value = FALSE))
              ),
              fluidRow(
                column(width = 2, h4("pH Adjustments:")),
                column(width = 2, numericInput("ph_target", "Target pH", value = 5.2, min = 3.0, max = 6.0)),
                column(width = 2, selectizeInput("ph_agent", "Select", choices = pHAgents)),
                column(width = 2, numericInput("ph_amt", "Amount (ml)", value = 0))
              ),
              fluidRow(
                column(width = 2, h4("Water Notes:")),
                column(width = 8, textAreaInput("h2o_notes", "", value = "", width = 700, rows = 12))
              )
            ),
            tabPanel("Mash & Boil",
              fluidRow(
                column(width = 2, h4("Mash:"))
              ),
              fluidRow(
                column(width = 2, h4("Boil:"))
              )
            ),
            tabPanel("Fermentation & Packaging"),
            tabPanel("Tasting")
          )
        )
      )
    ),
    server = function(input, output, session) {
      output$rec_title <- renderUI({
        inFile <- input$rec_xml

        if (is.null(inFile))
          return(
            fluidRow(
              column(width = 1),
              column(width = 6, h2("... upload BeerXML recipe file to begin log ..."))
            )
          )

        #Load xml file
        recipe_obj <- reactive({
          importBXML(inFile$datapath) %>%
            tibble::as_data_frame()
        })

        #Write data newBrewlog
        newBrew <- eventReactive(input$write_data, {
          out <- newBrewLog_db()[1, c("recipe_title", "style", "type", "category",
                                      "vol_postB_target", "vol_preB_target",
                                      "est_og", "est_fg")] <- recipe_obj()
          out$batch_id = batchID
          out$brewDate = input$date_brew

        })


        return(
          fluidPage(
            fluidRow(
              column(width = 6, h4(em("Beer Name:"), recipe_obj$TITLE)),
              column(width = 4, h4(em("Style:"), recipe_obj$STYLE))
            ),
            fluidRow(
              column(width = 2, h4(em("Target OG:"), recipe_obj$EST_OG)),
              column(width = 2, h4(em("Target FG:"), recipe_obj$EST_FG)),
              column(width = 2),
              column(width = 4, h4(em("Category:"), recipe_obj$CAT))
            )
          )
        )
      })

      # output$rec_type <- recipe_obj$TYPE
      # output$rec_style <- recipe_obj$STYLE
      # output$rec_cat <- recipe_obj$CAT
      # output$rec_og_pred <- recipe_obj$EST_OG
      # output$rec_batch_size <- recipe_obj$BATCH_SIZE
      # output$rec_boil_size <- recipe_obj$BOIL_SIZE
    }
  )
}
