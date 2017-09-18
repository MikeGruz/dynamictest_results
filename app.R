#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(dplyr)

# load db
source("/srv/shiny-server/dynamictest/db/load_db.R")

# Define UI for application that draws a histogram
ui <- bootstrapPage(

  navbarPage("Statistics Practice Results", inverse=TRUE),

  fluidRow(
    column(width=2),
    column(
      width=10,
      tableOutput("resultsTable")

    )
  )


)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # get db results into table
  db.df <- as_data_frame(
    dbGetQuery(db, "select * from results")
  ) %>%
    mutate(date = as.character(as.POSIXct(date, origin="1970-01-01")))

  print(db.df)

  output$resultsTable <- renderTable(db.df)


}

# Run the application
shinyApp(ui = ui, server = server)