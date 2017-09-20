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
require(DT)

# load db
db = dbConnect(SQLite(), dbname = "/srv/shiny-server/dynamictest/data/results.sqlite")


# Define UI for application that draws a histogram
ui <- bootstrapPage(

  navbarPage("Statistics Practice Results", inverse=TRUE),

  fluidRow(
    column(width = 2),
    column(
      width = 8,
      dataTableOutput("resultsTable"),
      dataTableOutput("studentDrilldown"),
      dataTableOutput("assignDrilldown")
    ),
    column(width = 2)
  )

)

# server logic for displaying data
server <- function(input, output) {

  # get db results into table
  db.df <- as_data_frame(
    dbGetQuery(db, "select * from results")
  ) %>%
    mutate(user_id = tolower(user_id),
           date = as.character(as.POSIXct(date, origin="1970-01-01")))

  # get unique list of usernames
  userlist.df <- db.df %>%
    group_by(user_id) %>%
    summarise(trials = n())
  
  # get unique list of users, by assignment for studentDrilldown
  assignlist.df <- db.df %>%
    group_by(user_id, assign) %>%
    summarise(trials = n(),
              correct = sum(correct, na.rm = TRUE),
              incorrect = trials - correct,
              correct_perc = correct/trials)
  
  # get specific assignment answers

  output$resultsTable <- renderDataTable(userlist.df)
  
  # get student drilldown from click
  studentDrilldata <- reactive({
    validate(
      need(length(input$resultsTable_rows_selected) > 0, "Select rows!")
    )
    
    selected_student <- userlist.df[as.integer(input$resultsTable_rows_selected),]$user_id
    assignlist.df[assignlist.df$user_id %in% selected_student,]
    
  })
  
  # get assignment drilldown from click
  assignDrilldata <- reactive({
    validate(
      need(length(input$studentDrilldown_rows_selected) > 0, "Select rows!")
    )
    
    selected_assign <- assignlist.df[as.integer(input$studentDrilldown_rows_selected),]
    subset(db.df, user_id == selected_assign$user_id & assign == selected_assign$assign)
    
  })
  
  
  # display subsetted data
  output$studentDrilldown <- renderDataTable(studentDrilldata())
  
  output$assignDrilldown <- renderDataTable(assignDrilldata())

}

# Run the application
shinyApp(ui = ui, server = server)
