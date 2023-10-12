library(shiny)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Challenge-8"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a dataset ----
      selectInput("dataset", "Choose a dataset:",
                  choices = c("rock", "pressure", "cars")),
      
      # Input: Specify the number of observations to view ----
      numericInput("obs", "Number of observations to view:", 15),
      
      # Include clarifying text ----
      helpText("Note: while the data view will show only the specified",
               "number of observations, the summary will still be based",
               "on the full dataset."),
      
      # Input: actionButton() to defer the rendering of output ----
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      actionButton("update", "Update View", class = "btn-info")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      h1("Summary Dashboard"),
      p("What happens when a",
        strong(span("geologist", style = "color: green")),
        "is caught between a rock and a hard place while driving?"),
      em("They take observations like these below..."),
      br(),
      
      # img(src = "imagerock.jpg", height = 200),
      # img(src = "imagepressure.jpg", height = 200),
      # img(src = "imagecars.jpg", height = 200),
      
      # Output: Header + summary of distribution ----
      h4(strong("Summary")),
      imageOutput("images", height = 200),
      br(),
      verbatimTextOutput("summary"),
      
      # Output: Header + table of distribution ----
      h4(strong("Observations")),
      tableOutput("view")
    )
    
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
  
  # Initialise a reactive variable to store the selected dataset
  selected_dataset <- reactive({
    input$update
    isolate(input$dataset)
  })
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    dataset_name <- selected_dataset()
    if (!is.null(dataset_name)) {
      dataset <- switch(dataset_name,
                      "rock" = rock,
                      "pressure" = pressure,
                      "cars" = cars)
    summary(dataset)
    }
  })
  
  output$images <- renderImage({
    # Get the filename based on the selected dataset (from the reactive value)
    dataset_name <- selected_dataset()
    if (!is.null(dataset_name)) {
     filename <- normalizePath(file.path('./www', paste('image', dataset_name, '.jpg', sep='')))
     list(src = filename, height = 200)
    }
  }, deleteFile = FALSE)
  
  # Show the first "n" observations ----
  # The use of isolate() is necessary because we don't want the table
  # to update whenever input$obs changes (only when the user clicks
  # the action button)
  output$view <- renderTable({
    dataset_name <- selected_dataset()
    if (!is.null(dataset_name)) {
      dataset <- switch(dataset_name,
                        "rock" = rock,
                        "pressure" = pressure,
                        "cars" = cars)
      head(dataset, n = isolate(input$obs))
    }
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)