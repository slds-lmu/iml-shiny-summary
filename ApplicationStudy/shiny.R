library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput(
        'PrObj', 'Choose file to upload',
        accept = c(
          'RDS.',
          'rds.'
        ))
     
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents <- renderTable({
    inFile = input$PrObj
    if (is.null(inFile))
      return(NULL)
    PrediObj = readRDS(inFile$datapath)
    x = PrediObj$data$get.x()
    
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
