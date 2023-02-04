library(shiny)
library(ggplot2)
library(DT)

# Load the dataset for the app
data <- read.csv("licensed_drivers.csv")
states <- sort(unique(data$State))

# Define UI for application that plots features of movies -----------
ui <- fluidPage(
  
  # Theme selector --------------------------------------------------
  shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("united"),
  
  # Application title -----------------------------------------------
  titlePanel("Movie browser"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      # Add two different types of inputs
      # Select which state to display
      selectInput(inputId = "selected_state", 
                  label = "Select State:",
                  choices = states),
      
      # Select which gender group to display
      checkboxGroupInput(inputId = "selected_gender",
                         label = "Select Gender:",
                         choices = c("Female", "Male", "All"),
                         selected = "All"),
      
      # Show data table 
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      
      # Horizontal line for visual separation 
      hr(),hr(),
      
      # Add a download button
      downloadButton("downloadData", "Download Data")
    ),
    
    # Output
    mainPanel(
      
      br(), br(),    # a little bit of visual separation
      # Add the three different types of plots
      plotOutput("linePlot"),
      plotOutput("barPlot"),
      plotOutput("boxPlot"),
      
      # Add the data table
      DT::dataTableOutput(outputId = "driversTable")
    )
  )
)

# Define server function required to create the scatterplot ---------
server <- function(input, output) {
  # Filter the data based on the inputs
  filteredData <- data
  
  # Create a data table
  output$driversTable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = filteredData)
    })
  
}

# Run the application -----------------------------------------------
shinyApp(ui = ui, server = server)

