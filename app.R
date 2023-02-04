library(shiny)
library(ggplot2)
library(DT)
library(dplyr)

# Load the dataset for the app
data <- read.csv("licensed_drivers.csv")
states <- sort(unique(data$State))

# Define UI for application that plots features of movies -----------
ui <- fluidPage(
  
  # Theme selector --------------------------------------------------
  shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("united"),
  
  # Application title -----------------------------------------------
  titlePanel("Licensed Drivers"),
  
  # Sidebar layout with a input and output definitions --------------
  sidebarLayout(
    
    # Inputs: Select variables to plot ------------------------------
    sidebarPanel(
      # Add two different types of inputs
      # Select which state to display
      selectInput(inputId = "selected_state", 
                  label = "Select State:",
                  choices = sort(unique(data$State)),
                  selected = "pennsylvania"),
      
      # Select which gender group to display
      radioButtons(inputId = "selected_gender",
                         label = "Select Gender:",
                         choices = c("Female", "Male")),
      
      # Select which age group to display
      checkboxGroupInput(inputId = "selected_cohort",
                         label = "Select Age group:",
                         choices = sort(unique(data$Cohort))),
      
      # Show data table 
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      
      # Horizontal line for visual separation 
      hr(),
      
      # Add a download button
      downloadButton("downloadData", "Download Data")
    ),
    
    # Output
    mainPanel(
      
      br(), br(),    # a little bit of visual separation
      # Add the three different types of plots
      plotOutput("linePlot"),
      plotOutput("barPlot"),
      plotOutput("scatterPlot"),
      
      # Add the data table
      DT::dataTableOutput(outputId = "driversTable")
    )
  )
)

# Define server function required to create the plots
server <- function(input, output) {
  # Filter the data based on the inputs
  drivers_subset <- reactive({ 
    req(input$selected_gender)
    filter(data, Gender %in% input$selected_gender, State %in% input$selected_state,
           Cohort %in% input$selected_cohort)
  })
  
  
  # Print data table if checked
  output$driversTable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = drivers_subset(), 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
  
  output$linePlot <- renderPlot({
    ggplot(data = drivers_subset(),aes(x=Year, y=drivers_sum, group=Gender, color=Cohort)) + 
      geom_line(aes(group = Cohort)) + 
      xlab('Year')
  })
  
  output$barPlot <- renderPlot({
    ggplot(data = drivers_subset(), aes(x = Year, y = drivers_sum, fill = Cohort)) +
      geom_bar(color = "black",position="stack", stat="identity") + 
      scale_fill_brewer(palette = "Set3") +
      theme_classic()
  })

  output$scatterPlot <- renderPlot({
    ggplot(data = drivers_subset())+
      geom_point(aes(x=Year, y=drivers_sum,
                     color=Cohort))
  })
  
    # Download the filtered data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data_", input$selected_state, "_", input$selected_gender, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(drivers_subset(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

