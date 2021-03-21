library(shiny)
library(shinydashboard)
library(tidyverse)


ui <- dashboardPage(
  dashboardHeader(title = "INFO APIE ĮMONES"),
  dashboardSidebar(textInput("imones_kodas", "Įmonės kodas", value = "")),
  dashboardBody(
    fluidRow(
      box(title = "Vidutinio atlyginimo dinamika 2020 metais",
          plotOutput("plot", height = 300), width = 10),
      
      box(title = "Informacija apie įmonę",
          tableOutput("table"), width = 10)
    
    )
  )
)



server <- function(input, output) {
  duomenys <- read_csv("/Users/vytautevail/KTU-duomenu-vizualizacija/laboratorinis/data/sodra_subset.csv")
  
  output$table <- renderTable(
    duomenys %>%
      filter(jarCode == input$imones_kodas) , digits = 0
  )
  
  output$plot <- renderPlot(
    duomenys %>%
      filter(jarCode == input$imones_kodas) %>%
      ggplot(aes(x = month, y = avgWage)) +
      geom_line(color="#e9ecef") +
      geom_point() +
      theme_minimal() 
  )
}



shinyApp(ui = ui, server = server)


