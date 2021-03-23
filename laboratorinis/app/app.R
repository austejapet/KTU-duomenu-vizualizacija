library(shiny)
library(tidyverse)

ui <- fluidPage(titlePanel("949900"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "imones_kodas", label = "Áveskite ámonës kodà",
                     choices = NULL, selected = NULL)
    ),
    mainPanel(tabsetPanel(
      tabPanel("grafikas", plotOutput("plot")),
      tabPanel("lentelë", tableOutput("table"))
      )
    )
    )
  )
server <- function(input, output, session) {
  data <- read_csv("https://github.com/austejapet/KTU-duomenu-vizualizacija/raw/main/laboratorinis/data/lab_sodra.csv")
  data<- filter(data, data$ecoActCode==949900)
  updateSelectizeInput(session, "imones_kodas", choices = data$code, server = TRUE)
  
  output$table <- renderTable(
    data %>%
      filter(code == input$imones_kodas) , digits = 0
  )
  
  output$plot <- renderPlot(
    data %>%
      filter(code == input$imones_kodas) %>%
      ggplot(aes(x = month, y = avgWage, group = name, colour = name)) +
      geom_line(stat="identity") +
      theme(legend.text=element_blank()) +
      theme(panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    )
}
shinyApp(ui, server)

