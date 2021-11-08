library(shiny)
library(ggplot2)
library(tidyverse)

#load data
data <- read.csv("./data/total_detections_by_phylum.csv")
data$date <- as.factor(data$date)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("Bar Chart",
      checkboxGroupInput(inputId = "phylum",
              label = "Select Phyla",
              choices = c(unique(data$phylum)),
              selected = c("Bangiophyceae", "Compsopogonophyceae", "Phaeophyceae",
                           "Rhodophyta", "Florideophyceae"),
              inline = T,
              width = "75%"),
  
      checkboxGroupInput(inputId = "site",
                     label = "Select Site",
                     choices = c(unique(data$site)),
                     selected = c("FH"),
                     inline = T),
  
      plotOutput("bar")),
  
    tabPanel("PCA Plot"))
)

server <- function(input, output) {
  output$bar <- renderPlot({
    
    data <- data %>%
      filter(phylum %in% input$phylum) %>%
      filter(site %in% input$site)
    
    ggplot(data, aes(fill=phylum, y=n_detections, x=date)) + 
      geom_bar(position="stack", stat="identity") +
      labs(title= paste("Number of Detections by Phylum at", list(input$site)),
           x="Month", y="N Detections")
  })
}

shinyApp(ui = ui, server = server)
