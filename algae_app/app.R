library(shiny)
library(ggplot2)
library(tidyverse)

#load data
bar_data <- read.csv("./data/total_detections_by_phylum.csv")
bar_data$date <- as.factor(bar_data$date)

#define NMDS checkbox lists
phyla_check_list <- list("Algae"= c("Florideophyceae", "Phaeophyceae", "Bacillariophyta", 
                              "Bangiophyceae", "Compsopogonophyceae", "Rhodophyta"), 
                  "Invertebrates"= c("Cnidaria", "Arthropoda", "Annelida", "Mollusca",
                                     "Bryozoa", "Echinodermata", "Nemertea", "Entoprocta",
                                     "Brachiopoda", "Nematoda"))


ui <- fluidPage(
  tabsetPanel(
    tabPanel("Bar Chart",
      checkboxGroupInput(inputId = "phylum",
              label = "Select Phyla",
              choices = c(unique(bar_data$phylum)),
              selected = c("Bangiophyceae", "Compsopogonophyceae", "Phaeophyceae",
                           "Rhodophyta", "Florideophyceae"),
              inline = T,
              width = "75%"),
  
      checkboxGroupInput(inputId = "site",
                     label = "Select Site",
                     choices = c(unique(bar_data$site)),
                     selected = c("FH"),
                     inline = T),
  
      plotOutput("bar")), 
  
    tabPanel("NMDS Plot",
      checkboxGroupInput(inputId = "NMDS_phyla",
              label = "Select Phyla Filter for NMDS",
              choices = phyla_check_list,
              selected = phyla_check_list, 
              inline = T,
              width = "75%"), #,
      
      plotOutput("NMDS_plot")
      # 
      # checkboxGroupInput(inputID = "NMDS_life_history",
      #         label = "Select Life History Filter for NMDS",
      #         choices = , #TODO
      #         selection = , #TODO
      #         inline = T, #TODO
      #         width = "75%"),
      # 
      # checkboxGroupInput(inputID = "NMDS_location",
      #         label = "Select Location for NMDS",
      #         choices = , #TODO
      #         selection = , #TODO
      #         inline = T, #TODO
      #         width = "75%"),
      # 
      # checkboxGroupInput(inputID = "NMDS_n_detection",
      #         label = "Select Minimum Number of Detections to Appear in NMDS",
      #         choices = , #TODO
      #         selection = , #TODO
      #         inline = T, #TODO
      #         width = "75%")
      ))
)

server <- function(input, output) {
  output$bar <- renderPlot({
    
    bar_data <- bar_data %>%
      filter(phylum %in% input$phylum) %>%
      filter(site %in% input$site)
    
    ggplot(bar_data, aes(fill=phylum, y=n_detections, x=date)) + 
      geom_bar(position="stack", stat="identity") +
      labs(title= paste("Number of Detections by Phylum at", list(input$site)),
           x="Month", y="N Detections")
  })
  
  output$NMDS_plot <- renderPlot({
    
    
  })
  
}

shinyApp(ui = ui, server = server)
