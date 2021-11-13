#===================================================
# Joe Duprey
# Practicing Interactive Data Viz and Exploring Algae Data
# Last edited 11/13/2021
#===================================================
library(shiny)
library(ggplot2)
library(tidyverse)
source("viz_metaMDS.R")

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
              choiceNames = list(icon("bug"), icon("seedling")),
              choiceValues = list("Arthropoda","Bacillariophyta"),
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
    
    filtered_pa_df <- filter_get_PA_data(species.by.sample.alltax, 
                                                             input$NMDS_phyla, 
                                                             all_life_listry,
                                                             SJI_only,
                                                             0)
    
    jaccard_nmds <- metaMDS(filtered_pa_df$wide_PA, distance = "jaccard")
    jaccard_MDS1 <- jaccard_nmds$points[,1] #store nmds values
    jaccard_MDS2 <- jaccard_nmds$points[,2] #store nmds values 
    
    jaccard_to_plot <- cbind(filtered_pa_df$metadata, jaccard_MDS1, jaccard_MDS2)
    print(jaccard_to_plot)
    
    ggplot(jaccard_to_plot, aes(x=jaccard_MDS1, y=jaccard_MDS2)) +
      geom_point(size=3, aes(color=factor(site))) +  # shape=factor())
      theme_bw() +
      labs(x="PC1",y="PC2", color="Site") +
      ggtitle("Hood Canal Benthic Algae - COI - Jaccard") # + geom_text(aes(label=sample))
    
  })
  
}

shinyApp(ui = ui, server = server)
