#===================================================
# Joe Duprey
# Practicing Interactive Data Viz and Exploring Algae Data
# Last edited 11/13/2021
#===================================================
library(shiny)
library(ggplot2)
library(tidyverse)
library(shinythemes)
source("viz_metaMDS.R")

#load data
bar_data <- read.csv("./data/total_detections_by_phylum.csv")
bar_data$date <- as.factor(bar_data$date) #date format bugfix 

# UI
#===================================================
ui <- fluidPage(
  theme = shinytheme("darkly"),
  tabsetPanel(
    # stacked barchart tab 
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
    
    # NMDS tab
    tabPanel("NMDS Plot",
    sidebarLayout(
      sidebarPanel(
      radioButtons(inputId = "NMDS_phyla",
                  label = "Select Phyla Filter for NMDS",
                  choices = c("Algae", "Invertebrates", "All Taxa"),
                  selected = c("All Taxa"),
                  inline = F,
                  width = "75%"), 

      radioButtons(inputId = "NMDS_life_history",
                  label = "Select Life History Filter for NMDS",
                  choices = c("Planktonic", "Benthic", "All Life History"),
                  selected = c("All Life History"),
                  inline = F,
                  width = "75%"),

      radioButtons(inputId = "NMDS_location",
                  label = "Select Location Filter for NMDS",
                  choices = c("Hood Canal", "San Juan Island", "Salish Sea"),
                  selected = c("Salish Sea"), 
                  inline = F,
                  width = "75%"),
     
      sliderInput(inputId = "NMDS_n_det",
                  label = paste("Include species with greater than X out of 81 detections in NMDS calculation."),
                  min = 0, max = 60,
                  value = 0,
                  )),
    
      mainPanel(
      plotOutput("NMDS_plot", width = "80%"))
      )))
)

# SERVER
#===================================================
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
    
    phyla_choice_list <- c()
    # checkbox choice logic PHYLA (probably a better way)
    if(input$NMDS_phyla == "Algae"){
      phyla_choice_list <- algae_list
    }
    if(input$NMDS_phyla == "Invertebrates"){
      phyla_choice_list <- invert_list
    }
    if(input$NMDS_phyla == "All Taxa"){
      phyla_choice_list <- c(invert_list, algae_list)
    }
    
    lh_choice_list <- c()
    # checkbox choice logic LIFE HISTORY
    if(input$NMDS_life_history == "Planktonic"){
      lh_choice_list <- plk_only #TODO bug bug bug 
    }
    if(input$NMDS_life_history == "Benthic"){
      lh_choice_list <- ben_only
    }
    if(input$NMDS_life_history == "All Life History"){
      lh_choice_list <- all_life_listry
    }
    
    loc_choice_list <- c()
    # checkbox choice logic LOCATION
    if(input$NMDS_location == "Hood Canal"){
      loc_choice_list <- hood_canal_only
    }
    if(input$NMDS_location == "San Juan Island"){
      loc_choice_list <- SJI_only
    }
    if(input$NMDS_location == "Salish Sea"){
      loc_choice_list <- all_locations
    }
    
    # filter_get_PA_data 
    # INPUTS
    # string list of phyla - expand to other taxanomic divisions later
    # string list of life history - benthic, planktonic etc
    # loc - san juan, hood canal or all sampling sites throughout salish sea
    # integer of minimum detections required for species to be included in NMDS alg.
    filtered_pa_df <- filter_get_PA_data(species.by.sample.alltax, 
                                                             phyla_choice_list, 
                                                             lh_choice_list,
                                                             loc_choice_list,
                                                             input$NMDS_n_det)
    
    jaccard_nmds <- metaMDS(filtered_pa_df$wide_PA, distance = "jaccard")
    jaccard_MDS1 <- jaccard_nmds$points[,1] #store nmds values
    jaccard_MDS2 <- jaccard_nmds$points[,2] #store nmds values 
    
    jaccard_to_plot <- cbind(filtered_pa_df$metadata, jaccard_MDS1, jaccard_MDS2)
    print(jaccard_to_plot)
    
    ggplot(jaccard_to_plot, aes(x=jaccard_MDS1, y=jaccard_MDS2)) +
      geom_point(size=3, aes(color=factor(site))) +  # shape=factor())
      theme_bw() +
      labs(x="NMDS1",y="NMDS2", color="Site") +
      ggtitle(paste("Community Structure of -", input$NMDS_life_history, "-", 
                    input$NMDS_phyla, "- throughout", input$NMDS_location)) # + geom_text(aes(label=sample))
    
  })
  
}

shinyApp(ui = ui, server = server)
