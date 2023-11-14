#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(janitor)
library(dplyr)

df <- read_csv("analysis_data.csv") %>% 
  clean_names %>% 
  mutate_at('ln_r_rlifespan', as.numeric) 

dl <- 
  df %>% 
  filter(ln_r_rlifespan != "NA")

categoricalVars <- c("class","order","family","sex_determination")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Longevity"),
    plotOutput("diffPlot"), # this plot will come from the server
    selectInput(
      inputId = "color_select",
      label = "Select Categorical Variable",
      choices = categoricalVars
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$diffPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        ggplot(dl) + 
          aes(x=as.numeric(ln_r_rlifespan),
              y=species,
              color=.data[[input$color_select]]) + 
          geom_point() +
        labs(x="log of lifespan") +
        geom_vline(xintercept = 0) +
        theme(
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()
        )
        # notes about plot: fix vertical spead, remove species with no data
      })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
