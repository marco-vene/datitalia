#ui.R
library(shiny)
library(readr)
library(leaflet)
library(DT)
library(tidyverse)
library(lubridate)
library(plotly)
library(zoo)
library(ggseas)
library(ggthemes)
library(scales)
library(ggplot2)
library(knitr)
library(caTools)
library(RColorBrewer)
library(eurostat)


######################### raw data ##############################
#setwd("C:/Users/Marco/Google Drive/Code/R/R Projects/Progetto Istat/Shiny App")
source("format_data.R")

##################### css ###################
my_css <- "
#reload {
/* Change the background colour of the action button */
background: red;
}
"


shinyUI( 
  

  #fluidPage(
  navbarPage( 
    img(src = "logo_datitalia.png", height = 30), # "DatItalia",
    #![logo](logo_datitalia.png),
             

  tabPanel("Intro",
           fluidRow(
             column(8,
                    includeMarkdown("Presentazione.md")
             ),
             column(4,
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    img(src = "italy_data.jpg"),
                    br(),
                    br(),
                    img(src = "istat_logo.jpg", width = 400)
             )
             
           )
  ),

  tabPanel("Analizza i Dati",
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    #tags$style(my_css),
    
    # Sidebar panel for inputs 
    sidebarPanel(
      
      # First input: Type of data
      selectInput(inputId = "metrica",
                  label = "Seleziona la metrica:",
                  choices = list("Popolazione" = "Popolazione",
                                 "Occupati" = "Occupati",
                                 "Disoccupati" = "Disoccupati",
                                 "Inattivi" = "Inattivi",
                                 "Tasso di Occupazione" = "Tasso_Occupazione",
                                 "Tasso di Disoccupazione" = "Tasso_Disoccupazione",
                                 "Tasso di Inattivita" = "Tasso_Inattivita")
      ),
   
      selectInput(inputId = "gruppo",
                  label = "Seleziona la dimensione:",
                  choices = list("Totale" = "Totale",
                                 "Territorio" = "Territorio",
                                 "Sesso" = "Sesso",
                                 "Eta" = "ETA1",
                                 "Titolo di Studio" = "Titolo.di.studio",
                                 "Cittadinanza" = "Cittadinanza")
      ), 
      
      checkboxGroupInput('periodo', 'Filtra il periodo:', 
                         choices = levels(dati$Periodo_text),
                         selected = levels(dati$Periodo_text)
                         
      ),
       
      br(),
      
      uiOutput("description")


      # Second input (choices depend on the choice for the first input)
      #uiOutput("secondSelection"),
      
      # Third input (choices depend on the choice for the first and second input)
      #uiOutput("thirdSelection")
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Hide errors
      tags$style(type = "text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
      
      # Output: interactive world map
      plotlyOutput("trendPlot")
      
    )
  )
),

tabPanel("Chi Sono",
         fluidRow(
            column(6,
                   includeMarkdown("Chi Sono.md")
            ),
           column(3,
                  #img(src = "square.jpg")
                  img(src = "square.jpg", height = 407, width = 407)
                  )
           
           )
         )
)

)

