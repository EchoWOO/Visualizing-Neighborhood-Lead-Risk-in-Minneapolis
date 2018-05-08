

library(shiny)
library(tidyverse)
library(leaflet)
library(ggmap)
library(sf)
library(rgdal)
library(htmlwidgets)
library(yaml)
library(DT)
library(shinydashboard)
library(shinythemes)


#read in data from folder
Combo_dataset <- read_sf("Combo_dataset.shp")
Community_dataset <- read_sf("Community_dataset.shp")
Neighborhood_dataset <- read_sf("Neighborhood_dataset.shp")


# Initial Shiny Map - Simple ----------------------------------------------
#---------------------------Color palettes all here---------------------
pal_blue6 = c("#154f4a", "#376b66", "#588782", "#7ea8a2", 
              "#a9ccc6", "#d8f2ed")

pal_blue11 = c("#d8f2ed",
               "#bfded8", "#a9ccc6", "#93bab4", "#7ea8a2",
               "#6b9993", "#588782", "#477872", "#376b66", 
               "#265c56", "#154f4a")

# Create color palette for communities on map
paletteContinuousC <- colorNumeric(palette = pal_blue11, domain = Community_dataset$Rate)
paletteContinuousN <- colorNumeric(palette = pal_blue11, domain = Neighborhood_dataset$Rate)


#Set up User Interface
ui <- fluidPage(theme = shinytheme("flatly"),
        titlePanel("Understanding Neighborhood Lead Risk in Minneapolis"),
             tabsetPanel(type = "tabs",
                  tabPanel("About", 
                           fluidRow(
                             column(4,
                                    h1(""),
                                    p("Cities with older housing stock face the challenge of how to best mitigate the risk of lead poisoning in individual residences. 
                                      Absent a systematic approach, a diagnosed child is often the main indicator of lead presence in a home. A data analysis was performed as part of 
                                      the Smart Cities Practicum at the University of Pennsylvania to help proactively identify which residences likely have hazardous lead paint and should be 
                                      targeted for remediation."),
                                    p(strong("Use this app to first explore historical data relevant to lead paint presence in homes (on the second tab) and then see the predicted rates of lead positive homes in your neighborhood (on the third tab)."))),
                                    #hr(),
                                   #h3("To learn more about testing your home for lead, visit the program website: 
                                  #http://www.minneapolismn.gov/health/homes/lead/lead-window"),
                                  # uiOutput("tab"),
                                   #uiOutput("tab2"),
                             column(7,
                                   img(src='image1.png', width = "100%",height = "700px"))),
                           fluidRow(
                             uiOutput("tab"),
                             uiOutput("tab2")
                           )
                           ),
                  tabPanel("Exploring Historical Data", 
                           #p("Select a data variable below to view it on the map:"),
                           fluidRow(
                             column(4,
                                    h1(""),
                                    p("Select a variable from the dropdown below:"),
                                    selectInput(inputId = "SelectedVariable",         #drop down menu -
                                               label = "Census Variable",
                                               selected = FALSE,           #how many can select
                                               selectize = FALSE,
                                               multiple = FALSE,
                                               choices = c("Population", "Ownership", "Rentership", "Vacancy"))),
                             column(7,
                                    plotOutput("test")
                                    )
                               )
                           ),
                                    #img(src='pop.gif', type = "gif", width = "100%",height = "600px")))),
                  tabPanel("Predicted Lead Rates", 
                        fluidRow(
                          column(4,
                                 h1(""),
                                 p("Select your community from the drop down to zoom into your neighborhood and surrounding communities on the map:"),
                                 selectInput(inputId = "SelectedCommunity",         #drop down menu -
                                             label = "Community",
                                             selected = FALSE,           #how many can select
                                             selectize = FALSE,
                                             multiple = FALSE,
                                             choices = c("All",sort(unique(Community_dataset$Name)))),
                                 p("Select your neighborhood from the dropdown to highlight your neighborhood on the map and comparison chart:"),
                                 uiOutput("SelectedNeighborhood"),
                                 # selectInput(inputId = "SelectedNeighborhood",         #drop down menu -
                                 # label = "Neighborhood",
                                 # selected = FALSE,           #how many can select
                                 # selectize = FALSE,
                                 # multiple = FALSE,
                                 # choices = c("All",sort(unique(Neighborhood_dataset$Name)))
                                 # ),
                                 #plotOutput("summaryPlot",width = "180px",height = "200px"),
                                 plotOutput("neighbourhoodPlot",width = "280px",height = "312px")),
                          column(7,
                                 leafletOutput("myratemap",width = "100%",height = "700px"))
                          )
                          ))
          
          )
                  

#dashboardBody(
#tags$head(tags$style(HTML('
#  .modal.in .modal-dialog{
# width:100%;
#  height:100%;
#  margin:0px;
# }
# .modal-content{
# width:100%;
# height:100%;
# }
# ')))
#)


