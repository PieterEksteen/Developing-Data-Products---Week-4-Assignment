library(shiny)
library(fst)
library(dplyr)
library(prophet)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(tictoc)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(lubridate)
library(leaflet)

options("scipen" = 999,"digits"=5)

cat("\n")
cat("Loading UI components . . .")
cat("\n")

ui <- dashboardPage(
    dashboardHeader(title = 'Population Prediction',
                    titleWidth = 500
    ),
    dashboardSidebar(
        tags$style(HTML('.shiny-notification{
                                        position:fixed;
                                        top: calc(50%);
                                        left: calc(50%)}'
        )
        ),
        tags$style(HTML('#tab_selection{
                                        position:fixed;
                                        top: calc(22%);
                                        left: calc(50%)}'
        )
        ),
        tags$style(HTML('#prediction{
                                        position:fixed;
                                        top: calc(59%);
                                        left: calc(20%)}'
        )
        ),
        tags$style(HTML('#prediction_data{
                                        position:fixed;
                                        top: calc(59%);
                                        left: calc(60%)}'
        )
        ),
        tags$style(HTML('#map_screenshot{
                                        position:fixed;
                                        top: calc(59%);
                                        left: calc(38%)}'
        )
        ),
        tags$head(
            tags$style(HTML('#plot{
                                          color: #fff;
                                          background-color:#3063A5;
                                          border-color:#3063A5}'
            )
            )
        ),
        width = 350,
        sidebarMenu(id = 'tabs',
                    menuItem('Application Documentation',
                             tabName = "documentation"),
                    
                    menuItem('Population Prediction',
                             tabName = "prediction"),
                    
                    uiOutput('select_country'),
                    
                    uiOutput('train_dates'),
                    
                    uiOutput('predict_until'),
                    
                    menuItem('Population Map',
                             tabName = 'map'),
                    
                    actionButton("plot", "Generate Plot"),
                    
                    textOutput('text1')
        )
    ),
    dashboardBody(
        
        tags$head(tags$style(HTML('
                      .main-header .logo {
                      font-weight: bold;
                      font-size: 24px;
                      }

                      /* logo */
                      .skin-blue .main-header .logo {
                      background-color: #222d32;
                      }

                      /* logo when hovered */
                      .skin-blue .main-header .logo:hover {
                      background-color: #222d32;
                      }

                      /* navbar (rest of the header) */
                      .skin-blue .main-header .navbar {
                      background-color: #3063A5;
                      }

                      /* active selected tab in the sidebarmenu */
                      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                      background-color: #3063A5;
                      }

                      /* other links in the sidebarmenu when hovered */
                      .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                      background-color: #94a5c8;
                      }

                      /* toggle button when hovered  */
                      .skin-blue .main-header .navbar .sidebar-toggle:hover{
                      background-color: #94a5c8;
                      }

                      /* body */
                      .content-wrapper, .right-side {
                      background-color: #ebebeb;
                      }
                                                '))),
        
        tags$style(".nav-tabs{
                                    background-color: #ebebeb;}

                                    .nav-tabs-custom .nav-tabs li.active:hover a,

                                    .nav-tabs-custom .nav-tabs li.active a {
                                    background-color: transparent;
                                    border-color: transparent;}

                                    .nav-tabs-custom .nav-tabs li.active {
                                    border-top-color: #3063A5;}"
        ),
        tags$head(tags$style(HTML('.main-header .logo{
                                                  font-weight: bold;
                                                  font-size: 24px;}'
        )
        )
        ),
        
        tags$style(type = "text/css", "#pop_map {height: calc(85vh - 80px) !important;}"),
        
        div(id="back",tags$b("Developed by Pieter Eksteen")),
        tags$style(type="text/css", "#back {position:absolute;bottom:0;color:#3063A5;
                   max-width: 100%; width: 100%;}"),
        tabItems(
            tabItem(tabName = 'documentation',
                    fluidRow(
                        box(title = "Application Overview",
                            status = "primary",
                            width = 12,
                            height = 400,
                            HTML(paste(
                                h4("Welcome to the Population Prediction app!"),
                                "<br/>",
                                "The app consists of two main tabs,",
                                "<b>"," Population Prediction", "</b>", " and ",
                                "<b>", "Population Map", "</b>",
                                ". These tabs are explained in more detail below.",
                                "<br/>", "<br/>",
                                "To switch between tabs, simply select the tab on the left of the screen.",
                                sep = ''
                            )),
                            imageOutput('tab_selection')
                        ),
                        tabBox(
                            tabPanel("Population Prediction",
                                     HTML(paste("The ", "<b>","Population Prediction", "</b>", 
                                                " tab consists of 3 inputs and 2 outputs.
                                                The first output is a time series plot and the
                                                second is a table.",
                                                "<br/>", "<br/>",
                                                ' The inputs allow the user to predict the population
                                                for a specific country or combinations of countries.
                                                The training date range can be adjusted using the
                                                slider and the "predict until" year can be selected.',
                                                "<br/>",
                                                'Once all selections are made,
                                                click the "Generate Plot" button to excecute.',
                                                sep = ''
                                     )),
                                     fluidRow(
                                         imageOutput('prediction'),
                                         imageOutput('prediction_data')
                                     )),
                            tabPanel("Population Map",
                                     HTML(paste("The ", "<b>","Population Map", "</b>", 
                                                " tab consists of 1 input and a map output.",
                                                "<br/>", "<br/>",
                                                ' The input allows the user to select a specific
                                                year for which the map should display the population
                                                by country.',
                                                "<br/>",
                                                'Population size is illustrated by the size and
                                                color of the circles. Clicking on the circles
                                                provides more details.',
                                                sep = ''
                                     )),
                                     imageOutput('map_screenshot')
                            ),
                            width = 12,
                            height = 580
                        )
                    )
            ),
            
            tabItem(tabName = "prediction",
                    fluidRow(
                        tabBox(
                            tabPanel(
                                style = "background-color: #ebebeb;",
                                title = "Population Prediction Plot"
                                ,
                                plotlyOutput("prediction_plot",
                                             height = 950)
                            ),
                            tabPanel(title = "Population Prediction Table",
                                     dataTableOutput('prediction_table'),
                                     style = "font-size: 80%;
                                                        width: 100%"
                            ),
                            width = 12,
                            height = 900
                        )
                    )),
            
            tabItem(tabName = 'map',
                    fluidRow(
                        box(title = "Population Map",
                            status = "primary",
                            width = 12,
                            height = 1000,
                            uiOutput('map_year'),
                            leafletOutput("pop_map")
                        )
                    )
            ),
            tabItem(tabName = "set_relations_graph",
                    box(title = "Set Relationships",
                        status = "primary",
                        height = 900,
                        width = 12
                    )
            )
        )
    )
)
