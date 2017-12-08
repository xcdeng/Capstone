library(shiny)


shinyUI(navbarPage("Car selection heler",
                   tabPanel("Selection",
                            # Sidebar
                            sidebarLayout(
                              sidebarPanel(
                                helpText("Provide trip detail and car preference"),
                                sliderInput('dis', 'Distance (in miles):', min = 1, max = 1000,value=c(1,1000),step=10),
                                sliderInput('cost', 'Gasoline Price (per gallon):', min = 2, max = 4, value=c(2,4),step=0.01),
                                sliderInput('gas', 'Maximum expenditure on gasoline:', min=1, max=1000,value=c(1,1000), step=10),
                                sliderInput('disp', 'Displacement', min=70, max=480, value=c(70,480), step=10),
                                sliderInput('hp', 'Gross horsepower', min=50, max=340, value=c(50,340), step=10),
                                selectInput('cyl', 'Number of cylinders:', c("4"=4, "6"=6, "8"=8), selected = c(4,6,8)),
                                selectInput('am', 'Transmission:', c("Automatic"=0, "Manual"=1), selected = c(0,1))
                              ),
                              mainPanel(
                                dataTableOutput('table')
                              )
                            )
                   ),
                   tabPanel("About this shiny App",
                            mainPanel("This report is part of project of Data Science and used as peer assessment for the coursera developing data products class. 
                                      please refer to datasets mtcars for raw data and 'https://github.com/xcdeng/Developing-Data-Product' for R code and 'http://rpubs.com/xcdeng/336959' for the presentation "
                           
                            )
                   )
))   