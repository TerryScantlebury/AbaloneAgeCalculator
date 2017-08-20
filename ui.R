#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that predicts the age samples of Abalone
shinyUI(fluidPage(

  # Application title
  titlePanel("Abalone Age Calculator"),

  # Sidebar with a slider input for number of samples
  sidebarLayout(
    sidebarPanel(
      h4("Predict Abalone Age:"),
      h5("from Sex and Diameter"),
      radioButtons("sex","Sex:",c("Male" = "M", "Female"="F","Infant"="I")),
      sliderInput("diam","Diameter in mm:", 0.055, 0.650, value = 0.408),
      checkboxInput("showFacets","Show/Hide facets", value = TRUE),
      checkboxInput("showSmooth","Show/Hide Smoother", value = TRUE)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Calculator",br(),
                 plotOutput("distPlot"),
                 h4("Predicted Abalone Age (Rings * 1.5 years):"),
                  textOutput("predAge")),
        tabPanel("Documentation",br(),htmlOutput("doc1"))))
  )
))
