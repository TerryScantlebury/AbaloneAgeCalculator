#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(grid)
library(gridExtra)
dataset <-
  read.csv("data/abalonedata.csv", stringsAsFactors = FALSE)
modelFit <- lm(Rings ~ Diameter + Sex, data = dataset)
modelFitMale <-
  lm(Rings ~ Diameter,
     data = dataset,
     subset = (dataset$Sex == "M"))
modelFitFemale <-
  lm(Rings ~ Diameter,
     data = dataset,
     subset = (dataset$Sex == "F"))
modelFitInfant <-
  lm(Rings ~ Diameter,
     data = dataset,
     subset = (dataset$Sex == "I"))

shinyServer(function(input, output) {
  modelpred <- reactive({
    predict(modelFit,
            newdata = data.frame("Sex" = input$sex, "Diameter" = input$diam))
  })
  modelpredMale <- reactive({
    if (input$sex == "M")
      predict(modelFitMale,
              newdata = data.frame("Sex" = input$sex, "Diameter" = input$diam))
  })
  modelpredFemale <- reactive({
    if (input$sex == "F")
      predict(modelFitFemale,
              newdata = data.frame("Sex" = input$sex, "Diameter" = input$diam))
  })
  modelpredInfant <- reactive({
    if (input$sex == "I")
      predict(modelFitInfant,
              newdata = data.frame("Sex" = input$sex, "Diameter" = input$diam))
  })
  output$distPlot <- renderPlot({
    # generate plots based on inputs from ui.R


    if (input$showFacets) {
      sexM <- factor("M", levels = c("F", "I", "M"))
      sexF <- factor("F", levels = c("F", "I", "M"))
      sexI <- factor("I", levels = c("F", "I", "M"))
      pMale <- ggplot(subset(dataset, Sex == "M"),
                      aes(
                        x = Diameter,
                        y = Rings,
                        colour = sexM
                      ),
                      pch = 19) + geom_point(col = 'blue')
      if (input$sex == "M") {
        df <-
          data.frame(
            "Sex" = input$sex,
            "x" = input$diam,
            "y" = modelpredMale(),
            stringsAsFactors = FALSE
          )
        df$Sex <- factor(df$Sex, levels = c("F", "I", "M"))

        pMale <- pMale + geom_point(aes(
          x = df$x,
          y = df$y,
          col = 'blue'
        ),
        cex = 6.0,
        pch = 19)
      }

      pFemale <-
        ggplot(subset(dataset, Sex == "F"),
               aes(
                 x = Diameter,
                 y = Rings,
                 colour = sexF
               ),
               pch = 19) + geom_point(col = 'red')
      if (input$sex == "F") {
        df <-
          data.frame(
            "Sex" = input$sex,
            "x" = input$diam,
            "y" = modelpredFemale(),
            stringsAsFactors = FALSE
          )
        df$Sex <- factor(df$Sex, levels = c("F", "I", "M"))

        pFemale <- pFemale + geom_point(aes(
          x = df$x,
          y = df$y,
          col = 'red'
        ),
        cex = 6.0,
        pch = 19)
      }

      pInfant <-
        ggplot(subset(dataset, Sex == "I"),
               aes(x = Diameter,
                   y = Rings,
                   col = sexI),
               pch = 19) + geom_point(col = 'green')
      if (input$sex == "I") {
        df <-
          data.frame(
            "Sex" = input$sex,
            "x" = input$diam,
            "y" = modelpredInfant(),
            stringsAsFactors = FALSE
          )
        df$Sex <- factor(df$Sex, levels = c("F", "I", "M"))

        pInfant <- pInfant + geom_point(aes(
          x = df$x,
          y = df$y,
          col = 'green'
        ),
        cex = 6.0,
        pch = 19)
      }
    }
    else {
      df <-
        data.frame("Sex" = input$sex,
                   "x" = input$diam,
                   "y" = modelpred())

      p <- ggplot(dataset, aes(
        x = Diameter,
        y = Rings,
        col = factor(Sex)
      ), pch = 19) + geom_point()

      p <- p + geom_point(aes(
        x = df$x,
        y = df$y,
        col = factor(df$Sex)
      ),
      cex = 6.0,
      pch = 19)
    }


    if (input$showSmooth) {
      if (input$showFacets) {
        pMale <- pMale  + geom_smooth(col = 'blue')
        pFemale <- pFemale  + geom_smooth(col = 'red')
        pInfant <- pInfant  + geom_smooth(col = 'green')
      } else
        p <- p  + geom_smooth()
    }

    if (input$showFacets)
      p <- grid.arrange(pFemale, pInfant, pMale)
    print(p)

  })

  output$predAge <- renderText({
    if (input$showFacets) {
      if (input$sex == "M")
        mtext <-
          predict(modelFitMale,
                  newdata = data.frame("Sex" = input$sex, "Diameter" = input$diam))
      if (input$sex == "F")
        mtext <-
          predict(modelFitFemale,
                  newdata = data.frame("Sex" = input$sex, "Diameter" = input$diam))
      if (input$sex == "I")
        mtext <-
          predict(modelFitInfant,
                  newdata = data.frame("Sex" = input$sex, "Diameter" = input$diam))
    }
    if (!input$showFacets)
      mtext <-
        predict(modelFit,
                newdata = data.frame("Sex" = input$sex, "Diameter" = input$diam))
    paste(round(mtext * 1.5, 1), "years old")
  })

  output$doc1 <- renderPrint({
    doc <- tags$html(tags$head(tags$title('How to use the Abalone Age Calculator')),
                     tags$body(
                       h2('Predicting the age of Abalone from Sex and Diameter.'),
                       br(),
                       h4('Description'),
                       p(
                         'This simple calculator uses lm models to estimate the age of an Abalone from its ',
                         strong('Sex'),
                         ' and its ',
                         strong('Diameter'),
                         '.'
                       ),
                       h4('Arguments'),
                       p(
                         pre(
                           paste('Sex:', '\t', '\t', '\t', 'Sex can be Male, Female or Infant'),
                           paste('Diameter:', '\t', '\t', 'Use the Slider to select a Diameter'),
                           paste('Show/Hide facets:', '\t', 'Show or Hide individual facets'),
                           paste('Show/Hide Smoother:', '\t', 'Show or Hide all smoothers')
                         ),
                         h4('Plots'),
                         p(' Each plot shows the points on a scatter chart with
                           diameter on the x-axis and rings on the y-axis.
                           A loess smoother curve can be shown,
                           and plots can be facetted by sex.
                           The large dot is the predicted value. For facetted
                           plots, only the plot with a prediction has a legend
                           to the right.'),
                         h4('Predicted Value'),
                         p(
                           ' Each model estimates the number of rings given a ',
                           strong('Sex'),
                           'and a ', strong('Diameter'),'. The Age, in years, is 1.5 times the
                           number of rings.'
                         )
                       ),br(),br(),
                       p(
                         'For details on the data set used for this project,
                         please visit
                         https://archive.ics.uci.edu/ml/datasets/abalone'
                       )
                     ))
    cat(as.character(doc))
  })
})
