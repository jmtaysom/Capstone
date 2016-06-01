library(shiny)

shinyUI(fluidPage(
  titlePanel("Next Word Predictor"),
  
  fluidRow(
    column(4, wellPanel(
      textInput("sentance", "Enter text ", "Merry")
    )),
    column(8, wellPanel(
      verbatimTextOutput("next.word")
    ))
  )
))