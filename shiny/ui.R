library(shiny)
library(shinythemes)
library(tidyverse)

source("misc.R")

fluidPage(
  titlePanel("Probability to survive at least 90 days after THA surgery?"),

  helpText("Bla bla bla ..."),

  sidebarPanel(
    sliderInput(
      'P_Age',
      'Age',
      min   = 35,
      max   = 99,
      value = 72,
      step  = 1,
      round = TRUE
    ),

    radioButtons(
      "P_Gender",
      "Sex",
      choices = c("Male", "Female"),
      selected = "Female",
      inline = TRUE,
    ),

    radioButtons(
      "P_ASA",
      "ASA class",
      choices = 1:3,
      selected = 1,
      inline = TRUE,
    ),

    checkboxGroupInput(
      "checkboxes",
      "Do you have any of the following:",
      choices = coefs$coef_present[coefs$checkbox],
    )

  ),

  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel("Prediction", htmlOutput("textout")),
      tabPanel("About", htmlOutput("about"))
    )
  ),

  theme = shinytheme("superhero")
)
