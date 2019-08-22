library(shiny)
library(ggplot2)


fluidPage(
  titlePanel("Will you survive 90 days after your surgery?"),

  sidebarPanel(
    sliderInput(
      'age',
      'Age',
      min   = 35,
      max   = 98,
      value = 72,
      step  = 1,
      round = TRUE
    ),

    checkboxInput('sex',    'Male sex'),
    checkboxInput('asa',    'ASA >= 3'),
    checkboxInput('kidney', 'Kidney disease?'),
    checkboxInput('heart',  'Heart condition?'),
    checkboxInput('cns',    'CNS?')
  ),

  mainPanel(
    textOutput(
      "textout"
    )
  )
)
