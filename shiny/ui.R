library(shiny)
library(shinythemes)
library(shinyBS)
library(tidyverse)
library(flexdashboard)

source("misc.R")

fluidPage(

  navbarPage(
    tags$style(HTML(".navbar-default .navbar-nav > li > a {color:white;}")),
    tabPanel("Svenska", icon = icon("stripe-s"), value = "se"),
    tabPanel("English", icon = icon("flag"), value = "en"),
    id = "lang"
  ),

  titlePanel("90-day survival after total hip replacement"),

  helpText(textOutput("text_intro")),

  sidebarPanel(
    p("Please provide some personal details so we can estimate your probability:"),
    sliderInput(
      'P_Age',
      'Age',
      min   = 35,
      max   = 99,
      value = 72,
      step  = 1,
      round = TRUE
    ),

    bsPopover(
      "P_Age",
      "Age at surgery",
      "How old will you be at the time of your planned hip replacement?",
      placement = "right",
      options = list(container = "body")
    ),

    radioButtons(
      "P_Gender",
      "Sex",
      choices = c("Male", "Female"),
      selected = "Female",
      inline = TRUE,
    ),

    bsPopover(
      "P_Gender",
      "Legal gender",
      "What is your legal gender? (Our model was based on legal gender identified by the third digit of Swedish personal identity numbers.)",
      placement = "right",
      options = list(container = "body")
    ),


   radioButtons(
      "P_ASA",
      tagList("ASA class", a("(?)", href = "https://www.asahq.org/standards-and-guidelines/asa-physical-status-classification-system")),
      choices = c(I = 1, II = 2, III = 3),
      selected = 1,
      inline = TRUE,
    ),
    bsPopover(
      "P_ASA",
      "ASA Class",
      "Do you have: normal health (I), a mild systemic disease (II), or a severe systemic disease (III)? Your physician might help you with this assessment.",
      placement = "right",
      options = list(container = "body")
    ),

    checkboxGroupInput(
      "checkboxes",
      "Do you have any of the following:",
      choices = coefs$coef_present[coefs$checkbox],
    ),

    bsPopover(
      "checkboxes",
      "Co-morbidities",
      "Did you have any of the following conditions during the last year: malignany with or without metastases including lymfoma (cancer); dementia, hemiplegia or paraplegia, depression, paralysis, psychoses, or any other neurological disorder (CNS disease); renal disease or failure (kidney disease); or diagnosed obesity?",
      placement = "right",
      options = list(container = "body")
    ),

  ),

  mainPanel(
    tabsetPanel(
      type = "tabs",

      tabPanel(
        "Prediction",
        textOutput("p_certainty"),
        gaugeOutput("gauge"),
      ),

      tabPanel("About", h1("hej"))
    )
  ),

  title = "Mortality predictor",
  theme = "shpr.css"
)
