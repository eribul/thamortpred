library(shiny)
library(shinythemes)
library(shinyBS)
library(tidyverse)
library(flexdashboard)

source("misc.R")

fluidPage(

  # https://github.com/ebailey78/shinyBS/issues/88
  singleton(tags$head(tags$script(src = "pop_patch.js"))),

  navbarPage(
    tags$style(HTML(".navbar-default .navbar-nav > li > a {color:white;}")),
    tabPanel("Svenska", icon = icon("stripe-s"), value = "se"),
    tabPanel("English", icon = icon("flag"), value = "en"),
    id = "lang"
  ),

  titlePanel(textOutput("text_title")),
  helpText(p(textOutput("text_intro"))),

  sidebarPanel(
    p(textOutput("text_details")),
    uiOutput("age"),
    uiOutput("sex"),
    uiOutput("asa"),
    uiOutput("comorbs"),

  ),

  mainPanel(
    tabsetPanel(
      type = "tabs",

      tabPanel(
        textOutput("text_tab_p"),
        h1(textOutput("text_your_prob")),
        p(textOutput("p_certainty")),
        gaugeOutput("gauge", width = "40%", height = 125),

        h1(textOutput("text_nnt_title")),
        uiOutput("NNT"),
        plotOutput("nntplot")
      ),

      tabPanel(
        textOutput("text_tab_about"),
        uiOutput("about")
      )
    )
  ),

  title = "Mortality predictor",
  theme = "shpr.css"
)
