library(shiny)
library(tidyverse)
source("misc.R")


# input <- list(P_Age = 35, P_ASA = 2, checkboxes = c("cns", "cancer")) # EXAMPLE


function(input, output) {
  make_pred <- reactive(pred(input))

  output$p <-
    renderText({
      sprintf("%.2f %%", make_pred())
    })

  output$p_certainty <-
    renderText({
      p <- make_pred()
      if (p > 97) "Around"
      else if (p > 94) "Not too far from"
      else if (p > 90) "We are very unsure but probably at least"
      else if (p > 80) "We do not know but hopefully at least"
      else             "We really have no idea but hopefully at least"
    })
}
