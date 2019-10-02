library(shiny)
library(tidyverse)
source("misc.R")


# input <- list(P_Age = 35, P_ASA = 2, checkboxes = c("cns", "cancer")) # EXAMPLE


function(input, output) {

  output$textout <-
    renderUI({

      inp <- vector("list", nrow(coefs))
      names(inp) <- coefs$coef_name
      inp <- imap(inp, ~ if (.y %in% names(input)) input[[.y]])

      inp[coefs$coef_present %in% input$checkboxes] <- TRUE
      inp <- map(inp, ~ if (is.null(.)) FALSE else .)
      inp <-
        as_tibble(inp) %>%
        mutate(P_Gender = ifelse(P_Gender == "Male", "Man", "Kvinna")
      )

      p <- (1 - predict(fit_brl_any_lean, inp, "response")) * 100

      p <-
        if (p > 99) {
          "Almost 100 %"
        } else if (p < 90) {
          "We do not know!"
        } else {
          sprintf("%s %%", prettyNum(p, digits = 0))
        }

      h1(p)
    })

  output$about <-
    renderUI({

      doc <- tags$html(
        tags$head(
          tags$title('My first page')
        ),
        tags$body(
          h1('My first heading'),
          p('My first paragraph, with some ',
            strong('bold'),
            ' text.'),
          div(id='myDiv', class='simpleDiv',
              'Here is a div with some attributes.')
        )
      )
      doc
    })

}
