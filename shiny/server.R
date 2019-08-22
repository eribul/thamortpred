library(shiny)
library(ggplot2)

function(input, output) {

   # From all_models$tidy[[3]]$estimate %>% dput()
  betas <-
    c(-6.93067750638018,
      1.45504113519804,
      0.726409647846406,
      0.836939634463218,
      0.718668411163381,
      0.772009274919897,
      0.747733943488821)

  output$textout <- renderPrint({

    x <- c(
      1, input$kidney, input$asa, input$cns, input$heart,
      (input$age - 72) / 7.8, input$sex
    )

    y <- sum(betas * x)

    p <- exp(y) / (1 + exp(y))
    p <-  1 - ifelse(is.na(p) & !is.na(y), 1, p)

    sprintf("You have a %f %% chance to survive at least 90 days after surgery!", p)
  })

}
