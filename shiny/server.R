library(shiny)
library(tidyverse)
library(flexdashboard)
library(shinyBS)
source("misc.R")


# input <- list(P_Age = 35, P_ASA = 2, checkboxes = c("cns", "cancer")) # EXAMPLE


function(input, output, session) {


# Intro texts -------------------------------------------------------------

  output$text_title     <- renderText({get_text(input$lang, "title")})
  output$text_intro     <- renderText({get_text(input$lang, "intro")})
  output$text_details   <- renderText({get_text(input$lang, "details")})
  output$text_tab_p     <- renderText({get_text(input$lang, "tab_p")})
  output$text_tab_about <- renderText({get_text(input$lang, "tab_about")})
  output$text_your_prob <- renderText({get_text(input$lang, "your_prob")})


# Input titles ------------------------------------------------------------

  # Select co-morbidities
  check_names <- reactive({
    if (input$lang == "en") {
      coefs$coef_present[coefs$checkbox]
    } else {
      coefs$coef_present_se[coefs$checkbox]
    }
  })

  TIP <- reactiveValues()
  observe({
    TIP$age_title      <- get_text(input$lang, "help_age_title")
    TIP$age_content    <- get_text(input$lang, "help_age_content")
    TIP$sex_title      <- get_text(input$lang, "help_sex_title")
    TIP$sex_content    <- get_text(input$lang, "help_sex_content")
    TIP$asa_title      <- get_text(input$lang, "help_asa_title")
    TIP$asa_content    <- get_text(input$lang, "help_asa_content")
    TIP$comorb_title   <- get_text(input$lang, "help_comorb_title")
    TIP$comorb_content <- get_text(input$lang, "help_comorb_content")
  })

  output$age <- renderUI({
    popify(
      sliderInput(
        'P_Age',
        renderText({get_text(input$lang, "age")}),
        min = 35, max = 99, value = 72, step = 1, round = TRUE
      ),
      TIP$age_title,
      TIP$age_content,
      placement = "right",
      options = list(container = "body")
    )
  })

  output$sex <- renderUI({
    popify(
      radioButtons(
        "P_Gender",
        renderText({get_text(input$lang, "sex")}),
        choices = c("\u2642", "\u2640"),
        selected = "\u2640",
        inline = TRUE,
      ),
      TIP$sex_title,
      TIP$sex_content,
      placement = "right",
      options = list(container = "body")
    )
  })

  output$asa <- renderUI({
    popify(
      radioButtons(
        "P_ASA",
        renderText({get_text(input$lang, "asa")}),
        choices = c(I = 1, II = 2, III = 3),
        selected = 1,
        inline = TRUE,
      ),
      TIP$asa_title,
      TIP$asa_content,
      placement = "right",
      options = list(container = "body")
    )
  })

  output$comorbs <- renderUI({
    popify(
      checkboxGroupInput(
        "checkboxes",
        renderText({get_text(input$lang, "comorb")}),
        choiceValues = coefs$coef_present[coefs$checkbox],
        choiceNames = check_names()
      ),
      TIP$comorb_title,
      TIP$comorb_content,
      placement = "right",
      options = list(container = "body")
    )
  })


# Estimated probability ---------------------------------------------------

  make_pred <- reactive(pred(input))
  nnt       <- reactive(round(1 / (1 -  make_pred() / 100)))

  output$p_certainty <-
    renderText({
      p <- make_pred()
      pre <- get_text(input$lang, "p_certainty_pre")
      if      (p > 97) paste(pre, get_text(input$lang, "p_certainty_close"))
      else if (p > 95) paste(pre, get_text(input$lang, "p_certainty_nottoofarfrom"))
      else get_text(input$lang, "p_certainty_dono")
    })

  output$gauge <-
    renderGauge({
      p <- unname(round(make_pred(), 2))
      if (p < 95) {
        NULL
      } else {
        gauge(
          p,
          min = 0, max = 100, symbol = " %",
          gaugeSectors(success = c(95, 100), warning = c(90, 94))
        )
      }
    })

  # Numbers needed to treat

  output$text_nnt_title <- renderText({
    if (make_pred() < 95) NULL
    else get_text(input$lang, "nnt_title")
  })

  output$NNT <- renderUI({
    if (make_pred() < 95) NULL
    else HTML(gsub("nnt", nnt(), get_text(input$lang, "nnt")))})

  output$nntplot <- renderPlot({
    if (make_pred() < 95) NULL
    else {
    n <- nnt()
    nsqrt <- ceiling(sqrt(n))

    crossing(x = seq_len(nsqrt), y = seq_len(nsqrt)) %>%
      slice(seq_len(n)) %>%
      mutate(res = sample(c(TRUE, logical(n - 1)))) %>%
      ggplot(., aes(x, y, col = res, size = res)) +
      geom_point() +
      theme_void() +
      theme(legend.position = "none")
    }
  })

  output$about <- renderUI({
    includeMarkdown(paste0("about-", input$lang, ".md"))
  })
}
