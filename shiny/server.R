library(shiny)
library(tidyverse)
library(flexdashboard)
source("misc.R")


# input <- list(P_Age = 35, P_ASA = 2, checkboxes = c("cns", "cancer")) # EXAMPLE


function(input, output) {
  make_pred <- reactive(pred(input))

  output$text_intro <-
    renderText({
      if (input$lang == "se") {
       "Har du höftledsartros och överväger operation med en (cementerad) totalprotes?
        Detta är en standardiserad och väldigt säker operation.
        Av de som opereras i Sverige är det bara 0.03 % som avlider inom 90 dagar
        (dvs 3 av 10 000 patienter). Med ytterligare information om just dig kan
        vi ge en mer anpassad skattning för just din sannolikhet."
      } else {
        "Do you have osteoarthritis of the hip? Do you consider a (cemented) total hip replacement?
        This is a secure and standardized procedure.
        The average risk to die within 90 days is 0.03 % in Sweden (3 deaths for every 10,000 patients).
        You can use this tool to get a more individualized estimate."
      }
    })

  output$p_certainty <-
    renderText({
      p <- make_pred()
      pre <- "Your probability to survive at least 90 days is"
      if      (p > 97) paste(pre, "close to")
      else if (p > 95) paste(pre, "not too far from")
      else "We have too little data from similar patients and are unable to make a reliable prediction."
    })

  output$gauge <-
    renderGauge({
      p <- unname(round(make_pred(), 2))
      if (p < 95) {
        NULL
      } else {
        gauge(
          p,
          min = 90, max = 100, symbol = " %",
          gaugeSectors(success = c(95, 100), warning = c(90, 94))
        )
      }
    })
}
