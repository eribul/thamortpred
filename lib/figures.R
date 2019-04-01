
# We have more data than we need for a plot. Simplify by loess model
# if more than 50 uniqe points on either axis
loesspreds <- function(data) {
  if (length(unique(data$sensitivity)) < 50 |
      length(unique(data$specificity)) < 50) {
    return(data)
  }
  spec <- seq(0, 1, 0.01)
  sens <- loess(sensitivity ~ specificity, data) %>%
    predict(spec)
  tibble(specificity = spec, sensitivity = sens)
}
