#' Creates a drawing filled with lots of cool, multicolored bubbles.
#'
#' @param number amount of bubbles
#' @param horizontal_shift shifts the bubbles further to the right depending on the magnitude of the value
#' @param vertical_shift shifts the bubbles further up or down depending on the value (can be negative)
#' @param seed a number
#' @param background_color background color, i.e "red"
#' @param bubble_fill the color inside the circle
#' @param bubble_alpha the transparency of the bubbles
#'
#' @return A nice bubble drawing
#'
#' @import ggplot2
#'
#' @export
#'

bubble_plot <- function(number = 1000,
                        horizontal_shift = 1,
                        vertical_shift = 0,
                        seed = 8,
                        background_color = "white",
                        bubble_fill = "white",
                        bubble_alpha = 0.8){

  if (!(horizontal_shift > 0)) {
    stop("Horizontal Shift must be greater than zero. Please check your inputs.")
  }

  set.seed(seed)

data <- data.frame(
  x = rgamma(number, shape = horizontal_shift),
  y = rnorm(number, mean = vertical_shift),
  color = factor(sample(1:30, number, replace = TRUE)),
  size = runif(number, 0.5, 100)
)

ggplot(data, aes(x = x, y = y, color = color, size = size)) +
  geom_point(shape = 21, fill = bubble_fill, stroke = 0.5, alpha = bubble_alpha, show.legend = FALSE) +
  theme_void() +
  theme(panel.background = element_rect(fill = background_color)) #ChatGPT consulted for code on user inputs for background color,bubble fill, and bubble transparency

}
