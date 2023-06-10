#' Generates random walk art
#'
#' @param num_steps the number of 'steps' taken on the walk
#' @param seed a number
#' @param line_thickness the thickness of the line
#'
#' @return Generative art!
#'
#' @import ggplot2
#' @import viridis
#'
#' @export
#'

walk_art <- function(num_steps = 1000, seed = 18, line_thickness = 1) {

  set.seed(seed)

  # Initialize the variables at (0,0)

  x <- 0
  y <- 0

  # Initialize the data frame

  walk <- data.frame(
    x = numeric(num_steps),
    y = numeric(num_steps)
  )

  for (i in 1:num_steps) {
    direction <- sample(1:4, 1)

    if (direction == 1) {
      x <- x + 1
    } else if (direction == 2) {
      x <- x - 1
    } else if (direction == 3) {
      y <- y + 1
    } else if (direction == 4) {
      y <- y - 1
    }

    walk[i, "x"] <- x
    walk[i, "y"] <- y
  }

  #Plot the function - get rid of the grids and axes to make it look like art!
    ggplot(walk, aes(x = x, y = y)) +
      geom_path(size = line_thickness, aes(color = seq(0, 1, length.out = num_steps))) +
      scale_color_viridis_c(option = "magma") +
      theme_void() +
      theme(legend.position = "none")
}

