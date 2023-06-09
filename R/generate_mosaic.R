#' A function that takes in user specificiations and returns generative "mosaic" art work
#'
#' @param num_boxes number specifying number of points
#' @param spiral_color input of str type specifying color of inward spiral
#' @param background_color input of str type specifying color of background
#'
#'@return A beautiful piece of art :)
#'
#'@export
#'
generate_mosaic <- function(num_boxes = 1000, spiral_color = "random", background_color = "random") {
  # Generate random x and y coordinates
  x <- runif(num_boxes, min = -1, max = 1)
  y <- runif(num_boxes, min = -1, max = 1)

  # Generate random colors
  colors <- sample(colors(), num_boxes, replace = TRUE)
  if(spiral_color == "random"){
    abline_color = sample(colors(), 1)
  }else{
    abline_color = spiral_color
  }

  if(background_color == "random"){
    background_color = sample(colors(), 1)
  }else{
    background_color = background_color
  }

  # Generate random sizes
  sizes <- runif(num_boxes, min = 1, max = 5)
  shapes <- sample(1:10, num_boxes, replace = TRUE)

  # Create the generative art plot
  plot <- ggplot() +
    geom_boxplot(aes(x = x, y = y, fill = colors), color = "black", alpha = .8) +
    geom_abline(colour = abline_color) +
    theme_void() +
    coord_polar() +
    theme(legend.position = "none") +
    theme(panel.background = element_rect(fill = background_color, colour = sample(colors(), 1)))

  return(plot)
}
