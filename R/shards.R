#' Function that creates generative art based on user input
#'
#' @param iterations number of figures to create
#' @param min minimum value for canvas
#' @param max maximum value for the canvas
#'
#' @return generative art
#'
#' @export
shards <- function(iterations, min, max){
  tri <- data.frame(x = runif(iterations, min = min, max = max),
                    y = runif(iterations, min = min, max = max))

  canvas <- ggplot(tri) +
    geom_polygon(aes(x = x, y = y, fill = sample(colors(), 1)), color = "white", alpha = .3) +
    theme_void() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "black"))

  return(canvas)
}
