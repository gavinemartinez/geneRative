#' Create the raw data needed to plot a circle
#'
#' @param radius A number
#' @param res A number
#'
#' @return A dataframe with xy coordinates that will plot a circle
#'
#' @export
#'

create_circle <- function(radius, res = 100) {

  stopifnot(radius > 0 & radius < 1)

  halfres <- res / 2

  x <- runif(halfres, min = -radius, max = radius)
  x <- as.matrix(x)
  x <- rbind(x, x)

  y <- sqrt(radius^2 - x^2)
  y[1:halfres] <- -1 * y[1:halfres]

  result <- data.frame(x, y)

  return(result)

}

