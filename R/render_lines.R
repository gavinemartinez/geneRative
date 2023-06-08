#' Render xy coordinates using lines
#'
#' @param dat A dataframe
#' @param lineAngle A number
#' @param seed A number
#'
#' @return A plot of the rendered dat dataframe
#'
#' @import ggplot2
#'
#' @export
#'

render_lines <- function(dat, lineAngle = 0, seed = 10, ...) {

  set.seed(seed)

  x <- as.matrix(dat[1])
  y <- as.matrix(dat[2])

  res <- as.numeric(length(x))

  lengths <- abs(rnorm(res, mean = 0.05, sd = 0.025))

  x1 <- x - lengths * cos(lineAngle * pi / 180)
  x2 <- x + lengths * cos(lineAngle * pi / 180)

  y1 <- y - lengths * sin(lineAngle * pi / 180)
  y2 <- y + lengths * sin(lineAngle * pi / 180)

  # Adapted from https://stackoverflow.com/questions/27826666/plotting-1000-lines-with-ggplot2

  cdat <- data.frame(x = rbind(x1, x2), y = rbind(y1, y2))

  cdat$line = paste("line", seq(res), sep="")

  p <- ggplot(data = cdat, mapping = aes(x = x, y = y, group = line)) +
    theme_void() +
    theme(panel.grid=element_blank()) +
    geom_line(size=0.4, ...) +
    xlim(-1, 1) +
    ylim(-1, 1)

  return(p)

}


