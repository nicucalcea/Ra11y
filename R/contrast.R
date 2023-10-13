#' Add the right colour to match the background
#'
#' Given a fill colour, return white or black so text is legible.
#'
#' @param colour The colour to check.
#' @param light_col The light output colour. Defaults to white.
#' @param dark_col The dark output colour. Defaults to black.
#' @examples contrast("darkred") # returns white
#' @examples contrast("yellow") # returns black
#' @export
#' @rdname contrast
#' @importFrom farver get_channel
contrast <- function(colour, light_col = "white", dark_col = "black") {
  out   <- rep(dark_col, length(colour))
  light <- farver::get_channel(colour, "l", space = "hcl")
  out[light < 50] <- light_col
  out
}

#' Add the right colour to match the background
#'
#' Given a fill colour, return white or black so text is legible.
#'
#' @examples
#' library(ggplot2)
#' grid_data <- expand.grid(X = LETTERS[1:10], Y = paste0("var", seq(1, 10)))
#' grid_data$Z <- runif(100, 0, 5)
#'
#' ggplot(grid_data, aes(X, Y, fill = Z)) +
#'   geom_tile() +
#'   geom_text(aes(label = Y, !!!autocontrast))
#' @export
#' @rdname autocontrast
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 after_scale
autocontrast <- ggplot2::aes(colour = ggplot2::after_scale(Ra11y::contrast(fill)))
