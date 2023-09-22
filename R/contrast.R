#' Add the right colour to match the background
#'
#' Given a fill colour, return white or black so text is legible.
#'
#' @param colour The colour to check.
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
#' @examples ggplot(df, aes(row, col, fill = value)) +
#'                  geom_raster() +
#'                  geom_text(aes(label = round(value, 2), !!!autocontrast))
#' @export
#' @rdname autocontrast
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 after_scale
autocontrast <- ggplot2::aes(colour = ggplot2::after_scale(contrast(fill)))
