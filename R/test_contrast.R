#' @#' Test text contrast for accessibility
#'
#' Pass two colours and receive a contrast ratio
#'
#' @param col1 The first colour
#' @param col2 The second colour
#' @examples
#' test_contrast("black", "white")
#' test_contrast("yellow", "white")
#'
#' @export
#' @title #' @#' Test
#' @description Pass two colours and receive a contrast ratio
#' @return Returns a contrast ratio.
#' @details Pass two colours and receive a contrast ratio. Courtesy of coloratio.
#' @rdname test_contrast
#' @importFrom grDevices colors
test_contrast <- function(col_1, col_2, threshold = 4.5) {

  if(
    (!grepl("^#", col_1) & !col_1 %in% grDevices::colors() |
     !grepl("^#", col_2) & !col_2 %in% grDevices::colors()) |
    (grepl("^#", col_1) & !grepl("^#[0-9a-fA-F]{6}$", col_1) |
     grepl("^#", col_2) & !grepl("^#[0-9a-fA-F]{6}$", col_2))
  ) {
    stop('Inputs must be in colors() if named, or of the hex form "#RRGGBB".\n')
  }

  # Convert colous to RGB and scale 0 to 1
  d <- t(grDevices::col2rgb(c(col_1, col_2))) / 255

  # Convert value
  d <- apply(
    d, 2, function(x) ifelse(
      x <= 0.03928, x / 12.92, ((x + 0.055) / 1.055) ^ 2.4
    )
  )

  # Calculate luminance values
  d <- as.data.frame(d)
  d$L <- (0.2126 * d$red) + (0.7152 * d$green) + (0.0722 * d$blue)

  # Calculate contrast ratio
  d <- d[order(d$L), ]
  cr <- (d[2, "L"] + 0.05) / (d[1, "L"] + 0.05)

  return(cr)

}





#
# # Process:starwars_plot$layers, function(x) {x$constructor[[1]]})
#
#   # Now let's get the index of each geom_text or geom_label
#   layers_text <- which(layers %in% c("geom_text", "ggplot2::geom_text", "geom_label", "ggplot2::geom_label"))
#
#   # Retrieve their colour and fill
#   plot_build <- ggplot2::ggplot_build(plot)
#
#   plot_colours <- plot_build$data[[layers_text]] |>
#     dplyr::select(tidyselect::any_of(c("colour", "fill")))
#
#   # print(plot_colours)
#
#   # Run several checks
#   # Check if the label has a fill colour
#   if ("fill" %in% colnames(plot_colours)) {
#     plot_colours_distinct <- dplyr::distinct(plot_colours) |>
#       rowwise() |>
#       mutate(contrast_ratio = test_contrast(colour, fill),
#              contrast_issue = contrast_ratio <= 4.5)
#   }
#   # If there's no fill, chances are there's another geom in the background
#
#
#   return(plot_colours_distinct)
# }
#
# test_plot_contrast(starwars_plot)
