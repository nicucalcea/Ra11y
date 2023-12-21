#' Test plot for colour blindness and alt text
#'
#' Find if a plot is colour-blind safe and return its alt text
#'
#' @param plot The plot you want to check
#' @param test The tests you want to perform
#' @param alt_text_loc Whether you want it displayed in the console, file, clipboard, or a combination
#' @examples
#' library(ggplot2)
#' starwars <- dplyr::starwars |>
#'   tidyr::drop_na(height, mass, gender)
#'
#' starwars_plot <- starwars |>
#'   ggplot(aes(x = height, y = mass, colour = gender)) +
#'   geom_point(size = 4) +
#'   geom_label(aes(label = name), colour = "red", fill = "pink") +
#'   scale_colour_manual(values = c("red", "darkgreen"))
#'
#' test_plot(starwars_plot)
#'
#' @export
#' @import ggplot2
#' @import ggplot2
#' @title Test plot for accessibility
#' @description Find if a plot is colour-blind safe and return its alt text.
#' @param test What test to run, Default: c("cvd", "alt")
#' @return Prints potential accessibility issues to the console.
#' @details Test plot for accessibility.
#' @seealso
#'  \code{\link[ggplot2]{ggplot_build}}
#'  \code{\link[Ra11y]{test_colourblind}}, \code{\link[Ra11y]{alt_text_check}}
#'  \code{\link[cli]{cli_h1}}, \code{\link[cli]{cli_alert}}, \code{\link[cli]{cli_ul}}, \code{\link[cli]{cli_text}}
#' @rdname test_plot
#' @importFrom ggplot2 ggplot_build
#' @importFrom cli cli_h1 cli_alert_danger cli_ul cli_text
test_plot <- function(plot, test = c("cvd", "alt"), alt_text_loc = c("console")) {

  if (any(c("cvd", "contrast") %in% test)) {
    plot_build <- ggplot2::ggplot_build(plot)
  }


  if ("cvd" %in% test) {
    # start by extracting colours and fills of the plot
    # plot_build <- ggplot2::ggplot_build(plot)
    plot_colours <- unique(unlist(lapply(plot_build$data, function(x) {try(c(x$colour, x$color, x$fill))})))
    plot_colours <- plot_colours[!is.na(plot_colours)]

    # test the colours for colour blindness
    if (length(plot_colours) > 1) {
      cvd_list <- Ra11y::test_colourblind(colours = plot_colours)

      # display warning if something is wrong
      if (length(cvd_list) > 0) {
        cli::cli_h1("Colour blindness")
        cli::cli_alert_danger("There may be issues with the following colour combinations, consider changing them:")
        cli::cli_ul(lapply(cvd_list, paste0, collapse = " — "))
        cli::cli_text("Run {.run colorblindr::cvd_grid()} for colour-deficiency simulations of your plot.")
      }
    }
  }


  if ("alt" %in% test) {
    Ra11y::alt_text_check(plot,
                          alt_text_loc = alt_text_loc)
  }


  if ("contrast" %in% test) {
    # Retrieve all geoms in the plot
    plot_layers <- purrr::map(plot$layers, function(x) {x$constructor[[1]]})

    # Get the indices of text layers
    plot_layers_text <- which(plot_layers %in% c("geom_label", "ggplot2::geom_label"))

    if (length(plot_layers_text) > 0) {

      # Get a df of colours and their fills
      plot_colours <- plot_build$data[[plot_layers_text]] |>
        dplyr::select(tidyselect::any_of(c("colour", "fill"))) |>
        dplyr::distinct()

      # Test if label colour is contrasting enough with its fill
      if ("fill" %in% colnames(plot_colours)) {
        purrr::pmap(plot_colours, Ra11y::contrast_check)

        # print(plot_colours)
      }
    }
  }
}
