#' @#' Test plot for colour blindness and alt text
#'
#' Find if a plot is colour-blind safe and return its alt text
#'
#' @param plot The plot you want to check
#' @param test The tests you want to perform
#' @examples
#' library(ggplot2)
#' starwars <- dplyr::starwars |>
#'   tidyr::drop_na(height, mass, gender)
#'
#' starwars_plot <- starwars |>
#'   ggplot(aes(x = height, y = mass, colour = gender)) +
#'   geom_point(size = 4) +
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
#'  \code{\link[Ra11y]{test_colourblind}}, \code{\link[Ra11y]{check_alt_text}}
#'  \code{\link[cli]{cli_h1}}, \code{\link[cli]{cli_alert}}, \code{\link[cli]{cli_ul}}, \code{\link[cli]{cli_text}}
#' @rdname test_plot
#' @importFrom ggplot2 ggplot_build
#' @importFrom cli cli_h1 cli_alert_danger cli_ul cli_text
test_plot <- function(plot, test = c("cvd", "alt")) {

  if ("cvd" %in% test) {
    # start by extracting colours and fills of the plot
    plot_build <- ggplot2::ggplot_build(plot)
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
    Ra11y::check_alt_text(plot)
  }
}
