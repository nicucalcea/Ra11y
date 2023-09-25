#' Test plot for colour blindness
#'
#' Find if a plot is colour-blind safe
#'
#' @param plot The plot you want to check
#' @param mode The tests you want to perform
#'
#' @examples
#' test_plot(plot_name, mode = "protan")
#'
#' @export
test_plot <- function(plot, test = c("cvd", "alt")) {

  if ("cvd" %in% test) {
    # start by extracting colours and fills of the plot
    plot_build <- ggplot2::ggplot_build(plot)
    # plot_colours <- unique(c(plot_build$data[[1]][["fill"]], plot_build$data[[1]][["colour"]], plot_build$data[[1]][["color"]]))
    plot_colours <- unique(unlist(lapply(plot_build$data, function(x) {try(c(x$colour, x$color, x$fill))})))
    plot_colours <- plot_colours[!is.na(plot_colours)]

    # test the colours for colour blindness
    if (length(plot_colours) > 1) {
      cvd_list <- test_colourblind(colours = plot_colours)

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
    check_alt_text(plot)
  }
}
