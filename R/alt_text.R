#' Print alt text to console
#'
#' Pass a vector of colours and find out if they are colour-blind safe
#'
#' @param colours The colours you want to check
#' @param mode The tests you want to perform
#'
#' @examples
#' test_colours(c("red", "green", "darkgreen"), mode = "protan")
#'
#' @export
check_alt_text <- function(plot_name, alt_text_loc = "console", save_filepath) {
  alt_text <- ggplot2::get_alt_text(plot_name)
  alt_text_set = alt_text != ""
  console <- "console" %in% alt_text_loc
  file <- "file" %in% alt_text_loc

  cli::cli_h1("Alt text")

  # If alt text hasn't been set
  if (!alt_text_set) {
    # Nudge to write some alt text
    cli::cli_alert_danger('Did you forget to include alt text? You can do so in {.run labs(alt = "This is some text describing the chart.")}.')
    # If alt text has been set
  } else {
    if (console) {
      # Write out alt text to console if they've asked for it
      cli::cli_alert_info(alt_text)
    }
  }

  # Save alt text to file
  if (file) {
    alt_text_file <- gsub(paste0(tools::file_ext(save_filepath), "$"), "txt", save_filepath)
    if (console) {
      cli::cli_alert_info(paste0("Alt text written out to: ", alt_text_file))
    }
    writeLines(alt_text, alt_text_file)
  }
}
