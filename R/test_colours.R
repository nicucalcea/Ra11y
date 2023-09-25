#' Test colours for colour blindness
#'
#' Pass a vector of colours and find out if they are colour blind safe
#'
#' @param colours The colours you want to check
#' @param mode The tests you want to perform
#'
#' @examples
#' test_colourblind(c("red", "green", "darkgreen"), mode = "protan")
#'
#' @export
# https://github.com/datawrapper/datawrapper/blob/58de8fd51c954cd03d7321c92667648a2a5fa514/services/app/src/lib/components/editor/ColorblindCheck.svelte#L37
test_colourblind <- function(colours, mode = c("deutan", "protan", "tritan"), smallestPercievableDistance = 9.2, ratioThres = 5) {

  # Find combinations of all colours
  colour_combo <- combn(unique(colours), 2)

  # Test each combination
  for (i in 1:ncol(colour_combo)) {

    # Extract two colours at a time
    colour_1 = colour_combo[1, i]
    colour_2 = colour_combo[2, i]

    # Compare how far apart they are
    dstNorm = khroma::compare(c(colour_1, colour_2))

    # Check to see if the distance is too big
    if (dstNorm < smallestPercievableDistance) {
      print("Smalles percievable difference")
    }

    # Function to compare colour blind versions of colours
    compare_cbv <- function(colour_1, colour_2, func) {

      # Transform colours to simulate CVD
      sim_1 = func(colour_1)
      sim_2 = func(colour_2)

      # print(paste0(sim_1, '" "', sim_2))

      # Compare how similar they are
      dstSim = khroma::compare(c(sim_1, sim_2))

      if (dstNorm / dstSim > ratioThres && dstSim < smallestPercievableDistance) {
        # cli::cli_alert_warning(paste0("Two colours (", colour_1, " and ", colour_2, ") might not be colour blind safe."))
        return(TRUE)
      } else return(FALSE)
    }

    # Check if there are any issues
    cvd_issues <- c()
    cvd_list <- list()

    if ("deutan" %in% mode) {
      deutan <- compare_cbv(colour_1, colour_2, colorspace::deutan)
      if (deutan) cvd_issues <- c(cvd_issues, "deutan")
      if (deutan) cvd_list <- c(cvd_list, list(c(colour_1, colour_2)))
    }

    if ("protan" %in% mode) {
      protan <- compare_cbv(colour_1, colour_2, colorspace::protan)
      if (protan) cvd_issues <- c(cvd_issues, "protan")
      if (protan) cvd_list <- c(cvd_list, list(c(colour_1, colour_2)))
    }

    if ("tritan" %in% mode) {
      tritan <- compare_cbv(colour_1, colour_2, colorspace::tritan)
      if (tritan) cvd_issues <- c(cvd_issues, "tritan")
      if (tritan) cvd_list <- c(cvd_list, list(c(colour_1, colour_2)))
    }

    # # Remove empty list elements
    # cvd_list <- Filter(length, cvd_list)

    # Print out a warning if there are issues
    if (length(cvd_list) > 0) {
      # cli::cli_alert_warning(paste0("There be issues"))
      return(cvd_list)
    }

  }
}
