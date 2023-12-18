#' Test text contrast for accessibility
#'
#' Pass two hex codes and receive a contrast ratio
#'
#' @param col1 The first colour
#' @param col2 The second colour
#' @examples
#' contrast_ratio("black", "white")
#' contrast_ratio("yellow", "white")
#'
#' @export
#' @title #' @#' Test
#' @description Pass two colours and receive a contrast ratio
#' @return Returns a contrast ratio.
#' @details Pass two colours and receive a contrast ratio. Courtesy of coloratio.
#' @rdname contrast_ratio
#' @importFrom grDevices colors
contrast_ratio <- function(col_1, col_2) {

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

#' Test text contrast for accessibility
#'
#' Pass two hex codes and print the result of a contrast check
#'
#' @param col1 The first colour
#' @param col2 The second colour
#' @examples
#' check_threshold("black", "white")
#' check_threshold("yellow", "white")
#'
#' @export
#' @description Pass two colours and print the result of a contrast check
#' @details Pass two colours and print the result of a contrast check.
#' @rdname contrast_check
#' @importFrom grDevices colors
#' @importFrom purrr map2
#' @importFrom cli cli_h1 cli_alert_danger cli_bullets
#' @importFrom farver decode_colour encode_colour
contrast_check <- function(col_1, col_2) {

  # Convert to hex codes if necessary
  col_1 = farver::decode_colour(col_1) |> farver::encode_colour()
  col_2 = farver::decode_colour(col_2) |> farver::encode_colour()

  ratio <- Ra11y::contrast_ratio(col_1, col_2)

  col_results <- c(
    AA = 4.5,
    `AA Large` = 3,
    AAA = 7,
    `AAA Large` = 4.5
  )

  result <- purrr::map2(col_results, names(col_results), function(x, y) {
    return_text <- paste0(y, ": ", ifelse(ratio >= x, "Pass", "Fail"))
    return(return_text)
  })

  names(result) <- ifelse(ratio >= col_results, "v", "x")

  if (any(ratio < col_results)) {
    cli::cli_h1("Contrast")
    cli::cli_alert_danger(paste0("Your colours have a colour contrast of ", round(ratio, digits = 2), ":1, which may be below recommendations for some types of text. Consult {.href [WebAIM](https://webaim.org/resources/contrastchecker/?fcolor=", gsub("#", "", col_1), "&bcolor=", gsub("#", "", col_2), ")} for more details."))
    cli::cli_bullets(result)
  }
}
