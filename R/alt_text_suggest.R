#' @#' Generate alt text automatically
#'
#' Pass a plot to genereate alt text for it using GPT
#'
#' @param plot_name The plot to check for alt text
#' @param prompt A question you want to ask GPT. Something like "Summarise this as a table".
#' @param prompt_system The system message helps set the behaviour of the AI. It can be something like "You are a helpful assistant."
#' @param api_key API key for GPT.
#' @param model Which model to use? If an API key is provided, it uses it to return the alt text to the console. Otherwise, it directs the user to ChatGPT.
#' @export
#' @title Generate alt text automatically
#' @description Generate alt text automatically
#' @return Prints the alt text to console
#' @details Generate alt text automatically.
#' @seealso
#'  \code{\link[ggplot2]{get_alt_text}}
#' @rdname alt_text_suggest
#' @importFrom cli cli_alert_info
#' @importFrom clipr write_clip
#' @importFrom jsonlite toJSON
#' @importFrom httr POST add_headers content
#' @importFrom stringr str_trim
alt_text_suggest <- function(plot,
                             prompt = 'Please generate a descriptive alt text for the the following ggplo2 chart, using the following formula: "Chart type (scatter plot, bar chart, line chart, etc.) + summary of the data (change in temperature, growth of sales, distribution of types of energy generation, etc.) + what is notable about the data (outliers, general trend, etc.).',
                             prompt_system = "You are a tool for helping ggplot2 users generate alt text in order to make their charts more accessible and SEO-friendly. You expect a ggplot2 object, look for relevant labels such as title, subtitle and annotations (if available) and return useful alt text. You only return alt text, no other comments or feedback is necessary.",
                             api_key = Sys.getenv("OPENAI_API_KEY"),
                             model = NULL,
                             temperature = 0,
                             max_tokens = 2000,
                             top_p = 1,
                             frequency_penalty = 0,
                             presence_penalty = 0) {

  # TODO: check for alternative models/AIs
  # https://github.com/mitvis/vistext


  # default model
  if (base::is.null(model)) {
    model <- "gpt-3.5-turbo-1106"
  }

  # determine the model we're using
  model_type <- function(model) {
    if (api_key == "" | model == "manual") return("manual")
    if (model %in% c("gpt-4", "gpt-4-0314", "gpt-4-32k", "gpt-4-32k-0314", "gpt-3.5-turbo", "gpt-3.5-turbo-0301", "gpt-3.5-turbo-1106")) return("chat")
    else if (model %in% c("text-davinci-003", "text-davinci-002", "text-curie-001", "text-babbage-001", "text-ada-001", "davinci", "curie", "babbage", "ada")) return("completion")
    else stop("Model not recognised.")
  }

  # this is an option for those without an API key
  if (model_type(model) == "manual") {

    cli::cli_alert_info('Open {.href [ChatGPT](https://chat.openai.com/)} and paste the contents of your clipboard in the chat box.')
    prompt <- paste0(prompt_system, "\n\n",
                     prompt, "\n\n\n",
                     paste0(as.character(plot[c('labels', 'layers', 'data')]), collapse = "\n")) |>
      clipr::write_clip()

  } else {

    plot_to_send <- plot[c('labels', 'layers', 'data')] |>
      as.character() |>
      paste0(collapse = "\\n")

    plot_to_send <- gsub("\\n", "\\\\n", plot_to_send) # format new lines for the API
    prompt <- paste0(prompt, "\\n", plot_to_send) # combine prompt and text

    # These are useful to make the model more consistent (or not if that's what you want)
    params <- list(
      model = model,
      temperature = temperature,
      max_tokens = max_tokens,
      top_p = top_p,
      frequency_penalty = frequency_penalty,
      presence_penalty = presence_penalty
    )

    # Do we use the chat or the completion endpoint? Chat is cheaper for now.
    # https://platform.openai.com/docs/models/model-endpoint-compatibility
    if (model_type(model) == "completion") {

      api_endpoint <- "/v1/completions"
      prompt <- paste0(prompt_system, " ", prompt, "\n\n", plot_to_send)
      data <- jsonlite::toJSON(c(params, list(prompt = prompt)), auto_unbox = TRUE)

    } else if (model_type(model) == "chat") {

      # The chat endpoint is different than the completion one
      api_endpoint <- "/v1/chat/completions"

      # It also needs a different kind of prompt
      # https://platform.openai.com/docs/guides/chat/chat-vs-completions
      prompt <- list(list(role = "system", content = prompt_system),
                     list(role = "user", content = prompt))

      # Create a the JSON to send to API
      data <- jsonlite::toJSON(c(params, list(messages = prompt)), auto_unbox = TRUE)
    }

    # Send the request
    res <- httr::POST(url = paste0("https://api.openai.com", api_endpoint),
                      httr::add_headers(.headers = c(
                        `Content-Type` = "application/json",
                        `Authorization` = paste0("Bearer ", api_key)
                      )),
                      body = data)

    # message(paste0("Used ", res$usage$total_tokens, " tokens, of which ", res$usage$prompt_tokens, " for the prompt, and ", res$usage$completion_tokens, " for the completion."))

    if (res$status_code == 200) {
      res <- res |> httr::content()
    } else {
      res <- res |> httr::content()
      warning(res$error$message)
      res <- NA
    }

    # Parse response
    if (model_type(model) == "completion") {
      res <- res$choices[[1]]$text
    } else if (model_type(res$model) == "chat") {
      res <- res$choices[[1]]$message$content
    } else {
      stop("Model not recognised.")
    }

    res <- stringr::str_trim(res) # Get rid of whitespace

    cli::cli_alert_info(res)
    # return(res)
  }
}
