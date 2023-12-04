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
                             prompt = 'Please generate a descriptive alt text for a chart using the information below. Use the following formula: "Chart type (scatter plot, bar chart, line chart, etc.) + summary of the data (change in temperature, growth of sales, distribution of types of energy generation, etc.) + what it shows and why is it interesting (general trend, interesting outliers, etc.).',
                             prompt_system = "You are a tool for helping ggplot2 users generate alt text in order to make their charts more accessible and SEO-friendly. You will be given a list of geoms used in the plot, the labels (like title, subtitle and annotations) and the data used to make the chart. Your job is to use that information to write alt text. You only return alt text, no other comments or feedback is necessary.",
                             api_key = Sys.getenv("OPENAI_API_KEY"),
                             model = "gpt-3.5-turbo-1106",
                             temperature = 0,
                             max_tokens = 2000,
                             top_p = 1,
                             frequency_penalty = 0,
                             presence_penalty = 0) {

  # Determine the AI model we're using
  model_type <- function(model) {
    if (api_key == "" | model == "manual") return("manual")
    if (model %in% c("gpt-4", "gpt-4-0314", "gpt-4-32k", "gpt-4-32k-0314", "gpt-3.5-turbo", "gpt-3.5-turbo-0301", "gpt-3.5-turbo-1106")) return("chat")
    else if (model %in% c("text-davinci-003", "text-davinci-002", "text-curie-001", "text-babbage-001", "text-ada-001", "davinci", "curie", "babbage", "ada")) return("completion")
    else stop("Model not recognised.")
  }

  # Create prompt text
  prompt_geoms <- lapply(plot[['layers']], function(x) x$constructor[[1]]) |>
    unlist() |>
    paste0(collapse = ", ")

  prompt_labs <- paste0(names(plot[['labels']]), ": ", plot[['labels']], collapse = "\n")

  prompt_data <- readr::format_csv(plot$data)

  prompt_figs <- paste0("Geom(s): ", prompt_geoms, "\n\n",
                        "Labels:", "\n", prompt_labs, "\n\n",
                        "Data as CSV:", "\n", prompt_data)

  prompt_text <- paste0(ifelse(model_type(model) == "manual", paste0(prompt_system, "\n\n"), ""),
                        prompt, "\n\n\n",
                        prompt_figs)

  # If there's no API key supplied
  if (model_type(model) == "manual") {

    cli::cli_alert_info('Open {.href [ChatGPT](https://chat.openai.com/)} and paste the contents of your clipboard in the chat box. You can set an OpenAI key to generate the text within R.')

    clipr::write_clip(prompt_text)

  } else {
    # If there's an API key supplied
    prompt_text <- gsub("\\n", "\\\\n", prompt_text) # format new lines for the API

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
      data <- jsonlite::toJSON(c(params, list(prompt = prompt_text)), auto_unbox = TRUE)

    } else if (model_type(model) == "chat") {

      # The chat endpoint is different than the completion one
      api_endpoint <- "/v1/chat/completions"

      # It also needs a different kind of prompt
      # https://platform.openai.com/docs/guides/chat/chat-vs-completions
      prompt <- list(list(role = "system", content = prompt_system),
                     list(role = "user", content = prompt_text))

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
    } else if (model_type(model) == "chat") {
      res <- res$choices[[1]]$message$content
    } else {
      stop("Model not recognised.")
    }

    res <- stringr::str_trim(res) # Get rid of whitespace

    cli::cli_alert_info(res)
  }
}
