# library(ggplot2)
#
# starwars <- dplyr::starwars |>
#   tidyr::drop_na(height, mass, gender)
#
# starwars_plot <- starwars |>
#   ggplot(aes(x = height, y = mass, colour = gender)) +
#   geom_point(size = 2) +
#   # geom_text(data = data.frame(x = 146.191969292745, y = 502.175988449265, label = "Some test text"),
#   #           mapping = aes(x = x, y = y, label = label), size = 6,
#   #           inherit.aes = FALSE) +
#   annotate(geom = "text", x = 146.191969292745, y = 502.175988449265, label = "Some test text", size = 6) +
#   scale_y_continuous(labels = scales::label_comma()) +
#   scale_colour_manual(values = c("red", "darkgreen")) +
#   theme_get() +
#   theme(axis.text.x = element_text(size = 9.9))
#
# starwars_plot
#
#
#
#
# plot_build <- ggplot2::ggplot_build(starwars_plot)
#
#
# plot_build_labels <- Filter(function(x) any(c("label", "text") %in% names(x)), plot_build$data)
#
# plot_build_txt <- as.character(plot_build)
#
# clipr::write_clip(plot_build_txt)
#
#
# plot_build$layout$panel_params[[1]]$y$get_labels()
#
#
# get_text_sizes <- function(plot) {
#
#   plot_build <- ggplot2::ggplot_build(starwars_plot)
#
#   # Annotations and geom_text
#   plot_build_labels <- Filter(function(x) any(c("label", "text") %in% names(x)), plot_build$data)
#
#   # Text size
#   plot_build[["plot"]][["theme"]][["text"]][["size"]]
#
#
# }
