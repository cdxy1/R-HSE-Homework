library(ggplot2)
library(dplyr)
library(ggthemes)
library(scales)

generate_random_data <- function(n = 100) {
  x <- rnorm(n, mean = 5, sd = 2)
  y <- rnorm(n, mean = -5, sd = 3)
  data.frame(x = x, y = y)
}

generate_random_colors <- function(n = 100) {
  colors <- sample(colors(), n, replace = TRUE)
  return(colors)
}

generate_random_size <- function(n = 100) {
  sizes <- runif(n, 1, 5)
  return(sizes)
}

set_random_axes_limits <- function() {
  xlim <- c(sample(-20:0, 1), sample(0:20, 1))
  ylim <- c(sample(-20:0, 1), sample(0:20, 1))
  return(list(xlim = xlim, ylim = ylim))
}

create_funny_plot <- function(n = 100) {
  data <- generate_random_data(n)
  colors <- generate_random_colors(n)
  sizes <- generate_random_size(n)
  axes_limits <- set_random_axes_limits()
  
  ggplot(data, aes(x = x, y = y)) +
    geom_point(aes(color = colors, size = sizes), alpha = 0.6) +
    scale_color_identity() +
    scale_size_continuous(range = c(1, 15)) +
    xlim(axes_limits$xlim) + ylim(axes_limits$ylim) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, color = "purple"),
      axis.text = element_text(size = 14, color = "orange"),
      axis.title = element_text(size = 16, color = "green")
    ) +
    labs(
      title = "График с немного смешными осями",
      x = "Невероятная X-ось",
      y = "Потрясающая Y-ось"
    ) +
    theme(legend.position = "none") +
    annotate("text", x = -15, y = 10, label = "Это не график\nЭто шедевр и он прекрасен!", size = 6, color = "red") +
    annotate("segment", x = -10, xend = 15, y = 5, yend = 5, color = "blue", size = 2) +
    annotate("segment", x = 0, xend = 0, y = -10, yend = 10, color = "yellow", size = 2)
}

main <- function() {
  create_funny_plot(100)
}

main()

