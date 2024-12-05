library(ggplot2)
library(dplyr)
library(ggthemes)
library(scales)
library(R6)
library(purrr)

Plotter <- R6Class("Plotter",
                   public = list(
                     n = 100,
                     data = NULL,
                     colors = NULL,
                     sizes = NULL,
                     axes_limits = NULL,
                     
                     initialize = function(n = 100) {
                       self$n <- n
                       self$data <- self$generate_random_data()
                       self$colors <- self$generate_random_colors()
                       self$sizes <- self$generate_random_size()
                       self$axes_limits <- self$set_random_axes_limits()
                     },
                     
                     generate_random_data = function() {
                       x <- rnorm(self$n, mean = 5, sd = 2)
                       y <- rnorm(self$n, mean = -5, sd = 3)
                       data.frame(x = x, y = y)
                     },
                     
                     generate_random_colors = function() {
                       sample(colors(), self$n, replace = TRUE)
                     },
                     
                     generate_random_size = function() {
                       runif(self$n, 1, 5)
                     },
                     
                     set_random_axes_limits = function() {
                       xlim <- c(sample(-20:0, 1), sample(0:20, 1))
                       ylim <- c(sample(-20:0, 1), sample(0:20, 1))
                       list(xlim = xlim, ylim = ylim)
                     },
                     
                     create_plot = function() {
                       self$data <- self$data[complete.cases(self$data), ]
                       self$data <- self$data %>%
                         filter(x >= min(self$axes_limits$xlim) & x <= max(self$axes_limits$xlim) &
                                  y >= min(self$axes_limits$ylim) & y <= max(self$axes_limits$ylim))
                       
                       self$colors <- self$colors[1:nrow(self$data)]
                       self$sizes <- self$sizes[1:nrow(self$data)]
                       
                       self$data$color <- self$colors
                       self$data$size <- self$sizes
                       
                       ggplot(self$data, aes(x = x, y = y)) +
                         geom_point(aes(color = color, size = size), alpha = 0.6) +
                         scale_color_identity() +
                         scale_size_continuous(range = c(1, 15)) +
                         xlim(self$axes_limits$xlim) + ylim(self$axes_limits$ylim) +
                         theme_minimal() +
                         theme(
                           plot.title = element_text(hjust = 0.5, size = 16, color = "purple"),
                           axis.text = element_text(size = 14, color = "orange"),
                           axis.title = element_text(size = 16, color = "green")
                         ) +
                         labs(
                           title = "Лучший график в R!",
                           x = "Невероятная X-ось",
                           y = "Потрясающая Y-ось"
                         ) +
                         theme(legend.position = "none") +
                         annotate("text", x = -15, y = 10, label = "Это не график\nЭто шедевр и он прекрасен!", size = 6, color = "red") +
                         annotate("segment", x = -10, xend = 15, y = 5, yend = 5, color = "blue", size = 2) +
                         annotate("segment", x = 0, xend = 0, y = -10, yend = 10, color = "yellow", size = 2)
                     }
                   )
)

create_funny_plot <- function(n = 100) {
  plotter <- Plotter$new(n)
  plotter$create_plot()
}

main <- function() {
  create_funny_plot(100)
}

main()

