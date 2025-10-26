library(nycflights13)
library(dplyr)
library(ggplot2)

#' @title Visualize Mean Airport Delays
#' @description
#' Creates a plot visualizing the mean arrival delay of flights for different
#' NYC airports, plotted by their longitude and latitude.
#' This function uses data from the \code{nycflights13} package.
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' \dontrun{
#'   visualize_airport_delays()
#' }
visualize_airport_delays <- function() {

  full_data <- flights %>% inner_join(airports, by=c("origin"= "faa"))

  arrival_delay <- full_data %>% group_by(origin, lon, lat) %>% summarise(mean_arrival_delay = mean(arr_delay, na.rm = TRUE))

  departure_delay <- full_data %>% group_by(origin, lon, lat) %>% summarise(mean_departure_delay = mean(dep_delay, na.rm = TRUE))

  p1 <- ggplot(arrival_delay, aes(x = lon,
                                 y = lat,
                                 size = mean_arrival_delay,
                                 color = mean_arrival_delay)) +
    geom_point(alpha = 0.7) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red",
                          midpoint = 0) +
    labs(
      title = "Mean Arrival Delay",
      x = "Longitude",
      y = "Latitude",
      size = "Mean Delay (min)",
      color = "Mean Delay (min)"
    ) +
    theme_minimal()

  p2 <- ggplot(departure_delay, aes(x = lon,
                                 y = lat,
                                 size = mean_departure_delay,
                                 color = mean_departure_delay)) +
    geom_point(alpha = 0.7) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red",
                          midpoint = 0) +
    labs(
      title = "Mean Departure Delay",
      x = "Longitude",
      y = "Latitude",
      size = "Mean Delay (min)",
      color = "Mean Delay (min)"
    ) +
    theme_minimal()

  return(list(arrival_plot = p1, departure_plot = p2))


}

visualize_airport_delays()

