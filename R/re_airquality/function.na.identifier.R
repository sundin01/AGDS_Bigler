na.identifier <- function(airquality_mutated){
  # Create a bar plot and mark the missing values with -1
  plot_1 <- airquality_mutated |>
    ggplot(aes(x = Date, y = Ozone, group = Group, color = Group)) +
    # Create a geom bar
    geom_bar(stat = "identity") +
    # Make a geom with points
    geom_point(size = 1.5) +
    # Make a geom with the missing values. change NA to -1
    geom_point(data = airquality_mutated |>
                 mutate_all( ~replace_na(.,-1)) |>
                 dplyr::filter(`Ozone` == -1) , size = 1.5, color = "red") +
    labs(x = "Time [1973]", y = "Ozon [ppb]",
         title = "Daily Ozone concentration",
         subtitle = "At Roosevelt Island, NYC [daily average concentration between 1pm and 3pm]",
         caption = "AGDS Report Exercise re_airquality (Chapter 4)") +
    # Chose a background and add a panel boarder
    theme_bw() +
    # Plot title bold
    theme(plot.title = element_text(size = 15, face = "bold")) +
    # Add an individual panel boarder
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
    # Add a panel boarder
    theme( legend.background = element_rect( fill = "lightblue",
                                             size = 0.5, linetype = "solid", colour = "black")) +
    # Add an individual panel boarder around the whole graph
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))
  return(plot_1)
}
