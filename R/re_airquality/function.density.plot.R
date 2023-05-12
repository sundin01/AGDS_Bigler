density.airquality <- function(airquality_mutated){
  plot_1 <- airquality_mutated|>
    ggplot(aes(x = Ozone, group = Group, fill = Group)) +
    # Add the geom_density and create an individual setup
    geom_density(adjust = 1.5, alpha = 0.4) +
    # Add title and subtitle and label the axes
    labs(x = "Ozon [ppb]", y = "Density",
         title = "Density of the Ozone concentration",
         subtitle = "At Roosevelt Island, NYC [Average concentration between 1pm and 3pm]",
         caption = "AGDS Report Exercise re_airquality (Chapter 4)") +
    # Add a vline with an individual setup (for the threshold)
    geom_vline(xintercept = 60, size = 0.3,
               linetype = "dotdash", color = "darkmagenta") +
    # Add a vline for the median
    geom_vline(xintercept = median(airquality_mutated$Ozone, na.rm = TRUE),
               size = 0.3, linetype = "dotdash", color = "royalblue") +
    # Add lable
    ggplot2::annotate("text", x = 110 , y =  0.0218, hjust = 0,
                      label = "Overall median [31.5 ppb]", color = "royalblue") +
    # Add lable
    ggplot2::annotate("text", x = 110 , y =  0.0235, hjust = 0,
                      label = "Threshold value [60 ppb]", color = "darkmagenta") +
    # Chose a background and add a panel boarder
    theme_bw() +
    # Plot the title bold
    theme(plot.title = element_text(size = 15, face = "bold")) +
    # Add a panel boarder
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
    theme( legend.background = element_rect( fill = "lightblue",
                                             size = 0.5, linetype = "solid", colour = "black")) +
    # Add a panel boarder around the whole graph
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))
  return(plot_1)
}
