density.airquality <- function(airquality_mutated){
  plot_1 <- airquality_mutated|>
    ggplot(aes(x = Ozone, group = Group, fill = Group)) +
    # add the geom_density and create an individual setup
    geom_density(adjust = 1.5, alpha = 0.4) +
    # add title and subtitle and label the axes
    labs(x = "Ozon [ppb]", y = "Density",
         title = "Density of the ozone concentration",
         subtitle = "At Roosevelt Island, NYC [Average concentration between 1pm and 3pm]",
         caption = "AGDS Report Exercise 2 (Chapter 4)") +
    # add a vline with an individual setup
    geom_vline(xintercept = 60, size = 0.3,
               linetype = "dotdash", color = "darkmagenta") +
    geom_vline(xintercept = median(airquality_mutated$Ozone, na.rm = TRUE),
               size = 0.3, linetype = "dotdash", color = "royalblue") +
    ggplot2::annotate("text", x = 110 , y =  0.0218, hjust = 0,
                      label = "Overall median [31.5 ppb]", color = "royalblue") +
    ggplot2::annotate("text", x = 110 , y =  0.0235, hjust = 0,
                      label = "Threshold value [60 ppb]", color = "darkmagenta") +
    # chose a background and add a panel boarder
    theme_bw() +
    # plot the title bold
    theme(plot.title = element_text(size = 15, face = "bold")) +
    # add a panel boarder
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
    theme( legend.background = element_rect( fill = "lightblue",
                                             size = 0.5, linetype = "solid", colour = "black")) +
    # add a panel boader around the whole graph
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))
  return(plot_1)
}
