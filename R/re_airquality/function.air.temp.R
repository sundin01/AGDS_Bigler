
temp.and.air <- function(database){

  plot_1 <- airquality_mutated|>
    ggplot(aes(x = Ozone, Wind, group = Temp, color = Temp))+
    geom_point()+
    labs(x = "Ozon [ppb]", y = "Wind [m/s]",
         title = "Scatter-plot Ozone vs. Wind",
         subtitle = "Wind with a temperature dependency",
         caption = "AGDS Report Exercise re_airquality (Chapter 4)") +

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

