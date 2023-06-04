emp.dist.airquality <- function(airquality_mutated){
  plot_1 <- airquality_mutated|>
    ggplot(aes(Ozone))+
    stat_ecdf(geom = "step")+
    labs(x = "Ozon [ppb]", y = "Density",
         title = "Empirical Cumulative Density Function for Ozone",
         subtitle = "At Roosevelt Island, NYC [Average concentration between 1pm and 3pm]",
         caption = "AGDS Report Exercise re_airquality (Chapter 4)") +
    # Add a vline for the median
    geom_vline(xintercept = 60, na.rm = TRUE,
               size = 0.3, linetype = "dotdash", color = "darkmagenta") +
    # Add lable
    ggplot2::annotate("text", x = 110 , y =  0.0218, hjust = 0,
                      label = "Threshold Value [60 ppb]", color = "darkmagenta") +
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
