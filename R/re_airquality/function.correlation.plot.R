correlation.plot <- function(airquality_mutated, method.call){
  plot_1 <- airquality_mutated |>
    select( Ozone, Wind, Temp, Solar.R) |>
    # Chose the right method
    cor(use = "pairwise", method = method.call) |>
    # Plot lower triangle with the correlation coefficients
    ggcorrplot(hc.order = TRUE,
               type = "lower", lab = TRUE) +
    # Label the graphic
    labs(title = "Correlation overview",
         subtitle = paste("Method:",method.call),
         caption = "AGDS Report Exercise re_airquality (Chapter 4)") +
    # Plot the title bold
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
