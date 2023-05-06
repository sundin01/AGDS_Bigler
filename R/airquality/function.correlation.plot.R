correlation.plot <- function(airquality_mutated, method.call){
  met.call <- c(method.call)
  plot_1 <- airquality_mutated |>
    select( Ozone, Wind, Temp, Solar.R) |>
    # chose the right method
    cor(use = "pairwise", method = method.call) |>
    # plot lower triangle with the correlation coefficients
    ggcorrplot(hc.order = TRUE,
               type = "lower", lab = TRUE) +
    # label the graphic
    labs(title = "Correlation overview",
         subtitle = paste("Method:",method.call),
         caption = "AGDS Report Exercise 2 (Chapter 4)") +
    # plot the title bold
    theme(plot.title = element_text(size = 15, face = "bold")) +
    # add an individual panel boarder
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
    theme( legend.background = element_rect( fill = "lightblue",
                                             size = 0.5, linetype = "solid", colour = "black")) +
    # add an individual panel boarder around the whole graph
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))
  return(plot_1)
}
