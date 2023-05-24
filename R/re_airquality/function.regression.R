airquality.regression <- function(airquality_mutated, xvar, lab){
  P_1 <- airquality_mutated |>
    # Make the function dynamic {{xvar}} is a flexible way to change the x axes
    ggplot(aes(x = {{xvar}}, y = Ozone, color = Group)) +
    geom_point(alpha = 0.5)+
    scale_y_continuous(limits = c(0, 250)) +
    # Use smooth for a linear regression, se for standard error
    geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
    # Show the RSQ on the plot
    stat_poly_eq(use_label( "R2")) +
    geom_hline(yintercept = 60, linetype = "dotdash",
               size = 0.3, color = "darkmagenta") +
    # Lab the axes and give the graphic a title
    labs(title = paste("Linear Regression: Ozone [ppm] vs. ", lab),
         x = lab, y = "Ozone [ppb]",
         caption = "AGDS Report Exercise re_airquality (Chapter 4)")+
    # Chose a background and add a panel boarder
    theme_bw() +
    # Plot title bold
    theme(plot.title = element_text(size = 15, face = "bold")) +
    # Add an individual panel boarder
    theme(panel.border = element_rect(colour = "black",
                                      fill = NA, linewidth = 1)) +
    # Add a panel boarder
    theme( legend.background = element_rect( fill = "lightblue",
                                             size = 0.5, linetype = "solid", colour = "black")) +
    # Add an individual panel boarder around the whole graph
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1)) +
    facet_wrap(~Group)
  return(P_1)
}
