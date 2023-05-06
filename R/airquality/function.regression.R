airquality.regression <- function(airquality_mutated, xvar, lab){
  dynamic.labs <- c(lab)
  P_1 <- airquality_mutated |>
    ggplot(aes(x = {{xvar}}, y = Ozone, color = Group)) +
    geom_point(alpha = 0.5)+
    scale_y_continuous(limits = c(0, 250)) +
    #use smooth for a linear regression, se for standard error
    geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
    stat_poly_eq(use_label( "R2")) +
    geom_hline(yintercept = 60, linetype = "dotdash",
               size = 0.3, color = "darkmagenta") +
    #lab the axes and giv the graphic a title
    labs(title = paste("Linear Regression: Ozone [ppm] vs. ", dynamic.labs),
         x = dynamic.labs, y = "Ozone [ppm]",
         caption = "AGDS Report Exercise Airquality (Chapter 4)")+
    theme_bw() +
    theme(plot.title = element_text(size = 15, face = "bold")) +
    theme(panel.border = element_rect(colour = "black",
                                      fill = NA, linewidth = 1)) +
    theme( legend.background = element_rect( fill = "lightblue",
                                             size = 0.5, linetype = "solid", colour = "black")) +
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1)) +
    facet_wrap(~Group)
  return(P_1)
}
