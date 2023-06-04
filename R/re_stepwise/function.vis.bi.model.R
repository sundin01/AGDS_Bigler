vis.bi.model <- function(database){
  plot_1 <- database|>
    ggplot(aes(x = database$PPFD_IN, y = database$GPP_NT_VUT_REF)) +
    geom_point(alpha = 0.5) +
    # create the model
    geom_smooth(formula = y~x, method = "lm", se = TRUE) +
    labs(title = "Bivariate lineare Regression Model",
         y = expression(paste("GPP [", mu,"mol CO"[2], " m"^-2, "s"^-1, "]")),
         x = expression(paste("PPFD_IN [", mu,"mol Photon", " m"^-2, "s"^-1, "]")),
         caption = "AGDS Report re_stepwise (Chapter 8)") +
    # Add some important parameters
    stat_poly_eq(use_label(c("eq", "R2")), label.x = "right",
                 label.y = "bottom",) +
    theme_bw()+
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))


  # Return the plot
  return(plot_1)
}
