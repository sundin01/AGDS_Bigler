vis.bi.model <- function(database){
  plot_1 <- database|>
    ggplot(aes(x = database$PPFD_IN, y = database$GPP_NT_VUT_REF)) +
    geom_point(alpha = 0.5) +
    # create the model
    geom_smooth(formula = y~x, method = "lm", se = TRUE) +
    labs(title = "Bivariate lineare Regression Model",
         y = "GPP_NT_VUT_REF", x = "PPFD_IN") +
    # Add some important parameters
    stat_poly_eq(use_label(c("eq", "R2", "AIC"))) +
    theme_bw()

  # Return the plot
  return(plot_1)
}
