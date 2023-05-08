Vis_of_the_develop.bi.model <- function(tibble.bi.model){
  # Wrangle the data
  tibble.bi.model <-  tibble.bi.model|>
    mutate(RR = round(RR, digits = 5))|>
    arrange(desc(RR))

  # Set new levels
  new.level <- c(tibble.bi.model$Predictor)
  tibble.bi.model$Predictor <- factor(tibble.bi.model$Predictor,
                                      levels =  new.level,
                                      labels = new.level)
  # Create a plot
  plot_1 <- tibble.bi.model|>
    ggplot(aes(x = `Predictor`, y = `RR`))+
    geom_bar(position = "dodge", stat = "identity", fill = "turquoise3", color = "black")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    geom_text(aes(label = `RR`), vjust = 0, hjust = 0, colour = "black", angle = 90) +
    scale_y_continuous(limits = c(0 , 0.6)) +
    labs(x = "Predictors"
         , y = "RR",
         title = "Overview of the development RR",
         subtitle = "Development of a bivariate linear Regression model",
         caption = "AGDS Report Exercise 4 (Chapter 8)")+
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))

  # Return the plot
  return(plot_1)
}
