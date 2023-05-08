vis.and.dev.multi.model <- function(tibble.multi.model){
  # We define a new order because R would sort the column alphabetically
  new.level <- c(tibble.multi.model$Predictors)
  tibble.multi.model$Predictors <- factor(tibble.multi.model$Predictors,
                                          levels =  new.level,
                                          labels = new.level)

  # We scale R square because it is easier to interpret if RR and AIC is on the same plot
  tibble.multi.model <- tibble.multi.model|>
    mutate(RR.mutated = round(RR * 10000, digits = 0),
           AIC = round(AIC, digits = 1),
           RR = round(RR, digits = 4))

  plot_1 <- tibble.multi.model|>

    # We use pivot longer...
    pivot_longer(cols = c(tibble.multi.model|>
                            select("RR.mutated", "AIC")|>
                            colnames()), names_to = "Variable", values_to = "Value")|>

    # ...and plot a bar plot
    ggplot(aes(x = `Predictors`, y = `Value`, fill = `Variable`)) +
    geom_bar(position = "dodge", stat = "identity") +
    geom_text(aes(label = `Value`), vjust = 0, hjust = 0, colour = "black", angle = 90) +
    scale_y_continuous(limits = c(0,40000)) +
    scale_fill_discrete(labels=c('AIC', 'RR (scaled with 10`000)')) +
    labs(x = "Predictors [Each variable contains all variables that are left of it]"
         , y = "Value",
         title = "Overview of the development of AIC and RR",
         subtitle = "Development of a multivariate linear Regression model",
         caption = "AGDS Report Exercise 4 (Chapter 8)")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))

  return(plot_1)
}
