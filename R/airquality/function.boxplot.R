airquality.boxplot <- function(airquality_mutated){
   # use conflicted package to prefer a package
  conflicts_prefer(dplyr::filter)
  conflicts_prefer(ggplot2::annotate)

  # use ggplot to visualise the distribution of ozone for each month
  plot_1 <- ggplot(data = airquality_mutated, aes(x = factor(Group), y = Ozone)) +

    # Boxplot with an individual setup in transparency, size and width
    geom_boxplot(fill = "skyblue", alpha = 0.5, lwd = 0.3, width = 0.5,
                 # red colour, individual shape and size for outliers
             outlier.color = "red", outlier.shape = 17, outlier.size = 2) +

    # Set new limits for the y-axis (we want it a little bit higher than max. value)
    scale_y_continuous(limits = c(0, 200)) +

    # count and write how many values are in the boxplot (without NA!)
    annotate("text", x = 1:length(table(airquality_mutated$Group)),
             y = 180, label = c(paste("n =", nrow(airquality_mutated |>
             drop_na(Ozone) |>
             filter(Group == "May") |>
             select(Ozone))) , paste("n =", nrow(airquality_mutated |>
             drop_na(Ozone) |>
             filter(Group == "Jun") |>
             select(Ozone))), paste("n =", nrow(airquality_mutated |>
             drop_na(Ozone) |>
             filter(Group == "Jul") |>
             select(Ozone))), paste("n =", nrow(airquality_mutated |>
             drop_na(Ozone) |>
             filter(Group == "Aug") |>
             select(Ozone))), paste("n =", nrow(airquality_mutated |>
             drop_na(Ozone) |>
             filter(Group == "Sept") |>
             select(Ozone)))), col = "black", vjust = -1) +

    # add an errorbar and make a individual setup
    stat_boxplot(geom = "errorbar", size = 0.3, width = 0.3) +

    # add mean and make a individual setup
    stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, color = "orange") +

    # make the values almost transparent and force the values with width statemant in a colomn
    geom_jitter(width = 0, alpha = 0.1) +

    # create and add a fine dotdashed line for the threshold values for Switzerland as a reference
    geom_hline(yintercept = 60, color = "darkmagenta", size = 0.3, linetype = "dotdash") +

    # create and add a dotted line for the median for the ozone concentration
    geom_hline(yintercept =  median(airquality_mutated$Ozone, na.rm = TRUE),
               color = "royalblue", size = 0.3, linetype = "dotdash") +

    # add label for the Threshold, hjust for a left side alignment
    annotate("text", x =  0.5 , y = 170, hjust = 0,
             label = "Threshold value [60 ppb]", color = "darkmagenta") +

    # add label for the median, hjust for a left side alignment
    annotate("text", x = 0.5 , y =  155, hjust = 0,
             label = "Overall median [31.5 ppm]", color = "royalblue") +

    # add label for the outliers, hjust for a left side alignment
    annotate("text", x = 0.5 , y =  140, hjust = 0,
             label = "Outliers", color = "red") +

    # add label for the mean, hjust for a left side alignment
    annotate("text", x = 0.5 , y =  125, hjust = 0,
             label = "Mean", color = "orange") +

    # label the graphic and axes
    labs(title = "Ozone concentration per month",
         subtitle = "At Roosevelt Island, NYC [Average concentration between 1pm and 3pm]",
         x = "Time [1973]", y = "Ozone concentration [ppb]",
         caption = "AGDS Report Exercise 2 (Chapter 4)") +

    # chose a background for the graph
    theme_bw() +

    # write the title bold
    theme(plot.title = element_text(size = 15, face = "bold")) +

    # add panel boarder for the graph
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +

    # add a individual panel boarder around the graph
    theme(plot.margin = margin(0.3, 1, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))
  return(plot_1)
}
