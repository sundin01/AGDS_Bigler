airquality.boxplot <- function(airquality_mutated){
  # Use package "conflicted" to set a preference
  conflicts_prefer(dplyr::filter)
  conflicts_prefer(ggplot2::annotate)

  # use ggplot to visualize the distribution of ozone for each month
  plot_1 <- ggplot(data = airquality_mutated, aes(x = factor(Group), y = Ozone)) +

    # Boxplot with an individual setup in transparency, size and width
    geom_boxplot(fill = "skyblue", alpha = 0.5, lwd = 0.3, width = 0.5,
            # Red color, individual shape and size for outliers
            outlier.color = "red", outlier.shape = 17, outlier.size = 2) +

    # Set new limits for the y-axis (we want it a little bit higher than max. value)
    scale_y_continuous(limits = c(0, 200)) +

    # Count and write how many values are in the boxplot (without NA!)
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

    # Add an error-bar and make a individual setup
    stat_boxplot(geom = "errorbar", size = 0.3, width = 0.3) +

    # Add mean and make a individual setup
    stat_summary(fun.y = mean, geom = "point", shape = 23, size = 4, color = "orange") +

    # Make the values almost transparent and force the values with a width statement in a colomn
    geom_jitter(width = 0, alpha = 0.1) +

    # Create and add a fine dotdashed line for the threshold values for Switzerland as a reference
    geom_hline(yintercept = 60, color = "darkmagenta", size = 0.3, linetype = "dotdash") +

    # Create and add a dotted line for the median for the ozone concentration
    geom_hline(yintercept =  median(airquality_mutated$Ozone, na.rm = TRUE),
               color = "royalblue", size = 0.3, linetype = "dotdash") +

    # Add label for the Threshold, hjust for a left side alignment
    annotate("text", x =  0.5 , y = 170, hjust = 0,
             label = "Threshold value [60 ppb]", color = "darkmagenta") +

    # Add label for the median, hjust for a left side alignment
    annotate("text", x = 0.5 , y =  155, hjust = 0,
             label = "Overall median [31.5 ppm]", color = "royalblue") +

    # Add label for the outliers, hjust for a left side alignment
    annotate("text", x = 0.5 , y =  140, hjust = 0,
             label = "Outliers", color = "red") +

    # Add label for the mean, hjust for a left side alignment
    annotate("text", x = 0.5 , y =  125, hjust = 0,
             label = "Mean", color = "orange") +

    # Label the graphic and axes
    labs(title = "Ozone concentration [monthly resolution]",
         subtitle = "At Roosevelt Island, NYC [Average concentration between 1pm and 3pm]",
         x = "Time [1973]", y = "Ozone concentration [ppb]",
         caption = "AGDS Report Exercise re_airquality (Chapter 4)") +

    # Chose a background for the graph
    theme_bw() +

    # Write the title bold
    theme(plot.title = element_text(size = 15, face = "bold")) +

    # Add panel boarder for the graph
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +

    # Add a individual panel boarder around the graph
    theme(plot.margin = margin(0.3, 1, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))
  return(plot_1)
}
