plot.time.variation <- function(database, xvar, yvar, title.label = c("Title"),
                                sub.lab = c(NULL), cap.lab = c("re_ml_02"),
                                prediction = FALSE, mod1 = NULL){
  if(prediction == FALSE){
    plot_1 <- database|>
      ggplot(aes(x = xvar, y = yvar))+
      geom_line(aes(color = Location))+
      labs(x = expression(paste("Time")),
           y = expression(paste("GPP [", mu,"mol CO"[2], " m"^-2, "s"^-1, "]")),
           title = paste(title.label),
           subtitle = paste("KNN model:",sub.lab),
           caption = paste("AGDS Report Exercise",cap.lab))+
      theme_bw()+
      theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
      theme(legend.background = element_rect( fill = "lightblue",
            size = 0.5, linetype = "solid", colour = "black"))+
      # add an individual panel boarder around the whole graph
      theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
            panel.background = element_rect(fill = "white"),
            plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))
    return((plot_1))
  }else{plot_1 <- df_test_predicted.mod1 <- database |>
    drop_na()

  df_test_predicted.mod1$fitted <- predict(mod1, newdata =   df_test_predicted.mod1)

  residue.mod1 <- tibble("Time" = database$TIMESTAMP,
                         "Residue" = database$GPP_NT_VUT_REF - df_test_predicted.mod1$fitted,
                         "Location" = database$Location)

  plot_1 <- ggplot(data = residue.mod1)+
    geom_line(aes(x = residue.mod1$Time, y = residue.mod1$Residue, color = Location))+
    labs(x = expression(paste("Time")),
         y = expression(paste("GPP [", mu,"mol CO"[2], " m"^-2, "s"^-1, "]")),
         title = paste(title.label),
         subtitle = paste("KNN model",sub.lab),
         caption = paste("AGDS Report Exercise",cap.lab ))+
    theme_bw()+
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
    theme(legend.background = element_rect(fill = "lightblue",
          size = 0.5, linetype = "solid", colour = "black"))+
    # add an individual panel boarder around the whole graph
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))
  return(plot_1)
  }

}

