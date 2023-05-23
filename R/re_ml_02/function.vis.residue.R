time.variation.ml.02 <- function(mod1, df_test_1, lab.model=NULL,
                                 label = NULL, caption = NULL, my.filter = NULL){

  if(is.null(my.filter)==TRUE){
    my.filter <- c("January", "February", "March",
                            "April", "May",  "June",
                            "July", "August", "September",
                            "October", "November", "December")}

  df_test_predicted_1 <- df_test_1 |>
    drop_na()

  # Predict the test data and write it into the fitted column
  df_test_predicted_1$fitted <- predict(mod1, newdata =   df_test_predicted_1)

  # Create a tibble. It is much easier to handle it
  residue <- tibble("Time" = df_test_predicted_1$TIMESTAMP,
             "Residue" = df_test_predicted_1$GPP_NT_VUT_REF - df_test_predicted_1$fitted)|>
    mutate(Group = format(Time,"%B"))|>
    mutate(rolling = rollmean(Residue,12, align="left", fill = NA, na.rm = TRUE))

  residue$Group <- factor(residue$Group,
                          levels =  c("January", "February", "March",
                                      "April", "May",  "June",
                                      "July", "August", "September",
                                      "October", "November", "December"),
                          labels = c("January", "February", "March",
                                     "April", "May",  "June",
                                     "July", "August", "September",
                                     "October", "November", "December"))

  plot_1 <- residue|>
    dplyr::filter(Group == my.filter)|>
    ggplot(aes(x = Time, y = Residue))+
    geom_line()+
    geom_smooth(method = "loess")+
    labs(x = "Time", y = "Residue",
         title = "Time variation of GPP prediction",
         subtitle = paste(lab.model,label ), caption = paste("AGDS Report Exercise", caption))+
    theme_bw()+
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
    # add an individual panel boarder around the whole graph
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))+
    facet_wrap(~Group)

  return(plot_1)
}

time.variation.year <- function(mod1, df_test,label = NULL, caption = NULL, plot = TRUE){

  df_test_predicted.mod1 <- df_test |>
    drop_na()

  df_test_predicted.mod1$fitted <- predict(mod1, newdata =   df_test_predicted.mod1)

  # We do not pipe from here, because we implemented a if/else statement
  residue.mod1 <- tibble("Time" = df_test$TIMESTAMP,
                         "Residue" = df_test$GPP_NT_VUT_REF - df_test_predicted.mod1$fitted)

  plot_1 <- ggplot(data = residue.mod1)+
    geom_line(aes(x = residue.mod1$Time, y = residue.mod1$Residue))+
    geom_smooth(aes(x = residue.mod1$Time, y = residue.mod1$Residue), method = "loess", se = FALSE)+
    labs(x = "Time", y = "Residue",
       title = "Time variation of GPP prediction ",
       subtitle = paste("KNN model",label ), caption = paste("AGDS Report Exercise",caption ))+
    theme_bw()+
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
    # add an individual panel boarder around the whole graph
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))

  if(plot == TRUE){
    return(plot_1)
  }else{return(residue.mod1)}

}

