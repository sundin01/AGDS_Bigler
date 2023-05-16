time.variation.ml.02 <- function(mod1, df_test_1, lab.model=NULL,
                                 label = NULL, caption = NULL, my.filter = NULL){

  if(is.null(my.filter)==TRUE){
    my.filter <- c("January", "February", "March",
                            "April", "May",  "June",
                            "July", "August", "September",
                            "October", "November", "December")}

  df_test_predicted_1 <- df_test_1 |>
    drop_na()
  df_test_predicted_1$fitted <- predict(mod1, newdata =   df_test_predicted_1)


  residue <- tibble("Time" = df_test_predicted_1$TIMESTAMP,
             "Residue" = df_test_predicted_1$GPP_NT_VUT_REF - df_test_predicted_1$fitted,)|>
    mutate(Group = as.character(month(Time)))|>
    mutate(Group = recode(Group, "1" = "January", "2" = "February", "3" = "March",
                          "4" = "April", "5" = "May", "6" = "June",
                          "7" = "July", "8" = "August", "9" = "September",
                          "10" = "October", "11" = "November", "12" = "December"))

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
    group_by(Group)|>
    dplyr::filter(Group == my.filter)|>
    ggplot(aes(x = Time, y = Residue))+
    geom_point(alpha = 0.5)+
    labs(x = "Time", y = "Residue",
         title = "Time variation of GPP prediction",
         subtitle = paste(lab.model,label ), caption = paste("AGDS Report Exercise", caption))+
    theme_bw()+
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
    # add an individual panel boarder around the whole graph
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))+
    facet_wrap(~Group)+
    geom_hline(aes(yintercept = mean(`Residue`)), colour="royalblue")

  return(plot_1)
}

time.variation.year <- function(mod1, df_test,label = NULL, caption = NULL){

  df_test_predicted.mod1 <- df_test |>
    drop_na()

  df_test_predicted.mod1$fitted <- predict(mod1, newdata =   df_test_predicted.mod1)

  residue.mod1 <- tibble("Time" = df_test$TIMESTAMP,
                         "Residue" = df_test$GPP_NT_VUT_REF - df_test_predicted.mod1$fitted)

  plot_1 <- ggplot(data = residue.mod1)+
    geom_point(aes(x = residue.mod1$Time, y = residue.mod1$Residue),
               alpha = 0.3)+
    geom_hline(yintercept = mean(residue.mod1$Residue,na.rm=TRUE),color = "royalblue")+
    labs(x = "Time", y = "Residue",
       title = "Time variation of GPP prediction ",
       subtitle = paste("KNN model",label ), caption = paste("AGDS Report Exercise",caption ))+
    theme_bw()+
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
    # add an individual panel boarder around the whole graph
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))


  return(plot_1)
}

