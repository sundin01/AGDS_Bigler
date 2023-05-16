time.variation <- function(mod1, mod2=NULL, df_test){

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
         subtitle = "KNN model (k=8)", caption = "AGDS Report Exercise re_ml_01 (Chapter 9)")+
    theme_bw()+
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
    # add an individual panel boarder around the whole graph
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))

  df_test_predicted.mod2 <- df_test |>
    drop_na()

  df_test_predicted.mod2$fitted <- predict(mod2, newdata = df_test_predicted.mod2)

  residue.mod2 <- tibble("Time" = df_test$TIMESTAMP,
                          "Residue" = df_test$GPP_NT_VUT_REF - df_test_predicted.mod2$fitted)

  plot_2 <- ggplot(data = residue.mod2)+
    geom_point(aes(x = residue.mod2$Time, y = residue.mod2$Residue), alpha = 0.3)+
    geom_hline(yintercept = mean(residue.mod2$Residue,na.rm=TRUE),color = "royalblue")+
    labs(x = "Time", y = "Residue",
         title = "Time variation of GPP prediction ",
         subtitle = "linear regression model", caption = "AGDS Report Exercise re_ml_01 (Chapter 9)")+
    theme_bw()+
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
    # add an individual panel boarder around the whole graph
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))

  out <- cowplot::plot_grid(plot_1, plot_2)

  return(out)
}
