time.variation <- function(mod1, mod2, df_test){

  df_test_predicted.mod1 <- df_test |>
    drop_na()

  df_test_predicted.mod1$fitted <- predict(mod1, newdata =   df_test_predicted.mod1)

  residuen.mod1 <- tibble("Time" = df_test$TIMESTAMP,
                          "Residuen" = df_test$GPP_NT_VUT_REF - df_test_predicted.mod1$fitted)

  plot_1 <- ggplot(data = residuen.mod1)+
    geom_point(aes(x = residuen.mod1$Time, y = residuen.mod1$Residuen),
               alpha = 0.3)+
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

  df_test_predicted.mod2$fitted <- predict(mod2, newdata =   df_test_predicted.mod2)

  residuen.mod2 <- tibble("Time" = df_test$TIMESTAMP,
                          "Residuen" = df_test$GPP_NT_VUT_REF - df_test_predicted.mod2$fitted)

  plot_2 <- ggplot(data = residuen.mod2)+
    geom_point(aes(x = residuen.mod2$Time, y = residuen.mod2$Residuen), alpha = 0.3)+
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
