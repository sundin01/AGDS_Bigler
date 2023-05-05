time.variation <- function(mod1, mod2, df_test){

  df_test_predicted.mod1 <- df_test |>
    drop_na()

  df_test$fitted <- predict(mod1, newdata =   df_test_predicted.mod1)

  residuen.mod1 <- df_test$fitted - df_test_predicted.mod1$fitted

  df_test_predicted.mod2 <- df_test |>
    drop_na()
  df_test$fitted <- predict(mod2, newdata =  df_test_predicted.mod2)

  residuen.mod2 <- df_test$fitted - df_test_predicted.mod2$fitted

  plot_1 <- ggplot(data = residuen) +
    geom_point(aes(x = df_test$TIMESTAMP , y = residuen.mod1))

  plot_2 <- ggplot(data = residuen) +
    geom_point(aes(x = df_test$TIMESTAMP , y = residuen.mod2))

  out <- cowplot::plot_grid(plot_1, plot_2)

  return(out)
}
