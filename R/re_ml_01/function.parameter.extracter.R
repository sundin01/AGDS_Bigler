
extract.mae <- function(mod, df_train){
  conflicts_prefer(dplyr::filter)

  df_train <- df_train |>
    drop_na()

  df_train$fitted <- predict(mod, newdata = df_train)

  metrics_train <- df_train |>
    yardstick::metrics(GPP_NT_VUT_REF, fitted)

  mae_train <- metrics_train |>
    filter(.metric == "mae") |>
    pull(.estimate)
  return(mae_train)
}

extract.rsq <- function(mod, df_train){
  conflicts_prefer(dplyr::filter)

  df_train <- df_train |>
    drop_na()

  df_train$fitted <- predict(mod, newdata = df_train)

  metrics_train <- df_train |>
    yardstick::metrics(GPP_NT_VUT_REF, fitted)

  rsq_train <- metrics_train |>
    filter(.metric == "rsq") |>
    pull(.estimate)
  return(rsq_train)
}

parameter.extracter <- function(own.sequence, df.1, df.2, label = NULL){
  # Set all parameters we need
  add.rsq.1 <- NULL
  add.rsq.2 <- NULL
  add.mae.1 <- NULL
  add.mae.2 <- NULL
  add.k <- NULL
  # initialize the for loop
  for (i in own.sequence) {
    # we save values for each round
    mod.knn <- knn.model(df.1, i)
    current.rsq.1 <- extract.rsq(mod.knn, df.1)
    current.mae.1 <- extract.mae(mod.knn, df.1)
    add.rsq.1 <- c(add.rsq.1, current.rsq.1)
    add.mae.1 <- c(add.mae.1, current.mae.1)
    current.rsq.2 <- extract.rsq(mod.knn, df.2)
    current.mae.2 <- extract.mae(mod.knn, df.2)
    add.rsq.2 <- c(add.rsq.2, current.rsq.2)
    add.mae.2 <- c(add.mae.2, current.mae.2)
    add.k <- c(add.k, i)
  }

  # We make a tibble --> it is easier to read the values
  my.tibble <- tibble("K" = add.k,
                      "RSQ_Train" = add.rsq.1,
                      "MAE_Train" = add.mae.1,
                      "RSQ_Test" = add.rsq.2,
                      "MAE_Test" = add.mae.2)

  # We read the optimal k
  optimal.k <- as.numeric(my.tibble|>
    dplyr::filter(`MAE_Test` == min(`MAE_Test`))|>
    select("K"))
  # We plot the model as a function of complexity
  plot_1 <- my.tibble|>
    # We use pivot longer
    pivot_longer(cols = c(my.tibble|>
                            select(starts_with(c("R", "M")))|>
                            colnames()),
                 names_to = "Variable",
                 values_to = "Value")|>
    ggplot(aes(x = `K`, y = `Value`, color = `Variable`)) +
    # we want a point for each k
    geom_point() +
    # connect all points
    geom_line() +
    # We add a big point for our optimal k
    geom_point(data = my.tibble, aes(x = optimal.k, y = min(my.tibble$MAE_Test),
                                     color = "Optimal K"), size = 4)+
    ggplot2::annotate('text', x = optimal.k, y = min(my.tibble$MAE_Test),
                      hjust = 0.5, vjust = -1, label = paste('k=',optimal.k))+
    # label the graph
    labs(x = "# k", y = "RSQ / MAE",
         title = "Overview of the development of RSQ and MAE",
        subtitle = "by using KNN-Algorithm", caption = paste("AGDS Report Exercise", label))+
    # add our border
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
    theme_bw()+
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) +
    theme( legend.background = element_rect( fill = "lightblue",
                                             size = 0.5, linetype = "solid", colour = "black")) +
    # add an individual panel boarder around the whole graph
    theme(plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "grey90",colour = "black", linewidth = 1))
  return(plot_1)
}


parameter.extracter.tibble <- function(own.sequence, df.1, df.2){
  # Set all parameters we need
  add.rsq.1 <- NULL
  add.rsq.2 <- NULL
  add.mae.1 <- NULL
  add.mae.2 <- NULL
  add.k <- NULL
  # initialize the for loop
  for (i in own.sequence) {
    # we save values for each round
    mod.knn <- knn.model(df.1, i)
    current.rsq.1 <- extract.rsq(mod.knn, df.1)
    current.mae.1 <- extract.mae(mod.knn, df.1)
    add.rsq.1 <- c(add.rsq.1, current.rsq.1)
    add.mae.1 <- c(add.mae.1, current.mae.1)
    current.rsq.2 <- extract.rsq(mod.knn, df.2)
    current.mae.2 <- extract.mae(mod.knn, df.2)
    add.rsq.2 <- c(add.rsq.2, current.rsq.2)
    add.mae.2 <- c(add.mae.2, current.mae.2)
    add.k <- c(add.k, i)
  }

  # We make a tibble --> it is easier to read the values
  my.tibble <- tibble("K" = add.k,
                      "RSQ_Train" = add.rsq.1,
                      "MAE_Train" = add.mae.1,
                      "RSQ_Test" = add.rsq.2,
                      "MAE_Test" = add.mae.2)

  return(my.tibble)
}
