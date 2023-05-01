# Function to determine the best fit for a single regression model
model.fitter <- function(dataframe, target.column.nr){
  # Give all column-names to a vector
  col.names <- c(colnames(dataframe))
  # Create a empty vector. We will fill it with the RR for each model
  my.vec <- c()
  aic.vec <- c()
  # For loop over all columns
  for (i in c(1 : ncol(dataframe))) {
    # If the target and the predictor the same variable RR will be 1.
    # Therefore we use a if statement
    if (i != target.column.nr){
      lm.model <- lm(unlist(dataframe[target.column.nr]) ~ unlist(dataframe[i]))
      RR <- summary(lm.model)$r.squared
      aic.vec <- c(aic.vec, extractAIC(lm.model)[2])
      my.vec <- c(my.vec, RR )
    }
    # If the target and the predictor the same, than we want RR = AIC = 0
    else{
      my.vec <- c(my.vec, NA)
      aic.vec <- c(aic.vec, NA)
    }
  }
  # We create a tibble for a proper overview
  my.tibble <- print(tibble("Target" = rep(col.names[target.column.nr],
                                           times = ncol(dataframe)),
                            "Predictor" = col.names,
                            "RR"  = my.vec,
                            "AIC" = aic.vec,
                            "Fit" = ifelse(RR >= max(RR, na.rm = TRUE), "BEST FIT", "NO")))
  return(print(my.tibble))
}
