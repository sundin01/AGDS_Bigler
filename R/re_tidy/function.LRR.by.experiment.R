LRR.by.experiment <- function(database){
  # Use kable()
  out.print <- knitr::kable(database |>
    group_by(Experiment) |>
    # Apply both methods and round the result
    summarise(LRR_Method_1 =
               round(log(mean(`mean ambient CO2`) /  mean(`mean increased CO2`)), digits = 3),
              LRR_Method_2 =
                round(mean(log(`mean ambient CO2`/ `mean increased CO2`)), digits = 3)))
  # Return the data frame
  return(out.print)
}