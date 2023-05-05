LRR.by.experiment <- function(database){
  out.print <- knitr::kable(database |>
    group_by(Experiment) |>
    # Apply both methods
    summarise(LRR_Method_1 =
                log(mean(`mean ambient CO2`) /  mean(`mean increased CO2`)),
              LRR_Method_2 =
                mean(log(`mean ambient CO2`/ `mean increased CO2`))))
  return(out.print)
}
