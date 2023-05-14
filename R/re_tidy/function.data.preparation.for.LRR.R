data.preparation.for.LRR <- function(database){
  new.database <- database|>

    # Group the database according to the "Experiment" column
    group_by(Experiment) |>

    # Select only the columns which are needed
    dplyr::select(Experiment,`Time (years)`, starts_with("mean")) |>

    # Filter the database for all experiments that run less than three years
    dplyr::filter(`Time (years)`< 3) |>

    # Call this phase "early"
    mutate(Phase = "early") |>

    # Use "bind_row" because not all rows have been used yet.
    # The phase for the other rows should be in the same column.
    # Iterate the procedure for all the other phases
    bind_rows(database |>
                dplyr::select(Experiment,`Time (years)`,starts_with("mean")) |>
                dplyr::filter(`Time (years)`  >= 3 & `Time (years)` <= 6) |>
                mutate(Phase = "mid")) |>

    bind_rows(database |>
                dplyr::select(Experiment,`Time (years)`, starts_with("mean")) |>
                dplyr::filter(`Time (years)` > 6) |>
                mutate(Phase = "late")) |>

    group_by(Experiment) |>

    # Select only the columns which are needed
    dplyr::select("Experiment", "mean ambient CO2", "mean increased CO2", "Phase")
  # return a data frame
  return(new.database)
}
