unit.converter <- function(database){
  new.database <- database |>
    # Change the units and make the data tidy
    mutate(Group = Month,
           Temp = (Temp - 32) / 1.8 + 273.15,
           Wind = Wind * 0.44704,
           Solar.R = Solar.R * 41840,
           Date = make_date(1973, Group, Day)) |>
    select("Date", "Day", "Group", "Ozone", "Temp", "Wind", "Solar.R")

  # Define a order for the rows. We need it for the x-axis of the graphs
  new.database$Group <- factor(new.database$Group,
                                     levels =  c(5, 6, 7, 8, 9),
                                     labels = c("May", "Jun", "Jul", "Aug", "Sept"))
  return(new.database)
}
