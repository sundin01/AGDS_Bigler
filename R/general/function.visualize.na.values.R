visualize.na.values <- function(database){
  plot_1 <- gg_miss_var(airquality, facet = Month)

  plot_2 <- gg_miss_fct(database, Month)

  out.plot <- cowplot::plot_grid(plot_1, plot_2)
  return(out.plot)
}

