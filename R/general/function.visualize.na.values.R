visualize.na.values <- function(database){
  plot_1 <- gg_miss_var(airquality, facet = Month)

  plot_2 <- gg_miss_fct(database, Month)

  out.plot <- cowplot::plot_grid(plot_1, plot_2)
  return(out.plot)
}

visualize.na.values.without.groups <- function(database){
  plot_1 <- vis_miss(database)

  plot_2 <- gg_miss_var(database, show_pct = TRUE)

  out.plot <- cowplot::plot_grid(plot_1, plot_2)
  return(out.plot)
}
