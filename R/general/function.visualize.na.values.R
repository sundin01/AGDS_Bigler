visualize.na.values <- function(database){
  # Plot the NA with percentage
  plot_1 <- vis_miss(database)

  # Plot NA per group
  plot_2 <- gg_miss_var(database, facet = Month)

  # Create a cowplot
  out.plot <- cowplot::plot_grid(plot_1, plot_2)
  return(out.plot)
}

visualize.na.values.without.groups <- function(database){
  plot_1 <- vis_miss(database)

  plot_2 <- gg_miss_var(database)

  out.plot <- cowplot::plot_grid(plot_1, plot_2)
  return(out.plot)
}
