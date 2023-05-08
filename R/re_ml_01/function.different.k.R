different.k <- function(own.sequence, df.train, df.test){
  for (i in own.sequence) {
    mod.knn <- knn.model(df.train, i)
    plot(eval_model(mod.knn, df.train, df.test))
  }
  return(mod.knn)
}


different.k.1 <- function(own.sequence, df.train, df.test){
  plot.vec <- NULL
  for (i in own.sequence) {
    mod.knn <- knn.model(df.train,i)
    plot.vec <- c(plot.vec, eval_model(mod.knn, df.train, df.test))
  }
  out.plot <- cowplot::plot_grid(plot.vec, ncol = 2)
  return(out.plot)
}
