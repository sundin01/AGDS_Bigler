regression.calculater <- function(dataframe, target, predictor){
  target.column.nr <- target
  predictor.column.nr <- predictor
  lm.model <- lm(unlist(dataframe[target.column.nr]) ~ unlist(dataframe[ predictor.column.nr]))
  return(summary(lm.model))
}
