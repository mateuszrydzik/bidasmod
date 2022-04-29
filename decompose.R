decompose <- function(feat, featAgg, pop) {
  weight <- feat/featAgg
  output <- weight * pop
  return(round(output, digit=1))
}