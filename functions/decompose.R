decompose <- function(feat, featAgg, pop) {
  ifelse (as.numeric(featAgg) == 0, 0, round(as.numeric(feat/featAgg) * pop, digit=1))
}