mpe <- function(err) {
  round(mean(err), 2)
}

rmse <- function(err) {
  round(sqrt(mean(err ^ 2)), 2)
}

r2 <- function(obs, est) {
  round(cor(obs, est) ^ 2, 2)
}

zeropop_err <- function(zeropop, pop) {
  round((length(zeropop[zeropop == 0 & pop > 0]) / length(pop[pop > 0])) * 100, 2)
}

