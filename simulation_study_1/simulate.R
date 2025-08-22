simulate <- function(seed = 1, N = 100, D = 10, sigma = 1, theta = 0.3, dgp = "sparse") {
  set.seed(seed)
  
  if(dgp == "sparse") {
    beta <- rnorm(D, 0, 1) * rbinom(D, 1, theta)
  }
  else if(dgp == "none") {
    beta <- numeric(D)
  }
  else {
    beta <- rnorm(D, 0, 1)
  }
  
  X <- matrix(rbinom(N * D, 1, 0.5), nrow = N, ncol = D)
  #for(d in 1:D) {
  #  X[, d] <- rbinom(N, 1, plogis(0))
  #}
  
  Y <- rnorm(N, X %*% beta, sigma)
  
  data <- as_tibble(X)
  colnames(data) <- paste0("X", 1:D)
  data$Y <- Y
  
  list(
    data = data,
    beta = tibble(variable = paste0("X", 1:D), beta = beta)
  )
}
