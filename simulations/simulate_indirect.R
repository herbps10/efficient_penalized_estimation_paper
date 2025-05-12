simulate <- function(seed = 5462, N = 1e3, m = 5, P = 5) {
  set.seed(seed)
  hospital_effect1 <- rbinom(m, 1, 0.5)
  X <- matrix(runif(N * P, 0, 1), ncol = P, nrow = N)
  colnames(X) <- paste0("X", 1:P)
  
  g <- matrix(0, ncol = m, nrow = N)
  g <- 1 + 20 * matrix(ifelse(X[, 1] > 0.7, 1, ifelse(X[, 1] > 0.4, 1.5, 0)), ncol = m, nrow = N, byrow = FALSE) * matrix(hospital_effect1, ncol = m, nrow = N, byrow = TRUE) + matrix(ifelse(X[, 2] > 0.4 & X[, 2] < 0.7, 1, 0), ncol = m, nrow = N, byrow = FALSE) - 0.5 * matrix(X[, 5], ncol = m, nrow = N, byrow = FALSE)

  g <- g / rowSums(g)
  colnames(g) <- paste0("g", 1:m)
  
  A <- map_int(1:N, \(i) sample(1:m, 1, prob = g[i, ]))
  
  trt_indicator <- matrix(0, ncol = m, nrow = N)
  for(a in 1:m) {
    trt_indicator[, a] <- A == a
  }
  
  Qbar <- 2 + 
    matrix((X[, 2])^2, ncol = m, nrow = N, byrow = FALSE) +
    matrix(ifelse(X[, 1] > 0.7, 1, ifelse(X[, 1] > 0.4, 3, 0)), ncol = m, nrow = N, byrow = FALSE) -
    matrix((X[, 4] + 1) * X[,5], ncol = m, nrow = N, byrow = FALSE) - 
    2 * matrix(ifelse(X[, 1] > 0.5, -1, 1), ncol = m, nrow = N) * matrix(hospital_effect1, ncol = m, nrow = N, byrow = TRUE)
  
  colnames(Qbar) <- paste0("Qbar", 1:m)
  
  Qtilde <- rowSums(Qbar * g)
  
  Ya <- matrix(rnorm(N * m, as.vector(Qbar), 0.1), ncol = m, nrow = N, byrow = FALSE)
  colnames(Ya) <- paste0("Y", 1:m)
  
  Y <- rowSums(Ya * trt_indicator)
  YA <- rowSums(Ya * g)

  gamma1 <- rowSums(Qbar * trt_indicator)
  gamma2 <- rowSums(Qbar * g)
  
  cbind(
    as.data.frame(X),
    as.data.frame(g),
    as.data.frame(Qbar),
    as.data.frame(Ya)
  ) %>%
    mutate(Y = Y, A = A, YA = YA, Qtilde = Qtilde, hospital_effect1 = hospital_effect1[A], gamma1 = gamma1, gamma2 = gamma2)
}
