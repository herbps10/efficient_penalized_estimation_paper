simulate <- function(seed = 1, N = 100, G = 5, beta = 2, theta = 0.2, sigma = 0.1) {
  set.seed(seed) 

  active_effects <- rbinom(G, 1, theta) 
  group_effects  <- runif(G, -1, 1) 
   
  D            <- sample(1:G, N, replace = TRUE)
  p_D          <- rep(1 / G, N) 
  group_active <- active_effects[D] 
  group_effect <- group_effects[D] 
   
  X1  <- runif(N)
  X2  <- runif(N)
  X3  <- runif(N)
  X4  <- runif(N)
  X5  <- runif(N)

  pi <- plogis(beta * X1 + group_effect - group_effect * X2) 
  A  <- rbinom(N, 1, pi) 
   
  mu0 <- 2 * X1 - 2 * X2 + 0.5 * X5^2
  mu1 <- 2 * X1 - 2 * X2 + 0.5 * X5^2 + group_active * group_effect  
  mu  <- ifelse(A == 1, mu1, mu0) 

  Y <- rnorm(N, mean = mu, sd = sigma)
   
  tibble( 
    p_D = p_D, 
    pi = pi, 
    group_active, 
    group_effect, 
    D, 
    X1, 
    X2, 
    X3, 
    X4, 
    X5, 
    A, 
    Y, 
    mu, 
    mu0 = mu0, 
    mu1 = mu1, 
    sd_Y = sigma 
  ) 
}
