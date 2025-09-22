library(lmridge)

hkb_ridge <- function(data, G) {
  df <- as.data.frame(model.matrix(~Y + A + D + X1 + X2 + X3 + X4 + X5, data = data |> mutate(D = as.factor(D)))) |> select(-`(Intercept)`)
  f <- as.formula(paste0("Y ~ . +", paste0(paste0("A * D", 2:G), collapse = " + ")))
  fit <- lmridge(f, data = df)
  fit2 <- lmridge(f, data = df, K = kest(fit)$HKB)

  data$muhat0 <- predict(fit2, mutate(df, A = 0))
  data$muhat1 <- predict(fit2, mutate(df, A = 1))

  data |>
    group_by(D, group_effect, group_active) |>
    summarize(psi = mean(muhat1 - muhat0), true_psi = group_effect[1] * group_active[1]) |>
    mutate(method = "HKB") |>
    rename(group = D)
}

wrapper <- function(data, G, SL.library) {
  comparison <- hkb_ridge(data, G)

  results <- tibble(
    group = 1:G
  ) %>%
    mutate(fit = map(group, \(group) {
      group_data <- filter(data, D == group)
      N <- nrow(group_data)

      if(N > 2 && sum(group_data$A == 0) > 1 && sum(group_data$A == 1) > 1) {
        fit <- tmle(
          Y = group_data$Y, 
          A = group_data$A, 
          W = group_data[, paste0("X", 1:5)], 
          Q.SL.library = SL.library, 
          g.SL.library = SL.library,
          V.Q = 5,
          V.g = 5
        )

        pN <- N / nrow(data)
        
        se               <- sqrt(fit$estimates$ATE$var.psi)
        point_estimate   <- fit$estimates$ATE$psi
        efficiency_bound <- fit$estimates$ATE$var.psi * N / pN

        return(tibble(
          method   = c("unpenalized"),
          psi      = c(point_estimate),
          se       = se,
          lower    = c(fit$estimate$ATE$CI[1]),
          upper    = c(fit$estimate$ATE$CI[2]),
          N        = N,
          efficiency_bound = efficiency_bound,
          true_psi = group_data$group_effect[1] * group_data$group_active[1]
        ))
      } 
      else {
        return(NULL)
      }
    })) %>%
    unnest(fit)

  
  fit_l0 <- CausalShrink::causal_shrink(results$psi, results$efficiency_bound, nrow(data), method = "l0", lambda_max = 2)
  fit_l1 <- CausalShrink::causal_shrink(results$psi, results$efficiency_bound, nrow(data), method = "l1", lambda_max = 2)
  fit_l2 <- CausalShrink::causal_shrink(results$psi, results$efficiency_bound, nrow(data), method = "l2", lambda_max = 2)
  fit_l2_adaptive <- CausalShrink::causal_shrink(results$psi, results$efficiency_bound, nrow(data), method = "empirical_bayes", lambda_max = 2)

  l2_eb_se <- fit_l2$shrinkage * results$se
  adaptive_eb_se <- fit_l2_adaptive$shrinkage * results$se

  psi = c(
    fit_l0$point_estimates, 
    fit_l1$point_estimates, 
    fit_l2$point_estimates, 
    fit_l2_adaptive$point_estimates
  )
  lambda = c(
    rep(fit_l0$lambda, length(fit_l0$point_estimates)),
    rep(fit_l1$lambda, length(fit_l1$point_estimates)),
    rep(fit_l2$lambda, length(fit_l2$point_estimates)),
    rep(NA, length(fit_l2_adaptive$point_estimates))
  )
  shrinkage = c(
    fit_l0$shrinkage, 
    fit_l1$shrinkage, 
    fit_l2$shrinkage,
    fit_l2_adaptive$shrinkage
  )
  lower <- c(
    fit_l0$point_estimates + qnorm(0.025) * results$se, 
    fit_l1$point_estimates + qnorm(0.025) * results$se, 
    fit_l2$point_estimates + qnorm(0.025) * l2_eb_se,
    fit_l2_adaptive$point_estimates + qnorm(0.025) * adaptive_eb_se
  )
  upper <- c(
    fit_l0$point_estimates + qnorm(0.975) * results$se, 
    fit_l1$point_estimates + qnorm(0.975) * results$se, 
    fit_l2$point_estimates + qnorm(0.975) * l2_eb_se,
    fit_l2_adaptive$point_estimates + qnorm(0.975) * adaptive_eb_se
  )
  method <- c(
    rep("l0", length(fit_l0$point_estimates)),
    rep("l1", length(fit_l1$point_estimates)), 
    rep("l2", length(fit_l2$point_estimates)), 
    rep("empirical_bayes", length(fit_l2_adaptive$point_estimates))
  )
  
  results <- bind_rows(select(results, -efficiency_bound, -N, -se), tibble(
    group     = rep(results$group, 4),
    method    = method,
    se        = c(results$se, results$se, l2_eb_se, adaptive_eb_se),
    lower     = lower,
    upper     = upper,
    psi       = psi,
    shrinkage = shrinkage,
    lambda = lambda,
    true_psi  = rep(results$true_psi, 4)
  )) |>
    bind_rows(comparison)

  results
}
