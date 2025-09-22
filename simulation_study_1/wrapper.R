optimal_glmnet <- function(x, y, ...) {
  model_glmnet <- cv.glmnet(as.matrix(x), y, ...)
  list(fit = glmnet(x, y, lambda = model_glmnet$lambda.min, ...), lambda = model_glmnet$lambda.min)
}

estimator_glmnet <- function(data, x, y, alpha = 0) {
  model <- optimal_glmnet(as.matrix(data[, x]), data[[y]], type.measure = "mse", alpha = alpha)

  ols <- estimator_ols(data, x, y)
  point_estimates <- coef(model$fit)[x, 1]
  lower <- point_estimates + qnorm(0.025) * ols$se
  upper <- point_estimates + qnorm(0.975) * ols$se
  lower_adj <- pmin(lower, ols$lower)
  upper_adj <- pmax(upper, ols$upper)

  list(
    point_estimates = point_estimates, 
    lambda = model$lambda, 
    lower = lower,
    upper = upper,
    lower_adj = lower_adj,
    upper_adj = upper_adj
  )
}

estimator_ols <- function(data, x, y) {
  formula <- as.formula(glue::glue("{y} ~ {paste0(x, collapse = '+')}"))
  model <- lm(formula, data = data)
  point_estimates <- coef(model)[x]
  if(all(x %in% rownames(summary(model)$coefficients))) {
    lower <- confint(model)[x, 1] 
    upper <- confint(model)[x, 2]
    se <- summary(model)$coefficients[x, 2]
  }
  else {
    lower <- upper <- se <- rep(NA, length(x))
  }
  list(
    point_estimates = point_estimates,
    lower = lower,
    upper = upper,
    se = se
  )
}

fit_models <- function(data, covariates, a, y, K = 5, learners = c("glm"), seed = NA) {
  N <- nrow(data)

  folds <- caret::createFolds(data[[a]], k = K)
  mu_models <- lapply(folds, \(fold) cv.glmnet(x = as.matrix(data[setdiff(1:N, fold), covariates]), y = data[[y]][setdiff(1:N, fold)]))
  pi_models <- lapply(folds, \(fold) cv.glmnet(x = as.matrix(data[setdiff(1:N, fold), covariates]), y = data[[a]][setdiff(1:N, fold)], family = "binomial"))

  mu_hat <- unlist(lapply(1:K, \(fold_index) predict(mu_models[[fold_index]], newx = as.matrix(data[folds[[fold_index]], covariates]))))[order(unlist(folds))]
  pi_hat <- unlist(lapply(1:K, \(fold_index) predict(pi_models[[fold_index]], newx = as.matrix(data[folds[[fold_index]], covariates]), type = "response")))[order(unlist(folds))]

  list(mu_hat = mu_hat, pi_hat = pi_hat)
}

estimator <- function(data, x, y, K = 5, seed = NA, lambda_max = 3, learners = c("glm")) {
  N <- nrow(data)
  point_estimates <- c()
  efficiency_bounds <- c()
  for(covar in x) {
    fit    <- fit_models(data, setdiff(x, covar), covar, y, K = K, seed = seed, learners = learners)
    mu_hat <- fit$mu_hat
    pi_hat <- fit$pi_hat
    
    numerator_plugin   <- mean((data[[y]] - mu_hat) * (data[[covar]] - pi_hat)) 
    denominator_plugin <- mean((data[[covar]] - pi_hat)^2)
    
    eif_numerator <- ((data[[y]] - mu_hat) * (data[[covar]] - pi_hat) - numerator_plugin)
    eif_denominator <- ((data[[covar]] - pi_hat)^2 - denominator_plugin)
    
    eif <- eif_numerator / denominator_plugin - 1 / denominator_plugin^2 * eif_denominator * numerator_plugin
    
    point_estimates[covar] <- numerator_plugin / denominator_plugin
    efficiency_bounds[covar] <- var(eif)
  }
  
  se <- sqrt(efficiency_bounds / N)
  lower <- point_estimates + qnorm(0.025) * se
  upper <- point_estimates + qnorm(0.975) * se
  
  results <- list()
  results$unpenalized <- list(point_estimates = point_estimates, efficiency_bounds = efficiency_bounds, se = se, lower = lower, upper = upper)
  
  for(method in c("l0", "l1", "l2")) {
    fit <- CausalShrink::causal_shrink(point_estimates, efficiency_bounds, N, method = method, lambda_max = lambda_max)
    
    if(method == "l2") {
      l2_eb_se <- se * fit$shrinkage
      results[[method]] <- fit
      results[[method]]$lower <- fit$point_estimates + qnorm(0.025) * l2_eb_se
      results[[method]]$upper <- fit$point_estimates + qnorm(0.975) * l2_eb_se
      results[[method]]$se <- se
      results[[method]]$shrinkage <- fit$shrinkage
    }
    else {
      results[[method]] <- fit
      results[[method]]$lower <- fit$point_estimates + qnorm(0.025) * se
      results[[method]]$upper <- fit$point_estimates + qnorm(0.975) * se
      results[[method]]$se <- se
    }
  }
  
  results
}

wrapper <- function(data, x, seed, N, D, sigma, dgp, theta, y, learners) {
  filename <- glue::glue("/gpfs/scratch/susmah01/PenalizedCausalInference/covariance_cache/{N}/{dgp}_{theta}_{dgp}_{sigma}_{D}_{seed}.rds")
  if(file.exists(filename)) {
    fit <- read_rds(filename)
  }
  else {
    fit <- estimator(data$data, x = x, y = y, learners = learners)
    write_rds(fit, filename)
  }
  fit
}

