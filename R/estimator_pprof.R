indirect_pprof <- function(data, outcome, trt, baseline, outcome_type = "continuous", model = "fe") {
  if(outcome_type == "binomial") {
    if(model == "fe") {
      fit <- pprof::logis_fe(Y = data[[outcome]], Z = data[, baseline], ProvID = data[[trt]])
    }
    else if(model == "re") {
      fit <- pprof::logis_re(Y = data[[outcome]], Z = data[, baseline], ProvID = data[[trt]])
    }
    sm <- confint(fit)$CI.indirect_ratio
  }
  else {
    if(model == "fe") {
      fit <- pprof::linear_fe(Y = data[[outcome]], Z = data[, baseline], ProvID = data[[trt]])
    }
    else if(model == "re") {
      fit <- pprof::linear_re(Y = data[[outcome]], Z = data[, baseline], ProvID = data[[trt]])
    }
    sm <- confint(fit)$CI.indirect_difference
  }

  res <- tibble(
    trt = rownames(sm),
    psi = sm[, 1],
    lower = sm[, 2],
    upper = sm[, 3],
    method = glue::glue("pprof_{model}")
  )

  res
}
