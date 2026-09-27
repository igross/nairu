# Refuse to export unstable posterior estimates. Constant quantities (for
# example an inactive dummy) have no mixing diagnostic and are excluded.
nairu_fit_diagnostics <- function(fit, max_treedepth = 12L) {
  draws <- as.array(fit)
  varying <- apply(draws, 3, function(x) diff(range(x)) > 0)
  d <- draws[, , varying, drop = FALSE]
  params <- data.frame(
    parameter = dimnames(d)[[3]],
    rhat = apply(d, 3, rstan::Rhat),
    ess_bulk = apply(d, 3, rstan::ess_bulk),
    ess_tail = apply(d, 3, rstan::ess_tail), row.names = NULL
  )
  sp <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
  chains <- do.call(rbind, lapply(seq_along(sp), function(i) {
    x <- sp[[i]]
    data.frame(chain = i, divergences = sum(x[, 'divergent__']),
      treedepth_hits = sum(x[, 'treedepth__'] >= max_treedepth),
      ebfmi = mean(diff(x[, 'energy__'])^2) / var(x[, 'energy__']))
  }))
  passed <- all(is.finite(as.matrix(params[, -1]))) &&
    all(params$rhat < 1.01) && all(params$ess_bulk >= 400) &&
    all(params$ess_tail >= 400) && all(chains$divergences == 0) &&
    all(chains$treedepth_hits == 0) && all(chains$ebfmi >= 0.3)
  list(passed = passed, parameters = params, chains = chains)
}

sample_nairu <- function(model, data) {
  fit <- rstan::sampling(model, data = data, chains = 4, cores = 4,
    iter = 4000, warmup = 2000, seed = 27092026,
    control = list(adapt_delta = 0.95, max_treedepth = 12))
  diagnostics <- nairu_fit_diagnostics(fit)
  message(sprintf("NAIRU diagnostics: max R-hat %.4f; min bulk ESS %.0f; min tail ESS %.0f",
    max(diagnostics$parameters$rhat), min(diagnostics$parameters$ess_bulk),
    min(diagnostics$parameters$ess_tail)))
  if (!diagnostics$passed) {
    print(diagnostics$chains)
    print(head(diagnostics$parameters[order(diagnostics$parameters$rhat, decreasing = TRUE), ], 10))
    stop('NAIRU fit failed convergence checks; no estimates from this fit exported. ',
         'Inspect R-hat, effective sample sizes and sampler diagnostics before retrying.')
  }
  fit
}
