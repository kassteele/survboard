# Observation probabilties given the state dependent distributions
prob_obs <- function(x, mu.baseline, theta, beta, log = FALSE) {

	# Baseline state
	prob <- dnbinom(x = x, mu = mu.baseline, size = theta, log = log)
	# Add outbreak state(s)
	for (k in seq_along(beta)) {
		prob <- cbind(
			prob,
			dnbinom(x = x, mu = mu.baseline*exp(beta[k]), size = theta, log = log))
	}

	# Return output
	return(prob)
}
