# Observation probabilties given the state dependent distributions
prob_obs <- function(x, mu.baseline, theta, beta, log = FALSE) {
	# Observation probabilties
	prob <- cbind(
		dnbinom(x = x, mu = mu.baseline,           size = theta, log = log), # Baseline state
		dnbinom(x = x, mu = mu.baseline*exp(beta), size = theta, log = log)) # Outbreak state
	# Return output
	return(prob)
}
