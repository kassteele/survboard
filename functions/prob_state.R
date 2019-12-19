# State probabilities given the observations
# This function is based on HiddenMarkov::Estep
prob_state <- function(Pi, prob, fw) {
	n <- nrow(prob)
	m <- nrow(Pi)
	logalpha <- fw$logalpha
	logbeta  <- fw$logbeta
	LL       <- fw$LL
	u <- exp(logalpha + logbeta - LL)
	v <- array(NA, dim = c(n - 1, m, m))
	for (k in 1:m) {
		logprob  <- log(prob[-1, k])
		logPi    <- matrix(log(Pi[, k]),             byrow = TRUE,  nrow = n - 1, ncol = m)
		logPbeta <- matrix(logprob + logbeta[-1, k], byrow = FALSE, nrow = n - 1, ncol = m)
		v[, , k] <- logPi + logalpha[-n, ] + logPbeta - LL
	}
	v <- exp(v)
	# Return output
	return(list(u = u, v = v))
}
