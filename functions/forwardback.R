# Forward and backward probabilities of HMM
# This function is based on HiddenMarkov::forwardback
forwardback <- function (Pi, delta, prob) {
	n <- nrow(prob)
	m <- nrow(Pi)

	# Log-forward probabilities
	logalpha <- matrix(rep(0, n*m), nrow = n, ncol = m)
	phi      <- delta
	lscale   <- 0
	for (i in 1:n) {
		if (i > 1) phi <- phi %*% Pi
		phi           <- phi*prob[i, ]
		sumphi        <- sum(phi)
		phi           <- phi/sumphi
		lscale        <- lscale + log(sumphi)
		logalpha[i, ] <- log(phi) + lscale
	}
	# Log-likelihood
	LL <- lscale

	# Log-backward probabilities
	logbeta <- matrix(rep(0, n*m), nrow = n, ncol = m)
	phi     <- rep(1/m, m)
	lscale  <- log(m)
	for (i in seq(from = n - 1, to = 1, by = -1)) {
		phi          <- Pi %*% (prob[i + 1, ]*phi)
		logbeta[i, ] <- log(phi) + lscale
		sumphi       <- sum(phi)
		phi          <- phi/sumphi
		lscale       <- lscale + log(sumphi)
	}

	# Return output
	return(list(
		logalpha = logalpha,
		logbeta  = logbeta,
		LL       = LL))
}
