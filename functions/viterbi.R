# Viterbi algorithm for global decoding of the most likely state at a given time

viterbi <- function (x, Pi, delta, prob) {
	n <- length(x)
	m <- nrow(Pi)
	logPi   <- log(Pi)
	logprob <- log(prob)
	logksi <- matrix(NA, nrow = n, ncol = m)
	logksi[1, ] <- log(delta) + logprob[1, ]
	for (i in 2:n) {
		logksi[i, ] <- apply(X = logksi[i - 1, ] + logPi, MARGIN = 2, FUN = max) + logprob[i, ]
	}
	y <- rep(NA, n)
	y[n] <- which.max(logksi[n, ])
	for (i in seq(from = n - 1, to = 1, by = -1)) {
		y[i] <- which.max(logPi[, y[i + 1]] + logksi[i, ])
	}
	return(y)
}
