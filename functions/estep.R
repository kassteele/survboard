# E-Step of EM Algorithm for HMM
estep <- function(x, Pi, delta) {

	# Calculate the observation probabilities given the state-dependent distributions
	# This is an n x m matrix
	prob.obs <- prob_obs(
		x           = outbreak.data$Cases,
		mu.baseline = outbreak.data$mu.baseline,
		theta = theta, beta = beta, log = FALSE)

	# Calculate the forward- and backward probabilities
	# Use forwardback.dthmm function from HMM package
	# This returns n x m matrices log(alpha) and log(beta) and log-likelihood LL
	prob.fw <- forwardback(Pi = Pi, delta = delta, prob = prob.obs)

	# Convergence reached?
	# If so, break the iteration loop
	LL <- prob.fw$LL
	cat("Iteration", iter, "| Log-Likelihood:", LL, "\n")
	#if (abs((oldLL - LL)/LL) < 1e-5 | LL < oldLL) break
	if (abs((oldLL - LL)/LL) < 1e-5) break
	oldLL <- LL

	# If not, calculate the state probabilities given the observations
	# This returns n x m matrix u and n x m x m array v
	prob.state <- prob_state(Pi = Pi, prob = prob.obs, fw = prob.fw)

	# Update u in outbreak.data
	outbreak.data <- outbreak.data %>%
		mutate(u = prob.state$u %>% split(f = t))
	# We do not need v in outbreak.data
	# It is only needed to update the transition probability matrix Pi in the M-step
	v <- prob.state$v
}
