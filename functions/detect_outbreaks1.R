# Function to detect outbreaks using HMM
detect_outbreaks1 <- function(outbreak.data, delaydist.list) {

	# Arguments
	# outbreak.data  = dataframe with weekly number of cases
	# delaydist.list = list with ecdf per DiseaseGroup

	# JK: to be done, correction for reporting delay:
	# Most likely corrected number (nowcast)
	# plot(10 + 0:70, dnbinom(x = 0:70, size = 10 + 1, prob = 0.55), type = "h")
	# 10 + floor((1 - 0.55)*(10 + 1 - 1)/0.55)

	#
	# Set-up ----
	#

	# Settings
	n <- nrow(outbreak.data) # Length of the time series
	# m <- 2                   # Number of states: baseline and outbreak state
	m <- 3                   # Number of states: baseline and outbreak state

	# Initial distribution delta of Markov Chain at t = 0 and transition probability matrix Pi
	# delta <- c(1, 0)
	# Pi <- matrix(c(0.99, 0.01, 0.20, 0.80), ncol = m, byrow = TRUE)
	delta <- c(1, 0, 0)
	Pi <- matrix(c(
		0.99, 0.01, 0.00,
		0.10, 0.80, 0.10,
		0.00, 0.20, 0.80),
		nrow = 3, byrow = TRUE)

	# Initial log(RR) of outbreaks state vs. baseline state
	# beta <- log(2)
	beta <- log(c(2, 4))

	# Add further things to outbreak.data
	outbreak.data <- outbreak.data %>%
		mutate(
			# Time: t      = time sequence in weeks, starting at t = 0
			#       t.seas = seasonal time in weeks with period 52.17857 weeks
			t      = seq_len(n) - 1,
			t.seas = t %% 52.17857,
			# Fraction of reported cases by week
			# This fraction is used to disentangle the downward trend near the present
			# from the fitted baseline trend
			r = delaydist.list[[DiseaseGroup[1]]](max(t) - t),
			# Set initial state probabilities to c(1, 0), for each week
			# Hence, u is a list column with dbl [2]
			# u = list(c(1, 0)))
			u = list(c(1, 0, 0)))

	# Fit log-baseline eta
	fit.baseline <- fit_baseline_NegBin(outbreak.data)

	# Add baseline fit to outbreak.data
	outbreak.data <- outbreak.data %>%
		mutate(mu.baseline = fit.baseline$mu.baseline)

	# Both fit_baseline functions return an overdispersion parameter theta
	# The Poisson fit returns theta = Inf (= no overdispersion)
	theta <- fit.baseline$theta

	#
	# Run HMM Bauw-Welch algorithm ----
	#

	oldLL <- -Inf
	for (iter in 1:50) {

		#
		# E-step
		#

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

		#
		# M-step
		#

		# Update initial distribution of Markov Chain delta
		# Update transition probability matrix Pi
		delta <- outbreak.data$u[[1]]
		Pi <- apply(v, MARGIN = 2:3, FUN = sum)/apply(v, MARGIN = 2, FUN = sum)

		# Update parameters of the state-dependent distributions
		# Again, first the baseline fit with the updated u's etc. from the E-step
		fit.baseline <- fit_baseline_NegBin(outbreak.data)
		# Then update baseline and theta
		outbreak.data <- outbreak.data %>%
			mutate(mu.baseline = fit.baseline$mu.baseline)
		theta <- fit.baseline$theta

		# With this new baseline, optimize beta,
		# beta = log(RR), the deviation from the linear predictor log(mu.baseline)
		beta <- optim(
			# Here we minimize the negative log-likelihood
			fn = function(u, x, mu.baseline, theta, par) {
				-sum(u*prob_obs(x = x, mu.baseline = mu.baseline, theta = theta, beta = par, log = TRUE))
			},
			# Parameter to optimize
			par = beta,
			# Data that is fixed during the optimization
			u           = outbreak.data$u %>% sapply(FUN = `[`, 1:m) %>% t,
			x           = outbreak.data$Cases,
			mu.baseline = outbreak.data$mu.baseline,
			theta       = theta,
			# Optim method: BFGS
			method = "BFGS")$par

		# End Bauw-Welch iteration loop
	}

	#
	# Post processing ----
	#

	# After convergence:
	outbreak.data <- outbreak.data %>%
		mutate(
			# Do global decoding of underlying hidden Markov state at each time point (Viterbi algorithm)
			State = viterbi(x = Cases, Pi = Pi, delta = delta, prob = prob.obs),
			# Local outbreak probability is u[, 2:m]
			p.outbreak = u %>% sapply(FUN = `[`, 2:m) %>% colSums %>%
				# Be sure the probabilities are between 0 and 1
				# This is a machine precision issue...
				# We need this avoid NA's in the colouring afterwards
				pmax(0) %>% pmin(1))

	# Return selection of variables
	# We do not the rest anymore
	return(
		outbreak.data %>%
			select(
				DiseaseGroup, DiseaseName, SubType, DiseaseName_SubType,
				WeekFS, Cases, mu.baseline, p.outbreak, State))

}
