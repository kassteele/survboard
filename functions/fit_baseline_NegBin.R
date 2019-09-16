# Function to fit Negative Binomial GAM to baseline counts

# Details:
# Fit Negative Binomial GAM with smooth time trend and seasonal effect to weekly counts
# Outbreaks states are given less weight by using weight = u[, 1] (Yes, this works, is HMM feature, see book)
#
# Include log(fraction reported cases) as offset
# Note that the predicted eta DOES include the offset
# In other words, the decline near the present is caused by the offset,
# and will NOT affect the time trend + seasonal effect fit
# Prediction with r = 1 everywhere will cause an upward correction (= nowcasting),
# but this is not of particular interest here

fit_baseline_NegBin <- function(outbreak.data) {

	# Number of basis functions
	kt      <- nrow(outbreak.data) %>% "/"(52.17857) %>% max(., 3) %>% round
	kt.seas <- 6

	# Fit GAM
	fit <- gam(
		formula = Cases ~ s(t, k = 8, bs = "cs") + s(t.seas, bs = "cc", k = kt.seas),
		knots = list(t.seas = c(0, 52.17857)),
		offset = log(r),
		weights = u %>% sapply(FUN = `[`, 1), # u[, 1]
		family = "nb",
		data = outbreak.data)

	# Return output
	return(list(
		mu.baseline = predict(fit, type = "response"),
		theta = fit$family$getTheta(TRUE)))
}
