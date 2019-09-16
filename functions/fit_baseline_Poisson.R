# Function to fit Poisson GLM to baseline counts

# Details:
# Fit Poisson GLM with constant time trend
# Outbreaks states are given less weight (is HMM feature, see book, this is how HMMs work)
# Include log(fraction reported cases) as offset
# Note that the predicted eta DOES include the offset!
# In other words, the decline near the present is caused by the offset,
#   and will NOT affect the time trend + seasonal effect fit
# Prediction with r = 1 everywhere will cause an upward correction (= nowcasting),
#   but this is not of particular interest here

fit_baseline_Poisson <- function(outbreak.data) {

	# Fit Poisson GLM
	fit <- glm(
		formula = Cases ~ ns(t, df = 2),
		offset = log(r),
		weights = u %>% sapply(FUN = `[`, 1), # u[, 1]
		family = poisson,
		data = outbreak.data)

	# Return output
	return(list(
		mu.baseline = predict(fit, type = "response"),
		theta = Inf))
}
