# Function to add age categories and pc4 population weighted centroids
add_agecat_pc4 <- function(data) {
	data %>%
		# Create five age categories
		mutate(
			Agecat = Age %>% cut(
				breaks = c(0, 5, 15, 25, 65, Inf),
				labels = c("0 - 4", "5 - 14", "15 - 25", "25 - 64", "65+"),
				right = FALSE, include.lowest = TRUE)) %>%
		# Left join with pc4_centroids.data
		left_join(
			x = .,
			y = pc4_centroids.data,
			by = c("PC4_numbers" = "pc4"))
}
