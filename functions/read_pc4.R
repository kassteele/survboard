# Description:
# This function reads pc4 centroids
#
# Arguments:
# table = name of the table in RIVM geodatabase
#
# Value:
# Dataframe
#
# Author:
# Jan van de Kassteele

read_pc4 <- function(table) {

	# Load packages
	library(sf)
	library(RPostgreSQL)

	# Connect to geo-database
	con <- dbConnect(
		drv = dbDriver("PostgreSQL"),
		dbname = "geodata",
		host   = "geodatabase.rivm.nl",
		port   = 5432,
		user     = credentials["Geodatabase", "username"],
		password = credentials["Geodatabase", "password"])

	# Import pc4 data
	pc4.data <- st_read(dsn = con, query = str_c("SELECT * FROM nl.", table)) %>%
		# Keep pc4 numbers only and sort
		select(pc4) %>%
		arrange(pc4) %>%
		# Project to WGS84 (CRS 4326) for plotting
		st_transform(crs = 4326) %>%
		# Get coordinates of centroids
		mutate(
			x = st_coordinates(.)[, 1],
			y = st_coordinates(.)[, 2]) %>%
		# Drop geometry
		st_drop_geometry %>%
		# Return tibble
		as_tibble

	# Close connection
	dbDisconnect(con)

	# Return output
	return(pc4.data)
}
