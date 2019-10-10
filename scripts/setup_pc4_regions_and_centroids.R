# This script imports pc4 regions and pc6 points with population from the RIVM geo-database
# It then calculates the population weighted centroids for the pc4 regions
# and returns the pc4 regions (sf object) and weighted centroids (tibble) for plotting on the map
# The output is stored in data/pc4/
#
# The script needs to run once a year, when updated pc4 and pc6 data is available
#
# Author:
# Jan van de Kassteele

#
# Init
#

# Load packages
library(tidyverse)
library(sf)
library(RPostgreSQL)

# Import credentials
credentials <- read.delim(
	file = "data/credentials.txt",
	stringsAsFactors = FALSE) %>%
	column_to_rownames(var = "host")

# Connect to RIVM geo-database
con <- dbConnect(
	drv = dbDriver("PostgreSQL"),
	dbname = "geodata",
	host   = "geodatabase.rivm.nl",
	port   = 5432,
	user     = credentials["Geodatabase", "username"],
	password = credentials["Geodatabase", "password"])

#
# Search in geo-database ----
#

# Show all tables in database
geotables <- con %>%
	dbListTables %>%
	sort

# Search for pc4 and pc6
# (useful to check if new data has become available)
geotables %>%
	str_subset(pattern = "pc4")
geotables %>%
	str_subset(pattern = "pc6")

#
# Import pc4 regions ----
#

pc4_regions.sf <- st_read(
	dsn = con,
	query = "SELECT * FROM nl.adm_pc4_2018_basis_gen") %>%
	# Rename, only keep pc4 numbers only and sort
	rename(pc4 = pc4, geometry = shape) %>%
	select(pc4) %>%
	arrange(pc4)

#
# Import pc6 points ----
# (takes a while)
#

pc6_points.sf <- st_read(
	dsn = con,
	query = "SELECT * FROM nl.adres_pc6_2018_01") %>%
	# Rename, only keep pc6 numbers, population and sort
	rename(pc6 = postcode, population = pc6pers, geometry = geometrie) %>%
	select(pc6, population) %>%
	arrange(pc6)

#
# Close connection to database ----
#

dbDisconnect(con)

#
# Calculate population weighted pc4 centroids ----
#

pc4_centroids.data <- pc6_points.sf %>%
	# Some prework
	mutate(
		# Give records with zero/missing population a very low number
		# so way we can still calculate a weighted mean
		population = population %>% replace_na(1e-8),
		# Convert point geometry to separate columns
		x = geometry %>% map_dbl(.f = 1),
		y = geometry %>% map_dbl(.f = 2),
		# Extract pc4 from pc6
		pc4 = pc6 %>% str_sub(end = 4) %>% as.integer) %>%
	# Drop geometry
	st_drop_geometry %>%
	# Group by pc4
	group_by(pc4) %>%
	# Calculate weighted coordinates
	summarize(
		x = x %>% weighted.mean(w = population),
		y = y %>% weighted.mean(w = population))

# It appears that there is a discrepancy between the pc4 numbers in both datasets
nrow(pc4_centroids.data)
nrow(pc4_regions.sf)

# Right join pc4_centroids.data and centroids of pc4_regions.sf
# (because pc4_regions.sf is leading!)
pc4_centroids.data <- right_join(
	pc4_centroids.data,
	pc4_regions.sf %>%
		st_centroid %>%
		mutate(
			x = geometry %>% map_dbl(.f = 1),
			y = geometry %>% map_dbl(.f = 2)) %>%
		st_drop_geometry,
	by = "pc4",
	suffix = c(".popw_cent", ".region_cent")) %>%
	# Find the first non-missing coordinates
	# Be sure popw_cent comes first
	mutate(
		x = coalesce(x.popw_cent, x.region_cent),
		y = coalesce(y.popw_cent, y.region_cent)) %>%
	# Select relevant variables
	select(pc4, x, y) %>%
	# Sort by pc4
	arrange(pc4)

#
# Transform coordinates to WGS84 (CRS 4326) for plotting ----
#

pc4_centroids.data <- pc4_centroids.data %>%
	# First convert to sf object
	st_as_sf(
		coords = c("x", "y"),
		crs = 28992) %>%
	# Transform
	st_transform(crs = 4326) %>%
	# Convert back to tibble
	mutate(
		x = geometry %>% map_dbl(.f = 1),
		y = geometry %>% map_dbl(.f = 2)) %>%
	# Drop geometry
	st_drop_geometry

pc4_regions.sf <- pc4_regions.sf %>%
	# Transform
	st_transform(crs = 4326)

#
# Save results ----
#

# Save results
save(pc4_regions.sf,     file = "data/pc4/pc4_regions.RData")
save(pc4_centroids.data, file = "data/pc4/pc4_centroids.RData")
