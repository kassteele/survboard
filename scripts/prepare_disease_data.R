#
# Init ----
#

# Load packages
library(parallel)
library(mgcv)
library(tidyverse)

# Source functions
list.files(path = "functions", full.names = TRUE) %>%
	walk(.f = source)

# Import credentials
credentials <- read.delim(
	file = "data/credentials.txt",
	stringsAsFactors = FALSE) %>%
	column_to_rownames(var = "host")

# Set palette for outbreak probabilities
# See https://www.rijkshuisstijl.nl/basiselementen/basiselementen-online/online-kleuren
# donkergroen > groen > geel > rood > violet
GrYlRd <- c("#275937", "#39870c", "#f9e11e", "#d52b1e", "#a90061") %>%
	colorRampPalette

# Import pc4 population weighted centroids (pc4_centroids.data)
load(file = "data/pc4/pc4_centroids.RData")

#
# Read and clean disease data ----
#

# We create one big tibble for all diseases
# This takes a while...
case.data <- bind_rows(
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_measles"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_gas"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_legionella"),
	read_db_osiris(database = "OWHDM_SOA", selection = "EPI.vw_SD_gonorrhoea"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_bof"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_listeria"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_pertussis"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_Hib_disease"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_Tuberculosis"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_Psittacosis"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_buiktyfus"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_hantavirus"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_HepatitisA"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_HepatitisBacuut"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_HepatitisCacuut"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_leptospirosis"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_ParatyfusA"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_ParatyfusB"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_ParatyfusC"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_Qfever"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_Rubella"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_Shigellosis"),
	read_db_osiris(database = "OWHDM_AIZ", selection = "EPI.vw_SD_meningokokken"),
	read_db_unilab())

#
# Apply week filter ----
#

# First add DiseaseGroup to case.data
# DiseaseGroup is equal to DiseaseName, except for Salmonella, which is one group
# Each DiseaseGroup gets its own reporting delay distribution
case.data <- case.data %>%
	mutate(
		DiseaseGroup = case_when(
			DiseaseName %>% str_detect(pattern = "Salmonella") %>% `!` ~ DiseaseName,
			DiseaseName %>% str_detect(pattern = "Salmonella")         ~ "Salmonella"))

# The command below gives an overview of starting and ending weeks per DiseaseGroup
# Not used in the dashboard, can be commented out
case.data %>%
	group_by(DiseaseGroup) %>%
	summarize(
		WeekFS.from = WeekFS %>% min,
		WeekFS.to   = WeekFS %>% max)

# Set week sequence
# This is a vector of weeks, starting on mondays
# Only cases within this sequence are considered in the dashboard
#
# Start: The week 8 years back from last week
# End:   The week BEFORE the current week, i.e. last week, because the current week is not complete yet
#        This incompleteness would lead to an undisered day-dependend reporting delay distribution
last.week <- (Sys.Date() - 7) %>% cut(breaks = "week") %>% as.Date
week.seq <- seq(
	from = (last.week - 365.25*8) %>% cut(breaks = "week") %>% as.Date,
	to   =  last.week,
	by   = "week")

# Apply week filter to case.data
case.data <- case.data %>%
	filter(WeekFS %in% week.seq)

#
# Reporting delay distribution ----
#

# Reporing delay distributions per DiseaseGroup
# This is the cumulative fraction of reported cases for delay 0, 1, ... weeks
# JK: still needs correction for right trunction of long delays near the present,
#     becuase long delays cannot be obeserved near the present
delaydist.list <- case.data %>%
	split(f = .$DiseaseGroup) %>%
	map(.f = function(data) {
		data %>% {pmax(0, .$WeekRep - .$WeekFS)} %>% "/"(7) %>% ecdf
	})

#
# Detect outbreaks ----
#

# First create outbreak.data
# This is one big tibble with the number of cases per week
# for each DiseaseName and, if any, SubType

# 1. Create outbreak.data for DiseaseName
outbreak.data_DiseaseName <- case.data %>%
	# Make weekFS a factor with levels identical to week.seq
	mutate(WeekFS = WeekFS %>% factor(levels = week.seq %>% as.character)) %>%
	# Group by DiseaseName and weekFS
	# Use .drop = FALSE to keep weeks with 0 cases
	group_by(DiseaseName, WeekFS, .drop = FALSE) %>%
	# Count the number of cases per DiseaseName and WeekFS
	count(name = "Cases") %>%
	# Add DiseaseGroup based on DiseaseName
	left_join(case.data %>% distinct(DiseaseName, DiseaseGroup)) %>%
	# Add SubType (= NA, this tibble only applies to DiseaseGroup, not SubType)
	# Add DiseaseName_SubType (to be used in app). Here it is only DiseaseName
	mutate(
		SubType = NA_character_,
		DiseaseName_SubType = DiseaseName)

# 2. Create outbreak.data for SubType
outbreak.data_SubType <- case.data %>%
	# Remove records without SubType
	filter(!is.na(SubType)) %>%
	# Make weekFS a factor with levels identical to week.seq
	mutate(WeekFS = WeekFS %>% factor(levels = week.seq %>% as.character)) %>%
	# Group by SubType and weekFS
	# Use .drop = FALSE to keep weeks with 0 cases
	group_by(SubType, WeekFS, .drop = FALSE) %>%
	# Count the number of cases per SubType and WeekFS
	count(name = "Cases") %>%
	# Add DiseaseName and DiseaseGroup based on SubType
	left_join(case.data %>% distinct(SubType, DiseaseName, DiseaseGroup) %>% na.omit) %>%
	# Add DiseaseName_SubType (to be used in app). Here it is DiseaseName PLUS SubType
	mutate(
		DiseaseName_SubType = str_c(DiseaseName, SubType, sep = " "))

# Bind them together in outbreak.data
outbreak.data <- bind_rows(
	outbreak.data_DiseaseName,
	outbreak.data_SubType)

# Clean up
rm(outbreak.data_DiseaseName, outbreak.data_SubType)

# Do pre-work for outbreak detection
#
# In outbreak.data, add outbreak characteristics by DiseaseName_SubType
# - mu.baseline: expected number of cases/week in baseline state. Initially the mean over the entire period
# - p.outbreak:  outbreak probability. Initially 0 for each week
# - State:       outbreak state. 1 = baseline, 2+ = outbreak. Initially 1 for each week
#
# Only perform outbreak detection on DiseaseName_SubType's with, on average, >= 5 cases/year = 5/52.17857 cases/week
# However, DiseaseName_SubType with 0 cases in the past will only be detected if
# they have >= 5 x 8 (8 years, see above) cases during a current outbreak.
# Therefore, also perform outbreak detection on DiseaseName_SubType's with, on average, >= 5 cases/last year = 5/52 cases/week

outbreak.data <- outbreak.data %>%
	# Add past last.year indicator: TRUE = last year (52 weeks), FALSE = before that
	mutate(last.year = (last.week - WeekFS) %>% as.numeric %>% "/"(7) < 52) %>%
	# Group by DiseaseName_SubType
	group_by(DiseaseName_SubType) %>%
	# Initial baseline (cases/week) per DiseaseName_SubType
	mutate(mu.baseline = mean(Cases)) %>%
	# Add group by last.year
	group_by(last.year, add = TRUE) %>%
	# Calculate initial baseline (cases/week) per DiseaseName_SubType and last.year
	mutate(mu.baseline_last.year = mean(Cases)) %>%
	# Apply filters. Keep DiseaseName_SubType's with:
	# - mu.baseline           >= 5/52.17857 cases/week on average over the entire period, or
	# - mu.baseline_last.year >= 5/52       cases/week on average in the last year (52 weeks)
	filter(mu.baseline >= 5/52.17857 | (last.year & mu.baseline_last.year >= 5/52)) %>%
	# Ungroup DiseaseName_SubType
	ungroup() %>%
	# Some other mutations to add
	mutate(
		# Convert WeekFS back to Date class
		WeekFS = WeekFS %>% as.Date,
		# Set inital outbreak probability and state
		p.outbreak  = 0,
		State       = 1L) %>%
	# Clean up
	select(-contains("last"))

# Now everything has been set up
# The actual outbreak detection starts here
# The algorithm in the detect_outbreaks function replaces the initial
# outbreak characteristics with the estimated characteristics

# Do outbreak detection
outbreak.data <- mclapply(
	mc.cores = detectCores(),
	X = outbreak.data %>% split(f = .$DiseaseName_SubType),
	FUN = detect_outbreaks,
	delaydist.list = delaydist.list) %>%
	bind_rows %>%
	# Add fifty shades of colour coding
	mutate(
		Color = GrYlRd(n = 50)[p.outbreak %>% cut(
			breaks = seq(from = 0, to = 1, length = 51),
			right = FALSE, include.lowest = TRUE, labels = FALSE)])

#
# Save results ----
#

saveRDS(case.data,     file = "data/case_data.rds")
saveRDS(outbreak.data, file = "data/outbreak_data.rds")

# Update time stamp of app.R to resolve data cache issue on Shiny Server
# https://community.rstudio.com/t/how-update-data-cache-for-shiny-server/27535
Sys.setFileTime(
	path = "app/app.R",
	time = Sys.time())
