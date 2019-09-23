#
# Init ----
#

# Load packages
library(parallel)
library(splines)
library(mgcv)
library(HiddenMarkov)
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

#
# PC4 data ----
#

# Read most recent pc4 centroids from RIVM geodatabase
pc4.data <- read_pc4(table = "adm_pc4_2018_centroide")

#
# Read and clean disease data ----
#

# We create one big tibble for all diseases
case.data <- bind_rows(
	read_db(database = "OWHDM_AIZ", selection = "EPI.vw_SD_measles"),
	read_db(database = "OWHDM_AIZ", selection = "EPI.vw_SD_gas"),
	read_db(database = "OWHDM_AIZ", selection = "EPI.vw_SD_legionella"),
	read_db(database = "OWHDM_SOA", selection = "EPI.vw_SD_gonorrhoea"))

#
# Salmonella ----
#

# JK: For now, Salmonella gets a special treatment
# This code chunk will be removed in the future

# 1. Import Salmonella csv file
salmo.data <- read_salmo(file = "data/raw/Salmonella/Salmonella humane data_ALL.csv")

# 2. Modify dates in salmo.data to mimic real-time data
last.week <- (Sys.Date() - 7) %>% cut(breaks = "week") %>% as.Date
salmo.data <- salmo.data %>%
	mutate(
		WeekFS  = WeekFS  - max(WeekFS)  + last.week,
		WeekRep = WeekRep - max(WeekRep) + last.week)

# 3. Add salmo.data to case.data
case.data <- bind_rows(
	case.data,
	salmo.data)

#
# Apply week filter ----
#

# First add DiseaseGroup to case.data
# DiseaseGroup is equal to DiseaseName, except for Salmonella, which is one group
# Each DiseaseGroup gets its own reporting delay distributions
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
# and do some pre-work for the outbreak detection
outbreak.data <- bind_rows(
	outbreak.data_DiseaseName,
	outbreak.data_SubType) %>%
	# Group by DiseaseName_SubType
	group_by(DiseaseName_SubType) %>%
	# In outbreak.data, add outbreak characteristics by DiseaseName_SubType
	# - mu.baseline: number of cases/week. Initially the overall mean for each week
	# - p.outbreak:  outbreak probability. Initially 0 for each week
	# - State:       outbreak state, 1 = no outbreak, 2 = outbreak. Initially 1 for each week
	mutate(
		mu.baseline = mean(Cases),
		p.outbreak  = 0,
		State       = 1L,
		# Finally, convert WeekFS back to Date class
		WeekFS = WeekFS %>% as.Date) %>%
	# Ungroup
	ungroup()

# Clean up
rm(outbreak.data_DiseaseName, outbreak.data_SubType)

# Now everything has been set up
# The actual outbreak detection starts here
# If there are enough cases, the algorithm in the detect_outbreaks function
# replaces the initial outbreak characteristics with the estimated characteristics

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

save(case.data,     file = "data/case data.Rdata")
save(outbreak.data, file = "data/outbreak data.Rdata")