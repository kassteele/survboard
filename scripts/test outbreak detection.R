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

# Set palette for outbreak probabilities
# See https://www.rijkshuisstijl.nl/basiselementen/basiselementen-online/online-kleuren
# donkergroen > groen > geel > rood > violet
GrYlRd <- c("#275937", "#39870c", "#f9e11e", "#d52b1e", "#a90061") %>%
	colorRampPalette

#
# Load data ----
#

# Load case.data and outbreak.data
list.files(path = "data", full.names = TRUE, recursive = TRUE) %>%
	str_subset(pattern = ".Rdata") %>%
	walk(load, envir = .GlobalEnv)

# Replace estimated outbreak characteristics by the defaults
# as in the prepare disease data scripts
outbreak.data <- outbreak.data %>%
	# Group by DiseaseName_SubType
	group_by(DiseaseName_SubType) %>%
	# In outbreak.data, add outbreak characteristics by DiseaseName_SubType
	# - mu.baseline: number of cases/week. Initially the overall mean for each week
	# - p.outbreak:  outbreak probability. Initially 0 for each week
	# - State:       outbreak state, 1 = no outbreak, 2 = outbreak. Initially 1 for each week
	mutate(
		mu.baseline = mean(Cases),
		p.outbreak  = 0,
		State       = 1L)
	# Ungroup
	ungroup()

# Create delaydist.list
delaydist.list <- case.data %>%
	split(f = .$DiseaseGroup) %>%
	map(.f = function(data) {
		data %>% {pmax(0, .$WeekRep - .$WeekFS)} %>% "/"(7) %>% ecdf
	})

#
# Outbreak detection ----
#

# Filter outbreak.data
outbreak.data.sub <- outbreak.data %>% filter(DiseaseName == "Salmonella Bovismorbificans")
outbreak.data.sub <- outbreak.data %>% filter(DiseaseName == "Salmonella Unknown")
outbreak.data.sub <- outbreak.data %>% filter(DiseaseName == "Salmonella Napoli")
outbreak.data.sub <- outbreak.data %>% filter(DiseaseName == "Salmonella Ohio")
outbreak.data.sub <- outbreak.data %>% filter(DiseaseName == "Measles")
outbreak.data.sub <- outbreak.data %>% filter(DiseaseName == "Legionella")
outbreak.data.sub <- outbreak.data %>% filter(DiseaseName == "Salmonella Enteritidis" & is.na(SubType))
outbreak.data.sub <- outbreak.data %>% filter(DiseaseName == "Salmonella Typhimurium" & is.na(SubType))
outbreak.data.sub <- outbreak.data %>% filter(DiseaseName == "Pertussis")

# Do outbreak detection
outbreak.data.sub <- detect_outbreaks(
	outbreak.data = outbreak.data.sub,
	delaydist.list = delaydist.list) %>%
	# Add fifty shades of colour coding
	mutate(
		Color = GrYlRd(n = 50)[p.outbreak %>% cut(
			breaks = seq(from = 0, to = 1, length = 51),
			right = FALSE, include.lowest = TRUE, labels = FALSE)])

# Plot
with(outbreak.data.sub, {
	plot(WeekFS, Cases, col = Color, lwd = 3, type = "h")
	lines(WeekFS, mu.baseline)
	rug(WeekFS[State == 2])
})

