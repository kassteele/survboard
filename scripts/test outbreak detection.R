# Create delaydist.list
delaydist.list <- case.data %>%
	split(f = .$DiseaseGroup) %>%
	map(.f = function(data) {
		data %>% {pmax(0, .$WeekRep - .$WeekFS)} %>% "/"(7) %>% ecdf
	})

# Filter outbreak.data
outbreak.data.sub <- outbreak.data %>% filter(DiseaseName == "Salmonella Bovismorbificans")
outbreak.data.sub <- outbreak.data %>% filter(DiseaseName == "Salmonella Unknown")
outbreak.data.sub <- outbreak.data %>% filter(DiseaseName == "Salmonella Napoli")
outbreak.data.sub <- outbreak.data %>% filter(DiseaseName == "Salmonella Ohio")
outbreak.data.sub <- outbreak.data %>% filter(DiseaseName == "Measles")
outbreak.data.sub <- outbreak.data %>% filter(DiseaseName == "Legionella")
outbreak.data.sub <- outbreak.data %>% filter(DiseaseName == "Salmonella Enteritidis" & is.na(SubType))
outbreak.data.sub <- outbreak.data %>% filter(DiseaseName == "Salmonella Typhimurium" & is.na(SubType))

# Do outbreak detection
x <- detect_outbreaks(
	outbreak.data = outbreak.data.sub,
	delaydist.list = delaydist.list) %>%
	# Add fifty shades of colour coding
	mutate(
		Color = GrYlRd(n = 50)[p.outbreak %>% cut(
			breaks = seq(from = 0, to = 1, length = 51),
			right = FALSE, include.lowest = TRUE, labels = FALSE)])

# Plot
with(x, {
	plot(WeekFS, Cases, col = Color, lwd = 3, type = "h")
	lines(WeekFS, mu.baseline)
	rug(WeekFS[State == 2])
})
