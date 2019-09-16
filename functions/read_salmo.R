# Read and clean Salmonella data

read_salmo <- function(file) {

	#
	# Load data from file ----
	#

	data <- read_csv(file = file)

	#
	# Clean data ----
	#

	data <- data %>%
		# Mutations
		mutate(
			DiseaseName =
				case_when(
					# Strip everything from Enteritidis and Typhimurium
					# These are just Typhimurium and Enteritidis
					str_detect(`serotype omschrijving`, pattern = "Typhimurium") ~ "Typhimurium",
					str_detect(`serotype omschrijving`, pattern = "Enteritidis") ~ "Enteritidis",
					# Unknown salmonella
					str_detect(`serotype omschrijving`, pattern = "n.v.t /not applicable") | is.na(`serotype omschrijving`) ~ "Unknown",
					# The rest is as is
					TRUE ~ `serotype omschrijving`) %>%
				# Strip special characters
				str_remove_all(pattern = "\\[") %>%
				str_remove_all(pattern = "\\]") %>%
				str_remove_all(pattern = "S\\. ") %>%
				# Put Salmonella in front of it
				str_c("Salmonella ", .),
			SubType =
				case_when(
					# SubType is MLVA when DiseaseName is Enteritidis or Typhimurium
					# Otherwise, there is no SubType
					DiseaseName %in% c("Salmonella Typhimurium", "Salmonella Enteritidis") ~ MLVA,
					TRUE ~ NA_character_),
			PatientID = NA_character_,
			CaseID = Monsternr %>% as.character,
			# Status is a factor
			Status = "Confirmed" %>% factor(levels = c("Suspected", "Confirmed", "Unknown")),
			# Be sure Date for stats and and reporting date are dates
			DateFS        = `Creatie dat.` %>% as.Date(format = "%d/%m/%Y"),
			ReportingDate = `Rap. dat.`    %>% as.Date(format = "%d/%m/%Y"),
			# If reporting date is missing, assume there is no reporting delay
			ReportingDate = ReportingDate %>% if_else(is.na(.), true = DateFS, false = .),
			# Date of birth
			DateOfBirth   = Geboortedat. %>% as.Date(format = "%d/%m/%Y"),
			# Postcode
			PC4_numbers = `Postcode nr.` %>% as.integer,
			# Recode sex labels
			Sex = S %>%
				factor(levels = c("M", "V", "T", "G", "?")) %>%
				fct_recode(Male = "M", Female = "V", Transgender = "T", `Gender neutral` = "G", Unknown = "?") %>%
				fct_explicit_na(na_level = "Unknown"),
			# Extra categories, description as is
			cat1desc = "Material",
			cat2desc = NA_character_,
			cat3desc = NA_character_,
			# Contents should be a factor with NA replaced by Unknown
			cat1 = case_when(
				materiaal %in% c("niet ingevuld", "onbekend materiaal") ~ "Unknown",
				TRUE ~ materiaal) %>%
				replace_na("Unknown"),
			cat2 = "Unknown",
			cat3 = "Unknown",
			# Derived quantities
			Age = (DateFS - DateOfBirth) %>% as.numeric %>% "/"(365.25) %>% floor) %>%
			# Filter out incorrect values
		filter(
			!is.na(DateFS) & DateFS <= Sys.Date() & Age %in% 0:120 & PC4_numbers %in% 1011:9999) %>%
		# Add Week (ISO 8601, starts on Monday)
		mutate(
			WeekFS  = DateFS        %>% cut(breaks = "week") %>% as.Date,
			WeekRep = ReportingDate %>% cut(breaks = "week") %>% as.Date) %>%
		# Add age categories and PC4 coordinates
		add_agecat_pc4

	#
	# Output ----
	#

	# Return final selection
	data <- data %>%
		select(
			DiseaseName, SubType,
			PatientID, CaseID, Status,
			WeekFS, WeekRep,
			Sex, Agecat,
			PC4_numbers, x, y,
			cat1desc, cat1,
			cat2desc, cat2,
			cat3desc, cat3)

}
