# Description:
# This function reads data from the Unilab database,
# does some mutations and returns a tibble
#
# Arguments:
# none, currently only one database and selection
#
# Value:
# Tibble
#
# Author:
# Jan van de Kassteele

read_db_unilab <- function() {

	#
	# Init ----
	#

	# Load packages
	library(ROracle)

	#
	# Import data from database ----
	#

	# Connect to database
	con <- dbConnect(
		drv = dbDriver("Oracle"),
		dbname = "
			(DESCRIPTION =
   			(ADDRESS =
					(PROTOCOL = TCP)
					(HOST = rivm-ulabdb-l02p.rivm.ssc-campus.nl)
					(PORT = 1521))
				(CONNECT_DATA =
					(SERVER = DEDICATED)
					(SERVICE_NAME = unilbp01)))",
		username = credentials["Unilab", "username"],
		password = credentials["Unilab", "password"])

	# Get query
	data <- dbGetQuery(
		conn = con,
		statement = "select * from unilab.AVEPIEXTRACTSALMO")

	# Close connection
	dbDisconnect(conn = con)

	#
	# Clean data ----
	#

	data <- data %>%
		# Mutations
		mutate(
			# Set data types, just to be sure
			DiseaseName   = DISEASENAME   %>% as.character,
			SubType       = SUBTYPE       %>% as.character,
			PatientID     = PATIENTID     %>% as.character,
			CaseID        = CASEID        %>% as.character,
			Status        = STATUS        %>% as.character,
			Sex           = SEX           %>% as.character,
			PC2_letters   = PC2_LETTERS   %>% as.character,
			cat1desc      = CAT1DESC      %>% as.character,
			cat2desc      = CAT2DESC      %>% as.character,
			cat3desc      = CAT3DESC      %>% as.character,
			cat1          = CAT1          %>% as.character,
			cat2          = CAT2          %>% as.character,
			cat3          = CAT3          %>% as.character,
			DayOfBirth    = DAYOFBIRTH    %>% as.integer,
			MonthOfBirth  = MONTHOFBIRTH  %>% as.integer,
			YearOfBirth   = YEAROFBIRTH   %>% as.integer,
			PC4_numbers   = PC4_NUMBERS   %>% as.integer,
			DateFS        = DATEFS        %>% as.Date,
			ReportingDate = REPORTINGDATE %>% as.Date,
			# Other mutations:
			DiseaseName =
				case_when(
					# Strip everything from Enteritidis and Typhimurium
					# These are just Typhimurium and Enteritidis
					str_detect(DiseaseName, pattern = "Typhimurium") ~ "Typhimurium",
					str_detect(DiseaseName, pattern = "Enteritidis") ~ "Enteritidis",
					# Unknown salmonella
					str_detect(DiseaseName,
						pattern = str_c(c(
							"Cultuur verontreinigd",
							"Geen Salmonella",
							"Mogelijk nieuw serotype",
							"n.v.t /not applicable",
							"Na herhaalde pogingen geen groei"),
							collapse = "|")) ~ "Unknown",
					# The rest is as is
					TRUE ~ DiseaseName) %>%
				# Strip special characters
				str_remove_all(pattern = "\\[|\\]|S\\. ") %>%
				# Put Salmonella in front of it
				str_c("Salmonella ", .),
			SubType =
				case_when(
					# SubType is MLVA when DiseaseName is Enteritidis or Typhimurium
					DiseaseName %in% c("Salmonella Typhimurium", "Salmonella Enteritidis") ~ SubType,
					# Otherwise, there is no SubType
					TRUE ~ NA_character_),
			# Status is a factor
			Status = STATUS %>% factor(levels = c("Suspected", "Confirmed", "Unknown")),
			# If reporting date is missing, assume there is no reporting delay
			ReportingDate = ReportingDate %>% if_else(is.na(.), true = DateFS, false = .),
			# If day and/or month are missing, put then halfway the month and/or year
			DayOfBirth   =  DayOfBirth   %>% if_else(is.na(.), true = 15L, false = .),
			MonthOfBirth =  MonthOfBirth %>% if_else(is.na(.), true =  7L, false = .),
			# Sex is a factor. Recode labels
			Sex = Sex %>%
				na_if("U") %>%
				factor(levels = c("M", "F", "T", "G")) %>%
				fct_recode(Male = "M", Female = "F", Transgender = "T", `Gender neutral` = "G"),
			# In categories, replace NA by Unknown
			cat1 = cat1 %>% replace_na("Unknown"),
			cat2 = cat2 %>% replace_na("Unknown"),
			cat3 = cat3 %>% replace_na("Unknown"),
			# Derived quantities
			DateOfBirth = str_c(YearOfBirth, MonthOfBirth, DayOfBirth, sep = "-") %>% as.Date,
			Age = (DateFS - DateOfBirth) %>% as.numeric %>% "/"(365.25) %>% floor) %>%
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
	data %>%
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
