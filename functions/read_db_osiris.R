# Description:
# This function reads data from the OSIRIS database,
# does some mutations and returns a tibble
#
# Arguments:
# database  = database name
# selection = database selection
#
# Value:
# Tibble
#
# Author:
# Jan van de Kassteele

read_db_osiris <- function(database, selection) {

	#
	# Init ----
	#

	# Load packages
	library(odbc)

	#
	# Import data from database ----
	#

	# Connect to database
	con <- dbConnect(
		drv = odbc(),
		driver = "ODBC Driver 11 for SQL Server",
		server = "RIVM-SQLDB-W03P.rivm.ssc-campus.nl",
		database = database,
		uid = credentials["OSIRIS", "username"],
		pwd = credentials["OSIRIS", "password"])

	# Get query
	data <- dbGetQuery(
		conn = con,
		statement = str_c("select * from ", selection)) %>%
		as_tibble

	# Close connection
	dbDisconnect(con)

	#
	# Clean data ----
	#

	data <- data %>%
		# Mutations
		mutate(
			# Set data types, just to be sure
			DiseaseName   = DiseaseName   %>% as.character,
			SubType       = SubType       %>% as.character,
			PatientID     = PatientID     %>% as.character,
			CaseID        = CaseID        %>% as.character,
			Status        = Status        %>% as.character,
			Sex           = Sex           %>% as.character,
			PC2_letters   = PC2_letters   %>% as.character,
			cat1desc      = cat1desc      %>% as.character,
			cat2desc      = cat2desc      %>% as.character,
			cat3desc      = cat3desc      %>% as.character,
			cat1          = cat1          %>% as.character,
			cat2          = cat2          %>% as.character,
			cat3          = cat3          %>% as.character,
			DayOfBirth    = DayOfBirth    %>% as.integer,
			MonthOfBirth  = MonthOfBirth  %>% as.integer,
			YearOfBirth   = YearOfBirth   %>% as.integer,
			PC4_numbers   = PC4_numbers   %>% as.integer,
			DateFS        = DateFS        %>% as.Date,
			ReportingDate = ReportingDate %>% as.Date,
			# Other mutations:
			# Status is a factor
			Status = Status %>% factor(levels = c("Confirmed", "Suspected", "Unknown")),
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
