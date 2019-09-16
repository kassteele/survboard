# Description:
# This function reads data from specified file or database,
# does some mutations and returns a dataframe
#
# Arguments:
# file = path and filename. Only xlxs file are currently supported
# database = database name
# selection = database selection
#
# Value:
# Dataframe
#
# Author:
# Jan van de Kassteele

read_db <- function(file = NULL, database, selection) {

	#
	# Init ----
	#

	# Load packages
	# library(readxl)
	library(odbc)

	#
	# Read data ----
	#

	# # Read from file or database?
	# if (!is.null(file)) {
	#
	# 	# If file, first determine file type...
	# 	ftype <- tools::file_ext(file)
	# 	# ...then load data from file
	# 	if (ftype == "xlsx") {
	# 		data <- read_xlsx(file, na = ".") # JK: . = NA in given xlsx file. Should be empty cell.
	# 	} else if (ftype == "csv") {
	# 		data <- read_csv(file)
	# 	} else {
	# 		# If file is not xlsx or csv, then stop
	# 		str_c("File type ", ftype, " not supported") %>% stop
	# 	}
	#
	# } else {
	# Load data from database

	# Windows or Linux? Needed to select driver
	# On Linux we need FreeTDS (SQL) and oracle-instantclient-basic (Oracle)
	platform <- Sys.info()["sysname"]
	attributes(platform) <- NULL

	# Connect to database
	con <- dbConnect(
		drv = odbc(),
		driver = ifelse(
			test = platform == "Windows",
			yes = "SQL Server", # Windows
			no  = "FreeTDS"),   # Linux
		server = "SQLW04-EXT-P\\RIVM01",
		database = database,
		uid = credentials["OSIRIS", "username"],
		pwd = credentials["OSIRIS", "password"])

	# Get query
	data <- dbGetQuery(
		conn = con,
		statement = str_c("select * from ", selection)) %>%
		as_tibble

	# library(ROracle)
	#
	# con <- dbConnect(
	# 	drv = dbDriver("Oracle"),
	# 	dbname = "(DESCRIPTION=(ADDRESS=(PROTOCOL=tcp)(HOST=unilabdbl01-ext-ota.rivm.nl)(PORT=1521))(CONNECT_DATA=(SID=uni64t)))",
	# 	username = credentials["LIMS", "username"],
	# 	password = credentials["LIMS", "password"])
	#
	# salmo.data <- dbGetQuery(
	# 	conn = con,
	# 	statement = "select * from unilab.AVEPIEXTRACTSALMO")
	#
	# dbDisconnect(conn = con)

	# Close connection
	dbDisconnect(con)
	# }

	#
	# Clean data ----
	#

	data <- data %>%
		# Mutations
		mutate(
			# As character
			DiseaseName = DiseaseName %>% as.character,
			SubType     = SubType     %>% as.character,
			PatientID   = PatientID   %>% as.character,
			CaseID      = CaseID      %>% as.character,
			# Status is a factor
			Status = Status %>% factor(levels = c("Confirmed", "Suspected", "Unknown")),
			# Be sure Date for stats and and reporting date are dates
			DateFS        = DateFS        %>% as.Date,
			ReportingDate = ReportingDate %>% as.Date,
			# If reporting date is missing, assume there is no reporting delay
			ReportingDate = ReportingDate %>% if_else(is.na(.), true = DateFS, false = .),
			# Be sure day, month and year of birth are integers
			DayOfBirth   = DayOfBirth  %>% as.integer,
			MonthOfBirth = DayOfBirth  %>% as.integer,
			YearOfBirth  = YearOfBirth %>% as.integer,
			# If day and/or month are missing, put then halfway the month and/or year
			DayOfBirth   =  DayOfBirth   %>% if_else(is.na(.), true = 15L, false = .),
			MonthOfBirth =  MonthOfBirth %>% if_else(is.na(.), true =  7L, false = .),
			# Postcode types
			PC4_numbers = PC4_numbers %>% as.integer,
			PC2_letters = PC2_letters %>% as.character,
			# Recode sex labels
			Sex = Sex %>%
				factor(levels = c("M", "F", "T", "G", "U")) %>%
				fct_recode(Male = "M", Female = "F", Transgender = "T", `Gender neutral` = "G", Unknown = "U") %>%
				fct_explicit_na(na_level = "Unknown"),
			# Extra categories, description as is
			cat1desc = cat1desc,
			cat2desc = cat2desc,
			cat3desc = cat3desc,
			# In contents, replace NA by Unknown
			cat1 = cat1 %>% replace_na("Unknown"),
			cat2 = cat2 %>% replace_na("Unknown"),
			cat3 = cat3 %>% replace_na("Unknown"),
			# Derived quantities
			DateOfBirth = str_c(YearOfBirth, MonthOfBirth, DayOfBirth, sep = "-") %>% as.Date,
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
