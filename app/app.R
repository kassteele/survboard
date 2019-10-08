#
# Init ----
#

# # Load packages
library(shiny)
library(shinythemes)
library(tidyverse)
library(plotly)
library(leaflet)

#
# Load data ----
#

# # Load case.data and outbreak.data
load(file = "../data/case_data.Rdata")
load(file = "../data/outbreak_data.Rdata")

# Week sequence of the whole period
# Used for time range
week.seq <- outbreak.data %>%
	pull(WeekFS) %>%
	unique

# Initial base map
basemap <- leaflet() %>%
	addTiles(
		urlTemplate = "https://geodata.nationaalgeoregister.nl/tiles/service/wmts/brtachtergrondkaart/EPSG:3857/{z}/{x}/{y}.png") %>%
	fitBounds(
		lng1 = 3.5,  lng2 =  7.1,
		lat1 = 50.6, lat2 = 53.6)

#
# User interface ----
#

# Create page with top level navigation bar
# Add id to determine which tab is currently active
ui <- navbarPage(
	theme = shinytheme("flatly"),
	title = "Surveillance Dashboard",
	id = "dashboard",

	#
	# Explore tab ----
	#

	tabPanel(
		title = "Explore",
		icon = icon("bar-chart"),

		# Sidebar layout: sidebar + main panel
		sidebarLayout(

			# Sidebar
			sidebarPanel(
				width = 2,

				# Disease selection
				selectInput(
					inputId   = "sl_inp_disease",
					label     = "Disease name",
					selected  = "Legionella",
					choices   = case.data %>% pull(DiseaseName) %>% unique %>% sort,
					selectize = FALSE),
				# Subtype selection
				selectInput(
					inputId   = "sl_inp_subtype",
					label     = "Subtype",
					choices   = "",
					selectize = FALSE),

				# Bar
				tags$hr(
					style = "border-color: grey"),

				# Footer
				tags$footer(
					"Dashboard created by Jan van de Kassteele, Jeroen Albas & Loes Soetens",
					style = "font-size: 10px; color: grey")),

			# Main panel
			mainPanel(
				width = 10,

				# First column
				column(
					width = 6,

					# Time series ----
					# Filter button
					actionButton(
						inputId = "ab_inp_flt_time",
						label   = "Filter",
						icon    = icon("filter"),
						style   = "width:60px; height:24px; padding:0px; font-size:80%"),
					# Reset button
					actionButton(
						inputId = "ab_inp_rst_time",
						label   = "Reset",
						icon    = icon("undo"),
						style   = "width:60px; height:24px; padding:0px; font-size:80%"),
					# Filter active text
					textOutput(
						outputId = "tx_out_flt_time",
						inline   = TRUE),
					tags$head(
						tags$style("#tx_out_flt_time{font-size:80%; font-style: italic;}")),
					# Plot
					plotlyOutput(
						outputId = "pl_out_time",
						height   = "300px"),

					# Bar
					tags$hr(
						style = "border-color: grey"),

					# Map ----
					# Filter button
					actionButton(
						inputId = "ab_inp_flt_map",
						label   = "Filter",
						icon    = icon("filter"),
						style   = "width:60px; height:24px; padding:0px; font-size:80%"),
					# Reset button
					actionButton(
						inputId = "ab_inp_rst_map",
						label   = "Reset",
						icon    = icon("undo"),
						style   = "width:60px; height:24px; padding:0px; font-size:80%"),
					# Decrease circle radius button
					actionButton(
						inputId = "ab_inp_dec_map",
						label   = "",
						icon    = icon("circle"),
						style   = "width:60px; height:24px; padding:0px; font-size:25%"),
					# Increase circle radius button
					actionButton(
						inputId = "ab_inp_inc_map",
						label   = "",
						icon    = icon("circle"),
						style   = "width:60px; height:24px; padding:0px; font-size:80%"),
					# Filter active text
					textOutput(
						outputId = "tx_out_flt_map",
						inline   = TRUE),
					tags$head(
						tags$style("#tx_out_flt_map{font-size:80%; font-style: italic;}")),
					# Plot
					leafletOutput(
						outputId = "pl_out_map",
						height   = "450px")),

				# Second column
				column(
					width = 6,

					# Age and sex ----
					# Insert row
					fluidRow(

						# First column
						column(
							width = 10,

							# Filter button
							actionButton(
								inputId = "ab_inp_flt_agesex",
								label   = "Filter",
								icon    = icon("filter"),
								style   = "width:60px; height:24px; padding:0px; font-size:80%"),
							# Reset button
							actionButton(
								inputId = "ab_inp_rst_agesex",
								label   = "Reset",
								icon    = icon("undo"),
								style   = "width:60px; height:24px; padding:0px; font-size:80%"),
							# Filter active text
							textOutput(
								outputId = "tx_out_flt_agesex",
								inline   = TRUE),
							tags$head(
								tags$style("#tx_out_flt_agesex{font-size:80%; font-style: italic;}")),
							# Plot
							plotlyOutput(
								outputId = "pl_out_agesex",
								height   = "300px")),

						# Second column
						column(
							width = 2,

							# Some space
							br(),
							br(),
							br(),
							# Sex selection checkboxes
							checkboxGroupInput(
								inputId      = "ch_inp_sel_sex",
								label        = NULL,
								choiceNames  = list("Males", "Females"),
								selected     = list("Male", "Female"),
								choiceValues = list("Male", "Female")),
							# Age selection checkboxes
							checkboxGroupInput(
								inputId      = "ch_inp_sel_age",
								label        = NULL,
								choiceNames  = list("0 - 4", "5 - 14", "15 - 25", "25 - 64", "65+"),
								selected     = list("0 - 4", "5 - 14", "15 - 25", "25 - 64", "65+"),
								choiceValues = list("0 - 4", "5 - 14", "15 - 25", "25 - 64", "65+")))),

					# Bar
					tags$hr(
						style = "border-color: grey"),

					# Categories
					tabsetPanel(

						# Cat 1 ----
						# Tab title
						tabPanel(
							title = uiOutput(
								outputId = "tb_out_cat1"),
							# Filter button
							actionButton(
								inputId = "ab_inp_flt_cat1",
								label   = "Filter",
								icon    = icon("filter"),
								style   = "width:60px; height:24px; padding:0px; font-size:80%"),
							# Reset button
							actionButton(
								inputId = "ab_inp_rst_cat1",
								label   = "Reset",
								icon    = icon("undo"),
								style   = "width:60px; height:24px; padding:0px; font-size:80%"),
							# Sort button
							actionButton(
								inputId = "ab_inp_srt_cat1",
								label   = "Sort",
								icon    = icon("sort"),
								style   = "width:60px; height:24px; padding:0px; font-size:80%"),
							# Filter active text
							textOutput(
								outputId = "tx_out_flt_cat1",
								inline   = TRUE),
							tags$head(
								tags$style("#tx_out_flt_cat1{font-size:80%; font-style: italic;}")),
							# Plot
							plotlyOutput(
								outputId = "pl_out_cat1",
								height   = "450px")),

						# Cat 2 ----
						tabPanel(
							# Tab title
							title = uiOutput(
								outputId = "tb_out_cat2"),
							# Filter button
							actionButton(
								inputId = "ab_inp_flt_cat2",
								label   = "Filter",
								icon    = icon("filter"),
								style   = "width:60px; height:24px; padding:0px; font-size:80%"),
							# Reset button
							actionButton(
								inputId = "ab_inp_rst_cat2",
								label   = "Reset",
								icon    = icon("undo"),
								style   = "width:60px; height:24px; padding:0px; font-size:80%"),
							# Sort button
							actionButton(
								inputId = "ab_inp_srt_cat2",
								label   = "Sort",
								icon    = icon("sort"),
								style   = "width:60px; height:24px; padding:0px; font-size:80%"),
							# Filter active text
							textOutput(
								outputId = "tx_out_flt_cat2",
								inline   = TRUE),
							tags$head(
								tags$style("#tx_out_flt_cat2{font-size:80%; font-style: italic;}")),
							# Plot
							plotlyOutput(
								outputId = "pl_out_cat2",
								height   = "450px")),

						# Cat 3 ----
						tabPanel(
							# Tab title
							title = uiOutput(
								outputId = "tb_out_cat3"),
							# Filter button
							actionButton(
								inputId = "ab_inp_flt_cat3",
								label   = "Filter",
								icon    = icon("filter"),
								style   = "width:60px; height:24px; padding:0px; font-size:80%"),
							# Reset button
							actionButton(
								inputId = "ab_inp_rst_cat3",
								label   = "Reset",
								icon    = icon("undo"),
								style   = "width:60px; height:24px; padding:0px; font-size:80%"),
							# Sort button
							actionButton(
								inputId = "ab_inp_srt_cat3",
								label   = "Sort",
								icon    = icon("sort"),
								style   = "width:60px; height:24px; padding:0px; font-size:80%"),
							# Filter active text
							textOutput(
								outputId = "tx_out_flt_cat3",
								inline   = TRUE),
							tags$head(
								tags$style("#tx_out_flt_cat3{font-size:80%; font-style: italic;}")),
							# Plot
							plotlyOutput(
								outputId = "pl_out_cat3",
								height   = "450px")))))
		) # End sidebarLayout
	), # End tabPanel Explore

	#
	# Signals tab ----
	#

	tabPanel(
		title = "Signals",
		icon  = icon("bell"),

		# Sidebar layout: sidebar + main panel
		sidebarLayout(
			# Sidebar
			sidebarPanel(
				width = 2,

				# Week selection
				dateInput(
					inputId = "dt_inp_week",
					label   = "Week",
					value   = week.seq %>% last(),
					weekstart = 1),
				# Plus/minus 1 week buttons
				tags$p(
					actionButton(
						inputId = "ab_inp_m1w",
						label   = NULL,
						icon    = icon("minus"),
						style   = "width:24px; height:24px; padding:0px; font-size:80%"),
					actionButton(
						inputId = "ab_inp_p1w",
						label   = NULL,
						icon    = icon("plus"),
						style   = "width:24px; height:24px; padding:0px; font-size:80%"),
					" 1 week"),
				# Plus/minus 4 weeks buttons
				tags$p(
					actionButton(
						inputId = "ab_inp_m4w",
						label   = NULL,
						icon    = icon("minus"),
						style   = "width:24px; height:24px; padding:0px; font-size:80%"),
					actionButton(
						inputId = "ab_inp_p4w",
						label   = NULL,
						icon    = icon("plus"),
						style   = "width:24px; height:24px; padding:0px; font-size:80%"),
					" 4 weeks"),
				# Plus/minus 52 weeks buttons
				tags$p(
					actionButton(
						inputId = "ab_inp_m52w",
						label   = NULL,
						icon    = icon("minus"),
						style   = "width:24px; height:24px; padding:0px; font-size:80%"),
					actionButton(
						inputId = "ab_inp_p52w",
						label   = NULL,
						icon    = icon("plus"),
						style   = "width:24px; height:24px; padding:0px; font-size:80%"),
					" 52 weeks"),

				# Bar
				tags$hr(
					style = "border-color: grey"),

				# Refine output:
				# Only show the top n signals with at least x cases in the past y weeks
				"Only show the top",
				sliderInput(
					inputId = "sl_inp_top",
					label   = NULL,
					min     = 1, max = 30, value = 15, step = 1,
					ticks   = FALSE),
				"signals with at least",
				sliderInput(
					inputId = "sl_inp_cases",
					label   = NULL,
					min     = 0, max = 5, value = 2, step = 1,
					ticks   = FALSE),
				"cases in the last",
				sliderInput(
					inputId = "sl_inp_lastweeks",
					label   = NULL,
					min     = 1, max = 8, value = 4, step = 1,
					ticks   = FALSE),
				"weeks.",

				# Bar
				tags$hr(
					style = "border-color: grey"),

				# 	# Directly explore selected disease and subtype
				# 	selectInput(
				# 		inputId   = "sl_inp_diseasesubtype",
				# 		label     = "Directly explore",
				# 		choices   = "",
				# 		selectize = FALSE),
				# 	# Show button
				# 	actionButton(
				# 		inputId = "ab_inp_diseasesubtype",
				# 		label   = "Show",
				# 		icon    = icon("bar-chart"),
				# 		style   = "width:60px; height:24px; padding:0px; font-size:80%"),
				#

				# Footer
				tags$footer(
					"Dashboard created by Jan van de Kassteele, Jeroen Albas & Loes Soetens",
					style = "font-size: 10px; color: grey")),

			# Main panel
			mainPanel(
				width = 10,

				# Plot
				plotlyOutput(
					outputId = "pl_out_signals",
					height = "850px"))

		) # End sidebarLayout
	) # End tabPanel Signals
) # End navbarPage

#
# Server ----
#

server <- function(input, output, session) {

	#
	# Disease selection ----
	#

	# When DiseaseName in sl_inp_disease updates,
	# then update the choices in sl_inp_subtype of the corresponding SubTypes
	observe({
		updateSelectInput(
			session = session,
			inputId = "sl_inp_subtype",
			label   = "Subtype",
			choices = c(
				`No subtype` = "",
				case.data %>%
					filter(DiseaseName == input$sl_inp_disease) %>%
					pull(SubType) %>% unique %>% sort))
	})

	#
	# Initital settings ----
	#

	# Create reactive value dis
	# This is a list with reactive values that can trigger updates
	# We could have used eventReactive() with output value 'dis',
	# but reactiveValues() i.c.w. observeEvent seems easier
	dis <- reactiveValues()

	# When sl_inp_disease or sl_inp_subtype updates,
	# Then update reactive value dis
	observeEvent(
		eventExpr = c(
			input$sl_inp_disease,
			input$sl_inp_subtype),
		handlerExpr = {

			# First filter case.data on DiseaseName
			dis$case.data <- case.data %>%
				filter(DiseaseName == input$sl_inp_disease)

			# Then filter dis$case.data on SubType,
			# but ONLY if input$sl_inp_subtype is been chosen (i.e. is not equal to "")
			# In this way, if input$sl_inp_subtype has not been chosen, SubType will be ignored
			if (input$sl_inp_subtype != "") {
				dis$case.data <- dis$case.data %>%
					filter(SubType == input$sl_inp_subtype)
			}

			# For outbreak.data, the above two-step filter operation can be done in one step,
			# because SubType = "" (NA in outbreak.data) has its own records in outbreak.data
			dis$outbreak.data <- outbreak.data %>%
				filter(
					DiseaseName                  == input$sl_inp_disease,
					(SubType %>% replace_na("")) == input$sl_inp_subtype)

			# Initially, all records are selected in dis$case.data
			# i.e. no active filters
			dis$case.data <- dis$case.data %>%
				mutate(
					select.time = TRUE,
					select.map  = TRUE,
					select.age  = TRUE,
					select.sex  = TRUE,
					select.cat1 = TRUE,
					select.cat2 = TRUE,
					select.cat3 = TRUE)

			# Remove all "Filter active" texts
			output$tx_out_flt_time   <- renderText({})
			output$tx_out_flt_map    <- renderText({})
			output$tx_out_flt_agesex <- renderText({})
			output$tx_out_flt_cat1   <- renderText({})
			output$tx_out_flt_cat1   <- renderText({})
			output$tx_out_flt_cat1   <- renderText({})

			# Set initial radius and step size for the circles on the map
			n <- dis$case.data %>%
				group_by(PC4_numbers) %>%
				summarize(Cases = n()) %>%
				pull(Cases) %>%
				mean
			dis$step <- 500/n
			dis$rad  <- 2*dis$step

			# Set inital sorting of categories 1-3 to TRUE
			dis$sort$cat1 <- TRUE
			dis$sort$cat2 <- TRUE
			dis$sort$cat3 <- TRUE

			# Plot initial base map
			# The rest of the map is added in the Map section
			# and is based on reactive values such as filter actions
			output$pl_out_map <- renderLeaflet(basemap)
		})

	#
	# Time series ----
	#

	# Plot
	output$pl_out_time <- renderPlotly(
		expr = {

			# Create plot.data from dis$case.data
			plot.data <- dis$case.data %>%
				# Apply filters
				filter(select.time & select.map & select.age & select.sex & select.cat1 & select.cat2 & select.cat3) %>%
				# Aggregate cases by week
				mutate(WeekFS = WeekFS %>% factor(levels = week.seq %>% as.character)) %>%
				group_by(WeekFS, .drop = FALSE) %>%
				summarize(Cases = n()) %>%
				mutate(WeekFS = as.Date(WeekFS)) %>%
				# Add outbreak coloring from dis$outbreak.data
				left_join(dis$outbreak.data %>% select(WeekFS, Color), by = c("WeekFS" = "WeekFS"))

			# Set plot limits
			if (all(dis$case.data$select.time)) {
				# If no time filter is active, xlim is the entire week sequence
				xlim <- week.seq %>% range %>% "+"(c(-4, 4))
			} else {
				# Else, xlim the selected time range
				xlim <- dis$case.data %>% filter(select.time) %>% pull(WeekFS) %>% range %>% "+"(c(-4, 4))
			}
			# ylim is always the same
			ylim <- c(0, plot.data %>% pull(Cases) %>% max %>% "*"(1.05))

			# Plot
			plot_ly(
				source = "pl_out_time") %>%
				add_bars(
					data = plot.data,
					x = ~ WeekFS,
					y = ~ Cases,
					marker = list(color = ~ Color),
					name = "Cases") %>%
				add_lines(
					data = dis$outbreak.data,
					x = ~ WeekFS,
					y = ~ mu.baseline %>% "*"(sum(plot.data$Cases)/sum(dis$outbreak.data$Cases)) %>% round(digits = 2),
					line = list(color = "black", width = 1),
					name = "Baseline") %>%
				layout(
					bargap = 0.05,
					xaxis = list(
						type = "date",
						range = xlim,
						title = "Week",
						# Add range selector buttons on top
						rangeselector = list(
							buttons = list(
								list(
									step = "month",
									count = 3,
									label = "3 mo",
									stepmode = "backward"),
								list(
									step = "month",
									count = 6,
									label = "6 mo",
									stepmode = "backward"),
								list(
									step = "year",
									count = 1,
									label = "1 yr",
									stepmode = "backward"),
								list(
									step = "all"))),
						# Add range slider at the bottom
						rangeslider = list(
							thickness = 0.15)),
					yaxis = list(
						range = ylim),
					hovermode = "compare",
					dragmode = "zoom",
					showlegend = TRUE,
					legend = list(
						orientation = "h",
						y = -0.5))
		})

	# When ab_inp_flt_time is pressed
	observeEvent(
		eventExpr = input$ab_inp_flt_time,
		handlerExpr = {

			# Get plot information from pl_out_time
			# Use plotly_relayout to return plot limits
			# unlist output to force it to a vector
			# This prevents buggy behavior in filtering
			plot.info <- event_data(
				event  = "plotly_relayout",
				source = "pl_out_time") %>%
				unlist
			# Do nothing if plot.info does not contain "xaxis"
			if (plot.info[1] %>% names %>% str_detect(pattern = "xaxis") %>% `!`) return()

			# Set elements of select.time in dis$case.data to TRUE if WeekFS is between xaxis.range
			dis$case.data <- dis$case.data %>%
				mutate(select.time = WeekFS %>% between(
					left  = plot.info[1] %>% as.Date,
					right = plot.info[2] %>% as.Date))

			# Display "Filter active" text
			output$tx_out_flt_time <- renderText({"Filter active"})
		})

	# When ab_inp_rst_time is pressed
	observeEvent(
		eventExpr = input$ab_inp_rst_time,
		handlerExpr = {

			# Reset all elements of select.time in dis$case.data back to TRUE
			dis$case.data <- dis$case.data %>%
				mutate(select.time = TRUE)

			# Remove "Filter active" text
			output$tx_out_flt_time <- renderText({})
		})

	#
	# Map ----
	#

	# When dis$case.data updates, the circle markers should be redrawn
	# By using the leafletProxy() function, ONLY the circle markers are redrawn, not the entire map
	# This preserves the map view
	#
	# Circles should also be redrawn after pressing the map reset button,
	# because this action redraws the base map without the circles
	# However, if dis$case.data was not updated (because no map filter was active),
	# the circles are then added by an updated input$ab_inp_rst_map value
	#
	# Circles are of course redrawn after pressing the decrease/increase circle radius buttons
	observeEvent(
		eventExpr = c(
			dis$case.data,
			input$ab_inp_rst_map,
			input$ab_inp_dec_map,
			input$ab_inp_inc_map),
		handlerExpr = {

			# Create plot.data from dis$case.data
			plot.data <- dis$case.data %>%
				# Apply filters
				filter(select.time & select.map & select.age & select.sex & select.cat1 & select.cat2 & select.cat3) %>%
				# Filter out missing coordinates
				filter(!is.na(x)) %>%
				# Aggregate cases by PC4 (and xy-coordinates to keep them)
				group_by(PC4_numbers, x, y) %>%
				summarize(Cases = n()) %>%
				# Radius of the circles
				mutate(radius = dis$rad*sqrt(Cases))

			# Plot
			leafletProxy(
				mapId = "pl_out_map",
				data = plot.data) %>%
				clearShapes() %>%
				addCircles(
					lng = ~ x, lat = ~ y, radius = ~ radius,
					color = "#a90061", opacity = 1, weight = 0.5,
					fillColor = "#a90061", fillOpacity = 0.33,
					label = ~ paste0(PC4_numbers, ": ", Cases))
		})

	# When ab_inp_flt_map is pressed
	observeEvent(
		eventExpr = input$ab_inp_flt_map,
		handlerExpr = {

			# Get plot information from pl_out_map
			plot.info <- input$pl_out_map_bounds

			# Set elements of select.map in dis$case.data to TRUE if coordinates are between map limits
			dis$case.data <- dis$case.data %>%
				mutate(
					select.map =
						x %>% between(plot.info$west,  plot.info$east) &
						y %>% between(plot.info$south, plot.info$north))

			# Display "Filter active" text
			output$tx_out_flt_map <- renderText({"Filter active"})
		})

	# When ab_inp_rst_map is pressed
	observeEvent(
		eventExpr = input$ab_inp_rst_map,
		handlerExpr = {

			# Reset all elements of select.map in dis$case.data back to TRUE
			dis$case.data <- dis$case.data %>%
				mutate(select.map = TRUE)

			# Return to base map
			output$pl_out_map <- renderLeaflet(basemap)

			# Remove "Filter active" text
			output$tx_out_flt_map <- renderText({})
		})

	# When ab_inp_dec_map or ab_inp_inc_map are pressed,
	# update dis$rad with +/- dis$step
	observeEvent(
		eventExpr = input$ab_inp_dec_map,
		handlerExpr = {
			newrad <- dis$rad - dis$step
			if (newrad <= 0) newrad <- 0
			dis$rad <- newrad
		})
	observeEvent(
		eventExpr = input$ab_inp_inc_map,
		handlerExpr = {
			newrad <- dis$rad + dis$step
			dis$rad <- newrad
		})

	#
	# Age and sex ----
	#

	output$pl_out_agesex <- renderPlotly(
		expr = {

			# Create plot.data from dis$case.data
			plot.data <- dis$case.data %>%
				# Apply filters
				filter(select.time & select.map & select.age & select.sex & select.cat1 & select.cat2 & select.cat3) %>%
				# Filter on selected age/sex input
				filter(Agecat %in% input$ch_inp_sel_age) %>%
				filter(Sex    %in% input$ch_inp_sel_sex) %>%
				# Only plot males and females
				mutate(Sex = Sex %>% factor(levels = c("Male", "Female"))) %>%
				# Aggregate cases by Agecat and Sex
				group_by(Agecat, Sex, .drop = FALSE) %>%
				summarize(Cases = n()) %>%
				# Trick to plot males left, females right
				mutate(
					Cases = ifelse(Sex == "Male", yes = -Cases, no = Cases))

			# Set plot limits
			xlim <- plot.data %>% pull(Cases) %>% abs %>% max %>% "*"(c(-1, 1)) %>% "*"(1.05)

			# Plot
			plot_ly(
				data   = plot.data,
				source = "pl_out_agesex") %>%
				add_bars(
					x = ~ Cases, y = ~ Agecat,
					color = ~ Sex, colors = c("#007bc7", "#a90061"),
					hoverinfo = "text", text = ~ abs(Cases)) %>%
				layout(
					barmode = "overlay",
					bargap = 0.05,
					xaxis = list(
						range    = xlim,
						tickvals = xlim %>% pretty(n = 6),
						ticktext = xlim %>% pretty(n = 6) %>% abs),
					yaxis = list(range = c(-0.6, 4.6)),
					hovermode = "compare",
					dragmode = "zoom",
					showlegend = FALSE)
		})

	# When ab_inp_flt_agesex is pressed
	observeEvent(
		eventExpr = input$ab_inp_flt_agesex,
		handlerExpr = {

			# Set elements of select.age/sex in dis$case.data to TRUE if age/sex has been selected
			dis$case.data <- dis$case.data %>%
				mutate(
					select.age = Agecat %in% input$ch_inp_sel_age,
					select.sex = Sex    %in% input$ch_inp_sel_sex)

			# Display "Filter active" text
			output$tx_out_flt_agesex <- renderText({"Filter active"})
		})

	# When ab_inp_rst_agesex is pressed
	observeEvent(
		eventExpr = input$ab_inp_rst_agesex,
		handlerExpr = {

			# Reset all elements of select.age/sex in dis$case.data back to TRUE
			dis$case.data <- dis$case.data %>%
				mutate(
					select.age = TRUE,
					select.sex = TRUE)

			# Check all age groups
			updateCheckboxInput(
				session = session,
				inputId = "ch_inp_sel_age",
				value = c("0 - 4", "5 - 14", "15 - 25", "25 - 64", "65+"))

			# Check both sex
			updateCheckboxInput(
				session = session,
				inputId = "ch_inp_sel_sex",
				value = c("Male", "Female"))

			# Remove "Filter active" text
			output$tx_out_flt_agesex <- renderText({})
		})

	#
	# Category 1 ----
	#

	# Plot
	output$pl_out_cat1 <- renderPlotly(
		expr = {

			# Create plot.data from dis$case.data
			plot.data <- dis$case.data %>%
				# Apply filters
				filter(select.time & select.map & select.age & select.sex & select.cat1 & select.cat2 & select.cat3) %>%
				# Aggregate cases by cat1
				group_by(cat1, .drop = TRUE) %>%
				summarize(Cases = n()) %>%
				# Make sure cat1 is a factor
				mutate(cat1 = cat1 %>% factor)

			# Sort plot.data by number of cases?
			if (dis$sort$cat1) {
				plot.data <- plot.data %>%
					mutate(cat1 = cat1 %>% fct_reorder(.x = Cases, .desc = TRUE))
			}

			# Set plot limits
			xlim <- c(-0.6, plot.data %>% pull(cat1) %>% as.integer %>% max %>% `-`(0.4))
			ylim <- c(-0.3, plot.data %>% pull(Cases) %>% max %>% log10 %>% "*"(1.05))

			# Plot
			plot_ly(
				data   = plot.data,
				source = "pl_out_cat1") %>%
				add_bars(
					x = ~ cat1, y = ~ Cases, key = ~ cat1,
					marker = list(color = "#007bc7"),
					hoverinfo = "text", text = ~ Cases) %>%
				layout(
					bargap = 0.05,
					autosize = FALSE,
					margin = list(b = 150),
					xaxis = list(range = xlim, title = FALSE, tickangle = 45),
					yaxis = list(range = ylim, type = "log"),
					dragmode = "select",
					showlegend = FALSE)
		})

	# When ab_inp_flt_cat1 is pressed
	observeEvent(
		eventExpr = input$ab_inp_flt_cat1,
		handlerExpr = {

			# Get plot information from pl_out_cat1
			plot.info <- event_data(
				event  = "plotly_selected",
				source = "pl_out_cat1")
			if (is.null(plot.info)) return()

			# Set elements of select.cat1 in dis$case.data to TRUE if cat1 has been selected
			dis$case.data <- dis$case.data %>%
				mutate(select.cat1 = cat1 %in% plot.info$key)

			# Display "Filter active" text
			output$tx_out_flt_cat1 <- renderText({"Filter active"})
		})

	# When ab_inp_rst_cat1 is pressed
	observeEvent(
		eventExpr = input$ab_inp_rst_cat1,
		handlerExpr = {

			# Reset all elements of select.cat1 in dis$case.data back to TRUE
			dis$case.data <- dis$case.data %>%
				mutate(select.cat1 = TRUE)

			# Remove "Filter active" text
			output$tx_out_flt_cat1 <- renderText({})
		})

	# When ab_inp_srt_cat1 is pressed
	observeEvent(
		eventExpr = input$ab_inp_srt_cat1,
		handlerExpr = {

			# Invert the value of dis$sort$cat1
			dis$sort$cat1 <- !dis$sort$cat1
		})

	# Tab title cat1
	output$tb_out_cat1 = renderText({
		dis$case.data$cat1desc[1] %>% replace_na("")
	})

	#
	# Category 2 ----
	#

	# Plot
	output$pl_out_cat2 <- renderPlotly(
		expr = {

			# Create plot.data from dis$case.data
			plot.data <- dis$case.data %>%
				# Apply filters
				filter(select.time & select.map & select.age & select.sex & select.cat1 & select.cat2 & select.cat3) %>%
				# Aggregate cases by cat2
				group_by(cat2, .drop = TRUE) %>%
				summarize(Cases = n()) %>%
				# Make sure cat2 is a factor
				mutate(cat2 = cat2 %>% factor)

			# Sort plot.data by number of cases?
			if (dis$sort$cat2) {
				plot.data <- plot.data %>%
					mutate(cat2 = cat2 %>% fct_reorder(.x = Cases, .desc = TRUE))
			}

			# Set plot limits
			xlim <- c(-0.6, plot.data %>% pull(cat2) %>% as.integer %>% max %>% `-`(0.4))
			ylim <- c(-0.3, plot.data %>% pull(Cases) %>% max %>% log10 %>% "*"(1.05))

			# Plot
			plot_ly(
				data   = plot.data,
				source = "pl_out_cat2") %>%
				add_bars(
					x = ~ cat2, y = ~ Cases, key = ~ cat2,
					marker = list(color = "#007bc7"),
					hoverinfo = "text", text = ~ Cases) %>%
				layout(
					bargap = 0.05,
					autosize = FALSE,
					margin = list(b = 150),
					xaxis = list(range = xlim, title = FALSE, tickangle = 45),
					yaxis = list(range = ylim, type = "log"),
					dragmode = "select",
					showlegend = FALSE)
		})

	# When ab_inp_flt_cat2 is pressed
	observeEvent(
		eventExpr = input$ab_inp_flt_cat2,
		handlerExpr = {

			# Get plot information from pl_out_cat2
			plot.info <- event_data(
				event  = "plotly_selected",
				source = "pl_out_cat2")
			if (is.null(plot.info)) return()

			# Set elements of select.cat2 in dis$case.data to TRUE if cat2 has been selected
			dis$case.data <- dis$case.data %>%
				mutate(select.cat2 = cat2 %in% plot.info$key)

			# Display "Filter active" text
			output$tx_out_flt_cat2 <- renderText({"Filter active"})
		})

	# When ab_inp_rst_cat2 is pressed
	observeEvent(
		eventExpr = input$ab_inp_rst_cat2,
		handlerExpr = {

			# Reset all elements of select.cat2 in dis$case.data back to TRUE
			dis$case.data <- dis$case.data %>%
				mutate(select.cat2 = TRUE)

			# Remove "Filter active" text
			output$tx_out_flt_cat2 <- renderText({})
		})

	# When ab_inp_srt_cat2 is pressed
	observeEvent(
		eventExpr = input$ab_inp_srt_cat2,
		handlerExpr = {

			# Invert the value of dis$sort$cat2
			dis$sort$cat2 <- !dis$sort$cat2
		})

	# Tab title cat2
	output$tb_out_cat2 = renderText({
		dis$case.data$cat2desc[1] %>% replace_na("")
	})

	#
	# Category 3 ----
	#

	# Plot
	output$pl_out_cat3 <- renderPlotly(
		expr = {

			# Create plot.data from dis$case.data
			plot.data <- dis$case.data %>%
				# Apply filters
				filter(select.time & select.map & select.age & select.sex & select.cat1 & select.cat2 & select.cat3) %>%
				# Aggregate cases by cat3
				group_by(cat3, .drop = TRUE) %>%
				summarize(Cases = n()) %>%
				# Make sure cat3 is a factor
				mutate(cat1 = cat3 %>% factor)

			# Sort plot.data by number of cases?
			if (dis$sort$cat3) {
				plot.data <- plot.data %>%
					mutate(cat3 = cat3 %>% fct_reorder(.x = Cases, .desc = TRUE))
			}

			# Set plot limits
			xlim <- c(-0.6, plot.data %>% pull(cat3) %>% as.integer %>% max %>% `-`(0.4))
			ylim <- c(-0.3, plot.data %>% pull(Cases) %>% max %>% log10 %>% "*"(1.05))

			# Plot
			plot_ly(
				data   = plot.data,
				source = "pl_out_cat3") %>%
				add_bars(
					x = ~ cat3, y = ~ Cases, key = ~ cat3,
					marker = list(color = "#007bc7"),
					hoverinfo = "text", text = ~ Cases) %>%
				layout(
					bargap = 0.05,
					autosize = FALSE,
					margin = list(b = 150),
					xaxis = list(range = xlim, title = FALSE, tickangle = 45),
					yaxis = list(range = ylim, type = "log"),
					dragmode = "select",
					showlegend = FALSE)
		})

	# When ab_inp_flt_cat3 is pressed
	observeEvent(
		eventExpr = input$ab_inp_flt_cat3,
		handlerExpr = {

			# Get plot information from pl_out_cat3
			plot.info <- event_data(
				event  = "plotly_selected",
				source = "pl_out_cat3")
			if (is.null(plot.info)) return()

			# Set elements of select.cat3 in dis$case.data to TRUE if cat3 has been selected
			dis$case.data <- dis$case.data %>%
				mutate(select.cat3 = cat3 %in% plot.info$key)

			# Display "Filter active" text
			output$tx_out_flt_cat3 <- renderText({"Filter active"})
		})

	# When ab_inp_rst_cat3 is pressed
	observeEvent(
		eventExpr = input$ab_inp_rst_cat3,
		handlerExpr = {

			# Reset all elements of select.cat3 in dis$case.data back to TRUE
			dis$case.data <- dis$case.data %>%
				mutate(select.cat3 = TRUE)

			# Remove "Filter active" text
			output$tx_out_flt_cat3 <- renderText({})
		})

	# When ab_inp_srt_cat3 is pressed
	observeEvent(
		eventExpr = input$ab_inp_srt_cat3,
		handlerExpr = {

			# Invert the value of dis$sort$cat3
			dis$sort$cat3 <- !dis$sort$cat3
		})

	# Tab title cat3
	output$tb_out_cat3 = renderText({
		dis$case.data$cat3desc[1] %>% replace_na("")
	})

	#
	# Signals ----
	#

	# Plot
	output$pl_out_signals <- renderPlotly(
		expr = {

			# A high outbreak probability with 0 cases is not relevant
			# Therefore, only show diseases with at least 2 cases in the past 4 weeks
			disease.names <- outbreak.data %>%
				filter(WeekFS %in% seq(
					from = input$dt_inp_week - 7*(input$sl_inp_lastweeks - 1),
					to   = input$dt_inp_week,
					by   = "week")) %>%
				group_by(DiseaseName_SubType) %>%
				summarize(Cases = sum(Cases)) %>%
				filter(Cases >= input$sl_inp_cases) %>%
				pull(DiseaseName_SubType)

			# Create plot.data from outbreak.data
			plot.data <- outbreak.data %>%
				# Filter outbreak.data on selected week and relevant diseases
				filter(
					WeekFS == input$dt_inp_week,
					DiseaseName_SubType %in% disease.names) %>%
				# Select top n based on p.outbreak
				top_n(n = input$sl_inp_top, wt = p.outbreak) %>%
				# Reorder by p.outbreak
				mutate(DiseaseName_SubType = DiseaseName_SubType %>% fct_reorder(.x = p.outbreak))

			# Plot
			plot_ly(
				data = plot.data,
				source = "pl_out_signals") %>%
				add_bars(
					x = ~ p.outbreak %>% "*"(100) %>% round(digits = 1),
					y = ~ DiseaseName_SubType,
					marker = list(color = ~ Color)) %>%
				layout(
					bargap = 0.05,
					xaxis = list(range = c(-1, 101), dtick = 10, title = "Outbreak probability (%)"),
					yaxis = list(title = FALSE),
					dragmode = "zoom",
					showlegend = FALSE)
		})

	# Floor selected date to week
	observeEvent(
		eventExpr = input$dt_inp_week,
		handlerExpr = updateDateInput(
			session = session,
			inputId = "dt_inp_week",
			value = input$dt_inp_week %>% cut(breaks = "week")))

	# Date action buttons
	# Minus or plus 1 week
	observeEvent(
		eventExpr = input$ab_inp_m1w,
		handlerExpr = updateDateInput(
			session = session,
			inputId = "dt_inp_week",
			value = input$dt_inp_week - 7))
	observeEvent(
		eventExpr = input$ab_inp_p1w,
		handlerExpr = updateDateInput(
			session = session,
			inputId = "dt_inp_week",
			value = input$dt_inp_week + 7))

	# Minus or plus 4 weeks
	observeEvent(
		eventExpr = input$ab_inp_m4w,
		handlerExpr = updateDateInput(
			session = session,
			inputId = "dt_inp_week",
			value = input$dt_inp_week - 28))
	observeEvent(
		eventExpr = input$ab_inp_p4w,
		handlerExpr = updateDateInput(
			session = session,
			inputId = "dt_inp_week",
			value = input$dt_inp_week + 28))

	# Minus or plus 52 weeks
	observeEvent(
		eventExpr = input$ab_inp_m52w,
		handlerExpr = updateDateInput(
			session = session,
			inputId = "dt_inp_week",
			value = input$dt_inp_week - 364))
	observeEvent(
		eventExpr = input$ab_inp_p52w,
		handlerExpr = updateDateInput(
			session = session,
			inputId = "dt_inp_week",
			value = input$dt_inp_week + 364))

	# # Directly explore selection
	# # When DiseaseName_SubType in dis$plot.data updates,
	# # then update the choices in sl_inp_diseasesubtype
	# observe({
	# 	updateSelectInput(
	# 		session = session,
	# 		inputId = "sl_inp_diseasesubtype",
	# 		label   = "Directly explore",
	# 		choices = dis$plot.data$DiseaseName_SubType %>% levels %>% rev)
	# })
	#
	# observeEvent(
	# 	eventExpr = input$ab_inp_diseasesubtype,
	# 	handlerExpr = {
	#
	# 		# Make Explore tab active
	# 		updateTabsetPanel(
	# 			session = session,
	# 			inputId = "dashboard",
	# 			selected = "Explore")
	#
	# 		d <- dis$plot.data %>% filter(DiseaseName_SubType == input$sl_inp_diseasesubtype) %>% pull(DiseaseName)
	# 		s <- dis$plot.data %>% filter(DiseaseName_SubType == input$sl_inp_diseasesubtype) %>% pull(SubType) %>% replace_na("")
	#
	# 		# First filter case.data on DiseaseName
	# 		dis$case.data <- case.data %>%
	# 			filter(DiseaseName == d)
	#
	# 		# Then filter dis$case.data on SubType,
	# 		# but ONLY if input$sl_inp_subtype is been chosen (i.e. is not equal to "")
	# 		# In this way, if input$sl_inp_subtype has not been chosen, SubType will be ignored
	# 		if (s != "") {
	# 			dis$case.data <- dis$case.data %>%
	# 				filter(SubType == s)
	# 		}
	#
	# 		# For outbreak.data, the above two-step filter operation can be done in one step,
	# 		# because SubType = "" (NA in outbreak.data) has its own records in outbreak.data
	# 		dis$outbreak.data <- outbreak.data %>%
	# 			filter(
	# 				DiseaseName                  == d,
	# 				(SubType %>% replace_na("")) == s)
	#
	# 		# Initially, all records are selected in dis$case.data
	# 		# i.e. no active filters
	# 		dis$case.data <- dis$case.data %>%
	# 			mutate(
	# 				select.time   = TRUE,
	# 				select.map    = TRUE,
	# 				select.agesex = TRUE,
	# 				select.cat1   = TRUE,
	# 				select.cat2   = TRUE,
	# 				select.cat3   = TRUE)
	#
	# 		# Set initial radius and step size for the circles on the map
	# 		n <- dis$case.data %>%
	# 			group_by(PC4_numbers) %>%
	# 			summarize(Cases = n()) %>%
	# 			pull(Cases) %>%
	# 			mean
	# 		dis$step <- 500/n
	# 		dis$rad  <- 2*dis$step
	#
	# 		# Set inital sorting of categories 1-3 to TRUE
	# 		dis$sort$cat1 <- TRUE
	# 		dis$sort$cat2 <- TRUE
	# 		dis$sort$cat3 <- TRUE
	#
	# 		# Plot initial base map
	# 		# The rest of the map is added in the Map section
	# 		# and is based on reactive values such as filter actions
	# 		output$pl_out_map <- renderLeaflet(basemap)
	#
	# 	})

}

# #
# # Diagnostics app ----
# #
#
# ui <- fluidPage(
# 	verbatimTextOutput(outputId = "getwd"),
# 	verbatimTextOutput(outputId = "path"),
# 	tableOutput(outputId = "packages"))
#
# server <- function(input, output) {
#
# 	output$getwd <- renderText({
# 		getwd()
# 	})
#
# 	output$path <- renderText({
# 		paste0(.libPaths(), collapse = "\n")
# 	})
#
# 	output$packages <- renderTable({
# 		installed.packages()[, 1:3]
# 	})
# }

#
# Run app ----
#

shinyApp(ui = ui, server = server)
