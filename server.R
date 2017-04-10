library(shiny)
source("global.R")

shinyServer(function(input, output, session) {
# ------------------------------------------------------------- SESSION ----

	session$allowReconnect(T)

# ------------------------------------------------------------- SHARED ----

	# reactive values
	v <- reactiveValues(markerId = 1,
						markerSel = NULL,
						# defaults view to the center of the atlantic ocean
						searchLoc = data.frame(lon = viewCenter[1],
											   lat = viewCenter[2],
											   zoom = viewCenter[3]))

# ---------------------------------------------------------------- MAP ----

	# render leaflet map
	output$leaf <- renderLeaflet({
		m <- leaflet(options = list(attributionControl = F)) %>%
			addTiles(
				urlTemplate = "http://{s}.google.com/vt/lyrs=s,h&x={x}&y={y}&z={z}",
				attribution = paste(
					"Map data &copy;",
					substr(Sys.Date(), 1, 4),
					" Google, Imagery &copy;",
					substr(Sys.Date(), 1, 4),
					" TerraMetrics"
				),
				options = list(
					minZoom = 3,
					maxZoom = 19,
					noWrap = T,
					subdomains = c("mt0", "mt1", "mt2", "mt3")
				)
			) %>%
			setMaxBounds(-180, -90, 180, 90)
		m
	})

	# update v$searchLoc with map search query
	observeEvent(input$select_search, {
		# evaluate geocode, but suppress messages and warnings
		options(warn = -1)
		gc <- suppressMessages(geocode(input$select_search))
		options(warn = 0)
		if (is.na(gc$lon)) {
			# defaults view to the center of the atlantic ocean
			v$searchLoc$lon <- viewCenter[1]
			v$searchLoc$lat <- viewCenter[2]
			v$searchLoc$zoom <- viewCenter[3]
		} else {
			# if user-selected region exists, center on it
			v$searchLoc$lon <- gc$lon
			v$searchLoc$lat <- gc$lat
			v$searchLoc$zoom <- 16
		}
	})

	# update map view with v$searchLoc coordinates
	observeEvent(v$searchLoc, {
		leafletProxy("leaf") %>%
			setView(
				lng = v$searchLoc$lon,
				lat = v$searchLoc$lat,
				zoom = v$searchLoc$zoom
			)
	})

	# draw markers by clicking on the map
	observeEvent(input$leaf_click, {
		id <- as.character(v$markerId)
		tmp <- input$leaf_click
		v$markerId <- v$markerId + 1
		leafletProxy("leaf") %>%
			addAwesomeMarkers(
				lng = tmp$lng,
				lat = tmp$lat,
				layerId = id,
				icon = iconSet$marker
			)
	})

	# select markers by clicking and show a 'Selected' label next to them
	observeEvent(input$leaf_marker_click, {
		tmp <- input$leaf_marker_click

		# if user clicks on an already 'Selected' marker, deselect it
		if (grepl("selected", tmp$id)) {
			leafletProxy("leaf") %>%
				addAwesomeMarkers(
					lng = tmp$lng,
					lat = tmp$lat,
					layerId = as.character(v$markerSel[3]),
					icon = iconSet$marker
				) %>%
				removeMarker("selectedLabel") %>%
				removeMarker("selectedHighlight")
			updateSelectInput(
				session,
				inputId = "select_satGet",
				choices = c("")
			)
			v$markerSel <- NULL
		} else {
			# redraw selected marker without highlighting it
			if (length(v$markerSel) > 0) {
				leafletProxy("leaf") %>%
					addAwesomeMarkers(
						lng = as.numeric(v$markerSel[1]),
						lat = as.numeric(v$markerSel[2]),
						layerId = v$markerSel[3],
						icon = iconSet$marker
					)
			}
			leafletProxy("leaf") %>%
				removeMarker(tmp$id) %>%
				removeMarker("selectedLabel") %>%
				removeMarker("selectedHighlight") %>%
				addAwesomeMarkers(
					lng = tmp$lng,
					lat = tmp$lat,
					layerId = "selectedHighlight",
					icon = iconSet$markerSel
				) %>%
				addLabelOnlyMarkers(
					lng = tmp$lng,
					lat = tmp$lat,
					layerId = "selectedLabel",
					label = "Selected",
					labelOptions = list(
						textsize = "12px",
						direction = "auto",
						noHide = T,
						offset = c(18,-40)
					),
					options = list(keyboard = F)
				)
			v$markerSel <- c(tmp$lng, tmp$lat, tmp$id)
		}
	})

# ----------------------------------------------------- ACTION BUTTONS ----

	# change the state of clearMarkers ab if at least a marker is drawn
	observeEvent(v$markerId, {
		shinyjs::toggleState(id = "action_clearMarkers",
							 condition = v$markerId > 1,
							 selector = NULL)
	})

	# select_satPlot observer
	observeEvent(input$select_satPlot, ignoreNULL = F, handlerExpr = {
		# change the state of time series download buttons
		shinyjs::toggleState(id = "action_downloadDataRaw",
							 condition = !is.null(input$select_satPlot),
							 selector = NULL)
		shinyjs::toggleState(id = "action_downloadPlotRaw",
							 condition = !is.null(input$select_satPlot),
							 selector = NULL)
	})

	# change of state of getTs ab if a marker is selected
	# also update select_satGet values
	observeEvent(length(v$markerSel), {
		shinyjs::toggleState(id = "action_getTs",
							 condition = length(v$markerSel) > 0,
							 selector = NULL)
		if (length(v$markerSel) > 0) {
			updateSelectInput(
				session = session,
				inputId = "select_satGet",
				choices = c("Select the satellite product(s)..." = "",
							satChoices)
			)
		}
	})

	# clears all markers currently drawn on the map
	observeEvent(input$action_clearMarkers, {
		leafletProxy("leaf") %>%
			clearMarkers()
		updateSelectInput(
			session,
			inputId = "select_satGet",
			choices = c("")
		)
		v$markerSel <- NULL
		v$markerId <- 1
	})

	# calling gee-px-ls.py to download satellite data
	event_getTs <- eventReactive(input$action_getTs, {
		withProgress(message = "Downloading satellite data...",
					 value = 0,
					 expr = {
			validate(need(
				input$select_satGet != "",
				"Please select a satellite from the list above."
			))

			# assign variable in Python with selected coordinates (lng,lat order)
			python.assign("coords", as.numeric(v$markerSel[1:2]))

			# call Python download script for each selected satellite
			serieList <- list()
			for(i in 1:length(input$select_satGet)) {
				# assign Python variables using Shiny inputs
				python.assign("satChoice", input$select_satGet[i])

				# execute Python download script
				python.load(paste0(getwd(), "/python/gee-px-ls.py"))

				# update progress
				incProgress(amount = 1 / length(input$select_satGet))

				# get Python output and show download message
				if (is.null(unlist(python.get("serie")))) {
					showNotification(
						ui = paste(names(satChoices[satChoices == input$select_satGet[i]]),
								   "- no data available."),
						type = "error",
						duration = 4,
						closeButton = F
					)
					serieList[[i]] <- NA
				} else {
					showNotification(
						ui = paste(names(satChoices[satChoices == input$select_satGet[i]]),
								   "- data downloaded."),
						type = "message",
						duration = 4,
						closeButton = F
					)
					serieList[[i]] <- unlist(python.get("serie"))
				}
			}

			# check for data availability
			validate(
				need(
					prod(is.na(serieList)) == 0,
					"No data available for the chosen satellite(s) and/or region... Please change your query and try again."
				)
			)

			# remove from serieList those satellites with no data
			# also keep track of the satellites names in satOrder
			satOrder <- input$select_satGet[!is.na(serieList)]
			serieList[is.na(serieList)] <- NULL

			# arrange each list element form serieList as a df
			serie <- lapply(serieList, function(x) {
				tmp <- data.frame(matrix(x,
										 ncol = python.get("numCol"),
										 byrow = T))

				# format data type and columns names
				tmp[, 1] <- as.Date(tmp[, 1], format = "%Y_%m_%d")
				tmp[, 2:ncol(tmp)] <- apply(tmp[, 2:ncol(tmp)],
											MARGIN = 2,
											as.numeric)
				colnames(tmp) <- python.get("colNames")

				# exclude saturated data
				filterWhich <- which(rowSums(tmp[, 2:ncol(tmp)] == 2) > 0)
				if (length(filterWhich) > 0) {
					tmp <- tmp[-filterWhich, ]
				}

				return(tmp)
			})

			# create new column for each list element with corresponding satellite
			# name, also include this info as an attribute of the list
			for (i in 1:length(serie)) {
				serie[[i]]$sat <- satOrder[i]
			}
			attributes(serie) <- list(satOrder = satOrder)

			# update select_satPlot with all possible satellite choices to be
			# visualized
			updateSelectInput(
				session,
				inputId = "select_satPlot",
				choices = satChoices[which(satChoices %in% satOrder)]
			)

			# update select_index
			updateSelectInput(
				session = session,
				inputId = "select_index",
				choices = python.get("colNames")[-1]
			)

			python.assign("aux", NULL)
			python.assign("serie", NULL)
			python.assign("values", NULL)
			python.assign("numCol", NULL)
			python.assign("colNames", NULL)

			return(serie)
		})
	})

	# download plot modal
	observeEvent(input$action_downloadPlotRaw, {
		showModal(modalDownloadPlot(type = "Raw"))
	})

	observeEvent(input$action_downloadPlotBfm, {
		showModal(modalDownloadPlot(type = "Bfm"))
	})

	observeEvent(input$action_downloadPlotBf01, {
		showModal(modalDownloadPlot(type = "Bf01"))
	})

	observeEvent(input$action_downloadPlotBfast, {
		showModal(modalDownloadPlot(type = "Bfast"))
	})

# ------------------------------------------------------------- MODALS ----

	modalDownloadPlot <- function(type = c("Raw", "Bfm", "Bf01", "Bfast")) {
		type = match.arg(type)

		modalDialog(
			title = "Please choose the output format",
			easyClose = T,

			div(align = "center",
			downloadButton(outputId = paste0("action_downloadPlot", type, "_jpg"),
						   label = "JPEG",
						   class = "btn-primary"),
			downloadButton(outputId = paste0("action_downloadPlot", type, "_png"),
						   label = "PNG",
						   class = "btn-primary"),
			downloadButton(outputId = paste0("action_downloadPlot", type, "_svg"),
						   label = "SVG",
						   class = "btn-primary")),

			footer = tagList(
				modalButton("Cancel")
			)
		)
	}

# ----------------------------------------------------------- ANALYSIS ----

	# call to event_getTs in Map tab
	# display a message if the download is successful
	output$text_getTs <- renderText({
		event_getTs()
		return(
			as.character(div(
					align = "center",
					strong("Data succesfully downloaded!"),
					HTML('Please head to the <i class="fa fa-bar-chart fa-lg"></i><b>Analysis</b> tab.')
			))
		)
	})

	# subset from serie using select_satPlot, and merge data in a single df
	serieSel <- eventReactive(input$select_satPlot, valueExpr = {
		serie <- event_getTs()

		# only show satellites selected in select_satPlot
		satOrder <- attr(serie, "satOrder")
		whichSel <- which(satOrder %in% input$select_satPlot)
		serie <- serie[whichSel]

		# join all data in a single df
		tmp <- NULL
		for (i in 1:length(whichSel)) {
			tmp <- rbind(tmp, serie[[i]])
		}
		serie <- tmp

		# sort data by date
		serie <- serie[order(serie$date), ]

		# remove leap year additional day (29th Feb), if it exists
		leapDay <- grep("-02-29", serie$date)
		if(length(leapDay) > 0) {
			serie <- serie[-leapDay, ]
		}

		# group by date and satellite using median
		serie <- serie %>%
			group_by(date) %>%
			summarise_all(function(x) {
				if (typeof(x) == "character") {
					if (x %>% unique() %>% length() > 1) {
						"Mixed"
					} else {
						x[1]
					}
				} else {
					median(x)
				}
			}) %>%
			data.frame()

		return(serie)
	})

	observe({
		# subset the chosen index from the data
		matchCol <- which(input$select_index == colnames(serieSel()))
		satOrder <- satChoices[which(satChoices %in% input$select_satPlot)]

		# add "Mixed" to satOrder, if it exists in the data
		if("Mixed" %in% serieSel()$sat) {
			satOrder <- c(satOrder, "Mixed" = "Mixed")
		}

		# define a vector of graphical parameters (color and pch),
		# per satellite
		seriePar <- matrix(sapply(serieSel()$sat, satPar),
						   ncol = 2,
						   byrow = T)

		# custom ylim parameter
		ylimCustom <- c(0, 1)
		if (sum(serieSel()[, matchCol] < 0) > 0) {
			ylimCustom[1] <- -1
		}
		if (sum(serieSel()[, matchCol] > 1) > 0) {
			ylimCustom[2] <- 1.5
		}

		# custom x axis
		xAxisCustom <- seq(as.numeric(substr(range(serieSel()$date), 1, 4))[1] - 1,
						   as.numeric(substr(range(serieSel()$date), 1, 4))[2] + 1,
						   1)

		# bfastmonitor line segment parameter
		h <- 0.25

		bfm_formula <- switch(
			input$select_bfm_formula,
			"trend + harmon" = response ~ trend + harmon,
			"harmon" = response ~ harmon,
			"trend" = response ~ trend
		)

		# conditions to check if few data is available for bfastmonitor
		cond1 <- switch(
			input$select_bfm_formula,
			"trend + harmon" = length(serieSel()[, matchCol]) > 4,
			"harmon" = length(serieSel()[, matchCol]) > 3,
			"trend" = length(serieSel()[, matchCol]) > 2
		)
		cond2 <- floor(h * length(serieSel()[, matchCol])) > 1

		# update input$select_bfm_monitor
		updateDateInput(
			session = session,
			inputId = "select_bfm_monitor",
			min = head(serieSel()$date, 1),
			max = tail(serieSel()$date, 1),
			value = tail(serieSel()$date,
						 floor(length(serieSel()$date)*0.3))[1]
		)

		# update input$select_bf01_order
		if(input$select_bf01_formula != "trend") {
			if(input$select_bf01_formula == "harmon") {
				pars <- 1
			} else { # "trend + harmon"
				pars <- 2
			}
			orderMaxBf01 <- 0
			cond3 <- T
			while(cond3) {
				orderMaxBf01 <- orderMaxBf01 + 1
				cond3 <- (5 * (pars + 2 * orderMaxBf01)) < length(serieSel()[, matchCol]) / 2
			}
			orderMaxBf01 <- orderMaxBf01 - 1
			updateSliderInput(
				session = session,
				inputId = "select_bf01_order",
				max = orderMaxBf01
			)
		}

		# update select_bfm_order (WIP)
		# if(input$select_bfm_formula != "trend") {
		# 	if(input$select_bfm_history == "ROC") {
		# 		orderMaxBfm <- 0
		# 		cond4 <- F
		# 		while(!cond4) {
		# 			orderMaxBfm <- orderMaxBfm + 1
		# 			data <- bfastts(data = serieSel()[, matchCol],
		# 							dates = serieSel()$date,
		# 							type = "irregular")
		# 			data_tspp <- bfastpp(data, order = orderMaxBfm, lag = NULL, slag = NULL)
		# 			history_tspp <- subset(data_tspp, time < decimal_date(input$select_bfm_monitor))
		# 			data_rev <- history_tspp[nrow(history_tspp):1,]
		# 			data_rev$response <- ts(data_rev$response)
		# 			suppressWarnings(
		# 				y_rcus <- efp(bfm_formula, data = data_rev, type = "Rec-CUSUM")
		# 			)
		# 			cond4 <- is.na(y_rcus$process[1])
		# 		}
		# 		orderMaxBfm <- orderMaxBfm - 1
		# 		updateSliderInput(
		# 			session = session,
		# 			inputId = "select_bfm_order",
		# 			max = orderMaxBfm
		# 		)
		# 	}
		# }

		# raw time series plot
		output$plot_raw <- renderPlot({
			output$action_downloadDataRaw <- downloadHandler(
				filename = paste0("be-data-ts", ".csv"),
				content = {function(file) {
					write.csv(x = serieSel(), file = file, row.names = F)
				}}
			)

			plotRawComb <- function() {
				plotRaw(
					serie = serieSel(),
					matchCol = matchCol,
					xAxisCustom = xAxisCustom,
					ylimCustom = ylimCustom,
					ylab = toupper(colnames(serieSel())[matchCol]),
					seriePar = seriePar
				)
				plotRawLegend(
					satOrder = satOrder,
					seriePar = seriePar
				)
			}

			output$action_downloadPlotRaw_jpg <- downloadHandler(
				filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-ts", ".jpg"),
				content = {function(file) {
					jpeg(file, width = 1080, height = 600)
					layout(mat = matrix(c(1, 2), ncol = 2),
						   widths = c(1.5, 0.5))
					plotRawComb()
					dev.off()
				}}
			)

			output$action_downloadPlotRaw_png <- downloadHandler(
				filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-ts", ".png"),
				content = {function(file) {
					png(file, width = 1080, height = 600)
					layout(mat = matrix(c(1, 2), ncol = 2),
						   widths = c(1.5, 0.5))
					plotRawComb()
					dev.off()
				}}
			)

			output$action_downloadPlotRaw_svg <- downloadHandler(
				filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-ts", ".svg"),
				content = {function(file) {
					svg(file, width = 16, height = 8)
					layout(mat = matrix(c(1, 2), ncol = 2),
						   widths = c(1.5, 0.5))
					plotRawComb()
					dev.off()
				}}
			)

			plotRaw(
				serie = serieSel(),
				matchCol = matchCol,
				xAxisCustom = xAxisCustom,
				ylimCustom = ylimCustom,
				ylab = toupper(colnames(serieSel())[matchCol]),
				seriePar = seriePar
			)
		})

		# bfastmonitor results plot
		output$plot_bfm <- renderPlot({
			# if(input$select_bfm_formula != "trend") {
			# 	validate(
			# 		need(
			# 			input$select_bfm_order <= orderMaxBfm,
			# 			FALSE
			# 		)
			# 	)
			# }
			validate(
				need(
					cond1 & cond2,
					"The selected history period hasn't enough observations."
				)
			)

			# run bfastmonitor
			res <- ppBfastmonitor(
				x = serieSel()[, matchCol],
				date = serieSel()$date,
				formula = bfm_formula,
				order = input$select_bfm_order,
				start = decimal_date(input$select_bfm_monitor),
				history = input$select_bfm_history,
				h = h
			)

			output$action_downloadDataBfm <- downloadHandler(
				filename = paste0("be-", colnames(serieSel())[matchCol], "-results-bfastmonitor", ".rds"),
				content = {function(file) {
					saveRDS(object = res,
							file = file)
				}}
			)

			plotBfmComb <- function() {
				plotBfm(
					serie = serieSel(),
					matchCol = matchCol,
					bfmOut = res,
					xAxisCustom = xAxisCustom,
					ylimCustom = ylimCustom,
					ylab = toupper(colnames(serieSel())[matchCol])
				)
				plotBfmLegend()
			}

			output$action_downloadPlotBfm_jpg <- downloadHandler(
				filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfastmonitor", ".jpg"),
				content = {function(file) {
					jpeg(file, width = 1080, height = 600)
					layout(mat = matrix(c(1, 2), ncol = 2),
						   widths = c(1.5, 0.5))
					plotBfmComb()
					dev.off()
				}}
			)

			output$action_downloadPlotBfm_png <- downloadHandler(
				filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfastmonitor", ".png"),
				content = {function(file) {
					png(file, width = 1080, height = 600)
					layout(mat = matrix(c(1, 2), ncol = 2),
						   widths = c(1.5, 0.5))
					plotBfmComb()
					dev.off()
				}}
			)

			output$action_downloadPlotBfm_svg <- downloadHandler(
				filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfastmonitor", ".svg"),
				content = {function(file) {
					svg(file, width = 16, height = 8)
					layout(mat = matrix(c(1, 2), ncol = 2),
						   widths = c(1.5, 0.5))
					plotBfmComb()
					dev.off()
				}}
			)

			# plot bfastmonitor results
			plotBfm(
				serie = serieSel(),
				matchCol = matchCol,
				bfmOut = res,
				xAxisCustom = xAxisCustom,
				ylimCustom = ylimCustom,
				ylab = toupper(colnames(serieSel())[matchCol])
			)
		})

		# bfast01 results plot
		output$plot_bf01 <- renderPlot({
			if(input$select_bf01_formula != "trend") {
				validate(
					need(
						input$select_bf01_order <= orderMaxBf01,
						FALSE
					)
				)
			}

			bf01_formula <- switch(
				input$select_bf01_formula,
				"trend + harmon" = response ~ trend + harmon,
				"harmon" = response ~ harmon,
				"trend" = response ~ trend
			)

			res <- ppBfast01(
				x = serieSel()[, matchCol],
				date = serieSel()$date,
				formula = bf01_formula,
				order = input$select_bf01_order
			)

			output$action_downloadDataBf01 <- downloadHandler(
				filename = paste0("be-", colnames(serieSel())[matchCol], "-results-bfast01", ".rds"),
				content = {function(file) {
					saveRDS(object = res,
							file = file)
				}}
			)

			plotBf01Comb <- function() {
				plotBf01(
					serie = serieSel(),
					matchCol = matchCol,
					bf01Out = res,
					xAxisCustom = xAxisCustom,
					ylimCustom = ylimCustom,
					ylab = toupper(colnames(serieSel())[matchCol])
				)
				plotBf01Legend()
			}

			output$action_downloadPlotBf01_jpg <- downloadHandler(
				filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfast01", ".jpg"),
				content = {function(file) {
					jpeg(file, width = 1080, height = 600)
					layout(mat = matrix(c(1, 2), ncol = 2),
						   widths = c(1.5, 0.5))
					plotBf01Comb()
					dev.off()
				}}
			)

			output$action_downloadPlotBf01_png <- downloadHandler(
				filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfast01", ".png"),
				content = {function(file) {
					png(file, width = 1080, height = 600)
					layout(mat = matrix(c(1, 2), ncol = 2),
						   widths = c(1.5, 0.5))
					plotBf01Comb()
					dev.off()
				}}
			)

			output$action_downloadPlotBf01_svg <- downloadHandler(
				filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfast01", ".svg"),
				content = {function(file) {
					svg(file, width = 16, height = 8)
					layout(mat = matrix(c(1, 2), ncol = 2),
						   widths = c(1.5, 0.5))
					plotBf01Comb()
					dev.off()
				}}
			)

			plotBf01(
				serie = serieSel(),
				matchCol = matchCol,
				bf01Out = res,
				xAxisCustom = xAxisCustom,
				ylimCustom = ylimCustom,
				ylab = toupper(colnames(serieSel())[matchCol])
			)
		})

		# bfast results plot
		output$plot_bfast <- renderPlot({
			res <- ppBfast(
				x = serieSel()[, matchCol],
				date = serieSel()$date,
				h = input$select_bfast_h,
				season = input$select_bfast_season
			)

			output$action_downloadDataBfast <- downloadHandler(
				filename = paste0("be-", colnames(serieSel())[matchCol], "-results-bfast", ".rds"),
				content = {function(file) {
					saveRDS(object = res,
							file = file)
				}}
			)

			plotBfastComb <- function() {
				plotBfast(
					serie = serieSel(),
					matchCol = matchCol,
					bfastOut = res,
					xAxisCustom = xAxisCustom,
					ylimCustom = ylimCustom,
					ylab = toupper(colnames(serieSel())[matchCol])
				)
				plotBfastLegend()
			}

			output$action_downloadPlotBfast_jpg <- downloadHandler(
				filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfast", ".jpg"),
				content = {function(file) {
					jpeg(file, width = 1080, height = 600)
					layout(mat = matrix(c(1, 2), ncol = 2),
						   widths = c(1.5, 0.5))
					plotBfastComb()
					dev.off()
				}}
			)

			output$action_downloadPlotBfast_png <- downloadHandler(
				filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfast", ".png"),
				content = {function(file) {
					png(file, width = 1080, height = 600)
					layout(mat = matrix(c(1, 2), ncol = 2),
						   widths = c(1.5, 0.5))
					plotBfastComb()
					dev.off()
				}}
			)

			output$action_downloadPlotBfast_svg <- downloadHandler(
				filename = paste0("be-", colnames(serieSel())[matchCol], "-plot-bfast", ".svg"),
				content = {function(file) {
					svg(file, width = 16, height = 8)
					layout(mat = matrix(c(1, 2), ncol = 2),
						   widths = c(1.5, 0.5))
					plotBfastComb()
					dev.off()
				}}
			)

			plotBfast(
				serie = serieSel(),
				matchCol = matchCol,
				bfastOut = res,
				xAxisCustom = xAxisCustom,
				ylimCustom = ylimCustom,
				ylab = toupper(colnames(serieSel())[matchCol])
			)
		})

		# raw time series legend
		output$plot_raw_legend <- renderPlot({
			plotRawLegend(
				satOrder = satOrder,
				seriePar = seriePar
			)
		})

		# bfastmonitor legend
		output$plot_bfm_legend <- renderPlot({
			plotBfmLegend()
		})

		# bfast01 legend
		output$plot_bf01_legend <- renderPlot({
			plotBf01Legend()
		})

		# bfast legend
		output$plot_bfast_legend <- renderPlot({
			plotBfastLegend()
		})
	})
})
