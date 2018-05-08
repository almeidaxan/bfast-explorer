# ------------------------------------------------------ TO-DO ADVANCES ----

## Dynamically set 'harmonic order' parameter for bfastmonitor

# currently, error occurs when:
# 	condition: ht = "ROC"; order > 9
# 	error msg: history_roc > Error in if: missing value where TRUE/FALSE needed
#
#	condition: ht = "BP"; order > 1
#	error msg: history_break -> Error in breakpoints.formula: minimum segment size must be smaller than half the number of observations
#
#	condition: ht = "all"; order > 18
#	error msg: monitor -> Error in if: missing value where TRUE/FALSE needed

library(strucchange)
load("test.RData")

x <- rbind(serie[[1]])
x <- x[order(x$date), ]

if(input$select_bfm_formula != "trend") {
	if(input$select_bfm_history == "ROC") {
		orderMaxBfm <- 0
		cond4 <- F
		while(!cond4) {
			orderMaxBfm <- orderMaxBfm + 1
			data <- bfastts(data = serieSel()[, matchCol],
							dates = serieSel()$date,
							type = "irregular")
			data_tspp <- bfastpp(data, order = orderMaxBfm, lag = NULL, slag = NULL)
			history_tspp <- subset(data_tspp, time < decimal_date(input$select_bfm_monitor))
			data_rev <- history_tspp[nrow(history_tspp):1,]
			data_rev$response <- ts(data_rev$response)
			suppressWarnings(
				y_rcus <- efp(bfm_formula, data = data_rev, type = "Rec-CUSUM")
			)
			cond4 <- is.na(y_rcus$process[1])
		}
		orderMaxBfm <- orderMaxBfm - 1
		updateSliderInput(
			session = session,
			inputId = "select_bfm_order",
			max = orderMaxBfm
		)
	}
}

# ----------------------------------------------------- DISCARDED CODE ----

# discarded code which my be useful in the future

## map draw toolbar observer
coordsDf <- reactive({
	# check if no markers are drawn on the map
	validate(
		need(!is.null(input$leaf_drawLayer_created$geometry$coordinates[[1]]),
			 "Place at least one marker on the map")
	)

	# create a data.frame with marker's lng-lat coordinates
	coords <- input$leaf_drawLayer_features$features %>%
		sapply(function(x) {
			c(x$geometry$coordinates[[1]],
			  x$geometry$coordinates[[2]])
		}) %>%
		t() %>%
		data.frame()

	# add names to dataframe columns
	if(ncol(coords) > 0) coords <- coords %>% setNames(c("lon", "lat"))

	# if at least one point is drawn and shown on map, show output list
	output$mapDraw <- renderPrint(coords)

	return(coords)
})

## coords data.frame observer to update input$select_coords
observe({
	coords <- coordsDf()

	if(ncol(coords) > 0) {
		# change the available choices of input$select_coords to 1:nrow(z)
		updateSelectInput(session,
						  inputId = "select_coords",
						  label = "Now, select a marker from the list below",
						  choices = 1:nrow(coords))
	} else {
		# if all points are deleted after drawn, reset input$select_coords
		updateSelectInput(session,
						  inputId = "select_coords",
						  label = "Place at least one marker on the map",
						  choices = "")
	}
})

## overlaying custom Tiles on the map
addTiles(
	map = m,
	urlTemplate = "https://earthengine.googleapis.com/map/47fdcba98ca0ee3bdd8c25059f8403f3/{z}/{x}/{y}?token=18335d5369c9a0ccebcd45458d1a1d79",
	options = list(
		minZoom = 3,
		maxZoom = 19,
		noWrap = T,
		unloadInvisibleTiles = T,
		updateWhenIdle = T,
		reuseTiles = T,
		opacity = 0.5
	)
)

## save a fixed time series to easily debug the Analysis tab

k <- c(-47.109375, -4.56547355071028)
python.assign("coords", k)

serieList <- list()
for (i in 1:3) {
	# assign Python variables using Shiny inputs
	python.assign("satChoice", c("LT5_SR", "LE7_SR", "LC8_SR")[i])

	# execute Python download script
	python.load(paste0(getwd(), "/python/gee-px-ls.py"))

	# get Python output and show download message
	if (is.null(unlist(python.get("serie")))) {
		serieList[[i]] <- NA
	} else {
		serieList[[i]] <- unlist(python.get("serie"))
	}
}

# remove from serieList those satellites with no data
# also keep track of the satellites names in satOrder
satOrder <- c("LT5_SR", "LE7_SR", "LC8_SR")
serieList[is.na(serieList)] <- NULL
satOrder[-is.na(serieList)]

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

save(file = "test.RData",
	 list = "serie",
	 envir = .GlobalEnv)

# load the previously saved data
load("test.RData")
