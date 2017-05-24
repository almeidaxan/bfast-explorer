# --------------------------------------------------------------- LIBS ----

# this tool is still not compatible with Windows OS
if(Sys.info()[["sysname"]] == "Windows") {
	stop("This tool is not yet compatible with Windows.\nPlease use a UNIX-like system.")
}

# load and install packages if needed
if(!require("pacman")) {
	install.packages("pacman")
	require("pacman")
}

# CRAN
packs <- c(
	"devtools",
	"dplyr",
	"ggmap",
	"knitr",
	"leaflet",
	"lubridate",
	"markdown",
	"RColorBrewer",
	"raster",
	"rPython",
	"shiny",
	"shinyBS",
	"shinyjs",
	"strucchange",
	"zoo"
)

# GitHub
packs_gh <- c(
	"verbe039/bfast"
)

p_load(char = packs)
p_load_gh(char = packs_gh)

# ---------------------------------------------------------- FUNCTIONS ----

# preprocessing
ppBfastmonitor <- function(x, date, ...) {
	tmpTs <- bfastts(data = x,
					 dates = date,
					 type = "irregular")
	tmp <- bfastmonitor(tmpTs, ...)
	return(tmp)
}

ppBfast01 <- function(x, date, ...) {
	tmpTs <- bfastts(data = x,
					 dates = date,
					 type = "irregular")
	tmp <- bfast01(tmpTs, ...)
	return(tmp)
}

ppBfast <- function(x, date, ...) {
	# bfast needs regularly spaced data. Here, we force the data regularity
	# with a monthly aggregation. Temporal gaps are linearly filled.
	timeYM <- strftime(date, "%Y-%m")
	x <- data.frame(time = timeYM, x)
	x <- aggregate(x = x$x,
				   by = list(x$time),
				   FUN = median)
	names(x)[1] <- "time"

	# create an empty continuous year-month df
	mYear <- as.numeric(substr(head(x$time, 1), 1, 4))
	MYear <- as.numeric(substr(tail(x$time, 1), 1, 4))
	mMonth <- as.numeric(substr(head(x$time, 1), 6, 7))
	MMonth <- as.numeric(substr(tail(x$time, 1), 6, 7))
	fullYear <- rep(mYear:MYear, each = 12)
	fullMonth <- rep(1:12, times = length(mYear:MYear))

	# correcting the first year
	if(mMonth > 1) {
		fullYear <- fullYear[-(1:(mMonth - 1))]
		fullMonth <- fullMonth[-(1:(mMonth - 1))]
	}

	# correcting the last year
	if (MMonth < 12) {
		fullYear <- fullYear[-((length(fullYear) - 12 + MMonth + 1):length(fullYear))]
		fullMonth <- fullMonth[-((length(fullMonth) - 12 + MMonth + 1):length(fullMonth))]
	}
	dfull <- data.frame(time = paste0(fullYear, "-", sprintf("%02d", fullMonth)))

	# merging x df with the full year-month df
	d <- merge(dfull, x, all = T)
	d$time <- NULL

	dts <- na.approx(
		ts(data = c(d)$x,
		   start = c(mYear, mMonth),
		   end = c(MYear, MMonth),
		   frequency = 12),
		rule = 2
	)

	tmp <- bfast(dts, max.iter = 1, ...)
	return(tmp)
}

# plotting
plotRaw <- function(serie, matchCol, xAxisCustom, ylimCustom, ylab, seriePar, coords) {
	# placeholder blank plot
	par(mar = c(4, 4, 0, 0) + 0.1)
	plot(y = serie[, matchCol],
		 x = serie$date,
		 axes = F,
		 xlab = "Time",
		 ylab = toupper(colnames(serie)[matchCol]),
		 ylim = ylimCustom,
		 col = "white")

	# redraw axis
	axis(side = 1,
		 at = date(paste0(xAxisCustom, "-01-01")),
		 labels = xAxisCustom)
	if(ylimCustom[1] == -1) {
		yAxisStep <- 0.2
	} else {
		yAxisStep <- 0.1
	}
	axis(side = 2,
		 at = seq(ylimCustom[1], ylimCustom[2], yAxisStep))

	# draw a grid on both axis directions
	for(i in xAxisCustom) {
		abline(v = date(paste0(i, "-01-01")),
			   col = rgb(.8, .8, .8),
			   lty = 2)
	}
	for(i in seq(ylimCustom[1], ylimCustom[2], yAxisStep)) {
		if(i == 0) {
			# highlight a guide line at "0"
			abline(h = i,
				   col = "black",
				   lty = 3,
				   lwd = 1.3)
		} else {
			abline(h = i,
				   col = rgb(.8, .8, .8),
				   lty = 2)
		}
	}
	box()

	# draw time series as points and lines
	lines(
		y = serie[, matchCol],
		x = serie$date,
		col = rgb(.1, .1, .1)
	)
	points(
		y = serie[, matchCol],
		x = serie$date,
		col = seriePar[, 1],
		pch = as.numeric(seriePar[, 2]),
		cex = 0.6
	)

	if(mean(serie[, matchCol]) > (ylimCustom[1] + 0.5 * (diff(ylimCustom)))) {
		posText <- 0.1
	} else {
		posText <- 0.9
	}

	if(coords$show) {
		text(
			x = serie$date %>% year() %>% unique() %>% range() %>% mean() %>% sum(0.5) %>% date_decimal() %>% format("%Y-%m-%d") %>% as.Date(),
			y = ylimCustom[1] + posText * (diff(ylimCustom)),
			labels = paste0("LatLong = (", sprintf("%.5f", round(coords$lat, 5)), ", ", sprintf("%.5f", round(coords$lon, 5)), ")"),
			cex = 1.3
		)
	}
}
plotRawLegend <- function(satOrder, seriePar) {
	# placeholder blank plot
	par(mar = c(4, 0, 0, 0) + 0.1)
	plot(0,
		 col = "white",
		 axes = F,
		 xlab = "",
		 ylab = "")

	# first, draw lines
	legend(x = "center",
		   lty = 1,
		   x.intersp = 2,
		   y.intersp = 1.5,
		   legend = names(satOrder),
		   text.col = "white",
		   bty = "n",
		   col = rgb(.1, .1, .1))

	serieParOrder <- unique(seriePar)
	serieParOrder <- rbind(serieParOrder[order(serieParOrder[, 2]), ])

	# then, draw points
	legend(x = "center",
		   x.intersp = 2,
		   y.intersp = 1.5,
		   legend = names(satOrder),
		   bty = "n",
		   col = serieParOrder[, 1],
		   pch = as.numeric(serieParOrder[, 2])
	)
}

plotBfm <- function(serie, matchCol, bfmOut, xAxisCustom, ylimCustom, ylab) {
	# define a vector with rounded dates to avoid strange rounding errors
	# to interfere with the conditions below
	dates <- round(decimal_date(serie$date), digits = 3)

	# conditions
	condHist <- dates >= round(bfmOut$history[1], digits = 3) & dates <= round(bfmOut$history[2], digits = 3)
	condMoni <- dates >= round(bfmOut$monitor[1], digits = 3)
	condStart <- dates <= round(bfmOut$history[2], digits = 3)
	condPred <- dates >= round(bfmOut$history[1], digits = 3)

	# background blank plot
	par(mar = c(4, 4, 0, 0) + 0.1)
	plot(y = serie[, matchCol],
		 x = serie$date,
		 col = "white",
		 axes = F,
		 ylim = ylimCustom,
		 ylab = ylab,
		 xlab = "Time")

	# redraw axis
	axis(side = 1,
		 at = date(paste0(xAxisCustom, "-01-01")),
		 labels = xAxisCustom)
	if(ylimCustom[1] == -1) {
		yAxisStep <- 0.2
	} else {
		yAxisStep <- 0.1
	}
	axis(side = 2,
		 at = seq(ylimCustom[1], ylimCustom[2], yAxisStep))
	for(i in xAxisCustom) {
		abline(v = date(paste0(i, "-01-01")),
			   col = rgb(.8, .8, .8),
			   lty = 2)
	}
	for(i in seq(ylimCustom[1], ylimCustom[2], yAxisStep)) {
		if(i == 0) {
			# highlight a guide line at "0"
			abline(h = i,
				   col = "black",
				   lty = 3,
				   lwd = 1.3)
		} else {
			abline(h = i,
				   col = rgb(.8, .8, .8),
				   lty = 2)
		}
	}
	box()

	# draw a guide line at "0"
	abline(h = 0,
		   lty = 3,
		   lwd = 1.3,
		   col = "black")

	# draw historical data
	lines(y = serie[, matchCol][which(condStart)],
		  x = serie$date[which(condStart)])

	# draw data of monitoring period
	lines(y = serie[, matchCol][which(condMoni)],
		  x = serie$date[which(condMoni)],
		  col = "red",
		  type = "o",
		  pch = 20)

	# draw stable history points
	points(y = serie[, matchCol][which(condHist)],
		   x = serie$date[which(condHist)],
		   pch = 20,
		   col = "darkgreen")

	# draw fit based on stable history
	lines(y = bfmOut$tspp$prediction,
		  x = serie$date[which(condPred)],
		  col = "blue",
		  lwd = 1.5)

	# draw line of start of the monitoring period
	abline(v = as.Date(date_decimal(bfmOut$monitor[1]), format = "YYYY-MM-DD"),
		   lty = 2,
		   lwd = 1.2)

	# draw line of time of detected break (if it exists)
	if(is.na(!bfmOut$mefp$breakpoint)) {
		abline(v = serie$date[c(which(condHist), which(condMoni))[bfmOut$mefp$breakpoint]-1],
			   lty = 2,
			   lwd = 2,
			   col = "red")
	}
}
plotBfmLegend <- function() {
	# placeholder blank plot
	par(mar = c(4, 0, 0, 0) + 0.1)
	plot(0,
		 col = "white",
		 axes = F,
		 xlab = "",
		 ylab = "")

	legendText <- c(
		"Historical data",
		"Stable history",
		"New data",
		"Fit using stable history",
		"Start of monitoring",
		"Detected break"
	)

	# first, draw the lines
	legend(x = "center",
		   x.intersp = 2,
		   y.intersp = 1.5,
		   legend = legendText,
		   text.col = "white",
		   bty = "n",
		   lty = c(rep(1, 4), rep(2, 2)),
		   lwd = c(rep(1, 2), 1.2, 1.5, .9, 2),
		   col = c(rep("black", 2), "red", "blue", "black", "red"))

	# then, draw the points
	legend(x = "center",
		   x.intersp = 2,
		   y.intersp = 1.5,
		   legend = legendText,
		   bty = "n",
		   col = c(rgb(0,0,0,0), "darkgreen", "red", rep(rgb(0,0,0,0), 3)),
		   pch = 20)
}

plotBf01 <- function(serie, matchCol, bf01Out, xAxisCustom, ylimCustom, ylab) {
	# background blank plot
	par(mar = c(4, 4, 0, 0) + 0.1)
	plot(y = serie[, matchCol],
		 x = serie$date,
		 col = "white",
		 axes = F,
		 ylim = ylimCustom,
		 ylab = ylab,
		 xlab = "Time")

	# redraw axis
	axis(side = 1,
		 at = date(paste0(xAxisCustom, "-01-01")),
		 labels = xAxisCustom)
	if(ylimCustom[1] == -1) {
		yAxisStep <- 0.2
	} else {
		yAxisStep <- 0.1
	}
	axis(side = 2,
		 at = seq(ylimCustom[1], ylimCustom[2], yAxisStep))
	for(i in xAxisCustom) {
		abline(v = date(paste0(i, "-01-01")),
			   col = rgb(.8, .8, .8),
			   lty = 2)
	}
	for(i in seq(ylimCustom[1], ylimCustom[2], yAxisStep)) {
		if(i == 0) {
			# highlight a guide line at "0"
			abline(h = i,
				   col = "black",
				   lty = 3,
				   lwd = 1.3)
		} else {
			abline(h = i,
				   col = rgb(.8, .8, .8),
				   lty = 2)
		}
	}
	box()

	# draw a guide line at "0"
	abline(h = 0,
		   lty = 3,
		   lwd = 1.3,
		   col = "black")

	lines(
		y = as.zoo(bf01Out)$response,
		x = serie$date,
		col = "black",
		lwd = 1
	)
	lines(
		y = as.zoo(bf01Out)$fitted,
		x = serie$date,
		col = "blue",
		lwd = 2
	)

	if(bf01Out$breaks == 1) {
		# bp
		abline(
			v = serie$date[bf01Out$confint[2]],
			lty = 2,
			lwd = 2,
			col = "red"
		)
	}
}
plotBf01Legend <- function() {
	# placeholder blank plot
	par(mar = c(4, 0, 0, 0) + 0.1)
	plot(0,
		 col = "white",
		 axes = F,
		 xlab = "",
		 ylab = "")

	legendText <- c(
		"Data",
		"Fitted model",
		"Detected break"
	)

	# draw the lines
	legend(x = "center",
		   x.intersp = 2,
		   y.intersp = 1.5,
		   legend = legendText,
		   bty = "n",
		   lty = c(1, 1, 2),
		   lwd = c(1, 1, 2),
		   col = c("black", "blue", "red"))
}

plotBfast <- function(serie, matchCol, bfastOut, xAxisCustom, ylimCustom, ylab) {
	# background blank plot
	par(mar = c(4, 4, 0, 0) + 0.1)
	plot(y = serie[, matchCol],
		 x = serie$date,
		 col = "white",
		 axes = F,
		 ylim = ylimCustom,
		 ylab = ylab,
		 xlab = "Time")

	# redraw axis
	axis(side = 1,
		 at = date(paste0(xAxisCustom, "-01-01")),
		 labels = xAxisCustom)
	if(ylimCustom[1] == -1) {
		yAxisStep <- 0.2
	} else {
		yAxisStep <- 0.1
	}
	axis(side = 2,
		 at = seq(ylimCustom[1], ylimCustom[2], yAxisStep))
	for(i in xAxisCustom) {
		abline(v = date(paste0(i, "-01-01")),
			   col = rgb(.8, .8, .8),
			   lty = 2)
	}
	for(i in seq(ylimCustom[1], ylimCustom[2], yAxisStep)) {
		if(i == 0) {
			# highlight a guide line at "0"
			abline(h = i,
				   col = "black",
				   lty = 3,
				   lwd = 1.3)
		} else {
			abline(h = i,
				   col = rgb(.8, .8, .8),
				   lty = 2)
		}
	}
	box()

	# draw a guide line at "0"
	abline(h = 0,
		   lty = 3,
		   lwd = 1.3,
		   col = "black")

	lines(y = bfastOut$Yt, x = as.Date(time(bfastOut$Yt)), col = "gray", lwd = 2)
	lines(y = bfastOut$output[[1]]$Tt + bfastOut$output[[1]]$St, x = as.Date(time(bfastOut$Yt)), col = "blue", lwd = 2)

	if(!bfastOut$nobp$Vt & !is.na(bfastOut$output[[1]]$Vt.bp)[1]) {
		for(i in 1:length(bfastOut$output[[1]]$Vt.bp)) {
			abline(
				v = as.Date(time(bfastOut$Yt))[bfastOut$output[[1]]$Vt.bp[i]],
				lty = 2,
				lwd = 1.5,
				col = "red"
			)
		}
	}
	if(!bfastOut$nobp$Wt & !is.na(bfastOut$output[[1]]$Wt.bp)[1]) {
		for(i in 1:length(bfastOut$output[[1]]$Wt.bp)) {
			abline(
				v = as.Date(time(bfastOut$Yt))[bfastOut$output[[1]]$Wt.bp[i]],
				lty = 2,
				lwd = 1.5,
				col = "darkgreen"
			)
		}
	}
}
plotBfastLegend <- function() {
	# placeholder blank plot
	par(mar = c(4, 0, 0, 0) + 0.1)
	plot(0,
		 col = "white",
		 axes = F,
		 xlab = "",
		 ylab = "")

	legendText <- c(
		"Data",
		"Fitted model",
		"Detected trend break",
		"Detected season break"
	)

	# draw the lines
	legend(x = "center",
		   x.intersp = 2,
		   y.intersp = 1.5,
		   legend = legendText,
		   bty = "n",
		   lty = c(1, 1, 2, 2),
		   lwd = c(1, 2, 2, 2),
		   col = c("black", "blue", "red", "darkgreen"))
}

# --------------------------------------------------------------- DEFS ----

# define custom AwesomeIcons markers
iconSet <- awesomeIconList(
	# unselected marker
	marker = makeAwesomeIcon(
		icon = "circle",
		markerColor = "lightgray",
		iconColor = "white",
		library = "fa"
	),
	# selected marker
	markerSel = makeAwesomeIcon(
		icon = "circle",
		markerColor = "blue",
		iconColor = "white",
		library = "fa"
	)
)

# define all available satellites the user can choose from:
# LHS is the alias name; RHS is the name that GEE understands
satChoices <- c(
	"Landsat 5 SR" = "LT5_SR",
	"Landsat 7 SR" = "LE7_SR",
	"Landsat 8 SR" = "LC8_SR"
)

# coordinates (long, lat, zoom) of the default fixed center
viewCenter <- c(-5.98, 24.69, 3)

# defining colors & pch for points from differente satellite sources
satPar <- function(x) {
	customPalette <- brewer.pal(length(satChoices) + 1, "Dark2")
	switch(
		x,
		"LT5_SR" = c(customPalette[1], 15),
		"LE7_SR" = c(customPalette[2], 16),
		"LC8_SR" = c(customPalette[3], 17),
		"Mixed" = c(customPalette[4], 18)
	)
}

# create a .md file from the following .Rmd, in order to be embedded into
# the Shiny app
knit(input = "./md/tutorial.Rmd", output = "md/tutorial.md", quiet = T)

# longlat projection default CRS
proj_ll <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
