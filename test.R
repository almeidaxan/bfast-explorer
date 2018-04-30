# BFAST01

z <- rbind(serie[[1]], serie[[2]], serie[[3]]) # serie[, matchCol]
z <- z[order(z$date), ]
date <- z$date # serie$date
z <- z$ndvi

res <- ppBfast(x = z, date = date, h = 0.15)

plot(
	res,
	regular = F
)

# -----

### nao incluir pois ja existe
ylimCustom <- c(0,1)
xAxisCustom <- seq(as.numeric(substr(range(date), 1, 4))[1] - 1,
				   as.numeric(substr(range(date), 1, 4))[2] + 1,
				   1)
yAxisStep <- 0.1

# -----

par(mar = c(4, 4, 0, 0) + 0.1)
plot(y = z,
	 x = date,
	 col = "white",
	 axes = F,
	 ylim = ylimCustom, # ylimCustom
	 ylab = "NDVI", # ylab
	 xlab = "Time")

# redraw axis
axis(side = 1,
	 at = date(paste0(xAxisCustom, "-01-01")),
	 labels = xAxisCustom)
axis(side = 2,
	 at = seq(0, 1, 0.1))
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

lines(y = res$Yt, x = as.Date(time(res$Yt)), col = "gray", lwd = 2)
lines(y = res$output[[1]]$Tt + res$output[[1]]$St, x = as.Date(time(res$Yt)), col = "blue", lwd = 2)

if(!res$nobp$Vt & !is.na(res$output[[1]]$Vt.bp)[1]) {
	for(i in 1:length(res$output[[1]]$Vt.bp)) {
		abline(
			v = as.Date(time(res$Yt))[res$output[[1]]$Vt.bp[i]],
			lty = 2,
			lwd = 1.5,
			col = "red"
		)
	}
}
if(!res$nobp$Wt & !is.na(res$output[[1]]$Wt.bp)[1]) {
	for(i in 1:length(res$output[[1]]$Wt.bp)) {
		abline(
			v = as.Date(time(res$Yt))[res$output[[1]]$Wt.bp[i]],
			lty = 2,
			lwd = 1.5,
			col = "darkgreen"
		)
	}
}
