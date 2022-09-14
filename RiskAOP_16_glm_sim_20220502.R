# YSO-BN
# Simulate new data with fitted GLM 

# 01.10.2018: Changing SD to SE, because SD gives too wide intervals (negative values) (From script 11 GAM)

# Number of simulations for new data
n.sim <- 50000
# Select 100 simulations for plotting
#v.sim.sel <- runif(500, min=1, max=n.sim)
v.sim.sel <- runif(200, min=1, max=n.sim)

# List for storing fitted values
list.sim.glm <- list()
length(list.sim.glm) <- length(v.KER.no)
names(list.sim.glm) <- v.KER.label
for (i in 1:length(v.KER.no))
{
	list.sim.glm[[i]] <- list()
	length(list.sim.glm[[i]]) <- 2
	names(list.sim.glm[[i]]) <- c("x.sim", "y.sim")
}

# Specify which model to select for each KER (2 = linear, 5 = logistic)
v.KER.mod <-c(2, 2, 2, 2, 5, 2, 2, 2, 5, 2, 2, 2, 2, 2, 2, 5)

windows(14,10)

par(mfrow=c(4,5), mar=c(2,3,3,1), oma=c(2,3,4,1))

for (i in 1:length(v.KER.no)) 
{
	x.label <- v.arrow.x[i]
	y.label <- v.arrow.y[i]
	x <- unlist(t.DATA.perm[t.DATA.perm$KER.no==i, "x"])
	y <- unlist(t.DATA.perm[t.DATA.perm$KER.no==i, "y"])
	x.min <- min(x)*0.75
	x.max <- max(x)*1.25
	y.min <- min(y)*0.75
	y.max <- max(y)*1.25
	fit <- list.glm[[i]][[v.KER.mod[i]]]
	x.sim <- seq(from=x.min, to=x.max, length=n.sim)
	y.pred <- predict(fit, newdata=data.frame(x=x.sim), se.fit=TRUE, type="response")
	y.pred[is.na(y.pred)] <- 0 # SE = NaN can occure for x.sim = 0
	y.sim <- rep(NA, n.sim)
	for (j in 1:length(y.sim))
	{
		#	y.sim[i] <- rnorm(n=1, mean=y.pred[i,"Prediction"], sd=y.pred[i,"SE"]*sqrt(length(x)))
#		y.sim[j] <- rnorm(n=1, mean=y.pred[j,"Prediction"], sd=y.pred[j,"SE"])
		y.sim[j] <- rnorm(n=1, mean=y.pred$fit[[j]], sd=y.pred$se.fit[[j]])
	}
# Try plotting obs then add fit + SE, to avoid SE down to 0
#	plot(x, y, col="red", pch=16, cex=1.2, 
#			 xlim=c(0, x.max), ylim=c(0, y.max), xlab="", ylab="", axes=FALSE)
#			 xlim=c(x.min, x.max), ylim=c(y.min, y.max), xlab="", ylab="", axes=FALSE)
#plot(fit, type="confidence", log="", add=TRUE)
#plot(fit,  xlim=c(0, x.max), ylim=c(0, y.max), xlab="", ylab="", las=1, axes=FALSE)
#	plot(x, fitted(fit), las=1, ylab="", xlim=c(x.min, x.max), ylim=c(y.min, y.max), 
#	     col="blue", lwd=1)
	plot(x.sim, y.pred$fit, las=1, ylab="", xlim=c(x.min, x.max), ylim=c(y.min, y.max), 
	     col="blue", lwd=1, type="l")
	lines(x.sim, y.pred$fit - 2*y.pred$se.fit, col="blue", lwd=1, lty=3)
	lines(x.sim, y.pred$fit + 2*y.pred$se.fit, col="blue", lwd=1, lty=3)
	points(x, y, col="red", pch=16, cex=1) 
	#plot(fit, type="confidence", log="", add=TRUE)
	box()
	axis(1)
	axis(2, las=1)
	mtext(x.label, side=1, line=2.0, cex=.7)
	mtext(y.label, side=2, line=2.5, cex=.7)
	mtext(paste("KER no.", v.KER.no[i]), side=3, line=.4, cex=.8)
	mtext(paste("(",letters[i], ")", sep=""), side=3, adj=-.3, cex=.8, line=.5)
	# Deviance explained
	dev.expl.gml <- (fit$null.deviance - deviance(fit))/fit$null.deviance
	#text(x.max, y.max*.95, paste(round(100*summary(fit)$dev.expl,0), "%"), adj=1)
	text(x.max, y.max*.95, paste(round(100*dev.expl.gml,0), "%"), adj=1)
	points(x.sim[v.sim.sel], y.sim[v.sim.sel])
	#lines(x[order(x)], fitted(fit)[order(x)], col="blue", lwd=2) ## Denne får ikke med alle x-verdier; blir noen hopp.
	# Og kan ikke ta add=TRUE på plot.glm. Får heller ha den i bakgrunnen.
	points(x, y, col="red", pch=16, cex=1.0)
#	lines(x.sim[-1], y.pred[-1, "Prediction"], lwd=1, col="red")
	abline(v=list.breaks.KER.x[[i]], col="grey") 
	abline(h=list.breaks.KER.y[[i]], col="grey")
	if(i %in% c(5, 9)) 
	{ 
		frame()
	}
	if(i %in% c(12)) 
	{ 
		frame()
		frame()
	}
	list.sim.glm[[i]][[1]] <- x.sim
	list.sim.glm[[i]][[2]] <- y.sim
}
title("AOP for UV - Daphnia: glm plots with simulations", outer=TRUE)

figname <- paste("AOP_Daphnia_UV_glm_sim_", Sys.Date(), ".png", sep="")
savePlot(figname, "png")
