# AOP-BN
# Explore relationships with GAM

library(data.table) # for making table from list of tables
library(mgcv)

# list for storing gam fits
list.gam <- list()
length(list.gam) <- length(v.KER.no)
names(list.gam) <- v.KER.label
# List for individual components of the fit (for easier unlist)
list.gam.p   <- list.gam
list.gam.dev <- list.gam
list.gam.df  <- list.gam

windows(14,10)

par(mfrow=c(4,4), mar=c(2,3,3,1), oma=c(2,3,4,1))

for (i in 1:length(v.arrow.x)) 
{
  x.label <- v.arrow.x[i]
  y.label <- v.arrow.y[i]
  x <- unlist(t.DATA.perm[t.DATA.perm$KER.no==i, "x"])
	y <- unlist(t.DATA.perm[t.DATA.perm$KER.no==i, "y"])
	x.min <- min(x)*0.75
	x.max <- max(x)*1.25
	y.min <- min(y)*0.75
	y.max <- max(y)*1.25
	fit <- gam(y ~ s(x, k=4))
  #summary(fit)
#	plot(x, y, col="red", pch=16, cex=1.2, 
#			 xlim=c(0, x.max), ylim=c(0, y.max), xlab="", ylab="", axes=FALSE)
	# plot(fit, shade=TRUE, las=1, shift=mean(y), add=TRUE) ## Fungerer ikke!
#	 lines(fit, shade=TRUE, las=1, shift=mean(y)) ## Fungerer ikke!
	plot.gam(fit, shade=TRUE, las=1, shift=mean(y), ylab="",
 				 xlim=c(x.min, x.max), ylim=c(y.min, y.max))
  v.splits <- list.RegTree$fit[[i]]$splits[,"index"]
  v.splits <- v.splits[1:min(5, length(v.splits))] # Select max. 5 first splits
  abline(v=v.splits, col="blue", lwd=(length(v.splits):1))
  # Add splits also to y-axis
  #	if(v.arrow.x[i] %in% v.arrow.y) ## Can replace these with splist from breaks scrip
  if(v.arrow.y[i] %in% v.arrow.x)
  {
  	i. <- which(v.arrow.x == v.arrow.y[i])[1] # Select first KER with the current y variable as x variable
  	v.splits.y <- list.RegTree$fit[[i.]]$splits[,"index"]
  	v.splits.y <- v.splits.y[1:min(5, length(v.splits.y))] # Select max. 5 first splits
  	abline(h=v.splits.y, col="green", lwd=(length(v.splits.y):1))
  }
	# Plot observations and gam curve again on top of split lines
  x.new <- seq(x.min, x.max, length=100)
  y.new <- predict.gam(fit, newdata=list(x=x.new))
  lines(x.new, y.new)
  points(x, y, pch=16)
  mtext(x.label, side=1, line=2.0, cex=.7)
  mtext(y.label, side=2, line=2.5, cex=.7)
  mtext(paste("KER no.", v.KER.no[i]), side=3, line=.4, cex=.8)
  mtext(paste("(",letters[i], ")", sep=""), side=3, adj=-.3, cex=.8, line=.5)
	points(x, y, pch=16, col="red") # Add points again on top of lines
	list.gam[[i]] <- fit
	list.gam.p[[i]]   <- summary(fit)$s.pv
	list.gam.dev[[i]] <- summary(fit)$dev.expl
	list.gam.df[[i]]  <- summary(fit)$edf
	text(x.max, y.max*.95, paste(round(100*summary(fit)$dev.expl,0), "%"), adj=1)
#	if(i==4) frame()
}
title("AOP for UV - Daphnia: GAM plots", outer=TRUE)

figname <- paste("AOP_Daphnia_UV_gam_", Sys.Date(), ".png", sep="")
savePlot(figname, "png")

############################################################################################


#### GAM output: deviance explained ####

# t.GAM <- rbindlist(list.gam.p) # Input må være list of data.table
t.GAM <- data.frame(matrix(NA, ncol=5, nrow=length(v.KER.no)))
names(t.GAM) <- c("KER.no", "KER.label", "s.pv", "dev.expl", "edf")
t.GAM$KER.no <- v.KER.no
t.GAM$KER.label <- v.KER.label
t.GAM$s.pv <- unlist(list.gam.p)
t.GAM$dev.expl <- unlist(list.gam.dev)
t.GAM$edf <- unlist(list.gam.df)

write.table(t.GAM, file=paste("t.GAM", Sys.Date(), "xls", sep="."), sep="\t", row.names=FALSE)
