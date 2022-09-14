# AOP-BN
# Make CPTs based on glm fit

# Make list with selected simulations 
list.sim.glm.sel <- list.sim.glm

for (i in 1:length(v.KER.no))
#for (i in 1:length(v.KER.no.sel))
{
  x.sim <- x.sim. <- list.sim.glm.sel[[i]][[1]]
  y.sim <- y.sim. <- list.sim.glm.sel[[i]][[2]]
  #x.sim <- x.sim. <- list.sim.glm[[v.KER.no.sel[i]]][[1]]
	#y.sim <- y.sim. <- list.sim.glm[[v.KER.no.sel[i]]][[2]]
	breaks.x <- list.breaks.KER.x[[i]]
	breaks.y <- list.breaks.KER.y[[i]]
	#breaks.x <- list.breaks.KER.x[[v.KER.no.sel[i]]]
	#breaks.y <- list.breaks.KER.y[[v.KER.no.sel[i]]]
	# Include values below 0 and above max
	x.sim[x.sim. < min(breaks.x)] <- min(breaks.x)
	y.sim[y.sim. < min(breaks.y)] <- min(breaks.y)
	x.sim[x.sim. > max(breaks.x)] <- max(breaks.x)
	y.sim[y.sim. > max(breaks.y)] <- max(breaks.y)
	glm.cut <- data.frame(
		X = cut(x.sim, breaks=breaks.x, right=FALSE, include.lowest=TRUE),
		Y = cut(y.sim, breaks=breaks.y, right=FALSE, include.lowest=TRUE))
	table.glm <- table(glm.cut, dnn=list(v.arrow.x[i], v.arrow.y[i]))
	#table.glm <- table(glm.cut, dnn=list(v.arrow.x[v.KER.no.sel[i]], v.arrow.y[v.KER.no.sel[i]]))
	CPT.glm <- ftable(table.glm, row.vars=c(1)) 
	 write.ftable(CPT.glm, file=paste("CPT.frm.KER-", i, ".", v.arrow.x[i], "--", v.arrow.y[i], ".glm.txt", sep="")) # for looking at
	 write.table(table.glm,  file=paste("CPT.txt.KER-", i, ".", v.arrow.x[i], "--", v.arrow.y[i], ".glm.txt", sep=""), 
	             row.names=dimnames(table.glm)[[1]], col.names=dimnames(table.glm)[[2]], sep="\t") # for copying to CPT
	 write.table(table.glm,  file=paste("CPT.xls.KER-", i, ".", v.arrow.x[i], "--", v.arrow.y[i], ".glm.xls", sep=""), 
	             row.names=dimnames(table.glm)[[1]], col.names=dimnames(table.glm)[[2]], sep="\t") # for copying to CPT
	#write.ftable(CPT.drc, file=paste("CPT.frm.KER-", v.KER.no.sel[i], ".", v.arrow.x[v.KER.no.sel[i]], "--", v.arrow.y[v.KER.no.sel[i]], ".drc.txt", sep="")) # for looking at
	#write.table(table.drc,  file=paste("CPT.txt.KER-", v.KER.no.sel[i], ".", v.arrow.x[v.KER.no.sel[i]], "--", v.arrow.y[v.KER.no.sel[i]], ".drc.txt", sep=""), 
	#            row.names=dimnames(table.drc)[[1]], col.names=dimnames(table.drc)[[2]], sep="\t") # for copying to CPT
	#write.table(table.drc,  file=paste("CPT.xls.KER-", v.KER.no.sel[i], ".", v.arrow.x[v.KER.no.sel[i]], "--", v.arrow.y[v.KER.no.sel[i]], ".drc.xls", sep=""), 
	#            row.names=dimnames(table.drc)[[1]], col.names=dimnames(table.drc)[[2]], sep="\t") # for copying to CPT
	
}
	
