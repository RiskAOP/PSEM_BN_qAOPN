# YSO-BN
# BREAKS FOR INTERVALS	

# 21.04.2021 SHORT CUT SOLUTION: FOR EACH NODE USE RANGE - EVENLY DISCRETISED IN 5 INTERVALS 

# Breaks suggested by regression ntrees: list.RegTree$splits[[i]] (x only); v.splits.x, v.splits.y
# Finally decided breaks: list.breaks.x, list.breaks.y 

## IKKE LENGER BASERT PÅ KERS, MEN PÅ KES
#list.breaks.x <- list.RegTree$splits
#list.breaks.y <- list()
#length(list.breaks.y) <- length(list.breaks.x)

# Make list of splits for each KE
# Select maximum 3 breaks from each (can be more)
list.splits.KE <- list()
length(list.splits.KE) <- length(v.EventID)
names(list.splits.KE) <- v.EventID
#for (j in length(v.EventID))
#for (j in 1:length(v.EventID))
#{
#	list.splits.KE[[j]] <- NULL # For appending later values from KER later
#}
# Noe rart skjer her, bare halvparten av lista blir igjen!!

# Find KE as x-variable for each KER
#for (i in 1:length(v.KER.no)) # Loop through KERs, since one KE may appear in more than one KER
# for (i in 1:11)	
# for (i in 1:16)	
#{
#	KE.index <- which(v.EventID==v.arrow.x[i]) # Find correct index for KE
#	v.splits.tmp <- list.RegTree$splits[[i]]
	# Append splits to exising splits from other KERs (or to NULL), maximum 2 (or 3) first splits from each KER
#	list.splits.KE[[KE.index]] <- sort(unique(
#		c(list.splits.KE[[KE.index]], v.splits.tmp[1:min(2, length(v.splits.tmp))]))) 
#	c(list.splits.KE[[KE.index]], v.splits.tmp[1:min(3, length(v.splits.tmp))]))) 
#	list.splits.KE[[KE.index]] <- c(list.splits.KE[[KE.index]], v.splits.tmp[1:min(3, length(v.splits.tmp))]) 
#}


# 20.04.2021:
# For each y-variable, check range and split in to 5 intervals
# (This will also handle all x values except UV, which is already discretised)
for (i in 2:length(list.splits.KE))
{
	# Select vector of y-values
	v.y <- t.DATA.perm[t.DATA.perm$KE.y==v.EventID[i], "y"]
	v.seq <- seq(min(v.y), max(v.y), length.out=4) 
	# If min(v.y) >0, add one interval down to 0. 
	# For upper interval, add 30% of max value (can be adjusted later)
	#v.breaks <- ifelse(min(v.y) > 0, c(0, v.seq, max(v.y)*1.3), c(v.seq, max(v.y)*1.3)) 
	if (min(v.y) > 0) v.breaks <- c(0, v.seq, max(v.y)*1.3) else v.breaks <- c(v.seq, max(v.y)*1.3)
	list.splits.KE[[i]] <- v.breaks
}

# Manually adjust some breaks
list.splits.KE.sel <- list.splits.KE
# UV
list.splits.KE.sel[[ 1]] <- v.Dose  ##  LAGER INTERVALLER SENERE
# MIE 
list.splits.KE.sel[[ 2]] <- c(0, 2.5, 5, 10, 20, 40, 80)
# KE1: High outliers
list.splits.KE.sel[[ 3]] <- c(0, 10, 20, 40, 80, 120)
# KE3: as % of control
list.splits.KE.sel[[ 5]] <- seq(0, 100, by=20) 
# KE4: High outliers
list.splits.KE.sel[[ 6]] <- c(0, 1, 1.25, 1.5, 2, 2.5)
# KE9: None in lowest interval
list.splits.KE.sel[[11]] <- c(0, 5e-06, 1e-05, 1.5e-05, 2e-05, 3e-05)
# KE10: None in lowest interval
list.splits.KE.sel[[12]] <- c(0, 0.8, 1.0, 1.1, 1.2, 1.4, 1.6)
# AO: Longevity as % of control
list.splits.KE.sel[[14]] <- seq(0, 100, by=20) 

######### IKKE OPPDATERT NEDENFOR ######

# > list.splits.KE
# $`UV`
# [1] 0.0254 0.1500 0.3000
# $MIE1
# [1] 1.007643 1.030573 1.050955 1.395992 1.870515 2.828152 3.109110 3.561579 4.826512 8.121690
# $KE1
# [1] 0.7471826 0.8630273 1.1020671
# $KE2
# [1] 1.135127 1.318320
# $KE3
# [1] 0.9451205 0.9963938
# $KE4
# [1] 0.5987871 0.7214808 0.8084978
# $KE5
# [1] 0.09964862 0.17546170 0.38848434
# $KE6
# [1] 1.121743 1.157310 1.304740
# $KE7
# [1] 1.356517 1.587785 1.605506
# $KE8
# [1] 1.058783 1.177488 1.379313
# $KE9
# [1] 0.4425607 0.5953608 0.8130755
# $AO1
# NULL
# $MIE2
# [1] 0.9668790 0.9757962 0.9949045

# Manually adjust the breaks:  max. 3 selected splits from regression trees, plus lower and upper (means 4 invervals)
# Can be higher in special cases
#list.splits.KE.sel <- list.splits.KE
#list.splits.KE.sel[[ 1]] <- list.splits.KE[[ 1]] # 3 splits OK
# MIE1: x-variable in 4 KERs. Keep 7 splits and  higher value: 30
#list.splits.KE.sel[[ 2]] <- c(list.splits.KE[[ 2]][c(1,4,5,6,8, 9, 10)],30) # x-variable in 3 KERs. Keep all 5 splits
#list.splits.KE.sel[[ 3]] <- list.splits.KE[[ 3]] # 3 splits 
#list.splits.KE.sel[[ 4]] <- list.splits.KE[[ 4]] # 2 splits
#list.splits.KE.sel[[ 5]] <- list.splits.KE[[ 5]] # 2 splits
#list.splits.KE.sel[[ 6]] <- list.splits.KE[[ 6]] # 3 splits
#list.splits.KE.sel[[ 7]] <- list.splits.KE[[ 7]] # 3 splits
#list.splits.KE.sel[[ 8]] <- c(mean(list.splits.KE[[ 8]][1:2]), list.splits.KE[[ 8]][3]) # KE6: merge 2 close splits
#list.splits.KE.sel[[ 9]] <- c(list.splits.KE[[ 9]][1], mean(list.splits.KE[[ 9]][2:3])) # KE7: merge 2 close splits
#list.splits.KE.sel[[10]] <- unique(list.splits.KE[[10]]) # 4-> 2 splits ## RART AT DET BLIR DE SAMME? men riktig.
#list.splits.KE.sel[[11]] <- list.splits.KE[[11]] # 2 splits
# # Add split for AO-2 (never x-variable): 0.5, 0.7 (from inspection of data)
#list.splits.KE.sel[[12]] <- c(0.5, 0.7) 
# MIE2: Hardly any effect. Keep splits anyway.
#list.splits.KE.sel[[13]] <- list.splits.KE[[13]]


# Make final list of breaks
# Add lower and upper boundary
	list.breaks.KE <- list()
	length(list.breaks.KE) <- length(v.EventID)
	# UV breaks: select all 6 doses, converted to ranges - can be adjusted later
	#> v.Dose
	#[1] 0e+00 8e-04 5e-02 1e-01 2e-01 4e-01
	#list.breaks.KE[[1]] <- c(0, mean(v.Dose[1:2]), mean(v.Dose[2:3]), mean(v.Dose[3:4]), 
	#												 		mean(v.Dose[4:5]), mean(v.Dose[5:6]), v.Dose[6]*1.5) 
	# Add more breaks to UV gradient
	list.breaks.KE[[1]] <- c(0, mean(v.Dose[1:2]), mean(v.Dose[2:3]), mean(v.Dose[3:4]), 
													 		mean(v.Dose[4:5]), 0.25, 0.35, 0.45, v.Dose[6]*1.5) 
	for (j in 2:length(v.EventID))
	{
	# 20.04.2021: Tidligere har jeg lagt til upper & lower boundary her, men nå har jeg gjort det ovenfor!
		# Så denne er egentlig overflødig
	#		y.max <- max(t.DATA.perm[t.DATA.perm$KE.y==v.EventID[j], "y"]) * 1.5
	#  	list.breaks.KE[[j]] <- round(c(0, sort(unique(list.splits.KE.sel[[j]])), y.max), 3) # For appending later values from KER later
		 list.breaks.KE[[j]] <- list.splits.KE.sel[[j]] # For appending later values from KER later
	}
	# Survival: adjust upper limit down to 1
	### SKAL GJØRES!!!
	# list.breaks.KE[[j]]
	
	# Transfer breaks from KE list to KER lists (for plotting etc.)
	list.breaks.KER.x <- list()
	length(list.breaks.KER.x) <- length(v.KER.no)
	names(list.breaks.KER.x) <- v.KER.label
	list.breaks.KER.y <- list.breaks.KER.x
	for (i in 1:length(v.KER.no))
	{
		list.breaks.KER.x[[i]] <- as.vector(list.breaks.KE[[which(v.EventID==v.arrow.x[i])]])
		list.breaks.KER.y[[i]] <- as.vector(list.breaks.KE[[which(v.EventID==v.arrow.y[i])]])
	}


