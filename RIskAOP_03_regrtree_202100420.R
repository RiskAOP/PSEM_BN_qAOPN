# YSO-BN
# SETTING CLASS BOUNDARIES: REGRESSION TREES ETC.

library(party)
library(rpart)

# Run regression tree for each KER in loop; store in list

# Make list for storing results: fit and splits
list.RegTree <- list()
length(list.RegTree) <- 2
names(list.RegTree) <- c("fit", "splits")
for (j in 1:length(list.RegTree))
{
	list.RegTree[[j]] <- list()
	length(list.RegTree[[j]]) <- length(v.KER.no)
	names(list.RegTree[[j]]) <- paste(v.KER.no, v.KER.label, sep=": ")
}

# Regression tree for each KER
for (i in 1:length(v.KER.no))
{
	x <- unlist(t.DATA.perm[t.DATA.perm$KER.no==i, "x"])
	y <- unlist(t.DATA.perm[t.DATA.perm$KER.no==i, "y"])
	fit.rpart <- rpart(y ~ x)
	list.RegTree$fit[[i]] <- fit.rpart
	list.RegTree$splits[[i]] <- fit.rpart$splits[,"index"]
}

#list.RegTree$splits

