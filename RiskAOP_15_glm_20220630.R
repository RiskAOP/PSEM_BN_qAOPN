# AOP-BN
# Explore relationships with alternative GLMs (compare with GAM)

library(data.table) # for making table from list of tables
library(mgcv)

# Models:
v.mod.name <- c("gam", "lin", "qua", "log", "sig")
v.mod.no <- 1:length(v.mod.name)

# list for storing glm fits
list.glm <- list()
length(list.glm) <- length(v.KER.no)
names(list.glm) <- v.KER.label
# Add list level for alternative models
for(i in v.KER.no)
{
  list.glm[[i]] <- list()
  length(list.glm[[i]]) <- length(v.mod.name)
  names(list.glm[[i]])  <- v.mod.name
#  for (j in v.mod.no)
#  {
#    list.glm[[i]][[j]] <- list()
#    length(list.glm[[i]][[j]]) <- 4
#    names(list.glm[[i]][[j]]) <- c("fit", "s.pv", "dev.expl", "edf")
#  }
}                

# List for individual components of the fit (for easier unlist)
# FUNKER FREMDELES MED DENNE STRUKTUREN?
list.glm.p   <- list.glm
list.glm.dev <- list.glm
list.glm.df  <- list.glm
list.glm.b0  <- list.glm
list.glm.b1  <- list.glm
list.glm.b2  <- list.glm
list.glm.b1.SE <- list.glm
list.glm.b1.pv <- list.glm

#windows(14,10)

#par(mfrow=c(4,4), mar=c(2,3,3,1), oma=c(2,3,4,1))

for (i in 1:length(v.arrow.x)) 
#for (i in 1:3) 
	{
  x.label <- v.arrow.x[i]
  y.label <- v.arrow.y[i]
  x <- unlist(t.DATA.perm[t.DATA.perm$KER.no==i, "x"])
	y <- unlist(t.DATA.perm[t.DATA.perm$KER.no==i, "y"])
# 07.03.2022: LEGGER TIL FLERE MODELLER HER
  for(j in v.mod.no)
  {
    if (j==1)
    {
      fit <- gam(y ~ s(x, k=4))
      list.glm[[i]][[j]]     <- fit
      list.glm.p[[i]][[j]]   <- summary(fit)$s.pv
      list.glm.dev[[i]][[j]] <- summary(fit)$dev.expl
      list.glm.df[[i]][[j]]  <- summary(fit)$edf
      list.glm.b0[[i]][[j]]  <- NA  
      list.glm.b1[[i]][[j]]  <- NA
      list.glm.b2[[i]][[j]]  <- NA
      list.glm.b1.SE[[i]][[j]]  <- NA
      list.glm.b1.pv[[i]][[j]]  <- NA 
      
    }
    if (j==2)
	    fit <- glm(y ~ x)
	  if (j==3)
	    fit <- glm(y ~ x + I(x^2))
  	if (j==4)
	    fit <- glm(y ~ x + log(x+0.001))
  	if (j==5)
  	{
  		# AO: logistic regression; all others: lm (placeholder)
  		ifelse(i %in% c(5,9,16), 
  			fit <- glm(y ~ x, family=binomial),
  			fit <- glm(y ~ x))
  	}
	  if(j %in% c(2:5))
	  {
	    list.glm[[i]][[j]]     <- fit
	    list.glm.p[[i]][[j]]   <- NA # summary(fit)$s.pv # How to get p value?
	    list.glm.dev[[i]][[j]] <- NA
	    list.glm.dev[[i]][[j]] <- (summary(fit)$null.deviance - summary(fit)$deviance)/summary(fit)$null.deviance
	    list.glm.df[[i]][[j]]  <- NA # (Will get >1 value.not of interest?) 
	    list.glm.b0[[i]][[j]]  <- coef(fit)[1]  
	    list.glm.b1[[i]][[j]]  <- coef(fit)[2]
	    list.glm.b2[[i]][[j]]  <- 0  
	    list.glm.b1.SE[[i]][[j]]  <- summary(fit)$coef["x", "Std. Error"]
	    list.glm.b1.pv[[i]][[j]]  <- summary(fit)$coef["x", 4]  # Pr(>|t|) or Pr(>|z|)
	  }
  	if(j %in% c(3:4)) # only quadratic and log-lin have 3 coef
  	{
  	  list.glm.b2[[i]][[j]]  <- coef(fit)[3]  
  	}
  	  
  #summary(fit)

  	
#  list.glm[[i]][[j]]     <- fit
#	list.glm.p[[i]][[j]]   <- summary(fit)$s.pv
#	list.glm.dev[[i]][[j]] <- summary(fit)$dev.expl
#	list.glm.df[[i]][[j]]  <- summary(fit)$edf
#	list.glm[[i]][[j]]$fit
#	list.glm[[i]][[j]]$s.pv   <- summary(fit)$s.pv
#	list.glm[[i]][[j]]$dev.expl <- summary(fit)$dev.expl
#	list.glm[[i]][[j]]$edf  <- summary(fit)$edf
	} # End of j model
} # End of i KER


############################################################################################


#### GLM output: deviance explained ####

# t.GLM <- rbindlist(list.glm.p) # Input må være list of data.table
t.GLM <- data.frame(matrix(NA, ncol=12, nrow=length(v.KER.no)*length(v.mod.no)))
names(t.GLM) <- c("KER.no", "KER.label", "model.no", "model.label", "s.pv", "dev.expl", "edf",
                  "b0", "b1", "b2", "b1.pv", "b1.SE")
#t.GLM$KER.no <- rep(v.KER.no, length(v.mod.no)) # ble feil!
#t.GLM$KER.label <- rep(v.KER.label, length(v.mod.no))
#t.GLM$model.no <- rep(v.mod.no, each=length(v.KER.no))
#t.GLM$model.label <- rep(v.mod.name, each=length(v.KER.no))
t.GLM$KER.no <- rep(v.KER.no, each=length(v.mod.no))
t.GLM$KER.label <- rep(v.KER.label, each=length(v.mod.no))
t.GLM$model.no <- rep(v.mod.no, length(v.KER.no))
t.GLM$model.label <- rep(v.mod.name, length(v.KER.no))
#t.GLM$s.pv <- unlist(list.glm.p) # FUNKER DISSE???
#t.GLM$dev.expl <- unlist(list.glm.dev)
#t.GLM$edf <- unlist(list.glm.df)
t.GLM$s.pv <- unlist(list.glm.p) # FUNKER DISSE???
t.GLM$dev.expl <- unlist(list.glm.dev)
t.GLM$edf <- unlist(list.glm.df)
t.GLM$b0 <- unlist(list.glm.b0)
t.GLM$b1 <- unlist(list.glm.b1)
t.GLM$b2 <- unlist(list.glm.b2)
t.GLM$b1.pv <- unlist(list.glm.b1.pv)
t.GLM$b1.SE <- unlist(list.glm.b1.SE)

t.GLM.ord <- t.GLM[order(t.GLM$KER.no, t.GLM$model.no), ]

write.table(t.GLM.ord, file=paste("t.GLM", Sys.Date(), "xls", sep="."), sep="\t", row.names=FALSE)

