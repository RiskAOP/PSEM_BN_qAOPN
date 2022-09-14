# Load necessary packages
library(dplyr)
library(zeallot)
library("xlsx")
library(sampling)
library(piecewiseSEM)
library(ggplot2)
library(pROC)


# Load data
load("UVAOP.rdata")

#Calculate variable means for control group
ctlmean<-dat[,c(1,3:5,7:14)]%>%
                   filter(UVB.dose==0)%>%
                   summarise_if(is.numeric, mean, na.rm=TRUE)

# Scale variable values using means of control group
dat[,c(3:5,7:14)]<-mapply(`/`, dat[,c(3:5,7:14)], ctlmean[2:12])

# Convert percentage to values between 0 and 1
dat$KE3<-dat$KE3/100
dat$AO<-dat$AO/100


# Creat an empty data frame to save coefficients of PSEM
coef_sem=data.frame()

# Creat an empty data frame to save AUC
AUC=data.frame()

# Commands below run 1000 PSEM analysis using resampled data
for (j in 1:1000){
    
    dat4SEM=data.frame() # Creat an empty data frame to save resampled data

    #Do resampling to get resampled data.
    for (k in 1:30) {
        #Randomly select 1 observation in each UVB dose for every variables
        #Using functions "strata()" and "getdata()" in package "sampling()"
        dat0=dat[1:6,1,drop=FALSE]
        for (i in 3:15){
          mydf=na.omit(dat[,c(1,i)])
          mydf[order(mydf$UVB.dose),]
          x <- strata(mydf, "UVB.dose", size = rep(1,6), method = "srswr")
          dat0=merge(dat0,getdata(mydf, x)[,c(1,2)],by="UVB.dose")
          set.seed(i+k*100+j*10000) # Change random seeds for each resampling
         }
        dat4SEM=rbind(dat4SEM,dat0) # Resampled data
    }

    # Commands below conduct PSEM analysis
    # Define the sturcture of the PSEM
    modelList<-psem(
           lm(KE1~MIE, dat4SEM),
           lm(KE4~MIE, dat4SEM),
           lm(KE7~MIE, dat4SEM),
           lm(KE9~MIE, dat4SEM),
           lm(KE2~KE1, dat4SEM),
           lm(KE3~KE2, dat4SEM),
           lm(KE5~KE4, dat4SEM),
           lm(KE8~KE7, dat4SEM),
           lm(KE6~KE5+KE8, dat4SEM),
           lm(KE10~KE9, dat4SEM),
           lm(KE11~KE10, dat4SEM),
           glm(AO~KE3+KE6+KE11,"binomial",dat4SEM),
           dat4SEM
       )

      coef_sem<-rbind(coef_sem,coefs(modelList)) # Save coeffients of a PSEM into the coefficient data frame

      # Obtain coefficients of the lowest pathway from the estimated PSEM
         # c(b0, b1, b2, b3, b4) %<-%  coefs(modelList)[c(7,8,9,10,11)]
      
      # Build predict logit model "mod" using the coefficients above
      # Generate "newdata" that only includes the variables of the lowest pathway   
      
      # Prodict probility of AO
      prob=predict(mod,type=c("response"), newdata=newdata)
      
      # Calculate area under the ROC curve
      g <- roc(AO ~ prob, data = dat4logit)
      AUC<-rbind(AUC,data.frame(g$auc[1]))  # Save AUC of a PSEM into the AUC data frame

}

# Rename coefficient variables
coef_sem$path<-paste(coef_sem$Predictor," -> ",coef_sem$Response)
# Rename p-value variale
names(coef_sem)[9]<-"sig"

# Make histogram for the coefficients
ggplot(coef_sem, aes(x=Estimate))+
    geom_histogram(bins=10)+
    facet_wrap(~path,ncol=4,scales="free")+
    xlab("Estimated coefficient")+ylab("Count")+
    theme(axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 8))

# Surmmarize the coefficients
summarized_coef<-coef_sem %>%
  group_by(path) %>%
  summarise_at(vars(-c(Response, Predictor, sig)), list(mean=mean),na.rm=TRUE)