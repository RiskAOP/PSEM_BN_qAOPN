# AOP-BN
# Import data

setwd("H:/Documents/Jannicke/Projects/RiskAOP/WP3_qAOP/UV_Daphnia/R/")


#library(arm) # Why?
library(data.table) # for making table from list of tables (rbindlist())

# List format
# filename <- "UVdata_BN_20190919_JMO.txt"
filename <- "UVAOP_data_final_JMO.txt"
DATA <- read.delim(filename, header=TRUE)
# Replicates in columns
# DATA.rep. <- read.delim("Daphnia-UV-rep.txt", header=T)
# DATA.rep. <- read.delim("UVdata_BN_rep_20190919_JMO.txt", header=T)
DATA.rep. <- read.delim(filename, header=TRUE)

# BRUKER IKKE LONGEVITY NÅ - KANSKJE SENERE
# DATA.longevity <- read.delim("Survival_temporal_20190507.txt", header=T)

##> names(DATA)
## #[1] "EventID"   "EventName" "Replicate" "Dose"      "Value" ## old
## [1] "EventID"   "Biomarker" "Replicate" "Dose"      "Value"   
##> names(DATA.rep)
##[1] "EventID"     "EventName"   "Dose"        "Replicate.1" "Replicate.2" "Replicate.3" "Replicate.4" "Replicate.5"
##[9] "Replicate.6"
#> names(DATA.rep.)
# [1] "EventID"               "Biomarker"             "Response"             
# [4] "Response_unit"         "Exposure_duration_day" "Dose"                 
# [7] "Rep1"                  "Rep2"                  "Rep3"                 
# [10] "Rep4"                  "Rep5"                  "Rep6" 

#DATA$EventID   <- as.character(DATA$EventID)
#DATA$EventName <- as.character(DATA$EventName)
#DATA$Biomarker <- as.character(DATA$Biomarker)

# Data before exclusions and additions
DATA.rep.$EventID   <- as.character(DATA.rep.$EventID)
#DATA.rep.$EventName <- as.character(DATA.rep.$EventName)
DATA.rep.$Biomarker <- as.character(DATA.rep.$Biomarker)
#v.EventID.   <- c(unique(DATA.rep.$EventID))
v.EventID.   <- unique(DATA.rep.$EventID)
#v.EventName. <- c(unique(DATA.rep.$EventName))
v.Biomarker. <- c(unique(DATA.rep.$Biomarker))

#v.Dose <- sort(unique(DATA$Dose))
#v.EventID   <- c("UV", unique(DATA$EventID))
#v.EventName <- c("UV", unique(DATA$Biomarker))
v.Dose <- sort(unique(DATA.rep.$Dose)) # 20.04.2021
#v.EventID   <- c("UV", unique(DATA.rep.$EventID)) # 20.04.2021
v.EventID.all   <- c("UV", v.EventID.) # 20.04.2021
#v.EventName <- c("UV", unique(DATA$EventName))
v.EventName.all <- c("UV", unique(DATA.rep.$Biomarker))
#t.Dose <- data.frame(EventID=rep("UV",6), EventName=rep("UVB exposure", 6), Dose=v.Dose, 
t.Dose <- data.frame(EventID=rep("UV",6), Biomarker=rep("UVB exposure", 6), 
										 Response=rep("Increase", 6), Response_unit=rep("W/m2", 6), Exposure_duration_day=rep(2, 6),
										 Dose=v.Dose, 
										 #Replicate.1=v.Dose, Replicate.2=v.Dose, Replicate.3=v.Dose, 
										 #Replicate.4=v.Dose, Replicate.5=v.Dose, Replicate.6=v.Dose)
										 Rep1=v.Dose, Rep2=v.Dose, Rep3=v.Dose, 
										 Rep4=v.Dose, Rep5=v.Dose, Rep6=v.Dose)
# Append UVB dose as a response, to simplify the loops.
t.DATA.rep <- rbind(t.Dose, DATA.rep.)

# Make selection of KEs
#v.sel <- !is.element(v.EventID.all, c("AO", "AO_c", "AO_d"))
# 2022-05-02: replacing AO_b (longevity) with AO. Later: try using both.
v.sel <- !is.element(v.EventID.all, c("AO_b", "AO_c", "AO_d"))
v.EventID <- v.EventID.all[v.sel]
#v.EventName <- c("UV", unique(DATA$EventName))
v.EventName <- v.EventName.all[v.sel]

# > v.EventID
# [1] "UV"   "MIE"  "KE1"  "KE2"  "KE3"  "KE4"  "KE5"  "KE6"  "KE7"  
# "KE8"  "KE9"  "KE10" "KE11" "AO"   "AO_b" "AO_c" "AO_d"

# 2022-05-02: Mortality: Replace 100 (% mortality) with 1 (binary):
t.DATA.rep[t.DATA.rep$Biomarker=="Mortality", grep("Rep", names(t.DATA.rep))] <- 
replace(t.DATA.rep[t.DATA.rep$Biomarker=="Mortality", grep("Rep", names(t.DATA.rep))], 
				t.DATA.rep[t.DATA.rep$Biomarker=="Mortality", grep("Rep", names(t.DATA.rep))] == 100, 1) 

# FORTSETT HER! 

# Order Key Event Relationships (arrows) according to figure layout
#v.arrow.x <- c("UV", "MIE-1-Cel", "KE-1", "KE-2",  #  1- 4 
#               "MIE-1-Cel", "KE-4", "KE-5", "KE-3", "KE-6", #  5- 9
#               "UV", "MIE-2", "MIE-1-Cel", "KE-8", "KE-5", # 10-14
#               "KE-7", "KE-9", "KE-7", "KE-9", "AO-1-S7") # 15-19
#v.arrow.y <- c("MIE-1-Cel", "KE-1", "KE-2", "KE-3",  #  1- 4 
#               "KE-4", "KE-5", "KE-6", "KE-6", "KE-7", #  5- 9
#               "MIE-2", "KE-9", "KE-8", "KE-9", "KE-9", # 10-14
#               "AO-1-S7", "AO-1-S7", "AO-2", "AO-2", "AO-2") # 15-19
# v.arrow.x <- c("UV", "MIE", "KE1", "MIE1",   #  1- 4 
#                "KE2", "KE3", "KE4", "KE5",   #  5- 8
# 							 "MIE1", "KE6", "KE7", "MIE1", #  9-12
#                "KE8", "KE9", "UV", "MIE2")   # 13-16
# v.arrow.y <- c("MIE1", "KE1", "AO1", "KE2",  #  1- 4 
# 							 "KE3", "KE4", "KE5", "AO1",   #  5- 8
# 							 "KE6", "KE7", "AO1", "KE8",   #  9-12
# 							 "KE9", "AO1", "MIE2", "KE6")   # 13-16
v.arrow.x <- c("UV",  "MIE",  "KE1",  "KE2", "KE3",  #  1- 5 
							 "MIE", "KE4",  "KE5",  "KE6",         #  5- 8
							 "MIE", "KE7",  "KE8",                 #  9-12
							 "MIE", "KE9",  "KE10", "KE11")        # 13-16
# v.arrow.y <- c("MIE", "KE1",  "KE2",  "KE3", "AO_b",  #  1- 5 
#                "KE4", "KE5",  "KE6",  "AO_b",         #  5- 8
#                "KE7", "KE8",  "KE6",                  #  9-12
#                "KE9", "KE10", "KE11", "AO_b")         # 13-16
v.arrow.y <- c("MIE", "KE1",  "KE2",  "KE3", "AO",  #  1- 5 
							 "KE4", "KE5",  "KE6",  "AO",         #  5- 8
							 "KE7", "KE8",  "KE6",                  #  9-12
							 "KE9", "KE10", "KE11", "AO")         # 13-16
# Ordered KERs
v.KER.label <- paste(v.arrow.x, v.arrow.y, sep=" -> ")
v.KER.no    <- 1:length(v.KER.label)

# Remove two KEs not used in analysis for now:
#t.DATA.rep <- t.DATA.rep[t.DATA.rep$EventID != c("MIE-1-Mit"),]
#t.DATA.rep <- t.DATA.rep[t.DATA.rep$EventID != c("AO-1-S2"),]

# Replace survival AO-1-S7) with longvity
#t.DATA.rep[t.DATA.rep$EventID=="AO-1-S7", grep("Rep", names(t.DATA.rep))] <- 
#	t(matrix(DATA.longevity$Longevity, ncol=6))

#v.EventID   <- as.character(unique(t.DATA.rep$EventID))
#v.EventName <- as.character(unique(t.DATA.rep$EventName))
#v.Biomarker <- as.character(unique(t.DATA.rep$Biomarker))
# Use Biomarker for EventName for now. Can be changed later.
#v.EventName <- v.Biomarker

## SKAL JEG HER SETTE ...?


#### MAKE PERMUTATIONS: ALL POSSIBLE COMBINATIONS OF REPLICATES

# List of tables for data for KERs, generated from permutations

list.DATA.perm <- list()
length(list.DATA.perm) <- length(v.KER.label)
names(list.DATA.perm) <- v.KER.label
# Temporary list, for rbind across doses 
list.DATA.tmp <- list()
length(list.DATA.tmp) <- length(v.Dose)
names(list.DATA.tmp) <- v.Dose


# Loop through KERs (with UV added)
for(i in 1:length(v.KER.no)) # Including MIEs (v.EventID no 1-3)
# for (i in 1:11)
{
	# loop through doses
	for(j in 1:length(v.Dose))
	{
		x. <- t.DATA.rep[t.DATA.rep$EventID==v.arrow.x[i] & t.DATA.rep$Dose==v.Dose[j], 
										 grepl("Rep", names(t.DATA.rep))]
		
		x <- x.[!is.na(x.)]
		n.rep.x <- length(x)
		y. <- t.DATA.rep[t.DATA.rep$EventID==v.arrow.y[i] & t.DATA.rep$Dose==v.Dose[j], 
										 grepl("Rep", names(t.DATA.rep))]
		y <- y.[!is.na(y.)]
		n.rep.y <- length(y)
		x.perm <- rep(x, each =n.rep.y)
		y.perm <- rep(y, times=n.rep.x)
#		xy.perm <- matrix(cbind(x.perm, y.perm), ncol=2, dimnames=list(NULL,c("x","y")))
#		t.xy.perm <- data.frame(xy.perm)
#		t.xy.perm <- data.frame(matrix(NA, ncol=9, nrow=length(v.Dose)*length(x.perm)))
		t.xy.perm <- data.frame(matrix(NA, ncol=9, nrow=length(x.perm)))
		names(t.xy.perm) <- c("KER.no", "KER", "KE.x", "KE.y", "DoseNo", "Dose", "PermNo", "x", "y")
		t.xy.perm$KER.no <- v.KER.no[i]
		t.xy.perm$KER    <- v.KER.label[i]
		t.xy.perm$KE.x   <- v.arrow.x[i]# FIKSE HER
		t.xy.perm$KE.y   <- v.arrow.y[i]
		t.xy.perm$Dose   <- v.Dose[j]
		t.xy.perm$DoseNo <- j
		t.xy.perm$PermNo <- seq(1:length(x.perm))
		t.xy.perm$x      <- x.perm 
		t.xy.perm$y      <- y.perm 		
		list.DATA.tmp[[j]] <- t.xy.perm
	}
	list.DATA.perm[[i]] <- rbindlist(list.DATA.tmp)
}

# Turn list of tables into one table

t.DATA.perm <- rbindlist(list.DATA.perm)
t.DATA.perm <- as.data.frame(t.DATA.perm)
write.table(t.DATA.perm, "t.DATA.perm.txt", sep="\t", row.names=FALSE)


