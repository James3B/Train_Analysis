

# Source AccidentInput2.R
setwd("D:\\Train_Accident")
source("AccidentInput2.R")


# Read the accident data from files in a directory
# which is the first parameter. The second parameter
# is the format of the files.
# The default is csv.+


totacts <- files2DF("D:\\Train_Accident\\TrainData", "txt")

dim(totacts)
# totacts should have 46883 rows and 140 variables

# Save your result to disk
# path is the location 

write.csv(totacts, "D:\\Train_Accident\\TrainOutput\\CleanerFile" , row.names = F)

#************************************
#
# Data Cleaning Function
#
#************************************


# Get the clean data set
# that removes duplicates
# removes the accident associated with the 9/11/01
# attack
# Removes rows with NA's but keeps TYPEQ
# Cleans TYPE, TYPEQ, and CAUSE

totactsClean <- traindataclean(totacts, list("TYPEQ", "ACCTRKCL", "AMPM"))

# totactsClean should have 37912 rows and 72 columns


# Save your result to disk
# path is the location 

write.csv(totactsClean, "D:\\Train_Accident\\TrainOutput\\totactsClean", row.names = F)
#************************************
#
# Extreme Damage Accidents
#
#************************************

# Function to get the accidents
# above the upper whisker in the
# box plot of accident damage
# Takes  clean DF as input

par(mfcol=c(1,1), oma=c(0,0,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))

xdmg <- extreme.damage(totactsClean)

# Save your result to disk
# path is the location 

write.csv(xdmg, "D:\\R\\R_Output\\ExtremeClean", row.names = F)

# xdmg has 4999 observations and 72 variables

# Remove unneeded variables
# Variables to keep 
# "AMPM"     "TYPE"     "CARS" 
# "TEMP"     "VISIBLTY" "WEATHER"  "TRNSPD"   "TONS"    
# "TYPEQ"    "POSITON1" "POSITON2" "HEADEND1" "MIDMAN1"  "MIDREM1" 
# "RMAN1"    "RREM1"    "HEADEND2" "MIDMAN2"  "MIDREM2"  "RMAN2"   
# "RREM2"    "LOADF1"   "LOADP1"   "EMPTYF1"  "EMPTYP1"  "CABOOSE1"
# "LOADF2"   "LOADP2"   "EMPTYF2"  "EMPTYP2"  "CABOOSE2"
# "CAUSE"   "ACCTRKCL"  "ACCDMG"
matrix(colnames(xdmg))
xdmgClean <- xdmg[,c("AMPM", "TYPE",  "CARS", "TEMP", "VISIBLTY",  "WEATHER",  "TRNSPD", "TONS", "TYPEQ", "POSITON1", "POSITON2", "HEADEND1", "MIDMAN1", "MIDREM1",
                    "RMAN1", "RREM1", "HEADEND2", "MIDMAN2","MIDREM2","RMAN2","RREM2", "LOADF1", "LOADP1", "EMPTYF1", "EMPTYP1", "CABOOSE1", "LOADF2", "LOADP2", "EMPTYF2", "EMPTYP2", "CABOOSE2",
                    "CAUSE", "ACCTRKCL", "ACCDMG")]

summary(xdmgClean$WEATHER)
xdmgClean$WEATHER <- factor(xdmg$WEATHER, labels = c("Clear", "Cloudy", "Rain", "Fog", "Sleet", "Snow"))
xdmgClean$VISIBLTY <- factor(xdmg$VISIBLTY, labels = c("Dawn", "Day", "Dusk", "Dark"))

summary(xdmgClean$AMPM)
xdmgClean$AMPM[which(xdmgClean$AMPM =="")] <- "AM"

# 4999 rows & 37 columns -- had 34

library(DMwR)

# Apply knn imputation (knnImputation) - putting in a value for missing values - Canears Neighbor - pick 10 rows with simmilar factors - adds the average of those

xdmgimpute <- knnImputation(xdmgClean)

write.csv(xdmgimpute, file ="D:\\Train_Accident\\TrainOutput\\xdmgImpute.csv", row.names = F)
# Get summary
summary(xdmgimpute[,c("ACCTRKCL", "TYPEQ", "AMPM")])




