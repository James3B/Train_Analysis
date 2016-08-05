# @file Train Accident_Data.R 
# 
# @brief Train Accident analysis for the
#        for Train Accident data from 2000 to 2014 in the U.S
# @objective of module is to explore data
# @author James Bennett <jbb4nb@virginia.edu>
#
# Analysis performed as part of the Accelerated Master's Program
# in Systems Engineering Statistics for Engineers <http://amp.sys.virginia.edu>
#

setwd("D:\\Train_Accident")
library(ggplot2)
##Read the 2014 accident file

acts14 <- read.table("TrainData\\RailAccidents14.txt", sep = ",", header = TRUE)

summary(acts14)
mean(acts14$ACCDMG, na.rm = FALSE, use = "complete")
var(acts14$ACCDMG, na.rm = FALSE, use = "complete")


mean(acts14$TOTKLD, na.rm = FALSE, use = "complete")
var(acts14$TOTKLD, na.rm = FALSE, use = "complete")

##Source Code contains function which reads all files in a directory 
source("AccidentInput.R")
##Call file.input1 function to read and store files into the acts variable
acts <- file.inputl("D:\\Train_Accident\\TrainData")


############################################
#### DATA CLEANING 

###RETRIVE NAMES OF VARIABLES

matrix(names(acts[[1]]))

matrix(names(acts[[4]]))

matrix(names(acts[[8]]))

###APPEARS DIFFERENT VARIABLES WERE USED IN DIFFERENT YEARS - SOME HAVE 141 Vs. 146 COLUMS

ncol(acts[[1]])
ncol(acts[[4]])
ncol(acts[[8]])
ncol(acts[[14]])

##Get columns in common

comvar <-  intersect(colnames(acts[[1]]), colnames(acts[[8]]))

##combine the columns 

totacts <- combine.data(acts, comvar)
dim(totacts)

###View of accidents  - Most of the accidents are minor 

par(mfcol=c(1,1), oma=c(1,0,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(2,0,0))
boxplot(totacts$ACCDMG,range = 1.5, main = "Boxplot of accident damage")

ggplot(totacts, aes (x = ACCDMG)) + geom_histogram()


###look for the most costly accident

which(totacts$ACCDMG == max(totacts$ACCDMG))

totacts[42881,]
totacts[42882,]

##Duplicate??

duplicated(totacts[1:100, c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])

totacts <- totacts[!duplicated(totacts[, c("YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")]),]

which(totacts$ACCDMG > 1.5e7)

totacts[1223,]
totacts <- totacts[-1223,] ## This accident was from a terroist attack - removing since out of their control

# Do a summary of totacts

names(summary(totacts))

# Are we missing values for any variables?


# How many?

nafind <- function(x){sum(is.na(x))}

apply(totacts,2,"nafind")

# Do we need all the variables?

matrix(names(totacts))

# Remove unnecessary variables, then get a summary

nacount <- apply(totacts,2, "nafind")


varWna <- which(nacount > 0)

# Keep TYPEQ, we'll use it. The others we don't need.


which(colnames(totacts)[varWna] == "TYPEQ")


varWna <- varWna[-which(colnames(totacts)[varWna]== "TYPEQ")]

totacts <- totacts[, -varWna]

# check you working directory and change it if necessary

getwd()

write.csv(totacts, file ="D:\\Train_Accident\\TrainOutput\\totactsClean.csv", row.names = F)

#***********************************
#
# 	Summary of accidents
#
#***********************************

# Get a summary of the cleaned data set
# It does not seem to address the expense associated with Bodily Injuries or Deaths or litigation from deaths or injuries 

summary(totacts)
# How many accidents?

dim(totacts)

# Total cost of all accidents
sum(totacts$ACCDMG)
#summary(totacts$CAUSE)
# Average annual cost of accidents
sum(totacts$ACCDMG)/14
# first yearly costs (sums)
dmgyrsum <- tapply(totacts$ACCDMG, totacts$YEAR, sum)

##what about the cost for injuries and deaths
## for instance

which(totacts$TOTKLD == max(totacts$TOTKLD))

mean(dmgyrsum)
# Total number killed
sum(totacts$TOTKLD)

# Largest number killed in an accident - But the data shows that 
max(totacts$TOTKLD)

# Total number injured
sum(totacts$TOTINJ)


# Largest number injured in an accident 

max(totacts$TOTINJ)
# What is the average number of injuries per year?
round(sum(totacts$TOTINJ)/14)

# types of variables

str(totacts)

#**************************************************
#
#   Time series of Accidents
#
#**************************************************

# Yearly no. of accidents

plot(1:max(totacts$YEAR), tapply(totacts$ACCDMG, totacts$YEAR, length), type = "l", col = "black", xlab = "Year", ylab = "Frequency", main = "Number of Accidents per Year", lwd =2)


# Yearly total cost of accidents

plot(1:max(totacts$YEAR), tapply(totacts$ACCDMG, totacts$YEAR, sum), type = "l", col = "black", xlab = "Year", ylab = "Cost ($)", main = "Total Damage per Year", lwd =2)

# Yearly maximum cost of accidents

plot(1:max(totacts$YEAR), tapply(totacts$ACCDMG, totacts$YEAR, max), type = "l", col = "black", xlab = "Year", ylab = "Cost ($)", main = "Total Damage per Year", lwd =2)

# Putting total and maximum together using symbols 

symbols(2001:2014, tapply(totacts$ACCDMG, totacts$YEAR, sum), circles=tapply(totacts$ACCDMG, totacts$YEAR, max),inches=0.35, fg="white", bg="red", xlab="Year", ylab="Cost ($)", main = "Total Accident Damage")
lines(2001:2014, tapply(totacts$ACCDMG, totacts$YEAR, sum))



# Repeat this for total killed and total injured and the sum of them.
symbols(2001:2014, tapply(totacts$TOTKLD, totacts$YEAR, sum), circles = tapply(totacts$TOTKLD, totacts$YEAR, max), inches = 0.35, fg ="yellow", bg ="red", xlab = "Year", ylab = "People Killed", main = "Total Killed")
lines(2001:2014, tapply(totacts$TOTKLD, totacts$YEAR, sum))

symbols(2001:2014, tapply(totacts$TOTINJ, totacts$YEAR, sum), circles = tapply(totacts$TOTINJ, totacts$YEAR, max), inches = 0.35, fg ="black", bg ="red", xlab = "Year", ylab = "People Injured", main = "Total Injured")
lines(2001:2014, tapply(totacts$TOTINJ, totacts$YEAR, sum))


#***********************************
#
# 	histograms of ACCDMG and TEMP
#
#***********************************

# These examples are for 2011 

hist(acts[[11]]$ACCDMG) # for 2011

hist(acts[[11]]$ACCDMG, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")


# Different bin widths

par(mfrow = c(2,2))

hist(totacts$TEMP, breaks = "scott", main = "Accident Temperatures (Scott)", xlab = "Temp (F)", col = "steelblue")

hist(totacts$TEMP, breaks = "fd", main = "Accident Temperatures (FD)", xlab = "Temp (F)", col = "steelblue")

hist(totacts$TEMP, main = "Accident Temperatures (Sturges)", xlab = "Temp (F)", col = "steelblue")

hist(totacts$TEMP, breaks = 100, main = "Accident Temperatures (100)", xlab = "Temp (F)", col = "steelblue")

par(mfrow = c(1,1))

# Different bin widths

hist(acts[[11]]$ACCDMG, breaks = "scott", main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")

hist(acts[[11]]$ACCDMG, breaks = "fd", main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")

hist(acts[[11]]$ACCDMG, breaks = 20, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")

hist(acts[[11]]$ACCDMG, breaks = 100, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")


# other years


par(mfrow = c(2,2))
hist(acts[[1]]$ACCDMG, main = "Total Accident Damage in 2001", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[4]]$ACCDMG, main = "Total Accident Damage in 2004", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[8]]$ACCDMG, main = "Total Accident Damage in 2008", xlab = "Dollars ($)", col = "steelblue")
hist(acts[[11]]$ACCDMG, main = "Total Accident Damage in 2011", xlab = "Dollars ($)", col = "steelblue")
par(mfrow = c(1,1))



#*********************************************************************
#
# 				Box Plots of Metrics
#         and Extreme Accidents
#
#*********************************************************************

#*****************************
# ACCDMG

boxplot(totacts$ACCDMG, main = "Xtreme Accident damage")
boxplot(totacts$TOTKLD, main = "Deaths in Extreme incident")

# Plot only the extreme points
# (extreme defined by the box plot rule)

# Get the values in the box plot

dmgbox <- boxplot(totacts$ACCDMG)

dmgbox2 <- boxplot(totacts$TOTKLD)

# How many extreme damage accidents?

length(dmgbox$out)
##extreme accident dmg 4862
length(dmgbox2$out)
dmgbox$stats
##extreme accident relative to deaths 479 (this is not commom)

# What proportion of accidents are extreme? (round to 2 digits) - 13%
round(length(dmgbox$out)/length(totacts$ACCDMG),2)

# What is the proportion of costs for extreme damage accidents? (round to 2 digits)
round(sum(dmgbox$out)/sum(totacts$ACCDMG),2) ##13% causes 74% of the damages - Insanity!!
# Create a data frame with just the extreme ACCDMG accidents
round(length(dmgbox2$out)/length(totacts$TOTKLD),2)
##.01 are extreme - deaths are an wear event
round(sum(dmgbox2$out)/sum(totacts$TOTKLD),2)

##all deaths are were events

xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

dim(xdmg)
###4862 are were 

# Look at the boxplots and histograms of these extreme accidents

boxplot(xdmg$ACCDMG, col = "steelblue", main = "Accidents with Extreme Damage", ylab = "Cost ($)")

plot(1:14, tapply(xdmg$ACCDMG, xdmg$YEAR, sum), type = "l", xlab = "Year", ylab = "Total Damage ($)", main = "Total Extreme Accident Damage per Year")

# also plot number of accidents per year.

plot(1:14, tapply(xdmg$ACCDMG, xdmg$YEAR, length), type = "l", xlab = "Year", ylab = "No. of Accidents", main = "Number of Extreme Accidents per Year")

# Frequency of accident types

barplot(table(xdmg$TYPE)) #compare with the totacts plot
##Lots of Derailments - wonder is speeding has to do with this - Type = 1

# Repeat for TOTKLD and TOTINJ
# Create a variable called Casualty = TOTKLD + TOTINJ

max(totacts$TOTINJ) ##1000 in a single accident 
max(totacts$TOTKLD) ##9 in a single maccident
Casualidad = totacts$TOTKLD + totacts$TOTINJ
max(Casualidad) ###1001
plot(1:max(totacts$YEAR), tapply(totacts$TOTKLD, totacts$YEAR, max), type = "l", col = "black", xlab = "Year", ylab = "Frequency", main = "Number of KILLED", lwd =2)
plot(1:max(totacts$YEAR), tapply(totacts$TOTINJ, totacts$YEAR, max), type = "l", col = "black", xlab = "Year", ylab = "Frequency", main = "Number of Injured", lwd =2)
plot(1:max(totacts$YEAR), tapply(Casualidad, totacts$YEAR, max), type = "l", col = "blue", xlab = "Year", ylab = "Frequency", main = "Combined Casualties", lwd =2)

