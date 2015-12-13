# Author: E. Crestaz
# Date:   2015-12-13
# Scope:  Create 'plot1.png' plot file, reporting frequency histogram of global active
#         power over period of interest (1-2/2/2007)
# Notes:  Code developed as part of the "Exploratory data analysis" distance learning
#         course, in the framework of Data Science Specialization of Johns Hopkins Un., 
#         Bloombert School of Public Health, at Coursera.org

library(dplyr)

setwd("C:/Data1/Coursera/JohnHopkins/DataScienceSpecialization/ExploratoryDataAnalysis/Assignments/CourseProject1")
filename <- "./exdata-data-household_power_consumption/household_power_consumption.txt"

# Read in text data file and filter for dates of interest
df <- read.table(filename, header=TRUE, sep=";", na.strings="?")

df <- df %>%
  filter (Date %in% c("1/2/2007","2/2/2007")) %>%
  droplevels()

# Create a date-time vector based on original distinct attributes
datetime <- paste(df$Date,df$Time)

# Pay attention to conversion string, %Y being used instead of %y because of extended date
df$datetime <- strptime(datetime, "%d/%m/%Y %H:%M:%S")

# Build frequency histogram of global active power frequency in given period
png(filename="plot1.png",width=480,height=480,units="px")

hist(df$Global_active_power,
     main="Global Active Power",
     xlab="Global Active Power (kilowatts)",
     ylab="Frequency",
     col="red")

dev.off()