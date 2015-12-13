# Author: E. Crestaz
# Date:   2015-12-13
# Scope:  Create 'plot4.png' plot file, reporting multiple base plots of key variables 
#         (global active power, voltage, sub-metering readings, global reactive power)
#         over period of interest (1-2/2/2007)
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

# Build multiple base plots of key variables (global active power, voltage,
# sub-metering readings, global reactive power) in given period
png(filename="plot4.png",width=480,height=480,units="px")

# Set plot parameters, number of rows/columns and margins (to maximize plotting area)
par(mfrow=c(2,2),mar=c(4,4,1,1))
with (df, {
  # Global active power vs. date/time (note: I would keep measurement units, kilowatts)
  plot(datetime,Global_active_power,type="l",
       xlab="",ylab="Global Active Power")
  
  # Voltage vs. date/time
  plot(datetime,Voltage,type="l",
       xlab="datetime",ylab="Voltage")
  
  # Sub-metering readings vs. date/time
  ymax <- max(df$Sub_metering_1,df$Sub_metering_2,df$Sub_metering_3)
  ymin <- min(df$Sub_metering_1,df$Sub_metering_2,df$Sub_metering_3)
  plot(datetime,Sub_metering_1,type="l",xlab='',ylab='Energy sub metering',col="black",
       ylim=c(ymin,ymax))
  par(new=T)
  plot(datetime,Sub_metering_2,xlab='',ylab='',axes=F,type="l",col="red",
       ylim=c(ymin,ymax))
  par(new=T)
  plot(datetime,Sub_metering_3,xlab='',ylab='',axes=F,type="l",col="blue",
       ylim=c(ymin,ymax))
  legend("topright", lty=c(1,1,1), col = c("black","red","blue"), 
         legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
  par(new=F)
  
  # Global reactive power vs. date/time (note: I would keep measurement units, kilowatts)
  plot(datetime,Global_reactive_power,type="l",
       xlab="datetime",ylab="Global Reactive Power")  
})

dev.off()