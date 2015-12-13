# Course project 1: Exploratory data analysis at Coursera.org
#
# Estimate total memory amount required to upload electric consumption data to a R data frame
# http://stackoverflow.com/questions/25674221/predict-memory-usage-in-r
library(dplyr)

setwd("C:/Data1/Coursera/JohnHopkins/DataScienceSpecialization/ExploratoryDataAnalysis/Assignments/CourseProject1")

# Note that here below we read in data both as if file would be comma delimited or 
filename <- "./exdata-data-household_power_consumption/household_power_consumption.txt"
top.size <- object.size(read.csv(filename, nrow=1000))
top.size <- object.size(read.table(filename, header=TRUE, sep=";", nrow=1000))

nrows <- 2075261  # Number of rows in input text file based on text file inspection in Notepad++
size.estimate <- nrows/1000 * top.size
size.estimate  # Approx 273Mb. Verify that this memory is available in the notebook

df <- read.table(filename, header=TRUE, sep=";", na.strings="?")
dim(df)

#df <- df %>%
#  filter (Date %in% c("1/2/2007","2/2/2007")) %>%
#  droplevels()

df <- df %>%
  filter (Date %in% c("1/2/2007","2/2/2007")) %>%
  droplevels()

str(df)
dim(df)

# REMEMBER NAs (? in the specific case of current data set)

# Create a date-time vector based on original distinct attributes
#dt <- paste(as.character(df$Date),as.character(df$Time))
datetime <- paste(df$Date,df$Time)
datetime
# Pay attention to conversion string, %Y being used instead of %y because of extended date
df$datetime <- strptime(datetime, "%d/%m/%Y %H:%M:%S")


# It is already numeric! perhaps after modification of read.table to keep trace of NAs
#df$Global_active_power <- as.numeric(df$Global_active_power)

# Build frequency histogram of global active power frequency in given period
png(filename="plot1.png",width=480,height=480,units="px")

hist(df$Global_active_power,
     main="Global Active Power",
     xlab="Global Active Power (kilowatts)",
     ylab="Frequency",
     col="red")  # breaks=xx

dev.off()

# Build global active power (in kilowatts) in given period
png(filename="plot2.png",width=480,height=480,units="px")

with(df,plot(datetime,Global_active_power,type="l",
             xlab="",ylab="Global Active Power (kilowatts)"))

dev.off()

# Build global active power (in kilowatts) in given period
png(filename="plot3.png",width=480,height=480,units="px")

# Build energy sub meatering readings in given period
# Compute maximum sub meatering value
ymax <- max(df$Sub_metering_1,df$Sub_metering_2,df$Sub_metering_3)
ymin <- min(df$Sub_metering_1,df$Sub_metering_2,df$Sub_metering_3)
with(df,plot(datetime,Sub_metering_1,type="l",xlab='',ylab='Energy sub metering',col="black",
        ylim=c(ymin,ymax)))
par(new=T)
with(df,plot(datetime,Sub_metering_2,xlab='',ylab='',axes=F,type="l",col="red",
        ylim=c(ymin,ymax)))
par(new=T)
with(df,plot(datetime,Sub_metering_3,xlab='',ylab='',axes=F,type="l",col="blue",
        ylim=c(ymin,ymax)))

# Add legend (size is optimized when output to png file)
legend("topright", lty=c(1,1,1), col = c("black","red","blue"), 
       legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

par(new=F)

dev.off()

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