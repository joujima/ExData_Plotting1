##
## Setting local patterns to avoid discrepancies between answers
Sys.setlocale(category = "LC_TIME", locale="en_US.UTF-8")

## Checking if there is already a downloaded zipfile, downloading it if necessary
zipfilename = "household_power_consumption.zip"
if(!file.exists(zipfilename)){
   download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",destfile = zipfilename)
}
## At this point, we must have a zip file.
## Checking if there is already a unzipped file, unzipping it if necessary
unzipfilename = "household_power_consumption.txt"
if(!file.exists(unzipfilename)){
   unzip(zipfilename)
}
## At this point, we must have a unzipped file.

## Reading data set as characters
power_consumption_dataset <- read.table(file = unzipfilename,header = TRUE, sep = ";", colClasses = "character")

## Converting characters to correct date formats
power_consumption_dataset$Date <- as.Date(power_consumption_dataset$Date, format="%d/%m/%Y")

library(dplyr)

## Filtering data corresponding to dates of interest ("2007-02-01" and "2007-02-02")
partial_power_consumption <- rbind(filter(power_consumption_dataset,Date==as.Date("2007-02-01")),filter(power_consumption_dataset,Date==as.Date("2007-02-02")))

## Converting characters to correct time format
partial_power_consumption$Time <- (format(strptime(partial_power_consumption$Time, format="%T"),"%T"))

## Preparing the date format corresponding to the data of interest
partial_power_consumption <- mutate(partial_power_consumption, FullDate = paste(Date,Time))
dates <-strptime(partial_power_consumption$FullDate, format = "%F %T")

make_plot3 <- function(partial_power_consumption,dates){
   ## Aspect of plotting area
   par(mfrow= c(1,1))
   
   ## Selecting the data of interest
   sub_metering <- select(partial_power_consumption, FullDate,Sub_metering_1:Sub_metering_3)
   sub_1 <- as.numeric(sub_metering$Sub_metering_1)
   sub_2 <- as.numeric(sub_metering$Sub_metering_2)
   sub_3 <- as.numeric(sub_metering$Sub_metering_3)
   
   ## Plotting desired data
   
   ## We have to choose the variable with the highest value to plot first
   max1 <- max(sub_1)
   max2 <- max(sub_2)
   max3 <- max(sub_3)
   highest <- max(max1,max2,max3)
   if(max1==highest){
      plot(dates, sub_1, type = "n", ylab = "Energy sub metering", xlab = NA)
   } else{
      if(max2==highest){
         plot(dates, sub_2, type = "n", ylab = "Energy sub metering", xlab = NA)
      } else{
         plot(dates, sub_3, type = "n", ylab = "Energy sub metering", xlab = NA)
      }
   }
   ## Drawing the lines for each dataset
   lines(dates, sub_1, col="black")
   lines(dates, sub_2, col="red")
   lines(dates, sub_3, col="blue")
   
   ## Adding legend
   legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), col = c("black", "red", "blue"), lty = 1)
}

## Plotting to PNG file
png(file= "plot3.png", width=480, height=480)
make_plot3(partial_power_consumption, dates)
dev.off()

## Applying the same legend to display
make_plot3(partial_power_consumption,dates)