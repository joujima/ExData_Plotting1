##
##

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
power_consumption_dataset <- read.table(file = unzipfilename,header = TRUE, sep = ";", colClasses = "character", na.strings = "?")

## Converting characters to correct date formats
power_consumption_dataset$Date <- as.Date(power_consumption_dataset$Date, format="%d/%m/%Y")

library(dplyr)

## Filtering data corresponding to dates of interest ("2007-02-01" and "2007-02-02")
partial_power_consumption <- rbind(filter(power_consumption_dataset,Date==as.Date("2007-02-01")),filter(power_consumption_dataset,Date==as.Date("2007-02-02")))

make_plot1 <- function(partial_power_consumption){
   ## Aspect of plotting area
   par(mfrow= c(1,1))
   
   ## Selecting data of interest
   active_power <- as.numeric(partial_power_consumption$Global_active_power)
   
   ## Generating desired histogram
   hist(active_power,col = "red",xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
}


## Plotting to PNG file
png(file= "plot1.png", width=480, height=480)
make_plot1(partial_power_consumption)
dev.off()

## Applying function again to display plotting
make_plot1(partial_power_consumption)