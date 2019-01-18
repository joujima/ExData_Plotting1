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

## Selecting the data of interest
active_power <- as.numeric(partial_power_consumption$Global_active_power)

## Preparing the date format corresponding to the data of interest
partial_power_consumption <- mutate(partial_power_consumption, FullDate = paste(Date,Time))
dates <-strptime(partial_power_consumption$FullDate, format = "%F %T")

make_plot2 <- function(partial_power_consumption, dates){
   ## Aspect of plotting area
   par(mfrow= c(1,1))
   
   ## Plotting desired data
   plot(dates, active_power, type = "n", ylab = "Global Active Power (kilowatts)", xlab = NA)
   lines(dates, active_power)
}

## Copying to PNG file
png(file= "plot2.png", width=480, height=480)
make_plot2(partial_power_consumption,dates)
dev.off()

## Applying function again to display plotting
make_plot2(partial_power_consumption,dates)