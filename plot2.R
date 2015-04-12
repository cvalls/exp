plot2 <- function ()
{
	# File is big but it fits into my memory. If not i would use
	# read.csv.sql from the package sqldf package 
	datafile <- read.csv("household_power_consumption.txt", 
				header = TRUE,
				stringsAsFactors = FALSE,
				na.strings = "?",
				sep = ";")
	filteredData <- subset (datafile, datafile$Date == "1/2/2007" | datafile$Date == "2/2/2007")
		
	## Add a new col with Time
	dttm1 <- paste(filteredData$Date, filteredData$Time)

	## Date and Time should be in GMT ref. system
	dttm2 <- as.POSIXct(dttm1, format="%d/%m/%Y %H:%M:%S",  tz = "GMT")  

	## Add a new col with Date-Time Objects
	filteredData[["dttm"]] <- dttm2

	## if we wanna get day names we can use format(dttm, '%a'%) but we've only two days
	## thursday and wednesday
	## first try shows week day in spanish, but not in english. Forums say to apply. and it works.
	Sys.setlocale("LC_ALL","C")
		
	## this is a pure line plot. So we need to avoid scatters
	with (filteredData, {plot(dttm, Global_active_power, type='n', ylab = "Global Active Power (Kilowatts)", xlab="")
		lines(dttm, Global_active_power) })
		
	## Export to png file
	dev.copy(png, file = "plot2.png")
	
	## Closing the device
	dev.off()
	return
 }