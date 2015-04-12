plot1 <- function ()
{
	# File is big but it fits into my memory. If not i would use
	# read.csv.sql from the package sqldf package 
	datafile <- read.csv("household_power_consumption.txt", 
				header = TRUE,
				stringsAsFactors = FALSE,
				na.strings = "?",
				sep = ";")
	filteredData <- subset (datafile, datafile$Date == "1/2/2007" | datafile$Date == "2/2/2007")

	## memory is finite. So it is better to clean and remove 
	rm(datafile)
	gc()
	
	## Add a new col with Time
	dttm <- paste(filteredData$Date, filteredData$Time)

	## Date and Time should be in GMT ref. system
	dttm2 <- as.POSIXct(dttm, format="%d/%m/%Y %H:%M:%S",  tz = "GMT")  

	## Add a new col with Date-Time Objects
	filteredData[["dttm"]] <- dttm2

	## plot the histogram
	hist(filteredData$Global_active_power, 
		main="Global Active Power", 
		xlab = "Global Active Power (Kilowatts)", 
		ylab = "Frecuency", 
		col = "red")
		
	## Export to png file
	dev.copy(png, file = "plot1.png")
	
	## Closing the device
	dev.off()
	return()
}
