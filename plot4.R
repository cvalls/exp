readTheData <- function()
{
	# File is big but it fits into my memory. If not i would use
	# read.csv.sql from the package sqldf package 
	datafile <- read.csv("household_power_consumption.txt", 
				header = TRUE,
				stringsAsFactors = FALSE,
				na.strings = "?",
				sep = ";")
	filteredData <- subset (datafile, datafile$Date == "1/2/2007" | datafile$Date == "2/2/2007")

	## to free memory from unused data
	rm(datafile)
	gc()
	
	## Add a new col with Time
	dttm1 <- paste(filteredData$Date, filteredData$Time)

	## Date and Time should be in GMT ref. system
	dttm2 <- as.POSIXct(dttm1, format="%d/%m/%Y %H:%M:%S",  tz = "GMT")  

	## Add a new col with Date-Time Objects
	filteredData[["dttm"]] <- dttm2
	return (filteredData)
}

histogram1 <- function (filteredData)
{
	## plot the histogram
	hist(filteredData$Global_active_power, 
		main="Global Active Power", 
		xlab = "Global Active Power (Kilowatts)", 
		ylab = "Frecuency", 
		col = "red")
}
plot1 <- function (filteredData)
{
	
	with (filteredData, {plot(filteredData$dttm, filteredData$Global_active_power, pch = 46, type = "n", ylab = "Global Active Power (Kilowatts)", xlab="")
		lines(filteredData$dttm, filteredData$Global_active_power)})
}	

plot2 <- function (filteredData)
{
	with (filteredData, {plot(filteredData$dttm, filteredData$Voltage, pch = 46, type = "n", ylab = "Voltage", xlab="datetime")
		lines(filteredData$dttm, filteredData$Voltage)})
}	
			
						
#third plot it is very similar to plot number 3, but without box around the legend
plot3 <- function (filteredData)
{
	## difficuly if this graps is in the legend. first we need a line so we'll use a lty = 1
	## Now The problem is how to avoid the point mixed with the line.
	## I couldn't find how to hide the point asking saint google, so It seems it is not possible to hide the point.
	## But a work aroud and possible solution is a little bit tricky. 
	## The effect we've got is to have a single pixel over the legend line. Using merge, only can move the point 
	## to the right so it is not a solution. Now setting line width to 1 pixels (lwd = 1), 
	## every line covers completly that non desired pixel and shows a continuos line, as is asked

	## this is a pure line plot. So we need to avoid scatters
	with (filteredData, {plot(c(dttm, dttm, dttm), c(Sub_metering_1,Sub_metering_2,Sub_metering_3), type='n', ylab = "Energy Sub metering", xlab="")
		lines(dttm, filteredData$Sub_metering_1, col = "black") 
		lines(dttm, filteredData$Sub_metering_2, col = "red") 
		lines(dttm, filteredData$Sub_metering_3, col = "blue") 
		legend("topright", bty= "n", lwd = c(1,1,1),  lty = c(1,1,1), col = c("black", "red", "blue"), cex = 0.65,
		legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))})
	
 
 }

plot4 <- function (filteredData)
{
	with (filteredData, {plot(filteredData$dttm, filteredData$Global_reactive_power, pch = 46, type = "n", ylab = "Global_reactive_power", xlab="datetime")
		lines(filteredData$dttm, filteredData$Global_reactive_power)})
}
 
 main <- function()
 {
 	## let's read the data
	filteredData <- readTheData()

	## if we wanna get day names we can use format(dttm, '%a'%) but we've only two days
	## thursday and wednesday
	## first try shows week day in spanish, but not in english. Forums say to apply. and it works.
	Sys.setlocale("LC_ALL","C")
	
	# we want a matrix of plots 2X2- we'll set margins too.
	par(mfrow = c(2, 2), mar = c(5, 4, 2, 1))
	
	## fill plot 3
	with (filteredData, {	plot1(filteredData)
				plot2(filteredData)
				plot3(filteredData)
				plot4(filteredData)})
			
	## Export to png file
	dev.copy(png, file = "plot4.png")
	
	## Closing the device
	dev.off()
	return()
 }
 