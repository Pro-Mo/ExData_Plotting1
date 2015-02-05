plot4 <- function(file = "household_power_consumption.txt"){
      ## check if file exists 
      ## if no argument is given, and default does not exists, return warning)
      if (!file.exists(file)){
            return (paste
                    ("The file '", file, "' cannot be loaded. ", 
                     "Make sure you pass valid filename or set your wd (use setwd()) to unzipped directory.",
                     sep = ""))
      }
      require(plyr)
      ## Load data: only take 2007-02-01 & 2007-02-02, take the "?" as 'NA'
      ## To avoid problems during loading, set colClasses first as character
      data <- fread(file, colClasses = "character", na.strings = "?")
      data <- data[(data$Date=="1/2/2007"|data$Date=="2/2/2007"),]
      ## Change all "?" to "NA" and put relevant columns to numeric for plotting
      data[data=="?"]<-NA
      data$Global_active_power <- as.numeric(as.character(data$Global_active_power))
      date_time <- paste(data$Date, data$Time)
      date_time <- strptime(date_time, "%d/%m/%Y %H:%M:%S")
      data <- mutate(data, Date = date_time)
      
      ## plot the graph and save as .png file
      ## default values of size is also required size (480x480)
      png(filename="plot4.png") 
      par(mfcol=c(2,2))
      ## first two are the ones already done in plot2 and plot3 script;
      
      ## from plot2 script
      plot(data$Date, data$Global_active_power, xaxt="n", type="l", xlab="",
           ylab = "Global Active Power")
      r <- as.POSIXct(round(range(data$Date), "days"))
      axis.POSIXct(1, at = seq(r[1], r[2], by = "day"), format = "%a")
      
      ## from plot3 script
      plot(data$Date, data$Sub_metering_1, xaxt="n", type="n", xlab="",
           ylab = "Energy sub metering")
      ## add line with sub_1, 2 and 3 seperate to give them different colors
      points(data$Date, data$Sub_metering_1, type ="l")   
      points(data$Date, data$Sub_metering_2, type ="l", col = "red")   
      points(data$Date, data$Sub_metering_3, type ="l", col = "blue")  
      r <- as.POSIXct(round(range(data$Date), "days"))
      axis.POSIXct(1, at = seq(r[1], r[2], by = "day"), format = "%a")
      legend("topright", legend = c("Sub_metering_1", "Sub_metering_2" , "Sub_metering_3"), 
             lty=c(1,1,1), col=c("black","red","blue"), bty="n")
      
      ## doing the richt upper one;
      plot(data$Date, data$Voltage, xaxt="n", type="l", xlab="datetime",
           ylab = "Voltage")
      r <- as.POSIXct(round(range(data$Date), "days"))
      axis.POSIXct(1, at = seq(r[1], r[2], by = "day"), format = "%a")
      
      ## And the last, lower-right one:
      plot(data$Date, data$Global_reactive_power, xaxt="n", type="l", xlab="datetime",
           ylab = "Global_reactive_power")
      r <- as.POSIXct(round(range(data$Date), "days"))
      axis.POSIXct(1, at = seq(r[1], r[2], by = "day"), format = "%a")
      
      ## close graphic device
      dev.off() 
}