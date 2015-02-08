plot2 <- function(file = "household_power_consumption.txt"){
      ## check if file exists 
      ## if no argument is given, and default does not exists, return warning)
      if (!file.exists(file)){
            return (paste
                    ("The file '", file, "' cannot be loaded. ", 
                     "Make sure you pass valid filename or set your wd (use setwd()) to unzipped directory.",
                     sep = ""))
      }
      require(plyr)
      require(data.table)
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
      png(filename="plot2.png") 
      plot(data$Date, data$Global_active_power, xaxt="n", type="l", xlab="",
           ylab = "Global Active Power (kilowatts)")
      r <- as.POSIXct(round(range(data$Date), "days"))
      axis.POSIXct(1, at = seq(r[1], r[2], by = "day"), format = "%a")
      ## close graphic device
      dev.off() 
}