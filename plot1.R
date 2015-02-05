plot1 <- function(file = "household_power_consumption.txt"){
      ## check if file exists 
      ## if no argument is given, and default does not exists, return warning)
      if (!file.exists(file)){
            return (paste
                    ("The file '", file, "' cannot be loaded. ", 
                     "Make sure you pass valid filename or set your wd (use setwd()) to unzipped directory.",
                     sep = ""))
      }
      require(data.table)
      require(plyr)
      ## Load data: only take 2007-02-01 & 2007-02-02, take the "?" as 'NA'
      ## To avoid problems during loading, set colClasses first as character
      data <- fread(file, colClasses = "character", na.strings = "?")
      data <- data[(data$Date=="1/2/2007"|data$Date=="2/2/2007"),]
      ## Change all "?" to "NA" and put relevant columns to numeric for plotting
      data[data=="?"]<-NA
      data$Global_active_power <- as.numeric(as.character(data$Global_active_power))
      
      ## plot the graph and save as .png file
      ## default values of size is also required size (480x480)
      png(filename="plot1.png") 
      hist(data$Global_active_power, col="red", 
           xlab = "Global Active Power (kilowatts)",
           main = "Gloabl Active Power")
      ## close graphic device
      dev.off() 
}