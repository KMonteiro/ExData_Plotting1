plot3 <- function() {
  
  ##############################################################################
  ## In order to make the homework more checkable, I have copied the contents ##
  ## of my getData function here so that the ploting function can stand alone.##
  ##############################################################################
  
  cat("Warning: This plot function takes awhile to run because it reads & manipulates the original data each time.\n")
  
  ## This function reads the data of the Household Power Consumption file already present in the working directory.
  
  myData <- read.table('household_power_consumption.txt', sep=';', header=TRUE, na.strings = "?") %>%
    
    ## Reformat the Date & Time data - turning text into date and time data
    mutate(Date = as.Date(Date, '%d/%m/%Y')) %>%
    mutate(Time = as.POSIXct(strptime(paste(Date, ' ', Time), '%Y-%m-%d %H:%M:%S'))) %>%
    
    ## Select out only the time period requested in the assignment
    filter(Time >= strftime('2007-02-01 00:00:00'), Time < strftime('2007-02-03 00:00:00'))  %>%
    
    ## Reformat the Global active power as numeric (rather than text, necessary in order to graph the data)
    mutate(Global_active_power = as.numeric(as.character(Global_active_power))) 
  
  
  
  ########################################################################
  ## This is the section that does all the plotting and saving of plots.##
  ########################################################################
  
  
  cat("This function removes previous versions of plot3 from your working directory.\n ")
  
  ## Remove previous plot1 files if they have any...
  if (file.exists("plot3.png")) file.remove("plot3.png")
  
  ## Set background to match the example in the homework assignment
  par(bg = "white")
  
  ## Using with to avoid typing myData multiple times, plot the three sub-metering variables over time.
  
  with(myData, {
    plot(Sub_metering_1~Time, type="l",
         ylab="Energy sub metering", xlab="")
    lines(Sub_metering_2~Time,col='Red')
    lines(Sub_metering_3~Time,col='Blue')
  })
  
  ## Add the appropriate legend
  
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, 
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  
  ## Copy the resulting plot to a PNG file in the working directory, setting height and width as required by the assignment
  
  dev.copy(png, file="plot3.png", width=480, height=480)
  dev.off()
  
  ## Inform the user that you just created a PNG file, what its name is and where it lives.
  
  cat("Plot3.png has been saved in", getwd())
}


