plot1 <- function() {

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
  
    
  ##Tell people you are blowing away their previous fiels named plot1 if they have any...
  
  cat("This function removes previous versions of plot1 from your working directory. \n ")

  ## Remove previous plot1 files if they have any...
  if (file.exists("plot1.png")) file.remove("plot1.png")
  
  ## Set background to match the example in the homework assignment
  par(bg = "white")

  ## Plot a histagram of myData Global Active Power -- note that the original data has previously been mutated from character to numeric to accomplish this
  hist(myData$Global_active_power, col = "red", main= "Global Active Power", xlab="Global Active Power (kilowatts)", ylab="Frequency", axes = TRUE, plot = TRUE)

  ## Copy the resulting plot to a PNG file in the working directory, setting height and width as required by the assignment
  dev.copy(png, file="plot1.png", height=480, width=480)
  dev.off()

  ## Inform the user that you just created a PNG file, what its name is and where it lives.

  cat("A new PNG file named Plot1 has been created in your working directory")

}

