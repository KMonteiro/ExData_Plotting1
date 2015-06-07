library(dplyr)

getData <- function() {
  
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
    
  cat("Done now. Your dataframe is in a variable called myData.")
}
