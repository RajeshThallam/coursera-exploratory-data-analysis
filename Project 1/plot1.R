#==============================================================================
#TITLE           : plot1.R
#DESCRIPTION     : Exploratory Data Analysis - Course Project 1
#         1. Download household power consumption data
#         2. Subset data for 2 days - 2007-02-01 and 2007-02-02
#         3. Draw frequency distribution plot of household global 
#            minute-averaged active power (in kilowatt) 
#AUTHOR          : Rajesh Thallam
#DATE            : 1/11/2015
#VERSION         : 0.1
#USAGE           : draw.plot1()
#NOTES           : Script can be executed in R console
#R_VERSION       : R version 3.1.1 (2014-07-10)
#==============================================================================

# helper method: logging to the console
p <- function(...) {
  cat("[plot1.R]", format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"),..., "\n")
}

# helper method: downloading data if not available
download.data <- function() {
  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  download.dir <- "data"
  data.dir <- "data"
  zip.file <- file.path(download.dir, "dataset.zip")
  
  # download data
  if(!file.exists(download.dir)) { dir.create(download.dir) }
  if(!file.exists(zip.file)) { download.file(url, zip.file) }
  
  # extract data
  if(file.exists(zip.file)) { unzip(zip.file, exdir = ".", overwrite = TRUE) }
  data.dir
}

# main function
# draw plot1 function
draw.plot1 <- function() {
  p("Exploratory Data Analysis Project 1")
  p("Starting up...")
  p("Preparing to draw plot 1")
  
  # download and extract data
  p("Downloading and extracting data files")
  download.data()
  
  # read electric power consumption data
  # set NA string as ? since missing code values are set as "?"
  # read first 2 data columns as character and next 7 as numeric
  data <- read.table(
    "household_power_consumption.txt",
    header = TRUE,
    sep = ";",
    colClasses = c(rep("character",2), rep("numeric",7)),
    na.strings = "?")
  
  # subset the desired data between 2007-02-01 to 2007-02-02
  p("Subset data for 2 days: 2007-02-01 and 2007-02-02")
  plot.data <- subset(
    data, 
    data$Date %in% c("1/2/2007", "2/2/2007")
    
    # convert first two columns to date/time
    p("Converting date/time columns")
    plot.data$timestamp <- strptime(
      paste(plot.data$Date, plot.data$Time),
      format = "%d/%m/%Y %H:%M:%S")
  
  # open png device to draw the plot
  p("Open PNG file to draw the plot") 
  png(
    filename = "plot1.png", 
    height = 480, 
    width = 480, 
    bg = "transparent")

  # plot the histogram for one dimension - Global Active Power
  p("Draw histogram for one dimension - Global Active Power")
  hist(
    plot.data$Global_active_power,
    col = "red",
    main = "Global Active Power",
    xlab = "Global Active Power (kilowatts)",
    ylab = "Frequency")

  # close the png device
  p("Close PNG file")
  dev.off()
}

# draw plot
draw.plot1
  