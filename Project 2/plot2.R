#==============================================================================
#TITLE           : plot2.R
#DESCRIPTION     : Exploratory Data Analysis - Course Project 2
#         1. Download emissions data
#         2. Explore total emissions PM2.5 for Baltimore city from 1999 to 2008
#         3. Draw bar plot to understand emission pattern for Baltimore city
#AUTHOR          : Rajesh Thallam
#DATE            : 1/25/2015
#VERSION         : 0.1
#USAGE           : draw.plot2()
#NOTES           : Script can be executed in R console
#R_VERSION       : R version 3.1.1 (2014-07-10)
#==============================================================================

# helper method: logging to the console
p <- function(...) {
  cat("[plot2.R]", format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"),..., "\n")
}

# helper method: downloading data if not available
download.data <- function() {
  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.dir <- "data"
  data.dir <- "data"
  zip.file <- file.path(download.dir, "dataset.zip")
  
  # download data
  if(!file.exists(download.dir)) { dir.create(download.dir) }

  if(!file.exists(zip.file)) { download.file(url, zip.file) } 
  else { p("Data already exists") }
  
  # extract data
  if(file.exists(zip.file)) { unzip(zip.file, exdir = "data", overwrite = TRUE) }
  data.dir
}

# main function
# draw plot2 function
draw.plot2 <- function() {
  p("Exploratory Data Analysis Project 2")
  p("Starting up...")
  p("Preparing to draw plot 2")
  
  # download and extract data
  p("Downloading and extracting data files")
  download.data()
  
  # read national emissions data
  p("Reading emissions data")
  NEI <- readRDS("data/summarySCC_PM25.rds")
  
  #read source code classification data
  p("Reading source code classification data")
  SCC <- readRDS("data/Source_Classification_Code.rds")

  # aggregate total emissions over the year in Baltimore city
  p("Preparing plot data by aggregating total emissions in Baltimore city 
    for each year")
  total.emissions <- aggregate(Emissions ~ year, NEI[NEI$fips=="24510", ], sum)
  
  # open png device to draw the plot
  p("Open PNG file to draw the plot") 
  png(
    filename = "plot2.png", 
    height = 480, 
    width = 480, 
    bg = "transparent")

  # draw bar plot between total emissions in Baltimore from all sources for 
  # each of the year
  p("Draw bar plot for total emissions in Baltimore city from all sources for 
    each year")
  
  barplot(
    height=total.emissions$Emissions, 
    names.arg=total.emissions$year,
    xlab="Year", 
    ylab=expression('Total PM'[2]*' emission (in tons)'),
    main=expression('Total PM'[2]*' emission in Baltimore City, Maryland for each year')
  )

  # close the png device
  p("Close PNG file")
  dev.off()
}

# draw plot
draw.plot2