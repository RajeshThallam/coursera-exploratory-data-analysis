#==============================================================================
#TITLE           : plot5.R
#DESCRIPTION     : Exploratory Data Analysis - Course Project 2
#         1. Download emissions data
#         2. Explore how motor vehicle emissions changed in Baltimore city
#         3. Draw a bar plot for motor vehicle emissions for each year 
#AUTHOR          : Rajesh Thallam
#DATE            : 1/25/2015
#VERSION         : 0.1
#USAGE           : draw.plot5()
#NOTES           : Script can be executed in R console
#R_VERSION       : R version 3.1.1 (2014-07-10)
#==============================================================================

# import libraries
library(ggplot2)

# helper method: logging to the console
p <- function(...) {
  cat("[plot5.R]", format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"),..., "\n")
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
# draw plot5 function
draw.plot5 <- function() {
  p("Exploratory Data Analysis Project 2")
  p("Starting up...")
  p("Preparing to draw plot 5")
  
  # download and extract data
  p("Downloading and extracting data files")
  download.data()
  
  # read national emissions data
  p("Reading emissions data")
  NEI <- readRDS("data/summarySCC_PM25.rds")
  
  # read source code classification data
  p("Reading source code classification data")
  SCC <- readRDS("data/Source_Classification_Code.rds")

  # aggregate motor vehicle emissions for each year
  p("Preparing plot data by aggregating emissions from motor vehicles for each year")
  motor.emissions <- aggregate(
  	Emissions ~ year, 
  	NEI[(NEI$fips=="24510") & (NEI$type=="ON-ROAD"), ], 
  	sum)
  
  # open png device to draw the plot
  p("Open PNG file to draw the plot") 
  png(
    filename = "plot5.png", 
    height = 480, 
    width = 480
  )

  # draw bar plot to show emissions from motor vehicle sources in Baltimore City, Maryland
  p("Draw bar plot to show emissions from motor vehicle sources in Baltimore City, Maryland")
  
  plot <- 
    ggplot(
      motor.emissions, 
      aes( x = factor(year), y = Emissions)
    ) +
      geom_bar(stat="identity") +
      xlab("Year") +
      ylab(expression("Total PM"[2.5]*" emissions (in tons)")) +
      ggtitle("Emissions from motor vehicle sources in Baltimore City, Maryland")
  
  print(plot)
  
  # close the png device
  p("Close PNG file")
  dev.off()
}

# draw plot
draw.plot5