plot2 <- function()
{
  library(png)
  library(dplyr)
  
  URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  rootpath <- file.path(".","Plots")
  filename <- "household_power_consumption.zip"
  
  if(!dir.exists(rootpath)) dir.create(rootpath)
  
  setClass('myDate')
  setAs("character", "myDate", function(from) as.Date(from, format = "%d/%m/%Y"))
  
  filepath <- file.path(rootpath,"powerdata.txt")
  
  if(!file.exists(filepath))
  {
    filepath <- file.path(rootpath, filename)
    download.file(URL, filepath, 'curl')
    unzip(filepath, exdir = rootpath)
    unlink(filepath)
  
    filepath <- file.path(rootpath,"household_power_consumption.txt")
  
    powerdata <- read.table(filepath, header = T, colClasses = c("Date" = "myDate"), sep = ";")
    unlink(filepath)
  
    powerdata <- subset(powerdata, Date <= "2007-02-02" & Date >= "2007-02-01")
    powerdata$Time <- strptime(paste(powerdata$Date, powerdata$Time), "%Y-%m-%d %H:%M:%S")
    powerdata <- powerdata %>% mutate_if(sapply(powerdata, is.character), as.numeric)
    write.table(powerdata,file.path(rootpath,"powerdata.txt"), sep = ";")
  }
  else 
  {
    powerdata <- read.table(filepath, header = T, colClasses = c("Date" = "Date"), sep = ";")
    powerdata$Time <- strptime(powerdata$Time, "%Y-%m-%d %H:%M:%S")
    powerdata <- powerdata %>% mutate_if(sapply(powerdata, is.character), as.numeric)
  }
  
  filepath <- file.path(rootpath, "plot2.png")
  if(exists(filepath)) unlink(filepath)
  png(filepath)
  plot(powerdata$Time, powerdata$Global_active_power, type = "l", xlab = "",
       ylab = "Global Active Power (kilowatts)")
  dev.off()
  img <- readPNG(filepath)
  grid::grid.raster(img)
}