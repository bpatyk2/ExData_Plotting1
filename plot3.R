plot3 <- function()
{
  library(png)
  library(data.table)
  
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
    
    colNames <- c("Date","Time","Gap","Grp","Voltage","GI","Sm1","Sm2","Sm3")
    
    powerdata <- fread(cmd = paste('grep -E "^1/2/2007|^2/2/2007"', filepath), 
                       colClasses = c("V1" = "myDate"), col.names = colNames)
    unlink(filepath)
    
    powerdata <- subset(powerdata, Date <= "2007-02-02" & Date >= "2007-02-01")
    powerdata$Time <- as.POSIXct(paste(powerdata$Date, powerdata$Time), tz = "UTC", 
                                 format = "%Y-%m-%d %H:%M:%S")
    fwrite(powerdata, file.path(rootpath,"powerdata.txt"))
  }
  else 
  {
    powerdata <- fread(filepath)
    powerdata$Time <- as.POSIXct(powerdata$Time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  }
  
  filepath <- file.path(rootpath, "plot3.png")
  if(exists(filepath)) unlink(filepath)
  png(filepath)
  plot(powerdata$Time, powerdata$Sm1, type = "l", col = "black", xlab = "", 
       ylab = "Energy sub metering")
  lines(powerdata$Time, powerdata$Sm2, type = "l", col = "red")
  lines(powerdata$Time, powerdata$Sm3, type = "l", col = "blue")
  legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         col = c("black", "red", "blue"), lty = rep(1, 3))
  dev.off()
  img <- readPNG(filepath)
  grid::grid.raster(img)
}