plot4 <- function()
{
  library(png)
  library(data.table)
  
  setClass('date_class')
  setAs("character", "date_class", function(from) as.Date(from, format = "%d/%m/%Y"))
  
  rootpath <- file.path(".", "Plots")
  
  if (!dir.exists(rootpath)) dir.create(rootpath)
  
  URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  filename <- "household_power_consumption.zip"
  filepath <- file.path(rootpath, "power_data.txt")
  
  if (!file.exists(filepath))
  {
    filepath <- file.path(rootpath, filename)
    download.file(URL, filepath, 'curl')
    unzip(filepath, exdir = rootpath)
    unlink(filepath)
    filepath <- file.path(rootpath, "household_power_consumption.txt")
    
    col_names <- c("Date", "Time", "GAP", "GRP", "Voltage", "GI", "SM_1", "SM_2", "SM_3")
    powerdata <- fread(cmd = paste('grep -E "^1/2/2007|^2/2/2007"', filepath), 
                       colClasses = c("V1" = "date_class"), 
                       col.names = col_names)
    unlink(filepath)
    powerdata$Time <- as.POSIXct(paste(powerdata$Date, powerdata$Time), 
                                 tz = "UTC", 
                                 format = "%Y-%m-%d %H:%M:%S")
    fwrite(powerdata, file.path(rootpath, "power_data.txt"))
  }else 
  {
    powerdata <- fread(filepath)
    powerdata$Time <- as.POSIXct(powerdata$Time, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  }
  
  filepath <- file.path(rootpath, "plot4.png")
  
  if (exists(filepath)) unlink(filepath)
  
  png(filepath)
  par(mfrow = c(2, 2))
  
  plot(powerdata$Time, powerdata$GAP, 
       type = "l", 
       xlab = "", 
       ylab = "Global Active Power")
  
  plot(powerdata$Time, powerdata$Voltage, 
       type = "l", 
       xlab = "datetime", 
       ylab = "Voltage")
  
  plot(powerdata$Time, powerdata$SM_1, 
       type = "l", 
       col = "black", 
       xlab = "", 
       ylab = "Energy sub metering")
  
  lines(powerdata$Time, powerdata$SM_2, 
        type = "l", 
        col = "red")
  
  lines(powerdata$Time, powerdata$SM_3, 
        type = "l", 
        col = "blue")
  
  legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
         col = c("black", "red", "blue"), 
         lty = rep(1, 3), 
         bty = "n")
  
  plot(powerdata$Time, powerdata$GRP, 
       type = "l", 
       lwd = 1, 
       xlab = "datetime",
       ylab = "Global Reactive Power")
  
  dev.off()
  img <- readPNG(filepath)
  grid::grid.raster(img)
}