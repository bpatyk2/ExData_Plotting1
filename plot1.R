plot1 <- function()
{
  URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  rootpath <- file.path(".","Plots")
  filename <- "household_power_consumption.zip"
  
  if(!dir.exists(rootpath)) dir.create(rootpath)
  
  filepath <- file.path(rootpath, filename)
  download.file(URL, filepath, 'curl')
  unzip(filepath, exdir = rootpath)
  unlink(filepath)
  
  filepath <- file.path(rootpath,"household_power_consumption.txt")
  con <- file(filepath, "r")
  lines <- c(readLines(con, 1))
  line <- readLines(con, 1)
  
  while(length(line) > 0) 
  {
    if(grepl("2/2/2007", line) || grepl("1/2/2007", line)) lines <- c(lines, line)
    line = readLines(con, 1)
  }
  
  close(con)
  unlink(filepath)
  
  filepath <- file.path(rootpath,"household_power_consumption_Feb.txt")
  con <- file(filepath, "w")
  writeLines(lines, con)
  close(con)
  
  powerdata <- read.delim(filepath, sep = ";")
  powerdata$Date <- as.Date(powerdata$Date, "%d/%m/%Y")
  powerdata <- subset(powerdata, Date <= "2007-02-02" & Date >= "2007-02-01")
  powerdata$Time <- strptime(paste(powerdata$Date, powerdata$Time), "%Y-%m-%d %H:%M")
  
  head(powerdata)
}