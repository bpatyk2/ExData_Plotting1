plot1 <- function()
{
  URL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  filepath <- file.path(".","Plots")
  filename <- "household_power_consumption.zip"
  if(!dir.exists(filepath)) dir.create(filepath)
  download.file(URL, file.path(filepath, filename), 'curl')
  unzip(file.path(filepath,filename), exdir = filepath)
  unlink(file.path(filepath,filename))
  newpath <- file.path(filepath,"household_power_consumption.txt")
  
  con <- file(newpath, "r")
  lines <- c(readLines(con, 1))
  
  while(TRUE) 
  {
    line = readLines(con, 1)
    if(length(line) == 0) break
    else if(grepl("2/2/2007", line) || grepl("1/2/2007", line)) lines <- c(lines, line)
  }
  
  close(con)
  newpath <- file.path(filepath,"household_power_consumption_Feb.txt")
  con <- file(newpath, "w")
  writeLines(lines, con)
  close(con)
  
  dataf <- read.delim(newpath, sep = ";")
  dataf$Date <- as.Date(dataf$Date, "%d/%m/%Y")
  dataf <- subset(dataf, Date <= "2007-02-02" & Date >= "2007-02-01")
  dataf$Time <- strptime(paste(dataf$Date,dataf$Time), "%Y-%m-%d %H:%M")
  head(dataf)
}