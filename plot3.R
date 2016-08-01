
# This program plots a line graph of energy submetering in watt-hours by over
#   time covering the days from 1st February 2007 to 2nd February 2007.


library(dplyr)

downdir <- "./downloads"
rawdir <- "./raw"

getRaw <- function(ddir, xdir, force = FALSE) {
  
  # set file and path names
  zurl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
  fname <- "HPCdata.zip"
  
  # check path(s)
  if (!file.exists(ddir)) {
    dir.create(ddir)
  }
  
  # get file 
  fpath <- file.path(ddir,fname)
  if (force || !file.exists(fpath)) {
    download.file(zurl, fpath)
    unzip(fpath, exdir = xdir)
  }
  
}

getRaw(downdir, rawdir)

# ddir <- file.path(rawdir, "")
ddir <- rawdir

if (!exists("xdf") || !all.equal(dim(xdf), c(2880,10))) {
  fname <- "household_power_consumption.txt"
  fpath <- file.path(ddir,fname)
  rawdf <- read.csv(fpath, sep = ";", na.strings = "?", # nrows = 100,
                    colClasses = c(rep("character",2),rep("numeric",7)))
  
  start <- as.POSIXct("2007-02-01", tz = "UTC")
  end <- as.POSIXct("2007-02-03", tz = "UTC") # the next day!
  
  xdf <- tbl_df(rawdf) %>%
    # sample_n(1000) %>%
    mutate(datetime = as.POSIXct(strptime(paste(Date, Time), "%d/%m/%Y %H:%M:%S", tz = "UTC"))) %>%
    filter(datetime >= start & datetime < end)
}

png(file = "plot3.png", width = 480, height = 480)
plot(Sub_metering_1~datetime, xdf, type = "l", xlab = "", ylab = "Energy sub metering")
points(Sub_metering_2~datetime, xdf, type = "l", col = "red")
points(Sub_metering_3~datetime, xdf, type = "l", col = "blue")
legend("topright", paste("Sub_metering_",1:3,sep=""),lty=rep(1,3),col=c(1,"red","blue"),
       bty = "o")
dev.off()
