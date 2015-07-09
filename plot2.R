# This script assumes the individual household electric power consumption
# data set zip file exists in the current working directory.

read.data <- function() {
    # Define some constants
    data.set.zip.file.name <- "exdata-data-household_power_consumption.zip"
    data.set.file.name <- "household_power_consumption.txt"
    date.format <- "%d/%m/%Y %H:%M:%S"
    start.date <- "1/2/2007 00:00:00"
    end.date <- "3/2/2007 00:00:00" # Exclusive!
    samples.per.day <- 60 * 24
    
    # Check that individual household electric power consumption data set
    # zip file exists in the current working directory. If not print an error
    # message and exit.
    if (!file.exists(data.set.zip.file.name)) {
        stop(paste("Data set zip file does not exist at ",
                   file.path(getwd(), data.set.zip.file.name),
                   sep = ""))
    }
    
    # Extract data set zip file
    unzip(data.set.zip.file.name)
    
    # Instead of reading the whole data set into memory (which can be
    # a scarce resource), the following code try to estimate the number of
    # rows to skip from the beginning of the file to reach date "2007-02-01"
    # and the number of rows to take just before reaching date "2007-02-03".
    # This is more efficient than using dplyr package. The code assume no time
    # gaps between samples.
    
    # First read a single row
    df1 <- read.table(data.set.file.name,
                      header = TRUE,
                      sep = ";",
                      nrows = 1,
                      na.strings = "?")
    
    # Compute the time difference and multiply by the number of samples per day
    date1 <- strptime(paste(df1$Date, df1$Time), format = date.format)
    date2 <- strptime(start.date, format = date.format)
    
    # Inluding header
    nrows.to.skip <- as.numeric((date2 - date1) * samples.per.day + 1)
    
    # Do the same for end date
    date3 <- strptime(end.date, format = date.format)
    nrows.to.take <- as.numeric((date3 - date2) * samples.per.day)
    
    # Read the needed portion
    df2 <- read.table(data.set.file.name,
                      header = FALSE, # Because we already skipped the header
                      sep = ";",
                      nrows = nrows.to.take,
                      na.strings = "?",
                      skip = nrows.to.skip)
    
    # Put header names back
    names(df2) <- names(df1)
    
    # Add a datatime field
    df2$datetime = strptime(paste(df2$Date, df2$Time), format = date.format)
    
    # Return the cleaned data.frame
    df2[, c("datetime",
            "Global_active_power",
            "Global_reactive_power",
            "Voltage",
            "Sub_metering_1",
            "Sub_metering_2",
            "Sub_metering_3")]
}

data <- read.data()

# Send a copy to the plot2.png output file
png("plot2.png", width=480, height=480, bg = "transparent")

# Plot 2
with(data, {
    plot(datetime,
         Global_active_power,
         type = "l",
         xlab = "",
         ylab = "Global Active Power (kilowatts)")
})

dev.off()
