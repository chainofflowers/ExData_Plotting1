## This is plot2.R, part of the assignment for the CourseProject 1 of the
## Exploratory Data Analysis course.
plot2 <- function() {
    # our first and last date:
    start_day <- as.Date('2007-02-01','%Y-%m-%d')
    end_day   <- as.Date('2007-02-02','%Y-%m-%d')
    
    # filename:
    FNAME='household_power_consumption.txt'
    
    # read the file line by line, to only get the
    # observations whose date is between 2007-02-01 and 2007-02-02.
    # NOTE: although building a dataframe this way might seem signicantly
    #       slower than reading the file all at once and then filtering,
    #       it is in reality not true, because the file is not read enterely:
    #       it is ORDERED (in ascending order) per date, so we can scan all
    #       the lines from the beginning, skipping the ones whose date is
    #       less than Feb 1st 2007, read all the following lines until we
    #       hit a date greater than Feb 2nd 2007 and then STOP.
    #       And this saves time on machines with a little memory.
    
    if (!file.exists(FNAME)) {
        stop(paste("Could not find", FNAME))
    }
    
    fCon <- file(FNAME,"r")                    # file connection
    
    # read the file header, note that this time ok=FALSE because we expect
    # readLines to return something:
    thisLine <- readLines(fCon, n=1, ok=FALSE)  
    
    # define the data frame:
    df <- data.frame(c(list(double(0)),        # to store big values for POSIXct
                       rep(list(numeric(0)),7)), # 7 numeric variables
                     stringsAsFactors=FALSE
    )
    colnames(df) <- c("datetime", strsplit(thisLine,';')[[1]][3:9])
    
    # read observations looping over the remaining lines:
    while (length(thisLine <- readLines(fCon,n=1, ok=TRUE)) >0 )
    {
        splitLine <- strsplit(thisLine, split=';')
        observDay <- as.Date(splitLine[[1]][1],'%d/%m/%Y') # get the date
        
        # check if the day of this observation belongs to the wanted time range
        if (observDay >= start_day) {
            break
        }
    }
    
    # here we have in splitLine the first observation to put into our dataset.
    repeat {
        # add this observation to our dataset:
        df[nrow(df)+1,] <-
            c(list(as.POSIXct(
                strptime(paste(splitLine[[1]][1],       # compute datetime
                               splitLine[[1]][2]), 
                         format = '%d/%m/%Y %H:%M:%S'))),
              as.numeric(splitLine[[1]][3:length(splitLine[[1]])])
            )

        thisLine <- readLines(fCon,n=1, ok=TRUE)           # read next line
        if (length(thisLine) == 0) {
            # we have reached the end of file
            break
        }
        
        splitLine <- strsplit(thisLine, split=';')
        observDay <- as.Date(splitLine[[1]][1],'%d/%m/%Y') # get the date
        if (observDay > end_day) {
            # we have just passed the last observation for our time frame
            break 
        }
    }
    close(fCon)                                # not needed any more

    # open a PNG graphic device for plotting:
    png(filename = 'plot2.png',width = 480, height = 480)
    
    # for simplicity in the next plot code, global parameters are changed.    
    # save the previous values of global parameters:
    previous_par <- par("cex","bg")
    
    # use slightly smaller label size to have a graph similar to the template
    # of the Course Project. The value was found empirically, I don't know
    # if it is platform-dependent. I use R version 3.1.2 (2014-10-31) on 
    # Linux/amd64.
    par(cex=0.95)
    
    # set transparent background, as in the template of the Course Project:
    par(bg="transparent")
    
    # plot data on the active device (the one just created:)
    plot(as.POSIXct(df$datetime,              # values on the X-axis
                    origin=as.Date('1970-01-01',
                                   '%Y-%m-%d')),
         df$Global_active_power,              # values on the Y-axis
         type='l',                            # line
         xlab='',                             # no X-axis description
         ylab="Global Active Power (kilowatts)"
    )
    
    # close (current) device (the PNG file just opened); 
    dev.off()
    
    # reset the global graphic parameters to the original values:
    par(previous_par)
}