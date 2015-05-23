# Get a list of times that the sun crosses the given angle above horizon
# starttime and endtime should be POSIXct values
# Uses package "insol" to calculate the solar angle,
# package "functional" to curry the solar angle function,
# and package "rootSolve" to find the crossing times
get_crossings <- function(angle,lat,long,timezone,starttime, endtime) {
    library(functional)
    library(insol)
    library(rootSolve)
    
    # Julian dates, need by insol package. JD is in insol package
    start_jd <- JD(starttime)
    end_jd <- JD(endtime)
    
    # Curry the sunvec function and make a derived function
    curry_sunvec <- Curry(sunvector,latitude = lat, longitude = long,timezone=0)
    
    # Use rootSolve package to find the crossings
    # There must be enough granularity to find them all, so make
    # granularity be 1441 points per day = number of seconds a day
    # (with a bit of noise to jitter it off integer values)
    # Note: sunvec function measures angle from overhead, which is complement
    # of our angle. Also sunvec function returns pairs of values. We only 
    # want the second one, hence [,2] syntax
    # Finally, uniroot function finds zero crossings so we must subtract
    # of the angle we're looking for so that the crossings occur at 0.
    threshold = 90 - angle
    zenith <- function(x) {sunpos(curry_sunvec(x))[,2] - threshold}
    granularity = 1441*(end_jd - start_jd)  # minute granularity
    crossings <- uniroot.all(zenith,c(start_jd,end_jd), n=granularity)
    
    # Need to convert these back to POSIX timestamps from Julian dates
    results <- as.data.frame(JD(crossings, inverse=TRUE))
    
    # Now create a data frame with dates, hhmm (= HH:MM) values, and type
    names(results) <- 'timestamp'
    results$date <- as.Date(results$timestamp, tz=timezone)
    results$hhmm <- format(results$timestamp, format='%H:%M', tz=timezone) # convert to string
    results$hhmm <- as.POSIXct(results$hhmm, format='%H:%M', tz=timezone)  # convert back to POSIX
    
    # Split the data into early and late sections, based on whether the sun
    # rising or falling. Note: this might break down at some angles in some
    # timezones, though I have not yet found such an edge case
    # In case of odd number of values we can't predict whether the exact
    # median will be early or late, so leave it as NA
    if (nrow(results) > 0) { 
        results$partition = NA
        results[results$hhmm < median(results$hhmm),]$partition = 'early'
        results[results$hhmm > median(results$hhmm),]$partition = 'late'
    }
    
    results
    
}