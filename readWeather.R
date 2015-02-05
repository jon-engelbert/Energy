library(RJSONIO)
library(RCurl)
library(timeDate)
library(lubridate)

# datetime is in POSIX format
# index and datetimeStartRound are the next row index to be added to weather (data.frame) and starting time for next row in weather
interpolateWeather = function(index, weather_df, datetimeStart, datetimePrev, datetimeNext, tempPrev, tempNext, tempMinPrev, tempMinNext, tempMaxPrev, tempMaxNext, windPrev, windNext, humidPrev, humidNext, cloudPrev, cloudNext) {
    #if times are outside bounds then skip
    datetimeEnd = datetimeStart + 3600
    if (difftime(datetimeNext, datetimeStart, units = "secs") < 0 || difftime(datetimePrev, datetimeEnd, units = "secs") > 0) {
        return            
    }
    #if both times are inside bounds then skip
    if (difftime(datetimePrev, datetimeStart, units = "secs") > 0 && difftime(datetimeNext, datetimeEnd, units = "secs") < 0) {
        return        
    }
    datetimeCurr =datetimeStart
#    tcurr.lub <- ymd_hms(datetimeCurr)
#    tcurr.str <- strptime(datetimeCurr, "%Y-%m-%d %H:%M:%S")
#    tnext.lub <- ymd_hms(datetimeNext)
#    tnext.str <- strptime(datetimeNext, "%Y-%m-%d %H:%M:%S")
#    tprev.lub <- ymd_hms(datetimePrev)
#    tprev.str <- strptime(datetimePrev, "%Y-%m-%d %H:%M:%S")
    while (datetimeCurr < datetimeNext) {
        if (index >= 39) {
            num= as.numeric(difftime(datetimeNext, datetimeCurr, units = "secs"))
            den= as.numeric(difftime(datetimeNext, datetimePrev, units = "secs"))
        }
        ratio = as.numeric(difftime(datetimeNext, datetimeCurr, units = "secs")) / as.numeric(difftime(datetimeNext, datetimePrev, units = "secs"))
        weather_df[index,1] = as.integer(datetimeCurr)
        dateCurr = as.Date(datetimeCurr)
        #strDatetime= sprintf("%s", dateCurr)
        strDatetime= sprintf("%s %s:%s", dateCurr, as.POSIXlt(datetimeCurr)$hour, as.POSIXlt(datetimeCurr)$min)
        weather_df[index,2] = strDatetime        
        tempRound = tempPrev + (tempNext - tempPrev) * ratio
        weather_df[index,3] = tempRound
        if (ratio < 0.5) 
            weather_df[index,4] = cloudPrev
        else 
            weather_df[index,4] = cloudNext
        
        humidityRound = humidPrev+ (humidNext - humidPrev)  * ratio
        weather_df[index,5] = humidityRound
        windRound = windPrev+ (windNext - windPrev)  * ratio
        weather_df[index,6] = windRound
        temp_min = tempMinPrev + ratio * (tempMinNext - tempMinPrev)
        temp_max = tempMaxPrev + ratio * (tempMaxNext - tempMaxPrev)
        weather_df[index,7] = temp_min
        weather_df[index,8] = temp_max        
        
        index = index + 1

        datetimeCurr = datetimeCurr + 3600
        tcurr.str <- strptime(datetimeCurr, "%Y-%m-%d %H:%M:%S")
    }
    weather_df
}

readWeather = function(starttime, endtime, cityid) {
    datetimeCurr = starttime
    starttime = starttime - 3600*5
    endtime = endtime + 3600*3
    urlString = sprintf("http://api.openweathermap.org/data/2.5/history/city?id=%s&type=hour&start=%i&end=%i&APPID=5582b0b32d7a81805cbce5455ccc8b46", cityID, starttime, endtime)
    raw_data <- getURL(urlString);
    # Then covert from JSON into a list in R
    data <- fromJSON(raw_data)
    length(data$list)
    weather_reading_count = length(data$list)
    weather_df <- data.frame()  
    datetimePrev= as.integer(starttime)-3600
    if (weather_reading_count >0) {
        for (i in 1:(weather_reading_count-1)) {
            #weather_df, index =
            index = nrow(weather_df)+1
            datetimePrev = as.POSIXlt(data$list[[i]]$dt, origin="1970-01-01")
            datetimeNext = as.POSIXlt(data$list[[i+1]]$dt, origin="1970-01-01")
            weather_df = interpolateWeather(index, weather_df, 
                                            datetimeCurr, 
                                            datetimePrev, 
                                            datetimeNext, 
                                            data$list[[i]]$main['temp'], 
                                            data$list[[i+1]]$main['temp'], 
                                            data$list[[i]]$main['temp_min'],
                                            data$list[[i+1]]$main['temp_min'],
                                            data$list[[i]]$main['temp_max'],
                                            data$list[[i+1]]$main['temp_max'],
                                            data$list[[i]]$wind['speed'], 
                                            data$list[[i+1]]$wind['speed'], 
                                            data$list[[i]]$main['humidity'], 
                                            data$list[[i+1]]$main['humidity'], 
                                            data$list[[i]]$clouds, 
                                            data$list[[i+1]]$clouds)
            while (index < nrow(weather_df)+1) {
                index = index+1
                datetimeCurr = datetimeCurr + 3600            
            }
         }
        colnames(weather_df)= c('date', 'time', 'temp', 'clouds', 'humidity', 'wind', 'temp_min', 'temp_max')
    }
    weather_df
}

# grab the weather data
weather = data.frame()
starttime = as.POSIXct("2014-05-16 0:00 EST")
nowdate =  as.POSIXct(Sys.Date())
while (starttime < nowdate- 3*60*60*24) {
    endtime = starttime + 3*60*60*24
    cityID = '4984247'
    print(starttime)
    weather_part = readWeather(starttime, endtime, cityID)
    if (nrow(weather_part) > 0) {
        if (is.null(weather))
            weather = weather_part
        else
            weather = rbind(weather,weather_part)
        starttime = endtime
    } else
        starttime = endtime+ 60*60
}
saveRDS(weather, file="sample_weather.rds")
write.csv(weather, file='weather.csv')

