library(RJSONIO)
library(RCurl)
library(timeDate)


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
        #time zone relative to utc
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
            datetimePrev = as.POSIXlt(data$list[[i]]$dt, origin="1970-01-01", tz = "EST")
            datetimeNext = as.POSIXlt(data$list[[i+1]]$dt, origin="1970-01-01", tz = "EST")
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
        colnames(weather_df)= c('datetime', 'time', 'temp', 'clouds', 'humidity', 'wind', 'temp_min', 'temp_max')
    }
    weather_df
}

readElectric = function(filename) {
    el = read.csv(file=filename, skip=14)
    el[ ,"Meter.Number"] = NULL
    el[ ,"Duration"] = NULL
    el[ ,"Flow.Direction"] = NULL
    el[ ,"Edit.Code"] = NULL
    #el$datetime = do.call(paste, c(el[c("Date", "Start.Time")], sep = ""))
    strTimeDate = do.call(paste, c(el[c("Start.Time", "Date")], sep = " "))
    dateTime= as.POSIXct(strptime(strTimeDate, "%I:%M %p%m/%d/%Y"))
    #el["datetime"] = dateTime
    el = cbind(el, as.numeric(dateTime))
    colnames(el)= c('start.time', 'date', 'watt.hours',  'datetime')
    el
}
readGas = function(filename) {
    gas = read.csv(file=filename, skip=14)
    gas[ ,"Meter.Number"] = NULL
    gas[ ,"Duration"] = NULL
    gas[ ,"Flow.Direction"] = NULL
    gas = gas[gas$Edit.Code != "Missing",]
    gas[ ,"Edit.Code"] = NULL
    strTimeDate = do.call(paste, c(gas[c("Start.Time", "Date")], sep = ""))
    dateTime= strptime(strTimeDate, "%H:%M %p%m/%d/%Y")
    #gas["datetime"] = dateTime
    gas = cbind(gas, as.numeric(dateTime))
    colnames(gas)= c('start.time', 'date', 'btus',  'datetime')
    #dateTime = as.numeric(as.POSIXct(gas["Date"])) #
    gas
}


fillScheduleInterval = function(firsttime, schedule, starttime, endtime, coolSP, heatSP) {
    time = starttime
    while (time < endtime) {
        scheduleRow = c(time, (coolSP - 32) * 5 / 9 + 273.15, (heatSP - 32) * 5 / 9 + 273.15)
        i = 1+difftime(time, firsttime, units="hours") 
        schedule[i,] = scheduleRow
        time = time+ 60*60
    }
    schedule
}

setSchedule = function(weather, starttime, endtime) {
#find the day of week for startTime
    sleepCool = 74
    sleepHeat= 64
    presentAmCool = 76
    presentAmHeat= 68
    awayCool = 78
    awayHeat= 63
    presentPmCool = 76
    presentPmHeat= 66
    timeWake = 6*60*60
    timeLeave = 8*60*60
    timeReturn = 18*60*60
    timeSleep = 22*60*60
    # on weekends, only wake & sleep settings
    N = (as.Date(nowtime)-as.Date(starttime))*24
    schedule = data.frame(num=rep(0, N), num=rep(0, N), num=rep(0,N))
    
    # for a weekday, for each weather time slot, add heat & cool setpoints
    time= starttime
    while (time+ timeWake < endtime) {
        schedule = fillScheduleInterval(starttime, schedule, time, time + timeWake, sleepCool, sleepHeat)
        #time zone relative to utc
        dayOfWeek = weekdays(as.Date(time+timeWake)) 
        if ((dayOfWeek != "Saturday") && (dayOfWeek != "Sunday")) {
            schedule = fillScheduleInterval(starttime, schedule, time +timeWake, time +timeLeave, presentAmCool, presentAmHeat)
            schedule = fillScheduleInterval(starttime, schedule, time +timeLeave, time +timeReturn, awayCool, awayHeat)
            schedule = fillScheduleInterval(starttime, schedule, time +timeReturn, time +timeSleep, presentPmCool, presentPmHeat)
        } else {
            schedule = fillScheduleInterval(starttime, schedule, time +timeWake, time +timeSleep, presentAmCool, presentAmHeat)            
        }
        schedule = fillScheduleInterval(starttime, schedule, time +timeSleep, time+ (24*3600), sleepCool, sleepHeat)
        time = time+ (24*3600)
    }
    if (time < endtime) {
        schedule = fillScheduleInterval(starttime, schedule, time, endtime, sleepCool, sleepHeat)
        
    }
    colnames(schedule)= c('datetime', 'cool', 'heat')
    schedule
}

el1 = readElectric("Demo_Electric_60_Minute_09-16-2013_12-14-2013_20140610161633364.csv")
gas1 = readGas("Demo_Gas_60_Minute_09-16-2013_12-14-2013_20140610161655472.csv")
el2 = readElectric("Demo_Electric_60_Minute_12-15-2013_03-11-2014_20140610161512948.csv")
gas2 = readGas("Demo_Gas_60_Minute_12-15-2013_03-12-2014_20140610161351948.csv")
el3 = readElectric("Demo_Electric_60_Minute_03-12-2014_06-09-2014_20140610161228420.csv")
gas3 = readGas("Demo_Gas_60_Minute_03-12-2014_06-09-2014_20140610161238389.csv")
el = readElectric("Demo_Electric_60_Minute_05-16-2014_06-15-2014_20140616153325085.csv")
electricAll = el #rbind(el1, el2, el3)
gasAll = rbind(gas1, gas2, gas3)

# grab the weather data
weather = data.frame()
starttime = as.POSIXct("2014-05-16 0:00 UTC")
starttime_weather = starttime
if (file.exists("sample_weather.rds")){
    weather <- readRDS("sample_weather.rds")
} else {
    nowdate =  as.POSIXct(Sys.Date())
    while (starttime_weather < nowdate- 4*60*60*24) {
        endtime = starttime_weather + 4*60*60*24
        cityID = '4984247'
        print(starttime_weather)
        weather_part = readWeather(starttime_weather, endtime, cityID)
        if (nrow(weather_part) > 0) {
            if (is.null(weather))
                weather = weather_part
            else
                weather = rbind(weather,weather_part)
            starttime_weather = endtime
        } else
            starttime_weather = endtime+ 60*60
    }
    saveRDS(weather, file="sample_weather.rds")
}
#add thermostat schedule
schedule = data.frame()
nowtime =  as.POSIXct(Sys.Date())
schedule = setSchedule(weather, starttime, nowtime)
weather_sched <- merge(weather, schedule, by="datetime")
weather_sched_el <- merge(weather_sched, electricAll, by="datetime")
mean_el = mean(weather_sched_el$watt.hours)
el_ac =  ((weather_sched_el$watt.hours > 2*mean_el) & (weather_sched_el$temp+5 >= weather_sched_el$cool)) * (weather_sched_el$watt.hours - mean_el)
write.csv(el, file="electric_usage")

# while (starttime < nowdate- 60*60*24) {
#     endtime = starttime + 60*60*24
# 
#     schedule_part = setSchedule(schedule, weather, starttime, endtime)
#     if (nrow(weather_part) > 0) {
#         if (is.null(schedule))
#             schedule = schedule_part
#         else
#             schedule = rbind(schedule,schedule_part)
#         starttime = endtime
#     } else
#         starttime = endtime+ 60*60
#     colnames(schedule)= c('time', 'heat', 'cool')
#     #cbind(weather, schedule)
# }

#: http://openweathermap.org/data/2.3/forecast/city?id=524901&APPID=5582b0b32d7a81805cbce5455ccc8b46