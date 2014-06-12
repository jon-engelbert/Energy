library(RJSONIO)
library(RCurl)
library(timeDate)

readElectric = function(filename) {
    el = read.csv(file=filename, skip=14)
    el[ ,"Meter.Number"] = NULL
    el[ ,"Duration"] = NULL
    el[ ,"Flow.Direction"] = NULL
    el[ ,"Edit.Code"] = NULL
    #el$datetime = do.call(paste, c(el[c("Date", "Start.Time")], sep = ""))
    strTimeDate = do.call(paste, c(el[c("Start.Time", "Date")], sep = ""))
    dateTime= as.POSIXct(strptime(strTimeDate, "%H:%M %p%m/%d/%Y"))
    #el["datetime"] = dateTime
    el = cbind(el, dateTime)
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
    gas = cbind(gas, dateTime)
    #dateTime = as.numeric(as.POSIXct(gas["Date"])) #
}
#weather appID: 5582b0b32d7a81805cbce5455ccc8b46
readWeather = function(starttime, endtime, cityid) {
    urlString = sprintf("http://api.openweathermap.org/data/2.5/history/city?id=%s&type=hour&start=%i&end=%i&APPID=5582b0b32d7a81805cbce5455ccc8b46", cityID, starttime, endtime)
    raw_data <- getURL(urlString);
    # Then covert from JSON into a list in R
    data <- fromJSON(raw_data)
    length(data)
    weather_reading_count = length(data$list)
    weather_df <- data.frame()  
    if (weather_reading_count >0) {
        for (i in 1:weather_reading_count) {
            weather_df[i,1] = as.POSIXct(data$list[[i]]$dt, origin="1970-01-01")
            datetime= as.POSIXlt(data$list[[i]]$dt, origin="1970-01-01")
            date= as.Date(datetime)
            datetime= sprintf("%s %s:%s", date, datetime$hour, datetime$min)
            weather_df[i,2] = datetime
            weather_df[i,3] = temp = data$list[[i]]$main[1]
            weather_df[i,4] = clouds =data$list[[i]]$clouds
            weather_df[i,5] = wind= data$list[[i]]$wind[1]
            weather_df[i,6] = humidity = data$list[[i]]$main[5]
            weather_df[i,7] = temp_min = data$list[[i]]$main[3]
            weather_df[i,8] = temp_max = data$list[[i]]$main[4]
            weather_df[i,9] = weather =data$list[[i]]$weather[[1]]$main
        }
        colnames(weather_df)= c('date', 'time', 'temp', 'clouds', 'humidity', 'wind', 'temp_min', 'temp_max', 'weather')
    }
    weather_df
}

fillScheduleInterval = function(firsttime, schedule, starttime, endtime, coolSP, heatSP) {
    time = starttime
    while (time < endtime) {
        scheduleRow = c(time, coolSP, heatSP)
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
    schedule
}

el1 = readElectric("Demo_Electric_60_Minute_09-16-2013_12-14-2013_20140610161633364.csv")
gas1 = readGas("Demo_Gas_60_Minute_09-16-2013_12-14-2013_20140610161655472.csv")
el2 = readElectric("Demo_Electric_60_Minute_12-15-2013_03-11-2014_20140610161512948.csv")
gas2 = readGas("Demo_Gas_60_Minute_12-15-2013_03-12-2014_20140610161351948.csv")
el3 = readElectric("Demo_Electric_60_Minute_03-12-2014_06-09-2014_20140610161228420.csv")
gas3 = readGas("Demo_Gas_60_Minute_03-12-2014_06-09-2014_20140610161238389.csv")
electricAll = rbind(el1, el2, el3)
gasAll = rbind(gas1, gas2, gas3)

# grab the weather data
weather = data.frame()
if (file.exists("sample_weather.rds")){
    weather <- readRDS("sample_weather.rds")
} else {
    starttime = as.POSIXct("2013-09-16 0:00 EST")
    nowdate =  as.POSIXct(Sys.Date())
    while (starttime < nowdate- 4*60*60*24) {
        endtime = starttime + 4*60*60*24
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
}
#add thermostat schedule
schedule = data.frame()
starttime = as.POSIXct("2013-09-16 0:00 EST")
nowtime =  as.POSIXct(Sys.Date())
schedule = setSchedule(weather, starttime, nowtime)
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