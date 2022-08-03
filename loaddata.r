# Load trades data frame and define some convenience functions and lists

tradesDataframe = read.csv("tradesDataframe.csv")
#print(head(tradesDataframe))

fiveMinuteTimesInHours = round(seq(570, 955, 5)/60, digits=3)
#print(fiveMinuteTimeInHours)

getWaitTimesInSeconds = function(dt, fiveMinuteSlot){
    # Return all the waiting time in seconds for five minute slot on one day
  
    # dt is one of 20140303, 20140304, 20140305, 20140306
  
    # fiveMinuteSlot is the start for a 5 minutes time interval, given in hour
    # from midnight with exactly 3 decimal places-- take it from fiveMinuteTimesInHours
    
    b = (tradesDataframe[, "Date"] == dt) * (tradesDataframe[, "fiveMinuteSlot"] == fiveMinuteSlot)
    y = tradesDataframe[b==1, "timeNumber"]*24*60*60
    return(diff(y))
}
#print(getWaitTimesInSeconds('20140303', fiveMinuteTimeInHours[3]))
