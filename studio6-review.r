# Load data and show some analysis

# Load data and test things out---
source("loaddata.r")

# This define:
# 1- tradesDataframe --- csv file that stores trades data
# 2- fiveTimesInHours -- a list of all the start time for the 5 minute slots
# 3 -getWaitTimesInSeconds -- a function that returns a list of all the wait times in second for 
# one five minute slot on one day

teststudio6 = function(){
  print(head(fiveMinuteTimeInHours))
  print(head(tradesDataframe))
  x = getWaitTimesInSeconds("20140303", fiveMinuteTimeInHours[6])
  print(head(x))
  
  opar = par("mfrow"= c(2, 1))
  plot(x, main = "Times between trades", ylab = "Time in seconds")
  hist(x, breaks = 0:12, main = "Histogram of time between trades", xlab = "Time in seconds")
  par(opar)
}

teststudio6()

# Explore the hypothesis the waiting times in 5 minute slots are exponential ---
# by plotting histograms
print(length(fiveMinuteTimeInHours))

dt= '20140303'
t = fiveMinuteTimeInHours[1]
x = getWaitTimesInSeconds(dt, t)
opar = par("mfrow"=c(2, 1))
plottitle = paste("Times between trades: ", dt, ", t = ", t, sep = " ")
plot(x, main = plottitle, xlab = "Time is seconds")
histtitle = paste("Histogram for: ", dt, ", t = ", t, sep = " ")
breaks = 0:(floor(max(x)+1))
hist(x, breaks = breaks, main = histtitle, xlab = "Time is seconds")
par(opar)

# Plot of data doesn't set off alarms
# Histogram resembles that of an exponential distribution

# Do a bunch at once
for (n in c(10, 25, 35, 40, 50, 60, 70, 77, 78)){
  t = fiveMinuteTimeInHours[n]
  x = getWaitTimesInSeconds(dt, t)
  opar = par("mfrow"=c(2, 1))
  plottitle = paste("Times between trades: ", dt, ", t = ", t, sep = " ")
  plot(x, main = plottitle, col = "blue", xlab = "Time in seconds")
  
  breaks = 0:(floor(max(x)+1))
  histtitle = paste("Histogram for ", dt, ", t = ", t)
  hist(x, breaks = breaks, col = "orange", main = histtitle, xlab = "Time in seconds")
  par(opar)
}