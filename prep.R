# This the code used to convert the original NASDAQ data to dataframe.csv

# tradesdata.csv has data for all NASDAQ trades of a stock for 4 days in March 2014

# Column            |      Description
#--------------------------------------------------
# Date              | Date of trades in YYYYMMDD format
# timeNumber        | time of trade as fraction of 24-hour period (0.5 = 12 noon)
# timeHHMMSS        | time of trade as an integer in HHMMSS format
# Size              | trade size in share
# Price             | trade price (volume-weighted over all trade records at  same timeNumber)

# We start with some exporatory data analysis
#load data----------------------
tradesdata = read.table(file = "studio6/tradesdata0.csv", sep = ',', header = TRUE)
# Take a peak -- note time is given as a decimal = fraction 24-hour and as HHMMSS
print(head(tradesdata))
print(dim(tradesdata))

# Plot price verse trade index(roughly time) 
plottitle = paste(c("Trade Prices", "Dates: 3/3, 3/4, 3/5, 3/6 in 2014"))
plot(tradesdata[, 'Price'], type = 'l', col = "blue", xlab = "Trade index", ylab = "Price",
     main = plottitle)

# Add vertical line at beginning of each Date
list.Dates = unique(tradesdata[, 'Date'])
for (dt in list.Dates){
  abline(v=sum(tradesdata[, 'Date'] < dt))
}

# Condense the data into 5 minute chunks----
tradesdata.timeColumn = tradesdata[, 'timeNumber']
tinyt = 1/(24*60*60*2)
x = floor((tradesdata.timeColumn+tinyt)*24*12)/12
tradeTimes5minute = round(x, digits = 3)
print(max(tradeTimes5minute))

# We combine all 4 days to get a table of count in 5 minute interval
tradeTimes5minute.cumtable = table(tradeTimes5minute)
head(tradeTimes5minute.cumtable)
plot(tradeTimes5minute.cumtable, xlab = "Time in hour", ylab = "Number of trades",
     main = "Trade counts in 5 minute periode (all days combined)", cex.main = .9)

# More trades at beginning and at end of the day than in middle.
# Note 9.5 = 9:30 am and 16.0 = 4 pm

# Explore individual days
# Make a table: rows = 5 minute periode, cols = Dates, entries = # of trades
tradeTimes5minute.daytable = table(tradeTimes5minute, tradesdata[,"Date"])
print(tradeTimes5minute.daytable)

# Make a plot for export
n = nrow(tradeTimes5minute.daytable)
cols =c(rep('orange', n), rep('blue', n), rep('green', n), rep('red', n))
barplot(tradeTimes5minute.daytable, beside = TRUE, 
        col = cols, cex.main = 1.2, space = c(0,1), border = NA, cex.names = 1,
        names.arg = c("3/3/2014", "3/4/2014", "3/5/2014", "3/6/2014"),
        main = "Table of trade counts in 5 minute periods by date")

# More trades at beginning and end of each day

# Create a csv file holding the dataframe for analysis in studio6_review
tradesDataframe = data.frame(Date = tradesdata[, 'Date'],
                       fiveMinuteSlot = tradeTimes5minute,
                       timeNumber = tradesdata.timeColumn)

write.csv(tradesDataframe, "tradesDataframe.csv")