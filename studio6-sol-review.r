# studio6-sol-review.r --- Bayes updating from tradesDataframe.csv
# load tradesDataframe
source('loaddata.r')

# This define:
# 1- tradesDataframe --- data set for project
# 2- fiveMinuteTimesInHours --- a list of all the start times for the 5 minute slots
# 3- getWaitTimesInSeconds --- a function that returns as list all the waiting times in seconds for 
# one five minute slot on one day

# Update one day/5 minute slot -----
# Once we discretize theta the updating is just like as discrete updating
dt = '20140304' # date = 4 March
t = fiveMinuteTimeInHours[8] # choose an arbitrary time slot
currentData = getWaitTimesInSeconds(dt, t) # get the data for this date/slot time

# Discretize theta --- start range at theta because of 1/dtheta
dtheta = 0.02
thetaRange = seq(dtheta, 8, dtheta)
# Uniform prior (function of theta)
unnormalizedUnifirmPrior = rep(1, length(thetaRange))
# To normalized we want sum(prior*dthete) = 1
uniformPrior = unnormalizedUnifirmPrior/(dtheta*sum(unnormalizedUnifirmPrior))

originalPrior = uniformPrior

# Initialize a matrix whose jth column store the posterior distribution after the jth updating
# This help to plot data easilly
ndata = length(currentData)
posteriorMat = matrix(NA, nrow = length(thetaRange), ncol = ndata)

# Update by looping over data
prior = originalPrior
for (idx in 1:ndata) {
  waitTime = currentData[idx]
  likelihood = exp(-waitTime/thetaRange)/thetaRange
  unnormalizedPosterior = prior * likelihood
  normalizedPosterior = unnormalizedPosterior/(dtheta*sum(unnormalizedPosterior))
  posteriorMat[, idx] = normalizedPosterior
  
  prior = normalizedPosterior
}

# Find the MAP estimate(thata value that maximizes posteriori)
# R function which.max returns the index of maximum in a list passed as argument
k = which.max(normalizedPosterior)
MAPestimate = thetaRange[k]
print(MAPestimate)

# Plot all the posterior on one graph
thetaMin = 0
thetaMax = max(thetaRange)
ymin = 0
ymax = max(posteriorMat, originalPrior)

# Create an empty plot withe the correct range
s = paste("Maech 4, 2014 at ", t, "hours" )
plottitle = paste(c("Plot of all posterior (and Prior) ", s))
plot(c(thetaMin, thetaMax), c(ymin, ymax), type = 'n', xlab = "theta", ylab = "pdf", main = plottitle)
lines(thetaRange, originalPrior, col = 'red')
for (j in 1:ndata) {
  # R let us to cycle through its list of color
  lines(thetaRange, posteriorMat[, j], col = j)
}

# Now we repeat the above for each slot time
# We only save the final posterior for each slot time

# Once we discretize theta the updating code is juat like the discrete updating 
# dt = "20140305"
timeSlots = fiveMinuteTimesInHours
nslots = length(timeSlots)

# Discretize theta --- start with dtheta because of 1/theta
dtheta = 0.02
thetaRange = seq(dtheta, 8, dtheta) # hypothesis
# Uniform prior (founction of theta)
unnormalizedUnifirmPrior = rep(1, length(thetaRange))
# To normalize we want sum(dtheta*prior) = 1
uniformPrior = unnormalizedUnifirmPrior/(dtheta*sum(unnormalizedUnifirmPrior))

# Quadratic prior (function of theta)
unnormalizedQuadraticPrior = (4-thetaRange)^2
# To normalize we want sum(dtheta*prior) = 1
quadraticPrior = unnormalizedQuadraticPrior/(dtheta*sum(unnormalizedQuadraticPrior))

# Set commenting to indicate the prior you choose
#originalPrior = uniformPrior
originalPrior  = quadraticPrior

# We initialize a matrix whose jth column stores the final posterior distribution for jth time slot
# We will use this for analysis an plotting at the end
posteriorMat = matrix(NA, nrow = length(thetaRange), ncol = nslots)

# loop over each day
dates = unique(tradesDataframe[,'Date'])
for (dt in dates) {
  # Loop over each time slot
  for (idxSlot in 1:nslots) {
    t = timeSlots[idxSlot]
    currentData = getWaitTimesInSeconds(dt, t)
    prior = originalPrior
    
    # Update by loopping over data
    ndata = length(currentData)
    for (idxData in 1:ndata) {
      waitTime = currentData[idxData]
      likelihood = exp(-waitTime/thetaRange)/(thetaRange)
      unnormalizedPosterior = likelihood * prior
      normalizedPosterior = unnormalizedPosterior/(dtheta*sum(unnormalizedPosterior))
      
      prior = normalizedPosterior
    }
    
    posteriorMat[, idxSlot] = normalizedPosterior
  }
  
  # Analysis an plotting
  
  # Find MAp estimate (theta that maximizes the posterior distribution)
  # R function which.max returns the max index of list given as argument
  MAPestimates = rep(0, nslots)
  for (j in 1:nslots) {
    k = which.max(posteriorMat[, j])
    MAPestimates[j] = thetaRange[k]
  }
  plottitle = paste("Date = ", dt, ", MAP estimate for theta")
  plot(timeSlots, MAPestimates, type = 'l', main=plottitle, cex.main = 1, col = 'blue', lwd = 3,
       xlab = "time(hours from midnight)", ylab = "MAP")
  
  # This plot shows that the mean waiting time between trades is bimodal, e.i trades are processed
  # more fastly at beginning and end than at midle of the day.
  
  # Fit curve to noisy data
  fit = lm(MAPestimates ~ poly(timeSlots, 2))
  lines(timeSlots, predict(fit, data.frame(x=timeSlots)), col = 'orange', lwd = 2)
}


