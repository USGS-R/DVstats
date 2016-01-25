### R code from vignette source 'MasterRecessionCurve.Rnw'

###################################################
### code chunk number 1: MasterRecessionCurve.Rnw:32-38
###################################################
# Load the smwrData, and smwrGraphs (used for graphing) packages
library(smwrData)
# Get the dataset
data(GlacialRidge)
library(smwrGraphs)
library(DVstats)


###################################################
### code chunk number 2: MasterRecessionCurve.Rnw:46-62
###################################################
# Compute the recessions
G12.fall <- with(GlacialRidge, fall(G12, datetime, MPelev=1126.42, STAID="G12"))
# Plot the recessions, setSweave required for this vignette
setSweave("plot01", 6, 6)
plot(G12.fall, set.up=FALSE)
# Required call to close PDF output graphics
graphics.off()
# None of the recessions look drastically out of place--accept all
# Note that recession number 36 does look a bit steep relative to the
# others near it, but not terribly out of place
# The confirm function is used to accept/reject recessions
G12.fall <- confirm(G12.fall, all=TRUE)
print(G12.fall)
# Extract and print the recession data
G12.rec <- G12.fall$Recessions
G12.rec


###################################################
### code chunk number 3: MasterRecessionCurve.Rnw:73-88
###################################################
# Build the regression model, Retain elevations rather than log as eqn 4
# Compute the minimum water level to use as starting value for d
MinWL <- min(G12.rec$EndWL)
G12.nls <- nls(EndWL ~ 
    exp(log(StartWL - d) + RR*Length) + d, data=G12.rec, start=c(d=MinWL-.5, RR=-.01))
G12.nls
# Plot the actual recessions and estimated recessions
# setSweave required for this vignette
setSweave("plot02", 6, 6)
plot(G12.fall, set.up=FALSE)
# Add the segments, setting line weights
with(G12.rec, segments(0, StartWL, Length, fitted(G12.nls), col="red",
   lwd=stdWt()))
# Required call to close PDF output graphics
graphics.off()


###################################################
### code chunk number 4: MasterRecessionCurve.Rnw:99-109
###################################################
# Set up the graphics device
setSweave("plot03", 6, 6)
# Construct a dataset for the estimations
G12.mrc <- data.frame(StartWL=1120.65, Length=0:100)
G12.mrc$EndWL <- predict(G12.nls, G12.mrc)
# Plot it
with(G12.mrc, xyPlot(Length, EndWL, Plot=list(what="lines", color="red"),
  xtitle="Recession duration, in days", ytitle="Groundwater Level"))
# Required call to close PDF output graphics
graphics.off()


###################################################
### code chunk number 5: MasterRecessionCurve.Rnw:123-153
###################################################
# First extract the recession identified by fall
# The extraction function for objects of class "fall" only accepts
# a single index value, hence requiring this unusual expression
G12.all <- do.call(rbind, lapply(seq(nrow(G12.rec)), function(i) G12.fall[i]))
# The ddply function in plyr is a convenient way to aggregate data
library(plyr)
# For this example, use a period of 3 days--StartWL is the midrange of the water level;
# del is the decrease in the water level, in units of per day; and Len is the
# length of the period--required to remove periods of less than 3 days
G12.agg <- ddply(G12.all, .(Seqno %/% 3, Index), summarize, 
  StartWL=sum(range(GWLevel))/2, del=diff(range(GWLevel))/2, Len=length(GWLevel))
G12.agg <- subset(G12.agg, Len == 3)
# Use loess to construct a smooth curve to describe the recession rate for
# any given starting elevation. The parameters of the loess fit must be set
# so that the result is a continuously increasing function
G12.lo <- loess(del ~ StartWL, data=G12.agg, degree=1, span=1,
  control=loess.control(surface = "direct"))
# Print the model and build a dataset of the alternate recession curve
G12.lo
G12.arc <- data.frame(StartWL=seq(1119,1120.65, by=.01))
G12.arc$del <- predict(G12.lo, G12.arc)
# Graph the data, the smooth recession curve and the master recession curve
setSweave("plot04", 5, 5)
with(G12.agg, xyPlot(StartWL, del, ytitle="Recession Rate"))
with(G12.arc, addXY(StartWL, del, Plot=list(what="lines")))
# the sense of del is opposite sign in the mrc
with(G12.mrc, addXY(EndWL[-length(EndWL)], -diff(EndWL),
  Plot=list(what="lines", color="red")))
# Required call to close PDF output graphics
graphics.off()


