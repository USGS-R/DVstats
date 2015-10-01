### R code from vignette source 'SevenDay.Rnw'

###################################################
### code chunk number 1: SevenDay.Rnw:20-30
###################################################
# Load the DVstats package and retrieve the data
library(DVstats)
library(dataRetrieval)
library(smwrBase) # needed for screenData and other functions
library(smwrGraphs) # needed to create the graphs
RRVM <- renameNWISColumns(readNWISdv("05484500", "00060", endDate="2012-09-30"))
# The screenData function is useful to review for 
# complete record, default is by calendar year.
with(RRVM, screenData(Date, Flow))
# The record is complete, beginning 1915-04-25


###################################################
### code chunk number 2: SevenDay.Rnw:44-54
###################################################
# Compute the 7-day low flow for each climate year.
RRVM7ClimY <- with(RRVM, 
  dvStat(Flow, Date,
    by=climateYear(Date),
    pre=movingAve,
    STAID="05484500",
    span=7, pos="trailing"))
# print the first and last few rows of the output
head(RRVM7ClimY)
tail(RRVM7ClimY)


###################################################
### code chunk number 3: SevenDay.Rnw:61-68
###################################################
# Extract complete climate year data.
RRVM7ClimY <- subset(RRVM7ClimY, Nobs >= 365)
# print the first and last few rows of the output
head(RRVM7ClimY)
tail(RRVM7ClimY)
# Change the name of the statistic
names(RRVM7ClimY)[4] <- "LowQ7"


###################################################
### code chunk number 4: SevenDay.Rnw:76-80
###################################################
# setSweave is required for the vignette.
setSweave("SevenDay_01", 5, 5)
with(RRVM7ClimY, probPlot(LowQ7, yaxis.log=T, xlabels=5))
graphics.off()


###################################################
### code chunk number 5: SevenDay.Rnw:82-84
###################################################
cat("\\includegraphics{SevenDay_01.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 6: SevenDay.Rnw:90-95
###################################################
# setSweave is required for the vignette.
setSweave("SevenDay_02", 5, 5)
with(RRVM7ClimY, timePlot(Date, LowQ7, 
  Plot=list(what="points")))
graphics.off()


###################################################
### code chunk number 7: SevenDay.Rnw:97-99
###################################################
cat("\\includegraphics{SevenDay_02.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 8: SevenDay.Rnw:108-109
###################################################
with(RRVM7ClimY, table(month(Date, label=TRUE)))


###################################################
### code chunk number 9: SevenDay.Rnw:117-127
###################################################
# Compute the 7-day low flow for each climate year.
RRVM7Summ <- with(RRVM, 
  dvStat(Flow, Date,
    by=seasonYear(Date),
    pre=movingAve,
    STAID="05484500",
    span=7, pos="trailing"))
# print the first and last few rows of the output
head(RRVM7Summ)
tail(RRVM7Summ)


###################################################
### code chunk number 10: SevenDay.Rnw:132-134
###################################################
# Change the name of the statistic
names(RRVM7Summ)[4] <- "LowQ7"


###################################################
### code chunk number 11: SevenDay.Rnw:142-146
###################################################
# setSweave is required for the vignette.
setSweave("SevenDay_03", 5, 5)
with(RRVM7Summ, probPlot(LowQ7, yaxis.log=T, xlabels=5))
graphics.off()


###################################################
### code chunk number 12: SevenDay.Rnw:148-150
###################################################
cat("\\includegraphics{SevenDay_03.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 13: SevenDay.Rnw:156-161
###################################################
# setSweave is required for the vignette.
setSweave("SevenDay_04", 5, 5)
with(RRVM7Summ, timePlot(Date, LowQ7, 
    Plot=list(what="points")))
graphics.off()


###################################################
### code chunk number 14: SevenDay.Rnw:163-165
###################################################
cat("\\includegraphics{SevenDay_04.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 15: SevenDay.Rnw:175-181
###################################################
# The Annual analysis
RRVM7ClimY.frq <- with(RRVM7ClimY, freqAnal(LowQ7, id=Group,
    desc="Annual 7-day Low Flow", STAID="05484500"))
# The Summer analysis
RRVM7Summ.frq <- with(RRVM7Summ, freqAnal(LowQ7, id=Group,
    desc="Summer 7-day Low Flow", STAID="05484500"))


###################################################
### code chunk number 16: SevenDay.Rnw:188-190
###################################################
# The Annual analysis
print(RRVM7ClimY.frq)


###################################################
### code chunk number 17: SevenDay.Rnw:194-196
###################################################
# The Summer analysis
print(RRVM7Summ.frq)


###################################################
### code chunk number 18: SevenDay.Rnw:202-206
###################################################
# The Annual analysis
setSweave("SevenDay_05", 5, 5)
plot(RRVM7ClimY.frq, which="default", set.up=FALSE)
graphics.off()


###################################################
### code chunk number 19: SevenDay.Rnw:208-210
###################################################
cat("\\includegraphics{SevenDay_05.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 20: SevenDay.Rnw:216-220
###################################################
# The Annual analysis
setSweave("SevenDay_06", 5, 5)
plot(RRVM7Summ.frq, which="default", set.up=FALSE)
graphics.off()


###################################################
### code chunk number 21: SevenDay.Rnw:222-224
###################################################
cat("\\includegraphics{SevenDay_06.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 22: SevenDay.Rnw:232-237
###################################################
# The Annual analysis
predict(RRVM7ClimY.frq)
setSweave("SevenDay_07", 5, 5)
plot(RRVM7ClimY.frq, which="default", set.up=FALSE)
graphics.off()


###################################################
### code chunk number 23: SevenDay.Rnw:239-241
###################################################
cat("\\includegraphics{SevenDay_07.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 24: SevenDay.Rnw:247-252
###################################################
# The Summer analysis
predict(RRVM7Summ.frq)
setSweave("SevenDay_08", 5, 5)
plot(RRVM7Summ.frq, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 25: SevenDay.Rnw:254-256
###################################################
cat("\\includegraphics{SevenDay_08.pdf}\n")
cat("\\paragraph{}\n")


