### R code from vignette source 'SevenDay.Rnw'

###################################################
### code chunk number 1: SevenDay.Rnw:31-41
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
### code chunk number 2: SevenDay.Rnw:55-65
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
### code chunk number 3: SevenDay.Rnw:72-79
###################################################
# Extract complete climate year data.
RRVM7ClimY <- subset(RRVM7ClimY, Nobs >= 365)
# print the first and last few rows of the output
head(RRVM7ClimY)
tail(RRVM7ClimY)
# Change the name of the statistic
names(RRVM7ClimY)[4] <- "LowQ7"


###################################################
### code chunk number 4: SevenDay.Rnw:87-91
###################################################
# setSweave is required for the vignette.
setSweave("SevenDay_01", 5, 5)
with(RRVM7ClimY, probPlot(LowQ7, yaxis.log=T, xlabels=5))
graphics.off()


###################################################
### code chunk number 5: SevenDay.Rnw:93-95
###################################################
cat("\\includegraphics{SevenDay_01.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 6: SevenDay.Rnw:101-106
###################################################
# setSweave is required for the vignette.
setSweave("SevenDay_02", 5, 5)
with(RRVM7ClimY, timePlot(Date, LowQ7, 
  Plot=list(what="points")))
graphics.off()


###################################################
### code chunk number 7: SevenDay.Rnw:108-110
###################################################
cat("\\includegraphics{SevenDay_02.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 8: SevenDay.Rnw:119-120
###################################################
with(RRVM7ClimY, table(month(Date, label=TRUE)))


###################################################
### code chunk number 9: SevenDay.Rnw:128-138
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
### code chunk number 10: SevenDay.Rnw:143-145
###################################################
# Change the name of the statistic
names(RRVM7Summ)[4] <- "LowQ7"


###################################################
### code chunk number 11: SevenDay.Rnw:153-157
###################################################
# setSweave is required for the vignette.
setSweave("SevenDay_03", 5, 5)
with(RRVM7Summ, probPlot(LowQ7, yaxis.log=T, xlabels=5))
graphics.off()


###################################################
### code chunk number 12: SevenDay.Rnw:159-161
###################################################
cat("\\includegraphics{SevenDay_03.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 13: SevenDay.Rnw:167-172
###################################################
# setSweave is required for the vignette.
setSweave("SevenDay_04", 5, 5)
with(RRVM7Summ, timePlot(Date, LowQ7, 
    Plot=list(what="points")))
graphics.off()


###################################################
### code chunk number 14: SevenDay.Rnw:174-176
###################################################
cat("\\includegraphics{SevenDay_04.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 15: SevenDay.Rnw:186-192
###################################################
# The Annual analysis
RRVM7ClimY.frq <- with(RRVM7ClimY, freqAnal(LowQ7, id=Group,
    desc="Annual 7-day Low Flow", STAID="05484500"))
# The Summer analysis
RRVM7Summ.frq <- with(RRVM7Summ, freqAnal(LowQ7, id=Group,
    desc="Summer 7-day Low Flow", STAID="05484500"))


###################################################
### code chunk number 16: SevenDay.Rnw:199-201
###################################################
# The Annual analysis
print(RRVM7ClimY.frq)


###################################################
### code chunk number 17: SevenDay.Rnw:205-207
###################################################
# The Summer analysis
print(RRVM7Summ.frq)


###################################################
### code chunk number 18: SevenDay.Rnw:213-217
###################################################
# The Annual analysis
setSweave("SevenDay_05", 5, 5)
plot(RRVM7ClimY.frq, which="default", set.up=FALSE)
graphics.off()


###################################################
### code chunk number 19: SevenDay.Rnw:219-221
###################################################
cat("\\includegraphics{SevenDay_05.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 20: SevenDay.Rnw:227-231
###################################################
# The Annual analysis
setSweave("SevenDay_06", 5, 5)
plot(RRVM7Summ.frq, which="default", set.up=FALSE)
graphics.off()


###################################################
### code chunk number 21: SevenDay.Rnw:233-235
###################################################
cat("\\includegraphics{SevenDay_06.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 22: SevenDay.Rnw:243-248
###################################################
# The Annual analysis
predict(RRVM7ClimY.frq)
setSweave("SevenDay_07", 5, 5)
plot(RRVM7ClimY.frq, which="default", set.up=FALSE)
graphics.off()


###################################################
### code chunk number 23: SevenDay.Rnw:250-252
###################################################
cat("\\includegraphics{SevenDay_07.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 24: SevenDay.Rnw:258-263
###################################################
# The Summer analysis
predict(RRVM7Summ.frq)
setSweave("SevenDay_08", 5, 5)
plot(RRVM7Summ.frq, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 25: SevenDay.Rnw:265-267
###################################################
cat("\\includegraphics{SevenDay_08.pdf}\n")
cat("\\paragraph{}\n")


