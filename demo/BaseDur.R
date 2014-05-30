# Load the necessary packages
library(DVstats)
# enter the station id
STAID <- "05315000"
# enter the begin date "" for earliest record
BEGIN <- ""
# enter the end date "" for latest (unapproved if current) record
END <- "2013-09-30"
# enter the base flow
BASE <- 2.2
# enter the minimum duration, 0 to keep all instances below base flow
MIN <- 5
# enter the output file name
OUT <- "Marshall2.2.txt"
# End of user input
tmp.df <- renCol(readNWIS(STAID, begin.date=BEGIN, end.date=END))
tmp.bd <- with(tmp.df, baseDur(Flow, datetime, base=BASE, STAID=STAID))
tmp.bd <- subset(tmp.bd, Duration >= MIN)
sink(OUT)
cat("Base flow durations less than ", BASE, "\n", sep="")
print(tmp.bd)
sink()
rm(tmp.df, tmp.bd)
