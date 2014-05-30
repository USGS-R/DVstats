# Load the necessary packages
library(DVstats)
# enter the station id
STAID <- "05315000"
# enter the begin date "" for earliest record
BEGIN <- ""
# enter the end date "" for latest (unapproved if current) record
END <- "2013-09-30"
# enter the output file name
OUT <- "Marshall.txt"
# End of user input
tmp.df <- renCol(readNWIS(STAID, begin.date=BEGIN, end.date=END))
tmp.ann <- with(tmp.df, flowDurClasses(Flow))
tmp.mon <- with(tmp.df, tapply(Flow, month(datetime, label=TRUE), flowDurClasses))
sink(OUT)
cat("Annual flow duration classes for ", STAID, "\n", sep="")
print(tmp.ann)
cat("\nMonthly flow duration classes\n")
print(tmp.mon)
sink()
rm(tmp.df, tmp.ann, tmp.mon)
