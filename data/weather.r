library(stringr)
library(reshape2)
library(plyr)
source("xtable.r")
source("read-fwf.r")
options(stringsAsFactors = FALSE)

# Define format for fixed width file
cols <- data.frame(
  name =  c("id", "year", "month", "element"),
  start = c(1,     12,    16,      18),
  end =   c(11,    15,    17,      21))

names <- str_c(c("value", "mflag", "qflag", "sflag"), rep(1:31, each = 4), sep = "_")
starts <- cumsum(c(22, rep(c(5, 1, 1, 1), 31)))
starts <- starts[-length(starts)]
ends <- c(starts[-1], starts[length(starts)] + 1) - 1

values <- data.frame(name = names, start = starts, end = ends)
cols <- rbind(cols, values)

# Load data and subset to small example
raw <- read.fwf2("weather.txt",  cols)
raw <- subset(raw, year == 2010 & element %in% c("TMIN", "TMAX")) 
raw <- raw[, c(1:4, which(str_detect(names(raw), "value")))]

names(raw)[-(1:4)] <- str_c("d", 1:31)
raw[raw == -9999] <- NA
rownames(raw) <- NULL

xtable(raw[1:10, 1:14], file = "weather-raw.tex")

# Melt and tidy

clean1 <- melt(raw, id = 1:4, na.rm = T)
clean1$day <- as.integer(str_replace(clean1$variable, "d", ""))
clean1$variable <- NULL

clean1 <- clean1[c("id", "year", "month", "day", "element", "value")]
clean1 <- arrange(clean1, year, month, day, element)
xtable(clean1[1:10, ], file = "weather-clean-1.tex")

# Cast

clean2 <- dcast(clean1, ... ~ element)
xtable(clean2[1:10, ], file = "weather-clean-2.tex")
