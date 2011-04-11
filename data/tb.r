library(reshape)
library(stringr)
source("xtable.r")
options(stringsAsFactors = FALSE)

# Load -----------------------------------------------------------------------
raw <- read.csv("tb.csv")
raw$new_sp <- NULL
raw <- subset(raw, year == 2000)

names(raw) <- str_replace(names(raw), "new_sp_", "")

xtable(raw[1:10, 1:11], file = "tb-raw.tex")


# Melt -----------------------------------------------------------------------

clean <- melt(raw, id = c("iso2", "year"), na.rm = TRUE)
names(clean)[4] <- "cases"

clean <- arrange(clean, iso2, variable, year)
xtable(clean[1:15, ], file = "tb-clean-1.tex")

# Break up variable in to sex and age ----------------------------------------

clean$sex <- str_sub(clean$variable, 1, 1)

ages <- c("04" = "0-4", "514" = "5-14", "014" = "0-14", "1524" = "15-24", "2534" = "25-34", "3544" = "35-44", "4554" = "45-54", "5564" = "55-64", "65"= "65+", "u" = NA)

clean$age <- factor(ages[str_sub(clean$variable, 2)], levels = ages)
clean$variable <- NULL

clean <- clean[c("iso2", "year", "sex", "age", "cases")]

xtable(clean[1:15, ], file = "tb-clean-2.tex")
