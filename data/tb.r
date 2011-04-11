library(reshape)
library(stringr)
options(stringsAsFactors = FALSE)

# Load -----------------------------------------------------------------------
note_raw <- read.csv("tb/TB_notification.csv")
note_raw$new_sp <- NULL

pop_raw <- read.csv("tb/Country_population.csv")
pop_raw$e_pop_num <- NULL
pop_raw <- subset(pop_raw, year < 2010)

# Melt -----------------------------------------------------------------------

note <- melt(note_raw, id = c("iso2", "year"), na.rm = TRUE)
names(note)[4] <- "cases"

pop <- melt(pop_raw, id = c("iso2", "year"), na.rm = TRUE)
names(pop)[4] <- "pop"

# Break up variable in to sex and age ----------------------------------------

note$variable <- str_replace(note$variable, "new_sp_", "")
pop$variable <- str_replace(pop$variable, "e_pop_", "")

note$sex <- str_sub(note$variable, 1, 1)
pop$sex <- str_sub(pop$variable, 1, 1)

ages <- c("04" = "0-4", "514" = "5-14", "014" = "0-14", "1524" = "15-24", "2534" = "25-34", "3544" = "35-44", "4554" = "45-54", "5564" = "55-64", "65"= "65+", "u" = NA)

pop$age <- factor(ages[str_sub(pop$variable, 2)], levels = ages)
note$age <- factor(ages[str_sub(note$variable, 2)], levels = ages)

pop$variable <- NULL
note$variable <- NULL

# Combine into one file ------------------------------------------------------

rate <- join(note, pop, type = "left")
rate <- transform(rate, rate = cases / pop)

# dput(names(rate))
rate <- rate[c("iso2", "year", "sex", "age", "cases", "pop", "rate")]
write.table(rate, "tb-notification.csv", sep = ",", row = F)

# Why this form? -------------------------------------------------------------
# Because it works well with all our existing tools

# Easy to summarise
totals <- ddply(rate, c("iso2", "year"), summarise, 
  rate = sum(cases, na.rm = T) / sum(pop, na.rm = T))
totals <- subset(totals, year > 1995 & iso2 != "TK")

# Easy to plot
library(ggplot2)
qplot(year, rate, data = totals, geom = "line", 
  group = iso2)

# Easy to model
summary(lm(rate ~ year + iso2, data = totals))

# Consistency ----------------------------------------------------------------

note_total <- read.csv("tb/TB_notification.csv")[1:3]
note_total2 <- ddply(rate, c("iso2", "year"), summarise, 
  total = sum(cases, na.rm = TRUE))

compare <- join(note_total, note_total2)
subset(compare, new_sp != total)

# Need to be careful with missings because subset will throw them out
subset(compare, is.na(new_sp) & !is.na(total))
subset(compare, is.na(total) & !is.na(new_sp))

library(ggplot2)
qplot(new_sp, total, data = compare)
qplot(new_sp, total / new_sp, data = compare)

# Cleanup --------------------------------------------------------------------
file.remove("tb-notification.csv")