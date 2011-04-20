options(stringsAsFactors = F)
library(reshape2)
library(ggplot2)
library(stringr)
library(MASS)
source("../data/xtable.r")

# Load data and do basic cleaning for this task
if (!exists("deaths", inherit = FALSE)) {
  load("deaths.rdata")
  deaths$hod[deaths$hod == 99] <- NA
  deaths$hod[deaths$hod == 24] <- 0
  deaths$hod[deaths$hod == 0] <- NA
  deaths$hod <- as.integer(deaths$hod)  
}

codes <- read.csv("icd-main.csv")
codes$disease <- sapply(codes$disease, function(x)
  str_c(strwrap(x, width = 30), collapse = "\n"))
names(codes)[1] <- "cod"
codes <- codes[!duplicated(codes$cod), ]

# Count deaths per hour, per disease
hod2 <- count(deaths, c("cod", "hod"))
hod2 <- subset(hod2, !is.na(hod))
hod2 <- join(hod2, codes)
hod2 <- ddply(hod2, "cod", transform, prop = freq / sum(freq))

# Compare to overall abundance
overall <- ddply(hod2, "hod", summarise, freq_all = sum(freq))
overall <- mutate(overall, prop_all = freq_all / sum(freq_all))

hod2 <- join(overall, hod2, by = "hod")

hod3 <- hod2[1:15, c("hod", "cod", "disease", "freq", "prop", "freq_all", "prop_all")]
hod3$disease <- unlist(lapply(hod3$disease, function(x) {
  pieces <- strwrap(x, width = 45)
  if (length(pieces) > 1) str_c(pieces[1], "...") else pieces
}))
xtable(hod3, "counts-all.tex")

devi <- ddply(hod2, "cod", summarise, n = sum(freq), 
  dist = mean((prop - prop_all)^2))
devi <- subset(devi, n > 50)

# Find outliers
xlog10 <- scale_x_log10(
  breaks = c(100, 1000, 10000), 
  labels = c(100, 1000, 10000), 
  minor_breaks = outer(1:9, 10^(1:5), "*"))
qplot(n, dist, data = devi)
ggsave("n-dist-raw.pdf", width = 6, height = 6)
qplot(n, log10(dist), data = devi) + 
  geom_smooth(method = "rlm", se = F) + 
  xlog10
ggsave("n-dist-log.pdf", width = 6, height = 6)

devi$resid <- resid(rlm(log(dist) ~ log(n), data = devi))
ggplot(devi, aes(n, resid)) + 
  geom_hline(yintercept = 1, colour = "grey50") +
  geom_point() + 
  xlog10

unusual <- subset(devi, resid > 1.5)
hod_unusual_big <- match_df(hod2, subset(unusual, n > 350))
hod_unusual_sml <- match_df(hod2, subset(unusual, n <= 350))

# Visualise unusual causes of death
ggplot(hod_unusual_big, aes(hod, prop)) + 
  geom_line(aes(y = prop_all), data = overall, colour = "grey50") +
  geom_line() + 
  facet_wrap(~ disease, ncol = 3)
ggsave("unusual-big.pdf", width = 8, height = 6)
last_plot() %+% hod_unusual_sml
ggsave("unusual-sml.pdf", width = 8, height = 4)


# Try to find unusual diseases some other way -------------------------------

# Just look at big diseases to make things easier
big <- subset(arrange(count(hod2, "cod"), freq), freq > 350)
hod_big <- match_df(hod2, big, "cod")

# Hierarchical clustering
wide <- dcast(hod_big, cod ~ hod, value_var = "prop")
wide[is.na(wide)] <- 0

cl <- hclust(dist(wide[, -1]), "ward")
plot(cl, labels = wide[, 1])

# Linear model doesn't work because we need frequency weighting
# mod <- lm(freq ~ factor(hod) * cod, data = hod_big)