# EDA
# Descriptive Statistics
# Measures of Dispersion

# loading the packages
library(aqp)
library(soilDB)

# Load from the loafercreek dataset
data("loafercreek")

# Construct generalized horizon designations
n <- c("A", "BAt", "Bt1", "Bt2", "Cr", "R")

# REGEX rules
p <- c("A", "BA|AB", "Bt|Bw", "Bt3|Bt4|2B|C",
	"Cr", "R")

# Compute genhz labels and add
# to loafercreek dataset
loafercreek$genhz <- generalize.hz(
					loafercreek$hzname,
					n, p)

# Extract the horizon table
h <- horizons(loafercreek)

# Examine the matching of pairing of
# the genhz label to the hznames
table(h$genhz, h$hzname)

vars <- c("genhz", "clay", "total_frags_pct",
		"phfield", "effclass")
summary(h[, vars])

sort(unique(h$hzname))
h$hzname <- ifelse(h$hzname == "BT",
				"Bt", h$hzname)

# first remove missing values
# and create a new vector
clay <- na.exclude(h$clay)
var(h$clay, na.rm=TRUE)
sd(h$clay, na.rm = TRUE)
cv <- sd(clay) / mean(clay) * 100
cv
quantile(clay)
range(clay)
IQR(clay)
