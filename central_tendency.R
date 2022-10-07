# EDA
# Descriptive Statistics
# Measures of Central Tendency

#loading the required packages
library(aqp)
library(soilDB)

# Load from the loafercreek dataset
data("loafercreek")

# Construct generalized horizon designations
n <- c("A", "BAt", "Bt1", "Bt2", "Cr", "R")

# REGEX rules
p <- c("A", "BA|AB", "Bt|Bw", "Bt3|Bt4|2B|C",
	"Cr", "R")

# Compute genhz labels and
# add to loafercreek dataset
loafercreek$genhz <- generalize.hz(
					loafercreek$hzname,
					n, p)

# Extract the horizon table
h <- horizons(loafercreek)

# Examine the matching of pairing
# of the genhz label to the hznames
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

mean(clay)
median(clay)
sort(table(round(h$clay)),
	decreasing = TRUE)[1]
table(h$genhz)
# append the table with
# row and column sums
addmargins(table(h$genhz,
				h$texcl))

# calculate the proportions
# relative to the rows, margin = 1
# calculates for rows, margin = 2 calculates
# for columns, margin = NULL calculates
# for total observations
round(prop.table(table(h$genhz, h$texture_class),
				margin = 1) * 100)
knitr::kable(addmargins(table(h$genhz, h$texcl)))

aggregate(clay ~ genhz, data = h, mean)
aggregate(clay ~ genhz, data = h, median)
aggregate(clay ~ genhz, data = h, summary)
