## Basic AFH tables

library(acs)

## potential variables for function: geography, end year, span

jeff.parish.county <- geo.make(state=22, county=51)

# new.orleans.msa <- geo.make(msa=35380)
# new.orleans.msa.county <- geo.make(state=22, county=c(51, 71, 75, 87, 89, 95, 103))

b03002 <- acs.lookup(table.number="B03002", endyear = 2013, span = 5) # lookup variables
races <- b03002[c(1, 3:9, 12)] # subsetting by race/ethnicity

alias <- c("TOTAL", "WHITE", "BLACK OR AFRICAN-AMERICAN",
           "AMERICAN INDIAN AND ALASKA NATIVE", "ASIAN OR PACIFIC ISLANDER",
           "OTHER", "HISPANIC OR LATINO")

race.table <- acs.fetch(geography = jeff.parish.county, endyear = 2013,
                        span = 5, variable = races)
race.table[, 5] <- race.table[, 5] + race.table[, 6]
race.table[, 7] <- race.table[, 7] + race.table[, 8]
race.table <- race.table[, c(1:5, 7, 9)]

percentize <- function(x, ...) divide.acs(x, ...) * 100

#race.table.pct <- apply(race.table[, 2:7], MARGIN = 1, FUN = divide.acs,
#                        denominator = race.table[, 1], method = "proportion",
#                        verbose = FALSE)

race.table.pct <- apply(race.table[, 2:7], MARGIN = 1, FUN = percentize,
                        denominator = race.table[, 1], method = "proportion",
                        verbose = FALSE)

acs.colnames(race.table) <- alias
acs.colnames(race.table.pct) <- alias[2:7]

my.data <- data.frame(count = estimate(race.table), percent = estimate(race.table.pct))