## Basic AFH tables

library(acs)

## potential variables for function: geography, end year, span

## Function takes state, county, and MSA FIPS codes as arguments, retrieves
## relevant data via the acs package, and returns a table containing
## jurisdiction- and region-level data on basic demographic composition.
table1 <- function(state, county, msa) {

        jurisdiction <- geo.make(state = state, county = county)
        region <- geo.make(msa = msa)
        
        ## Setting specific table, looking up variables.  Also an opportunity
        ## to adjust the vintage of data (endyear and span).
        b03002 <- acs.lookup(table.number="B03002", endyear = 2013, span = 5)
        races <- b03002[c(1, 3:9, 12)] # subsetting by race/ethnicity
        
        alias <- c("TOTAL", "WHITE", "BLACK OR AFRICAN-AMERICAN",
                   "AMERICAN INDIAN AND ALASKA NATIVE", "ASIAN OR PACIFIC ISLANDER",
                   "OTHER", "HISPANIC OR LATINO")
        
        ## Fetches relevant data for the specified geography, 
        table <- function(x) {
                race.table <- acs.fetch(geography = x, endyear = 2013,
                                        span = 5, variable = races)
                race.table[, 5] <- race.table[, 5] + race.table[, 6]
                race.table[, 7] <- race.table[, 7] + race.table[, 8]
                race.table <- race.table[, c(1:5, 7, 9)]
                
                percentize <- function(y, ...) divide.acs(y, ...) * 100
        
                race.table.pct <- apply(race.table[, 2:7], MARGIN = 1, FUN = percentize,
                                        denominator = race.table[, 1], method = "proportion",
                                        verbose = FALSE)
        
                acs.colnames(race.table) <- alias
                acs.colnames(race.table.pct) <- alias[2:7]
        
                cols <- cbind(estimate(race.table[, 2:7]), estimate(race.table.pct))
                cols
        }
        pkg <- rbind(table(jurisdiction), table(region))
        pkg
}