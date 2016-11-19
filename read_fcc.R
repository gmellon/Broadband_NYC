rm(list = ls())
setwd("C:/Users/Jon/Google Drive/Broadband Access")

library(data.table)

library(readr)
library(mellonMisc)

formatFCC <- function(file) {
  usage <- read_csv(file, col_types = rep(list(col_character()), 17) )
  usage$state <- substr(usage$BlockCode, 1, 2)
  usage$county <- substr(usage$BlockCode, 3, 5)
  usage$tract <- substr(usage$BlockCode, 6, 11)
  usage$block <- substr(usage$BlockCode, 12, 15)
  usage <- usage[usage$county  %in% c("005", "047", "061", "081", "085"), ]
  usage$fiber <- usage$TechCode=="50"
  usage$otherfast <- usage$TechCode %in% 40:42
  usage$slow <- !usage$fiber & !usage$otherfast
  
  usage <- usage[, c("Provider_Id", "county", "tract", "block", "BlockCode", "slow", "otherfast", "fiber")]
  
  usage.sum <- aggregate(usage[, c("slow", "otherfast", "fiber")], 
                         list(usage$county, usage$tract, usage$block, usage$BlockCode), sum)
  colnames(usage.sum)[1:4] <- c("county", "tract", "block", "BlockCode")

  return(usage.sum)
}

usage.14 <- formatFCC(file = "data/NY-Fixed-Dec14-v1.csv")
usage.15 <- formatFCC("data/NY-Fixed-Dec2015-v1.csv")


library(mellonMisc)
colnames(usage.14)[5:7] <- paste0(colnames(usage.14)[5:7], "14") 
colnames(usage.15)[5:7] <- paste0(colnames(usage.15)[5:7], "15") 


usage.all <- safemerge(usage.14, usage.15, by = c("county", "tract", "block", "BlockCode" ))
save(usage.all, file = "data/usage_change.rda")


usage.14$summary.tech

library(reshape2)

usage.14$Provider_Id
cast(usage, )





# DOT dataset of permit requests related to fiber construction
# Community district
# 


# correlation between single family home and multiple dwlling units and change in levels of service


# 1880s brooklyn laws
# 228 petition
# public hearing  city council oct 2015, staff included map of 228 petitions

# multiple dwlling units versus single dwelling  units

