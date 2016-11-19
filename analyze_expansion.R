# rm(list = ls())
setwd("C:/Users/Jon/Google Drive/Broadband Access")

load(file = "data/usage_change.rda")
library(readr)
dwell <- read.csv("data/Multiple_Dwelling_Registrations.csv", stringsAsFactors = FALSE)
library(lubridate)
library(readxl)
usage.all$fiber1415 <- (usage.all$fiber15>0) - (usage.all$fiber14>0)
usage.all$fast1415 <- usage.all$otherfast15 - usage.all$otherfast14

pop <- read_excel("data/t_sf1_h3_ct.xlsx")
pop$percent.over65 <- pop$`65 Years and Over` / pop$Total
pop$percent.45to64 <- pop$`45 to 64 Years` / pop$Total
pop$percent.25to44 <- pop$`25 to 44 Years` / pop$Total
pop$percent.15to24 <- pop$`15 to 24 Years` / pop$Total
pop$tract.bor <- mapply(paste0, str_pad(pop$`2010 Census FIPS County Code`, 3, pad = "0"), pop$`2010 Census Tract`)
pop <- pop[!is.na(pop$`2010 Census Tract`), ]
pop <- pop[, c("tract.bor", "percent.over65", "percent.45to64",  "percent.25to44", "percent.15to24")]
library(stringr)
usage.all$tract.bor <- mapply(paste0, usage.all$county, usage.all$tract)

library(mellonMisc)

usage.all <- safemerge(usage.all, y = pop, all.x = T, by = "tract.bor", type = "m:1")
usage.all$percent.over65[is.nan(usage.all$percent.over65)] <- NA
usage.all$percent.over65[usage.all$percent.over65 ==Inf] <- NA 
usage.all$fiber.expand <- usage.all$fiber1415==1

poverty <- read_csv("data/all_poverty.csv")
poverty$tract.bor <- mapply(paste0, poverty$county, poverty$tract)
poverty$pov.rate <- poverty$B17021_002E / poverty$B17021_001E
poverty <- poverty[, c("tract.bor", "pov.rate")]
usage.all <- safemerge(usage.all, poverty, all.x = T, by = "tract.bor", type = "m:1")

rent <- read_csv("data/rent_burdened.csv")
rent$tract.bor <- mapply(paste0, rent$county, rent$tract)

head(rent)

(rent$B25070_007E + rent$B25070_008E + rent$B25070_009E + rent$B25070_010E) 
rent$prop.rent <- (rent$B25070_007E + rent$B25070_008E + rent$B25070_009E + rent$B25070_010E) / rent$B25070_001E
rent$prop.rent[is.nan(rent$prop.rent)] <- NA

rent <- rent[, c("tract.bor", "prop.rent")]


readHousing <- function(file, county = "061") {
  mn <- read.csv(file, stringsAsFactors = FALSE)
  
  start <- sapply(strsplit(as.character(mn$CT2010), "\\."), function(x) x[1])
  suffix <- sapply(strsplit(as.character(mn$CT2010), "\\."), function(x) x[2])
  suffix[is.na(suffix)] <- "00"
  
  mn$CT2010 <- mapply(paste0, str_pad(start, 4, pad = "0"), suffix)
  mn$census.block <- mapply(paste0, mn$CT2010, mn$CB2010)
  mn$census.block <- paste0(county, mn$census.block)
  mn$census.block <- paste0(36, mn$census.block)
  mn$mfwalkup <- mn$NumBldgs
  mn$mfwalkup[!mn$LandUse %in% 2:3] <- 0
  
  mn$sfhome <- mn$NumBldgs
  mn$sfhome[mn$LandUse!=1] <- 0
  mn <- aggregate(mn[, c("NumBldgs", "mfwalkup", "sfhome")], list(mn$census.block), sum, na.rm = T)
  return(mn)
}
mn <- readHousing("data/MN.csv", county = "061")
bk <- readHousing("data/BK.csv", county = "047")
si <- readHousing("data/SI.csv", county = "085")
bx <- readHousing("data/BX.csv", county = "005")
qn <- readHousing("data/QN.csv", county = "081")



land.usage <- rbind(mn, bk, si, bx, qn)

usage.all <- safemerge(usage.all, land.usage, 
                       by.x = "BlockCode", by.y = "Group.1", all.x = T)

usage.all$mfwalkup.percent <- usage.all$mfwalkup / usage.all$NumBldgs
usage.all$sfhome.percent <- usage.all$sfhome / usage.all$NumBldgs
max(usage.all$sfhome.percent, na.rm = T)
usage.all$sfhome.percent[usage.all$sfhome.percent==Inf] <- NA
usage.all$mfwalkup.percent[usage.all$mfwalkup.percent==Inf] <- NA
usage.all$fiber.baseline <- usage.all$fiber14>0

usage.all <- safemerge(usage.all, rent, by = "tract.bor", type = "m:1")

usage.all$borough <- NA
usage.all$borough[usage.all$county=="061"] <- "Manhattan"
usage.all$borough[usage.all$county=="047"] <- "Brooklyn"
usage.all$borough[usage.all$county=="085"] <- "Staten Island"
usage.all$borough[usage.all$county=="005"] <- "Bronx"
usage.all$borough[usage.all$county=="081"] <- "Queens"


usage.all$percent.over65 <- usage.all$percent.over65 * 100
usage.all$pov.rate <- usage.all$pov.rate * 100
usage.all$sfhome.percent <- usage.all$sfhome.percent * 100

standardize(usage.all$sfhome.percent)
usage.all$sfhome.percent <- scale(usage.all$sfhome.percent)
usage.all$pov.rate <- scale(usage.all$pov.rate)
usage.all$percent.over65 <- scale(usage.all$percent.over65)
save(usage.all, file = "data/usage_analyze.rda")



formula.change <- fiber.expand ~ fast1415 + otherfast14 + 
  percent.over65 + pov.rate  + sfhome.percent + borough

formula.baseline <- fiber.baseline ~ otherfast14 + percent.over65 + pov.rate + 
  sfhome.percent +  borough


summary(glm(data = usage.all[usage.all$fiber14==0, ],
            formula.change, family = "binomial"))
summary(glm(data = usage.all, 
            formula.baseline, 
            family = "binomial"))

summary(glm(data = usage.all, formula.current, family = "binomial"))
library(mfx)

plotMFX <- function(expansion.mfx) {
  expansion.mfx <- data.frame(expansion.mfx$mfxest)
  
  expansion.mfx$uci<- expansion.mfx$dF.dx + (expansion.mfx$Std..Err. * 1.96)
  expansion.mfx$lci<- expansion.mfx$dF.dx - (expansion.mfx$Std..Err. * 1.96)
  
  library(ggplot2)
  expansion.mfx$variable <- rownames(expansion.mfx)
  ggplot(data = expansion.mfx, aes(y = dF.dx, x = variable)) + geom_point() + 
    geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.25) + coord_flip() + 
    geom_hline(yintercept = 0, colour = "red") + theme_bw() + ylim(c(-0.2, 0.2))
}

baseline.mfx <- logitmfx(data = usage.all, 
                        formula.baseline)

expansion.mfx <- logitmfx(data = usage.all[usage.all$fiber14==0, ], 
                          formula.change)

hist(usage.all$sfhome.percent)
hist(usage.all$pov.rate)
plotMFX(baseline.mfx) + ggtitle("Baseline")
plotMFX(expansion.mfx) + ggtitle("2014-2015")


match(names(block.counts))
usage.all$block
head(dwell)
hist(block.counts)
dwell[dwell$Block=="10640", ]


head(dwell$LastRegistrationDate)
head(dwell)
colnames(dwell)
