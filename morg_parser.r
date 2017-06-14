# CPS MORG data data available at https://www.nber.org/morg/annual/
# Data description avaialbe at https://www.nber.org/morg/annual/desc/

library(foreign)
datyears = c(12:14)
dat <- data.frame()
for(i in 1:3){
  yr <- datyears[i]
  filename <- paste0("morg", as.character(yr), ".dta")
  tmpdat <- read.dta(filename)
  tmpdat <- as.data.frame(cbind(tmpdat, "year"))
  colnames(tmpdat)[length(tmpdat)] <- "year"
  year <- as.integer(paste0("20", yr))
  tmpdat$year <- year
  tmpdat <- subset(tmpdat, select = c("hhid", "lineno", "stfips", "sex", "race", "ethnic", "class94", "age", "uhourse", "hourslw", "ftpt94", "otc", "paidhre", "earnwke", "earnhre", "earnwt", "weight", "year"))
  dat <- rbind(dat, tmpdat)
}

rm(tmpdat, filename, datyears, yr, year, i)

## Data clean-up and preparation for survey analysis

# gender
dat$sex <- factor(dat$sex, levels = c(1,2), labels = c("m", "f"))

# employed
dat <- as.data.frame(cbind(dat, "employed"))
colnames(dat)[19] <- "employed"
dat$employed <- ifelse(dat$class94 == "Self-Employed, Incorporated" | dat$class94 == "Self-Employed, Unincorporated" | dat$class94 == "Without Pay", yes = "n", no = "y")
dat$employed <-factor(dat$employed)

# race/ethnicity
# https://www2.census.gov/programs-surveys/cps/techdocs/questionnaires/Demographics.pdf
dat <- as.data.frame(cbind(dat, "race_ethnicity"))
colnames(dat)[20] <- "race_ethnicity"
dat$race_ethnicity <- NA
dat$race_ethnicity <- dat$race
dat$race_ethnicity[dat$race == 1] <- "White"
dat$race_ethnicity[dat$race == 2] <- "Black"
dat$race_ethnicity[dat$race == 3] <- "Native American"
dat$race_ethnicity[dat$race == 4] <- "Asian"
dat$race_ethnicity[dat$race == 5] <- "Pacific Islander"
dat$race_ethnicity[dat$race >= 6] <- "Other"
dat$race_ethnicity[dat$ethnic == 1] <- "Latinx"
dat$race_ethnicity <- factor(dat$race_ethnicity)

# hourly wages column set up
dat$earnhre <- dat$earnhre/100
dat$earnhre[is.na(dat$earnhre)] <- 0
dat <- cbind.data.frame(dat, "calwage")
colnames(dat)[21] <- "calwage"
dat <- cbind.data.frame(dat, "hourlywage")
colnames(dat)[22] <- "hourlywage"
dat$hourlywage <- 0
#dat$calwage <- ifelse(dat$otc != "y", (dat$earnwke/dat$uhourse), 0)
dat$calwage <- dat$earnwke/dat$uhourse
dat$calwage[is.infinite(dat$calwage)] <- 0
dat$calwage <- ifelse(is.na(dat$uhourse), dat$earnwke/dat$hourslw, dat$calwage)

# calculate from weekly
dat$hourlywage <- ifelse(dat$earnhre != 0, dat$earnhre, dat$calwage)
dat$hourlywage <- ifelse(dat$otc == "y" & dat$earnhre ==0, 0, dat$hourlywage)
dat$hourlywage <- round(dat$hourlywage, 2)
dat$hourlywage[dat$hourlywage == 0] <- NA
dat$hourlywage <- ifelse(dat$employed == "n", NA, dat$hourlywage)

# adjust wages for inflation
# https://www.minneapolisfed.org/community/teaching-aids/cpi-calculator-information/consumer-price-index-and-inflation-rates-1913
dat <- as.data.frame(cbind(dat, "awages"))
colnames(dat)[23] <- "awages"
dat$awages <- dat$hourlywage
dat$awages <- ifelse(dat$year == 2012, (dat$awages*1.021), dat$awages)
dat$awages <- ifelse(dat$year == 2013, (dat$awages*1.016), dat$awages)
dat$awages <- ifelse(dat$awages == 0, NA, dat$awages)

# adjust the suvey weights
dat <- as.data.frame(cbind(dat, "aweight"))
dat <- as.data.frame(cbind(dat, "aearn_wt"))
colnames(dat)[24:25] <- c("aweight", "aearn_wt")
dat[24:25] <- NA
dat$aweight <- dat$weight/9
dat$aweight <- ifelse(is.na(dat$weight), NA, dat$aweight)
dat$aearn_wt <- dat$earnwt/9
dat$aearn_wt <- ifelse(is.na(dat$earnwt), NA, dat$aearn_wt)
dat <- as.data.frame(cbind(dat, "total"))
colnames(dat)[26] <- "ntl"
dat$ntl <- factor(dat$ntl)

write.dta(dat, "morg1214.dta")
