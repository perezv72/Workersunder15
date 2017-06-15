# Modeling numbers of workers earning less than $15 per hour
## Survey object set-up
library(foreign)
dat <- read.dta("morg1214.dta")
library(survey)
tot.svy <- svydesign(ids = ~1, data = dat, weights = ~aweight)
earn.svy <- svydesign(ids = ~1, data = dat, weights = ~aearn_wt)

## National analysis
geography = "National"
t_employed <- as.integer(as.data.frame(svyby(~employed, ~ntl, tot.svy, svytotal, na.rm = TRUE))[3])
p_u15 <- round((sum(subset(dat$aearn_wt, dat$awages <=15 & dat$awages>0)))/(sum(subset(dat$aearn_wt, dat$awages >0))), 4)
t_u15 <- as.integer(t_employed*p_u15)

### by gender
t_female <- as.integer(svyby(~employed, ~ntl, subset(tot.svy, sex == "f"), svytotal, na.rm = TRUE))[3]
p_female_u15 <- round((sum(subset(dat$aearn_wt, dat$awages <=15 & dat$awages >0 & dat$sex == 'f')))/(sum(subset(dat$aearn_wt, dat$awages >=0 & dat$sex == 'f'))), 4)
t_female_u15 <- as.integer(t_female*p_female_u15)

### Latinx
t_latinx <- as.integer(svyby(~employed, ~ntl, subset(tot.svy, race_ethnicity == "Latinx"), svytotal, na.rm = TRUE))[3]
p_latinx_u15 <- round((sum(subset(dat$aearn_wt, dat$awages <=15 & dat$awages >0 & dat$race_ethnicity == "Latinx")))/(sum(subset(dat$aearn_wt, dat$awages >=0 & dat$race_ethnicity == "Latinx"))), 4)
t_latinx_u15 <- as.integer(t_latinx*p_latinx_u15)

### Black
t_black <- as.integer(svyby(~employed, ~ntl, subset(tot.svy, race_ethnicity == "Black"), svytotal, na.rm = TRUE))[3]
p_black_u15 <- round((sum(subset(dat$aearn_wt, dat$awages <=15 & dat$awages >0 & dat$race_ethnicity == "Black")))/(sum(subset(dat$aearn_wt, dat$awages >=0 & dat$race_ethnicity == "Black"))), 4)
t_black_u15 <- as.integer(t_black*p_black_u15)

### millenials
t_youth <- as.integer(svyby(~employed, ~ntl, subset(tot.svy, age <= 35), svytotal, na.rm = TRUE))[3]
p_youth_u15 <- round((sum(subset(dat$aearn_wt, dat$awages <=15 & dat$awages >0 & dat$age <= 35)))/(sum(subset(dat$aearn_wt, dat$awages >=0 & dat$age <= 35))), 4)
t_youth_u15 <- as.integer(t_youth*p_youth_u15)

ntlresults <- cbind(geography, t_employed, p_u15, t_u15, t_female, p_female_u15, t_female_u15, t_latinx, p_latinx_u15, t_latinx_u15, t_black, p_black_u15, t_black_u15, t_youth, p_youth_u15, t_youth_u15)

#total
t_employed <- as.integer(as.data.frame(svyby(~employed, ~ntl, tot.svy, svytotal, na.rm = TRUE))[3])
p_u15 <- round((sum(subset(dat$aearn_wt, dat$awages <=15 & dat$awages>0)))/(sum(subset(dat$aearn_wt, dat$awages >0))), 4)
t_u15 <- as.integer(t_employed*p_u15)

# by gender
t_female <- as.integer(svyby(~employed, ~ntl, subset(tot.svy, sex == "f"), svytotal, na.rm = TRUE))[3]
p_female_u15 <- round((sum(subset(dat$aearn_wt, dat$awages <=15 & dat$awages >0 & dat$sex == 'f')))/(sum(subset(dat$aearn_wt, dat$awages >=0 & dat$sex == 'f'))), 4)
t_female_u15 <- as.integer(t_female*p_female_u15)

# Latinx
t_latinx <- as.integer(svyby(~employed, ~ntl, subset(tot.svy, race_ethnicity == "Latinx"), svytotal, na.rm = TRUE))[3]
p_latinx_u15 <- round((sum(subset(dat$aearn_wt, dat$awages <=15 & dat$awages >0 & dat$race_ethnicity == "Latinx")))/(sum(subset(dat$aearn_wt, dat$awages >=0 & dat$race_ethnicity == "Latinx"))), 4)
t_latinx_u15 <- as.integer(t_latinx*p_latinx_u15)

# Latinx women
t_flatinx <- as.integer(svyby(~employed, ~ntl, subset(tot.svy, race_ethnicity == "Latinx" &  sex == "f"), svytotal, na.rm = TRUE))[3]
p_flatinx_u15 <- round((sum(subset(dat$aearn_wt, dat$awages <=15 & dat$awages >0 & dat$race_ethnicity == "Latinx" & dat$sex == "f")))/(sum(subset(dat$aearn_wt, dat$awages >=0 & dat$race_ethnicity == "Latinx" & dat$sex == "f"))), 4)
t_flatinx_u15 <- as.integer(t_latinx*p_latinx_u15)

# Black
t_black <- as.integer(svyby(~employed, ~ntl, subset(tot.svy, race_ethnicity == "Black"), svytotal, na.rm = TRUE))[3]
p_black_u15 <- round((sum(subset(dat$aearn_wt, dat$awages <=15 & dat$awages >0 & dat$race_ethnicity == "Black")))/(sum(subset(dat$aearn_wt, dat$awages >=0 & dat$race_ethnicity == "Black"))), 4)
t_black_u15 <- as.integer(t_black*p_black_u15)

# Black women
t_fblack <- as.integer(svyby(~employed, ~ntl, subset(tot.svy, race_ethnicity == "Black" &  sex == "f"), svytotal, na.rm = TRUE))[3]
p_fblack_u15 <- round((sum(subset(dat$aearn_wt, dat$awages <=15 & dat$awages >0 & dat$race_ethnicity == "Black" & dat$sex == "f")))/(sum(subset(dat$aearn_wt, dat$awages >=0 & dat$race_ethnicity == "Black" & dat$sex == "f"))), 4)
t_fblack_u15 <- as.integer(t_black*p_black_u15)

# millenials
t_youth <- as.integer(svyby(~employed, ~ntl, subset(tot.svy, age <= 35), svytotal, na.rm = TRUE))[3]
p_youth_u15 <- round((sum(subset(dat$aearn_wt, dat$awages <=15 & dat$awages >0 & dat$age <= 35)))/(sum(subset(dat$aearn_wt, dat$awages >=0 & dat$age <= 35))), 4)
t_youth_u15 <- as.integer(t_youth*p_youth_u15)

geography = "national"

ntlresults <- cbind(geography, t_employed, p_u15, t_u15, t_female, p_female_u15, t_female_u15, t_latinx, p_latinx_u15, t_latinx_u15, t_black, p_black_u15, t_black_u15, t_youth, p_youth_u15, t_youth_u15)

ntlresults_expanded <- cbind(geography, t_employed, p_u15, t_u15, t_female, p_female_u15, t_female_u15, t_latinx, p_latinx_u15, t_latinx_u15, t_female_u15, t_flatinx, p_flatinx_u15, t_flatinx_u15, t_black, p_black_u15, t_black_u15, t_fblack, p_fblack_u15, t_fblack_u15, t_youth, p_youth_u15, t_youth_u15)

### State analysis
state <- c(levels(dat$stfips))
stresults <- data.frame()
for(i in 1:51){
  # Total
  st <- as.character(state[i])
  t_employed <- as.integer(as.data.frame(svyby(~employed, ~ntl, subset(tot.svy, stfips == st), svytotal, na.rm = TRUE))[3])
  p_u15 <- round((sum(subset(dat$aearn_wt, dat$awages <=15 & dat$awages > 0 & dat$stfips == st ))) / (sum(subset(dat$aearn_wt, dat$awages > 0 & dat$stfips == st))), 4)
  t_u15 <- as.integer(t_employed*p_u15)

  # Women
  t_female <- as.integer(as.data.frame(svyby(~employed, ~ntl, subset(tot.svy, stfips == st & sex == 'f'), svytotal, na.rm = TRUE))[3])
  p_female_u15 <- round((sum(subset(dat$aearn_wt, dat$awages <=15 & dat$awages > 0 & dat$stfips == st & dat$sex == 'f' ))) / (sum(subset(dat$aearn_wt, dat$awages > 0 & dat$stfips == st & dat$sex == 'f'))), 4)
  t_female_u15 <- as.integer(t_female*p_female_u15) 
  
  # Latinx
  t_latinx <- as.integer(as.data.frame(svyby(~employed, ~ntl, subset(tot.svy, stfips == st & race_ethnicity == 'Latinx'), svytotal, na.rm = TRUE))[3])
  p_latinx_u15 <- round((sum(subset(dat$aearn_wt, dat$awages <=15 & dat$awages > 0 & dat$stfips == st & dat$race_ethnicity == 'Latinx' ))) / (sum(subset(dat$aearn_wt, dat$awages > 0 & dat$stfips == st & dat$race_ethnicity == 'Latinx'))), 4)
  t_latinx_u15 <- as.integer(t_latinx*p_latinx_u15) 
  
  # Black
  t_black <- as.integer(as.data.frame(svyby(~employed, ~ntl, subset(tot.svy, stfips == st & race_ethnicity == 'Black'), svytotal, na.rm = TRUE))[3])
  p_black_u15 <- round((sum(subset(dat$aearn_wt, dat$awages <=15 & dat$awages > 0 & dat$stfips == st & dat$race_ethnicity == 'Black' ))) / (sum(subset(dat$aearn_wt, dat$awages > 0 & dat$stfips == st & dat$race_ethnicity == 'Black'))), 4)
  t_black_u15 <- as.integer(t_black*p_black_u15) 
  
  # Millenials
  t_youth <- as.integer(as.data.frame(svyby(~employed, ~ntl, subset(tot.svy, stfips == st & age <= 35), svytotal, na.rm = TRUE))[3])
  p_youth_u15 <- round((sum(subset(dat$aearn_wt, dat$awages <=15 & dat$awages > 0 & dat$stfips == st & dat$age <= 35 ))) / (sum(subset(dat$aearn_wt, dat$awages > 0 & dat$stfips == st & dat$age <= 35))), 4)
  t_youth_u15 <- as.integer(t_youth*p_youth_u15) 
  
  # Results
  tmpresults <- cbind(st, t_employed, p_u15, t_u15, t_female, p_female_u15, t_female_u15, t_latinx, p_latinx_u15, t_latinx_u15, t_black, p_black_u15, t_black_u15, t_youth, p_youth_u15, t_youth_u15)
  stresults <- rbind(stresults, tmpresults)
  
}

colnames(stresults)[1] <- "geography"
total <- as.data.frame(rbind(ntlresults, stresults))
write.csv(total, "workers_under15.csv")
