####LIbraries and Data @@@@
library(easypackages)
libs <- c("tidyverse", "haven", "psych")
libraries(libs)

dat <- read_sav("201027 - PSCI 230 assignment 3_November 5, 2020_14.48.sav")

#### Data Cleaning and manipulation ####
summary(dat$therm.dem_1)
summary(dat$therm.rep_1)

dat <- dat %>%
  mutate(aff.pol=abs(therm.rep_1-therm.dem_1))

summary(dat$PP1)
summary(dat$PP2)
summary(dat$PP3)

dat$pp1.r <- dat$PP1
dat$pp2.r <- dat$PP2
dat$pp3.r <- car::recode(dat$PP3, "1=4; 2=3; 3=2; 4=1")

dat$pp.index <- rowMeans(dat[c("pp1.r", "pp2.r", "pp3.r")], na.rm=T)
alpha.ppol <- psych::alpha(dat[c("pp1.r", "pp2.r", "pp3.r")], check.keys = T)

t.test(dat$pp.index[dat$condition==1], dat$pp.index[dat$condition==2])
t.test(dat$poor[dat$condition==1], dat$poor[dat$condition==2])
t.test(dat$wealthy[dat$condition==1], dat$wealthy[dat$condition==2])







