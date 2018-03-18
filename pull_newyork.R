# merge PBJ

library(tidyverse)
library(readr)
library(lubridate)
library(stringr)
library(forcats)
library(ggthemes)



get_mphrd <- function(mds, cna_hours) {
  cna_minutes = cna_hours * 60
  (cna_minutes / mds) / 24
}

unzip("../large_files/PBJ_Daily_Nurse_Staffing_2017_Q1-Q3.zip") 

pbj_2017_q1 <- read_csv("PBJ_Daily_Nurse_Staffing_2017_Q1.csv")
pbj_2017_q2 <- read_csv("PBJ_Daily_Nurse_Staffing_2017_Q2.csv")
pbj_2017_q3 <- read_csv("PBJ_Daily_Nurse_Staffing_2017_Q3.csv")


pbj_2017_all <- rbind(pbj_2017_q1, pbj_2017_q2)
pbj_2017_all <- rbind(pbj_2017_all, pbj_2017_q3)

remove(pbj_2017_q1)
remove(pbj_2017_q2)
remove(pbj_2017_q3)

pbj_2017_all$WorkDate <- ymd(pbj_2017_all$WorkDate)
pbj_2017_all$day <- wday(as.Date(pbj_2017_all$WorkDate,'%Y-%m-%d'), label=TRUE)

day_levels <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")
pbj_2017_all$day <- factor(pbj_2017_all$day, levels = day_levels)


pbj_2017_all <- pbj_2017_all %>%
  mutate(cna_hprd = round((hrs_CNA / MDScensus),2), cna_mphrd = round(get_mphrd(MDScensus, hrs_CNA),2), lpn_hprd = round((hrs_LPN / MDScensus),2), rn_hprd = round((hrs_RN / MDScensus),2))
  
newyork <- pbj_2017_all %>%
  filter(STATE == "NY")

write_csv(newyork, "data/newyork_2017_q1_q3.csv")
