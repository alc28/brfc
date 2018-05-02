# daily line chart

library(tidyverse)
library(readr)

newyork_2017 <- read_csv("~/beechtree_consolidated/brfc/data/newyork_2017.csv")

pbj_2017_beechtree <- newyork_2017 %>%
  filter(PROVNUM == '335017')

# CNA
pbj_2017_beechtree %>% 
  group_by(CY_Qtr) %>%
  summarise(mean = mean(cna_hprd), median = median(cna_hprd), high = max(cna_hprd), min = min(cna_hprd))

# LPN
pbj_2017_beechtree %>% 
  group_by(CY_Qtr) %>%
  summarise(mean = mean(lpn_hprd), median = median(lpn_hprd), high = max(lpn_hprd), min = min(lpn_hprd))

# RN
pbj_2017_beechtree %>% 
  group_by(CY_Qtr) %>%
  summarise(mean = mean(rn_hprd), median = median(rn_hprd), high = max(rn_hprd), min = min(rn_hprd))

# LPN + RN
pbj_2017_beechtree %>% 
  group_by(CY_Qtr) %>%
  summarise(mean = mean(rn_hprd + lpn_hprd), median = median(rn_hprd + lpn_hprd), high = max(rn_hprd + lpn_hprd), min = min(rn_hprd + lpn_hprd))

var(pbj_2017_beechtree$cna_hprd)
var(pbj_2017_beechtree$rn_hprd)
var(pbj_2017_beechtree$lpn_hprd)

# CNA by day
pbj_2017_beechtree %>% 
  group_by(day) %>%
  summarise(high = max(cna_hprd), mean = mean(cna_hprd), median = median(cna_hprd),  min = min(cna_hprd)) %>%
  arrange(desc(mean))

df_beechtree <- pbj_2017_beechtree
df_beechtree$mycaption <- NA
#df_beechtree$mycaption[df_beechtree$WorkDate == "2017-04-13"] <- "Thurs, Apr 13\n 2.55 HPRD = 6min 30sec per hour\n\n\n"
#df_beechtree$mycaption[df_beechtree$WorkDate == "2017-06-21"] <- "Jun 21\n"
#df_beechtree$mycaption[df_beechtree$WorkDate == "2017-09-12"] <- "\n\n\nTues, Sep 12\n0.92 HPRD = 2min 20sec per hour"

# hours per resident day
ggplot(df_beechtree, aes(WorkDate, cna_hprd, label = mycaption), vjust = 0.3) +
  theme_grey() +
  geom_line(size = 0.1) +
  scale_x_date(date_breaks="1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0.5, 3.0), breaks = seq(0,5, by = 0.5)) +
  ggtitle("\nCNA hours per resident day (HPRD)", subtitle = "Beechtree Center for Rehabilitation and Nursing, Ithaca NY") +
  xlab("January 1 - December 31, 2017") +
  ylab("Hours") +
  labs(caption = paste("HPRD = total CNA hours / number of residents \nData source: https://data.cms.gov/browse?q=PBJ \nCompiled by Adam Chandler, Beechtree Family Council,", now(), "\n\n")) +
  geom_text(data=subset(df_beechtree, !is.na(mycaption)), size = rel(3)) +
  geom_point(size = 1.35, aes(fill = day), shape = 21) + scale_fill_brewer("day") 


ggplot(df_beechtree, aes(WorkDate, rn_hprd, label = mycaption), vjust = 0.3) +
  theme_grey() +
  geom_line(size = 0.1) +
  scale_x_date(date_breaks="1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0.0, 1.5), breaks = seq(0,5, by = 0.5)) +
  ggtitle("\nRN hours per resident day (HPRD)", subtitle = "Beechtree Center for Rehabilitation and Nursing, Ithaca NY") +
  xlab("January 1 - December 31, 2017") +
  ylab("Hours") +
  labs(caption = paste("HPRD = total RN hours / number of residents \nData source: https://data.cms.gov/browse?q=PBJ \nCompiled by Adam Chandler, Beechtree Family Council,", now(), "\n\n")) +
  geom_text(data=subset(df_beechtree, !is.na(mycaption)), size = rel(3)) +
  geom_point(size = 1.35, aes(fill = day), shape = 21) + scale_fill_brewer("day") 


ggplot(df_beechtree, aes(WorkDate, lpn_hprd, label = mycaption), vjust = 0.3) +
  theme_grey() +
  geom_line(size = 0.1) +
  scale_x_date(date_breaks="1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0.0, 1.5), breaks = seq(0,5, by = 0.5)) +
  ggtitle("\nLPN hours per resident day (HPRD)", subtitle = "Beechtree Center for Rehabilitation and Nursing, Ithaca NY") +
  xlab("January 1 - December 31, 2017") +
  ylab("Hours") +
  labs(caption = paste("HPRD = total LPN hours / number of residents \nData source: https://data.cms.gov/browse?q=PBJ \nCompiled by Adam Chandler, Beechtree Family Council,", now(), "\n\n")) +
  geom_text(data=subset(df_beechtree, !is.na(mycaption)), size = rel(3)) +
  geom_point(size = 1.35, aes(fill = day), shape = 21) + scale_fill_brewer("day") 


ggplot(df_beechtree, aes(WorkDate, MDScensus, label = mycaption), vjust = 0.3) +
  theme_grey() +
  geom_line(size = 0.3) +
  scale_x_date(date_breaks="1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(90, 130), breaks = seq(90,130, by = 10)) +
  ggtitle("\n2017 daily resident census", subtitle = "Beechtree Center for Rehabilitation and Nursing, Ithaca NY") +
  xlab("January 1 - December 31") +
  ylab("Number of residents") +
  labs(caption = paste("Daily resident census \nData source: https://data.cms.gov/browse?q=PBJ \nCompiled by Adam Chandler, Beechtree Family Council,", now(), "\n\n")) +
  geom_text(data=subset(df_beechtree, !is.na(mycaption)), size = rel(3))


tidy_beechtree <- df_beechtree %>%
  select(WorkDate, cna_hprd, lpn_hprd, rn_hprd) %>%
  gather(key = 'staff_type', value = 'hprd', 2:4)

ggplot(tidy_beechtree, aes(x=WorkDate, y=hprd)) +
  geom_line() +
  #scale_y_continuous(limits = c(0, 3.0), breaks = seq(0,5, by = 0.5)) +
  facet_wrap(~ staff_type, ncol = 1)  


# ithaca

pbj_2017_ithaca <- newyork_2017 %>%
  filter(PROVNUM %in% c("335017", "335249", "335225", "335793"))

tidy_ithaca <- pbj_2017_ithaca %>%
  select(WorkDate, day, PROVNAME, cna_hprd, lpn_hprd, rn_hprd) %>%
  gather(key = 'staff_type', value = 'hprd', 4:6)

tidy_ithaca %>% filter(staff_type == "cna_hprd") %>%
  ggplot(tidy_beechtree, aes(x=WorkDate, y=hprd)) +
  geom_line() +
  #scale_y_continuous(limits = c(0, 3.0), breaks = seq(0,5, by = 0.5)) +
  facet_wrap(~ PROVNAME, ncol = 1)  

