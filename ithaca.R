# ithaca.R

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


pbj_2017_all <- read_csv("data/newyork_2017.csv")
# pbj_2017_all$WorkDate <- ymd(pbj_2017_all$WorkDate)
# pbj_2017_all$day <- wday(as.Date(pbj_2017_all$WorkDate,'%Y-%m-%d'), label=TRUE)
# day_levels <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")
# pbj_2017_all$day <- factor(pbj_2017_all$day, levels = day_levels)


# pbj_2017_all <- pbj_2017_all %>%
#   mutate(cna_hprd = round((hrs_CNA / MDScensus),2), cna_mphrd = round(get_mphrd(MDScensus, hrs_CNA),2), lpn_hprd = round((hrs_LPN / MDScensus),2), rn_hprd = round((hrs_RN / MDScensus),2))
# 

# ithaca providers
# beechtree 335017
# cayuga ridge 335249
# oak hill manor 335225
# kendal 335793
#filter(item1 %in% c("collections", "users", "libraries")) %>%

pbj_2017_ithaca <- pbj_2017_all %>%
  filter(PROVNUM %in% c("335017", "335249", "335225", "335793"))

df_ithaca <- pbj_2017_ithaca %>% 
  group_by(PROVNAME) %>%
  summarize(cna_to_census = round(mean(cna_hprd),2)) %>%
  arrange(desc(cna_to_census))


tidy_summary <- gather(df_ithaca, key = "key", value = "value", 2:2)
tidy_summary$key[tidy_summary$key == "cna_to_census"] <- "Average daily CNA hours per resident"

tidy_summary$PROVNAME[tidy_summary$PROVNAME == "BEECHTREE CENTER FOR REHABILITATION AND NURSING"] <- "Beechtree"
tidy_summary$PROVNAME[tidy_summary$PROVNAME == "OAK HILL MANOR NURSING HOME"] <- "Oak Hill Manor"
tidy_summary$PROVNAME[tidy_summary$PROVNAME == "CAYUGA RIDGE EXTENDED CARE"] <- "Cayuga Ridge"
tidy_summary$PROVNAME[tidy_summary$PROVNAME == "KENDAL AT ITHACA"] <- "Kendal at Ithaca"


# [FC understanding of the current starting wages for CNAs in Ithaca are: Beechtree: $12.50, Cayuga Ridge: $13.75, Oak Hill: $14.25, Kendal: $16.25.]

tidy_summary$mycaption <- NA
tidy_summary$mycaption[tidy_summary$PROVNAME == "Beechtree"] <- "$12.50"
tidy_summary$mycaption[tidy_summary$PROVNAME == "Oak Hill Manor"] <- "$14.25"
tidy_summary$mycaption[tidy_summary$PROVNAME == "Cayuga Ridge"] <- "$13.75"
tidy_summary$mycaption[tidy_summary$PROVNAME == "Kendal at Ithaca"] <- "$16.25"

#
# ggplot(tidy_summary, aes(x = reorder(PROVNAME, value), value)) +
#   geom_col(width = 0.3) +
#   facet_grid(~key) +
#   labs(x = "Facility",
#        y = "CNA hours per resident day",
#        title = "Average CNA hours per resident day, 2017 Jan - Sep",
#        subtitle = "Long term care facilities in Ithaca, NY",
#        caption = "Data source: Payroll-Based Journal data, medicare.gov ") +
#   scale_y_continuous(breaks=seq(0, 3, 0.1)) +
#   theme(axis.text.x = element_text(vjust=.25, size = 6))
# 

#ggplot(tidy_summary, aes(x = reorder(PROVNAME, value), value, label=value)) +
ggplot(tidy_summary, aes(x = reorder(PROVNAME, value), value, label=paste(value, "\n", "(", mycaption, ")", sep = ""   )  )) +
  geom_label(size = 4) +
 # facet_grid(~key) +
  labs(x = "Facility",
       y = "CNA hours per resident day",
       title = "Average CNA hours per resident day, 2017 (with April 2018 starting CNA wage)",
       subtitle = "Long term care facilities in Ithaca, NY",
       caption = "Data source: Payroll-Based Journal datasets, http://data.cms.gov/ ") +
  scale_y_continuous(breaks=seq(0, 3, 0.2)) +
  theme(axis.text.x = element_text(vjust=.25, size = 7)) +
  coord_flip()


ggsave(plot=last_plot(),"output/Ithaca_cna_hprd_2017.png", width=10, height = 6)


#########

pbj_2017_ithaca %>%
  filter(CY_Qtr > "2017Q1") %>%
  ggplot(aes(WorkDate, cna_hprd)) + geom_smooth(aes(linetype=str_trunc(PROVNAME,10)  ), size = .5) +
  labs(x = "Time",
       y = "CNA hours per resident day",
       linetype = "Facility",
       title = "Trends in CNA staffing, April - December 2017",
       subtitle = "Long term care facilities in Ithaca, NY",
       caption = "Data source: Payroll-Based Journal datasets, http://data.cms.gov/ ")

ggsave(plot=last_plot(),"output/Ithaca_cna_trends_2017.png", width=10, height = 6)

pbj_2017_ithaca %>%
  filter(CY_Qtr > "2017Q1") %>%
  mutate(RNandLPN = lpn_hprd + rn_hprd) %>%
  ggplot(aes(WorkDate, RNandLPN)) + geom_smooth(aes(linetype=str_trunc(PROVNAME,15)  ), size = .5) +
  labs(x = "Time",
       y = "RN plus LPN hours per resident day",
       linetype = "Facility",
       title = "Trends in RN plus LPN staffing, April - December 2017",
       subtitle = "Long term care facilities in Ithaca, NY",
       caption = "Data source: Payroll-Based Journal datasets, http://data.cms.gov/ ")


ggsave(plot=last_plot(),"output/Ithaca_rnandlpn_trends_2017.png", width=10, height = 6)
