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


#pbj_2017_q1 <- read_csv("data/medicare.gov/PBJ_Daily_Nurse_Staffing_2017_Q1.csv")
#pbj_2017_q2 <- read_csv("data/medicare.gov/PBJ_Daily_Nurse_Staffing_2017_Q2.csv")
#pbj_2017_q3 <- read_csv("data/medicare.gov/PBJ_Daily_Nurse_Staffing_2017_Q3.csv")

pbj_2017 <- read_csv("data/newyork_2017.csv")
#medicare_ownership <- read_csv("data/medicare.gov/Ownership_Download.csv", col_types = cols(ZIP = col_character()))

#beechtree_by_provnum <- medicare_ownership %>%
#  filter(PROVNUM == '335017')

pbj_2017_all <- pbj_2017

df <- head(pbj_2017_all)
df$WorkDate <- ymd(df$WorkDate)
df$day <- wday(as.Date(df$WorkDate,'%Y-%m-%d'), label=TRUE)

pbj_2017_all$WorkDate <- ymd(pbj_2017_all$WorkDate)
pbj_2017_all$day <- wday(as.Date(pbj_2017_all$WorkDate,'%Y-%m-%d'), label=TRUE)


day_levels <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")
pbj_2017_all$day <- factor(pbj_2017_all$day, levels = day_levels)


#pbj_2017_all$day <- ymd(pbj_2017_all$WorkDate)
#pbj_2017_all$day <- wday(as.Date(pbj_2017_all$WorkDate,'%Y-%m-%d'), label=TRUE)

pbj_2017_all <- pbj_2017_all %>%
  mutate(cna_hprd = round((hrs_CNA / MDScensus),2), cna_mphrd = round(get_mphrd(MDScensus, hrs_CNA),2), lpn_hprd = round((hrs_LPN / MDScensus),2), rn_hprd = round((hrs_RN / MDScensus),2))
  



# Beechtree only

pbj_2017_beechtree <- pbj_2017_all %>%
  filter(PROVNUM == '335017')


# Visualize

#df_beechtree$day <- factor(df_beechtree$day, levels = day_levels)

df_beechtree <- pbj_2017_beechtree
df_beechtree$mycaption <- NA
df_beechtree$mycaption[df_beechtree$WorkDate == "2017-04-13"] <- "Thurs, Apr 13\n 2.55 HPRD = 6min 30sec per hour\n\n\n"
#df_beechtree$mycaption[df_beechtree$WorkDate == "2017-06-21"] <- "Jun 21\n"
df_beechtree$mycaption[df_beechtree$WorkDate == "2017-09-12"] <- "\n\n\nTues, Sep 12\n0.92 HPRD = 2min 20sec per hour"

# hours per resident day
ggplot(df_beechtree, aes(WorkDate, cna_hprd, label = mycaption), vjust = 0.3) +
  theme_grey() +
#  theme(legend.position = c(0.950,0.80), plot.title = element_text(size = rel(2)), plot.caption = element_text(size = rel(0.6)), axis.text.y = element_text(size = rel(1.5))) +
    geom_line(size = 0.1) +
  scale_x_date(date_breaks="1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0.5, 3.0), breaks = seq(0,5, by = 0.5)) +
  ggtitle("\nCNA hours per resident day (HPRD)", subtitle = "Beechtree Center for Rehabilitation and Nursing, Ithaca NY") +
  xlab("January 1 - December 31, 2017") +
  ylab("Hours") +
  labs(caption = paste("HPRD = total CNA hours / number of residents \nData source: https://data.cms.gov/browse?q=PBJ \nCompiled by Adam Chandler, Beechtree Family Council,", now(), "\n\n")) +
  geom_text(data=subset(df_beechtree, !is.na(mycaption)), size = rel(3)) +
  geom_point(size = 1.35, aes(fill = day), shape = 21) + scale_fill_brewer("day") 
#  theme(legend.position = c(0.68,0.82), plot.title = element_text(size = rel(2)), plot.caption = element_text(size = rel(0.6)), axis.text.y = element_text(size = rel(1.5)))
  
   
ggsave(plot = last_plot(), filename = "output/cna_hprd-2017_8.5x14.pdf", height = 8.5, width = 14, units = "in", dpi = 300)



# tidy df

ggplot(data = df_beechtree) + 
  geom_line(mapping = aes(x = WorkDate, y = cna_hprd)) +
  scale_x_date(date_breaks="1 month", date_labels = "%b")

ggplot(df_beechtree, aes(x = day, y = cna_hprd)) +
  geom_boxplot()
  
tidy_beechtree <- df_beechtree %>%
  select(WorkDate, cna_hprd, lpn_hprd, rn_hprd) %>%
  gather(key = 'staff_type', value = 'hprd', 2:4)

ggplot(tidy_beechtree, aes(x=WorkDate, y=hprd)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 3.0), breaks = seq(0,5, by = 0.5)) +
  facet_wrap(~ staff_type, ncol = 1)


# summaries
  
byday_beechtree_cna_minutes <- pbj_2017_beechtree %>%
  group_by(day) %>%
  summarize(numdays = n(), mean_cna_mphrd = round(mean(cna_mphrd),2), sd_cna_mphrd = round(sd(cna_mphrd),2), min_cna_mphrd = round(min(cna_mphrd),2), max_cna_mphrd = round(max(cna_mphrd),2) )

byday_beechtree_cna_hours <- pbj_2017_beechtree %>%
  group_by(day) %>%
  summarize(numdays = n(), mean_cna_hprd = round(mean(cna_hprd),2), sd_cna_hprd = round(sd(cna_hprd),2), min_cna_hprd = round(min(cna_hprd),2), max_cna_hprd = round(max(cna_hprd),2) )

  
  
pbj_2017_all %>%
  filter(grepl("KENDAL", PROVNAME)) %>%
  select(PROVNUM, PROVNAME, WorkDate, starts_with("cna"))

df_beechtree %>%
  filter(cna_mphrd > 6.2) %>%
  select(WorkDate, cna_hprd, cna_mphrd, mycaption)
  #glimpse()  

# 
# 
# ggplot(mtcars, aes(wt, mpg, label=name)) +
#   geom_point() +
#   geom_text(data=subset(mtcars, wt > 4 | mpg > 25))
  