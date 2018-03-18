# ithaca.R

library(tidyverse)
library(readr)
library(lubridate)
library(stringr)
library(forcats)
library(ggthemes)
library(readxl)


get_mphrd <- function(mds, cna_hours) {
  cna_minutes = cna_hours * 60
  (cna_minutes / mds) / 24
}


pbj_2017_all <- read_csv("data/medicare.gov/newyork_2017_q1_q3.csv")
usg_lookup <- read_excel("data/upstate_services_group/usg_lookup.xlsx")

pbj_2017_all$WorkDate <- ymd(pbj_2017_all$WorkDate)
pbj_2017_all$day <- wday(as.Date(pbj_2017_all$WorkDate,'%Y-%m-%d'), label=TRUE)
day_levels <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")
pbj_2017_all$day <- factor(pbj_2017_all$day, levels = day_levels)


pbj_2017_all <- pbj_2017_all %>%
  mutate(cna_hprd = round((hrs_CNA / MDScensus),2), cna_mphrd = round(get_mphrd(MDScensus, hrs_CNA),2), lpn_hprd = round((hrs_LPN / MDScensus),2), rn_hprd = round((hrs_RN / MDScensus),2))


# ithaca providers
# beechtree 335017
# cayuga ridge 335249
# oak hill manor 335225
# kendal 335793
#filter(item1 %in% c("collections", "users", "libraries")) %>%

pbj_2017_usg<- pbj_2017_all %>%
  filter(PROVNUM %in% usg_lookup$id_fed_provname)

df <- pbj_2017_usg %>% 
  group_by(PROVNAME) %>%
  summarize(cna_to_census = round(mean(cna_hprd),2)) %>%
  arrange(desc(cna_to_census))


tidy_summary <- gather(df, key = "key", value = "value", 2:2)
tidy_summary$key[tidy_summary$key == "cna_to_census"] <- "Average daily CNA hours per resident"

ggplot(tidy_summary, aes(x = reorder(PROVNAME, value), value, label=value)) +
  geom_label(size = 3) +
 # annotate("text", x=-Inf, y=Inf, label = paste("Average = ", mean(df$cna_to_census)), size = 4, vjust=-22, hjust=1.5) +
  facet_grid(~key) +
  labs(x = "Facility",
       y = "CNA hours per resident day",
       title = "Average CNA hours per resident day, 2017 Jan - Sep",
       subtitle = "Long term care facilities owned by Uri Koenig (Upstate Services Group)",
       caption = "Data source: Payroll-Based Journal data, medicare.gov ") +
  scale_y_continuous(breaks=seq(0, 3, 0.2)) +
  theme(axis.text.x = element_text(vjust=.25, size = 7)) +
  coord_flip()

ggsave(plot=last_plot(),"output/usg_cna_hprd_2017_jan-sep.png", width=10, height = 6)




