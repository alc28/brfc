# eda1

library(tidyverse)
library(readr)

newyork_2017 <- read_csv("~/beechtree_consolidated/brfc/data/newyork_2017.csv")

#newyork_2017 <- newyork_2017 %>%
#  mutate(total)

pbj_2017_beechtree <- newyork_2017 %>%
  filter(PROVNUM == '335017')

df1 <- pbj_2017_beechtree %>% 
  group_by(CY_Qtr) %>%
  summarise_all(c("min", "mean", "max")) %>%
  select(matches("CY_QTR|hprd"))


