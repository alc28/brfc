
library(tidyverse)
library(readr)
providers <- read_csv("../large_files/SNF10-REPORTS/snf10_PRVDR_ID_INFO.CSV")

beechtree <- providers %>%
  filter(str_detect(snf10_Name, regex('beechtree', ignore_case = T)))




