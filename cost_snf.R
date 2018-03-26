
library(tidyverse)
library(readr)
library(stringr)
providers <- read_csv("../large_files/SNF10-REPORTS/snf10_PRVDR_ID_INFO.CSV")

beechtree <- providers %>%
  filter(str_detect(snf10_Name, regex('beechtree', ignore_case = T)))

snf10_2016_RPT <- read_csv("../large_files/SNF10FY2016/snf10_2016_RPT.CSV", 
                           col_names = FALSE)

beechtree_report <- snf10_2016_RPT %>%
  filter(X3 == beechtree$PROVIDER_NUMBER)


snf10_2016_ALPHA <- read_csv("../large_files/SNF10FY2016/snf10_2016_ALPHA.CSV", 
                             col_names = FALSE)

beechtree_alpha <- snf10_2016_ALPHA %>%
  filter(X1 == beechtree_report$X1)


snf10_2016_NMRC <- read_csv("../large_files/SNF10FY2016/snf10_2016_NMRC.CSV", 
                            col_names = FALSE)

beechtree_nmrc <- snf10_2016_NMRC %>%
  filter(X1 == beechtree_report$X1)

df_glimpse <- beechtree_nmrc %>%
  filter(str_detect(X2, regex('S3', ignore_case = T))) %>%
  arrange(desc(X3))

