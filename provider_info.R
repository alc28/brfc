# provider_info.R
# by Adam Chandler

library(tidyverse)

provider_info <- read_csv("data/medicare.gov/Provider_Info.csv")

beechtree <- provider_info %>%
  filter(`Federal Provider Number` == "335017")

df_tidy <- beechtree %>%
  gather(key = 'metric', value = 'value')

write_csv(df_tidy, "output/beechtree_provider_info.csv")
