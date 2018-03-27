# provider adjusted
# 
# URL: https://data.medicare.gov/api/views/4pq5-n9py/rows.csv?accessType=DOWNLOAD
# From the dataset abstract
# 
# General information on currently active nursing homes, including number of certified beds, quality measure scores, staffing and other information used in the Five-Star Rating System. Data...
# 
# Source: Provider Info


library(tidyverse)
library(readr)
library(lubridate)
library(stringr)
library(forcats)
library(ggthemes)

clean_df_names <- function(df, i) {
  for (i in seq_along(df) ) {
    newname <- str_to_lower(names(df)[i] )
    newname <- str_replace_all(newname, " ", "_")
    print(newname)
    names(df)[i] <- newname
  }
  return(df)
}

provider_info <- read_csv("data/medicare.gov/Provider_Info.csv")
provider_info <- clean_df_names(provider_info)

# Ithaca
ithaca <- provider_info %>%
  filter(federal_provider_number %in% c("335017", "335249", "335225", "335793")) %>%
  select(provider_name, reported_cna_staffing_hours_per_resident_per_day, expected_cna_staffing_hours_per_resident_per_day) %>%
  mutate(diff = reported_cna_staffing_hours_per_resident_per_day - expected_cna_staffing_hours_per_resident_per_day ) %>%
  mutate(diff = round(diff,2))





# USG
# usg_lookup <- read_excel("data/upstate_services_group/usg_lookup.xlsx")

# provider_info_usg<- provider_info %>%
#   filter(`Federal Provider Number` %in% usg_lookup$id_fed_provname) %>%
# select(`Provider Name`, `Reported CNA Staffing Hours per Resident per Day`, `Expected CNA Staffing Hours per Resident per Day`) %>%
#   mutate(diff = `Reported CNA Staffing Hours per Resident per Day` - `Expected CNA Staffing Hours per Resident per Day`) %>%
#   mutate(diff = round(diff,2))






