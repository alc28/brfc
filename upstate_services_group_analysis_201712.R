# upstate_services_group_analysis_201712


# load libraries
library(tidyverse)
library(readr)
library(readxl)
library(stringr)
library(openxlsx)
library(knitr)


# load data for EDA
usg_lookup <- read_excel("data/upstate_services_group/usg_lookup.xlsx")

ownership_by_upstate_services_group <- read_excel("~/R_projects/beechtree/data/private_upstate_services_group/ownership_by_upstate_services_group.xlsx", n_max = 14)

usg_city <- ownership_by_upstate_services_group %>%
  select(PROVNAME, CITY)

US_SNF_PBJ_Direct_Care_Staff_2017Q2 <- read_excel("data/ltccc/US-SNF-PBJ-Direct-Care-Staff-2017Q2.xlsx",sheet = "US SNF Staffing")


# match data sets

usg_staffing <- US_SNF_PBJ_Direct_Care_Staff_2017Q2 %>% 
  filter(STATE == "NY") %>%
  subset(NAME %in% usg_lookup$name_medicare.gov)

# mutate

for (i in seq_along(usg_staffing)) {
  names(usg_staffing)[i] <- str_replace_all(names(usg_staffing)[i], " ", "_") 
  names(usg_staffing)[i] <- tolower(names(usg_staffing)[i]) 
}

usg_staffing <- usg_staffing  %>%
  mutate(rn_to_census = rn_hours / mds_census) %>%
  mutate(lpn_to_census = lpn_hours / mds_census) %>%
  mutate(cna_to_census = cna_hours / mds_census)

usg_staffing <- usg_staffing %>%
  mutate_if(is.numeric, funs(round(., 2)))

df_usg <- usg_staffing %>% 
  group_by(name) %>%
  summarize(rn_to_census = round(mean(rn_to_census),2), lpn_to_census = round(mean(lpn_to_census),2), cna_to_census = round(mean(cna_to_census),2))

tidy_summary <- gather(df_usg, key = "key", value = "value", 2:4)
tidy_summary$key[tidy_summary$key == "rn_to_census"] <- "Average daily RN hours per resident"
tidy_summary$key[tidy_summary$key == "lpn_to_census"] <- "Average daily LPN hours per resident"
tidy_summary$key[tidy_summary$key == "cna_to_census"] <- "Average daily CNA hours per resident"
tidy_summary <- tidy_summary %>%
  filter(key == "Average daily CNA hours per resident") %>%
  arrange(desc(value))
  

# visualize


ggplot(tidy_summary, aes(name, value)) +
  geom_col() +
  facet_wrap(~key) +
  labs(x = "Facility",
       y = "Hours",
       title = "Average hours per resident day, by direct care staff job category, 2017 Q2",
       subtitle = "Long term care facilities in Ithaca, NY",
       caption = "Data source: Payroll-Based Journal data reported to CMS.gov ; available at http://nursinghome411.org/nursing-home-staffing-2017q2/ ") +
  scale_y_continuous(breaks=seq(0, 3, 0.1)) +
  theme(axis.text.x = element_text(vjust=.25, size = 6))


# aes(x = Estimated.deaths, y = reorder(Who, -Estimated.deaths))) +
ggplot(tidy_summary, aes(x = value, y = reorder(  str_trunc(name,50)  ,value)) ) +
  geom_point(size=5) + 
  labs(x = "Average CNA hours per resident day",
       y = "Facility name",
       title = "Upstate Services Group CNA staffing compared to CMS recommendation (2.8 HPRD)",
       subtitle = "Average CNA hours per resident day, April-June 2017 (Q2)",
       caption = "Data source: http://nursinghome411.org/nursing-home-staffing-2017q2/") +
  scale_x_continuous(limits = c(1.8, 3.0), breaks = seq(1.8,3.0, by = 0.1)) +
  geom_vline(xintercept = 2.8, linetype = "dotted")


ggsave(plot=last_plot(), filename = "output/Upstate Services Group Facilities staffing compared to CMS recommendation Q2 2017.jpg", width = 11, height = 4)


