rm(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)


# location code: /Users/markhatcher/Documents/new_ways_of_working/covid_analysis
# location data Users/markhatcher/TEMP_not_on_cloud/covid-19

vct_file_path <- list.files("/Users/markhatcher/TEMP_not_on_cloud/covid-19/data", full.names = TRUE)
vct_names <- list.files("/Users/markhatcher/TEMP_not_on_cloud/covid-19/data", full.names = FALSE) %>%
  gsub(".csv", "", .)
lst_csv <- lapply(vct_file_path, read.csv)
names(lst_csv) <- vct_names

vct_country <- c("Australia", "Belgium", "Canada", "Finland", "France", "Germany",
                "Hong Kong","Italy", "Japan", "Singapore", "South Korea", 
                "Spain", "Sweden","Taiwan","UK", "US")




# countries-aggregated` no provinces.
# selection of countries as a column
# time-series-19-covid-combined`  provinces
# us_confirmed spatial US
# us_death spatial US
# worldwide-aggregated world totals per day

df_agg <- lst_csv$`countries-aggregated` %>% 
  filter(Country %in% vct_country) %>% 
  rename_all(tolower) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

df_agg_30 <- df_agg %>% arrange(desc(date)) %>%
  group_by(date) %>% slice(1:30) %>%
  ungroup()



