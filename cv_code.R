rm(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)
library(openxlsx)
library(stringr)



library(coronavirus)
df_dv_data <- coronavirus

vct_names <- c("province", "country", "lat", "long", "date", "cases", "type")
names(df_dv_data) <- vct_names

vct_join <- vct_names[c(1, 2, 5, 7)]
names(vct_join) <- vct_join


# make sure that all combinations of provinces, countries,
# date, type are populated.
df_data <- expand.grid(province = unique(df_dv_data$province), 
            country = unique(df_dv_data$country), 
            date = unique(df_dv_data$date), 
            type = unique(df_dv_data$type), 
            val = 0, 
            KEEP.OUT.ATTRS = FALSE,
            stringsAsFactors = FALSE) %>% 
  left_join(df_dv_data, by = vct_join) %>% 
  mutate(value = ifelse(is.na(cases), val, cases)) %>% 
  mutate(province = ifelse(province == "", "BLANK", province)) %>%
  select(province, country, date, type, value) 

library (readr)

str_file <- "/Users/markhatcher/Documents/new_ways_of_working/google-location-coronavirus/2020-03-29.tsv"
df <- readr::read_tsv(str_file)

# we have 4 rows and 3 column (act is rows 1 and 2, nsw is rows 3 and 4)
df %>% filter(country_name == "Australia") %>% filter(page == 3) %>% select(row, col) %>% distinct()

# act retail
df %>% filter(country_name == "Australia") %>% filter(page == 3) %>% filter(row == 1 & col == 1)





mydata <- read.table(str_file, sep = '\t', header = TRUE)

library (readr)


urlfile = "https://raw.githubusercontent.com/nacnudus/google-location-coronavirus/master/2020-03-29.tsv"
mydata <- read.table(url(urlfile), sep = '\t', header = TRUE)

library(RCurl)
x <- getURL(urlfile)
df <- readr::read_tsv(x)


mydata <- read.table(x, sep = '\t', header = TRUE)

readr::read_tsv('drug_info.tsv')


read.table(file = 'drug_info.tsv', sep = '\t', header = TRUE)


setwd("/Users/markhatcher/TEMP_not_on_cloud/COVID-19/csse_covid_19_data/csse_covid_19_time_series")

# https://github.com/CSSEGISandData/COVID-19.git

rm(list=ls())
library("RCurl")
library(rworldmap)
library(rworldxtra)

x <- getURL("https://raw.githubusercontent.com/thefactmachine/k-means/master/countryData.csv")
dfComplete <- read.csv(text = x)




fn_date <- function(q) gsub("X", "0", q) %>% as.Date(format = "%m.%d.%y")
fn_diff <- function(z) c(0, diff(z))



df_first_four <- df_raw[, 1:4]
df_remainder <- df_raw[, 5:ncol(df_raw)]
names(df_first_four) <- c("province", "country", "lat", "long")


df_raw_names <- cbind(df_first_four, df_remainder)

# australia
df_init_long_aust <- df_raw_names %>% filter(country == "Australia") %>%
  tidyr::gather("date", "cum_val", -province, -country, -lat, -long) %>%
  mutate(date = fn_date(date)) %>% select(country, province, date, cum_val) 


df_init_long_pc_aust <- df_init_long_aust %>%
  arrange(country, date) %>% group_by(country) %>%
  mutate(incr = fn_diff(cum_val)) %>% ungroup %>%
  arrange(country, desc(date)) %>% group_by(country) %>%
  slice(1:5) %>% ungroup() %>% arrange(country, date) %>%
  mutate(pc_diff = ifelse(incr == 0 | cum_val == 0 |
                            incr == cum_val, 1, 1 + (incr / (cum_val - incr))))





df_init_long <- df_raw_names %>%
  tidyr::gather("date", "cum_val", -province, -country, -lat, -long) %>%
  mutate(date = fn_date(date)) %>% select(country, date, cum_val) %>%
  group_by(country, date) %>% summarise(cum_val_tot = sum(cum_val, na.rm = TRUE))


df_top_country <- df_init_long %>% arrange(country, date) %>%
  group_by(country) %>% slice(n()) %>%
  ungroup() %>% arrange(desc(cum_val_tot)) %>%
  filter(cum_val_tot > 50)


df_init_long_pc <- df_init_long %>%
  filter(country %in% df_top_country$country) %>%
  arrange(country, date) %>% group_by(country) %>%
  mutate(incr = fn_diff(cum_val_tot)) %>% ungroup %>%
  arrange(country, desc(date)) %>% group_by(country) %>%
  slice(1:5) %>% ungroup() %>% arrange(country, date) %>%
  mutate(pc_diff = ifelse(incr == 0 | cum_val_tot == 0 |
  incr == cum_val_tot, 1, 1 + (incr / (cum_val_tot - incr))))


vct_names <- c("country" , rev(paste0("t", c("", "-1", "-2", "-3", "-4"))))

df_init_wide_pc <- df_init_long_pc %>% select(country, date, pc_diff) %>%
  spread(date, pc_diff)

names(df_init_wide_pc) <- vct_names

df_top_country %>% nrow()

df_fin <- df_init_wide_pc %>%
  inner_join(df_top_country[, c("country", "cum_val_tot")],
  by = c("country" = "country"))



df_fin$cum_prod <- df_fin[, c(2, 3, 4, 5, 6)] %>% apply(1, prod)
names(df_fin) <- c("country", "t_4", "t_3", "t_2", "t_1", "t", "cum_val", "cum_prod")
df_fin %>% arrange((country)) %>% View()


df_fin$cagr <- df_fin$cum_prod ^ (1/5)

vct_first_world <- c("Australia", "Austria", "Belgium", "Canada",
                    "China", "Denmark" ,"Estonia", "Finland", "France",
                    "Germany", "Greece", "Iceland", "Ireland",
                     "Italy", "Japan", "Korea, South", "Netherlands",
                     "Norway", "Portugal", "Singapore", "South Africa","Spain","Sweden",
                     "Switzerland", "Taiwan*", "United Kingdom", "US")


df_fin %>% filter(country %in% vct_first_world) %>% View()










# finally create the spreadsheet
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "cv_19")
openxlsx::writeData(wb,  "cv_19", df_fin)
openxlsx::saveWorkbook(wb, file = "cv_19.xlsx", overwrite = TRUE) 
df_fin %>% arrange((country)) %>% View()