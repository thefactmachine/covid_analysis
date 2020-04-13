rm(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)
library(stringr)
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)

install.packages("coronavirus")

library(coronavirus)
df_dv_data <- coronavirus

vct_names <- c("province", "country", "lat", "long", "date", "cases", "type")
names(df_dv_data) <- vct_names

vct_join <- vct_names[c(1, 2, 5, 7)]
names(vct_join) <- vct_join

df_dv_data$province <- ifelse(df_dv_data$province == "", 
                              "blank", df_dv_data$province )




# 72 x 2
df_prov_country <- df_dv_data %>% 
  select(province, country) %>% distinct()

vct_date <- df_dv_data$date %>% unique()
vct_type <- df_dv_data$type %>% unique()


# [78 x 2]
df_date_type <- expand.grid(vct_date, vct_type , KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

df_lu <- df_prov_country %>% tidyr::crossing(df_date_type)





df_prov_country %>% full_join(df_date_type, by = character()) %>% dim()


fn_exp_grid(vct_date, vct_type)

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
  select(province, country, date, type, value) %>% 
  filter(country %in% c("Australia", "Belgium", "Canada", "Finland", 
                        "France", "Germany","Hong Kong","Italy", "Japan", "Singapore", "South Korea", 
                        "Spain", "Sweden","Taiwan","UK", "US"))





df_data_country <- df_data %>% group_by(country, date, type) %>% 
  summarise(value = sum(value))



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

setwd("/Users/markhatcher/TEMP_not_on_cloud/covid-19/data")




lst_csv <- list.files("/Users/markhatcher/TEMP_not_on_cloud/covid-19/data", full.names = TRUE) %>% 
  lapply(function())



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








library(devtools)
devtools::install_github("tylermorganwall/rayshader")

library(doParallel)
library(progress)
library(rayrender)

library(rayshader)
library(ggplot2)
library(dplyr)

ggdiamonds = ggplot(diamonds) +
  stat_density_2d(aes(x = x, y = depth, fill = stat(nlevel)), 
                  geom = "polygon", n = 100, bins = 10, contour = TRUE) +
  facet_wrap(clarity~.) +
  scale_fill_viridis_c(option = "A")

par(mfrow = c(1, 2))

plot_gg(ggdiamonds, width = 5, height = 5, raytrace = FALSE, preview = TRUE)


plot_gg(ggdiamonds, width = 5, height = 5, multicore = TRUE, scale = 250, 
        zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800))

?plot_gg



render_snapshot(clear = TRUE)
volcano %>%
  sphere_shade() %>%
  plot_3d(volcano, zscale = 2)





render_snapshot()

str_file_name <- paste0("/Users/markhatcher/TEMP_not_on_cloud/COVID-19/csse_covid_19_data/csse_covid_19_time_series", "/test_dplyr.obj")
save_obj(str_file_name)




filename_obj = tempfile(fileext = ".obj")
save_obj(filename_obj)

ggdiamonds = ggplot(diamonds, aes(x, depth)) +
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon", n = 100, bins = 10,contour = TRUE) +
  facet_wrap(clarity~.) +
  scale_fill_viridis_c(option = "A")

plot_gg(ggdiamonds,multicore=TRUE,width=5,height=5,scale=250,windowsize=c(1400,866),
        zoom = 0.55, phi = 30)
render_snapshot()
str_file_name <- paste0("/Users/markhatcher/TEMP_not_on_cloud/COVID-19/csse_covid_19_data/csse_covid_19_time_series", "/test_dplyr.obj")
save_obj(str_file_name)

print("hi there")
Sys.sleep(0.2)
print("bye there")

render_snapshot(clear = TRUE)



x <- getURL("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")


rm(list = ls())
library(RCurl)
library(dplyr)
library(tidyr)

options(stringsasfactors = FALSE)
setwd("/Users/markhatcher/TEMP_not_on_cloud/COVID-19/csse_covid_19_data/csse_covid_19_time_series")

# 225 x 50
df_covid <- read.csv("time_series_19-covid-Confirmed.csv")

df_covid %>% names() %>% tail(1)

# first four columns - distinct by provice and country
# df_first_four %>% distinct(Province.State, Country.Region) %>% nrow()
df_first_four <- df_covid[, 1:4]
# rename and change order
names(df_first_four) <- c("province", "country", "lat", "long")
df_first_four <- df_first_four %>% select(country, province, lat, long)


df_first_four$country <- as.character(df_first_four$country)
# 225 x 50
# 225 x (50 - 4)
df_covid_long <- cbind(df_first_four, df_covid[, 5:ncol(df_covid)]) %>% 
  tidyr:: gather("date", "cum_num", -country, -province, -lat, -long) %>%
  mutate(date = gsub("X", "0", date) %>% as.Date(format = "%m.%d.%y"))

# will be lots of zeros due to the gather process
df_covid_long_country <- df_covid_long %>% 
  group_by(country, date) %>% 
  summarise(tot_cum_num = sum(cum_num)) %>% 
  filter(tot_cum_num != 0)

# let get the most recent cumulative numbers for each country
df_covid_long_country %>% arrange(country, date) %>% 
  group_by(country) %>% slice(n()) %>% ungroup() %>%
  arrange(desc(tot_cum_num)) %>% as.data.frame() %>% head(30)

# this block is better
df_covid_long_country  %>% 
  group_by(country) %>% arrange(date) %>% slice(n()) %>% ungroup() %>%
  arrange(desc(tot_cum_num)) %>% as.data.frame() %>% head(30)


# now store them as a vector.
vct_pop_countries <- df_covid_long_country %>% 
  arrange(country, date) %>% 
  group_by(country) %>% slice(n()) %>% ungroup() %>%
  arrange(desc(tot_cum_num)) %>% as.data.frame() %>% head(30) %>%
  pull(country)

# now we have our structure of popular countries
df_pop_country <- df_covid_long_country %>% 
  filter(country %in% vct_pop_countries)

fn_zero_diff <- function(q) c(0, diff(q))


df_pop_country_gwth <- df_pop_country %>% group_by(country) %>% 
  arrange(country, date) %>% mutate(diff = fn_zero_diff(tot_cum_num)) %>%
  mutate(pro_inc = tot_cum_num / (tot_cum_num - diff)) %>% 
  ungroup() %>% arrange(country, date) %>% as.data.frame() 




df_top_country <- df_pop_country_gwth %>% group_by(country) %>% arrange(country, date) %>% 
  slice(n()) %>% ungroup() %>% arrange(desc(pro_inc)) 

df_top_country$double <- log10(2) / log10(df_top_country$pro_inc)


df_top_country %>% filter(tot_cum_num > 60) %>%  as.data.frame() %>% arrange(desc(pro_inc))








# install.packages("rgl")
library(rgl)

open3d()
plot3d(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, type = "s", col = as.numeric(iris$Species))

str_file_name <- paste0("/Users/markhatcher/TEMP_not_on_cloud/COVID-19/csse_covid_19_data/csse_covid_19_time_series", "/test_dplyr.obj")
writeOBJ(str_file_name)


str_file_name <- paste0("/Users/markhatcher/TEMP_not_on_cloud/COVID-19/csse_covid_19_data/csse_covid_19_time_series", "/simple_test.obj")
open3d()
shade3d( icosahedron3d() )
writeOBJ(str_file_name)



setRepositories(graphics = getOption("menu.graphics"),
                ind = NULL, addURLs = character())
install.packages("ggplot2", repos = 'https://cran.csiro.au')
install.packages("Quandl", repos = 'https://cran.csiro.au')
install.packages("quantmod", repos = 'https://cran.csiro.au')
library(Quandl)
str_key <- "kfH8V1qTrsnx-bFxGKTA"

mydata = Quandl("OPEC/ORB")


df_pop_country_gwth %>% 
  
  library(quantmod)
aa <- getSymbols("INDEXSP", src = "yahoo")

xtc_result <- getSymbols("AAPL", from="1997-12-31", src="yahoo", auto.assign = FALSE)
xtc_result %>% str()
# this one is s and p 500
xtc_s_p_test <- getSymbols("^GSPC", from="1997-12-31", src="yahoo", auto.assign = FALSE)

getSymbols("QQQ;SPY", from="1997-12-31", src='yahoo')  


xtc_asx_test <- getSymbols("^AXJO", from="1997-12-31", src="yahoo", auto.assign = FALSE)










df_covid_long_country %>% filter(country == "Hong Kong") %>% arrange(date) %>% View()
df_covid_long_country %>% filter(country == "Italy") %>% arrange(date) %>% View()
df_covid_long_country %>% filter(country == "Germany") %>% arrange(date) %>% View()
df_covid_long_country %>% filter(country == "Australia") %>% arrange(date) %>% View()
df_covid_long_country %>% filter(country == "Switzerland") %>% arrange(date) %>% View()
df_covid_long_country %>% filter(country == "Japan") %>% arrange(date) %>% View()
df_covid_long_country %>% filter(country == "UK") %>% arrange(date) %>% View()







df_covid_long_country %>% group_by(country) %>% arrange(date) %>% slice(n())





df_covid_2[1:10, 1:10] %>% gather("date", "cum_num", -country, -province, -lat, -long) %>% nrow()



library(dplyr)

#Sample dataframe
mydfdata.frame(x=rep(letters[1:9]),
               day=c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun","Fri","Mon"))

#1.Create the 7 dummy variables separately
daysdummy<-sjmisc::to_dummy(mydf$day,suffix="label")

#2. append to dataframe
mydf<-bind_cols(mydf,daysdummy)



mydf %>% 
  mutate(var = 1) %>% 
  spread(day, var, fill = 0, sep = "_") %>% 
  left_join(mydf) %>% 
  select(x, day, everything())


library(tidyr)
df <- data.frame(id = c(1,1,2,3,4), fruit = c("apple","pear","apple","orange","apple"))
df %>% mutate(i = 1) %>% spread(fruit, i, fill = 0)



















