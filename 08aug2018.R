library(readxl)
library(dplyr)
library(tidyverse)

input_file <- "data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx"
load_data <- function (input_file, bridge_name) {
  bikecounts <- read_excel(input_file, sheet=bridge_name, skip=1)
  bikecounts$name <- bridge_name
  bikecounts
}

Tilikum <- load_data(input_file, "Tilikum")
Hawthorne <- load_data(input_file, "Hawthorne")

# use the column names of Tilikum for Hawthorne
names(Hawthorne) <- names(Tilikum)

Steel <- load_data(input_file, "Steel")
names(Steel) <- c("date", "lower", "westbound", "eastbound", "total", "name")

# combine all three data frame for all three bridges
bikecounts <- bind_rows(Hawthorne, 
                        Tilikum, 
                        Steel %>% select(-lower)) # exclude the `lower` col in Steel data frame

PDXweather <- read_csv("data/NCDC-CDO-USC00356750.csv")
iris <- read_dta("data/iris.dta")

bikecounts_dir <- bikecounts %>% gather ("westbound","eastbound", key="direction",value="count")
bikecounts$day <- wday(bikecounts$date, label = TRUE)
bikecounts <- bikecounts %>% mutate(dow=wday(date, label=TRUE),
                      lntotal=log(total)
                      )

(bikecounts_days <- bikecounts %>% group_by(name, day) %>% 
  summarize(mean(total)))

filter(bikecounts, is.na(westbound) | is.na(eastbound)) %>% group_by(name, date) %>% 
  tally
filter(PDXweather, is.na(SNOW)) %>% tally

bikecounts %>% mutate(year=year(date)) %>% group_by (year) %>% summarize(annual_total = sum(total))

filter(bikecounts) %>% group_by(name,year) %>% summarize(total)

bikecounts %>% filter(name!="Steel") %>% group_by (name) %>% summarize(annual_total = sum(total))

bikecounts %>% group_by (name) %>% summarize(annual_total = sum(total))

bikecounts_weather <- bikecounts %>% left_join(PDXweather, by(date))

filter(bikecounts, is.na(total)) %>% tally
filter(PDXweather, is.na(DATE)) %>% tally

bikecounts_weather <- bikecounts %>% 
  mutate(date=as_date(date)) %>%
  inner_join(PDXweather, by=c("date"="DATE"))

bikecounts %>% mutate(month=month(date)) %>% group_by (month) %>% summarize(monthly_total = sum(total))

bikecounts %>% mutate(dow=wday(date, label=TRUE)) %>% group_by(dow) %>%
                                      summarize(daily_total = sum(total))

bikecounts %>% 
  group_by (name) %>%
  filter(min_rank(desc(total))<=3) %>%
  arrange(name, rank)

bikecounts %>%
  mutate(week=floor_date(date, "week")) %>%
  group_by(name, week) %>%
  summarize(weekly_total = sum(total))

nhts <- read_csv("https://raw.githubusercontent.com/cities/datascience2017/master/data/NHTS2009_dd.csv")
nhts %>% group_by(HOUSEID) %>% summarize(total_hh_miles = sum(TRPMILES))

nhts %>% mutate(driving=case_when(
  TRPTRANS %in% c("01", "02", "03", "04", "05", "06", "07") ~ 1,
  TRPTRANS %in% c("-1", "-7", "-8", "-9") ~ as.double(NA),
  TRUE ~0)) %>%
  filter(driving==1) %>%
  group_by(HOUSEID) %>% 
  summarize(total_vmt=sum(TRPMILES))
))

