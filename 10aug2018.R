library(purrr)
library(broom)
library(readxl)
library(tidyverse)
library(lubridate)

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

# bikecounts_tidy <- bikecounts %>% spread (name, total)
weather <- read_csv("data/NCDC-CDO-USC00356750.csv")
bikecounts_weather <- bikecounts %>% 
  mutate(date=as_date(date)) %>%
  left_join(weather, by=c("date"="DATE")) %>% 
  select (date, name, total, PRCP, TMAX, TMIN)

bikecounts_nested <- bikecounts_weather %>% 
  group_by(name) %>%
  nest()

count_vs_prcp <- function(df) {
  lm(total ~ PRCP, data = df)
}

bikecounts_prcp <- bikecounts_nested %>% 
  mutate(fit = purrr::map(data, count_vs_prcp),
         tidy = purrr::map(fit, tidy)) %>% 
  select(name, tidy) %>% 
  unnest(tidy)

ggplot(bikecounts_prcp, aes(x = estimate)) +
  geom_density() + geom_rug() + facet_wrap(~ term, scales = "free")

count_vs_tmax <- function(df) {
  lm(total ~ TMAX, data = df)
}

bikecounts_tmax <- bikecounts_nested %>% 
  mutate(fit = purrr::map(data, count_vs_tmax),
         tidy = purrr::map(fit, tidy)) %>% 
  select(name, tidy) %>% 
  unnest(tidy)

ggplot(bikecounts_tmax, aes(x = estimate)) +
  geom_density() + geom_rug() + facet_wrap(~ term, scales = "free")

count_vs_weather <- function(df) {
  lm(total ~ TMIN+TMAX+PRCP, data = df)
}

bikecounts_weather_lm <- bikecounts_nested %>% 
  mutate(fit = purrr::map(data, count_vs_weather),
         tidy = purrr::map(fit, tidy),
         glance = purrr::map(fit, glance))

bikecounts_weather_lm %>% 
  select(name, tidy) %>% 
  unnest(tidy)

bikecounts_weather_lm %>% 
  select(name, glance) %>% 
  unnest(glance)

ggplot(bikecounts_tmin, aes(x = estimate)) +
  geom_density() + geom_rug() + facet_wrap(~ term, scales = "free")

