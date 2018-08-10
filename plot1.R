library(readxl)
library(dplyr)
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

PDXweather <- read_csv("data/NCDC-CDO-USC00356750.csv")

bikecounts_weather <- bikecounts %>% 
  mutate(date=as_date(date)) %>%
  left_join(PDXweather, by=c("date"="DATE"))

library(ggplot2)

count_plot <- ggplot(data = bikecounts_weather) + 
  geom_point(mapping = aes(x = date, y = total, color = name))

