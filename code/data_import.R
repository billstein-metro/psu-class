


fwf_pos <- fwf_empty("data/NCDC-CDO-USC00356750.txt")
test_df <- read_fwf("data/NCDC-CDO-USC00356750.txt", col_positions = fwf_pos)

test_df <- read_fwf("data/NCDC-CDO-USC00356750.txt", 
                    col_positions = fwf_positions(c(1, 20, 112),
                                                  end=c(19, 69, 120),
                                                  col_names = c("STATION", "NAME", "PRCP")),
                    skip=2)

library(tidyverse)
library(lubridate)
require(readxl)

(tilikum <- read_excel("data/Tilikum Crossing daily bike counts 2015-16 082117.xlsx", skip=1))
hawthorne <- read_excel("data/Hawthorne Bridge daily bike counts 2012-2016 082117.xlsx")

tilikum$bridge <- "Tilikum"
hawthorne$bridge <- "Hawthorne"

names(hawthorne) <- c("date", "westbound", "eastbound", "total", "bridge")

bikecounts <- bind_rows(tilikum, hawthorne) %>% 
  mutate(date=as_date(date))

#bikecounts <- bikecounts %>% 
#  gather(westbound, eastbound, key="direction", value="counts")

#bikecounts %>% 
#  gather(westbound, eastbound, key="direction", value="counts") -> bikecounts

bikecounts <- bikecounts %>% 
  mutate(week=date - wday(date) + 1,
         month=date - mday(date) + 1,
         year=date - yday(date) + 1)

bikecounts_week <- bikecounts %>% 
  group_by(bridge, week) %>% 
  summarize(total=sum(total))

bikecounts_month <- bikecounts %>% 
  group_by(bridge, month) %>% 
  summarize(total=sum(total))

bikecounts_annual <- bikecounts %>% 
  group_by(bridge, year) %>% 
  summarize(total=sum(total))

ggplot(bikecounts) +
  geom_line(aes(x=date, y=total, color=bridge)) +
  scale_y_log10()

ggplot(bikecounts_week) +
  geom_line(aes(x=week, y=total, color=bridge)) +
  scale_y_log10()
