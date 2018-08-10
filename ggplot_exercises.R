library(ggplot2)

ggplot(data = bikecounts_weather) + 
  geom_point(mapping = aes(x = date, y = total, color = name))

ggplot(data = bikecounts_weather) + 
  geom_point(mapping = aes(x = month(date, label=TRUE), y = total, color = name))

ggplot(data = bikecounts_weather) + 
  geom_point(mapping = aes(x = year(date), y = total, color = name))

library(readxl)
library(lubridate)
sunlight <- read_excel("data/sunlight.xlsx")
sunlight <- sunlight %>% gather(Jan.:Dec., key="month", value="raw_hours") %>% 
  mutate(hours=raw_hours - floor_date(raw_hours, "day")) %>% 
  select(Day, month, hours)
View(sunlight)

feb29hours <- 11.125
sunlight["Day"==29 & "month"=="Feb."] <- feb29hours
  
sunlight_years <- sunlight %>% 
  mutate(date=case_when(
    Day %in"% c("01", "02", "03", "04", "05", "06", "07") ~ 1,
    Month %in% c("-1", "-7", "-8", "-9") ~ as.double(NA),
    TRUE ~0)) %>%

bikecounts_weather <- bikecounts %>% 
  mutate(date=as_date(date)) %>%
  inner_join(PDXweather, by=c("date"="DATE"))

sunlight_years <- bikecounts %>% mutate(date)

install.packages('ggfortify')
library(ggfortify)
autoplot(stl(bikecounts, s.window = 'periodic'), ts.colour = 'blue')
autoplot(acf(bikecounts, plot = FALSE))
ggfreqplot(bikecounts)

bikecounts <- bikecounts %>% 
  mutate(month=as.character(month(date)),
         day=as.character(day(date)))

install.packages("directlabels")
library(directlabels)

