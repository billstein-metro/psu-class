library(readxl)

load_data <- function (input_file, bridge_name) {
  bikecounts <- read_excel(input_file, sheet=bridge_name, skip=1)
  bikecounts$name <- bridge_name
}

input_file <- "data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx"
bridge_name <- "Hawthorne"
load_data (input_file,bridge_name)
bridge_name <- "Tilikum"
load_data (input_file,bridge_name)

Hawthorne <- load_data (input_file, "Hawthorne")
Tilikum <- load_data (input_file, "Tilikum")
names (Hawthorne) <- names (Tilikum)

Steel <- load_data (input_file, "Steel")
names(Steel) <- c("date","lower", "westbound", "eastbound","total", "name")
bike_counts <- bind_rows (Tilikum, Hawthorne, Steel %>% select(-lower))

bike_counts %>%
  group_by(name) %>%
  summarize(avg_daily_counts=mean(total, na.rm=TRUE))

library(lubridate)

bike_counts %>%
  group_by (name, ym=floor_date(date,"month")) %>%
  summarize(total_monthly_counts=sum(total), counts=n()) %>%
  group_by (name, month(ym)) %>%
  summarize (avg_monthly_count=mean(total_monthly_counts))

bike_counts %>%
  group_by(name, month(date)) %>%
  summarize (total_counts=sum(total)) %>%
  summarize(avg_monthly_counts=sum(total, na.rm=TRUE))


