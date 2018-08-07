library(readxl)
Hawthorne <- read_excel("data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx",
sheet = "Hawthorne", skip = 1)
Hawthorne$bridge <- "Hawthorne"
Tilikum <- read_excel("data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx",
sheet = "Tilikum", skip = 1)
Tilikum$bridge <- "Tilikum"

names(Hawthorne) <- names(Tilikum)

Steel <- read_excel("data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx",
sheet = "Steel", skip = 1)
names(Steel) <- c("date", "lower", "westbound", "eastbound", "total")
Steel$bridge <- "Steel"

bike_counts <- bind_rows(Hawthorne, Tilikum, Steel %>% select(-lower))



count.Hawthorne <- count(Hawthorne[,1])
sum.Hawthorne <- sum(Hawthorne[,4])
count.Tilikum <- count(Tilikum[,1])
count.Steel <- count(Steel[,1])
sum.Tilikum <- sum(Tilikum[,4])
sum.Steel <- sum (Steel[,5])
(mean.Hawthorne <- sum.Hawthorne / count.Hawthorne)
(mean.Steel <- sum.Steel / count.Steel)
(mean.Tilikum <- sum.Tilikum / count.Tilikum)
