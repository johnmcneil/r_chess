jun2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-06.csv", sep = "\t", header=TRUE)
jul2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-07.csv", sep = "\t", header=TRUE)
aug2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-08.csv", sep = "\t", header=TRUE)
sep2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-09.csv", sep = "\t", header=TRUE)
oct2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-10.csv", sep = "\t", header=TRUE)


names(july2019)
names(august2020)

nrow(jul2020)
nrow(jun2020)
nrow(aug2020)
nrow(sep2020)
nrow(oct2020)

summary(sep2020$Number_of_names_searched)
summary(sep2020$HTTP_REFERER)
summary(sep2020$format)

boxplot(sep2020$Number_of_names_searched)

hist(sep2020$Number_of_names_searched)

referer <- table(july2019$HTTP_REFERER)
barplot(referer)

format <- table(july2019$format)
barplot(format)

install.packages("dplyr")
library(dplyr)

summer <- june2019 + july2019 + august2019
