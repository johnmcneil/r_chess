library(tidyverse)

jan2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-01.csv", sep = "\t", header=TRUE)
feb2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-02.csv", sep = "\t", header=TRUE)
mar2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-03.csv", sep = "\t", header=TRUE)
apr2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-04.csv", sep = "\t", header=TRUE)
may2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-05.csv", sep = "\t", header=TRUE)
jun2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-06.csv", sep = "\t", header=TRUE)
jul2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-07.csv", sep = "\t", header=TRUE)
aug2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-08.csv", sep = "\t", header=TRUE)
sep2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-09.csv", sep = "\t", header=TRUE)
oct2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-10.csv", sep = "\t", header=TRUE)
nov2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-11.csv", sep = "\t", header=TRUE)
dec2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-12.csv", sep = "\t", header=TRUE)

jan2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-01.csv", sep = "\t", header=TRUE)
feb2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-02.csv", sep = "\t", header=TRUE)
mar2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-03.csv", sep = "\t", header=TRUE)
apr2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-04.csv", sep = "\t", header=TRUE)
may2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-05.csv", sep = "\t", header=TRUE)
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
summary(jun2020$format)

boxplot(sep2020$Number_of_names_searched)

hist(sep2020$Number_of_names_searched)

referer <- table(sep2020$HTTP_REFERER)
barplot(referer)

format <- table(sep2020$format)
barplot(format)


