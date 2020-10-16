library(tidyverse)


# read csv files 
# note that 2019-05.csv is not a complete month and 2020-10.csv currently is not complete

may2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-05.csv", quote="", sep ="\t", header=TRUE)
jun2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-06.csv", quote="", sep ="\t", header=TRUE)
jul2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-07.csv", quote="", sep ="\t", header=TRUE)
aug2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-08.csv", quote="", sep ="\t", header=TRUE)
sep2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-09.csv", quote="", sep ="\t", header=TRUE)
oct2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-10.csv", quote="", sep ="\t", header=TRUE)
nov2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-11.csv", quote="", sep ="\t", header=TRUE)
dec2019 <- read.csv("~/R_Projects/chessgraphs_logs/2019-12.csv", quote="", sep ="\t", header=TRUE)
jan2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-01.csv", quote="", sep ="\t", header=TRUE)
feb2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-02.csv", quote="", sep ="\t", header=TRUE)
mar2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-03.csv", quote="", sep ="\t", header=TRUE)
apr2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-04.csv", quote="", sep ="\t", header=TRUE)
may2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-05.csv", quote="", sep ="\t", header=TRUE)
jun2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-06.csv", quote="", sep ="\t", header=TRUE)
jul2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-07.csv", quote="", sep ="\t", header=TRUE)
aug2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-08.csv", quote="", sep ="\t", header=TRUE)
sep2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-09.csv", quote="", sep ="\t", header=TRUE)
oct2020 <- read.csv("~/R_Projects/chessgraphs_logs/2020-10.csv", quote="", sep ="\t", header=TRUE)

# check column names
names(jul2019)
names(aug2020)

# combine logs into one data frame
all_logs <- do.call("rbind", list(may2019, jun2019, jul2019, aug2019, sep2019, oct2019, nov2019, dec2019, 
                                  jan2020, feb2020, mar2020, apr2020, may2020, jun2020, jul2020, aug2020, 
                                  sep2020, oct2020))

# number of observations each month
monthCounts <- data.frame(
  month = c("2019-06", "2019-07", "2019-08", "2019-09", "2019-10", "2019-11", "2019-12",
        "2020-01", "2020-02", "2020-03", "2020-04", "2020-05", "2020-06", "2020-07", "2020-08", "2020-09"),
  obs = c(nrow(jun2019), nrow(jul2019), nrow(aug2019), nrow(sep2019), nrow(oct2019), nrow(nov2019), nrow(dec2019),
      nrow(jan2020), nrow(feb2020), nrow(mar2020), nrow(apr2020), nrow(may2020), nrow(jun2020), nrow(jul2020),
      nrow(aug2020), nrow(sep2020)))

# summarize month counts
summary(monthCounts$obs)

# histogram
hist(monthCounts$obs)

# scatter plot of month counts  
ggplot(monthCounts, aes(x=month, y=obs)) + 
  geom_point() 


# boxplot of month counts
boxplot(monthCounts$obs)


# focus on most recent complete month, 2020-09

summary(sep2020$X.Number_of_names_searched.)
summary(sep2020$X.HTTP_REFERER.)
summary(sep2020$X.format.)

boxplot(sep2020$X.Number_of_names_searched.)

hist(sep2020$X.Number_of_names_searched.)

referer <- table(sep2020$X.HTTP_REFERER.)
barplot(referer)

format <- table(sep2020$format)
barplot(format)

# focus on all logged data()

# having a problem with the number of names searched being treated as a character
summary(all_logs$X.Number_of_names_searched.)
summary(all_logs$X.HTTP_REFERER.)
summary(all_logs$format)

boxplot(all_logs$X.Number_of_names_searched.)

hist(all_logs$X.Number_of_names_searched.)

referer <- table(all_logs$X.HTTP_REFERER.)
barplot(referer)

format <- table(sep2020$format)
barplot(format)

# explore rating formats
allFideStandard <- all_logs %>% filter(format == "fide-standard")
allFideRapid <- all_logs %>% filter(format == "fide-rapid")
allFideBlitz <- all_logs %>% filter(format == "fide-blitz")

allUscfRegular <- all_logs %>% filter(format == "uscf-regular")
allUscfQuick <- all_logs %>% filter(format == "uscf-quick")
allUscfBlitz <- all_logs %>% filter(format == "uscf-blitz")

allUrs <- all_logs %>% filter(format == "urs")

allStandard <- all_logs %>% filter(format == "standard")
allRapid <- all_logs %>% filter(format == "rapid")
allBlitz <- all_logs %>% filter(format == "blitz")

formatCounts <- data.frame(
  format = c("standard", "rapid", "blitz", "fide-standard", "fide-rapid", "fide-blitz",
             "uscf-regular", "uscf-quick", "uscf-blitz", "urs"),
  obs = c(nrow(allStandard), nrow(allRapid), nrow(allBlitz), nrow(allFideStandard), nrow(allFideRapid), 
          nrow(allFideBlitz), nrow(allUscfRegular), nrow(allUscfQuick), nrow(allUscfBlitz),
          nrow(allUrs)))
monthCounts <- data.frame(
  month = c("2019-06", "2019-07", "2019-08", "2019-09", "2019-10", "2019-11", "2019-12",
            "2020-01", "2020-02", "2020-03", "2020-04", "2020-05", "2020-06", "2020-07", "2020-08", "2020-09"),
  obs = c(nrow(jun2019), nrow(jul2019), nrow(aug2019), nrow(sep2019), nrow(oct2019), nrow(nov2019), nrow(dec2019),
          nrow(jan2020), nrow(feb2020), nrow(mar2020), nrow(apr2020), nrow(may2020), nrow(jun2020), nrow(jul2020),
          nrow(aug2020), nrow(sep2020)))