library(tidyverse)
library(lubridate)


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
all_logs <- do.call("rbind", list(jun2019, jul2019, aug2019, sep2019, oct2019, nov2019, dec2019, 
                                  jan2020, feb2020, mar2020, apr2020, may2020, jun2020, jul2020, aug2020, 
                                  sep2020))

names(all_logs) <-  c("Time", "PHP_SELF", "argv", "argc", "GATEWAY_INTERFACE", "SERVER_ADDR", "SERVER_NAME",
                      "SERVER_SOFTWARE", "SERVER_PROTOCOL", "REQUEST_METHOD", "REQUEST_TIME", "REQUEST_TIME_FLOAT",
                      "QUERY_STRING", "DOCUMENT_ROOT", "HTTP_ACCEPT", "HTTP_ACCEPT_CHARSET", "HTTP_ACCEPT_ENCODING",
                      "HTTP_ACCEPT_LANGUAGE", "HTTP_CONNECTION", "HTTP_HOST", "HTTP_REFERER", "HTTP_USER_AGENT",
                      "HTTPS", "REMOTE_ADDR", "REMOTE_HOST", "REMOTE_PORT", "REMOTE_USER", "REDIRECT_REMOTE_USER",
                      "SCRIPT_FILENAME", "SERVER_ADMIN", "SERVER_PORT", "SERVER_SIGNATURE", "PATH_TRANSLATED",
                      "SCRIPT_NAME", "REQUEST_URI", "PHP_AUTH_DIGEST", "PHP_AUTH_USER", "PHP_AUTH_PW", "AUTH_TYPE", 
                      "PATH_INFO", "ORIG_PATH_INFO", "Number_of_names_searched", "format", "Name1", "Name2", "Name3",
                      "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10", "X")

# set data types
all_logs$Time <- as.Date(all_logs$Time, "%Y-%m-%d:%H:%M::%S")
all_logs$Number_of_names_searched <- as.integer(all_logs$Number_of_names_searched)

# number of observations each month
monthCounts <- data.frame(
  month = c("2019-06-01", "2019-07-01", "2019-08-01", "2019-09-01", "2019-10-01", "2019-11-01", "2019-12-01",
        "2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01"),
  obs = c(nrow(jun2019), nrow(jul2019), nrow(aug2019), nrow(sep2019), nrow(oct2019), nrow(nov2019), nrow(dec2019),
      nrow(jan2020), nrow(feb2020), nrow(mar2020), nrow(apr2020), nrow(may2020), nrow(jun2020), nrow(jul2020),
      nrow(aug2020), nrow(sep2020)))

monthCounts$month <- as.Date(monthCounts$month, "%Y-%m-%d")
monthCounts$obs <- as.integer(monthCounts$obs)
str(monthCounts)

# summarize month counts
summary(monthCounts$obs)

# histogram
hist(monthCounts$obs)

# scatter plot of month counts  
ggplot(monthCounts, aes(x=month, y=obs)) + 
  geom_point() +
  stat_smooth(method="lm")


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

# explore Number_of_names_searched
summary(all_logs$Number_of_names_searched)
str(all_logs$Number_of_names_searched)
boxplot(all_logs$Number_of_names_searched)
hist(all_logs$Number_of_names_searched)


# explore referrer
summary(all_logs$HTTP_REFERER)

referer <- table(all_logs$HTTP_REFERER)
barplot(referer)

format <- table(sep2020$format)
barplot(format)

# explore rating formats
summary(all_logs$format)
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

# explore referers
refererFullRow <- all_logs %>% filter(X.HTTP_REFERER. != "" 
                               & X.HTTP_REFERER. != "https://www.chessgraphs.com"
                               & X.HTTP_REFERER. != "https://www.chessgraphs.com/"
                               & X.HTTP_REFERER. != "https://chessgraphs.com"
                               & X.HTTP_REFERER. != "https://chessgraphs.com/"
                               & X.HTTP_REFERER. != "chessgraphs.com"
                               & X.HTTP_REFERER. != "chessgraphs.com/"
                               & X.HTTP_REFERER. != "www.chessgraphs.com"
                               & X.HTTP_REFERER. != "www.chessgraphs.com/"
                               & X.HTTP_REFERER. != "http://www.chessgraphs.com"
                               & X.HTTP_REFERER. != "http://www.chessgraphs.com/"
                               & X.HTTP_REFERER. != "http://chessgraphs.com"
                               & X.HTTP_REFERER. != "http://chessgraphs.com/")

referer <- table(refererFullRow$X.HTTP_REFERER.)
referer <- referer %>% desc()
view(referer)



