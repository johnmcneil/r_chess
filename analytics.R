library(tidyverse)
library(lubridate)
library(rjson)

## 1. setup

# read csv files 
data_dir = "~/R_Projects/chessgraphs_logs/"
filePaths <- list.files(data_dir, "\\.csv$", full.names = TRUE)
all_logs <- do.call(rbind, lapply(filePaths, read.csv, quote = "", sep = "\t", header = TRUE, stringsAsFactors = FALSE))

# set column names
names(all_logs) <-  c("DateTime", "PHP_SELF", "argv", "argc", "GATEWAY_INTERFACE", "SERVER_ADDR", "SERVER_NAME",
                      "SERVER_SOFTWARE", "SERVER_PROTOCOL", "REQUEST_METHOD", "REQUEST_TIME", "REQUEST_TIME_FLOAT",
                      "QUERY_STRING", "DOCUMENT_ROOT", "HTTP_ACCEPT", "HTTP_ACCEPT_CHARSET", "HTTP_ACCEPT_ENCODING",
                      "HTTP_ACCEPT_LANGUAGE", "HTTP_CONNECTION", "HTTP_HOST", "HTTP_REFERRER", "HTTP_USER_AGENT",
                      "HTTPS", "REMOTE_ADDR", "REMOTE_HOST", "REMOTE_PORT", "REMOTE_USER", "REDIRECT_REMOTE_USER",
                      "SCRIPT_FILENAME", "SERVER_ADMIN", "SERVER_PORT", "SERVER_SIGNATURE", "PATH_TRANSLATED",
                      "SCRIPT_NAME", "REQUEST_URI", "PHP_AUTH_DIGEST", "PHP_AUTH_USER", "PHP_AUTH_PW", "AUTH_TYPE", 
                      "PATH_INFO", "ORIG_PATH_INFO", "Number_of_names_searched", "format", "Name1", "Name2", "Name3",
                      "Name4", "Name5", "Name6", "Name7", "Name8", "Name9", "Name10", "X")

# set data types

# convert Time from character to POSIXlt
all_logs$DateTime <- strptime(all_logs$DateTime, format = "%Y-%m-%d:%H:%M::%S", tz = "CET")

#convert to POSIXct
all_logs$DateTime <- as.POSIXct(all_logs$DateTime)

# convert Number_of_names_searched from character to integer
all_logs$Number_of_names_searched <- as.integer(all_logs$Number_of_names_searched)



## 2. compare observations per month

# remove 2019-05, an incomplete month
complete_months <- all_logs %>% filter(year(DateTime) > 2019 | month(DateTime) != 5)

# group by year and month, summarize number of observations
monthCounts <- complete_months %>% group_by(Year = year(DateTime), Month = month(DateTime, label = TRUE)) %>% summarise(yearMonth = "", obs = n())

# histogram of observations per month
hist(monthCounts$obs)
# boxplot of month counts
boxplot(monthCounts$obs)

# convert to date
monthCounts$yearMonth = paste(monthCounts$Year, monthCounts$Month, "01", sep="-")
monthCounts$yearMonth <- as.Date(monthCounts$yearMonth, "%Y-%b-%d")

# scatter plot of observations per month
ggplot(monthCounts, aes(x=yearMonth, y=obs)) + 
  geom_point() +
  stat_smooth(method="lm")


## 3. analytics of last complete month
# get logs of last complete month into a variable
today <- Sys.Date()
first_of_this_month <- floor_date(today, unit = "month")
p <- months(1)
first_of_last_month <- first_of_this_month - p
last_month <- month(first_of_last_month)
last_months_year <- year(first_of_last_month)

last_complete_month_logs <- all_logs %>% filter( month(all_logs$DateTime) == last_month 
                                                 & year(all_logs$DateTime) == last_months_year)

# analytics of number_of_names_searched
boxplot(last_complete_month_logs$Number_of_names_searched)
hist(last_complete_month_logs$Number_of_names_searched)

# analytics of text searches only
text_search <- last_complete_month_logs %>% filter( Number_of_names_searched > 0 )
boxplot(text_search$Number_of_names_searched)
hist(text_search$Number_of_names_searched)
table(text_search$Number_of_names_searched)

# what names are people searching for?

# analytics of format searched
format_table <- table(text_search$format)
view(format_table)
barplot(format)

# analytics of referrers
# remove cases where there was no referrer or chessgraphs.com was the referrer
referrer <- last_complete_month_logs %>% filter(HTTP_REFERRER != "" 
                                     & HTTP_REFERRER != "https://www.chessgraphs.com"
                                     & HTTP_REFERRER != "https://www.chessgraphs.com/"
                                     & HTTP_REFERRER != "https://chessgraphs.com"
                                     & HTTP_REFERRER != "https://chessgraphs.com/"
                                     & HTTP_REFERRER != "chessgraphs.com"
                                     & HTTP_REFERRER != "chessgraphs.com/"
                                     & HTTP_REFERRER != "www.chessgraphs.com"
                                     & HTTP_REFERRER != "www.chessgraphs.com/"
                                     & HTTP_REFERRER != "http://www.chessgraphs.com"
                                     & HTTP_REFERRER != "http://www.chessgraphs.com/"
                                     & HTTP_REFERRER != "http://chessgraphs.com"
                                     & HTTP_REFERRER != "http://chessgraphs.com/")

referrer_table <- table(referrer$HTTP_REFERRER)
referrer_table <- sort(referrer_table, decreasing = TRUE )
view(referrer_table)

# analytics of user agent
user_agents_last_month <- last_complete_month_logs %>% group_by(HTTP_USER_AGENT) %>% summarise(Count = n() )

# parse user agent string

# analytics of IP address
ip_addresses_last_month <- last_complete_month_logs %>% group_by(REMOTE_ADDR) %>% summarise( Count = n() )

# parse IP address



## 4. analytics of all logged data

# explore Number_of_names_searched
summary(all_logs$Number_of_names_searched)
number_searched <- all_logs %>% filter(all_logs$Number_of_names_searched < 100)

boxplot(number_searched$Number_of_names_searched)
hist(number_searched$Number_of_names_searched)

ggplot(number_searched, aes(x=DateTime, y=Number_of_names_searched)) +
  geom_point()


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
view(formatCounts)

# explore referrers for all data
referrerFullRow <- all_logs %>% filter(HTTP_REFERRER != "" 
                               & HTTP_REFERRER != "https://www.chessgraphs.com"
                               & HTTP_REFERRER != "https://www.chessgraphs.com/"
                               & HTTP_REFERRER != "https://chessgraphs.com"
                               & HTTP_REFERRER != "https://chessgraphs.com/"
                               & HTTP_REFERRER != "chessgraphs.com"
                               & HTTP_REFERRER != "chessgraphs.com/"
                               & HTTP_REFERRER != "www.chessgraphs.com"
                               & HTTP_REFERRER != "www.chessgraphs.com/"
                               & HTTP_REFERRER != "http://www.chessgraphs.com"
                               & HTTP_REFERRER != "http://www.chessgraphs.com/"
                               & HTTP_REFERRER != "http://chessgraphs.com"
                               & HTTP_REFERRER != "http://chessgraphs.com/")


referrer <- table(referrerFullRow$HTTP_REFERRER)
view(referrer)

# analytics of user agent
user_agents_all_time <- all_logs %>% group_by(HTTP_USER_AGENT) %>% summarise(Count = n() )

# parse user agent string

# analytics of IP address
ip_addresses_all_time <- all_logs %>% group_by(REMOTE_ADDR) %>% summarise( Count = n() )

# parse IP address