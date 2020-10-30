library(tidyverse)
library(lubridate)

# load csv
# supplements for comparison
ratings2019 <- read.csv("R_Projects/2019-05_supp.csv", header=TRUE, stringsAsFactors = FALSE)
ratings2014 <- read.csv("R_Projects/2014-05_supp.csv", header=TRUE, stringsAsFactors = FALSE)

# most recent csv of all fide ratings
allFide <- read.csv("R_Projects/fide_ratings.csv", header=FALSE, stringsAsFactors = FALSE )

# add column names to allFide
names(allFide) <- c("ID", "DATE", "RATING_FORMAT", "FIDE_ID", "NAME", "FED", 
                    "SEX", "TIT", "WTIT", "OTIT", "RATING", "GMS",
                    "K", "BIRTH_YEAR", "FLAG", "BIRTH_DATE", "FOA")

# set data types
allFide$ID <- as.integer(allFide$ID)


# change date from 00 to 01 because that's what as.Date() accepts
allFide$DATE <- str_replace(allFide$DATE, "-00", "-01")
allFide$DATE <- as.Date(allFide$DATE)


allFide$RATING_FORMAT <- as.factor(allFide$RATING_FORMAT)
allFide$FIDE_ID <- as.integer(allFide$FIDE_ID)
allFide$NAME <- as.character(allFide$NAME)
allFide$FED <- as.factor(allFide$FED)
allFide$SEX <- as.factor(allFide$SEX)
allFide$TIT <- as.factor(allFide$TIT)
allFide$WTIT <- as.factor(allFide$WTIT)
allFide$OTIT <- as.factor(allFide$OTIT)
allFide$RATING <- as.integer(allFide$RATING)
allFide$GMS <- as.integer(allFide$GMS) 
allFide$K <- as.integer(allFide$K)
allFide$BIRTH_YEAR <- as.integer(allFide$BIRTH_YEAR)
allFide$FLAG <- as.factor(allFide$FLAG)
allFide$BIRTH_DATE <- as.Date(allFide$BIRTH_DATE, "%Y-%m-%d")
allFide$FOA <- as.factor(allFide$FOA)

# explore data vectors
names(allFide)
str(allFide)
str(allFide$ID)
str(allFide$TIT)
str(allFide$DATE)
levels(allFide$TIT)

# explore rating dates
hist(allFide$DATE, breaks="years")
eighties <- allFide %>% filter(year(allFide$DATE) > 1979
                          & year(allFide$DATE) < 1990)
hist(eighties$DATE, breaks="years")

# what's the distribution of birth years in the ratings records?
birthYear <- allFide %>% filter(allFide$BIRTH_YEAR != 0)
hist(birthYear$BIRTH_YEAR)


# look at standard ratings above 2200 only. summarize average rating by date
allFideStandardMaster <- allFide %>% filter(allFide$RATING_FORMAT == "standard"
                                      & allFide$RATING > 2199
                                      & allFide$FLAG != "i")
aveFideStandardMaster <- allFideStandardMaster %>% group_by(DATE) %>% summarise(AVE_RATING = mean(RATING))
ggplot(aveFideStandardMaster, aes(x=DATE, y=AVE_RATING)) +
  geom_point() +
  scale_x_date(breaks = "10 years")
    


# how does the average change in rating compare by gender?
allFideStandardMasterMale <- allFideStandardMaster %>% filter(allFideStandardMaster$SEX == "M")
aveRatingsByDateMasterMale <- allFideStandardMasterMale %>% group_by(DATE) %>% summarise(AVE_RATING = mean(RATING))
ggplot(aveRatingsByDateMasterMale, aes(x=DATE, y=AVE_RATING)) +
  geom_point()

allFideStandardMasterFemale <- allFideStandardMaster %>% filter(allFideStandardMaster$SEX == "F")
aveRatingsByDateMasterFemale <- allFideStandardMasterFemale %>% group_by(DATE) %>% summarise(AVE_RATING = mean(RATING))
ggplot(aveRatingsByDateMasterFemale, aes(x=DATE, y=AVE_RATING)) +
  geom_point()

# compare average rating of all known gender by date
genderKnown <- allFide %>% filter(allFide$SEX != "" 
                                  & allFide$FLAG != "i"
                                  & RATING > 1999)
genderKnownSummary <- genderKnown %>% group_by(DATE, SEX) %>% summarise(AVE_RATING = mean(RATING))
ggplot(genderKnownSummary, aes(x=DATE, y=AVE_RATING, col=SEX)) +
  geom_point() +
  ggtitle("Mean rating of players at or above 2000 FIDE, by gender")

# compare number of all known gender by date
genderCount <- genderKnown %>% group_by(DATE, SEX) %>% summarise(COUNT = n())
ggplot(genderCount, aes(x=DATE, y=COUNT, col=SEX)) +
  geom_point() +
  ggtitle("Count of players at or above 2000 FIDE by gender")


# use colors!
allFideStandard2600plusGendered <- allFideStandard2600plus %>% 
  filter(allFideStandard2600plus$SEX != "")
as.Date(allFideStandard2600plusGendered$DATE, "%Y-%m-%d")
ggplot(allFideStandard2600plusGendered, aes(x=DATE, y=RATING, col=SEX)) +
  geom_point() + 
  scale_x_date(breaks = "10 years")

allFideStandardMaster <- allFideStandardMaster %>%
  filter(allFideStandardMaster$FED != "")
ggplot(allFideStandardMaster, aes(x=DATE, y=RATING, col=FED)) +
  geom_point()
