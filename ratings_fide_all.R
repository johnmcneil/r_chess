library(tidyverse)

# load csv
# supplements for comparison
ratings2019 <- read.csv("R_Projects/2019-05_supp.csv", header=TRUE)
ratings2014 <- read.csv("R_Projects/2014-05_supp.csv", header=TRUE)

# most recent csv of all fide ratings
allFide <- read.csv("R_Projects/fide_ratings.csv", header=FALSE)

# add column names to allFide
names(allFide)
names(ratings2019)
names(allFide) <- c("ID", "DATE", "RATING_FORMAT", "FIDE_ID", "NAME", "FED", 
                    "SEX", "TIT", "WTIT", "OTIT", "RATING", "GMS",
                    "K", "BIRTH_YEAR", "FLAG", "BIRTH_DATE", "FOA")
names(allFide)

# set data types
as.integer(allFide$ID)
as.Date(allFide$DATE, "%Y-%m-%d")
as.factor(allFide$RATING_FORMAT)
as.integer(allFide$FIDE_ID)
as.character(allFide$NAME)
as.factor(allFide$FED)
as.factor(allFide$SEX)
as.factor(allFide$TIT)
as.factor(allFide$WTIT)
as.factor(allFide$OTIT)
as.integer(allFide$RATING)
as.integer(allFide$GMS) 
as.integer(allFide$K)
as.integer(allFide$BIRTH_YEAR)
as.factor(allFide$FLAG)
as.Date(allFide$BIRTH_DATE, "%Y-%m-%d")
as.factor(allFide$FOA)

# explore data vectors
names(allFide)
str(allFide$TIT)
levels(allFide$TIT)

summary(allFide$BIRTH_YEAR)
hist(allFide$BIRTH_YEAR)

# what's the distribution of birth years in the ratings records?
birthYear <- allFide %>% filter(allFide$BIRTH_YEAR != 0)
hist(birthYear$BIRTH_YEAR)

# look at standard ratings only. summarize average rating by date
allFideStandard <- allFide %>% filter(allFide$RATING_FORMAT == "standard")
aveRatingByDate <- allFideStandard %>% group_by(DATE) %>% summarise(AVE_RATING = mean(RATING))

# look at standard ratings above 2200 only. summarize average rating by date
allFideStandardMaster <- allFideStandard %>% filter(allFideStandard$RATING_FORMAT == "standard"
                                      & allFideStandard$RATING > 2199
                                      & allFideStandard$FLAG != "i")
aveRatingByDateMaster <- allFideStandardMaster %>% group_by(DATE) %>% summarise(AVE_RATING = mean(RATING))
ggplot(aveRatingByDateMaster, aes(x=DATE, y=AVE_RATING))+
         geom_point()


# for standard ratings above 2400, summarize average rating by date
allFideStandard2400plus <- allFideStandardMaster %>% filter(allFideStandardMaster$RATING > 2399
                                                            & allFideStandardMaster$FLAG != "i")
aveRatingByDate2400plus <- allFideStandard2400plus %>% group_by(DATE) %>% summarise(AVE_RATING = mean(RATING))
ggplot(aveRatingByDate2400plus, aes(x=DATE, y=AVE_RATING))+
  geom_point()

# for standard ratings above 2600, summarize average rating by date
allFideStandard2600plus <- allFideStandardMaster %>% filter(allFideStandardMaster$RATING > 2599
                                                            & allFideStandardMaster$FLAG !="i")
aveRatingByDate2600plus <- allFideStandard2600plus %>% group_by(DATE) %>% summarise(AVE_RATING = mean(RATING))
ggplot(aveRatingByDate2600plus, aes(x=DATE, y=AVE_RATING)) +
  geom_point()                                                  

# does the change over time in average rating differ by gender?
allFideStandardMasterMale <- allFideStandardMaster %>% filter(allFideStandardMaster$SEX == "M")
aveRatingsByDateMasterMale <- allFideStandardMasterMale %>% group_by(DATE) %>% summarise(AVE_RATING = mean(RATING))
ggplot(aveRatingsByDateMasterMale, aes(x=DATE, y=AVE_RATING)) +
  geom_point()

allFideStandardMasterFemale <- allFideStandardMaster %>% filter(allFideStandardMaster$SEX == "F")
aveRatingsByDateMasterFemale <- allFideStandardMasterFemale %>% group_by(DATE) %>% summarise(AVE_RATING = mean(RATING))
ggplot(aveRatingsByDateMasterFemale, aes(x=DATE, y=AVE_RATING)) +
  geom_point()


male <- allFideStandard2600plus %>% filter(allFideStandard2600plus$SEX == "M")
ave_male <- male %>% group_by(DATE) %>% summarise(AVE_RATING = mean(RATING))
ggplot(ave_male, aes(x=DATE, y=AVE_RATING)) +
  geom_point()

female <- allFideStandard2600plus %>% filter(allFideStandard2600plus$SEX == "F")
ave_female <- female %>% group_by(DATE) %>% summarise(AVE_RATING = mean(RATING))
ggplot(ave_female, aes(x=DATE, y=AVE_RATING)) +
  geom_point()

# how has the number of top female players changed?
gender <- allFideStandardMaster %>% group_by(DATE, SEX) %>% summarize(n())

femaleMastersByDate <- allFideStandardMastersFemale %>% group_by(DATE) %>% summarise(N_FEMALE = )