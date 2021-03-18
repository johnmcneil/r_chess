library(tidyverse)

ratings2019 <- read.csv("fide/2019-05_supp.csv", header=TRUE)
ratings2014 <- read.csv("fide/2014-05_supp.csv", header=TRUE)
allFide <- read.csv("R_Projects/fide_ratings.csv", header=FALSE)


# explore the data set
summary(ratings2019$RATING)
str(ratings2019$RATING)
str(ratings2019$DATE)
str(ratings2019$BIRTH_YEAR)
mean(ratings2019$BIRTH_YEAR)
summary(ratings2019$BIRTH_YEAR)
plot(ratings2019$BIRTH_YEAR, ratings2019$RATING)
hist(ratings2019$RATING)


# filter for standard ratings of active players
may2019_standard <- ratings2019 %>% filter(ratings2019$RATING_FORMAT == "standard"
                                           & ratings2019$BIRTH_YEAR != "0"
                                           & ratings2019$FLAG != "i")
hist(may2019_standard$RATING)

# compare age and rating
may2019_standard$AGE <- 2020 - may2019_standard$BIRTH_YEAR
summary(may2019_standard$AGE)
plot(may2019_standard$AGE, may2019_standard$RATING)
hist(may2019_standard$AGE)
age_table <- table(may2019_standard$AGE)
view(age_table)

# compare age and rating for masters
may2019_standard_masters <- may2019_standard %>% filter(may2019_standard$RATING > 2199)
boxplot(may2019_standard_masters$RATING)
hist(may2019_standard_masters$RATING)
plot(may2019_standard_masters$AGE, may2019_standard_masters$RATING)


# group by title
boxplot(may2019_standard_masters$RATING ~ may2019_standard_masters$TIT)
title_table <- table(may2019_standard_masters$TIT)
barplot(title_table)

# group by country
boxplot(may2019_standard_masters$RATING ~ may2019_standard_masters$FED)
masters_per_country <- table(may2019_standard_masters$FED)
barplot(masters_per_country)

# group by gender
boxplot(may2019_standard_masters$RATING ~ may2019_standard_masters$SEX)
ggplot(may2019_standard_masters, aes(x=SEX, y=RATING)) +
  geom_boxplot()

masters_by_gender <- table(may2019_standard_masters$SEX)
barplot(masters_by_gender)

# group by games played
games_per_master <- table(may2019_standard_masters$GMS)
barplot(games_per_master)

played <- may2019_standard_masters %>% filter(may2019_standard_masters$GMS > 0)
games_played_per_master <- table(played$GMS)
barplot(games_played_per_master)
nrow(played)

# old stuff

selected_ratings <- ratings2019 %>% 
  filter(FED %in% c('RUS', 'IND', 'USA', 'GER') & !(BIRTH_YEAR == 0)) %>%
  mutate(AGE = 2019 - BIRTH_YEAR)

ggplot(selected_ratings, aes(x=FED, y=RATING)) +
  geom_boxplot()

ggplot(selected_ratings, aes(x=FED, y=AGE)) +
  geom_boxplot()


# What is the percentage of female players by country?

gender_balance_by_country <- ratings2019 %>% group_by(FED) %>% 
  summarize(pct_female = 100 * sum(ifelse(SEX == 'F', 1, 0)) / n(),
            total_players = n()) %>%
  arrange(desc(pct_female))


# Scatterplot by age and country

ggplot(selected_ratings, aes(x=AGE, y=RATING, col=FED)) + 
  #geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~FED)



# Boxplot by gender and country

ggplot(selected_ratings, aes(x=SEX, y=RATING, col=SEX)) +
  geom_boxplot() +
  facet_wrap(~FED)




# Are the gender differences statistically significant?


usa <- selected_ratings %>% filter(FED == 'USA')
ger <- selected_ratings %>% filter(FED == 'GER')
ind <- selected_ratings %>% filter(FED == 'IND')

t.test(data=selected_ratings, RATING~SEX)
t.test(data=usa, RATING~SEX)
t.test(data=ger, RATING~SEX)
t.test(data=ind, RATING~SEX)

# How many unique countries are represented in the dataset?

length(unique(ratings2019$FED))

ratings %>% select(FED) %>% unique() %>% arrange(FED)
