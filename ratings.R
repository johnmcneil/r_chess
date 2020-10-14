library(tidyverse)

ratings <- read.csv("R_Projects/2019-05_standard.csv", header=TRUE)

selected_ratings <- ratings %>% filter(FED %in% c('RUS', 'IND', 'USA', 'GER', 'CRO') & !(BIRTH_YEAR == 0)) %>%
  mutate(AGE = 2019 - BIRTH_YEAR)

ggplot(selected_ratings, aes(x=SEX, y=RATING, col=FED)) +
  geom_boxplot() +
  facet_wrap(~FED)

head(ratings)
names(ratings)
str(ratings$FED)
nrow(ratings)
nrow(selected_ratings)
  tail(ratings)
summary(ratings$rating)  
summary(ratings$FED)
summary(ratings$BIRTH_YEAR)
summary(ratings$NAME)
summary(ratings$RATING)
plot(ratings$BIRTH_YEAR, ratings$RATING)
head(selected_ratings)
plot(selected_ratings$AGE, selected_ratings$RATING, pch=20)


ratings$age <- 2019 - ratings$BIRTH_YEAR
head(ratings)



boxplot(ratings$RATING)
boxplot(selected_ratings$RATING ~ selected_ratings$FED)
summary(selected_ratings$FED)

boxplot(ratings$RATING ~ ratings$SEX)
hist(ratings$RATING)
sum(is.na(ratings$RATING))


fed_counts <- table(ratings$FED)
barplot(fed_counts)
head(ratings)
plot(ratings$GMS, ratings$RATING)
summary(ratings$GMS)


str(ratings$GMS)
str(ratings$RATING)
str(ratings$RATING_FORMAT)

# dplr stuff
library(dplyr)

# select()
name_rating_only <- ratings %>% select(NAME, RATING)

# Alison said filter() is like the WHERE clause in SQL
age_over_21 <- ratings %>% filter(BIRTH_YEAR >= 1998)
head(age_over_21)

# I can't think of how I would use mutate()

# use arrange() to put the table in descending order by RATING
ratings_descending <- ratings %>% 
  arrange(desc(RATING))
head(ratings_descending)
tail(ratings_descending)

# use group_by() and summarize()
# to find the average rating in each country
ave_rating_by_country <- ratings %>%
  group_by(FED) %>%
  summarise(ave_rating = mean(RATING), player_count = n())
ave_rating_by_country_desc <- ave_rating_by_country %>% arrange(desc(ave_rating))

# plot number of rated players in each country versus rating
plot(ave_rating_by_country$player_count, ave_rating_by_country$ave_rating)


#ggplot2 starts here
install.packages("ggplot2")
