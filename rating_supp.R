library(tidyverse)

ratings2019 <- read.csv("R_Projects/2019-05_supp.csv", header=TRUE)
ratings2014 <- read.csv("R_Projects/2014-05_supp.csv", header=TRUE)
allFide <- read.csv("R_Projects/fide_ratings.csv", header=TRUE)
allFide <- read.csv.ffdf(file = "R_Projects/fide_ratings.csv", header=TRUE)

ggplot(ratings2019, aes(x=SEX, y=RATING)) +
  geom_boxplot()

# summarize, explore rating distribution
may2019_standard <- ratings2019 %>% filter(ratings2019$RATING_FORMAT == "standard"
                                           & ratings2019$BIRTH_YEAR != "0")
plot(may2019_standard$BIRTH_YEAR, may2019_standard$RATING)
  
summary(ratings2019$RATING)
str(ratings2019$RATING)
str(ratings2019$DATE)
str(ratings2019$BIRTH_YEAR)
mean(ratings2019$BIRTH_YEAR)
summary(ratings2019$BIRTH_YEAR)
plot(ratings2019$BIRTH_YEAR, ratings2019$RATING)

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
