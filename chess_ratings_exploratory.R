library(tidyverse)

# this was the way to read the raw fixed width file
# ratings <- read.fwf("standard_rating_list.txt", 
#                    widths=c(16, 61, 4, 4, 5, 5, 15, 4, 6, 4, 3, 6, 4), 
#                    header=TRUE)


ratings <- read.csv("fide/2019-05_standard.csv", header=TRUE)

ggplot(ratings, aes(x=SEX, y=RATING)) +
  geom_boxplot()

# Do the ratings and age distributions differ by country?

selected_ratings <- ratings %>% 
  filter(FED %in% c('RUS', 'IND', 'USA', 'GER') & !(BIRTH_YEAR == 0)) %>%
  mutate(AGE = 2019 - BIRTH_YEAR)

ggplot(selected_ratings, aes(x=FED, y=RATING)) +
  geom_boxplot()

ggplot(selected_ratings, aes(x=FED, y=AGE)) +
  geom_boxplot()


# What is the percentage of female players by country?

gender_balance_by_country <- ratings %>% group_by(FED) %>% 
  summarize(pct_female = sum(ifelse(SEX == 'F', 1, 0)) / n(),
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

length(unique(ratings$FED))

ratings %>% select(FED) %>% unique() %>% arrange(FED)
