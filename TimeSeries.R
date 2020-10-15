library(tidyverse)
library(RColorBrewer)

year1_data <- read.csv('R_Projects/2014-05_supp.csv', header = TRUE)
year2_data <- read.csv('R_Projects/2019-05_supp.csv', header = TRUE)
year2020_data <- read.csv('R_Projects/2020.csv', header = TRUE)



unique(year1_data$FIDE_ID %in% year2_data$FIDE_ID)

year1_data_standard <- year1_data %>% filter(RATING_FORMAT == 'standard') %>% distinct()
year2_data_standard <- year2_data %>% filter(RATING_FORMAT == 'standard') %>% distinct()

people_w_multiple_obs <- year1_data_standard %>% 
  filter(FIDE_ID %in% year2_data_standard$FIDE_ID)


standard_ratings_joined <- inner_join(year1_data_standard, year2_data_standard, 
                                      by=c("FIDE_ID", "RATING_FORMAT"), suffix=c("_yr1", "_yr2"))


ratings_diff <- standard_ratings_joined %>% 
  select(FIDE_ID, FED_yr1, FED_yr2, BIRTH_YEAR_yr1, BIRTH_YEAR_yr2, RATING_yr1, RATING_yr2, SEX_yr1) %>%
  mutate(RATING_DIFF = RATING_yr2 - RATING_yr1,
         BIRTH_YEAR = ifelse(BIRTH_YEAR_yr1 > BIRTH_YEAR_yr2, BIRTH_YEAR_yr1, BIRTH_YEAR_yr2)) %>%
  filter(BIRTH_YEAR != 0 & (FED_yr1 %in% c('USA', 'RUS', 'IND', 'GER', 'VIE')) & BIRTH_YEAR > 1980)


ggplot(ratings_diff, aes(x=BIRTH_YEAR, y=RATING_DIFF)) + 
  geom_point(position = "jitter") +
  geom_smooth(inherit.aes = TRUE) +
  geom_hline(yintercept=0, col="red")


ggplot(ratings_diff, aes(x=RATING_yr1, y=RATING_DIFF, col=BIRTH_YEAR)) + 
  geom_point(position = "jitter") +
  geom_smooth(inherit.aes = TRUE, col = "black") +
  geom_hline(yintercept=0, col="red") +
  scale_color_gradient(low="blue", high="red") +
  facet_grid(SEX_yr1 ~ FED_yr1)
  