library(tidyverse)
library(readr)

# elecdf <- read_csv("https://raw.githubusercontent.com/colincookman/pakistan_election_results_2018/master/pk_constituency_data_2018.csv")
# 
# save(elecdf,file="data/elecdf.RData")

load("data/elecdf.RData")

## Inspection of data

dim(elecdf)

nrow(elecdf)

colnames(elecdf)


## Overview 

elecdf |> glimpse()

elecdf |> select(assembly) |> n_distinct()


## cleaning names

library(janitor)

elecdf <- elecdf |> clean_names()

elecdf |> glimpse()

library(vtable)

vtable(elecdf)


## Analysis

elecdf |> filter(assembly=="National") |> 
ggplot()+aes(votes_cast)+geom_histogram()

elecdf |> filter(assembly=="National") |> 
ggplot()+aes(x=votes_disq, fill=winbuckets)+geom_histogram()

elecdf |> filter(assembly=="National") |> 
  ggplot()+aes(y=votes_disq, x=winbuckets)+geom_boxplot()+coord_flip()




elecdf |> filter(assembly=="National") |> 
  ggplot()+aes(y=votes_cast, x=votes_disq,col=win_party)+geom_point()
  


elecdf |> filter(assembly=="National" & province=="Punjab", win_party%in% c("Pakistan Muslim League (N)", "Pakistan Peoples Party Parliamentarians",
                                                       "Pakistan Tehreek-e-Insaf")) |> 
  mutate(gap=win_votes-second_votes) |> filter(gap<5000) |> 
  ggplot()+aes(y=votes_cast, x=votes_disq,col=win_party)+geom_point()




elecdf |> filter(assembly=="National" & province=="Punjab", win_party%in% c("Pakistan Muslim League (N)", "Pakistan Peoples Party Parliamentarians",
                                                                            "Pakistan Tehreek-e-Insaf")) |> 
  mutate(gap=win_votes-second_votes) |> filter(gap<5000) |> select(win_name,win_votes,votes_disq, second_votes, win_party, second_name, gap) |> View()
elecdf |> glimpse()


ec_df <- elecdf |> filter(win_party=="Pakistan Peoples Party Parliamentarians" )|> 
  mutate(first_digit = as.numeric(substr(abs(win_votes), 1, 1)))

elecdf |> mutate(first_digit = as.numeric(substr(abs(ahmadinejad), 1, 1)))

digit_freq <- ec_df %>%
  count(first_digit) %>%
  mutate(percent = n / sum(n) * 100)


# Plot the Benford's distribution using ggplot2
ggplot(digit_freq, aes(x = as.factor(first_digit), y = percent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(name = "Leading Digit") +
  scale_y_continuous(name = "Frequency (%)", labels = scales::percent_format(scale = 1)) +
  labs(title = "PTI's Distribution", subtitle = "Frequency of Leading Digits in Data") +
  theme_minimal()






# Calculate the frequency of each leading digit
digit_freq <- idf %>%
  count(first_digit) %>%
  mutate(percent = n / sum(n) * 100)


# Plot the Benford's distribution using ggplot2
ggplot(digit_freq, aes(x = as.factor(first_digit), y = percent)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_discrete(name = "Leading Digit") +
  scale_y_continuous(name = "Frequency (%)", labels = scales::percent_format(scale = 1)) +
  labs(title = "Ahmadnejad's Distribution", subtitle = "Frequency of Leading Digits in Data") +
  theme_minimal()
