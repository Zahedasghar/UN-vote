#https://rforpoliticalscience.com/tag/text-mining/
#https://voyant-tools.org/?corpus=25e68e8df11fd1b6148642778046b42f
library(unvotes)
library(tidyverse)
library(magrittr)
library(bbplot)
library(waffle)
library(tidytext)
library(wordcloud)
library(wesanderson)



un_votes <- unvotes::un_roll_calls

un_votes_issues <- unvotes::un_roll_call_issues

unvotes::un_votes -> country_votes 


un_votes %>% 
  inner_join(un_votes_issues, by = "rcid") %>% 
  inner_join(country_votes, by = "rcid") %>% 
  mutate(year = format(date, format = "%Y")) %>%
  filter(country %in%c("India","Pakistan")) -> indopak_un


data("stop_words") 

## Pakistan Yes Votes

pk_yes_votes <- indopak_un %>% 
  filter(country == "Pakistan") %>% 
  filter(vote == "yes") %>%  
  select(descr, year) %>% 
  mutate(decade = substr(year, 1, 3)) %>% 
  mutate(decade = paste0(decade, "0s")) %>% 
  # group_by(decade) %>% 
  unnest_tokens(word, descr) %>% 
  mutate(word = gsub(" ", "", word)) %>% 
  mutate(word = gsub('_', '', word)) %>% 
  count(word, sort = TRUE) %>% 
  ungroup() %>% 
  anti_join(stop_words)  %>% 
  mutate(word = case_when(grepl("palestin", word) ~ "Palestine", 
                          grepl("nucl", word) ~ "nuclear",
                          TRUE ~ as.character(word)))  %>%
  filter(word != "resolution") %>% 
  filter(word != "assembly") %>% 
  filter(word != "draft") %>% 
  filter(word != "committee") %>% 
  filter(word != "requested") %>% 
  filter(word != "report") %>% 
  filter(word != "practices") %>% 
  filter(word != "affecting") %>% 
  filter(word != "follow") %>% 
  filter(word != "acting") %>% 
  filter(word != "adopted") 


## Count each word

pk_yes_votes %<>% 
  count(word) %>% 
  arrange(desc(n))

## Remove numbers
nums <- pk_yes_votes %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()

#And remove the stop words

pk_yes_votes %<>%
  anti_join(nums, by = "word")


my_colors <- c("#0450b4", "#046dc8", "#1184a7","#15a2a2", "#6fb1a0", 
               "#b4418e", "#d94a8c", "#ea515f", "#fe7434", "#fea802")





#And lastly, plot the wordcloud with the top 50 words

wordcloud(pk_yes_votes$word, 
          pk_yes_votes$n, 
          random.order = FALSE, 
          max.words = 30, 
          colors = my_colors)


## India yes votes

ind_yes_votes <- indopak_un %>% 
  filter(country == "India") %>% 
  filter(vote == "yes") %>%  
  select(descr, year) %>% 
  mutate(decade = substr(year, 1, 3)) %>% 
  mutate(decade = paste0(decade, "0s")) %>% 
  # group_by(decade) %>% 
  unnest_tokens(word, descr) %>% 
  mutate(word = gsub(" ", "", word)) %>% 
  mutate(word = gsub('_', '', word)) %>% 
  count(word, sort = TRUE) %>% 
  ungroup() %>% 
  anti_join(stop_words)  %>% 
  mutate(word = case_when(grepl("palestin", word) ~ "Palestine", 
                          grepl("nucl", word) ~ "nuclear",
                          TRUE ~ as.character(word)))  %>%
  filter(word != "resolution") %>% 
  filter(word != "assembly") %>% 
  filter(word != "draft") %>% 
  filter(word != "committee") %>% 
  filter(word != "requested") %>% 
  filter(word != "report") %>% 
  filter(word != "practices") %>% 
  filter(word != "affecting") %>% 
  filter(word != "follow") %>% 
  filter(word != "acting") %>% 
  filter(word != "adopted") 


## Count each word

ind_yes_votes %<>% 
  count(word) %>% 
  arrange(desc(n))

## Remove numbers
nums <- ind_yes_votes %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()

#And remove the stop words

ind_yes_votes %<>%
  anti_join(nums, by = "word")


my_colors <- c("#0450b4", "#046dc8", "#1184a7","#15a2a2", "#6fb1a0", 
               "#b4418e", "#d94a8c", "#ea515f", "#fe7434", "#fea802")





#And lastly, plot the wordcloud with the top 50 words

wordcloud(ind_yes_votes$word, 
          ind_yes_votes$n, 
          random.order = FALSE, 
          max.words = 30, 
          colors = my_colors)



indopak_un %>% 
  group_by(country, vote) %>% 
  count() %>% 
  mutate(count_ten = n /25) %>% 
  ungroup() %>% 
  ggplot(aes(fill = vote, values = count_ten)) +
  geom_waffle(color = "white",
              size = 2.5,
              n_rows = 10,
              flip = TRUE) +
  facet_wrap(~country) + bbplot::bbc_style() +
  scale_fill_manual(values = wesanderson::wes_palette("Darjeeling1"))




indopak_un %>%
  filter(vote == "abstain") %>% 
  mutate(issue = case_when(issue == "Nuclear weapons and nuclear material" ~ "Nukes",
                           issue == "Arms control and disarmament" ~ "Arms",
                           issue == "Palestinian conflict" ~ "Palestine",
                           TRUE ~ as.character(issue))) %>% 
  select(country, issue, year) %>% 
  group_by(issue, country) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(country) %>% 
  mutate(country = as.factor(country),
         issue = reorder_within(issue, n, country)) %>%
  ggplot(aes(x = reorder(issue, n), y = n)) + 
  geom_bar(stat = "identity", width = 0.7, aes(fill = country)) + 
  labs(title = "Abstaining UN General Assembly Votes by issues",
       subtitle = ("Since 1950s"),
       caption = "         Source: unvotes ") +
  xlab("") + 
  ylab("") +
  facet_wrap(~country, scales = "free_y") +
  scale_x_reordered() +
  coord_flip() + 
  expand_limits(y = 65) + 
  ggthemes::theme_pander() + 
  scale_fill_manual(values = sample(my_colors)) + 
  theme(plot.background = element_rect(color = "#f5f9fc"),
        panel.grid = element_line(colour = "#f5f9fc"),
        # axis.title.x = element_blank(),
        # axis.text.x = element_blank(),
        axis.text.y = element_text(color = "#000500", size = 16),
        legend.position = "none",
        # axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(family = "Gadugi"),
        plot.title = element_text(size = 28, color = "#000500"),
        plot.subtitle = element_text(size = 20, color = "#484e4c"),
        plot.caption = element_text(size = 20, color = "#484e4c"))



indopak_un %>% 
  filter(year < 2019) %>% 
  filter(issue == "Human rights") %>% 
  filter(vote == "yes") %>% 
  group_by(country, year) %>% 
  count() %>% 
  ggplot(aes(x = year, y = n, group = country, color = country)) + 
  geom_line(size = 2) + 
  geom_point(aes(color = country), fill = "white", shape = 21, size = 3, stroke = 2.5) +
  scale_x_discrete(breaks = round(seq(min(korea_un$year), max(korea_un$year), by = 10),1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 22)) + 
  bbplot::bbc_style() + facet_wrap(~country) + 
  theme(legend.position = "none") + 
  scale_color_manual(values = sample(my_colors)) + 
  labs(title = "Human Rights UN General Assembly Yes Votes ",
       subtitle = ("Since 1990s"),
       caption = "         Source: unvotes ")






library(countrycode)

speech$iso2 <- countrycode(speech$country, "country.name", "iso2c")

library(bbplot)

speech %>% 
  dplyr::filter(!is.na(iso2)) %>% 
  group_by(year) %>% 
  count() %>% 
  ggplot(aes(x = year, y = n)) + 
  geom_line(size = 1.2, alpha = 0.4) +
  geom_label(aes(label = n)) +
  bbplot::bbc_style() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = "Number of speeches given by countries at UNSC")
