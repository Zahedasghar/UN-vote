# load the tidyverse and tidytext packages
library(tidyverse)
library(tidytext)
library(stopwords)
# TED talk transcripts
# The TED talk transcripts are available to you in a dataframe called ted_talks. There are three variables in this data set:
#   
#   talk_id: the identifier from the TED website for this particular talk
# text: the text of this TED talk
# speaker: the main or first listed speaker (some TED talks have more than one speaker)

# glimpse `ted_talks` to see what is in the data set
ted_talks <- readRDS("D:/RepTemplates/UN-vote/data/ted_talks.rds")

ted_talks |> glimpse()


##A token is a meaningful unit of text for analysis; in many cases, this just
##means a single word. The process of tokenization identifies and breaks apart
##text into individual tokens. You can use tidytext’s unnest_tokens() function
##to accomplish all of this at once, both the tidying and the tokenization

tidy_talks <- ted_talks |> unnest_tokens(word, text)

tidy_talks


# What did unnest_tokens() do here? Instead of having 992 rows and reading each
# talk across the line in text, we now have 2,005,342 rows and can read each
# talk down the column in word. We have tokenized and tidied the text, as well
# as a few other transformations:

# Other columns have been retained.
# Punctuation has been stripped out.
# Words have been converted to lower-case.

#### Tokenize to bigrams

ted_bigrams <- ted_talks |> 
  unnest_tokens(bigram, text, token="ngrams",n=2)


ted_bigrams



## Most common TED talk words

tidy_talks |> count(word, sort=TRUE)



## Removing stop words
get_stopwords()

tidy_talks |> 
  anti_join(get_stopwords()) |> 
  count(word, sort=TRUE)

# Remove stop words (the default list) via an anti_join().
# Create a plot with n on the x-axis and word on the y-axis.


tidy_talks |> 
  anti_join(get_stopwords()) |> 
  count(word, sort=TRUE) |> 
  slice_max(n,n=20) |> 
  mutate(word=reorder(word,n)) |> 
  ggplot(aes(x=n,y=word))+
  geom_col()



## Compare TED talk vocabularies




tidy_talks |>
  filter(speaker %in% c("Jane Goodall", "Temple Grandin")) |>
  # remove stop words
  anti_join(get_stopwords()) |>
  # count with two arguments
  count(speaker, word) |>
  group_by(word) |>
  filter(sum(n) > 10) |>
  ungroup() |>
  pivot_wider(names_from = "speaker", values_from = "n", values_fill = 0) 



## Visualize Vocabulary Comparison



library(ggrepel)

tidy_talks %>%
  filter(speaker %in% c("Jane Goodall", "Temple Grandin")) %>%
  anti_join(get_stopwords()) %>%
  count(speaker, word) %>%
  group_by(word) %>%
  filter(sum(n) > 10) %>%
  ungroup() %>%
  pivot_wider(names_from = "speaker", values_from = "n", values_fill = 0) %>%
  ggplot(aes(`Jane Goodall`, `Temple Grandin`)) +
  geom_abline(color = "gray50", size = 1.2, alpha = 0.8, lty = 2) +
  # use the special ggrepel geom for nicer text plotting
  geom_text_repel(aes(label = word)) +
  coord_fixed()
