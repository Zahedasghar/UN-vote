library(pdftools)
library(pdftools)
library(stringi)
library(tidyverse)
library(janitor)
library(tidytext)
# pdf.text <- pdf_text("data/1377233084_761.pdf")
# 
# 
# # Replace "your_file.pdf" with the actual file path of your PDF document
# pdf_text_data <- readPDF("data/1377233084_761.pdf")
# 
# 
# # Replace "your_file.pdf" with the actual file path of your PDF document
# pdf_text_data <- readPDF(control = list(text = "-layout"), elem=list(uri="data/1377233084_761.pdf"))
# 
# 
# glimpse(text)
# 
# tidy_text <- pdf.text |> tibble(text)
# 

# spkdf <- spk |>  tibble(line = 1:4, text = spk)
# 
# spkdf |> unnest_tokens(word, text)
# 
# library(stopwords)
# 
# spkdf |> unnest_tokens(word, text) |> anti_join(stop_words)
# 
# 
# spkdf |> unnest_tokens(word, text) |> anti_join(stop_words) |> 
#   count(word,sort = TRUE)
# 
# 
# 
# spkdf |> unnest_tokens(word, text) |> anti_join(stop_words) |> 
#   count(word,sort = TRUE) |> 
#   filter(n>2)




save(spk, file="data/spk.RData")

load("data/spk.RData")


  tibble(line = 1, text = spk) |>  unnest_tokens(word, text) |> anti_join(stop_words) |> 
  count(word,sort = TRUE ) |> filter(n>2) |> mutate(word = reorder(word, n)) %>%
    ggplot(aes(n, word)) +
    geom_col() +
    labs(y = NULL)
  
 eco <-  pdf_text("data/Overview.pdf")

 eco |> tibble(lin=1:16, text=eco) -> eco_ov

 eco_ov |> unnest_tokens(word, text) |> anti_join(stop_words) |> 
   count(word, sort=TRUE) |> filter(n>30)
 
 
 eco_ov |> unnest_tokens(word, text) |> anti_join(stop_words) |> 
   count(word, sort=TRUE) |> filter(n>10)|> mutate(word = reorder(word, n)) %>%
   ggplot(aes(n, word)) +
   geom_col() +
   labs(y = NULL)

 
 
## Rebuttal by Fin Ministry
 
atif_mian <- c("Atif Mian, a well-respected economist, has criticized Pakistan’s economic policy terming it ‘non-sensical’. While comparing the experience of Ghana and Sri Lanka, he has concluded that Pakistan should “take decisive actions, aggressively restructure and take courageous actions”. This is a veiled suggestion to declare default. This is a misplaced criticism made from a purely theoretical point of view. The gentleman has no idea how the practical economics operates in practice. His comparison with Ghana and Sri Lanka, is also misplaced given the incomparably small size of their economies and populations relative to Pakistan.

Fundamentally, he didn’t care to analyze the structure of Pakistan’s debt which has less than 10% share in commercial bonds/sukuks, with the next maturity falling due in April 2024. The rest of the debt is owed to the multilateral and bilateral creditors. Both these classes of creditors are engaged with Pakistan and none has assessed that Pakistan should default.

The author has completely ignored the deep-rooted reforms Pakistan has undertaken in the last 9 months. These included market exchange rate, interest rate adjustments, mid-year taxation to improve fiscal position, imposition of levy on petroleum products and non-monetization of fiscal deficit. All these actions were undertaken under an IMF program which was unprecedented as never in country’s history such front-loaded conditionality was imposed. However, we accomplished it through heroic efforts. It is unfortunate that despite such actions, the staff level agreement (SLA) has still not been reached delaying the release of 9th review tranche. The country is surviving economically and would continue to survive. What Pakistan has done is decisive and courageous; we would continue to walk the road to reforms to stabilize our economy and, in course of time, to steer it toward the path of sustainable growth.

The comparison of nominal exchange rate is also unwarranted. Pakistan’s real exchange rate is currently estimated to be 15% undervalued. The nominal rate is the result of speculation, market manipulation and general distraught from political instability. The undervalued exchange rate is reflective of the fact that underlying fundamentals are improving. Pakistan has historically sold petroleum products at significantly lower prices than regional countries. With petroleum levy of Rs.50 achieved, this doesn’t involve any subsidy from the government. It would be unwise to levy additional tax on consumers on top of prices that have doubled in less than a year, especially when they are facing rising inflation. The author has cited this as an example of non-sensical policies. This is simply a misplaced example.

Pakistan economy has suffered because of international shocks of COVID, Ukraine War and devastating floods of last summer. The challenges that resulted from an overly heated economy, bequeathed to us in April 2022, and the breach of IMF conditionality on the eve of departure of PTI government, have been overcome by the present government. The current account deficit, the primary indicator of balance of payments imbalance has firmly been brought down from a high of $17.5 billion to around $3.2 billion. This achievement is a reflection of bringing the economy to within its latent strengths and not on borrowed resources.

The author is also oblivious to unprecedented political challenges faced by the country. We are not living in calm and serene times. The present situation has major repercussions for the economy. With political stability likely to emerge soon, there would be a major economic turnaround.")


atif_mian |> tibble(line=1, text=atif_mian) |> unnest_tokens(word, text) |> anti_join(stop_words)|> 
  count(word, sort=TRUE) |> filter(n>2)|> mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
