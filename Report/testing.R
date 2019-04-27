library(readr)
library(tidyverse)
library(tidytext)
library(tidyr)
library(plotly)
library(dplyr)

# report <- read_csv("https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv") %>% 
#   unnest_tokens(word, text)
# 
# contributions <- report %>%
#   count(word) %>% 
#   inner_join(get_sentiments("afinn")) %>% 
#   mutate(contribution = score * n / sum(n)) %>% 
#   arrange(desc(contribution))
# 
# bing <- get_sentiments("bing") %>% 
#   filter(word != "trump")
# 
# over_time <- report %>% 
#   inner_join(bing) %>%
#   count(page, sentiment) %>% 
#   spread(sentiment, n, fill = 0) %>% 
#   mutate(total_sentiment = positive - negative) 
# 
# over_time <- over_time %>% 
#   complete(page = seq(1, 448)) %>% 
#   replace(is.na(.),0)
# 
# total_opinion <- over_time %>% 
#   mutate(group_page = 1 + (1:nrow(over_time) %/% 30)) %>%
#   group_by(group_page) %>%
#   summarise(total_sentiment = sum(total_sentiment)) %>% 
#   mutate(actual_page = group_page*30) %>% 
#   ggplot() +
#   geom_line(aes(actual_page, total_sentiment)) +
#   geom_smooth(aes(actual_page, total_sentiment)) +
#   geom_hline(yintercept = 0) %>% 
#   
# 
# mean_opinion <- over_time %>% 
#   mutate(group_page = 1 + (1:nrow(over_time) %/% 30)) %>%
#   group_by(group_page) %>%
#   summarise(total_sentiment = mean(total_sentiment)) %>% 
#   mutate(actual_page = group_page*30) %>% 
#   ggplot() +
#   geom_line(aes(actual_page, total_sentiment)) +
#   geom_smooth(aes(actual_page, total_sentiment)) +
#   geom_hline(yintercept = 0)

# test_count <- read_csv("https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv") %>% 
#   unnest_tokens(word, text)
# # book <- read_csv("https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv") %>% 
# #   unnest_tokens(word, text) %>% 
# #   count(word)
# 
# test_count %>%
#   mutate(group_page = page %/% 100) %>%
#   count(group_page, word) %>% 
#   bind_tf_idf(word, group_page, n) %>% 
#   replace(is.na(.),0) %>% 
#   arrange(desc(tf_idf)) %>%
#   mutate(word = factor(word, levels = rev(unique(word)))) %>% 
#   group_by(group_page) %>% 
#   top_n(15) %>% 
#   ungroup() %>%
#   ggplot(aes(word, tf_idf, fill = group_page)) +
#   geom_col(show.legend = FALSE) +
#   labs(x = NULL, y = "zipp's law read more about this, figure out formating, n = 100") +
#   facet_wrap(~group_page, ncol = 2, scales = "free") +
#   coord_flip()
read_csv("https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  filter(!grepl("\\d", bigram)) %>% 
  filter(bigram != "NA NA") %>% 
  mutate(group_page = page %/% 113) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  count(word1, word2, group_page) %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  bind_tf_idf(bigram, group_page, n) %>% 
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(group_page) %>% 
  top_n(6) %>% 
  ungroup() %>% 
  ggplot(aes(bigram, tf_idf, fill= group_page)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Zipp's Law weighting") + 
  facet_wrap(~group_page, ncol = 1, scales = "free") +
  coord_flip()
  



  
  