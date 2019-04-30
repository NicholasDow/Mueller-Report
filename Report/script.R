library(readr)
library(tidyverse)
library(tidytext)
library(tidyr)
library(plotly)
library(dplyr)

data(stop_words)

data1 <- read_csv("https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  filter(!grepl("\\d", bigram)) %>% 
  filter(bigram != "NA NA") %>% 
  mutate(group_page = paste("Section ", 1 + page %/% 113)) %>% 
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
  facet_wrap(~group_page, scales = "free_y") +
  coord_flip()

write_rds(data1, path = "Report/data1.rds")

data2 <- read_csv("https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  filter(!grepl("\\d", bigram)) %>% 
  filter(bigram != "NA NA") %>% 
  mutate(group_page = page %/% 50) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  count(word1, word2, sort = TRUE) %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  filter(n >= 5)

write_rds(data2, path = "Report/data2.rds")

data3 <- read_csv("https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv") %>% 
  unnest_tokens(word, text) %>% 
  filter(!grepl("\\d", word))

write_rds(data3, path = "Report/data3.rds")

data4 <- read_csv("https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  

write_rds(data4, path = "Report/data4.rds")