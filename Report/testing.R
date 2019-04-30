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

# library(igraph)
# library(ggraph)
# read_csv("https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv") %>% 
#   unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% 
#   filter(!grepl("\\d", trigram)) %>% 
#   filter(trigram != "NA NA NA") %>% 
#   separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
#   filter(!word1 %in% stop_words$word,
#          !word2 %in% stop_words$word,
#          !word3 %in% stop_words$word) %>%
#   count(word1, word2, word3, sort = TRUE) %>% 
#   filter(n > 10) %>% 
#   graph_from_data_frame() %>% 
#   ggraph(layout = "fr") +
#   geom_edge_link() +
#   geom_node_point() + 
#   geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
#   theme_void()

# %>% 
#   separate(bigram, c("word1", "word2"), sep = " ") %>% 
#   filter(!word1 %in% stop_words$word) %>%
#   filter(!word2 %in% stop_words$word) %>% 
#   count(word1, word2, group_page) %>% 
#   unite(bigram, word1, word2, sep = " ") %>% 
#   bind_tf_idf(bigram, group_page, n) %>% 
#   mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
#   group_by(group_page) %>% 
#   top_n(6) %>% 
#   ungroup() %>% 
#   ggplot(aes(bigram, tf_idf, fill= group_page)) +
#   geom_col(show.legend = FALSE) +
#   labs(x = NULL, y = "Zipp's Law weighting") + 
#   facet_wrap(~group_page, ncol = 1, scales = "free") +
#   coord_flip()
#   
# library(ggpage)
# 
# mueller_report <- read_csv("https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv")
# mueller_pages <- 
#   read_csv("https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv") %>%
#   # pad pages with fewer lines than expected
#   complete(
#     page, 
#     line = 1:max(mueller_report$line),
#     fill = list(text = "")
#   ) %>% 
#   # Pre-process for {ggpage}
#   ggpage_build(
#     ncol = 30, 
#     bycol = FALSE, 
#     page.col = "page", 
#     wtl = FALSE, 
#     x_space_pages = 10,
#     y_space_pages = 100
#   ) %>% 
#   mutate(
#     color = case_when(
#       str_detect(word, "trump|president") ~ "Trump",
#       str_detect(word, "russia")     ~ "Russia",
#       str_detect(word, "cohen")      ~ "Cohen",
#       str_detect(word, "co(m|rn)ey") ~ "Comey",
#       str_detect(word, "flynn")      ~ "Flynn",
#       str_detect(word, "manafort")   ~ "Manafort",
#       str_detect(word, "sessions")   ~ "Sessions",
#       str_detect(word, "mcgahn")     ~ "McGahn",
#       TRUE ~ "normal"
#     ),
#     color = factor(color, c(
#       "Trump", "Russia", "Cohen", "Comey",
#       "Flynn", "Manafort", "Sessions", "McGahn", "normal"
#     ))
#   )
# colors <- rep("", length(levels(mueller_pages$color)))
# names(colors) <- levels(mueller_pages$color)
# colors["Trump"]    <- "#FF4023"
# colors["Russia"]   <- "#004983"
# colors["Cohen"]    <- "#FF922E"
# colors["Comey"]    <- "#559B30"
# colors["Flynn"]    <- "#4D276D"
# colors["Manafort"] <- "#7BCAFD"
# colors["Sessions"] <- "#7F1327"
# colors["McGahn"]   <- "#FFD040"
# colors["normal"]   <- "#d0d0d0"
# 
# ggpage_plot(mueller_pages) +
#   aes(fill = color) +
#   scale_fill_manual(
#     values = colors, 
#     breaks = setdiff(names(colors), "normal")
#   ) +
#   labs(fill = NULL) +
#   guides(fill = guide_legend(nrow = 1)) +
#   theme(legend.position = "bottom")



  
  