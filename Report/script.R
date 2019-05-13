

# We explain the use of these in app.R, all pretty standard

library(readr)
library(tidyverse)
library(tidytext)
library(tidyr)
library(plotly)
library(dplyr)

# Read in data

data1 <-
  read_csv(
    "https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv"
  ) %>%
  
  # Tidytext take out the individual word pairs
  
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  
  # Take out numbers
  
  filter(!grepl("\\d", bigram)) %>%
  
  # Take out NA, but we could use this later to count black space
  
  filter(bigram != "NA NA") %>%
  
  # We break everything into quarters with group page
  
  mutate(group_page = paste("Section ", 1 + page %/% 113)) %>%
  
  # We seperate the bigram pair
  
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  
  # We filter out bigrams that contain stop words
  
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  
  # We put the table backtogether w/o stop
  
  count(word1, word2, group_page) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  
  # We weight everything by contents of the rest of the data
  
  bind_tf_idf(bigram, group_page, n) %>%
  
  # We need this line to make the graph work, but will also use in expanded proj
  
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>%
  
  # We group by page and get the top 6 entries in that page group
  
  group_by(group_page) %>%
  top_n(6) %>%
  
  # We ungroup the data for the chart
  
  ungroup() %>%
  
  # We create the graphic inputing the variables where needed
  
  ggplot(aes(bigram, tf_idf, fill = group_page)) +
  
  # Labs dealt with here
  
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Zipp's Law weighting") +
  
  # We facet by group, look sep data
  
  facet_wrap( ~ group_page, scales = "free_y") +
  
  # Make text labs readable
  
  coord_flip() +
  
  # Gen theme
  
  theme_dark()

# Write into rds

write_rds(data1, path = "Report/data1.rds")

# We create the bigram table here, read in the base data too

data2 <-
  read_csv(
    "https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv"
  ) %>%
  
  # We unnest for bigram
  
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  
  # Get rid of nums and na vals
  
  filter(!grepl("\\d", bigram)) %>%
  filter(bigram != "NA NA") %>%
  
  # Seperate the bigram so we can filter each word for stop words that would natually occur next to key words
  
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  
  # We make counts of the word combos
  
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  
  # Filter out n > 5 to make it load faster
  
  filter(n >= 5)

# Write into rds file for app.R

write_rds(data2, path = "Report/data2.rds")

# This just gives us every word in the report with page and line and no nums

data3 <-
  read_csv(
    "https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv"
  ) %>%
  
  # Tidytext unnest
  
  unnest_tokens(word, text) %>%
  
  # No nums
  
  filter(!grepl("\\d", word))

# We write rds files

write_rds(data3, path = "Report/data3.rds")



# The way that I put together the webcrawler data was a little hacky. First a used a package called webcrawler, which I might cite.
# I kept experimenting with it, but I had a lot of trouble using it in conjunction with rvest, so what I ultimately did was I utilized
# the list that was created through the webcrawler, without actually using the webcrawler.

# I used the command below. It runs like shit so don't run it. and using the Data list to get a lsit of the links in the paragraph tags on the page
# I later figured out a better way of doing this using Rvest, but without the depth capacity that I like about this package. I plan to use the
# Rcrawler package again which is the only reason I am keepin this code


# This command takes forever to run.
# Rcrawler(Website = "https://en.wikipedia.org/wiki/Mueller_Report", no_cores = 3, no_conn = 4, KeywordsFilter = c("Mueller", "Russian", "Russia", "Investigation"), KeywordsAccuracy = 50, crawlZoneCSSPat = "p", MaxDepth=1)

# g is a list of the links I want to read in, I got that from the command above, but I could have also gotten it from htmltreeparse/string searches for a/href

# for (i in g) {
#
#   v <- html_session(i)
#   info <- html_nodes(v, "p") %>% html_text() %>% as.character()
#   x <- data.frame(i,info)
#   grandlist <- rbind(grandlist, x)
# }

# this will both get rid of excess lines \n for example, but will also reduce \n , \n\n, and so on to ""
# grandlist <- grandlist %>% mutate(info = str_trim(info))

# this will get rid of lines where there is only a blank space
# df <- grandlist[-which(grandlist$info == ""), ]

# this will give us the first line of each entry on teh wikipedia page
# grandlist <- grandlist[!duplicated(grandlist$i),]

# this will clean up our names column to make it printable
# grandlist <- grandlist %>% mutate(i = str_remove(i, "https://en.wikipedia.org/wiki/")) %>% mutate(i = str_replace_all(i, "_|%", " "))

# then we will have a list that we can save into an rds file to printed in the main app
