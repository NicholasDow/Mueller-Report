#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readr)
library(tidyverse)
library(tidytext)
library(tidyr)
library(plotly)
library(dplyr)
library(shinydashboard)

data(stop_words)

bigram_bars <- read_csv("https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv") %>% 
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
  facet_wrap(~group_page, scales = "free_y") +
  coord_flip()

bigram_table <- read_csv("https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  filter(!grepl("\\d", bigram)) %>% 
  filter(bigram != "NA NA") %>% 
  mutate(group_page = page %/% 50) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  count(word1, word2, sort = TRUE) %>% 
  unite(bigram, word1, word2, sep = " ") %>% 
  head(30)
  

report <- read_csv("https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv") %>% 
  unnest_tokens(word, text) %>% 
  filter(!grepl("\\d", word))

report_ngram <- read_csv("https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv") %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

total_book <- report %>% 
  count()

freq_word <- report %>% 
  anti_join(stop_words)
  
sentiments_freq <- freq_word %>% 
  inner_join(get_sentiments("afinn"))
  
contributions <- report %>%
  count(word) %>% 
  inner_join(get_sentiments("afinn")) %>% 
  mutate(contribution = score * n / sum(n)) %>% 
  arrange(desc(contribution))

bing <- get_sentiments("bing") %>% 
  filter(word != "trump")

over_time <- report %>% 
  inner_join(bing) %>%
  count(page, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(total_sentiment = positive - negative)

over_time <- over_time %>% 
  complete(page = seq(1, 448)) %>% 
  replace(is.na(.),0)



ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Mueller Report Sentiment"),
  dashboardSidebar(
    checkboxGroupInput(
      inputId = "type", 
      label = "Type of Shot",
      choices = c("place", "holder"), 
      selected = "place"
    ),
    sidebarMenu(
      menuItem(text = "Sentiment Overtime",
               tabName = "Sent_overtime"),
      menuItem(text = "Word Frequency",
               tabName = "word_freq"),
      menuItem(text = "bigram bars overflow",
               tabName = "bigram_bar"),
      menuItem(text = "The Report",
               tabName = "report"),
      menuItem(text = "Page sentiment",
               tabName = "page_sentiment"),
      menuItem(text = "Web Scraping and Searches",
               tabName = "Web_scraping"),
      menuItem(text = "Github",
               tabName = "github")
    )
    
  ),
  
  
  # We create a dashboard body for the graph and text
  
  dashboardBody(
    tabItems(
      # We output our map here, and set height to avoid scroll
      tabItem(
        tabName = "Sent_overtime",
        plotlyOutput("plot"),
        plotlyOutput("plot_mean"),
        sliderInput(
          inputId = "bin",
          label = "Bin Width",
          min = 1,
          max = 50,
          value = 5,
          sep = "",
          step = 1,
          animate = animationOptions(interval = 1200, loop = FALSE))
        
      ),
      tabItem(
        tabName = "word_freq",
        plotOutput("table"),
        sliderInput(
          inputId = "wordbin",
          label = "Page Denominator?",
          min = 50,
          max = 125,
          value = 50,
          sep = "",
          step = 25
        ),
        tableOutput("bigram_table")
      ),
      tabItem(
        tabName = "bigram_bar",
        plotOutput("bigram_bars")
        
      ),
      tabItem(
        tabName = "github",
        h3("https://github.com/NicholasDow/Mueller-Report")
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  sum_data <- reactive({
    total_opinion <- over_time %>% 
      mutate(group_page = 1 + (1:nrow(over_time) %/% input$bin)) %>%
      group_by(group_page) %>%
      summarise(total_sentiment = sum(total_sentiment)) %>%
      mutate(actual_page = group_page*input$bin) %>% 
      mutate(actual_actual_page = paste0(((group_page - 1)* input$bin), "-",(group_page*input$bin))) 
    
  })
  mean_data <- reactive({
    total_opinion <- over_time %>% 
      mutate(group_page = 1 + (1:nrow(over_time) %/% input$bin)) %>%
      group_by(group_page) %>%
      summarise(mean_sentiment = mean(total_sentiment)) %>%
      mutate(actual_page = group_page*input$bin) %>% 
      mutate(actual_actual_page = paste0(((group_page - 1)* input$bin), "-",(group_page*input$bin))) 
    
  })
  
  word_freq_data <-  reactive({
    x <- report %>% 
      mutate(group_page = page %/% input$wordbin)
    
  })
  
  output$plot <- renderPlotly({
    total_opinion <- sum_data()
    plot_mueller <- plot_ly(data = total_opinion, 
                            x = ~actual_page, 
                            y = ~total_sentiment, 
                            type = 'scatter', 
                            mode = 'lines') 
   })
   output$plot_mean <- renderPlotly({
     mean_opinion <- mean_data()
     plot_mueller <- plot_ly(data = mean_opinion, 
                             x = ~actual_page, 
                             y = ~mean_sentiment, 
                             type = 'scatter', 
                             mode = 'lines') %>%
       
       layout(
         title = 'Mean Sentiment',
         xaxis = list(
           title = 'Ceiling Page Number'
         ),
         yaxis = list(
           title = 'Sentiment', 
           range = c(-18, 18)
         )) 
   })
   output$table <- renderPlot({
     test_count <- word_freq_data() 
     
     test_count %>%
       count(group_page, word) %>% 
       bind_tf_idf(word, group_page, n) %>% 
       replace(is.na(.),0) %>% 
       arrange(desc(tf_idf)) %>%
       mutate(word = factor(word, levels = rev(unique(word)))) %>% 
       group_by(group_page) %>% 
       top_n(10) %>% 
       ungroup() %>%
       ggplot(aes(word, tf_idf, fill = group_page)) +
       geom_col(show.legend = FALSE) +
       labs(x = NULL, y = "Zipp's Law Weighted Frequency") +
       facet_wrap(~group_page, scales = "free_y") +
       coord_flip()
   })
   output$bigram_table <- renderTable(bigram_table)
   output$bigram_bars <- renderPlot(bigram_bars)
}

# Run the application 
shinyApp(ui = ui, server = server)

