# Read in data constructed by script so that the page loads faster

bigram_bars <- read_rds("data1.rds")
bigram_table <- read_rds("data2.rds")
report <- read_rds("data3.rds")

# This line is speical. it reads in data that was webscraped, the process took a while because it was done in an odd manner using rcrawler
# I am reading the data in here from an rds file, but it took longer to put together. The main advantage of a more polished version of what
# I did is the ability to crawl into the depths > 1 of a webpage

summary <- read_rds("summary.rds")

# We find the total number of words and provide a table

total_book <- report %>%
  
  # We count the words
  
  count(word) %>%
  
  # We sort out stop_words which are "the and what here, ect"
  
  anti_join(stop_words) %>%
  
  # We left join sentiment scores so they are there if people want them
  
  left_join(get_sentiments("afinn")) %>%
  
  # We get rid of na values
  
  replace(is.na(.), 0) %>%
  
  # We give them contribution scores
  
  mutate(contribution = score * n / sum(n)) %>%
  
  # We arrange by desc n to show the most used words
  
  arrange(desc(n))

# We take trump, which is considered a positive word out of the bing lexicon as it is a name

bing <- get_sentiments("bing") %>%
  filter(word != "trump")

# We filter the report to find the net change in sentiment on each page

over_time <- report %>%
  
  # we join the words by bing, which tells us if the words were negative or positive
  
  inner_join(bing) %>%
  
  # We count page and sentiment to get tally's on each page
  
  count(page, sentiment) %>%
  
  # We spread so that we can do col manipulations and fill by 0 for the same reason
  
  spread(sentiment, n, fill = 0) %>%
  
  # We take a difference and make it a col
  
  mutate(total_sentiment = positive - negative)

# We fill missing pages with their numbers, and replace the empty values for those pages.
# So we can do manipulations later in an interactive graph

over_time <- over_time %>%
  
  # Full Report was 1 - 448 pages
  
  complete(page = seq(1, 448)) %>%
  
  # You cant add na, so we make it 0
  
  replace(is.na(.), 0)

# We read the data as a csv

searchable_report <-
  read_csv(
    "https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv"
  ) %>%
  
  # We get rid of junk lines, explained on page
  
  filter(line != 2) %>%
  filter(!(page == 1 & line == 3))

# We make a server to interpret all the info inputs, create output, and keep track of the current actions

server <- function(input, output, session) {
  
  # We create a reactive varible, dependent on input values that update
  
  cumulative_data <- reactive({
    
    # This is the value that is returned, we take a value from above rds possibly and interpret
    
    x <- over_time %>%
      
      # We create groups dependent on the slider var
      
      mutate(group_page = 1 + (1:nrow(over_time) %/% input$bin)) %>%
      
      # We group by those groups
      
      group_by(group_page) %>%
      
      # We get the total sentiment in each of the groups
      
      summarise(total_sentiment = sum(total_sentiment)) %>%
      
      # We are going to take the cumulative sum of our total
      
      mutate(total_sentiment = cumsum(total_sentiment)) %>%
      
      # We fix the labels to represent the actual bins
      
      mutate(actual_page = group_page * input$bin) %>%
      
      # We make an actual actual page that will be used later to be the label factored by the label created above
      # I did this else where so fixing this one is just a matter of time
      
      mutate(actual_actual_page = paste0(((group_page - 1) * input$bin), "-", (group_page *
                                                                                 input$bin)))
    
  })
  sum_data <- reactive({
    
    # This is the value that is returned, we take a value from above rds possibly and interpret
    
    total_opinion <- over_time %>%
      
      # We create groups dependent on the slider var
      
      mutate(group_page = 1 + (1:nrow(over_time) %/% input$bin)) %>%
      
      # We group by those groups
      
      group_by(group_page) %>%
      
      # We get the total sentiment in each of the groups
      
      summarise(total_sentiment = sum(total_sentiment)) %>%
      
      # We fix the labels to represent the actual bins
      
      mutate(actual_page = group_page * input$bin) %>%
      
      # We make an actual actual page that will be used later to be the label factored by the label created above
      # I did this else where so fixing this one is just a matter of time
      
      mutate(actual_actual_page = paste0(((group_page - 1) * input$bin), "-", (group_page *
                                                                                 input$bin)))
    
  })
  
  # We do exactly what we did in the reactive var above except we take the mean, this is a 3 letter difference
  
  mean_data <- reactive({
    total_opinion <- over_time %>%
      mutate(group_page = 1 + (1:nrow(over_time) %/% input$bin)) %>%
      group_by(group_page) %>%
      
      # Instead of taking group sum, we take group mean
      
      summarise(mean_sentiment = mean(total_sentiment)) %>%
      mutate(actual_page = group_page * input$bin) %>%
      mutate(actual_actual_page = paste0(((group_page - 1) * input$bin), "-", (group_page *
                                                                                 input$bin)))
    
  })
  
  # We provide data to a plotted table, that reacts to bin size
  
  word_freq_data <-  reactive({
    
    # We take our report and interpret it
    
    x <- report %>%
      
      # We create a var that we can group by page with
      
      mutate(base_page = page %/% input$wordbin) %>%
      
      # We create a label to be used by factoring group group page
      
      mutate(group_page = paste("Pages ", ((input$wordbin) * (page %/% input$wordbin)
      ), "-", (input$wordbin * (
        1 + (page %/% input$wordbin)
      ))))
    
  })
  # We create the searchable report var to be displayed in a table here
  
  searched_report <- reactive({
    
    x <- searchable_report %>%
      
      # We filter the report by the variable ranges provided by the slider
      
      filter(line >= input$line_number[1] &
               line <= input$line_number[2]) %>%
      filter(page >= input$page_number[1] &
               page <= input$page_number[2])
  })
  
  # We finally render a plotly graph here to be displayed in the ui
  
  output$plot <- renderPlotly({
    
    # We take the reactive var and input
    
    total_opinion <- sum_data()
    
    # We return a line graph using scattern points and the data in sum data
    
    plot_mueller <- plot_ly(
      data = total_opinion,
      x = ~ actual_page,
      y = ~ total_sentiment,
      type = 'scatter',
      mode = 'lines'
    ) %>%
      
      # This just provides the proper labels for the graph
      
      layout(
        title = 'Sentiment Sum per Bin',
        xaxis = list(title = 'Ceiling Page Number'),
        yaxis = list(title = 'Sentiment Score')
      )
  })
  
  # We do the exact same thing with the cumsum plot
  
  output$plot_cumsum <- renderPlotly({
    
    # Read in reactive var
    
    cumsum_opinion <- cumulative_data()
    
    # Return a plotly line graph
    
    plot_mueller <- plot_ly(
      data = cumsum_opinion,
      x = ~ actual_page,
      y = ~ total_sentiment,
      type = 'scatter',
      mode = 'lines'
    ) %>%
      
      # We create the basic layout with titles and labels
      
      layout(
        title = 'Cumsum Sentiment per Bin',
        xaxis = list(title = 'Ceiling Page Number'),
        yaxis = list(title = 'Sentiment Score')
      )
    
  })
  
  # We are outputting the mean plotly value
  
  output$plot_mean <- renderPlotly({
    
    # Read in reactive var
    
    mean_opinion <- mean_data()
    
    # Return a plotly line graph
    
    plot_mueller <- plot_ly(
      data = mean_opinion,
      x = ~ actual_page,
      y = ~ mean_sentiment,
      type = 'scatter',
      mode = 'lines'
    ) %>%
      
      # We create the basic layout with titles and labels
      
      layout(
        title = 'Mean Sentiment per Bin',
        xaxis = list(title = 'Ceiling Page Number'),
        yaxis = list(title = 'Sentiment Score',
                     
                     # I wanted the data to really represent the location of 0 sentiment hence a symetric range
                     
                     range = c(-18, 18))
      )
  })
  
  # We render a ggplot here, its super slow so I will fix later
  
  output$table <- renderPlot({
    # We load in a reactive data
    
    test_count <- word_freq_data()
    
    # We create a var so we can factor labels later
    
    y <- test_count %>%
      
      # We count by these three variables, but we only need word and one of the two others. An extra var is there to be preserved for labs
      
      count(group_page, word, base_page) %>%
      
      # This takes a while, it basically creates a weighting based on the other variables
      
      bind_tf_idf(word, group_page, n) %>%
      
      # cant add NA
      
      replace(is.na(.), 0) %>%
      
      # We want to see the top scores and take them
      
      arrange(desc(tf_idf)) %>%
      
      # Do this to make it easier to do other stuff later. In this case it helps makes words unique in their own graphs
      
      mutate(word = factor(word, levels = rev(unique(word)))) %>%
      
      # We group by group page
      
      group_by(group_page) %>%
      
      # Take the top values from each of them
      
      top_n(10) %>%
      
      # Ungroup for graph
      
      ungroup()
    
    # We basically make group_page a factor based on base page which is a number. 50-100 > 400-500 otherwise
    
    y$group_page <-  reorder(y$group_page, y$base_page)
    
    # start again on making the graphic
    
    y %>%
      
      # We assign the values here
      
      ggplot(aes(word, tf_idf, fill = group_page)) +
      
      # We get rid of col labs
      
      geom_col(show.legend = FALSE) +
      
      # We add more labs
      
      labs(x = NULL, y = "Zipf's Law Weighted Frequency") +
      
      # Create facetted graphs to show differences between different groups of pages
      
      facet_wrap(~ group_page, scales = "free_y") +
      
      # This makes text readable
      
      coord_flip() +
      
      # Because I'm an edgy teen
      
      theme_dark()
  })
  
  # We render a reactive vars that we created before, no reactivity.
  
  output$bigram_table <- renderDataTable(bigram_table)
  output$bigram_bars <- renderPlot(bigram_bars)
  
  # We read in a single reactive variable
  
  output$searchable_report <- renderDataTable({
    x <- searched_report()
    x
  })
  
  # For some reason one of the pages didn't work correctly, so we remove it here.
  
  summary <- summary %>% filter(info != "Barack ObamaDemocratic")
  
  # We are renderingUI here, basically what we are doing is converting a table to html text
  
  output$summary <- renderUI({
    
    # initalize var
    
    y <- ""
    
    # for loop for summary tbl
    
    for (row in 1:nrow(summary)) {
      
      # We paste everything into a long string formatted accordingly
      
      y <- paste(y, "<br>", "<h4>", summary[row, "i"], "</h4>")
      y <- paste(y, "<br>", "<p>", summary[row, "info"], "</p>")
    }
    
    # We convert the string into html y
    
    HTML(y)
  })
  
  # We render data we created before
  
  output$total_book <- renderDataTable(total_book)
}