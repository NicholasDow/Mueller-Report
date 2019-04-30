#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# Readr to read in files

library(readr)

# Tidyverse for data manipulation 

library(tidyverse)

# Tidytext for text mining

library(tidytext)

# Tidyr for data manipulation

library(tidyr)

# Plotly for graphs

library(plotly)

# Dplyr for table manipulation

library(dplyr)

# Shinydashboards for better ui

library(shinydashboard)

# Read in data constructed by script so that the page loads faster

bigram_bars <- read_rds("data1.rds")
bigram_table <- read_rds("data2.rds")
report <- read_rds("data3.rds")
report_ngram <- read_rds("data4.rds")

# We find the total number of words and provide a table

total_book <- report %>% 
  
  
  # We count the words
  
  count(word) %>% 
  
  # We sort out stop_words which are "the and what here, ect"
  
  anti_join(stop_words) %>% 
  
  # We left join sentiment scores so they are there if people want them
  
  left_join(get_sentiments("afinn")) %>% 
  
  # We get rid of na values
  
  replace(is.na(.),0) %>% 
  
  # We give them contribution scores
  
  mutate(contribution = score * n / sum(n)) %>% 
  
  # We arrange by desc n to show the most used words
  
  arrange(desc(n))

# We create a variable 

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

searchable_report <- read_csv("https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv") %>% 
  filter(line != 2) %>%
  filter(!(page == 1 & line == 3))

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Mueller Report Sentiment"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "The Report",
               tabName = "the_report",
               icon = icon("home")),
      menuItem(text = "Sentiment Overtime",
               tabName = "Sent_overtime",
               icon = icon("smile")),
      menuItem(text = "Word Frequency",
               tabName = "word_freq",
               icon = icon("signal")),
      menuItem(text = "Bigram Analysis",
               tabName = "bigram_bar",
               icon = icon("dice-two")),
      menuItem(text = "Report Search",
               tabName = "search",
               icon = icon("search")),
      menuItem(text = "Web Scraping and Searches",
               tabName = "Web_scraping",
               icon = icon("search-plus")),
      menuItem(text = "About & Sources",
               tabName = "about",
               icon = icon("align-justify"))
    )
    
  ),
  
  
  # We create a dashboard body for the graph and text
  
  dashboardBody(
    tabItems(
      # We output our map here, and set height to avoid scroll
      tabItem(
        tabName = "Sent_overtime",
        h1("Sentiment Over Time", align = "center"),
        p("The Muller Report is over 400 pages long. Without actually reading it
          how could you possibly figure out if what it says? is for or against
          what ever the report is about?", br(), "You could assign commonly used words a score
          for their negativeness and positiveness and measure the values through the paper. 
          using the afinn lexicon from tidytext we do just that to find that the overall report
          is negative in nature. Drag the page bin size to right to get a more overall idea of sentiment", align = "center"),
        box(plotlyOutput("plot")),
        box(plotlyOutput("plot_mean")),
        sliderInput(
          inputId = "bin",
          label = "Page Bin Width",
          min = 1,
          max = 50,
          value = 5,
          sep = "",
          step = 1,
          animate = animationOptions(interval = 1200, loop = FALSE))
        
      ),
      tabItem(
        tabName = "word_freq",
        h1("Highest Count Words in Range with Zipf's law Weight on y axis"),
        p("In order to summarise the report in sections we use use a function
          apply's zipf's law to weight words by their relative occurence in
          the other chapers. This will allow us to get an idea about the topics
          in each chapter. You can read more about that", a(href = "https://en.wikipedia.org/wiki/Zipf%27s_law", "here"), "or", a(href = "https://www.tidytextmining.com/tfidf.html#zipfs-law", "here")),
        plotOutput("table"),
        sliderInput(
          inputId = "wordbin",
          label = "Page Denominator",
          min = 50,
          max = 125,
          value = 50,
          sep = "",
          step = 25
        ),
        p("Most Common Words in the Report"),
        dataTableOutput("total_book")
      ),
      tabItem(
        tabName = "bigram_bar",
        h1("Most common pairs of words in each quarter of the Report"),
        p("Zipf's law allows us to find the most important words relative to the other quarters.
          by looking the words with the highest counts in each section we can figure out which pairs
          of words are the most important both by count in the chapter and weighting"),
        plotOutput("bigram_bars"),
        p("You can look at the most common bigram's here"),
        dataTableOutput("bigram_table")
        
      ),
      tabItem(
        tabName = "about",
        h1("About", align = "center"),
        p("  Hi, my name is Nicholas Dow. I am a member of the 
          Harvard Class of 2022 concentrating in Statistics. 
          I put together this project as a my final project for ", 
          a(href = "https://www.davidkane.info/files/gov_1005.html", "Gov 1005"),
          ". This project was mostly written in R using shiny and shiny dashboards 
          to create the interface and layout. You can find the source files for this project ", a(href = "https://github.com/NicholasDow/Mueller-Report", "here"),
          "and much of the data came from", a(href = "https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv", "here"),
          "(though more attention is given to the sources in the sources tab if you are interested).",
          align = "center"),
        p("Email:", a("dow@college.harvard.edu")),
        p("Github:", a("https://github.com/NicholasDow")),
        p("LinkedIn:", a("https://www.linkedin.com/in/nicholas-dow-1824b2174/")), 
        h1("Sources", align = "center"),
        p("https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv", align = "center"),
        p("https://www.tidytextmining.com", align = "center"),
        p("https://www.justice.gov/storage/report.pdf", align = "center"),
        p("https://www.reuters.com/article/us-usa-trump-prosecutors-explainer/explainer-probes-spawned-by-mueller-target-trump-business-others-idUSKCN1S60UY", align = "center")
      ),
      tabItem(
        tabName = "the_report",
        h1("What is in the Report", align = "center"),
        p("The Mueller Report is a 448 page document publically shared in a redacted form on March 22, 2019. It looks into Russian Interference in the 2016 Presidential Election. It is 
          officially titled \"Report on the Investigation into Russian Interference in the 2016 Prsidential Election\". The Report was produced by a special council led by Robert Mueller, 
          put together by Democratic Representatives in the US Congress after the firing of two Directors of the FBI investigating Russian interference with the 2016 election.", br(), br(), "Though
          offically the report found no collusion between Trump and Russia, it did find serious interference by Russia and in the process made 34 indictments", br(), br(), "
          The Report is split into two volumes. The first looks at Russian interference, and starting on page 208 the second volume looks at Potential Obstruction of Justice.", align = "center"),
        h3("Volume 1"),
        p("Volume 1 of the Mueller report found the Russian IRA or Internet Research Agency did interfere with the 2016 presidential Election. They had specifically
          paid and constructed fake materials that were circulated on social media to support the Donald Trump and Bernie Sanders campaign, in order to oppose
          Hillary Clinton's campaign. It also found the Involvement of the GRU, or Russian Intelligence Agency, in the hacking fo the DNC and the release of damaging 
          material onto Wikileaks."),
        h3("Volume 2"),
        p("Volume 2 looks at specific cases where there was the potential to be obstructions of justice by Donald Trump involving Russia, obstruction including attempts
          withhold or manipulate the report. You can read more about it", a(href = "https://www.reuters.com/article/us-usa-trump-prosecutors-explainer/explainer-probes-spawned-by-mueller-target-trump-business-others-idUSKCN1S60UY", "here"),
          a(href = "https://www.nytimes.com/2019/04/20/us/politics/mueller-report-summary.html", "here"))
      ),
      tabItem(
        tabName = "search",
        h1("Sources and pdfReader"),
        p("The Mueller report is from the Department of Justice. They released a pdf with redacted information and explainations pasted on top of readactions.
          You can view the actual report ", a(href = "https://cdn.cnn.com/cnn/2019/images/04/18/mueller-report-searchable.pdf", "here"), br(), br(),
          "To make the report analyzable using textmining methods, the report was read in using a package called pdf reader.
           I had to manually removed lines that were miss read, pdfread misinterpreting crossed out lines. I specifically removed the second line of every page. pdfreader also missed some spaes", br(), br(),
          "Because of the redactions, it might also be confusing to see the words HOM, Harm to Ongoing Investigation, Personal Privacy, Grand Jury, and Investigative Technique in seemingly random places
          these were reasons for redaction. Looking at them in this table will not give you an idea of how much of the text was redacted. I suggest you look at the
          ", a(href = "https://cdn.cnn.com/cnn/2019/images/04/18/mueller-report-searchable.pdf", "report"), "if you are interested in the size of these redactions.
          this part of the tool is really meant to give you a quick way to reference or look up information in relation to some of the graphs I show elsewhere"),
        splitLayout(
          sliderInput(inputId = "page_number",
                      label = "Page Number",
                      min = 1,
                      max = 448,
                      value = c(1,448),
                      sep = ""),
          sliderInput(inputId = "line_number",
                      label = "Line number",
                      min = 1,
                      max = 59,
                      value = c(1, 59),
                      sep = "")
        ),
        dataTableOutput("searchable_report")
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
      mutate(base_page = page %/% input$wordbin) %>% 
      mutate(group_page = paste("Pages ", ((input$wordbin) * (page %/% input$wordbin)), "-", ( input$wordbin * (1 + (page %/% input$wordbin)))))
    
  })
  
  searched_report <- reactive({
    x <- searchable_report %>% 
      filter(line >= input$line_number[1] & line <= input$line_number[2]) %>% 
      filter(page >= input$page_number[1] & page <= input$page_number[2])
  })
  
  
  output$plot <- renderPlotly({
    total_opinion <- sum_data()
    plot_mueller <- plot_ly(data = total_opinion, 
                            x = ~actual_page, 
                            y = ~total_sentiment, 
                            type = 'scatter', 
                            mode = 'lines') %>% 
      layout(
        title = 'Sentiment Sum per Bin',
        xaxis = list(
          title = 'Ceiling Page Number'
        ),
        yaxis = list(
          title = 'Sentiment Score'
        )) 
   })
   output$plot_mean <- renderPlotly({
     mean_opinion <- mean_data()
     plot_mueller <- plot_ly(data = mean_opinion, 
                             x = ~actual_page, 
                             y = ~mean_sentiment, 
                             type = 'scatter', 
                             mode = 'lines') %>%
       
       layout(
         title = 'Mean Sentiment per Bin',
         xaxis = list(
           title = 'Ceiling Page Number'
         ),
         yaxis = list(
           title = 'Sentiment Score', 
           range = c(-18, 18)
         )) 
   })
   output$table <- renderPlot({
     test_count <- word_freq_data() 
     
     y <- test_count %>%
       count(group_page, word, base_page) %>% 
       bind_tf_idf(word, group_page, n) %>% 
       replace(is.na(.),0) %>% 
       arrange(desc(tf_idf)) %>%
       mutate(word = factor(word, levels = rev(unique(word)))) %>% 
       group_by(group_page) %>% 
       top_n(10) %>% 
       ungroup()
     y$group_page <-  reorder(y$group_page, y$base_page)
     y %>%
       ggplot(aes(word, tf_idf, fill = group_page)) +
       geom_col(show.legend = FALSE) +
       labs(x = NULL, y = "Zipf's Law Weighted Frequency") +
       facet_wrap(~group_page, scales = "free_y") +
       coord_flip()
   })
   output$bigram_table <- renderDataTable(bigram_table)
   output$bigram_bars <- renderPlot(bigram_bars)
   output$searchable_report <- renderDataTable({
     x <- searched_report()
     x
     })
   output$total_book <- renderDataTable({total_book})
}

# Run the application 
shinyApp(ui = ui, server = server)

