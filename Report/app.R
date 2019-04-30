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

# We create the ui

ui <- dashboardPage(
  # We make the header look red
  
  skin = "red",
  
  # Add Title
  
  dashboardHeader(title = "Mueller Report Sentiment"),
  
  # We make a sidebar with option
  
  dashboardSidebar(
    # We make the sidebar a menu with options
    
    sidebarMenu(
      # In each of the menu items we assign a text value which is to be shown
      # We add a Tab name which hooks it up to a page
      # We add an icon from the dashboard package, it looks nice
      
      menuItem(
        text = "The Report",
        tabName = "the_report",
        icon = icon("home")
      ),
      menuItem(
        text = "Sentiment Overtime",
        tabName = "Sent_overtime",
        icon = icon("smile")
      ),
      menuItem(
        text = "Word Frequency",
        tabName = "word_freq",
        icon = icon("signal")
      ),
      menuItem(
        text = "Bigram Analysis",
        tabName = "bigram_bar",
        icon = icon("dice-two")
      ),
      menuItem(
        text = "Report Search",
        tabName = "search",
        icon = icon("search")
      ),
      menuItem(
        text = "Web Scraping and Searches",
        tabName = "Web_scraping",
        icon = icon("search-plus")
      ),
      menuItem(
        text = "About & Sources",
        tabName = "about",
        icon = icon("align-justify")
      )
    )
    
  ),
  
  
  # We create a dashboard body for the graph, tables, and text
  
  dashboardBody(
    # We create a list of tab items here, that allows us to access and embed items
    
    tabItems(
      # In this tab item we write some text and provide two graphs
      
      tabItem(
        # Hooks up to Menu Item
        
        tabName = "Sent_overtime",
        
        # Title and text
        
        h1("Sentiment Over Time", align = "center"),
        p(
          "The Muller Report is over 400 pages long. Without actually reading it
          how could you possibly figure out if what it says? is for or against
          what ever the report is about?",
          br(),
          "You could assign commonly used words a score
          for their negativeness and positiveness and measure the values through the paper.
          using the afinn lexicon from tidytext we do just that to find that the overall report
          is negative in nature. Drag the page bin size to right to get a more overall idea of sentiment",
          align = "center"
        ),
        
        # This puts our two plots in two boxes to be manipulated by a slider
        
        box(plotlyOutput("plot")),
        box(plotlyOutput("plot_mean")),
        
        # We add a slider input for the bin width
        
        sliderInput(
          # This hooks it up to a variable
          
          inputId = "bin",
          
          # Title for the slider
          
          label = "Page Bin Width",
          
          # Min value set
          
          min = 1,
          
          # Max value set
          
          max = 50,
          
          # Default value set
          
          value = 1,
          
          # Doesn't look good without this
          
          sep = "",
          
          # Step size
          
          step = 1,
          
          # I allow you to animate the graph
          
          animate = animationOptions(interval = 1200, loop = FALSE)
        )
        
      ),
      
      # This tab item provides the word freq in each section graphic
      
      tabItem(
        # Hooks up to menu item
        
        tabName = "word_freq",
        
        # Title and explaination
        
        h1("Highest Count Words in Range with Zipf's law Weight on y axis"),
        p(
          "In order to summarise the report in sections we use use a function
          apply's zipf's law to weight words by their relative occurence in
          the other chapers. This will allow us to get an idea about the topics
          in each chapter. You can read more about that",
          a(href = "https://en.wikipedia.org/wiki/Zipf%27s_law", "here"),
          "or",
          a(href = "https://www.tidytextmining.com/tfidf.html#zipfs-law", "here")
        ),
        
        # We output a table here, it has all the sections is dependent on slider
        
        plotOutput("table"),
        
        # We add a slider here
        
        sliderInput(
          # Hooks up to variable
          
          inputId = "wordbin",
          
          # Title
          
          label = "Page Denominator",
          
          # We set the min max and default value here
          
          min = 50,
          max = 125,
          value = 50,
          
          # We do this to make it look nice
          
          sep = "",
          
          # We set the step size to 25 because it divides well and facet_wrap looks good with it
          
          step = 25
        ),
        
        # We provide a title, and an idea of the most common words all througout the report
        
        p("Most Common Words in the Report"),
        
        # Data table output
        
        dataTableOutput("total_book")
      ),
      
      # In this tab item we look at bigrams
      
      tabItem(
        # We hook up to menu
        
        tabName = "bigram_bar",
        
        # Title and explaination
        
        h1("Most common pairs of words in each quarter of the Report"),
        p(
          "Zipf's law allows us to find the most important words relative to the other quarters.
          by looking the words with the highest counts in each section we can figure out which pairs
          of words are the most important both by count in the chapter and weighting"
        ),
        
        # Basic bigram graph
        
        plotOutput("bigram_bars"),
        
        # Title
        
        p("You can look at the most common bigram's here"),
        
        # We provide the data we use
        
        dataTableOutput("bigram_table")
        
      ),
      
      # This tab provides info about myself and the sources
      
      tabItem(
        # Hooks up to menu item
        
        tabName = "about",
        
        # Title
        
        h1("About", align = "center"),
        
        # Information about myself
        
        p(
          "  Hi, my name is Nicholas Dow. I am a member of the
          Harvard Class of 2022 concentrating in Statistics.
          I put together this project as a my final project for ",
          a(href = "https://www.davidkane.info/files/gov_1005.html", "Gov 1005"),
          ". This project was mostly written in R using shiny and shiny dashboards
          to create the interface and layout. You can find the source files for this project ",
          a(href = "https://github.com/NicholasDow/Mueller-Report", "here"),
          "and much of the data came from",
          a(href = "https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv", "here"),
          "(though more attention is given to the sources in the sources tab if you are interested).",
          align = "center"
        ),
        
        # Contact methods
        
        p("Email:", a("dow@college.harvard.edu")),
        p("Github:", a("https://github.com/NicholasDow")),
        p(
          "LinkedIn:",
          a("https://www.linkedin.com/in/nicholas-dow-1824b2174/")
        ),
        
        # These are my sources
        
        h1("Sources", align = "center"),
        p(
          "This is where I took the data from: https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv. Thank you gadenbuie",
          align = "center"
        ),
        p(
          "Most of what I did was from this text: https://www.tidytextmining.com",
          align = "center"
        ),
        p(
          "This is the actual report: https://www.justice.gov/storage/report.pdf",
          align = "center"
        ),
        p(
          "This was an article I read for info on the report: https://www.reuters.com/article/us-usa-trump-prosecutors-explainer/explainer-probes-spawned-by-mueller-target-trump-business-others-idUSKCN1S60UY",
          align = "center"
        )
      ),
      
      # In this tab I introduce information about the report, basically it is just a text summary
      
      tabItem(
        # Hooks up to menu item
        
        tabName = "the_report",
        
        # This is the text, for all purposes in R it is self explainatory
        
        h1("What is in the Report", align = "center"),
        p(
          "The Mueller Report is a 448 page document publically shared in a redacted form on March 22, 2019. It looks into Russian Interference in the 2016 Presidential Election. It is
          officially titled \"Report on the Investigation into Russian Interference in the 2016 Prsidential Election\". The Report was produced by a special council led by Robert Mueller,
          put together by Democratic Representatives in the US Congress after the firing of two Directors of the FBI investigating Russian interference with the 2016 election.",
          br(),
          br(),
          "Though
          offically the report found no collusion between Trump and Russia, it did find serious interference by Russia and in the process made 34 indictments",
          br(),
          br(),
          "
          The Report is split into two volumes. The first looks at Russian interference, and starting on page 208 the second volume looks at Potential Obstruction of Justice.",
          align = "center"
        ),
        h3("Volume 1"),
        p(
          "Volume 1 of the Mueller report found the Russian IRA or Internet Research Agency did interfere with the 2016 presidential Election. They had specifically
          paid and constructed fake materials that were circulated on social media to support the Donald Trump and Bernie Sanders campaign, in order to oppose
          Hillary Clinton's campaign. It also found the Involvement of the GRU, or Russian Intelligence Agency, in the hacking fo the DNC and the release of damaging
          material onto Wikileaks."
        ),
        h3("Volume 2"),
        p(
          "Volume 2 looks at specific cases where there was the potential to be obstructions of justice by Donald Trump involving Russia, obstruction including attempts
          withhold or manipulate the report. You can read more about it",
          a(href = "https://www.reuters.com/article/us-usa-trump-prosecutors-explainer/explainer-probes-spawned-by-mueller-target-trump-business-others-idUSKCN1S60UY", "here"),
          a(href = "https://www.nytimes.com/2019/04/20/us/politics/mueller-report-summary.html", "here")
        )
      ),
      
      # This tab lets you search through the text line by line
      
      tabItem(
        # Hooks up to menu item
        
        tabName = "search",
        
        # Title
        
        h1("Sources and pdfReader"),
        
        # Just text
        
        p(
          "The Mueller report is from the Department of Justice. They released a pdf with redacted information and explainations pasted on top of readactions.
          You can view the actual report ",
          a(href = "https://cdn.cnn.com/cnn/2019/images/04/18/mueller-report-searchable.pdf", "here"),
          br(),
          br(),
          "To make the report analyzable using textmining methods, the report was read in using a package called pdf reader.
          I had to manually removed lines that were miss read, pdfread misinterpreting crossed out lines. I specifically removed the second line of every page. pdfreader also missed some spaes",
          br(),
          br(),
          "Because of the redactions, it might also be confusing to see the words HOM, Harm to Ongoing Investigation, Personal Privacy, Grand Jury, and Investigative Technique in seemingly random places
          these were reasons for redaction. Looking at them in this table will not give you an idea of how much of the text was redacted. I suggest you look at the
          ",
          a(href = "https://cdn.cnn.com/cnn/2019/images/04/18/mueller-report-searchable.pdf", "report"),
          "if you are interested in the size of these redactions.
          this part of the tool is really meant to give you a quick way to reference or look up information in relation to some of the graphs I show elsewhere"
        ),
        
        # This lets us put sliders next to eachother
        
        splitLayout(
          # This slider lets you select the page number from 1 to 448 the range of the report
          
          sliderInput(
            inputId = "page_number",
            label = "Page Number",
            min = 1,
            max = 448,
            
            # We set this to a vector because we want this to a range
            # it will return a list
            
            value = c(1, 448),
            sep = ""
          ),
          
          # Here we return the line number
          
          sliderInput(
            inputId = "line_number",
            label = "Line number",
            
            # 1 line is the lowwest line number present
            
            min = 1,
            
            # 59 was the highest line number
            
            max = 59,
            
            # We set this to a vector because we want this to a range
            # it will return a list
            
            value = c(1, 59),
            sep = ""
          )
        ),
        
        # This returns the table that is being searched
        
        dataTableOutput("searchable_report")
      )
    )
  )
)

# We make a server to interpret all the info inputs, create output, and keep track of the current actions

server <- function(input, output, session) {
  # We create a reactive varible, dependent on input values that update
  
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
  
  # We do the exact same thing with the sum plot
  
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
      
      facet_wrap( ~ group_page, scales = "free_y") +
      
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
  
  # We render data we created before
  
  output$total_book <- renderDataTable(total_book)
}

# Run the application

shinyApp(ui = ui, server = server)
