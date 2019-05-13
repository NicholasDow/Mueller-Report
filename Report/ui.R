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
        text = "Wikipedia Webscraping",
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
    
    # Here we add a style sheet to our webpage
    # This is our head tag 
    
    tags$head(
      
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
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
          whatever the report is about?",
          br(),
          "You could assign commonly used words a score
          for their negativeness and positiveness and measure the values through the paper.
          using the afinn lexicon from tidytext we do just that to find that the overall report
          is negative in nature. Drag the page bin size to right to get a more overall idea of sentiment"
        ),
        # We add a slider input for the bin width
        column(
          4,
          
          offset = 4,
          
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
        
        # This creaties the three plots into tabs
        
        tabsetPanel(
          position = "right",
          tabPanel("Total Sentiment Overtime", plotlyOutput("plot")),
          tabPanel("Mean Sentiment Overtime", plotlyOutput("plot_mean")),
          tabPanel("Cumulative Sum Overtime", plotlyOutput("plot_cumsum"))
        ),
        br()
        
        ),
      
      # This tab item provides the word freq in each section graphic
      
      tabItem(
        
        # Hooks up to menu item
        
        tabName = "word_freq",
        
        # Title and explaination
        
        h1(
          "Highest Count Words in a Range with Zipf's law Weight on y axis",
          align = "center"
        ),
        p(
          "In order to summarise the report in sections, we use a function to apply zipf's law to weight words by their relative occurrence in the other chapters. This will allow us to get an idea about the topics in each chapter. You can read more about that",
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
        
        h1("Most Common Words in the Report", align = "center"),
        
        # Data table output
        
        dataTableOutput("total_book")
      ),
      
      # In this tab item we look at bigrams
      
      tabItem(
        
        # We hook up to menu
        
        tabName = "bigram_bar",
        
        # Title and explaination
        
        h1("Most Common Pairs of Words in Each Quarter of the Report", align = "center"),
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
          "Hi, my name is Nicholas Dow. I am a member of the Harvard Class of 2022 concentrating in Statistics. This project was mostly written in R using shiny and shiny dashboards to create the interface and layout. You can find the source files for this project ",
          a(href = "https://github.com/NicholasDow/Mueller-Report", "here"),
          " and much of the data came from ",
          a(href = "https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv", "here")
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
          "This is where I took the data from: https://raw.githubusercontent.com/gadenbuie/mueller-report/master/mueller_report.csv. Thank you gadenbuie"
        ),
        p(
          "Most of what I did was from this text: https://www.tidytextmining.com"
        ),
        p(
          "This is the actual report: https://www.justice.gov/storage/report.pdf"
        ),
        p(
          "This was an article I read for info on the report: https://www.reuters.com/article/us-usa-trump-prosecutors-explainer/explainer-probes-spawned-by-mueller-target-trump-business-others-idUSKCN1S60UY"
        )
      ),
      
      # In this tab I show you the first entry of every link on the mueller report wikipedia page for context in the other data.
      
      tabItem(
        
        # we hook the tab up
        
        tabName = "Web_scraping",
        
        # Add a title
        
        h1("Mueller Report Wikipedia Page Webscraping", align = "center"),
        
        # Explain our junk code
        
        p(
          "To provide context, I webscraped the mueller report wikipedia page and all associated page to provide a list of definitions.",
          a("https://en.wikipedia.org/wiki/Mueller_Report")
        ),
        
        # Output our html summary text from the server
        
        htmlOutput("summary")
      ),
      
      # In this tab I introduce information about the report, basically it is just a text summary
      
      tabItem(
        
        # Hooks up to menu item
        
        tabName = "the_report",
        
        # This is the text, for all purposes in R it is self explainatory
        
        h1("What is in the Report", align = "center"),
        p(
          "The Mueller Report, offically titled \"Report on the Investigation into Russian Interference in the 2016 Presidential Election\", is a 448-page document publically shared in a redacted form on March 22, 2019. It looks into Russian Interference in the 2016 Presidential Election. The Report was produced with Robert Mueller as special council, and authorized by the US Congress after the Director of the FBI investigating Russian interference with the 2016 election was fired. ",
          br(),
          br(),
          "Though officially the report found no collusion between Trump and Russia, it did find serious interference by Russia and in the process made 34 indictments ",
          br(),
          br(),
          "The Report is split into two volumes. The first looks at Russian interference, and starting on page 208 the second volume looks at potential obstruction of justice."
        ),
        h3("Volume 1"),
        p(
          "Volume 1 of the Mueller report found the Russian IRA or Internet Research Agency did interfere with the 2016 Presidential Election. They had specifically paid and constructed fake materials that were circulated on social media to support the Donald Trump and Bernie Sanders campaigns for the purpose of weakening the Hillary Clinton campaign. It also found the Involvement of the GRU, or Russian Intelligence Agency, in the hacking of the DNC and the release of damaging material onto Wikileaks."
        ),
        h3("Volume 2"),
        p(
          "Volume 2 looks at specific cases where there was the potential to be obstructions of justice by Donald Trump involving Russia, obstruction including attempts withhold or manipulate the report. You can read more about it",
          a(href = "https://www.reuters.com/article/us-usa-trump-prosecutors-explainer/explainer-probes-spawned-by-mueller-target-trump-business-others-idUSKCN1S60UY", "here"),
          " and ",
          a(href = "https://www.nytimes.com/2019/04/20/us/politics/mueller-report-summary.html", "here")
        )
      ),
      
      # This tab lets you search through the text line by line
      
      tabItem(
        
        # Hooks up to menu item
        
        tabName = "search",
        
        # Title
        
        h1("Sources and pdfReader", align = "center"),
        
        # Just text
        
        p(
          "The Mueller report is a document from the Department of Justice. They chose not to release the full report and instead released a pdf with heavy redactions. You can view the actual report ",
          a(href = "https://cdn.cnn.com/cnn/2019/images/04/18/mueller-report-searchable.pdf", "here"),
          br(),
          br(),
          "To make the report analyzable using text-mining methods, the report was read in using a package called pdf reader. Though pdfreader is great when the text is very clean, it does not handle crossed-out lines or other special characters very well. I had to manually removed lines that were misread. I specifically removed the second line of every page because of this.",
          br(),
          br(),
          "Because of the redactions, you might also see the words HOM, Harm to Ongoing Investigation, Personal Privacy, Grand Jury, and Investigative Technique in seemingly random places. These were the reasons for redaction. Looking at them in this table will not give you an idea of how much of the text was redacted. I suggest you look at the ",
          a(href = "https://cdn.cnn.com/cnn/2019/images/04/18/mueller-report-searchable.pdf", "report"),
          "if you are interested in the size of these redactions. This part of the tool is really meant to give you a quick way to reference or look up information in relation to some of the graphs I show elsewhere."
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