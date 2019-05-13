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

# Shinythemes for a better looking app 

library(shinythemes)

# We bring in the ui

source('ui.R')

# We bring in the server

source('server.R')

# Run the application

shinyApp(ui = ui, server = server)
