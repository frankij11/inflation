#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(lubridate)
source("global.R")
source("pages/multi_query.R")

# Define UI for application that draws a histogram


ui <- navbarPage("Joint Inflation Calculator Example",id="nav",collapsible = T,
    multi_query_ui()
)


server <- function(input, output, session) {
    multi_query_serv(input, output, session)
    
}


shinyApp(ui=ui, server = server)

#    h4(HTML('The PB20 NCCA Inflation Indices and <a href="https://www.ncca.navy.mil/tools/inflation.cfm">Joint Inflation Calculator (JIC)</a>  is for both Army and Navy users')),
