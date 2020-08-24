library(shiny)

#shinyUI(fluidPage(
#  
#  titlePanel("Slovenske občine"),
#  
#  tabsetPanel(
#      tabPanel("Velikost družine",
#               DT::dataTableOutput("druzine")),
#      
#      tabPanel("Število naselij",
#               sidebarPanel(
#                  uiOutput("pokrajine")
#                ),
#               mainPanel(plotOutput("naselja")))
#    )
#))

#shinyUI(fluidPage(
#  titlePanel("Izpusti"), 
#  DT::dataTableOutput("regije")))
 
fluidPage(
  titlePanel('Izpusti'),
  sidebarLayout(
    sidebarPanel(
      selectInput('izbrana', 'Izberi regijo', choices = regije$regija)
  ),
  
  mainPanel(
    DT::dataTableOutput('regijedata')
    )
  )
)
