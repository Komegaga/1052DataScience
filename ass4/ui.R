fluidPage(
  titlePanel('Downloading Data'),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose a dataset:", 
                  choices = c("method1", "method2","method3",
                              "method4","method5","method6",
                              "method7","method8","method9","method10")),
      
      selectInput("query", "Choose a query:", 
                  choices = c("male", "female")),
      
      downloadButton('downloadData', 'Download')
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", plotOutput("plot")), 
                  tabPanel("Summary", verbatimTextOutput("summary")), 
                  tabPanel("Table", tableOutput("table"))
      )
    )
  )
)
