fluidPage(
  titlePanel('Hw 4 Shiny'),
  sidebarLayout(
    sidebarPanel(
      selectInput("query", "Choose a query:", 
                  choices = c("male", "female"), selected = "male"),
      
      checkboxInput("method1", "method1", value = T),
      checkboxInput("method2", "method2", value = F),
      checkboxInput("method3", "method3", value = F),
      checkboxInput("method4", "method4", value = F),
      checkboxInput("method5", "method5", value = F),
      checkboxInput("method6", "method6", value = F),
      checkboxInput("method7", "method7", value = F),
      checkboxInput("method8", "method8", value = F),
      checkboxInput("method9", "method9", value = F),
      checkboxInput("method10", "method10", value = F),
      
      # checkboxGroupInput('dataset', 'Columns in diamonds to show:',
      #                    choices = c("method1", "method2","method3",
      #                      "method4","method5","method6",
      #                      "method7","method8","method9","method10")
      #                    #selected = "method1"
      #                    #multiple = TRUE
      #             ),
      
      downloadButton('downloadData', 'Download')
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", plotOutput("plot")), 
                  # tabPanel("Summary", verbatimTextOutput("summary")), 
                  tabPanel("Table", tableOutput("table"))
                  # tabPanel("Table2", textOutput("table2")),
                  # tabPanel("Plot2", plotOutput("plot2"))
      )
    )
  )
)
