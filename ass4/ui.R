fluidPage(
  titlePanel('Hw 4 Shiny'),
  sidebarLayout(
    sidebarPanel(
      selectInput("content", "Method content:", 
                  choices = gsub(".csv", "", basename(dir("methods/")))),
      selectInput("query", "Choose a query:", 
                  choices = c("male", "female"), selected = "male"),
      
      # checkboxInput("method1", "method1", value = T),
      # checkboxInput("method2", "method2", value = F),
      # checkboxInput("method3", "method3", value = F),
      # checkboxInput("method4", "method4", value = F),
      # checkboxInput("method5", "method5", value = F),
      # checkboxInput("method6", "method6", value = F),
      # checkboxInput("method7", "method7", value = F),
      # checkboxInput("method8", "method8", value = F),
      # checkboxInput("method9", "method9", value = F),
      # checkboxInput("method10", "method10", value = F),
      
      selectInput("y", "Y axis:", 
                  choices = c("sensitivity", "specificity","F1","AUC"), 
                  selected = "sensitivity"),
      
      selectInput("x", "X axis:", 
                  choices = c("sensitivity", "specificity","F1","AUC"), 
                  selected = "specificity"),
      
      
      
      checkboxGroupInput('datasets', 'Show method:',
                         choices = gsub(".csv", "", basename(dir("methods/")))
                         #selected = "method1"
                         #multiple = TRUE
                  )
      
      
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Method Content", tableOutput("table2")),
                  #tabPanel("Plot", textOutput("plot")), 
                  # tabPanel("Summary", verbatimTextOutput("summary")), 
                  
                  tabPanel("Method Effect", tableOutput("table")),
                  tabPanel("Plot", plotOutput("plot2"))
      )
    )
  )
)
