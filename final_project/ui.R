fluidPage(
  titlePanel('Final Project'),
  sidebarLayout(
    sidebarPanel(
      selectInput("content", "Method content:", 
                  choices = gsub(".csv", "", basename(dir("Data/methods/")))),
      
      selectInput("y", "Y axis:", 
                  choices = c("sensitivity", "specificity","F1","AUC"), 
                  selected = "sensitivity"),
      
      selectInput("x", "X axis:", 
                  choices = c("sensitivity", "specificity","F1","AUC"), 
                  selected = "AUC"),
      
      checkboxGroupInput('datasets', 'Show method:',
                         choices = gsub(".csv", "", basename(dir("Data/methods/")))
                         #selected = "method1"
                         #multiple = TRUE
                  )
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Method Content", tableOutput("table2")),
                  tabPanel("Method Effect", tableOutput("table")),
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Plotly", plotlyOutput("plot2"))
      )
    )
  )
)
