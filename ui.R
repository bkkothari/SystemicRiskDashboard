library(plotly)
shinyUI(fluidPage(
  
  titlePanel("Systemic Risk Scoring"),
  
  sidebarLayout(
    
    sidebarPanel(
      # Inputs excluded for brevity
      p('Upload a .csv file having header as Credit Scores and names of n banks. Dimensions of file will be
        (n*n+1) excluding the header.'),
      fileInput("file", label = h3("File input")),
      actionButton("compute","Compute Scores"),
      hr(),
      textOutput("text1"),
      textOutput("text2"),
      hr(),
      p('Please refer following Paper published for further details',
        a("Matrix Metrics: Network-Based Systemic Risk Scoring.", 
          href = "http://algo.scu.edu/~sanjivdas/jai_das_issue.pdf"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Network Graph", plotOutput("plot",width="100%")), 
        tabPanel("Risk Decomposition", plotlyOutput("plot2")), 
        tabPanel("Node Centrality", plotlyOutput("plot3")), 
        tabPanel("Risk Increments", plotlyOutput("plot4")),
        tabPanel("Criticality", plotlyOutput("plot5"))

      )
    )
  )
))