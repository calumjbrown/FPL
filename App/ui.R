ui <- fluidPage(
  titlePanel("FPL Insights"),
  
  # Navigation bar implemented as tabs
  navbarPage(
    "Navigation",
    
    # Tab 1
    tabPanel("Tab 1",
             h3("Welcome to Tab 1"),
             p("This is the content of Tab 1.")
    ),
    
    # Tab 2
    tabPanel("Data Tables",
             DTOutput('DT_leagueTable')
    )
  )
)