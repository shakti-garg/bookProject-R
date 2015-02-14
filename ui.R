library(shiny)

shinyUI(
  pageWithSidebar(
    headerPanel("Meluha Synopsis: Har Har Mahadev!!", windowTitle = "Meluha Synopsis!"),
    
    sidebarPanel(
      helpText(div(HTML("<u>Filter Panel:</u>"))),
      uiOutput("bookFilter"),
      
      br(),
      
      uiOutput("chapterFilter"),
      
      br(),
      
      helpText(div(HTML("<u>Information:</u>"))),
      conditionalPanel(condition = "input.tabName == 'Character Appearances'",
                       helpText(div(HTML("This visualization shows mentions of the main characters throughout the book. It was inspired by Jeff Clark's <a href=\"http://neoformix.com/2013/NovelViews.html\" target=\"_blank\">Novel Views</a>.")))),
      conditionalPanel(condition = "input.tabName == 'Character Sentiments'",
                       helpText(div(HTML("This visualization shows sentiments of the main characters throughout the book. It was inspired by Jeff Clark's <a href=\"http://neoformix.com/2013/NovelViews.html\" target=\"_blank\">Novel Views</a>.")))),
      conditionalPanel(condition = "input.tabName == 'Character Co-Occurrences'",
                       helpText(div(HTML("This visualization shows when the main characters are mentioned in the same chapter together. It was inspired by Mike Bostock's <a href=\"http://bost.ocks.org/mike/miserables/\" target=\"_blank\">Co-occurrence</a>."))))
    ),
      
    mainPanel(
        tabsetPanel(id="tabName", type = "tabs", 
          tabPanel("Character Appearances", plotOutput("appearancePlot", height="100%")), 
          tabPanel("Character Sentiments", plotOutput("sentimentPlot", height="100%")),
          tabPanel("Character Co-Occurrences", plotOutput("cooccurrencePlot", height="100%"))
      )
    )
  )
)