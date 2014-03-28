library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Rare event detection"),
  
  sidebarPanel(
    #radio button or dropdown?
    
    selectInput(inputId = "algo",
                label = "Algorithm:",
                choices = c("Convex hull"      = "convex",
                            "DB"    = "db",
                            "BACON"           = "bacon",
                            "LOF"           = "lof")
                #,
                #selected = "convex"
                ),
    
    
    br(),
    
    uiOutput("convex_l"),
    uiOutput("eps"),
    uiOutput("ratio"),
   
    uiOutput("lof_k"),
    br(),
    uiOutput("threshold"),
    uiOutput("mansel"),
    br(),
    uiOutput("alpha"),
    uiOutput("size"),
    uiOutput("set"),
    
    br(),
    br(),
    br(),
    sliderInput("animation", "Step counter", 1, 10, 1, step = 1, 
                animate=animationOptions(interval=1000, loop=F))
    #uiOutput("animation")
    ),
  
  mainPanel(
    plotOutput("plot"),
    verbatimTextOutput("summary")
    ##div(textOutput("area"), align = "center", style="font-size:150%;")
  )
))