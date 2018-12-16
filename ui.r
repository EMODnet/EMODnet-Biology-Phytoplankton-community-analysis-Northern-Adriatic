library(shiny)
shinyUI(fluidPage(
  titlePanel("LTER North Adriatic plankton series"),
  fluidRow(
    column(4,
           wellPanel(
             selectInput(inputId = "set1", label = "Currently restricted to Phytoplankton",
                         choices = c("Phytoplankton"), #,"Microzooplankton","Mesozooplankton"),
                         selected = "Phytoplankton"),
             selectInput(inputId = "spec.set1", label = "Choose a species (group)",
                         choices="",selected = "Bacillariophyceae"),
             br(),
             # br(),
             # selectInput(inputId = "set2", label = "Choose a set for middle graph",
             #             choices = c("Phytoplankton","Microzooplankton","Mesozooplankton"),
             #             selected = "Phytoplankton"),
             # selectInput(inputId = "spec.set2", label = "Choose a species (group)",
             #             choices="",selected = "Bacillariophyceae"),
             # br(),
             # br(),
             # selectInput(inputId = "set3", label = "Choose a set for lower graph",
             #             choices = c("Phytoplankton","Microzooplankton","Mesozooplankton"),
             #             selected = "Phytoplankton"),
             # selectInput(inputId = "spec.set3", label = "Choose a species (group)",
             #             choices="",selected = "Bacillariophyceae"),
             # br(),
             # br(),
             
             checkboxInput(inputId = "transform",
                           label = strong("value double sqrt transformed"),
                           value = TRUE) 
             )
    ),
    column(8,
           tabsetPanel(
             tabPanel("Observations", plotOutput("plot1", height ="375px", width = "100%"),
                                plotOutput("plot4", height ="375px", width = "100%")),
             tabPanel("Multiv 1",plotOutput("plM1", height ="700px", width = "100%")),
             
             # tabPanel("Yearly", plotOutput("plot1", height ="275px", width = "600px"),
             #          plotOutput("plot2", height ="275px", width = "600px"),
             #          plotOutput("plot3", height ="275px", width = "600px")), 
             # tabPanel("Seasonal", plotOutput("plot4", height ="275px", width = "600px"),
             #          plotOutput("plot5", height ="275px", width = "600px"),
             #          plotOutput("plot6", height ="275px", width = "600px")),
             # tabPanel("Multiv 1",plotOutput("plM1", height ="600px", width = "600px")),
             # tabPanel("Multiv 2",plotOutput("plM2", height ="600px", width = "600px")),
             # tabPanel("Multiv 3",plotOutput("plM3", height ="600px", width = "600px")),
             tabPanel("about",htmlOutput("about"))
             
           )
           
    )
  )
))

