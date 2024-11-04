library(shiny)
library(shinyalert)
library(tidyverse)
library(DT)

#source("helpers.R")

# load in the data
data = read.csv("user_behavior_dataset.csv")
colnames(data) = c("id","device","os","apptime",
                   "screentime","battery","noapps",
                   "datause","age","gender","class")
vars = c("","noapps","apptime") 
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Select Data Subset!"),
  sidebarLayout(
    sidebarPanel(
      h2("Choose a subset of the data:"),
      radioButtons("gender",
                   "Gender",
                   choiceValues = c("All", 
                                    "Male",
                                    "Female"
                   ),
                   choiceNames = c("All",
                                   "Male",
                                   "Female"
                   )
      ),
      radioButtons("os",
                   "Operating System",
                   choiceValues = c("All", 
                                    "Android",
                                    "iOS"
                   ),
                   choiceNames = c("All",
                                   "Android",
                                   "iOS"
                   )
      ),
    h2("Select Variable to View Details/Plots:"),
    selectizeInput("var1", 
                   "",
                   choices = vars, 
                   selected = "",
                   ),
    conditionalPanel(condition = "input.var1 !== ''", 
      sliderInput("min", "Lower Bound of Value:",
                min = 0, max = 100, value = 5),
      sliderInput("max", "Upper Bound of Value:",
                min = 0, max = 100, value = 10),
    ),
      # Action button!
    actionButton("button", "Confirm Selections Here!"),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("About",
                 textOutput("about")
                 ),
        tabPanel("Data Download",
                 DT::dataTableOutput("table")),
        tabPanel("Data Exploration",
                 verbatimTextOutput("TBC")),
    )
   )
  )
) 

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observe({print(input$os) # for debuggin purpose
    print(input$gender)
    print(input$button)
    print(input$var1)
  })

  ##################################################
  # set up hardcoded about text
  output$about = renderText({
    "this is a test"
    })

    
    #update input boxes & slider automatically
  observe({
    var1 = input$var1
    choices = vars
    
    updateSliderInput(session, "var1", max = input$max)
    })
   
  # Now we add in the actionButton to subset the data when appropriate
  output$table = DT::renderDataTable(DT::datatable({
    data_sub = data
    if (input$os != "All") {
      data_sub |>
        filter(data_sub$os == input$os)
      #data_sub = data[data$os == input$os,]
    }
    if (input$gender != "All") {
      data_sub |>
        filter(data_sub$gender == input$gender)
      #data_sub = data[data$gender == input$gender,]
    }
    data_sub
  }))
} 
  
# Run the application 
shinyApp(ui = ui, server = server)
