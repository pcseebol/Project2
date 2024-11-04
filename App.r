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
                 verbatimTextOutput("about")
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
  
  #observe({print(input$button[1]) # for debuggin purpose
   # print(input$gender)
 # })

  ##################################################
  # set up hardcoded about text
  output$about = renderText({
    "Welcome to my app! Here you can explore and visualize some basic variables
    relating to phone use from a sample data set. The data and more information
    about the data for this app are available here:
    https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset
    
    The Data Download tab lets you to see the data table.
    The Data Plotting lets you to see plots and numerical summaries of your variable.
    
    In the side bar, the top two multi-choice section will let you subset the 
    data by gender and operating system. The drop-down menu below this will let
    you choose which numerical variable you would like to see. The dynamic slider
    (which will appear once you pick your variable!) will let you further subset
    your data.
    
    Thanks for visiting, and have fun! :D"
    })
  
  # Now we add in the actionButton to subset the data when appropriate
  observeEvent(input$button, {
    var1 = input$var1
    choices = vars
    updateSliderInput(session, "var1", max = input$max)
    output$table = DT::renderDataTable(
      DT::datatable({
        data_sub = data
        if (input$os != "All") {
          data_sub = data_sub |>
            filter(data_sub$os == input$os)
        }
        if (input$gender != "All") {
          data_sub =data_sub |>
            filter(data_sub$gender == input$gender)
        }
     data_sub
    }))
  })
}
  
# Run the application 
shinyApp(ui = ui, server = server)
