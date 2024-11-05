library(shiny)
library(shinyalert)
library(tidyverse)
library(DT)

# This Rshiny app will let users interact dynamically with phone user behavior data!

# load in the data
data = read.csv("user_behavior_dataset.csv")
colnames(data) = c("id","device","os","apptime",
                   "screentime","battery","numapps",
                   "datause","age","gender","class")
vars = c("","numapps","apptime","screentime","battery") 


# Define the UI
ui <- fluidPage(
  titlePanel("Select Data Subset!"),
  # we'll use sidebarLayout to keep things simple for our sidebar
  sidebarLayout(
    sidebarPanel(
      # Set up the subset for gender 
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
      # And the subset for operating system
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
      
    # Now let our users select between the numerical variables of interest
    h2("Select X Variable Here:"),
    selectizeInput("var1", 
                   "",
                   choices = vars, 
                   selected = "",
                   ),
    # For all four variable we'll allow further subsetting
    conditionalPanel(condition = "input.var1 == 'numapps'", 
        sliderInput("appnum", "Maximum Number of Apps:",
                min = 10, max = 99, value = 99)
        ),
    conditionalPanel(condition = "input.var1 == 'apptime'", 
        sliderInput("apptime", "Maximum Minutes on Apps per day:",
                min = 30, max = 600, value = 600),
      ),
    conditionalPanel(condition = "input.var1 == 'battery'", 
                     sliderInput("battery", "Maximum Battery Usage per Day (mAh):",
                                 min = 302, max = 2993, value = 2993)
    ),
    conditionalPanel(condition = "input.var1 == 'screentime'", 
                     sliderInput("screentime", "Maximum Hours on Screen per Day:",
                                 min = 1, max = 12, value = 12),
    ),
    
      # Action button to generate the data subset
    actionButton("button", "Confirm Selections Here!"),
    ),
    
    # Set up the three tabs - About, Data Download, Data Exploration
    mainPanel(
      tabsetPanel(
        tabPanel("About",
                 verbatimTextOutput("about"),
                 img(src = "phone.jpg")
                 ),
        tabPanel("Data Download",
                 DT::dataTableOutput("table"),
                 downloadLink('downloadData', 'Download Full Data')),
        tabPanel("Data Exploration",
                 plotOutput("scatter"),
                 plotOutput("heat"),
                 DT::dataTableOutput("num"),
                      conditionalPanel("input.button",
                      h2("Select Y"),
                      selectInput("var2",
                                   "",
                                   choices = vars, 
                                   selected = ""
                                   ),
                      radioButtons("type",
                                 "Select Summary Type",
                                 choiceValues = c("num", 
                                                  "scatter",
                                                  "heat"
                                 ),
                                 choiceNames = c("Numerical Summaries",
                                                 "Scatterplot",
                                                 "Heat map"
                                 )
                      
                    ),
                    actionButton("button2", "Confirm Y Variable"),
                 ),
    )
   )
  )
  ) 
)
# Now we define our server logic to get everything running smoothly!
server <- function(input, output, session) {
  
  #observe({print(input$appnum)
  #  print(input$var1) # for debugging purpose
  #})

  ##################################################
  # set up hard-coded text for the 'About' Tab - don't need to set up the image on server side
  output$about = renderText({
    "Welcome to my app! Here you can explore and visualize some basic variables
    relating to phone use from a sample data set. The data and more information
    about the data for this app are available here:
    https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset
    
    The Data Download tab lets you to see the data table. 
    The Data Plotting lets you to see plots and numerical summaries of your variable.
    
    NOTE!! You'll need to generate your data with the button on the side panel
    before using the Data Download or Data Plotting tabs.
    
    In the side bar, the top two multi-choice section will let you subset the 
    data by gender and operating system. The drop-down menu below this will let
    you choose which numerical variable you would like to see. The dynamic slider
    (which will appear once you pick your variable!) will let you further subset
    your data.
    
    Thanks for visiting, and have fun! :D"
    })
  
  # set up data download - we only allow users to download the full dataset
  output$downloadData <- downloadHandler(
       filename = function() {
         paste('data-', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         write.csv(data, con)
       }
  )
  
  # create a reactive Value for holding the subsetted data 
  subset_data = reactiveValues(data = NULL)
  
  # Set up the action-button execution of subsetting the data
  observeEvent(input$button, {
    output$table = DT::renderDataTable(
      DT::datatable({
        data_sub = data
        if (input$os != "All") {
          data_sub = data_sub |>
            filter(data_sub$os == input$os)
        }
        if (input$gender != "All") {
          data_sub = data_sub |>
            filter(data_sub$gender == input$gender)
        }
        if (input$var1 == "numapps"){
          data_sub = data_sub |>
            filter(data_sub$numapps <= input$appnum)
        }
        if (input$var1 == "apptime"){
          data_sub = data_sub |>
            filter(data_sub$apptime <= input$apptime)
        }
        if (input$var1 == "screentime"){
          data_sub = data_sub |>
            filter(data_sub$screentime <= input$screentime)
        }
        if (input$var1 == "battery"){
          data_sub = data_sub |>
            filter(data_sub$battery <= input$battery)
        }
    subset_data$data = data_sub
    data_sub
     
    }))
  })
  
  observeEvent(input$button2, {
    if (input$var1 == input$var2){ # Throw a warning for if both variables match
      shinyalert(title = "Oh no!", "Please make sure to select a different variable than your X variable!", type = "error")
    }
    if (input$type == "scatter"){
      output$scatter = renderPlot({
        validate(
          need(!is.null(subset_data$data), "Please select your variables, subset, and click the buttons.")
        )
        ggplot(subset_data$data, aes_string(x = isolate(input$var1), y = isolate(input$var2)))+
          geom_point() +
          labs(x = input$var1, y = input$var2, title = paste(input$var1, " by ", input$var2)) +
          theme_minimal()
      })}
    if (input$type == "heat"){
      output$heat = renderPlot({
        validate(
          need(!is.null(subset_data$data), "Please select your variables, subset, and click the buttons.")
        )
        ggplot(subset_data$data, aes_string(x = isolate(input$var1), y = isolate(input$var2)))+
          geom_tile() +
          theme_minimal()
      })}
    if (input$type == "num"){
      output$num = renderTable({
        validate(
          need(!is.null(subset_data$data), "Please select your variables, subset, and click the buttons.")
        )
        summ = subset_data$data |>
          summarize( mean_apps = mean(numapps, na.rm = TRUE),
                     sd_apps = sd(numapps, na.rm = TRUE),
                     mean_screentime = mean(screentime, na.rm = TRUE),
                     sd_screentime = sd(screentime, na.rm = TRUE),
                     mean_apptime = mean(apptime, na.rm = TRUE),
                     sd_apptime = sd(apptime, na.rm = TRUE),
                     mean_battery = mean(battery, na.rm = TRUE),
                     sd_battery = sd(battery, na.rm = TRUE))
      })}
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
