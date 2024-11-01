library(shiny)
library(shinyalert)
library(tidyverse)

source("helpers.R")

# load in the data
data = read.csv("user_behavior_dataset.csv")
colnames(data) = c("id","device","os","apptime",
                   "screentime","battery","noapps",
                   "datause","age","gender","class")

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
                                    "Female",
                   ),
                   choiceNames = c("All",
                                   "Male",
                                   "Female"
                   )
      ),
      radioButtons("os",
                   "Operating System",
                   choiceValues = c("all", 
                                    "Android",
                                    "iOS"
                   ),
                   choiceNames = c("All",
                                   "Android",
                                   "iOS"
                   )
      ),
    #  h2("Select a Numeric Variable"),
    #  sliderInput("corr_n", "", min = 20, max = 500, value = 20),
    #  actionButton("corr_sample","Get a Sample!")
    #  conditionalPanel(
    #    condition = "input.choice == 'show'",
    #    sliderInput("slider","Select Subset:",
    #                 min)
    #  )
    #  ),
    h2("Select Variables to Find Correlation:"),
    selectizeInput("corr_x",
                   "x Variable",
                   choices = numeric_vars[-1], 
                   selected = numeric_vars[2]),
    selectizeInput("corr_y",
                   "y Variable",
                   choices = numeric_vars[-2],
                   selected = numeric_vars[1]),
    
    mainPanel(
      plotOutput("corr_scatter"),
      conditionalPanel("input.corr_sample",
                       h2("Guess the correlation!"),
                       column(6, 
                              numericInput("corr_guess",
                                           "",
                                           value = 0,
                                           min = -1, 
                                           max = 1
                              )
                       ),
                       column(6, 
                              actionButton("corr_submit", "Check Your Guess!"))
      )
    )
  )
)

my_sample = data

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  ##################################################
  ##Correlation tab
  sample_corr <- reactiveValues(corr_data = NULL, corr_truth = NULL)
  
  #update input boxes so they can't choose the same variable
  observeEvent(c(input$corr_x, input$corr_y), {
    corr_x <- input$corr_x
    corr_y <- input$corr_y
    choices <- numeric_vars
    if (corr_x == corr_y){
      choices <- choices[-which(choices == corr_x)]
      updateSelectizeInput(session,
                           "corr_y",
                           choices = choices)
    }
    
    #Note: this was shinyalert wasn't asked for but is needed
    #for rent vs property taxes/value
    if (((input$corr_x == "GRPIP") & (input$corr_y %in% c("TAXAMT", "VALP"))) | ((input$corr_y == "GRPIP") & (input$corr_x %in% c("TAXAMT", "VALP")))){
      shinyalert(title = "Oh no!", "Those with Property taxes and/or Property Values usually don't have a rent payment. Please select a different combination of variables.", type = "error")
      updateSelectizeInput(session,
                           "corr_x",
                           choices = choices[-2],
                           selected = choices[1]
      )
      updateSelectizeInput(session,
                           "corr_y",
                           choices = choices[-1],
                           selected = choices[2]
      )
    }
  })
  
  #make sure two variables are selected
  observeEvent(input$corr_sample, {
    
    if(input$hhl_corr == "all"){
      hhl_sub <- HHLvals
    } else if(input$hhl_corr == "english"){
      hhl_sub <- HHLvals["1"]
    } else if(input$hhl_corr == "spanish"){
      hhl_sub <- HHLvals["2"]
    } else {
      hhl_sub <- HHLvals[c("0", "3", "4", "5")]
    }
    
    if(input$fs_corr == "all"){
      fs_sub <- FSvals
    } else if(input$fs_corr == "yes"){
      fs_sub <- FSvals["1"]
    } else {
      fs_sub <- FSvals["2"]
    }
    
    if(input$schl_corr == "all"){
      schl_sub <- SCHLvals
    } else if(input$schl_corr == "no_hs"){
      schl_sub <- SCHLvals[c("0", "01", "02", "03", "04", 
                             "05", "06", "07", "08", "09",
                             "10", "11", "12", "13", "14", "15")]
    } else if(input$schl_corr == "hs"){
      schl_sub <- SCHLvals[as.character(16:19)]
    } else {
      schl_sub <- SCHLvals[as.character(20:24)]
    }
    
    corr_vars <- c(input$corr_x, input$corr_y)
    
    subsetted_data <- my_sample |>
      filter(#cat vars first
        HHLfac %in% hhl_sub,
        FSfac %in% fs_sub,
        SCHLfac %in% schl_sub
      ) %>% #make sure numeric variables are in appropriate range, must use %>% here for {} to work
      {if("WKHP" %in% corr_vars) filter(., WKHP > 0) else .} %>%
      {if("VALP" %in% corr_vars) filter(., !is.na(VALP)) else .} %>%
      {if("TAXAMT" %in% corr_vars) filter(., !is.na(TAXAMT)) else .} %>%
      {if("GRPIP" %in% corr_vars) filter(., GRPIP > 0) else .} %>%
      {if("GASP" %in% corr_vars) filter(., GASP > 0) else .} %>%
      {if("ELEP" %in% corr_vars) filter(., ELEP > 0) else .} %>%
      {if("WATP" %in% corr_vars) filter(., WATP > 0) else .} %>%
      {if("PINCP" %in% corr_vars) filter(., AGEP > 18) else .} %>%
      {if("JWMNP" %in% corr_vars) filter(., !is.na(JWMNP)) else .}
    
    index <- sample(1:nrow(subsetted_data),
                    size = input$corr_n,
                    replace = TRUE,
                    prob = subsetted_data$PWGTP/sum(subsetted_data$PWGTP))
    sample_corr$corr_data <- subsetted_data[index, ]
    sample_corr$corr_truth <- cor(sample_corr$corr_data |>
                                    select(corr_vars))[1,2]
  })
  
  
  
  #Code for rendering the regression plot. It changes whether a line is requested
  #or not
  output$corr_scatter <- renderPlot({
    validate(
      need(!is.null(sample_corr$corr_data), "Please select your variables, subset, and click the 'Get a Sample!' button.")
    )
    ggplot(sample_corr$corr_data, aes_string(x = isolate(input$corr_x), y = isolate(input$corr_y))) +
      geom_point()
  })
  
  
  
  #Code for the correlation guessing game
  observeEvent(input$corr_submit, {
    close <- abs(input$corr_guess - sample_corr$corr_truth) <= .05
    if(close){
      shinyalert(title = "Nicely done!",
                 paste0("The sample correlation is ",
                        round(sample_corr$corr_truth, 4),
                        "."),
                 type = "success"
      )
    } else {
      if(input$corr_guess > sample_corr$corr_truth){
        shinyalert(title = "Try again!",
                   "Try guessing a lower value.")
      } else {
        shinyalert(title = "Try again!",
                   "Try guessing a higher value.")
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
