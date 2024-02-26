#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(randomForest)
load("rfModel_2Tests.Rdata")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Choledocholithiasis risk prediction model"),
  
   hr(),
   
   fluidRow(
     column(3, 
            numericInput("age", label = h5("Age:"), value = NA, min = 0, max = 100, step = 1)
     ),
     column(3,
            selectInput("gender", h5("Gender:"),
                        c("Male" = 1,"Female" = 2,"Unknown" = 1000))
     ),
     column(3, 
            selectInput("usbds", h5("US BDS:"),
                        c("Yes" = 1,"No" = 0,"Unknown" = 1000))
     ), 
     column(3,
            selectInput("uscbd6", h5("US CBD > 6mm:"),
                        c("Yes" = 1,"No" = 2,"Unknown" = 1000))
     )
   ),
   hr(),
   
   fluidRow(
     column(3, 
            numericInput("tbili1", label = h5("First TBili:"), value = NA)
     ), 
     column(3,
            numericInput("tbili2", label = h5("Second TBili:"), value = NA)
     ),
     column(3, 
            numericInput("AST1", label = h5("First AST:"), value = NA)
     ),
     column(3,
            numericInput("AST2", label = h5("Second AST:"), value = NA)
     )
   ),
   hr(),
   
   fluidRow(
     column(3, 
            numericInput("ALT1", label = h5("First ALT:"), value = NA)
     ), 
     column(3,
            numericInput("ALT2", label = h5("Second ALT:"), value = NA)
     ),
     column(3, 
            numericInput("ALP1", label = h5("First ALP:"), value = NA)
     ),
     column(3,
            numericInput("ALP2", label = h5("Second ALP:"), value = NA)
     )
   ),
   hr(),
   
   h3(textOutput("risk"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
##### Names
## [1] "Age"        "Gender_M.1" "US_BDS"     "US_CBD.6mm" "TBili1"     "TBili2"     "TBiliDrop." "AST1"       "AST2"      
## [10] "AST_drop."  "ALT1"       "ALT2"       "ALT_drop."  "ALP1"       "ALP2"       "ALP_drop." 
#####
  risk = reactive({
    patient=as.data.frame(matrix(NA, 1, nrow(rfModel$importance)))
    colnames(patient) = rownames(rfModel$importance)
    
    patient["Age"] = input$age; patient["Gender_M.1"] = input$gender
    patient["US_BDS"] = input$usbds; patient["US_CBD.6mm"] = input$uscbd6
    
    patient["TBili1"] = input$tbili1; patient["TBili2"] = input$tbili2
    patient["TBiliDrop."] = patient["TBili2"] / patient["TBili1"] - 1
    
    patient["AST1"] = input$AST1; patient["AST2"] = input$AST2
    patient["AST_drop."] = patient["AST2"] / patient["AST1"] - 1
    
    patient["ALT1"] = input$ALT1; patient["ALT2"] = input$ALT2
    patient["ALT_drop."] = patient["ALT2"] / patient["ALT1"] - 1
    
    patient["ALP1"] = input$ALP1; patient["ALP2"] = input$ALP2
    patient["ALP_drop."] = patient["ALP2"] / patient["ALP1"] - 1
    
    patient[is.na(patient)] = 1000
    c(predict(rfModel, patient, type = "prob")[2])
  })
  
  output$risk <- renderText({
     sprintf("Risk for having Bile Duct Stone: %f", risk())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


# ui <- fluidPage(
#   actionButton("runif", "Uniform"),
#   actionButton("rnorm", "Normal"), 
#   hr(),
#   plotOutput("plot")
# )
# 
# server <- function(input, output){
#   v <- reactiveValues(data = NULL)
#   
#   observeEvent(input$runif, {
#     v$data <- runif(100)
#   })
#   
#   observeEvent(input$rnorm, {
#     v$data <- rnorm(100)
#   })  
#   
#   output$plot <- renderPlot({
#     if (is.null(v$data)) return()
#     hist(v$data)
#   })
# }
# 
# shinyApp(ui, server)
