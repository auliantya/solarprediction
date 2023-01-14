############################################
# Solar Forecasting                        #
############################################

# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(e1071)

# Read in the RF model
model <- readRDS("model.rds")

# Training set
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]


####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("sandstone"),
                navbarPage("WQD7001 - Group 11 Project App",
                           
                           tabPanel("Home",
                                    # Page header
                                    headerPanel('Solar Radiation Forecasting using SVM'),
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input meteorological parameters</h4>"),
                                      sliderInput("temperature", label = "24 Hour Mean Temperature (°C)", value = 30.5,
                                                  min = 0,
                                                  max = 100),
                                      sliderInput("humidity", label = "24 Hour Mean Relative Humidity (%)", value = 75.5,
                                                  min = 0,
                                                  max = 100),
                                      sliderInput("wind", label = "24 Hour Mean Wind (m/s)", value = 1.5,
                                                  min = min(TrainSet$wind),
                                                  max = max(TrainSet$wind)),
                                      sliderInput("msl_pressure", label = "Mean MSL Pressure(Hpa)", value = 1010.8,
                                                  min = min(TrainSet$msl_pressure),
                                                  max = max(TrainSet$msl_pressure)),
                                      
                                      actionButton("submitbutton", "Submit", class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Forecast Status & Output')), # Status/Output Text Box
                                      verbatimTextOutput('contents'),
                                      tableOutput('tabledata') # Prediction results table
                                    )
                                    
                           ),
                           
                           tabPanel("About", 
                                    titlePanel("About"), 
                                    div(includeMarkdown("about.md"), 
                                        align="justify")
                           )
                           
                )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("temperature",
               "humidity",
               "wind",
               "msl_pressure"),
      Value = as.character(c(input$temperature,
                             input$humidity,
                             input$wind,
                             input$msl_pressure)),
      stringsAsFactors = FALSE)
    
    solar_radiation <- 0
    df <- rbind(df, solar_radiation)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Solar_Radiation_Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)['Solar_Radiation_Prediction']
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Solar Forecast Completed. Below the prediction of solar radiation (MJm-2): ") 
    } else {
      return("Server is ready for forecast.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
