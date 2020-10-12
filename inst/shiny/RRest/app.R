## Shiny to check the effect of the smoothing

library(shiny)
library(shinydashboard)
#library(shinyalert)
library(plotly)
library(shinyWidgets)

library('Covid19RR')

dat <- preprocess.data(download.data())

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Estimation of Rt: The effect of smoothing"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         sliderInput("logrsigma",
                     "Noise level for R [log scale]",
                     min = -10,
                     max = 0,
                     step = 0.1,
                     value=-5)
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("fitPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$fitPlot <- renderPlot({
	   
       obj <- setup.TMB.object(dat)

       opt <- fit(obj,fix=c(logrsigma=input$logrsigma))

       plot_fit(dat,opt)
   })
}

# Run the application
shinyApp(ui = ui, server = server)
