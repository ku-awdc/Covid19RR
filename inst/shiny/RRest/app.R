## Shiny to check the effect of the smoothing

library("shiny")
library("shinydashboard")
#library(shinyalert)
library("plotly")
library("shinyWidgets")

library('Covid19RR')

dat <- download_data()

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Estimation of Rt: The effect of smoothing"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         sliderInput("tau",
                     "Time horizon for smoothing",
                     min = 3,
                     max = 60,
                     step = 1,
                     value=10)
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

      mod <- estimate_cv19rr(dat, fix=c(logtau=log(input$tau)))
      plot(mod)

   })
}

# Run the application
shinyApp(ui = ui, server = server)
