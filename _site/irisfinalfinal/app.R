#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
data("iris")
options(scipen = 999)

# Define UI for application that draws a histogram
ui <- fluidPage(
  h1("Iris Correlation by Carmen Garcia-Russell"),
  h6("Diagram of Flower"),
  tags$img(src = '51518iris img1.png', height=550, width=700),
  

    # Application title
    titlePanel("Petals and Sepal Measurements"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "VarX",
                        label = "Petals:",
                        choices = list("Petal.Length", "Petal.Width")),
            selectInput(inputId = "VarY",
                        label = "Sepals:",
                        choices = list("Sepal.Length", "Sepal.Width")),
            tags$img(src = 'irisdrawing.webp', height=200, width=200)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatter"),
           verbatimTextOutput("correlation")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatter <- renderPlot({
        
      iris1 <- iris[ ,c(input$VarX, input$VarY)]
      
      ggplot(data = iris1, aes(x=iris1[,1], y=iris1[,2]))+
        geom_point()+
        
      geom_smooth(method = 'lm',se=FALSE)+
        
        labs(x = colnames(iris1)[1],
             y = colnames(iris1)[2])
      
    })
    
    output$correlation <- renderPrint({
      
      irisfinalset <- iris[ , c(input$VarX, input$VarY)]
      
      cor.test(irisfinalset[, 1], irisfinalset[, 2])
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

#### photo credit
#https://www.analyticsvidhya.com/blog/2022/06/iris-flowers-classification-using-machine-learning/
#https://lifestyle.howstuffworks.com/crafts/drawing/how-to-draw-an-iris.htm
