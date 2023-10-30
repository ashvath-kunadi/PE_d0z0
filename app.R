#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gganimate)
dat <- read.csv("H:/My Documents/Project and related work/Rainfall-Throughfall/forshiny.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Change in log profile with parameters"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(width = 4, wellPanel(
            sliderInput("z0",
                        "Roughness Length",
                        min = 0.1,
                        max = 4,
                        value = 0.7),
            sliderInput("d0",
                        "Zero Plane Displacement Length",
                        min = 0,
                        max = 6.9,
                        value = 4.7)
        )),
        
        # Show a plot of the generated distribution
        column(width = 8,
           plotOutput("logprofile")
        )
    ),
    fluidRow(
        column(6, plotOutput("z0density")),
        column(6, plotOutput("d0density"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$logprofile <- renderPlot({
        # generate bins based on input$bins from ui.R
        z    <- seq((input$d0+0.1),30,0.1)
        Windspeed <- 0.63212032/0.4*log((z-input$d0)/input$z0)
        logprfdat <- data.frame(z,Windspeed)
        # draw the histogram with the specified number of bins
        fun.1 <- function(x) {(exp(0.4*x/0.63212032)*0.7)+4.7}
        ggplot(logprfdat, aes(Windspeed, z)) + geom_line(color = "darkgreen") + xlim(0,10) + ylim(0,30) + xlab("Wind speed [m/s]") + ylab("z [m]") + theme_minimal() + stat_function(fun = fun.1, color = "red") + geom_segment(aes(x = 1, y = 7, xend = 2, yend = 7))  + geom_segment(aes(x = 1, y = 10.6, xend = 2, yend = 10.6)) + geom_segment(aes(x = 1, y = 15.5, xend = 2, yend = 15.5)) + geom_segment(aes(x = 0, y = 15, xend = 1, yend = 15), colour = "orange") 

    })
    output$z0density <- renderPlot({
        ggplot(dat, aes(x=varz02, fill = wdcat)) + geom_density(alpha=0.4) + scale_fill_grey() + theme_classic() + geom_vline(xintercept = 0.7, col = "red") + geom_vline(xintercept = input$z0, col = "darkgreen") + xlab("Roughnes length [m]")
    })
    output$d0density <- renderPlot({
        ggplot(dat, aes(x=vard2, fill = wdcat)) + geom_density(alpha=0.4) + scale_fill_grey() + theme_classic() + geom_vline(xintercept = 4.7, col = "red") + geom_vline(xintercept = input$d0, col = "darkgreen") + xlab("Zero Plane Displacement length [m]")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
