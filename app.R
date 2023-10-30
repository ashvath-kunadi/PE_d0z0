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
dat <- readRDS("U:/UWA/Dual action/Et0datimp.Rds")
dat2 <- read.csv("U:/UWA/Final Pres/forshiny.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Change in potential evaporation with parameters"),
  
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
                  value = 4.7),
      sliderInput("A",
                  "Available Energy [W/m^2]:",
                  min = -100,
                  max = 900,
                  value = 30),
      sliderInput("u",
                  "Wind Speed [m/s]:",
                  min = 0.2,
                  max = 11,
                  value = 2.8),
      sliderInput("Tair",
                  "Air Temperature [C]:",
                  min = 1,
                  max = 37,
                  value = 14),
      sliderInput("RH",
                  "Relative Humidity:",
                  min = 0,
                  max = 100,
                  value = 14)
      
    )),
    
    
    # Show a plot of the generated distribution
    column(width = 2,
           plotOutput("PotentialET")
    ),
    column(width = 2,
           plotOutput("z0density")
    ),
    column(width = 2,
           plotOutput("d0density")
    )
  ),
  fluidRow(
    column(4, plotOutput("Aden")),
    column(4, plotOutput("uden")),
    column(4, plotOutput("Taden"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$PotentialET <- renderPlot({
    # generate bins based on input$bins from ui.R
    D <- 4098*(0.6108*exp(17.27*input$Tair/(input$Tair+237.3)))/(input$Tair+237.3)^2 ##kPa/C
    psyc <- 0.067
    es <- (6984.505294+(input$Tair+273.15)*(-188.9039310+(input$Tair+273.15)*
                                  (2.1333357675+(input$Tair+273.15)*(-1.288580973*10^-2+(input$Tair+273.15)*(4.393587233 * 10^-5 + 
                                  (input$Tair+273.15)*(-8.023923082 * 10^-8 + (input$Tair+273.15)*6.136820929 * 10^-11))))))*0.1
    cp <- 1013
    rho <- 1.21
    convra <- 1/0.4/0.4/(input$u)*(log((15-4.7)/0.7))^2
    yourra <- 1/0.4/0.4/(input$u)*(log((15-input$d0)/input$z0))^2
    convPET <- D*input$A/(D+psyc)+((rho*cp)*(1-(input$RH/100))*es/(D+psyc)/convra)*24*60*60/2465758
    yourPET <- D*input$A/(D+psyc)+((rho*cp)*(1-(input$RH/100))*es/(D+psyc)/yourra)*24*60*60/2465758
    top <- data.frame(x = c("Conventional PE", "Your PE"),y= c(convPET,yourPET))
    ggplot(top, aes(x,y)) + geom_bar(stat = "identity") + geom_text(aes(label=round(y,2)), vjust=0) + 
      theme_classic() + ylab("Potential Evaporation [mm/day]") + xlab(element_blank())
  })
  output$Aden <- renderPlot({
    ggplot(dat, aes(x=(Rn-G))) + geom_histogram(alpha=0.4) + scale_fill_grey() + theme_classic() + 
      geom_vline(xintercept = input$A, col = "darkgreen") + 
      xlab("Avilable Energy [W/m^2]")
  })
  output$Taden <- renderPlot({
    ggplot(dat, aes(x=Tair)) + geom_histogram(alpha=0.4) + scale_fill_grey() + theme_classic() + 
      geom_vline(xintercept = input$Tair, col = "darkgreen") + 
      xlab("Air Temperature [C]")
  })
  output$uden <- renderPlot({
    ggplot(dat, aes(x=u)) + geom_histogram(alpha=0.4) + scale_fill_grey() + theme_classic() + 
      geom_vline(xintercept = input$u, col = "darkgreen") + 
      xlab("Wind Speed [m/s]")
  })
  output$z0density <- renderPlot({
    ggplot(dat2, aes(x=varz02)) + geom_histogram(alpha=0.4) + scale_fill_grey() + theme_classic() + geom_vline(xintercept = 0.7, col = "red") + geom_vline(xintercept = input$z0, col = "darkgreen") + xlab("Roughnes length [m]")
  })
  output$d0density <- renderPlot({
    ggplot(dat2, aes(x=vard2)) + geom_histogram(alpha=0.4) + scale_fill_grey() + theme_classic() + geom_vline(xintercept = 4.7, col = "red") + geom_vline(xintercept = input$d0, col = "darkgreen") + xlab("Zero Plane Displacement length [m]")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
