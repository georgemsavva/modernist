#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mystery curve plotter"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textAreaInput("mainFunction", "f(t)=", "exp(1i*12*pi*t) +\n0.3*exp(-1i*25*pi*t)",),
            textInput("maxt","max(t)=","2"),
            textInput("widthFunction", "w(t)=", "pmax(4*Arg(exp(2i*pi*12*t)^2),2)"),
            textInput("hFunction", "hue(t)=", "sin(t)^2"),
            textInput("sFunction", "saturation(t)=", ".2"),
            textInput("vFunction", "value(t)=", "sin(2*pi*t)^2")
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            
           plotOutput("distPlot",width = 600,height=600)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    vecToSegments <- function(z){
        d <- data.frame(x0=Re(z), y0=Im(z))
        d$x1 <- c(d$x0[-1], d$x0[1])
        d$y1 <- c(d$y0[-1], d$y0[1])
        d
    }
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        t <- seq(0,as.numeric(input$maxt),l=2000)
        eval(parse(text = paste0("f <- function(t) ", input$mainFunction)))
        eval(parse(text = paste0("w <- function(t) ", input$widthFunction)))
        eval(parse(text = paste0("h <- function(t) ", input$hFunction)))
        eval(parse(text = paste0("s <- function(t) ", input$sFunction)))
        eval(parse(text = paste0("v <- function(t) ", input$vFunction)))
        z <- f(t)
        d <- vecToSegments(z)
        l <- max(abs(z))
        par(mar=c(0,0,0,0))
        plot(NA, xlim=c(-l,l),ylim=c(-l,l),axes=F, ann=F )
        segments(d$x0,d$y0, d$x1, d$y1, lwd=abs(w(t)), col=hsv(h(t),s(t),v(t)) )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
