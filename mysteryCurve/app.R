
#

library(shiny)
library(plotly)
library(gifski)
library(png)
library(colourpicker)

ui <- fluidPage(

    # # Application title
 
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #radioButtons("mode","Mode", c("Domain colouring"="domain","Curve plotting"="curve")),
            radioButtons("animatemode","Animation:", 
                               c("Single value of 'a' (animate with slider)"="single",
                                 "All values of 'a'"="simultaneous",
                                 "Animate over a=(0,1) (make .gif)"="animate")),
            textAreaInput("mainFunction", "f(t)=", "exp(1i*12*pi*t) +\n(.4+.2*sin(2*a*pi))*exp(-1i*25*pi*t)",),
            tags$table(width="100%",
                tags$tr(width = "100%",
                        tags$td(width = "00%", div("a=")),
                        tags$td(width = "80%", sliderInput("avalue",label=NULL,min = 0,max=1, step=0.001,value=0, 
                                                           animate=animationOptions(interval=200)))
                        ),
                tags$tr(width = "100%",
                        tags$td(width = "0%", div("Background")),
                        tags$td(width = "100%", colourInput("bgcol",label = NULL,value = "white"))
                        ),
                tags$tr(width = "100%",
                        tags$td(width = "00%", div("w(t)=")),
                        tags$td(width = "100%", textInput("widthFunction", label=NULL, "pmax(4*Arg(exp(2i*pi*12*(t-4*pi*a/100))^2),1)"))
                        ),
                tags$tr(width = "100%",
                        tags$td(width = "00%", div("scale=")),
                        tags$td(width = "100%", textInput("limitScale", label=NULL, "1.2"))
                ),        
                tags$tr(width = "100%",
                        tags$td(width = "0%", div("max(t)=")),
                        tags$td(width = "100%", textInput("maxt",label=NULL,"2"))
                        ),
                tags$tr(width = "100%",
                        tags$td(width = "0%", div("hue(t)=")),
                        tags$td(width = "100%", textInput("hFunction", label=NULL, "sin(t)^2"))
                ),
                tags$tr(width = "100%",
                        tags$td(width = "0%", "saturation(t)="),
                        tags$td(width = "100%", textInput("sFunction", label=NULL , ".7"))
                ),
                tags$tr(width = "100%",
                        tags$td(width = "0%", div("value(t)=")),
                        tags$td(width = "100%", textInput("vFunction", label=NULL, "sin(2*pi*t)^2"))
                ),
                tags$tr(width = "100%",
                        tags$td(width = "0%", div("alpha(t)=")),
                        tags$td(width = "100%", textInput("alphaFunction", label=NULL, "1"))
                ),
                tags$tr(width = "100%",
                        tags$td(width = "0%", div("Frames")),
                        tags$td(width = "100%", textInput("nFrames", label=NULL, "100"))
                ),
                tags$tr(width = "100%",
                        tags$td(width = "0%", div("Steps")),
                        tags$td(width = "100%", textInput("nSteps", label=NULL, "1000"))
                ),
                tags$tr(width = "100%",
                        tags$td(width = "0%", div("Resolution")),
                        tags$td(width = "100%", textInput("resolution", label=NULL, "600"))
                )
            
                
                
                
                
            )
        ),
        
        mainPanel(
        h1("Mystery curve plotter"), 
        imageOutput("distPlot", height=800, width=800)
        )
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    vecToSegments <- function(z){
        d <- data.frame(x0=Re(z), y0=Im(z))
        d$x1 <- c(d$x0[-1], d$x0[1])
        d$y1 <- c(d$y0[-1], d$y0[1])
        d
    }
    output$distPlot <- renderImage({
        graphics.off()
        maxt = eval(parse(text = input$maxt))
        fr = as.numeric(input$nFrames)
        if(input$animatemode=="single") a = input$avalue else a = seq(0,1,l=fr)
        t = seq(0,maxt,l=as.numeric(input$nSteps))
        d = expand.grid(ai=1:length(a),t=t)
        d$a = a[d$ai]
        eval(parse(text = paste0("f <- function(t,a) ", input$mainFunction)))
        e <- exp(1i*t)
        eval(parse(text = paste0("w <- function(t,a) ", input$widthFunction)))
        eval(parse(text = paste0("h <- function(t,a) ", input$hFunction)))
        eval(parse(text = paste0("s <- function(t,a) ", input$sFunction)))
        eval(parse(text = paste0("v <- function(t,a) ", input$vFunction)))
        eval(parse(text = paste0("alpha <- function(t,a) ", input$alphaFunction)))
        
        d$z <- f(d$t,d$a)
        d$x0 <- Re(d$z)
        d$y0 <- Im(d$z)
        d$h <- h(d$t,d$a)
        d$alpha <- alpha(d$t,d$a)
        d$s <- s(d$t,d$a)
        d$v <- v(d$t,d$a)
        d$lwd <- w(d$t,d$a)
        l=max(abs(d$z))*as.numeric(input$limitScale)
        res=as.numeric(input$resolution)
        png_path <- file.path(tempdir(), "frame%03d.png")
        png(png_path, type = "cairo",width=res,height=res)
        par(ask = FALSE,mar=c(0,0,0,0), bg=input$bgcol)
        
        if(input$animatemode=="animate"){
            pl=as.numeric(input$nFrames)
            for (j in 1:pl) {
            plot(NA,xlim=c(-l,l), ylim=c(-l,l),axes=F, ann=F)
            d2 <- d[d$ai==j,]
            d2$x1 <- c(Re(d2$z)[-1], NA)
            d2$y1 <- c(Im(d2$z)[-1],NA)
            segments(d2$x0,d2$y0, d2$x1,d2$y1, col = hsv(d2$h, d2$s, d2$v, d2$alpha), lwd=d2$lwd)
            print(j)
            }
            dev.off()
            png_files <- sprintf(png_path, 1:pl)
            gif_file <- tempfile(fileext = ".gif")
            gifski(png_files, gif_file,delay = 0.033, width = res,height=res)
            unlink(png_files)
            return(list(src = gif_file,
                 contentType = 'image/gif'
                 # width = 400,
                 # height = 300,
                 # alt = "This is alternate text"
                 ))
            
        } else {
            pl=1
            plot(NA,xlim=c(-l,l), ylim=c(-l,l),axes=F, ann=F)
            for (j in 1:length(unique(d$a))) {
                d2 <- d[d$ai==j,]
                d2$x1 <- c(Re(d2$z)[-1], NA)
                d2$y1 <- c(Im(d2$z)[-1],NA)
                segments(d2$x0,d2$y0, d2$x1,d2$y1, col = hsv(d2$h, d2$s, d2$v, d2$alpha), lwd=d2$lwd)
                print(j)
            }
            dev.off()
            png_file <- sprintf(png_path, 1)
                        return(list(src = png_file,
                        contentType = 'image/png'
                        # width = 400,
                        # height = 300,
                        # alt = "This is alternate text"
            ))
        }
        }, deleteFile = TRUE)}

# Run the application 
shinyApp(ui = ui, server = server)




###
#
# exp(1i*2*pi*t)  +.3*exp(-1i*a*2000*pi*t) * sin(pi*t*10-a)^3
# 7000 steps  (ring of circles)
#
#(t<=1)*(exp(1i*2*pi*t)  +.3*exp(-1i*(a+t/400)*4000*pi*t) * sin(pi*t*8)^2) + 
# (t>1)*.25*(exp(1i*2*pi*t) + .7*exp(-1i*6*pi*t) + .4*exp(-1i*a*600*pi*t) * sin(pi*t*4+pi/2)^3)
