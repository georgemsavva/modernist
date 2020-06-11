
#

library(shiny)
library(gifski)
library(png)
library(colourpicker)
library(ambient)

stipple <- function(a){
    ifelse(a>0,1,rbinom(length(a),1,1+a))
}


C <- function(freqency=1,amplitude=1,offset=0){
    function(t,a) exp(2i*pi*(t+offset))*amplitude
}

ui <- function(request) {
    fluidPage(
        

            fluidRow(
            column(5,h3("Mystery Curve Plotter"),
                   fluidRow(
                       column(6,
                              #radioButtons("mode","Mode", c("Domain colouring"="domain","Curve plotting"="curve")),
                              textAreaInput("mainFunction", "z(t,a)", "exp(1i*12*pi*t) +\n(.4+.2*sin(2*a*pi))*exp(-1i*25*pi*t)",),
                              
                              
                              textInput("widthFunction", label="width(t,a)", "pmax(4*Arg(exp(2i*pi*12*(t-4*pi*a/100))^2),1)"),
                                textInput("hFunction", label="hue(t,a)", "sin(t)^2"),
                                textInput("sFunction", label="saturation(t,a)", ".7"),
                                textInput("vFunction", label="value(t,a)", "sin(2*pi*t)^2"),
                                textInput("alphaFunction", label="alpha", "1"),
                                selectInput("presets", label="Preset", choices=NULL, selected=NULL, multiple=FALSE),
                                fileInput("fileup",label = "Open settings from file"),
                                downloadButton("save","Download current settings")
                       ),
                       column(6,
                              radioButtons("animatemode","Animation:", 
                                           c("Single value of 'a' (animate with slider)"="single",
                                             "All 'a' superimposed"="simultaneous",
                                             "All 'a' in grid layout"="grid",
                                             #"Pixel"="pixel",
                                             "Animate over a=(0,1) (make .gif)"="animate")),
                              sliderInput("avalue",label="a",min = 0,max=1, step=0.001,value=0, 
                                          animate=animationOptions(interval=200)),
                              fluidRow(
                                  column(6,numericInput("nrowsInGrid","Rows in grid layout",value=3)),
                                  column(6,numericInput("oma","Outer Margin",value=1))),
                              radioButtons("drawmode","Draw:", 
                                           c("Segments"="segments",
                                             "Points"="points",
                                             "Pixels (Domain coloring)"="pixels"),),
                              fluidRow(
                                  column(4,textInput("resolution", label="Resolution", "2000")),
                                  column(4,textInput("nSteps", label="length(t)", "1000")),
                                  column(4,textInput("nFrames", label="a=", "seq(0,1,l=100)"))),
                              fluidRow(
                                  column(4,textInput("maxt",label="max(t)","2")),
                                  column(4,textInput("limitScale", label="Scale", "1.2")),
                                  column(4,colourInput("bgcol",label = "Background",value = "#f7f7f7"))),
                              fluidRow(column(6,bookmarkButton()),
                                       column(6,actionButton("hires",label = "Hi res image")),
                                       column(6,actionButton("gif", label = "Make animated gif"))),
                              fluidRow(column(6,textInput("seed", "seed=", "28052002")))
                       
                   )
            )
        ),
        
        column(7,
               imageOutput("distPlot", height="80%")
        )
    ))
}

## Each preset is in a list in its own rds file.

populatePresets <- function(){
    
}


readfromFile <- function(filename,session){
    list <- as.list(read.table(filename,stringsAsFactors = FALSE))
    for(x in names(list)) updateInput(x,list[[x]],session)
}


updateInput <- function(name, value, session){
    updateTextAreaInput(session, name,value=value)
    updateTextInput(session, name,value=value)
}

    

# Define server logic required to draw a histogram
server <- function(input, output, session) {


    
    vecToSegments <- function(z){
        d <- data.frame(x0=Re(z), y0=Im(z))
        d$x1 <- c(d$x0[-1], d$x0[1])
        d$y1 <- c(d$y0[-1], d$y0[1])
        d
    }
    
#    l <- list(nFrames=25)
#    saveOut("test.csv",l)
#    readfromFile("test.csv",session)

    output$save <- downloadHandler(
        filename = function() {
            "settings.tab"
        },
        content = function(file) {
            settings <-  lapply(reactiveValuesToList(input), as.character)
            settings$fileup <- NULL
            write.table(settings, file)   
        })
    
    
    
    observeEvent(input$fileup, {
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        inFile <- input$fileup
        if (is.null(inFile))
            return(NULL)
        readfromFile(inFile$datapath,session)

    })

    trunc01 <- function(x){
        pmin(pmax(x,0),1)
    }
    
    mandelbrot <- function(t,max=20){
        n=rep(0, length(t)) 
        z=rep(0, length(t))
        for(i in 1:max){
            z <- z^2 + t
            n <- n + ifelse(Mod(z)<10,1,0)
        }
        n 
        }
    
    
    output$distPlot <- renderImage({
        print(Sys.time())
        graphics.off()
        res=as.numeric(input$resolution)
        set.seed(as.integer(input$seed))
        maxt = eval(parse(text = input$maxt))
        if(input$animatemode=="single") a = input$avalue else a = eval(parse(text=input$nFrames))
        pl=length(a)
        t = seq(0,maxt,l=as.numeric(input$nSteps))
        if(input$drawmode=="pixels") {
            td = as.numeric(input$maxt)*expand.grid( t1=seq(-1, 1, l=res) , t2=seq(-1, 1, l=res))
            t = 1i*td[,1] + td[,2]
            }
        d = expand.grid(ai=1:length(a),ti=1:length(t))
        d$t = t[d$ti]
        d$a = a[d$ai]
        eval(parse(text = paste0("f <- function(t,a) ", input$mainFunction)))
        e <- exp(1i*t)
        eval(parse(text = paste0("w <- function(t,a) ", input$widthFunction)))
       # eval(parse(text = paste0("radius <- function(t,a) ", input$radiusFunction)))
        eval(parse(text = paste0("h <- function(t,a) ", input$hFunction)))
        eval(parse(text = paste0("s <- function(t,a) ", input$sFunction)))
        eval(parse(text = paste0("v <- function(t,a) ", input$vFunction)))
        eval(parse(text = paste0("alpha <- function(t,a) ", input$alphaFunction)))
        print(Sys.time())
        d$z <- f(d$t,d$a)
        d$x0 <- x <- Re(d$z)
        d$y0 <- y <- Im(d$z)
        d$h <- h(d$t,d$a)
        d$alpha <- alpha(d$t,d$a)
        d$s <- s(d$t,d$a)
        d$v <- v(d$t,d$a)
        if(input$drawmode=="pixels"){
            d$hr <- trunc01(Mod(h(d$z,d$a)))
            d$sr <- trunc01(Mod(s(d$z,d$a)))
            d$vr <- trunc01(Mod(v(d$z,d$a)))
            d$rgb <- hsv(d$hr, d$sr, d$vr)
        }
        print(Sys.time())
        #d$radius <- radius(d$t,d$a)
        d$lwd <- w(d$t,d$a)
        l=max(Mod(d$z))*as.numeric(input$limitScale)
        if(input$drawmode=="pixels") l = as.numeric(input$resolution)
        lmin=-l
        if(input$drawmode=="pixels") lmin = 0
        png_path <- file.path(tempdir(), "frame%03d.png")
        png(png_path, type = "cairo-png",width=res,height=res, antialias = "subpixel")
        par(ask = FALSE,mar=c(0,0,0,0), bg=input$bgcol)
        
        if(input$animatemode=="animate"){
 
            print(paste0("printing ",pl," frames."))
            for (j in 1:pl) {
                plot(NA,xlim=c(lmin,l), ylim=c(lmin,l),axes=F, ann=F)
                d2 <- d[d$ai==j,]
                d2$x1 <- c(Re(d2$z)[-1], NA)
                d2$y1 <- c(Im(d2$z)[-1],NA)
                if(input$drawmode=="segments") segments(d2$x0,d2$y0, d2$x1,d2$y1, col = hsv(d2$h, d2$s, d2$v, d2$alpha), lwd=d2$lwd)
                if(input$drawmode=="points") points(d2$x1,d2$y1, col = hsv(d2$h, d2$s, d2$v, d2$alpha), cex=d2$lwd, pch=20)
                #if(input$drawmode=="circles") symbols(d2$x1,d2$y1, col = hsv(d2$h, d2$s, d2$v, d2$alpha), circles=d2$lwd)
                if(input$drawmode=="pixels") {
                    ras <- as.raster(matrix(d2$rgb, nrow=res))
                    plot(ras, add=TRUE)
                }
                
                print(j)
            }
            dev.off()
            png_files <- sprintf(png_path, 1:pl)
            gif_file <- tempfile(fileext = ".gif")
            gifski(png_files, gif_file,delay = 0.033, width = res,height=res)
            unlink(png_files)
            rm(d)
            gc()
            print(Sys.time())
            return(list(src = gif_file,
                        contentType = 'image/gif'
                        # width = 400,
                        # height = 300,
                        # alt = "This is alternate text"
            ))
            
            
        } else {
            pl=1
            if(input$animatemode!="grid") plot(NA,xlim=c(lmin,l), ylim=c(lmin,l),axes=F, ann=F)
            if(input$animatemode=="grid") par(mfrow=c(3,3), oma=rep(input$oma,4))
            for (j in 1:length(unique(d$a))) {
                
                d2 <- d[d$ai==j,] 
                #print(d2)
                d2$x1 <- c(Re(d2$z)[-1], NA)
                d2$y1 <- c(Im(d2$z)[-1],NA)
                if(input$animatemode=="grid") plot(NA,xlim=c(lmin,l), ylim=c(lmin,l),axes=F, ann=F)
                if(input$drawmode=="segments") segments(d2$x0,d2$y0, d2$x1,d2$y1, col = hsv(d2$h, d2$s, d2$v, d2$alpha), lwd=d2$lwd)
                if(input$drawmode=="points") points(d2$x1,d2$y1, col = hsv(d2$h, d2$s, d2$v, d2$alpha), cex=d2$lwd, pch=20)
                if(input$drawmode=="pixels") {
                    ras <- as.raster(matrix(d2$rgb, nrow=res))
                    #print(ras)
                    plot(ras, add=TRUE)
                }
            }
            dev.off()
            png_file <- sprintf(png_path, 1)
            rm(d)
            gc()
            print(Sys.time())
            
                        return(list(src = png_file,
                        contentType = 'image/png',
                        width = "80%",
                        height = "80%"
                        # alt = "This is alternate text"
            ))
        }
    }, deleteFile = TRUE)}

# Run the application 
shinyApp(ui = ui, server = server,enableBookmarking = "url")




###
#
# exp(1i*2*pi*t)  +.3*exp(-1i*a*2000*pi*t) * sin(pi*t*10-a)^3
# 7000 steps  (ring of circles)
#
#(t<=1)*(exp(1i*2*pi*t)  +.3*exp(-1i*(a+t/400)*4000*pi*t) * sin(pi*t*8)^2) + 
# (t>1)*.25*(exp(1i*2*pi*t) + .7*exp(-1i*6*pi*t) + .4*exp(-1i*a*600*pi*t) * sin(pi*t*4+pi/2)^3)
