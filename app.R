library(imager)
library(shiny)
require(Cairo)

ui <- fluidPage(
    titlePanel("de stijl generator"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId="symbols",
                        label= "Number of symbols:",
                        min=1,max=10,
                        value=3),
            
            sliderInput(inputId="nfills",
                        label= "Number of fills:",
                        min=1,max=10,
                        value=3),
            
            checkboxInput("allowcurves", "Allow circles", value=TRUE),
            checkboxInput("allowdiagonals", "Allow diagonals", value=TRUE),
            checkboxInput("allowfloats", "Allow floating shapes", value=TRUE),
            
            actionButton("refresh", "Create"),
            
            downloadButton(outputId = "downloadbutton")
        ),
        mainPanel(
            plotOutput(outputId = "imgOut", width = 600, height=600, click="plot_click",)
        )
    ))

server <- function(input, output){
    
    n = 800
    colours <- c("lightblue", "#3030ff", "#dddd22", "#ee2222", "black")
    currentplot <- reactiveValues(currentplot=imfill(n,n,val=.9),
                                  baseplot=imfill(n,n,val=.9))
    
    
    output$imgOut <- renderPlot({
        par(mar=c(0,0,0,0))
           plot(currentplot$currentplot, axes=F, ann=F, rescale=FALSE)
    })
    
    
    observeEvent(input$refresh, {
        ns=input$symbols
        allowedpch <- c(0,3,12)
        if(input$allowdiagonals) allowedpch <- c(allowedpch,5,4,7,8)
        if(input$allowcurves) allowedpch <- c(allowedpch,10,1)
        bg <- imfill(n,n,val=.9)
        bg <- implot(bg,points(runif(ns,0,n),runif(ns,0,n),
                               cex=exp(runif(ns,5,10)), 
                               pch=sample(allowedpch,ns, replace=TRUE),
                               lwd=sample(10:20,ns,replace=TRUE),col="black",
                               ljoin=1,lend=0))
        
        #plot(bg, axes=F, ann=F, rescale=FALSE)
        for(i in 1:input$nfills) bg <- bucketfill(bg,
                                                  x=runif(1,0,n),
                                                  y=runif(1,0,n),
                                                  color = sample(colours,1), 
                                                  sigma=.9)
        
        bg[bg<0] <- 0
        bg[bg>1] <- 1
        currentplot$baseplot <- bg
        currentplot$currentplot <- bg
        })
    
    observeEvent(input$plot_click, {
        colours <- c(colours, rgb(.9,.9,.9))
        px1 <- px.flood(currentplot$baseplot,
                        x=input$plot_click$x,
                        y=input$plot_click$y,
                        sigma=.8)
        currentplot$currentplot <- colorise(currentplot$currentplot,
                                                 px1,
                                                 sample(colours,1))
            
    })
    
    output$downloadbutton <- downloadHandler(
        filename = "modernist.png",
        content = function(file) {
            save.image(currentplot$currentplot, file, quality=1)
        })

    
    
}
shinyApp(ui = ui, server = server)

