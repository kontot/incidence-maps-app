# http://herethere.net/~samson/php/color_gradient/?cbegin=000000&cend=FFFFFF&steps=7
# http://stackoverflow.com/questions/25931312/r-shiny-ui-inputs-format
#-----------------------------------------------------------------------------------
# Libraries
#-----------------------------------------------------------------------------------
##install.packages("sp")
library(sp)
##install.packages("maptools")
library(maptools)
##install.packages("RColorBrewer")
library(RColorBrewer)
#See available palettes #In this case, the YlGnBu palette was used
##display.brewer.all()
library(shiny)
options(shiny.reactlog=TRUE)
options(display.mode = "showcase")
library(ggplot2)
# install.packages("devtools")#if not alrady installed
# devtools::install_github("ShinySky","AnalytixWare")
# require(shinysky)

## Load L?nkarta

# load(file.path("E:/Shiny_Map", "L?nkarta.RData"))
load(file.path("G:/Projekt/Biostatistik/BIMs R-bibliotek/R tips Gustaf", "Länkarta.RData"))
# load("C:/Users/Achilleas/Desktop/shiny app/Lankarta.RData")
# load(file.path("D:/work related/work related/work/BIMs R-bibliotek/R tips Gustaf", "Lankarta2.RData"))


## Merge

merge.sp<-function(karta,df,by.x,by.y){
  require(maptools)
  require(sp)
  df<-df[match(karta@data[,by.x],df[,by.y]),]
  rownames(df)<-rownames(karta@data)
  resultat<-spCbind(karta,df)
  return(resultat)
}


shinyServer(function(input, output) {
  
  data = reactive({
    inFile<-input$file1
    read.csv(inFile$datapath, header=T, sep=";",dec=",")
    
  })
  
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
  
    if (is.null(inFile))
      return(invisible())
    
    read.csv(inFile$datapath, header=T, sep=";",dec=",")
  })
  
  output$title <- renderText({ 
    
    if (is.null(input$file1))
      return(invisible())
    "Summary"
  })  
  
  output$summary <- renderPrint({ 
    
      if (is.null(input$file1))
      return(invisible())
      data1 <- data()
    summary(data1[,"Incidens"])
  })  




textInput3 <- function (inputId, label, value = "", ...) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$textarea(id=inputId, rows=1, cols=5, value = value))
#       tags$input(id = inputId, type = "text", value = value,class="input-mini", ...))
  
}


textInput4 <- function (inputId, label, value = "", ...) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$textarea(id=inputId, rows=1, cols=10, value = value))
#       tags$input(id = inputId, type = "text", value = value, class="input-small", ...))
  
}


colorInput <- function(inputId, label, value, ...){
    div(style="display:inline-block", 
        tags$label(label, `for` = inputId),
        tags$select(id = inputId, class="input-mini", 
                    tagList(mapply(tags$option, value = c("#FFFF99", "#FFCC72", 
                                                          "#FF994C", "#FF6626", 
                                                          "#FF3300", "#FF0000", 
                                                          "#990000", "#CCCCFF", 
                                                          "#AAAADD", "#8888BB", 
                                                          "#666699", "#444477", 
                                                          "#212155", "#000033", 
                                                          "#99FF99", "#7FDD7F", 
                                                          "#66BB66", "#4C994C", 
                                                          "#337733", "#195519", 
                                                          "#003300", "#FFFFFF", 
                                                          "#DFDFDF", "#BFBFBF", 
                                                          "#9F9F9F", "#7F7F7F", 
                                                          "#5F5F5F", "#3F3F3F", 
                                                          "#1F1F1F", "#000000"), 
                                   style = paste("background-color:", c("#FFFF99", "#FFCC72", 
                                                                        "#FF994C", "#FF6626", 
                                                                        "#FF3300", "#FF0000", 
                                                                        "#990000", "#CCCCFF", 
                                                                        "#AAAADD", "#8888BB", 
                                                                        "#666699", "#444477", 
                                                                        "#212155", "#000033", 
                                                                        "#99FF99", "#7FDD7F", 
                                                                        "#66BB66", "#4C994C", 
                                                                        "#337733", "#195519", 
                                                                        "#003300", "#FFFFFF", 
                                                                        "#DFDFDF", "#BFBFBF", 
                                                                        "#9F9F9F", "#7F7F7F", 
                                                                        "#5F5F5F", "#3F3F3F", 
                                                                        "#1F1F1F", "#000000"), sep=""), 
                                   SIMPLIFY=FALSE)))
    )
    
}


boxes <- reactive({
    
    n <- input$n_breaks
    data1 <- data()
    labLow <- label_low()
    labUp <- label_up()
    
    for(i in 1:n){
        label_min <- paste("group", i, "min", sep="_")
        label_max <- paste("group", i, "max", sep="_")
        label_col <- paste("col", i, sep="_")

        legend_map <- paste("leg",i,"map",sep="_")       
        break.points <- quantile(data1[,"Incidens"], probs = seq(0, 1, 1/n), na.rm=TRUE)
        break.points[1] <- floor(break.points[1])
        break.points[2:length(break.points)] <- ceiling(break.points[2:length(break.points)])
        
        color_list <- c("#FFFF99", "#FFCC72", "#FF994C", "#FF6626", 
                        "#FF3300", "#FF0000", "#990000", "#CCCCFF", 
                        "#AAAADD", "#8888BB", "#666699", "#444477", 
                        "#212155", "#000033", "#99FF99", "#7FDD7F", 
                        "#66BB66", "#4C994C", "#337733", "#195519", 
                        "#003300", "#FFFFFF", "#DFDFDF", "#BFBFBF", 
                        "#9F9F9F", "#7F7F7F", "#5F5F5F", "#3F3F3F", 
                        "#1F1F1F", "#000000")
        
        print(textInput3(inputId = label_min, label=h6(paste("Low",i,"")), value = as.numeric(break.points)[i] , class="input-mini"))

        print(textInput3(inputId = label_max, label=h6(paste("Upper",i,"")), value = as.numeric(break.points)[i+1],class="input-mini"))
        
        print(textInput4(inputId = legend_map, label=h6(paste("Leg_map",i,"")), class="input-small"))
        
        print(colorInput(inputId = paste("col", i, sep="_"), label= h6("Välj färg"), 
                       value = color_list[i] ,class="input-mini"))
        
        print(br())
      
    }
})

output$breaks <- renderPrint({boxes()})
    



scale.karta <- reactive({
  
    n <- input$n_breaks

    lapply(1:n, function(i) {
        input[[paste("group", i, "max", sep="_")]]
    })
})



col.karta <- reactive({
    
    n <- input$n_breaks
    
    lapply(1:n, function(i) {
        input[[paste("col", i, sep="_")]]
    })
})


label_low <- reactive({
  
    n <- input$n_breaks
    
    lapply(1:n, function(i) {
        input[[paste("group", i, "min", sep="_")]]
    })
})


label_up <- reactive({
  
    n <- input$n_breaks
    
    lapply(1:n, function(i) {
        input[[paste("group", i, "max", sep="_")]]
    })
})


leg_map <- reactive({
  
  n <- input$n_breaks
  
  lapply(1:n, function(i) {
    input[[paste("leg", i,"map", sep="_")]]
  })
})



output$map <- renderPlot({
    n <- input$n_breaks
  
    dat <- data()
    karta <- merge.sp(Länkarta, dat, by.x="countyname", by.y="Län")
    

    
    scal<-scale.karta()

    scal2 <- c(input$group_1_min, scal)
    lab <- leg_map()
    col.k <- as.character(col.karta())
    
    a <- which(colnames(karta@data)=="Incidens")
   

    plot(0,0, type="n", axes=FALSE, frame.plot=F, xlab="", ylab="",main="")
    print(spplot(karta,zcol=a, pretty=T, at=scal2, colorkey=FALSE,
                 col.regions= col.k,
                 par.settings = list(axis.line = list(col = 'transparent')),main=input$plot_title))
     mtext(input$legend_title, at=-0.65, line=0)
     legend(-1, 1.10, legend=lab,fill= col.k, bty="n")


}, height=600,width=400)



# output$map <- renderPlot({
#   print(plotInput())
# },height=600,width=400)






output$downloadPlot <- downloadHandler(
  filename = function(){
    paste("map",input$var1,sep=".")
  },
  content = function(file) {
    
    if (input$var1=="png"){
      png(file)
    } else {

      jpeg(file, quality = 100, width = 800, height = 800)
    }
    n <- input$n_breaks
    
    dat <- data()
    karta <- merge.sp(Länkarta, dat, by.x="countyname", by.y="Län")
    
    
    
    scal<-scale.karta()
    
    scal2 <- c(input$group_1_min, scal)
    lab <- leg_map()
    col.k <- as.character(col.karta())
    
    a <- which(colnames(karta@data)=="Incidens")
    
    
    plot(0,0, type="n", axes=FALSE, frame.plot=F, xlab="", ylab="",main="")
    print(spplot(karta,zcol=a, pretty=T, at=scal2, colorkey=FALSE,
                 col.regions= col.k,
                 par.settings = list(axis.line = list(col = 'transparent')),main=input$plot_title))
    mtext(input$legend_title, at=-0.65, line=0)
    legend(-1, 1.10, legend=lab,fill= col.k, bty="n")
    dev.off()
  })


# output$downloadPlot <- downloadHandler(
#   filename = function() {
#     paste("map.jpg")
#   }, 
#   content = function(file) {
#     mymap <- plotInput()
#     jpeg(file, quality = 100, width = 800, height = 800)
#     plot(mymap)
#     dev.off()
#   }
# )




})