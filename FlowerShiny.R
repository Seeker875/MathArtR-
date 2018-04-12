library(shiny)
library(ggplot2)


# UI
ui <- fluidPage(
    headerPanel(h1("Mathematical Art",style="color:Black")),
    
    sidebarLayout(
      sidebarPanel(
        
        tags$style(".well {background-color:White;}"),
        
        sliderInput('angle','Angle',min=1,max=50,value=10,step=1),
        
        sliderInput('sz','Size',min=1,max=5,value=3,step=0.5),
        
        #sliderInput('shp','Shape',min=1,max=16,value=16,step=1),
        
        selectInput(inputId = "col", 
                    label = "Color:",
                    choices = c("Green", "Blue", "Red", "Orange","Pink","Black"), 
                    selected = "Red"),
        
        selectInput(inputId = "shp", 
                    label = "Shape of Points:",
                    choices = c("Circle", "Square", "Triangle"), 
                    selected = "Circle")
        
      ),
     
    
      mainPanel(
        plotOutput(outputId = "plotBw")
      )  
    
    )
    
  
)

server <- function(input, output) {
  
  
  df <- reactive({
    points <- 500
    t <- (1:points) * input$angle
    x <- sin(t)
    y <-cos(t)
    
    k <- (1:points) * input$angle+5
    x1 <- sin(k)
    y1 <-cos(k)
    
    
    df <- data.frame(t, x, y, k, x1, y1)
    return(df)
    })
  
  shp <- reactive(
    
    if (input$shp=='Circle') {
      return(20)
    } else if(input$shp=='Square'){
      return(15)
    } else{
      return(17)
    }
  )
  
  
  # Create scatterplot object the plotOutput function is expecting
  output$plotBw <- renderPlot(ggplot(df(), aes(x*t, y*t)) +
                                geom_point(alpha =  0.7,color=input$col,size=input$sz,shape=shp())+
                                geom_point(aes((x1*k), (y1*k)),alpha =  0.4,color= input$col)+
                                theme(panel.background = element_rect(fill = "white"),panel.grid=element_blank(),
                                                                              axis.title=element_blank(),axis.text=element_blank(),
                                                                              axis.ticks=element_blank()))
}


shinyApp(ui = ui, server = server)
