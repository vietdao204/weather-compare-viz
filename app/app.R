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
library(gridExtra)
library(lattice)

dataset <- diamonds

ui <- fluidPage(
    titlePanel("Weather Comparison Visualization", windowTitle = "Weather Comparison Visualization"),
    
    sidebarLayout(
        
        # Sidebar with a slider input
        sidebarPanel(
            h4("Data to display"),
            checkboxInput("fullData", "Use full data", value = F),
            checkboxInput("hanoi", "Hanoi, Vietnam", value = T),
            checkboxInput("stpeter", "St. Peter, MN", value = T),
            checkboxInput("sf", "San Francisco, CA", value = T),
            checkboxInput("oakland", "Oakland, CA", value = T),
            checkboxInput("swarthmore", "Swarthmore, PA", value = T),
            checkboxInput("victoria", "Victoria, BC", value = T),
            checkboxInput("leuven", "Leuven, Belgium", value = T),
            h4("Time of Year"),
            sliderInput("months","Months", min=1, max=12, value = c(1, 3), dragRange = TRUE),
            width = 3
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot")
        )
    ),
    
    fluidRow(
        column(3,
               h4("Diamonds Explorer"),
               sliderInput('sampleSize', 'Sample Size', 
                           min=1, max=nrow(dataset),
                           value=min(1000, nrow(dataset)), 
                           step=500, round=0),
               br(),
               checkboxInput('jitter', 'Jitter'),
               checkboxInput('smooth', 'Smooth')
        ),
        column(4, offset = 1,
               selectInput('x', 'X', names(dataset)),
               selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
               selectInput('color', 'Color', c('None', names(dataset)))
        ),
        column(4,
               selectInput('facet_row', 'Facet Row',
                           c(None='.', names(diamonds[sapply(diamonds, is.factor)]))),
               selectInput('facet_col', 'Facet Column',
                           c(None='.', names(diamonds[sapply(diamonds, is.factor)])))
        )
    )
)

server <- function(input, output) 
{
    dataset <- reactive({
        diamonds[sample(nrow(diamonds), input$sampleSize),]
    })
    
    output$plot <- renderPlot({
        
        p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
        
        if (input$color != 'None')
            p <- p + aes_string(color=input$color)
        
        facets <- paste(input$facet_row, '~', input$facet_col)
        if (facets != '. ~ .')
            p <- p + facet_grid(facets)
        
        if (input$jitter)
            p <- p + geom_jitter()
        if (input$smooth)
            p <- p + geom_smooth()
        # p = grid.arrange(p1, p2, p3, p4, ncol=2)  
        print(p)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
