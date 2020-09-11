## Weather Compare Shiny App #
## Viet Dao ##################
## Jul 2, 2020 ###############

library(shiny)
library(ggplot2)
library(gridExtra)
library(lattice)
library(ggimage)
library(dplyr)

ui <- fluidPage(
    titlePanel("Weather Comparison Visualization", windowTitle = "Weather Comparison Visualization"),
    
    sidebarLayout(
        sidebarPanel(
            h4("Background"),
            p("I am fortunate to have lived in a few places throughtout my life, from my hometown of Hanoi, Vietnam to my next venture in Victoria, Canada. This visualization provides critical information when I need to plan for my subsequent moves."),
            p("Weather data for each city are provided by NOAA and except for Victoria, are of the last year of the time I lived in that city."),
            p("Click on Visualize for magic."),
            br(),
            h4("Cities"),
            checkboxInput("hanoi_chek", "Hanoi, Vietnam", value = T),
            checkboxInput("stpeter_chek", "St. Peter, MN", value = F),
            checkboxInput("sf_chek", "San Francisco, CA", value = F),
            checkboxInput("oakland_chek", "Oakland, CA", value = F),
            checkboxInput("swarthmore_chek", "Swarthmore, PA", value = F),
            checkboxInput("victoria_chek", "Victoria, BC", value = T),
            verbatimTextOutput("warning"),
            actionButton("visualize", "Visualize"),
            # sliderInput("months","Months", min=1, max=12, value = c(1, 12), dragRange = TRUE),
            p(" "),
            br(),
            tags$a(target="_blank", href="https://github.com/vietdao204/weather-compare-viz", "Developed by Viet Dao"),
            width = 3
        ),
        mainPanel(
            plotOutput("temp_plot"),
            plotOutput("snow_prep_plots")
        )
    )
)

server <- function(input, output) {
    observeEvent(input$visualize, {
        # show text warning if no checkbox is selected
        output$warning <- renderText({
            validate(
                need(input$hanoi_chek|input$stpeter_chek|input$sf_chek|input$oakland_chek|input$swarthmore_chek|input$victoria_chek, 
                     "Please select at least one city.")
            )
        })
        
        filtered_cities <- c()
        if (!input$hanoi_chek) {
            filtered_cities <- c(filtered_cities, 'HANOI')
        }
        if (!input$stpeter_chek) {
            filtered_cities <- c(filtered_cities, 'STPETER')
        }
        if (!input$sf_chek) {
            filtered_cities <- c(filtered_cities, 'SF')
        }
        if (!input$oakland_chek) {
            filtered_cities <- c(filtered_cities, 'OAKLAND')
        }
        if (!input$swarthmore_chek) {
            filtered_cities <- c(filtered_cities, 'SWARTHMORE')
        }
        if (!input$victoria_chek) {
            filtered_cities <- c(filtered_cities, 'VICTORIA')
        }
        
        # filter out city data based on checkbox selection
        temp_plot_data <- temp_plot_data %>% filter(!(CITY %in% filtered_cities))
        
        output$temp_plot <- renderPlot({
            ggplot(temp_plot_data, aes(x=WEEK, y=TMAX_mean, group=CITY, colour=CITY)) + 
                geom_ribbon(aes(ymin=TMIN_mean, ymax=TMAX_mean, fill=CITY), alpha=0.6, linetype=0) +
                scale_x_continuous(breaks = c(1, 14, 27, 40, 52),
                                   labels = c("1"="1 (Jan)", "14"="14 (Apr)", "27"="27 (Jul)", "40"="40 (Oct)", "52"="52 (Dec)")) + 
                scale_y_continuous(breaks = pretty(temp_plot_data$TMAX_mean, n = 5)) +
                labs(
                    x = 'Week',
                    y = 'Celcius',
                    title = 'Weekly Average Temperatures'
                ) +
                theme(plot.title = element_text(hjust = 0.5),
                      legend.title = element_blank(),
                      plot.margin = margin(t=4,1,1,1, "lines"),
                      legend.direction = "horizontal",
                      legend.position = c(0.5, 1.2))
        })
        
        output$snow_prep_plots <- renderPlot({
            prep_plot <- ggplot(temp_plot_data, aes(x=WEEK, y=PRCP_mean, group=CITY, colour=CITY)) +
                geom_ribbon(aes(ymin=0, ymax=PRCP_mean, fill=CITY), alpha=0.6, linetype=0) +
                scale_x_continuous(breaks = c(1, 14, 27, 40, 52),
                                   labels = c("1"="1 (Jan)", "14"="14 (Apr)", "27"="27 (Jul)", "40"="40 (Oct)", "52"="52 (Dec)")) + 
                scale_y_continuous(breaks = pretty(temp_plot_data$PRCP_mean, n = 5)) +
                labs(
                    x = 'Week',
                    y = 'mm',
                    title = 'Weekly Average Precipitation'
                ) +
                theme(plot.title = element_text(hjust = 0.5), 
                      legend.position = "none")
            
            snow_plot <- ggplot(snow_plot_data, aes(x=SNOW_TOTALS, y=SNOW_DAYS, label=CITY)) + 
                geom_point(size=3) +
                geom_image(aes(image=c("snowflake.png"))) +
                geom_text(hjust=-.15,vjust=.3) +
                xlim(0, 650) +
                ylim(0, 50) +
                labs(
                    x = 'Total snow fall (mm)',
                    y = 'Number of snow days',
                    title = 'Snow Fall Over Year'
                ) +
                theme(plot.title = element_text(hjust = 0.5))
            
            p = grid.arrange(prep_plot, snow_plot, ncol=2)  
            print(p)
        })
    })
}

shinyApp(ui = ui, server = server)
