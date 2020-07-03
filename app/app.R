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
            p("Weather data for each city are provided by NOAA and are of the last year of the time I lived in that city."),
            p("Click on Visualize for magic."),
            h4("Cities"),
            checkboxInput("hanoi_chek", "Hanoi, Vietnam", value = F),
            checkboxInput("stpeter_chek", "St. Peter, MN", value = F),
            checkboxInput("sf_chek", "San Francisco, CA", value = F),
            checkboxInput("oakland_chek", "Oakland, CA", value = F),
            checkboxInput("swarthmore_chek", "Swarthmore, PA", value = T),
            checkboxInput("victoria_chek", "Victoria, BC", value = T),
            actionButton("visualize", "Visualize"),
            sliderInput("months","Months", min=1, max=12, value = c(1, 12), dragRange = TRUE),
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
                theme(plot.title = element_text(hjust = 0.5)) +
                theme(legend.position = "none")
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
                geom_image(aes(image=c("~/git/weather-compare-viz/snowflake.png"))) +
                geom_text(hjust=-.15,vjust=.3) +
                xlim(0, 650) +
                ylim(0, 50) +
                labs(
                    x = 'Total snow fall over year (mm)',
                    y = 'Number of snow days over year',
                    title = 'Snow Fall'
                ) +
                theme(plot.title = element_text(hjust = 0.5))
            
            p = grid.arrange(prep_plot, snow_plot, ncol=2)  
            print(p)
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
