#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
    titlePanel("Rate die Parameter der linearen Regression"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                "n",
                "Stichprobengröße",
                value = 10,
                min = 5,
                max = 20,
                step = 1
            ),
            sliderInput(
                "b",
                "Steigung",
                value = 1,
                min = 0,
                max = 2,
                step = 0.05
            ),
            sliderInput(
                "c",
                "Interzept",
                value = 1,
                min = 0,
                max = 2,
                step = 0.05
            ),
            shiny::checkboxInput(
                inputId = "show",
                "Zeige echte Regressionsgerade",
                value = F
            )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(plotOutput("distPlot"))
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    data_set <- reactive({
        df_sigma <- matrix(c(2, 1, 1, 2), nrow = 2)
        df <-
            MASS::mvrnorm(input$n, c(3, 3), df_sigma, empirical = T) %>% data.frame()
        df
    })
    
    my_model <- reactive({
        lm(data = data_set(), X2 ~ X1)
    })
    
    plotformatter <- list(
        aes(X1, X2),
        geom_point(na.rm = T),
        labs(x = "X Variable", y = "Y Variable"),
        coord_cartesian(xlim = c(-.5, 6.5), ylim = c(-.5, 6.5)), 
        labs(title = "Lineare Regression")
    )
    
    output$distPlot <- renderPlot({
        model <- my_model()
        
        coef_c <- model$coefficients[1]
        coef_b <- model$coefficients[2]

        data_set() %>% ggplot() +
            geom_abline(slope = input$b,# coef_b,
                        intercept = input$c, #coef_c,
                        color = "blue") +
            plotformatter +
            NULL -> p
        
        if (input$show) {
            lr <-  geom_abline(slope = coef_b,
                               intercept = coef_c,
                               color = "red")
            p <- p + lr
        }
        
        p2 <- p
        sum_y <- 0
        for (i in 1:input$n) {
            x1 <- data_set()[i, 1]
            y1 <- data_set()[i, 2]
            
            
            y2 <- x1 * input$b + input$c
            ydiff <- y1 - y2
            sum_y <- sum_y + (ydiff * ydiff)
            
            p2 <- p2 + geom_line(
                data = data.frame(x = c(x1, x1), y = c(y1, y2)),
                aes(
                    x = x,
                    y = y,
                    group = i
                ),
                color = "blue",
                inherit.aes = F
            )
            p2 <-
                p2 + geom_label(
                    label = paste("", round(ydiff * ydiff, 2)),
                    x = x1,
                    y = y2 + (y1 - y2) / 2
                )
        }
        p3 <-
            p2 + geom_label(
                label = paste("Quadratische Abweichung:", round(sum_y, 2)),
                x = 5,
                y = 1,
                size = 8
            )
        p3
    })
}

# Run the application
shinyApp(ui = ui, server = server)
