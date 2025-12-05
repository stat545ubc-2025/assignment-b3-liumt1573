#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
main_data <- iris

# The UI
ui <- fluidPage(

    # Creating a Title
    titlePanel("Flower Iris Measurements"),
    
    sidebarLayout(
        sidebarPanel(
          strong("Description"),
          p("This is a Rshiny app that produce a custom histogram and a 
            custom table for the built-in IRIS dataset. The histogram in
            question is a histogram of \'Sepal.Width\', with the user able to 
            specify the species to be included and the number of breaks.
            The app also produce a table showcaseing all five measurements 
            for each observation, with the user able to filter the table by 
            imposing a maximum and minimum on \'Sepal.Length\'. Lastly, the user 
            is also able to specify whether or not they wish for the  table to 
            be sorted in ascending order by \'Sepal.Length\'. They, too, can also
            specify whether the histogram should plot the emperical density or 
            the frequency."),
          radioButtons("hist_spec",
                       "Speices to include in histogram",
                       choices = c("versicolor", "setosa", "virginica"),
                       selected = "virginica"
                       ),
          sliderInput("hist_bin",
                      "Number of breaks for the histogram",
                      min = 1,
                      max = 15,
                      value = 10
                      ),
          sliderInput("table_spec",
                        "Range for \'Sepal.Length\' in table",
                        min = 4.3,
                        max = 7.9,
                        value = c(5.1, 6.2), post = "cm"
                        ),
          checkboxInput("sorting_YN", 
                        "Sort table in ascending order by \'Sepal.Length\'? ",
                        value = FALSE
                        ),
          checkboxInput("prob_YN", 
                        "Show density as opposed to frequency?",
                        value = FALSE
                        ),
          downloadButton('download',"Download the table")
          ),

        # Show a plot of the generated distribution
        mainPanel(
           strong("Below is the output plot"),
           plotOutput("plot_out"),
           br(),br(),
           strong("Below is the output table"),
           tableOutput("table_out")
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot_out <- renderPlot({
        plot_data <- filter(main_data, Species == input$hist_spec)
        hist(plot_data$Sepal.Width, breaks = input$hist_bin, prob = input$prob_YN, col = 'skyblue', border = 'white',
             xlab = 'Sepal Width (cm)',
             main = 'Histogram of Sepal Width for various flowers')
    })
    
    output$table_out <- renderTable({
      table_data <- filter(main_data, Sepal.Length <= input$table_spec[2] & 
                                       Sepal.Length >= input$table_spec[1])
      ifelse(input$sorting_YN, table_data <- arrange(table_data, Sepal.Length), table_data <- table_data)
      table_data
    })
    
    downloadable <- reactive({
      filter(arrange(main_data, Sepal.Length), Sepal.Length <= input$table_spec[2] & 
               Sepal.Length >= input$table_spec[1])
    })
    
    output$dto <- renderDataTable(downloadable())
    output$download <- downloadHandler(
      filename = function(){"iris_app.csv"}, 
      content = function(fname){
        write.csv(downloadable(), fname)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
