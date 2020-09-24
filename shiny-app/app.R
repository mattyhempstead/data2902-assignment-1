#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bomrang)
library(ggplot2)
library(plotly)
library(shinyjs)

# rsconnect::deployApp('.')


y_var_choices = c("Apparent temperatureddw" = "apparent_t",
                  "Dew point" = "dewpt")

binary_cols = c("","Gender (Cisgender only)" = "gender",
                "Dog/Cat" = "dog_or_cat",
                "Living with parents" = "live_with_parents",
                "Asthma" = "asthma",
                "Glasses/Contacts" = "glasses",
                "Dominant hand (Excluding ambidextrous)" = "dominant_hand")

interval_cols = c("","University hours"="university_work",
                  "Exercising hours"="exercising",
                  "Paid work hours"="paid_work",
                  "Height (cm)"="height",
                  "Stress rating (1-10)"="stress_level",
                  "Covid tests"="covid_test")

col_names_short = c("university_work" = "University Hours",
                    "gender" = "Gender",
                    "dog_or_cat" = "Dog/Cat",
                    "asthma" = "Asthma",
                    "glasses" = "Glasses/Contacts",
                    "dominant_hand" = "Dominant Hand",
                    "exercising" = "Exercising Hours",
                    "paid_work" = "Paid Work Hours",
                    "stress_level" = "Stress rating",
                    "covid_test" = "Covid Tests",
                    "height" = "Height",
                    "live_with_parents" = "Living with Parents")

df = read.csv("./clean.csv")
#write.csv(df, "./clean.csv")


# Define UI for application that draws a histogram
ui <- pageWithSidebar(
    # Application title
    headerPanel("DATA2902 Assignment 1"),
    
    # Sidebar with a slider input for number of observations
    sidebarPanel(
        useShinyjs(),
        
        # sliderInput("obs",
        #             "Number of observations:",
        #             min = 1,
        #             max = 1000, 
        #             value = 500),
        
        selectInput("test_type", "Testing Method", 
                    choices = c("Two-sample t-test", "Chi-squared test")),
        
        selectInput("binary_dataset", "Binary Data", 
                    choices = binary_cols),
        
        selectInput("interval_dataset", "Nominal/Interval Data", 
                    choices = interval_cols)
    ),

    
    # Show a plot of the generated distribution
    mainPanel(
        h3(textOutput("question")),
        plotOutput("boxPlot")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    observeEvent(input$test_type, {
        if(input$test_type == "Two-sample t-test"){
            shinyjs::enable("binary_dataset")
        }else{
            shinyjs::disable("binary_dataset")
        }
    })
    
    #"Does the mean X change with Y"
    
    # Generate a summary of the dataset
    output$question <- renderPrint({
        if (input$binary_dataset == "" | input$interval_dataset == "") {
            cat("Please select both input variables...")
        } else {
            binary_name = names(which(binary_cols == input$binary_dataset))
            interval_name = names(which(interval_cols == input$interval_dataset))
            binary_name = toupper(col_names_short[input$binary_dataset])
            interval_name = toupper(col_names_short[input$interval_dataset])
            cat("Does the mean ", interval_name, " change with ", binary_name, "?", sep="")            
        }
    })
    
    output$boxPlot <- renderPlot({
        if (input$binary_dataset == "" | input$interval_dataset == "") return()
        
        binary_name = names(which(binary_cols == input$binary_dataset))
        interval_name = names(which(interval_cols == input$interval_dataset))

        samples = df[!is.na(df[input$binary_dataset]) & !is.na(df[input$interval_dataset]),]
        
        binary_type_1 = unique(samples[input$binary_dataset])[1,]
        binary_type_2 = unique(samples[input$binary_dataset])[2,]
        
        samples_type_1 = samples[samples[input$binary_dataset] == binary_type_1,]
        samples_type_2 = samples[samples[input$binary_dataset] == binary_type_2,]
        
        print(input$binary_dataset)
        print(names(which(binary_cols == input$binary_dataset)))
        
        bplot = boxplot(
            unlist(samples_type_1[input$interval_dataset]),
            unlist(samples_type_2[input$interval_dataset]),
            names=c(binary_type_1, binary_type_2),
            horizontal = T,
            xlab=interval_name,
            ylab=col_names_short[input$binary_dataset],
            main=paste("Boxplot of", 
                       toupper(col_names_short[input$interval_dataset]), 
                       "grouped by", 
                       toupper(col_names_short[input$binary_dataset])),
            col=c("light blue","light green")
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
