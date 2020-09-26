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


binary_cols = c("Gender (Cisgender only)" = "gender",
                "Dog/Cat" = "dog_or_cat",
                "Living with parents" = "live_with_parents",
                "Asthma" = "asthma",
                "Glasses/Contacts" = "glasses",
                "Dominant hand (Excluding ambidextrous)" = "dominant_hand")

interval_cols = c("University hours"="university_work",
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
        
        selectInput("test_type", "Testing Method", 
                    choices = c("Two-sample t-test")),
        
        selectInput("binary_dataset", "Binary Data", 
                    choices = binary_cols),
        
        selectInput("interval_dataset", "Nominal/Interval Data", 
                    choices = interval_cols)
    ),

    
    # Show a plot of the generated distribution
    mainPanel(
        h2(textOutput("question")),
        HTML('<br>'),
        tabsetPanel(
            #tabPanel("Question", h3(textOutput("question"))),
            tabPanel("Data Visualisation", div(style="width:90%;padding-left:1em",fluidRow(
                HTML("<br>"),
                h4("To determine whether the mean differs, we should first analyse the spread and location of both groups."),
                h4("This may also reveal outliers (hollow dots), which will be ignored during testing."),
                plotOutput("boxPlot")
            ))),
            tabPanel("Assumptions", div(style="width:90%;padding-left:1em",fluidRow(
                HTML("<br>"),
                h4("To perform a two-sample t-test, we require the assumption that both samples come from a normal distribution."),
                h4(textOutput("assumptionsText")),
                plotOutput("assumptionsPlot")
            ))),
            tabPanel("Statistical Test", div(style="width:80%;padding-left:1em",fluidRow(
                HTML("<br>"),
                h4("If the assumption of normality is satisfied, we can perform a two-sample t-test to check if the means differ."),
                HTML("<br>"),
                verbatimTextOutput("testOutput"),
                HTML("<br>"),
                h4(textOutput("testConclusion")),
                h4(textOutput("testConclusion2"), width="20%")
            )))
            #tabPanel("dwa", plotOutput("boxPlot"))
        )
        
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
        
        # print(input$binary_dataset)
        # print(names(which(binary_cols == input$binary_dataset)))
        # 
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
    
    output$assumptionsText <- renderPrint({
        if (input$binary_dataset == "" | input$interval_dataset == "") {
            cat("Please select both input variables...")
        } else {
            binary_name = names(which(binary_cols == input$binary_dataset))
            interval_name = names(which(interval_cols == input$interval_dataset))
            binary_name = (col_names_short[input$binary_dataset])
            interval_name = (col_names_short[input$interval_dataset])
            cat("We can approximately determine this by plotting", interval_name,"for both groups of", binary_name, "onto a QQ-plot.")
        }
    })
    
    output$assumptionsPlot <- renderPlot({
        if (input$binary_dataset == "" | input$interval_dataset == "") return()
        
        binary_name = names(which(binary_cols == input$binary_dataset))
        interval_name = names(which(interval_cols == input$interval_dataset))
        
        samples = df[!is.na(df[input$binary_dataset]) & !is.na(df[input$interval_dataset]),]
        
        binary_type_1 = unique(samples[input$binary_dataset])[1,]
        binary_type_2 = unique(samples[input$binary_dataset])[2,]
        
        samples_type_1 = samples[samples[input$binary_dataset] == binary_type_1,]
        samples_type_2 = samples[samples[input$binary_dataset] == binary_type_2,]
        
        samples_type_1_interval = unlist(samples_type_1[input$interval_dataset])
        samples_type_2_interval = unlist(samples_type_2[input$interval_dataset])
        samples_type_1_interval = samples_type_1_interval[!samples_type_1_interval %in% boxplot(samples_type_1_interval)$out]
        samples_type_2_interval = samples_type_2_interval[!samples_type_2_interval %in% boxplot(samples_type_2_interval)$out]
        

        par(mfrow=c(1,2))
        qqnorm(
            samples_type_1_interval, 
            main=paste("QQ-Plot of", col_names_short[input$interval_dataset], "for", col_names_short[input$binary_dataset], "=", binary_type_1), 
            col="dark green"
        )
        qqline(samples_type_1_interval)
        qqnorm(
            samples_type_2_interval, 
            main=paste("QQ-Plot of", col_names_short[input$interval_dataset], "for", col_names_short[input$binary_dataset], "=", binary_type_2), 
            col="blue"
        )
        qqline(samples_type_2_interval)
    })
    
    testResults <- reactive({  
        "Reacting"
        
        if (input$binary_dataset == "" | input$interval_dataset == "") return()
        
        binary_name = names(which(binary_cols == input$binary_dataset))
        interval_name = names(which(interval_cols == input$interval_dataset))
        samples = df[!is.na(df[input$binary_dataset]) & !is.na(df[input$interval_dataset]),]
        binary_type_1 = unique(samples[input$binary_dataset])[1,]
        binary_type_2 = unique(samples[input$binary_dataset])[2,]
        samples_type_1 = samples[samples[input$binary_dataset] == binary_type_1,]
        samples_type_2 = samples[samples[input$binary_dataset] == binary_type_2,]
        samples_type_1_interval = unlist(samples_type_1[input$interval_dataset])
        samples_type_2_interval = unlist(samples_type_2[input$interval_dataset])
        samples_type_1_interval = samples_type_1_interval[!samples_type_1_interval %in% boxplot(samples_type_1_interval)$out]
        samples_type_2_interval = samples_type_2_interval[!samples_type_2_interval %in% boxplot(samples_type_2_interval)$out]
        
        t.test(samples_type_1_interval, samples_type_2_interval)
    })
    
    output$testOutput <- renderPrint({
        if (input$binary_dataset == "" | input$interval_dataset == "") {
            cat("Please select both input variables...")
        } else {
            binary_name = names(which(binary_cols == input$binary_dataset))
            interval_name = names(which(interval_cols == input$interval_dataset))
            binary_name = (col_names_short[input$binary_dataset])
            interval_name = (col_names_short[input$interval_dataset])
            
            testResults()
        }
    })
    
    output$testConclusion <- renderPrint({
        if (input$binary_dataset == "" | input$interval_dataset == "") {
            cat("Please select both input variables...")
        } else {
            results = testResults()
            cat(
                "The t-test produces a test statistic of ", 
                unlist(results["statistic"]), 
                " and a p-value of ",
                unlist(results["p.value"]),
                ".",
                sep=""
            )
        }
    })
    
    output$testConclusion2 <- renderPrint({
        if (input$binary_dataset == "" | input$interval_dataset == "") {
            cat("Please select both input variables...")
        } else {
            results = testResults()
            reject = unlist(results["p.value"]) < 0.05
            if (unlist(results["p.value"]) < 0.05) {
                cat(
                    "As our p-value is less than 0.05, we reject the null hypothesis that the means are identical, and accept the alternative hypothesis that the means differ between ",
                    col_names_short[input$binary_dataset],
                    ".",
                    sep=""
                )
            } else {
                cat(
                    "As our p-value is greater than 0.05, we accept the null hypothesis that the means are identical, and reject the alternative hypothesis that the means differ between ",
                    col_names_short[input$binary_dataset],
                    ".",
                    sep=""
                )
            }
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
