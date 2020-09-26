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
library(tableHTML)

# rsconnect::deployApp('.')

test_types = c("",
               "Two-sample t-test"="t-test",
               "Fisher's exact test"="fisher")

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
                    choices = test_types),
        
        conditionalPanel(
            condition = "input.test_type != ''",
            selectInput(
                "binary_dataset", 
                "Binary Dataset", 
                choices = binary_cols
            )
        ),
        
        conditionalPanel(
            condition = "input.test_type == 't-test'",
            selectInput(
                "interval_dataset", 
                "Nominal/Interval Dataset", 
                choices = interval_cols
            )
        ),
        
        conditionalPanel(
            condition = "input.test_type == 'fisher'",
            selectInput(
                "binary2_dataset", 
                "Binary Dataset (Secondary)",
                choices = binary_cols,
                selected = binary_cols[2]
            )
        )
            
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        h2(style="text-align:center; font-weight:bold", textOutput("question")),
        HTML('<br>'),
        conditionalPanel(
            condition = "input.test_type == 't-test'",
            tabsetPanel(
                tabPanel("Data Visualisation", div(style="width:90%;padding-left:1em",fluidRow(
                    HTML("<br>"),
                    h4("To determine whether the mean differs, we should first analyse the spread and location of both groups."),
                    h4("This may also reveal outliers (hollow dots), which will be ignored during testing."),
                    plotOutput("boxPlot")
                ))),
                tabPanel("T-Test", div(style="width:90%;padding-left:1em",fluidRow(
                    HTML("<br>"),
                    HTML("<h4><b>H<sub>0</sub></b>: The true means of both groups are identical.</h4>"),
                    HTML("<h4><b>H<sub>1</sub></b>: The true means of both groups differ.</h4>"),
                    HTML("<br>"),
                    h4("To perform a two-sample t-test, we require the assumption that both samples come from a normal distribution."),
                    h4(textOutput("assumptionsText")),
                    plotOutput("assumptionsPlot")
                ))),
                tabPanel("Results", div(style="width:90%;padding-left:1em",fluidRow(
                    HTML("<br>"),
                    h4("Given the assumptions are satisfied, we can perform a two-sample t-test to check if the means differ."),
                    HTML("<br>"),
                    verbatimTextOutput("testOutput"),
                    HTML("<br>"),
                    h4(textOutput("testConclusion")),
                    h4(textOutput("testConclusion2"), width="20%")
                )))
            )
        ),
        conditionalPanel(
            condition = "input.test_type == 'fisher'",
            tabsetPanel(
                tabPanel("Data Visualisation", div(style="width:90%;padding-left:1em",fluidRow(
                    HTML("<br>"),
                    h4("We first visualise the 2x2 contingency to find the observed odds ratio."),
                    HTML("<br>"),
                    tableHTML_output("fisherContingencyTable"),
                    HTML("<br>"),
                    h4(textOutput("fisherOddsRatio"))
                ))),
                tabPanel("Fisher's Exact Test", div(style="width:90%;padding-left:1em",fluidRow(
                    HTML("<br>"),
                    HTML("<h4><b>H<sub>0</sub></b>: The odds ratio is equal to 1 (the groups are independant)</h4>"),
                    HTML("<h4><b>H<sub>1</sub></b>: The odds ratio is not equal to 1 (the groups are dependant)</h4>"),
                    HTML("<br>"),
                    h4("To perform a Fisher's Exact Test, we require the assumption that all samples are iid.")
                ))),
                tabPanel("Results", div(style="width:90%;padding-left:1em",fluidRow(
                    HTML("<br>"),
                    h4("Given the assumptions are satisfied, we can perform a Fisher's Exact Test to determine if the odds ratio equals 1."),
                    HTML("<br>"),
                    verbatimTextOutput("fisherTestOutput"),
                    HTML("<br>"),
                    h4(textOutput("fisherTestConclusion")),
                    h4(textOutput("fisherTestConclusion2"), width="20%")
                )))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Generate a summary of the dataset
    output$question <- renderPrint({
        if (input$test_type == "") {
            cat("Please select a testing method.")
        } else if (input$test_type == "t-test") {
            binary_name = names(which(binary_cols == input$binary_dataset))
            interval_name = names(which(interval_cols == input$interval_dataset))
            binary_name = toupper(col_names_short[input$binary_dataset])
            interval_name = toupper(col_names_short[input$interval_dataset])
            cat("Does the mean ", interval_name, " change with ", binary_name, "?", sep="")            
        } else {
            binary_name = names(which(binary_cols == input$binary_dataset))
            binary2_name = names(which(binary_cols == input$binary2_dataset))
            binary_name = toupper(col_names_short[input$binary_dataset])
            binary2_name = toupper(col_names_short[input$binary2_dataset])
            cat("Are ", binary_name, " and ", binary2_name, " independent?", sep="")  
        }
    })
    
    output$boxPlot <- renderPlot({
        binary_name = names(which(binary_cols == input$binary_dataset))
        interval_name = names(which(interval_cols == input$interval_dataset))

        samples = df[!is.na(df[input$binary_dataset]) & !is.na(df[input$interval_dataset]),]
        
        binary_type_1 = unique(samples[input$binary_dataset])[1,]
        binary_type_2 = unique(samples[input$binary_dataset])[2,]
        
        samples_type_1 = samples[samples[input$binary_dataset] == binary_type_1,]
        samples_type_2 = samples[samples[input$binary_dataset] == binary_type_2,]
        
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
        binary_name = names(which(binary_cols == input$binary_dataset))
        interval_name = names(which(interval_cols == input$interval_dataset))
        binary_name = (col_names_short[input$binary_dataset])
        interval_name = (col_names_short[input$interval_dataset])
        cat("We can approximately determine this by plotting", interval_name,"for both groups of", binary_name, "onto a QQ-plot.")
    })
    
    output$assumptionsPlot <- renderPlot({
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
        testResults()
    })
    
    output$testConclusion <- renderPrint({
        results = testResults()
        cat(
            "The t-test produces a test statistic of ", 
            unlist(results["statistic"]), 
            " and a p-value of ",
            unlist(results["p.value"]),
            ".",
            sep=""
        )
    })
    
    output$testConclusion2 <- renderPrint({
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
    })
    
    output$fisherOddsRatio <- renderPrint({
        binary_name = names(which(binary_cols == input$binary_dataset))
        binary2_name = names(which(binary_cols == input$binary2_dataset))
        samples = df[!is.na(df[input$binary_dataset]) & !is.na(df[input$binary2_dataset]),]
        
        binary_type_1 = unique(samples[input$binary_dataset])[1,]
        binary_type_2 = unique(samples[input$binary_dataset])[2,]
        binary2_type_1 = unique(samples[input$binary2_dataset])[1,]
        binary2_type_2 = unique(samples[input$binary2_dataset])[2,]
        
        samples_binary = unlist(samples[input$binary_dataset])
        samples_binary2 = unlist(samples[input$binary2_dataset])
        
        con_table = table(samples_binary, samples_binary2)
        con_table = con_table[2:1, 2:1]
        
        cat("Using the table above, the observed odds ratio is ", (con_table[1]*con_table[4]) / (con_table[2]*con_table[3]), ".", sep="")
    })
    
    output$fisherContingencyTable <- render_tableHTML({
        binary_name = names(which(binary_cols == input$binary_dataset))
        binary2_name = names(which(binary_cols == input$binary2_dataset))
        samples = df[!is.na(df[input$binary_dataset]) & !is.na(df[input$binary2_dataset]),]
        
        binary_type_1 = unique(samples[input$binary_dataset])[1,]
        binary_type_2 = unique(samples[input$binary_dataset])[2,]
        binary2_type_1 = unique(samples[input$binary2_dataset])[1,]
        binary2_type_2 = unique(samples[input$binary2_dataset])[2,]
        
        samples_binary = unlist(samples[input$binary_dataset])
        samples_binary2 = unlist(samples[input$binary2_dataset])
        
        con_table = table(samples_binary, samples_binary2)
        con_table = con_table[2:1, 2:1]
        
        
        con_df = data.frame(con_table[1:2],con_table[3:4], row.names=rownames(con_table))
        names(con_df) = colnames(con_table)
        con_df["Total",] = c(sum(con_df[1]), sum(con_df[2]))
        con_df["Total"] = c(sum(con_df[1,]), sum(con_df[2,]), sum(con_df[3,]))
        print(con_df)
        
        tableHTML(
            con_df, 
            second_headers = list(c(1,1,3), c("","", col_names_short[input$binary2_dataset])),
            row_groups=list(c(3),c(col_names_short[input$binary_dataset])),
            widths = c(100, 100, 100, 100, 100)
        ) %>% 
            #add_css_row(css = list('background-color', '#f2f2f2'), rows = c(1,2))
            #add_css_row(css=list("margin", "10px")) %>%
            add_css_column(css=list("font-weight", "bold"), columns=c("rownames","row_groups")) %>%
            add_css_row(css=list("text-align", "center"), rows=1:5) %>%
            add_css_header(css=list("text-align", "center"), headers=1:5) %>%
            add_css_second_header(css=list("text-align", "center"), second_headers=1:5)
            
        
    })
    
    fisherTestResults <- reactive({  
        binary_name = names(which(binary_cols == input$binary_dataset))
        binary2_name = names(which(binary_cols == input$binary2_dataset))
        samples = df[!is.na(df[input$binary_dataset]) & !is.na(df[input$binary2_dataset]),]
        binary_type_1 = unique(samples[input$binary_dataset])[1,]
        binary_type_2 = unique(samples[input$binary_dataset])[2,]
        binary2_type_1 = unique(samples[input$binary2_dataset])[1,]
        binary2_type_2 = unique(samples[input$binary2_dataset])[2,]
        samples_binary = unlist(samples[input$binary_dataset])
        samples_binary2 = unlist(samples[input$binary2_dataset])
        con_table = table(samples_binary, samples_binary2)
        con_table = con_table[2:1, 2:1]
        
        fisher.test(con_table)
    })
    
    output$fisherTestOutput <- renderPrint({
        fisherTestResults()
    })
    
    
    output$fisherTestConclusion <- renderPrint({
        results = fisherTestResults()
        cat(
            "The t-test produces a 95% confidence interval for the odds ratio of [", 
            unlist(results["conf.int"])[1],
            ", ",
            unlist(results["conf.int"])[2],
            "] and a p-value of ",
            unlist(results["p.value"]),
            ".",
            sep=""
        )
    })
    
    output$fisherTestConclusion2 <- renderPrint({
        results = fisherTestResults()
        reject = unlist(results["p.value"]) < 0.05
        if (unlist(results["p.value"]) < 0.05) {
            cat("As our p-value is less than 0.05, we reject the null hypothesis that the odds ratio is 1, and accept the alternative hypothesis that the odds ratio is not 1.")
        } else {
            cat("As our p-value is greater than 0.05, we accept the null hypothesis that the odds ratio is 1, and reject the alternative hypothesis that the odds ratio is not 1.")
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
