# Text Prediction Widget
# DS Module 10 / Capstone Project
# Paul Ringsted 4th March 2019
# ui.R - UI part

# Define simple UI for application that runs text preduction and displays next word
shinyUI(fluidPage(tabsetPanel(

        tabPanel("App",
                h2("NexText"),
                h5("Text Prediction Widget"),
                
                sidebarLayout(
                        
                        # Sidebar with a inputs for park and spline fit
                        sidebarPanel(
                                sliderInput("gen_words", label=h5("Max words for Run"), value=50,min=5,max=200,step=5),
                                hr(),
                                sliderInput("max_ngram",label=h5("Max N-Gram Length"),value=5,min=2,max=5),
                                hr(),
                                checkboxInput("show_top", label = h5("Show top suggestions"), value=TRUE),
                                sliderInput("num_top",label=h5("Number of top suggestions"),value=5,min=1,max=10)
                        ),
                        
                        mainPanel(
                                
                                # Get phrase from the user
                                
                                textInput("txt",label = h3("Type something..."),placeholder="feed me words"),
                                htmlOutput("new_sentence"),
                                hr(),
                                actionButton("clear",label="Clear"),
                                actionButton("reset",label="Reset"),
                                actionButton("walk",label="Walk"),
                                actionButton("run",label="Run"),
                                hr(),
                                
                                # Show predicted next word next sentence and table
                                
                                h4("Suggestion:"),
                                fluidRow(column(5,verbatimTextOutput("next_word"))),
                                fluidRow(column(12,dataTableOutput("top_words")))
                        )
                )
        ),
        
        tabPanel("Help",
                h2("NexText"),
                h5("Text Prediction Widget"),
                hr(),
                includeMarkdown("NexTextHelp.md")
        )
)))
