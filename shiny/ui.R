# Text Prediction Widget
# DS Module 10 / Capstone Project
# Paul Ringsted 4th March 2019
# ui.R - UI part

# Define simple UI for application that runs text preduction and displays next word
shinyUI(fluidPage(

        # Application title
        titlePanel("NexText *** PROTOTYPE ***"),
        h3("Text Prediction Widget"),

        sidebarLayout(
                
                # Sidebar with a inputs for park and spline fit
                sidebarPanel(
                        checkboxInput("show_top", label = h5("Show top suggestions"), value=TRUE),
                        numericInput("num_top",label=h5("Number of top suggestions"),value=5,min=1),
                        numericInput("gen_words", label=h5("Generate Sentence - Max words:"), value=30,min=1,max=100)
                ),
                
                mainPanel(
                        
                        # Get phrase from the user
                        
                        textInput("txt",label = h3("Type something...")),
                        fluidRow(column(12,verbatimTextOutput("new_sentence"))),
                        actionButton("clear",label="Clear"),
                        actionButton("random",label="Generate Sentence"),
                        hr(),
                        
                        # Show predicted next word next sentence and table
                        
                        h4("Next word:"),
                        fluidRow(column(5,verbatimTextOutput("next_word"))),
                        hr(),
                        fluidRow(column(12,dataTableOutput("top_words")))
                )
        )
))
