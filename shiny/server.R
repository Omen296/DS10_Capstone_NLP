# Text Prediction Widget
# DS Module 10 / Capstone Project
# Paul Ringsted 4th March 2019
# server.R - server part

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

        # Initialize variables to control the random walk        
        v <- reactiveValues(gen_random = FALSE, gen_words = 0)
        
        observe({
        
                # Run the model, get top predictions
                dt_top_words <- ng_predict(input$txt,verbose=FALSE,choices=input$num_top,max_length=v$gen_words)
                new_word <- dt_top_words[1,]$next_word
                new_sentence <- paste(input$txt,new_word)
                
                #next_word <- ng_predict(input$txt,verbose=FALSE,choices=0)
                
                # Render outputs
                output$next_word <- renderText({new_word})
                output$new_sentence <- renderText({new_sentence})
                if (input$show_top & !v$gen_random) {
                        output$top_words <- renderDataTable(
                                                dt_top_words,
                                                colnames=c("Suggestion","N-Gram Length","N-Gram Freq","N-Gram Prob","N-Gram Rank")
                                                )
                } else {
                        output$top_words <- NULL
                }
                
                if (!is.null(new_word) & v$gen_random) {
                        
                                # Extend sentence, triggers loop
                                updateTextInput(session, "txt", value = new_sentence)
                        
                } else {
                        
                                # No word suggestion / reached limit - stop generating
                                v$gen_random <- FALSE
                                v$gen_words <- 0
                }

        })
        
        observeEvent(
                input$clear, { 
                        updateTextInput(session, "txt", value = "")
                        updateCheckboxInput(session, "show_top", value = TRUE)
                }
        )

        observeEvent(
                input$random, {
                        # Trigger random setence generation (hides table for performance)
                        v$gen_random <- TRUE
                        v$gen_words <- input$gen_words
                        updateCheckboxInput(session, "show_top", value = FALSE)
                }
        )
        
})