# Text Prediction Widget
# DS Module 10 / Capstone Project
# Paul Ringsted 4th March 2019
# server.R - server part

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
        
        session$onSessionEnded(stopApp)

        # Initialize variables to control the random walk        
        v <- reactiveValues(gen_walk = FALSE, gen_run = FALSE, gen_words = 0)
        
        observe({
        
                # Run the model, get top predictions
                dt_top_words <- ng_predict_app(input$txt,
                                               choices=input$num_top,
                                               max_length=v$gen_words-1,
                                               max_ngram=input$max_ngram)
                
                # If doing a "run", pick a random top result to prevent looping same phrases
                if (v$gen_run) {
                        idx <- sample(1:min(input$num_top,nrow(dt_top_words)),1)
                        new_word     <- dt_top_words[idx,]$next_word
                } else {
                        new_word     <- dt_top_words[1,]$next_word
                }
                new_sentence <- paste(str_squish(input$txt),new_word)
                new_sentence_marked <- paste0(str_squish(input$txt),
                                              " ","<mark>",new_word,"</mark>")
                
                # Render outputs
                output$next_word <- renderText({new_word})
                output$new_sentence <- renderText({new_sentence_marked})
                
                # If show stop is selected and not doing a "run" then build table
                if (input$show_top & !v$gen_run) {
                        output$top_words <- renderDataTable(
                                                dt_top_words,
                                                colnames=c("Suggestions","N-Gram Length",
                                                           "N-Gram Freq","N-Gram Prob","N-Gram Rank"),
                                                selection='single'
                                                )
                } else {
                        output$top_words <- NULL
                }
                
                if (!is.null(new_word)) {
                        
                        if (v$gen_run) {
                        
                                # Doing a run - extend sentence, triggers loop
                                updateTextInput(session, "txt", value = new_sentence)
                                
                        } else if (v$gen_walk) {
                        
                                # Doing a walk - extend sentence but isolated to prevent loop
                                isolate({updateTextInput(session, "txt", value = new_sentence)})
                                v$gen_walk <- FALSE
                        
                        }
                
                } else {
                        
                        # No word suggestion / reached limit - stop running
                        v$gen_walk  <- FALSE
                        v$gen_run   <- FALSE
                        v$gen_words <- 0
                }

        })
        
        observeEvent(
                
                # Clear button - reset the input box and top suggestion checkbox
                input$clear, { 
                        updateTextInput(session, "txt", value = "")
                        updateCheckboxInput(session, "show_top", value = TRUE)
                }
        )
        
        observeEvent(
                
                # Reset button - clear input and restore default parameters
                input$reset, {
                        updateTextInput(session, "txt", value = "")
                        updateSliderInput(session, "gen_words", value = 50)
                        updateSliderInput(session, "max_ngram", value = 5)
                        updateSliderInput(session, "num_top", value = 5)
                        updateCheckboxInput(session, "show_top", value = TRUE)
                }
        )
        
        observeEvent(
                
                # Walk button - trigger one word walk forward with isolated reaction
                input$walk, {
                        v$gen_walk <- TRUE
                }
        )
        observeEvent(
                
                # Run button - trigger random setence generation (hides table for performance)
                input$run, {
                        v$gen_run <- TRUE
                        v$gen_words <- input$gen_words
                        updateCheckboxInput(session, "show_top", value = FALSE)
                }
        )
        
        observeEvent(
                
                # Select a value from the grid to extend the sentence
                input$top_words_cell_clicked, {
                        new_word     <- input$top_words_cell_clicked$value
                        new_sentence <- paste(str_squish(input$txt),new_word)
                        isolate({updateTextInput(session, "txt", value = new_sentence)})
                }
        )
        
})