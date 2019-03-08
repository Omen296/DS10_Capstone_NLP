#################################################################################################
## Project	: Module 10 / Capstone - Text Prediction
## Author	: Paul Ringsted
## Date		: 2018-02-24
## Function	: ng_predict_app.R
##		: Function for apps to perform prediction lookup against n-gram list
##		: Removed logging for optimization
##		: Assumes existence of objects ng (dt of ngrams) and dic (dt of dictionary)
#################################################################################################

library(data.table)
library(quanteda)

ng_predict_app <- function(phrase,choices=1,max_length=0,max_ngram=5) {

	# Convert the phrase with same parameters as n-gram model
	tok <- tokens(char_tolower(phrase),
				remove_symbols=TRUE,
				remove_punct=TRUE,
				remove_twitter=TRUE,
				remove_numbers=TRUE,
				remove_url=TRUE)

	# Convert result to datatable and check there is valid sequence left
	words <- as.data.table(as.character(tok))
	colnames(words)<-"feature"

	num_words <- words[,.N]
	if (max_length > 0 & num_words >= max_length)
		return()				# Hook for UI to gen sentence without blowing up

	max_to_try <- min(max_ngram-1,num_words)
	if (max_to_try == 0)
		return()

	# Convert these words to reverse list of dic IDs
	words[,word_order:=1:.N]			# preserve order for reference
	setkey(words,feature)				# set up for lookup
	setkey(dic,feature)
	words[,word_id:=dic[words,rank]]		# lookup ids
	word_ids<-words[order(-word_order),word_id]	# go in reverse order

	# Loop from longest possible n-gram to 1 and loop over ngram lists looking for match
	next_word_res <- as.data.table(NULL)

	for (i in max_to_try:0) {

		# Get all the matching n-grams for this level i.e. for i=1 we want all bigrams which
		# have first index as the first word id (last word in phrase), etc.

		if (i > 0) {
			next_word_info <- switch(
				i,
				ng[ngram_len==2 & idx1==word_ids[1], 
					.(idx2,ngram_len,ngram_freq,ngram_prob,ngram_prob_rank)],
				ng[ngram_len==3 & idx1==word_ids[2] & idx2==word_ids[1], 
					.(idx3,ngram_len,ngram_freq,ngram_prob,ngram_prob_rank)],
				ng[ngram_len==4 & idx1==word_ids[3] & idx2==word_ids[2] & idx3==word_ids[1], 
					.(idx4,ngram_len,ngram_freq,ngram_prob,ngram_prob_rank)],
				ng[ngram_len==5 & idx1==word_ids[4] & idx2==word_ids[3] & idx3==word_ids[2] & idx4==word_ids[1], 
					.(idx5,ngram_len,ngram_freq,ngram_prob,ngram_prob_rank)]
			)
		} else {
			# Return the 1-grams as default
			next_word_info <- 
				ng[ngram_len==1,
					.(idx1,ngram_len,ngram_freq,ngram_prob,ngram_prob_rank)]
		}

		colnames(next_word_info)[1] <- "idx"
		setkey(next_word_info,idx)

		# Lookup the word string from the dictionary
		setkey(dic,rank)
		next_word_info[,next_word:=dic[next_word_info,feature]]

		# Consolidate results list
		next_word_res <- rbind(next_word_res,next_word_info)
	}
	
        if (choices<0) {

                # Return first -1*choices words with highest ngram length and best rank
		# Supports benchamark program and quiz runs
                next_word_res[order(-ngram_len,ngram_prob_rank)][1:(-1*choices),next_word]

        } else {

                # Return data table with top # of choices
                next_word_res[order(-ngram_len,ngram_prob_rank)][
                        1:choices,.(next_word,ngram_len,ngram_freq,round(ngram_prob,5),ngram_prob_rank)]
        }
	
}
