#################################################################################################
## Project	: Module 10 / Capstone - Text Prediction
## Author	: Paul Ringsted
## Date		: 2018-02-24
## Module	: ng.R
##		: Functions to perform prediction lookup against n-gram list
##		: Assumes existence of objects ng (dt of ngrams) and dic (dt of dictionary)
#################################################################################################

library(data.table)
library(quanteda)


wk3_test <- function(remove_stopwords=FALSE) {

	phrases<-c(
		"The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
		"You're the reason why I smile everyday. Can you follow me please? It would mean the",
		"Hey sunshine, can you follow me and make me the",
		"Very early observations on the Bills game: Offense still struggling but the",
		"Go on a romantic date at the",
		"Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
		"Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
		"After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
		"Be grateful for the good times and keep the faith during the",
		"If this isn't the cutest thing you've ever seen, then you must be"
	)

	answers <- NULL
	for (phrase in phrases) {
		answers <- c(answers,ng_predict(phrase,remove_stopwords,choices=20)[1,1])
	}

	print(transpose(answers))
}

wk4_test <- function(remove_stopwords=FALSE) {

	phrases<-c(
		"When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
		"Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
		"I'd give anything to see arctic monkeys this",
		"Talking to your mom has the same effect as a hug and helps reduce your",
		"When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
		"I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
		"I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
		"Every inch of you is perfect from the bottom to the",
		"I’m thankful my childhood was filled with imagination and bruises from playing",
		"I like how the same people are in almost all of Adam Sandler's"
	)

	answers <- NULL
	for (phrase in phrases) {
		answers <- c(answers,ng_predict(phrase,remove_stopwords,choices=20)[1,1])
	}

	print(transpose(answers))
}



#################################################################################################
## Function	: ng_log(x,verbose=TRUE)
## Description 	: if verbose mode, print x and flush console
#################################################################################################

ng_log <- function(verbose=TRUE,x) {
	if (verbose) {
		print(x)
		flush.console()
	}
}


#################################################################################################
## Function	: ng_predict(phrase,max_ngrams=5)
## Description 	: For given string, predict and return the next word
#################################################################################################

ng_predict <- function(phrase,remove_stopwords=FALSE,verbose=TRUE,choices=1,max_length=0,max_ngram=5) {

	ng_log(verbose,">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
	ng_log(verbose,paste0("Phrase: ",phrase))

	# Convert the phrase with same parameters as n-gram model
	tok <- tokens(char_tolower(phrase),
				remove_symbols=TRUE,
				remove_punct=TRUE,
				remove_twitter=TRUE,
				remove_numbers=TRUE,
				remove_url=TRUE)

	if (remove_stopwords) {
		tok <- tokens_remove(tok,stopwords("english"))
	}

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

	ng_log(verbose,word_ids)

	# Loop from longest possible n-gram to 1 and loop over ngram lists looking for match
	next_word_res <- as.data.table(NULL)

	for (i in max_to_try:1) {

		# Get all the matching n-grams for this level i.e. for i=1 we want all bigrams which
		# have first index as the first word id (last word in phrase), etc.

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

		colnames(next_word_info)[1] <- "idx"
		setkey(next_word_info,idx)

		# Lookup the word string from the dictionary
		setkey(dic,rank)
		next_word_info[,next_word:=dic[next_word_info,feature]]

		# Consolidate results list
		next_word_res <- rbind(next_word_res,next_word_info)
	}
	
	ng_log(verbose,next_word_res[order(-ngram_len,ngram_prob_rank)])
	
	# if choices is 0 just return the first word, otherwise datatable of choices with cols
	if (choices<0) {

		# Return first -1*choices words with highest ngram length and best rank
		next_word_res[order(-ngram_len,ngram_prob_rank)][1:(-1*choices),next_word]

	} else {

		# Return data table with top # of choices
		next_word_res[order(-ngram_len,ngram_prob_rank)][
			1:choices,.(next_word,ngram_len,ngram_freq,round(ngram_prob,5),ngram_prob_rank)]
	}
	
}

