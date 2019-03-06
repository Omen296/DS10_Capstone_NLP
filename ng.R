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
		answers <- c(answers,ng_predict(phrase,remove_stopwords))
	}

	print(answers)
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
		answers <- c(answers,ng_predict(phrase,remove_stopwords))
	}

	print(answers)
}


#################################################################################################
## Function	: ng_predict(phrase,max_ngrams=5)
## Description 	: For given string, predict and return the next word
#################################################################################################

	

ng_predict <- function(phrase,remove_stopwords=FALSE,max_ngram=5) {

	print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
	print(paste0("Phrase: ",phrase))

	tok <- tokens(char_tolower(phrase),
				remove_symbols=TRUE,
				remove_punct=TRUE,
				remove_twitter=TRUE,
				remove_numbers=TRUE,
				remove_url=TRUE)

	if (remove_stopwords) {
		tok <- tokens_remove(tok,stopwords("english"))
	}

	words <- as.data.table(as.character(tok))

	colnames(words)<-"feature"

	max_words <- words[,.N]
	max_to_try <- min(max_ngram-1,max_words)

	if (max_to_try == 0)
		return()

	words[,word_order:=1:.N]			# preserve order
	setkey(words,feature)				# set up for lookup
	setkey(dic,feature)
	words[,word_id:=dic[words,rank]]		# lookup ids
	word_ids<-words[order(-word_order),word_id]	# go in reverse order

#	print(paste0("Num words: ",max_words," max to try: ",max_to_try))
#	print(words[order(-word_order)])

	print(word_ids)

	next_word_res <- as.data.table(NULL)
	first_choice <- NULL
	next_word <- NULL
	next_word_id <- NULL

	for (i in max_to_try:1) {

#		print(paste0("Checking ",i,"-gram: "))

		next_word_info <- switch(
			i,
			ng[ngram_len==2 & idx1==word_ids[1], .(idx2,ngram_len,ngram_freq,ngram_prob,ngram_prob_rank)],
			ng[ngram_len==3 & idx1==word_ids[2] & idx2==word_ids[1], .(idx3,ngram_len,ngram_freq,ngram_prob,ngram_prob_rank)],
			ng[ngram_len==4 & idx1==word_ids[3] & idx2==word_ids[2] & idx3==word_ids[1], .(idx4,ngram_len,ngram_freq,ngram_prob,ngram_prob_rank)],
			ng[ngram_len==5 & idx1==word_ids[4] & idx2==word_ids[3] & idx3==word_ids[2] & idx4==word_ids[1], 
				.(idx5,ngram_len,ngram_freq,ngram_prob,ngram_prob_rank)]
		)

		colnames(next_word_info)[1] <- "idx"
		setkey(next_word_info,idx)
		setkey(dic,rank)

		next_word_info[,next_word:=dic[next_word_info,feature]]

		next_word_res <- rbind(next_word_res,next_word_info)

#		print(next_word_info)
#
#		# Allow for multiple row results if ngrams generated with rank > 1
#
#		if (nrow(next_word_info) > 0) {
#			for (j in 1:nrow(next_word_info)) {
#
#				next_word_id <- as.integer(next_word_info[j,1])
#				next_word_id_freq <- next_word_info[j,2]
#				next_word_id_prob <- next_word_info[j,3]
#				next_word_id_prob_rank <- next_word_info[j,4]
#
#				if (length(next_word_id) != 0) {
#					next_word <- dic[rank==next_word_id,feature]
#					print(paste0("Found >>>>>> ",next_word, " <<<<<< word # ",next_word_id, 
#						" N-gram freq: ",next_word_id_freq,
#						" N-gram prob: ",next_word_id_prob))
#					print(paste0("Full text >> ",phrase," ",next_word))
#					print("")
#					if (is.null(first_choice)) {
#						first_choice <- next_word
#					}
#					next_word_info[j,]$next_word <- next_word
#				}
#			}
#		}
#
#		next_word_res <- rbind(next_word_res,next_word_info,fill=TRUE)

	}

#	print(">>>>>>>>> RESULTS <<<<<<<<<")
	print(next_word_res[order(-ngram_len,ngram_prob_rank)],nrows=200)

	first_choice
}

ng_predict_app <- function(phrase,remove_stopwords=FALSE,max_ngram=5) {

	tok <- tokens(char_tolower(phrase),
				remove_symbols=TRUE,
				remove_punct=TRUE,
				remove_twitter=TRUE,
				remove_numbers=TRUE,
				remove_url=TRUE)

	if (remove_stopwords) {
		tok <- tokens_remove(tok,stopwords("english"))
	}

	words <- as.data.table(as.character(tok))

	colnames(words)<-"feature"

	max_words <- words[,.N]
	max_to_try <- min(max_ngram-1,max_words)

	if (max_to_try == 0)
		return()

	words[,word_order:=1:.N]			# preserve order
	setkey(words,feature)				# set up for lookup
	words[,word_id:=dic[words,rank]]		# lookup ids
	word_ids<-words[order(-word_order),word_id]	# go in reverse order

	next_word_id <- NULL
	first_choice <- NULL
	next_word <- NULL

	for (i in max_to_try:1) {

		next_word_info <- switch(
			i,
			ng[ngram_len==2 & idx1==word_ids[1], .(idx2,ngram_freq)],
			ng[ngram_len==3 & idx1==word_ids[2] & idx2==word_ids[1], .(idx3,ngram_freq)],
			ng[ngram_len==4 & idx1==word_ids[3] & idx2==word_ids[2] & idx3==word_ids[1], .(idx4,ngram_freq)],
			ng[ngram_len==5 & idx1==word_ids[4] & idx2==word_ids[3] & idx3==word_ids[2] & idx4==word_ids[1], .(idx5,ngram_freq)]
		)


		next_word_id <- next_word_info[[1]]
		next_word_id_freq <- next_word_info[[2]]

		if (length(next_word_id) != 0) {
			next_word <- dic[rank==next_word_id,feature]
			if (is.null(first_choice)) {
				first_choice <- next_word
			}
		}
	}

	first_choice
}

