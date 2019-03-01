#################################################################################################
## Project	: Module 10 / Capstone - Text Prediction
## Author	: Paul Ringsted
## Date		: 2018-02-24
##		: Work-in-progress functions for exploratory analysis
#################################################################################################

library(stringr)
library(readtext)
library(quanteda)
library(data.table)
library(parallel)
library(sqldf)
library(tidytext)

qBlogs <- function() {

	print("Loading data")
	#blogs <- readtext("./corpus/en_US/en_US.blogs.txt")
	blogs <- readtext("./test/en_US/en_US.blogs.txt")
	
	genng(blogs)
}

train_gen_all_csv <- function(remove_stop_words=FALSE) {

	for (f in 1:2) {
	
		filename_in  <- paste0("./train",f,".txt")
		filename_csv <- paste0("./train",f,".dt.csv")
		print(paste0("Loading data from ",filename_in))
		tx <- readtext(filename_in)
		ng <- genng(tx,remove_stop_words)

		print("Converting to data table")
		dt <- as.data.table(as.character(ng))
		colnames(dt) <- c("ngram_words")
		print(paste0("Rows:          ",nrow(dt)))
		
		print("Compressing to unique n-grams")
		dtu <- sqldf("select ngram_words,count(*) as freq from dt group by ngram_words")
		print(paste0("Rows:          ",nrow(dtu)))

		print(paste0("Saving data to ",filename_csv))
		write.csv(dtu,filename_csv,sep=",")

		print("Cleaning up")
		rm(tx)
		rm(ng)
		rm(dt)
		rm(dtu)
		gc()
	}
}

train_gen_all_dt <- function(remove_stop_words=FALSE) {

	for (f in 1:9) {
	
		filename_in  <- paste0("./train",f,".txt")
		filename_rds <- paste0("./train",f,".dt.rds")
		print(paste0("Loading data from ",filename_in))
		tx <- readtext(filename_in)
		ng <- genng(tx,remove_stop_words)

		print("Converting to data table")
		dt <- as.data.table(as.character(ng))
		colnames(dt) <- c("ngram_words")
		print(paste0("Rows:          ",nrow(dt)))
		
		print("Compressing to unique n-grams")
		dtu <- sqldf("select ngram_words,count(*) as freq from dt group by ngram_words")
		print(paste0("Rows:          ",nrow(dtu)))

		print(paste0("Saving data to ",filename_rds))
		saveRDS(dtu,filename_rds)

		print("Cleaning up")
		rm(tx)
		rm(ng)
		rm(dt)
		rm(dtu)
		gc()
	}
}

train_merge_dt <- function() {

	for (f in 1:9) {
	
		filename_in <- paste0("./train",f,".dt.rds")
		print(paste0("Loading ",filename_in))

		dt_in <- readRDS(filename_in)
		print(paste0("Loaded ",filename_in," with ", nrow(dt_in)," rows"))

		if (f == 1) {
			dt <- dt_in
		} else {
			l <- list(dt,dt_in)
			dt <- rbindlist(l)
		}

		print(paste0("Combined rows: ",nrow(dt)))
	}

	print("Compressing to unique ngrams")

	dtu <- sqldf("select ngram_words,sum(freq) as freq from dt group by ngram_words")
	print(paste0("Final rows: ",nrow(dtu)))

	print("Saving corpus list")
	filename_rds <- paste0("./train_all.dt.rds")
	saveRDS(dtu,filename_rds)

	print("Cleaning up")
	rm(dt)
	rm(dtu)
	rm(dt_in)
	gc()
	
}

train_gen <- function(remove_stop_words=FALSE,force_run=FALSE) {

	if (remove_stop_words) {
		filename_rds <- c("./train_nostop.rds")
	} else {
		filename_rds <- c("./train.rds")
	}

	if (file.exists(filename_rds) & !force_run) {

		print(paste0("Loading data from file",filename_rds))
		dt <- readRDS(filename_rds)

	} else {

		filename_in <- c("./train.txt")
		print(paste0("Loading data from ",filename_in))
		tx <- readtext(filename_in)
		ng <- genng(tx,remove_stop_words)
		dt <- genng_stats(ng)

		print(paste0("Saving data to ",filename_rds))
		saveRDS(dt,filename_rds)

	}
	
	dt
}

test_model <- function(dt,remove_stop_words=FALSE) {

	filename <- "./test.txt"
	con_in  <- file(filename, open = "r")

	max_tries <- 100
	try <- 0
	success <- 0

	while ((length(line_in <- readLines(con_in, n = 1, warn = FALSE)) > 0) & (try < max_tries)) {

		try <- try + 1
		clean_in  <- tolower(gsub("[[:punct:]]", "", line_in))
		base      <- gsub("\\s*\\w*$", "", clean_in)
		if (base != "") {
		
			last_word <- substring(clean_in,str_length(base)+2)

			choices <- predictNextMult(dt,base,remove_stop_words)

			if (nrow(choices)>0) {
				first_choice <- choices[1,]$next_word
			} else {
				first_choice <- c("<N/A>")
			}
		
			if (first_choice==last_word) {
				success <- success + 1
			}

			print(paste(base,last_word," >> PREDICTED: ",first_choice,(first_choice == last_word)))
		}
	}

	close(con_in)

	print(paste0("Tries: ",try," Success: ",success))

}

wk3_gen <- function(remove_stop_words=FALSE,force_run=FALSE) {

	if (remove_stop_words) {
		filename <- c("./wk3_nostop.rds")
	} else {
		filename <- c("./wk3.rds")
	}

	if (file.exists(filename) & !force_run) {

		print("Loading data from file")
		dt <- readRDS(filename)

	} else {

		print("Loading data from corpus")
		corpus <- readtext("./test/en_US/Wk3Quiz/*.txt")

		ng <- genng(corpus,remove_stop_words)
		dt <- genng_stats(ng)

		print("Saving data")
		saveRDS(dt,filename)

	}
	
	dt
}


wk3_test <- function(dt,remove_stop_words=FALSE) {

	predictNextMult(dt,"The guy in front of me just bought a pound of bacon, a bouquet, and a case of",remove_stop_words)
	predictNextMult(dt,"You're the reason why I smile everyday. Can you follow me please? It would mean the",remove_stop_words)
	predictNextMult(dt,"Hey sunshine, can you follow me and make me the",remove_stop_words)
	predictNextMult(dt,"Very early observations on the Bills game: Offense still struggling but the",remove_stop_words)
	predictNextMult(dt,"Go on a romantic date at the",remove_stop_words)
	predictNextMult(dt,"Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",remove_stop_words)
	predictNextMult(dt,"Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",remove_stop_words)
	predictNextMult(dt,"After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",remove_stop_words)
	predictNextMult(dt,"Be grateful for the good times and keep the faith during the",remove_stop_words)
	predictNextMult(dt,"If this isn't the cutest thing you've ever seen, then you must be",remove_stop_words)

	return()
}

load_test <- function() {

	print("Loading data")
	corpus <- readtext("./test/en_US/*.txt")

	genng(corpus)
}

load_corpus <- function() {

	print("Loading data")
	corpus <- readtext("./corpus/en_US/*.txt")

	genng(corpus)
}

load_corpus_blogs <- function() {

	print("Loading data")
	corpus <- readtext("./corpus/en_US/en_US.blogs.txt")

	genng(corpus)
}

load_corpus_news <- function() {

	print("Loading data")
	corpus <- readtext("./corpus/en_US/en_US.news.txt")

	genng(corpus)
}

load_corpus_twitter <- function() {

	print("Loading data")
	corpus <- readtext("./corpus/en_US/en_US.twitter.txt")

	genng(corpus)
}

removeWords <- function(str, stopwords) {
  x <- unlist(strsplit(str, " "))
  paste(x[!x %in% stopwords], collapse = " ")
}
	
#################################################################################################
## Function	: genng(corpus,num_grams=5)
## Description 	: Use Quanteda to load blogs data with parallel processing to gen list of ngrams
##              : Convert to datatable, split base and next word, calculate probabilities
##              : and select highest probability next_word for given base
#################################################################################################

genng <- function(corpus,remove_stop_words=FALSE,num_grams=5) {

	print("Generating tokens and ngrams")

	quanteda_options(threads=detectCores()-1)

	#ng <- tokens(char_tolower(corpus$text),remove_symbols=TRUE,remove_punct=TRUE,ngrams=2:num_grams,concatenator=" ",verbose=TRUE)
	toks <- tokens(char_tolower(corpus$text),remove_symbols=TRUE,remove_punct=TRUE,verbose=TRUE)

	if (remove_stop_words) {
		toks_nostop <- tokens_remove(toks,pattern=stopwords('en'),verbose=TRUE)
		ng <- tokens_ngrams(toks_nostop,n=2:num_grams,concatenator=" ")
	} else {
		ng <- tokens_ngrams(toks,n=2:num_grams,concatenator=" ")
	}		
	
	ng
}

genng_stats <- function(ng) {

	print("Converting to data table and getting unique ngrams with counts")
	dt <- as.data.table(as.character(ng))
	colnames(dt) <- c("ngram_words")

	dtu <- sqldf("select ngram_words,count(*) as freq from dt group by ngram_words")

	print("Splitting ngram to base and next_word")
	dtu$base      <- gsub("\\s*\\w*$", "", dtu$ngram_words)
	dtu$next_word <- substring(dtu$ngram_words,str_length(dtu$base)+2)

	print(paste0("Done... N-Grams: ",sum(dtu$freq)))

	print("Appending base counts")
	base_cnt <- sqldf("select base,sum(freq) as base_freq from dtu group by base")

	dt_cnt <- sqldf("select d.base,d.next_word,d.freq,b.base_freq from dtu d, base_cnt b where d.base = b.base")

	print("Calculating probabilities and selecting max prob next_word for each base")
	dt_cnt$prob <- dt_cnt$freq/dt_cnt$base_freq

	print(paste0("Done.  Rows: ",nrow(dt_cnt)))
	dt_cnt

#	dt_rnk <- sqldf("select d.* from dt_cnt d, (select base,max(prob) as prob from dt_cnt group by base) e where d.base = e.base and d.prob = e.prob")
#	print(paste0("Done.  Rows: ",nrow(dt_rnk)))
#	dt_rnk
}
	

#################################################################################################
## Function	: predictNext
## Description 	: For given string, predict next word
#################################################################################################

predictNext <- function(dt,phrase_full,max_ngram=5) {

	#phrase <- removeWords(phrase_full,stopwords())
	phrase <- phrase_full

	print(paste0("Stripped: ",phrase))

	max_words <- sapply(strsplit(phrase, " "), length)
	max_to_try <- min(max_ngram-1,max_words)

	if (max_to_try == 0)
		return()

	print(paste0("Num words: ",max_words," max to try: ",max_to_try))

	next_word <- NULL

	for (i in max_to_try:1) {

		input_base <- word(phrase,-i,-1)

		print(paste0("Checking ",i,"-gram: ",input_base))

		lookup <- dt[which(dt$base==input_base),]

		if (nrow(lookup) != 0) {
			next_word <- lookup$next_word
			print(paste0("Found >>>>>> ",next_word))
			print(paste0("Full text >> ",phrase_full," ",next_word))
			print("")
			break
		}
	}

	next_word
}

#################################################################################################
## Function	: predictNextMult
## Description 	: For given string, predict next word with multiple options
#################################################################################################

predictNextMult <- function(dt,phrase_full,remove_stop_words=FALSE,max_ngram=5) {

	if (remove_stop_words) {
		phrase <- removeWords(phrase_full,stopwords())
	} else {
		phrase <- phrase_full
	}

	print(paste0("Stripped: ",phrase))

	max_words <- sapply(strsplit(phrase, " "), length)
	max_to_try <- min(max_ngram-1,max_words)

	if (max_to_try == 0)
		return()

	print(paste0("Num words: ",max_words," max to try: ",max_to_try))

	choices<-as.data.frame(NULL)

	for (i in max_to_try:2) {

		input_base <- word(phrase,-i,-1)

		print(paste0("Checking ",i,"-gram: ",input_base))

		lookup <- dt[which(dt$base==input_base),]
		lookup_rnk <- sqldf("select d.* from lookup d, (select base,max(prob) as prob from lookup group by base) e where d.base = e.base and d.prob = e.prob")

		if (nrow(lookup) != 0) {
			choices <- rbind(choices,as.data.frame(lookup_rnk))
		}
	}

	print(choices)
	choices
}

