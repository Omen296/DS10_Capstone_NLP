#################################################################################################
## Project      : Module 10 / Capstone - Text Prediction
## Author       : Paul Ringsted
## Date         : 2019-03-02
## Function	: proc_ng_freq(filename,min_ngrams=1,max_ngrams=5)
## Description  : Process input filename to ngrams using quanteda, return datatable with unique
##		: population and frequences, for minimal columns needed to aggregate
##		: Output cols: base_word (key), next_word, ngram_freq
#################################################################################################

library(stringr)
library(readtext)
library(quanteda)
library(data.table)
library(parallel)

proc_ng_freq <- function(filename,min_ngrams=1,max_ngrams=5) {

	# Set threads

		quanteda_options(threads=detectCores()-1)	

	print(paste0("Loading: ",filename))
	
		tx <- readtext(filename)

	print(paste0("Generating N-Grams... N=",min_ngrams,":",max_ngrams))

		ng <- tokens(char_tolower(tx$text),
			remove_symbols=TRUE,remove_punct=TRUE,
			ngrams=min_ngrams:max_ngrams,concatenator=" ")

	# Convert to datatable, determine the ngram length and summarize to unique n-grams
	print("Convert to datatable of unique n-grams...")

		dt <- as.data.table(as.character(ng))
		colnames(dt) <- c("ngram")
		dt[,ngram_len := str_count(ngram," ")+1]
		dtn<-dt[,.(ngram_freq=.N),by=.(ngram,ngram_len)]

	print("Splitting base and next word:")

		# base word: \\s*\\S*$   {space}{not space}{end string} replaced by ""
		# next word: ^.* (\\S+)$ {begin}{any chars}{space}{1=one word}{end string} replaced by \\1

		dtn[,c("base_word","next_word"):=
			.(gsub("\\s*\\S*$","",ngram),
			  gsub("^.* (\\S+)$","\\1",ngram))]

		# Set the key to base_word for fast lookups

		setkey(dtn,base_word)

	print("Calculating base freq, ngram probs and rank")

		dtn[,base_freq := sum(ngram_freq),by=base_word]

	print(paste0("Done.  Datatable rowcount:",dtn[,.N]," N-Grams: ",dtn[,sum(ngram_freq)]))

		dtn[,.(base_word,next_word,ngram_freq)]
}

