#################################################################################################
## Project      : Module 10 / Capstone - Text Prediction
## Author       : Paul Ringsted
## Date         : 2019-03-01
## Function	: proc_ng(filename,min_ngrams=1,max_ngrams=5)
## Description  : Process input filename to ngrams using quanteda, return datatable with unique
##		: population and statistics, for all ngrams or those top-ranked only (default)
#################################################################################################

library(stringr)
library(readtext)
library(quanteda)
library(data.table)
library(parallel)

proc_ng <- function(filename,min_ngrams=1,max_ngrams=5,top_rank=TRUE) {

	print(paste0("Loading: ",filename))
	
		tx <- readtext(filename)

	print(paste0("Generating N-Grams... N=",min_ngrams,":",max_ngrams))

		ng <- tokens(char_tolower(tx$text),
			remove_symbols=TRUE,remove_punct=TRUE,
			ngrams=min_ngrams:max_ngrams,concatenator=" ")

	# Convert to datatable, determine the ngram length and summarize to unique n-grams
	print("Convert to datatable...")

		dt <- as.data.table(as.character(ng))
		colnames(dt) <- c("ngram")
		dt[,ngram_len:=str_count(ngram," ")+1]
		dtn<-dt[,.(ngram_freq=.N),by=.(ngram,ngram_len)]

	print(paste0("Done.  Datatable rowcount:",dtn[,.N]))

	# Get totals, order by ngram_freq and build data to plot CDF
	print("Generate summary totals...")

		tot_ng  <- dtn[,sum(ngram_freq),by=ngram_len]
		colnames(tot_ng)<-c("ngram_len","total")
		setkey(tot_ng,ngram_len)

	print("Totals by N-Gram:")
	print(tot_ng)

		unq_ng  <- dtn[,.N,by=ngram_len]
		colnames(unq_ng)<-c("ngram_len","total")
		setkey(unq_ng,ngram_len)

	print("Unique N-Grams:")
	print(unq_ng)

	# Order by ngram and then ngram_freq, copy to new data table and calculate rank and CDF scores
	print("Augment data table with statistics...")

		dtn <- dtn[order(ngram_len,-ngram_freq)]
		setkey(dtn,ngram_len)

		dtn[,pct:=ngram_freq/tot_ng[dtn,total]]
		dtn[,rank:=1:.N,by=ngram_len]
		dtn[,cdf := cumsum(pct),by=ngram_len]

	print("Splitting base and next word:")

		# base word: \\s*\\S*$   {space}{not space}{end string} replaced by ""
		# next word: ^.* (\\S+)$ {begin}{any chars}{space}{1=one word}{end string} replaced by \\1

		dtn[,c("base_word","next_word"):=
			.(gsub("\\s*\\S*$","",ngram),
			  gsub("^.* (\\S+)$","\\1",ngram))
		]

		# Set the key to base_word for fast lookups

		setkey(dtn,base_word)

	print("Calculating base_freq, ngram_prob and ngram_prob_rank by base word")

		dtn[,base_freq	     := sum(ngram_freq),by=base_word]
		dtn[,ngram_prob	     := ngram_freq/base_freq]
		dtn[order(base_word,-ngram_prob),ngram_prob_rank := 1:.N,by=base_word]

		dtn_top <- dtn[ngram_prob_rank==1,.N,by=ngram_len]
		colnames(dtn_top)<-c("ngram_len","total")

	print("Total top ranked N-Grams:")
	print(dtn_top)

	print(paste0("Done.  Total rowcount:",dtn[,.N]," Top ranked: ",dtn_top[,sum(total)]))

	# Return top-ranked n-grams or all of them
	if (top_rank) {
		dtn[ngram_prob_rank==1]
	} else {
		dtn
	}
	
}

