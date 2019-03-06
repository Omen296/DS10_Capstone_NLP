#################################################################################################
## Project      : Module 10 / Capstone - Text Prediction
## Author       : Paul Ringsted
## Date         : 2019-03-02
## Module	: ng_pre.R
##		:
## Description  : Functions to process the corpusX.txt files into dictionary feature lookup file
##		: and master n-gram file for prediction
##		: ng_pre_dic	generates dictionary from given filenums, with 90% CDF cutoff
##		: ng_pre_corpus	processes corpus into filtered n-gram file with valid dic IDs
##		: ng_pre_merge	merges the corpus n-gram files into MASTER file
##		: ng_pre_top	calculates prob/ranks and collapses n-grams to top-ranked/freq>1
##		:
## I/O		: Inputs	./corpus_split/corpusX.txt where X=0:9
##		: Outputs:	./dic.RDS		dictionary from ng_pre_dic(x:y)
##		: 		./ngramX.RDS		ngram file from ng_pre_corpus(x:y)
##		: 		./ngramMASTER.RDS	ngram file from ng_pre_merge(x:y)
##		:		./ngramTOP.RDS		ngram file from ng_pre_top()
##		:		
## Example	: source('ng_pre.R')	Load this script (no pre-requisites)
##		: ng_pre_dic(0:8)	Prep dictionary on corpus slices 0-8
##		: ng_pre_corpus(0:8)	Generate interim ngram files from corpus slices 0-8
##		: ng_pre_merge(0:8)	Merge to MASTER unique ngrams from corpus slices 0-8
##		: ng_pre_top()		Generate final master TOP file from MASTER
##		:
## Notes	: Whilst code has been added to remove objects and clean up memory using gc()
##		: during execution, considerable memory leak was observed.  Advise running in 
##		: chunks with use of gc() at each stage
##		: 
#################################################################################################

# Load libraries

library(stringr)
library(readtext)
library(quanteda)
library(data.table)
library(parallel)

# Set up execution parameters here

DIC_CDF_PCT		<- c(0.50,0.75,0.90,0.95,0.975,1.0)	# CDF levels to display
DIC_CDF_PCT_CUTOFF	<- 0.95			# CDF cutoff level for final dictionary
DTOP_MAX_RANK		<- 20			# How many rankings to include in TOP file
DTOP_MIN_FREQ		<- 2			# Minimium freq to include in TOP file



ng_pre_corpus <- function(filenum_list,remove_stopwords=FALSE,min_ngrams=1,max_ngrams=5) {

	# This routine takes corpus file(s) and generates ngram files
	# N-grams containing words which are not in the dictionary are removed
	# N-grams are converted to dictionary indices to save space

	# Set threads
	quanteda_options(threads=detectCores()-1)	

	if (remove_stopwords) {
		dicname <- "./dic_nostop.RDS"
	} else {
		dicname <- "./dic.RDS"
	}
	print(paste0("Loading dictionary ",dicname))
	dic<-readRDS(dicname)
	setkey(dic,feature)
	print(paste0("Features: ",dic[,.N]))

	for (filenum in filenum_list) {

		filename <- paste0("./corpus_split/corpus",filenum,".txt")

		if (remove_stopwords) {
			fileout  <- paste0("./ngram_nostop",filenum,".RDS")
		} else {
			fileout  <- paste0("./ngram",filenum,".RDS")
		}

		print(paste0("Loading: ",filename))
		tx <- readtext(filename)

		print(paste0("Generating N-Grams... Remove stopwords: ",remove_stopwords," for N=",min_ngrams,":",max_ngrams))

		if (remove_stopwords) {

			# Two-step process to generate tokens, strip stopwords and then generate n-grams
			tok <- tokens_remove( 
						tokens(char_tolower(tx$text),
							remove_symbols=TRUE,
							remove_punct=TRUE,
							remove_twitter=TRUE,
							remove_numbers=TRUE),
						stopwords("english")
					)

			ng <- tokens_ngrams(tok,n=min_ngrams:max_ngrams,concatenator=" ")

			rm(tok)

		} else {

			ng <- tokens(char_tolower(tx$text),
				remove_symbols=TRUE,
				remove_punct=TRUE,
				remove_twitter=TRUE,
				remove_numbers=TRUE,
				remove_url=TRUE,
				ngrams=min_ngrams:max_ngrams,
				concatenator=" ")
	
		}

		# Convert to datatable, determine the ngram length and summarize to unique n-grams
		print("Convert to datatable of unique n-grams...")

		dt <- as.data.table(as.character(ng))
		colnames(dt) <- c("ngram")
		dtn<-dt[,.(ngram_freq=.N),by=.(ngram)]
		setkey(dtn,ngram)

		print("Get N-gram lengths")
		dtn[,ngram_len:=str_count(ngram," ")+1]

		print("Break up n-gram and apply dictionary index")	
		dtn[,c(paste0("word",1:5)) := tstrsplit(ngram," ")]
		setkey(dtn,word1)
		dtn[,idx1:=dic[dtn,rank]]
		setkey(dtn,word2)
		dtn[,idx2:=dic[dtn,rank]]
		setkey(dtn,word3)
		dtn[,idx3:=dic[dtn,rank]]
		setkey(dtn,word4)
		dtn[,idx4:=dic[dtn,rank]]
		setkey(dtn,word5)
		dtn[,idx5:=dic[dtn,rank]]

		setkey(dtn,idx1,idx2,idx3,idx4,idx5)

		# Get number of cols which have a valid dictionary lookup
		dtn[,idx_val:=5-Reduce("+", lapply(.SD, is.na)),.SDcols=paste0("idx",1:5)]
	
		dtf<-dtn[ngram_len==idx_val,.(idx1,idx2,idx3,idx4,idx5,ngram_len,ngram_freq)]

		print(paste0("This file... Stripped N-grams: ",dtf[,.N],
				" Unique N-grams: ",dtn[,.N],
				" Total N-grams: ",dtn[,sum(ngram_freq)]))

		print(paste0("Saving file ",fileout))
		saveRDS(dtf,fileout)

		print("Cleaning up memory for next chunk...")
		rm(tx)
		rm(ng)
		rm(dt)
		rm(dtn)
		rm(dtf)
		gc()
	}
	print("Done.")
}


ng_pre_merge <- function(filenum_list,remove_stopwords=FALSE) {

	# No filtering is performed here just aggregate unique N-grams to MASTER file

	first_run <- TRUE
	ngram_cnt <- 0
	ngram_freq_tot <- 0

	for (filenum in filenum_list) {

		if (remove_stopwords) {
			filename <- paste0("./ngram_nostop",filenum,".RDS")
		} else {
			filename <- paste0("./ngram",filenum,".RDS")
		}

		print(paste0("Loading: ",filename))
		dt <- readRDS(filename)
		setkey(dt,idx1,idx2,idx3,idx4,idx5,ngram_len)
		ngram_cnt <- ngram_cnt + dt[,.N]
		ngram_freq_tot <- ngram_freq_tot + dt[,sum(ngram_freq)]

		print(paste0("This file >>> N-gram count: ",dt[,.N],
				" N-gram freq: ",dt[,sum(ngram_freq)],
				" Running N-gram count: ",ngram_cnt,
				" Running N-gram freq: ",ngram_freq_tot))

		# Merge totals with running list of all n-grams

			if (first_run) {

			# First time, no merge just take the results
			print("First time - take these results")
			dtm <- dt
			first_run <- FALSE
		
		} else {
	
			# Merge dtm and the latest n-gram list, sum up the frequencies, remove cols
			print("Merging results")
			dt2<-rbind(dtm,dt)

			print("Reducing to unique n-grams")
			dtm<-dt2[,.(ngram_freq=sum(ngram_freq)),
					by=.(idx1,idx2,idx3,idx4,idx5,ngram_len)]

		}
		
		setkey(dtm,idx1,idx2,idx3,idx4,idx5,ngram_len)

		print(paste0("Merged totals... Unique N-grams: ",dtm[,.N],
				" Total N-gram freq: ",dtm[,sum(ngram_freq)]))
		print(paste0("Compression: ",round(100*dtm[,.N]/ngram_cnt,1),"%"))

		print("Cleaning up memory...")
		gc()
		
	}

	if (remove_stopwords) {
		fileout <- paste0("./ngram_nostopMASTER.RDS")
	} else {
		fileout <- paste0("./ngramMASTER.RDS")
	}
	print(paste0("Saving file ",fileout))
	saveRDS(dtm,fileout)
	
	print("Done.")
}


ng_pre_top <- function(remove_stopwords=FALSE) {

	# Calculate probabilities and ranking and then filter down to top results

	if (remove_stopwords) {
		filein  <- paste0("./ngram_nostopMASTER.RDS")
		fileout <- paste0("./ngram_nostopTOP.RDS")
	} else {
		filein  <- paste0("./ngramMASTER.RDS")
		fileout <- paste0("./ngramTOP.RDS")
	}

	print(paste0("Loading ",filein))
	dtn <- readRDS(filein)

	setkey(dtn,idx1,idx2,idx3,idx4,idx5)

	print("Generating base_id")
	dtn[,base_id:=paste(ifelse(ngram_len>1,idx1,"NA"),
				ifelse(ngram_len>2,idx2,"NA"),ifelse(ngram_len>3,idx3,"NA"),
				ifelse(ngram_len>4,idx4,"NA"),"NA",sep="_")]

	setkey(dtn,base_id)
	gc()			# added since last run

	print("Calculating probabilities and ranks...")
	dtn[,base_freq       := sum(ngram_freq),by=base_id]
	dtn[,ngram_prob      := ngram_freq/base_freq]
	dtn[order(base_id,-ngram_prob),ngram_prob_rank := 1:.N,by=base_id]

	setkey(dtn,ngram_prob_rank,ngram_freq)
	dtop<-dtn[ (ngram_prob_rank <= DTOP_MAX_RANK) & (ngram_freq >=DTOP_MIN_FREQ),
			.(ngram_len,idx1,idx2,idx3,idx4,idx5,ngram_freq,base_freq,ngram_prob,ngram_prob_rank)]
	setkey(dtop,ngram_len,idx1,idx2,idx3,idx4,idx5)
	
	print(paste0("Source N-Gram count total:",dtn[,.N]))
	print(dtn[,.N,by=ngram_len])

	print(paste0("Top ranked N-Gram count total:",dtop[,.N]))
	print(dtop[,.N,by=ngram_len])

	print(paste0("Saving N-Grams to ",fileout))
	saveRDS(dtop,fileout)

	print("Cleaning up memory")
	rm(dtop)
	rm(dtn)
	gc()

	print("Done.")
}


ng_pre_dic <- function(filenum_list,remove_stopwords=FALSE) {

	# Set threads
	quanteda_options(threads=detectCores()-1)

	if (remove_stopwords) {
		dicname <- "./dic_nostop.RDS"
	} else {
		dicname <- "./dic.RDS"
	}
	filenames <- paste0("./corpus_split/corpus",filenum_list,".txt")

	print("Reading filenames:")
	print(filenames)

	# Load all the files and generate DFMs, gather feature frequencies
	tx <- readtext(filenames)
	txcorpus<-corpus(tx)

	# Generate DFM and frequencies

	if (remove_stopwords) {

		# Two-step process to generate tokens, strip stopwords and then generate n-grams
		tok <- tokens_remove( 
					tokens(char_tolower(tx$text),
						remove_symbols=TRUE,
						remove_punct=TRUE,
						remove_twitter=TRUE,
						remove_numbers=TRUE),
					stopwords("english")
				)

		txdfm <- dfm(tok)
		rm(tok)

	} else {

		txdfm <- dfm(txcorpus,
			tolower=TRUE,
			remove_symbols=TRUE,
			remove_punct=TRUE,
			remove_twitter=TRUE,
			remove_numbers=TRUE,
			remove_url=TRUE)

	}

	txstat <- as.data.table(textstat_frequency(txdfm))

	# Store feature,frequency,rank as the dictionary
	dic <- txstat[order(rank),.(feature,frequency,rank)]

	tot <- dic[,sum(frequency)]
	dic[,pct:=frequency/tot]
	dic[,cdf:=cumsum(pct)]

	# Display example CDFs for reference only
	for (cdf_pct in DIC_CDF_PCT) {
		
		cdf_rank <- dic[cdf>=cdf_pct,min(rank)]	
		print(paste0("Number of features for ",100*cdf_pct,"% coverage: ",cdf_rank))

	}

	# Make the cut
	cutoff_rank <- dic[cdf >= DIC_CDF_PCT_CUTOFF, min(rank)]
	print(paste0("Using ",100*DIC_CDF_PCT_CUTOFF,"% cutoff for dictionary, at rank ",cutoff_rank))

	dic_cutoff<-dic[rank<=cutoff_rank]
	
	print(paste0("Saving... Features: ",dic_cutoff[,.N]," out of ",dic[,.N]))

	# Dump to file and cleanup
	saveRDS(dic_cutoff,dicname)
	gc()

	print("Done")
}
