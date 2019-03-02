#################################################################################################
## Project	: Module 10 / Capstone - Text Prediction
## Function     : proc_corpus_split()
## Author	: Paul Ringsted
## Date		: 2019-02-28
## Description  : Randomly breaks up the 3 Corpus files into 10 files ./corpus_split/corpusN.txt N=0:9
##		  Does not perform any cleanup, this will be taken care of in quanteda
#################################################################################################

proc_corpus_split <- function() {
		
	set.seed(5678)
	
	con1 <- file("./corpus_split/corpus1.txt", open = "w")
	con2 <- file("./corpus_split/corpus2.txt", open = "w")
	con3 <- file("./corpus_split/corpus3.txt", open = "w")
	con4 <- file("./corpus_split/corpus4.txt", open = "w")
	con5 <- file("./corpus_split/corpus5.txt", open = "w")
	con6 <- file("./corpus_split/corpus6.txt", open = "w")
	con7 <- file("./corpus_split/corpus7.txt", open = "w")
	con8 <- file("./corpus_split/corpus8.txt", open = "w")
	con9 <- file("./corpus_split/corpus9.txt", open = "w")
	con0 <- file("./corpus_split/corpus0.txt", open = "w")

	# Keep counts for inbound and outbound lines
	out_ct <- rep(0,10)
	in_ct <- rep(0,3)

	files <- c("blogs","news","twitter")
	for (i in 1:3) {

		filename <- paste0("./corpus/en_US/en_US.",files[i],".txt")
		print(paste0("Processing ",filename))
		con_in  <- file(filename, open = "r")

		while (length(line_in <- readLines(con_in, n = 1, warn = FALSE)) > 0) {

			file_num <- sample(1:10,1)			# choose random file #
			con_out <- switch(file_num,con1,con2,con3,con4,con5,con6,con7,con8,con9,con0)

			cat(line_in, file = con_out, sep = "\n") 	# output the line

			out_ct[file_num] <- out_ct[file_num] + 1
			in_ct[i] <- in_ct[i] + 1
		}
		
		close(con_in)

	}

	close(con1)
	close(con2)
	close(con3)
	close(con4)
	close(con5)
	close(con6)
	close(con7)
	close(con8)
	close(con9)
	close(con0)

	print("Corpus file counts: ")
	print(in_ct)
	print(paste0("Corpus total count: ",sum(in_ct)))
	print("Output file counts: ")
	print(out_ct)
	print(paste0("Output total count: ",sum(out_ct)))
	
}
