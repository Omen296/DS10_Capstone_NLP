# Natural Language Processing - Text Prediction
## Capstone Project (Course 10)
### Paul Ringsted, 27th February 2019

See R  and Rmd documents for R code.

Week 2 Analysis Report: https://ringspagit.github.io/DS10_Capstone_NLP/CapstoneWk2Analysis.html

Shiny App: https://ringspagit.shinyapps.io/NexText/

Shiny App Deck: https://ringspagit.github.io/DS10_Capstone_NLP/NexText.html

---
**Final Working Scripts / Functions:**

* **proc_corpus_split()**
    + Randomly splits the 3 corpus docs into 10 output files
* **ng_pre.R:**
    + **ng_pre_dic()**		generates dictionary to IDs from given filenums, with CDF cutoff
    + **ng_pre_corpus()**	processes corpus into filtered n-gram file with valid dic IDs
    + **ng_pre_merge()**	merges the corpus n-gram files into MASTER file
    + **ng_pre_top()**		create TOP file with basic prob/ranks, pruned by rank & freq
* **ng_load.R:**
    + Setup fresh R session to run model: Loads dic and ng datatables, sources ng.R and **ng_predict_app.R**
* **ng_predict_app.R:**
    + **ng_predict_app()** 	returns table or list of top predictions, minimal code for shiny

---
Testing Scripts:

* ng.R:
    + ng_predict()		returns singular (top) prediction for phrase - old version with logging
    + wk3_test()		runs the test predictions for Week 3 Quiz
    + wk4_test() 		runs the test predictions for Week 4 Quiz
* ng_load_nostopR
    + Setup fresh R session with _nostop versions of models.  Used for testing but not final app.

---
**Resources:**

* Benchmarking	https://github.com/hfoffani/dsci-benchmark

* N-Gram Paper	https://lagunita.stanford.edu/c4x/Engineering/CS-224N/asset/slp4.pdf

* Quanteda	https://github.com/rstudio/cheatsheets/raw/master/quanteda.pdf
