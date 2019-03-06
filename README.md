# Natural Language Processing - Text Prediction
## Capstone Project (Course 10)
### Paul Ringsted, 27th February 2019

See R  and Rmd documents for R code.

Week 2 Analysis Report: https://ringspagit.github.io/DS10_Capstone_NLP/CapstoneWk2Analysis.html

Working Modules / Functions:

* **proc_corpus_split()**
    + Randomly splits the 3 corpus docs into 10 output files
* **ng_pre.R:**
    + **ng_pre_dic()**		generates dictionary to IDs from given filenums, with CDF cutoff
    + **ng_pre_corpus()**	processes corpus into filtered n-gram file with valid dic IDs
    + **ng_pre_merge()**	merges the corpus n-gram files into MASTER file
    + **ng_pre_top(**)		create TOP file with basic prob/ranks, pruned by rank & freq
