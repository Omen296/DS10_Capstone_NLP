#### Capstone Project (Course 10)
#### Paul Ringsted, 27th February 2019

### Parameters

| **Parameter** | Description |

|--|--|
|**Generate Sentence - Max words**|Max sentence length to generate after clicking "Run"|
|**Max N-Gram Length to use**|Maximum N-Gram length to include in prediction.  Higher N provides more accuracy and coherence in text generation|
|**Show top suggestions**|Whether to show the top suggestion grid.  Automatically turned off during a "Run" sequence for performance|
|**Number of top suggestions**|How many suggestions from the prediction to display, or use during the random selection in a "Run" sequence.  Setting this to 1 just uses top results (word sequences will be reproducible)|

### UI Functions

* **Clear**		Clear input box to start over, leave parameters unchanges
* **Reset**		Restore parameter defaults and clear input box to start over
* **Walk**		Runs the prediction and appends the top choice word to the input box
* **Run**		Loops the prediction with random selection of top specified # of suggestions

### N-Gram Model Methodology

* Trained on 90% of Corpus (random sampling)
* Dictionary pruned to 95% of language coverage (20,205 words; only N-Grams containing these words are included)
* N-Grams generated including stopwords, probabilities based on basic relative frequency of N-Gram vs. N-1 Prefix
* N-Grams with frequency 1 discarded
* Top 5 (highest probability) N-Grams for each N=1:5 retained in dataset
* Prediction function returns data table with all matches up to UI limit, higher N-Gram matches taking precedence
* Benchmarked average execution time 30ms
