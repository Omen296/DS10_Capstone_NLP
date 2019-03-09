### Getting Started

* Just type a phrase into the input box and a prediction will appear, along with a list of other suggestions if desired.

* Clicking **Walk** will accept the current prediction, and get the next prediction (keep clicking Walk to extend the sentence).
* Clicking a suggestion in the grid will use that word instead of the top prediction, and get the next prediction.
* Clicking **Run** will automatically generate words up to the maximum numnber of words set by the slider on the left.
    + Note: In this mode, NexText will randomly select from the top suggestions, to prevent repeating phrases.
    + Set the "Number of top suggestions" slider to 1 to just use the top prediction (word sequence will be reproducible).

### UI Parameters

* **Max words for Run**
    + Maximum sentence length to generate after clicking "Run"
* **Max N-Gram Length**
    + Maximum N-Gram length to include in prediction.  Higher N provides more accuracy and coherence in text generation
* **Show top suggestions**
    + Whether to show the top suggestion grid.  Automatically turned off during a "Run" sequence for performance
* **Number of top suggestions**
    + How many suggestions from the prediction to display, or use during the random selection in a "Run" sequence

### UI Buttons

* **Clear**
    + Clear input box to start over, leave parameters unchanged
* **Reset**
    + Clear input box to start over, reset parameters to default values
* **Walk**
    + Runs the prediction and appends the top choice word to the input box
* **Run**
    + Loops the prediction with random selection of top specified # of suggestions, for specified # of words

### N-Gram Model Methodology

* Trained on 90% of Corpus (random sampling)
* Dictionary pruned to 95% of language coverage (20,205 words; only N-Grams containing these words are included)
* N-Grams generated including stopwords, probabilities based on relative frequency of N-Gram vs. N-1 Prefix
* N-Grams with frequency 1 discarded
* Top 5 (highest probability) N-Grams for each N=1:5 retained in dataset
* Prediction function returns data table with all matches up to UI limit; longer N-Gram matches take precedence
