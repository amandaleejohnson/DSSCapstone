# DSSCapstone
NextWord - Data Science Specialization Capstone Project


For the capstone project of the Data Science Specialization at Johns Hopkins University, I was tasked with creating a predictive text algorithm, similar to what you see when typing on a smartphone. When typing "I went to the", the keyboard presents three options for the most probable next word or phrase: 1) gym, 2) tanning salon, or 3) laundromat.

In this presentation, I will answer the following questions:

1. How does NextWord work?

2. How well does NextWord perform?

3. How can I use NextWord?

The skills needed to complete this capstone include data cleaning processes using regular expressions, exploratory data analysis, statistical inference, machine learning, natural language processing, and developing data products. NextWord was created using the Shiny Application in RStudio.


How does NextWord work?
========================================================

First, a large corpus of text from US english-based tweets, news articles, and blog posts was read into R. The corpus was cleaned by removing punctuation, replacing contractions, removing emojis/emoticons, identifying internet slang (e.g., lol to "laughing out loud"), and removing profanity. 

Then, a series of summary tables were created to identify the most common words and n-grams in the corpus. Tne "n" in n-gram is the number of words in a phrase. For example, "I love you" is a common 3-gram. 

These summary tables were then used to create a series of prediction models using the [Katz Back-off (KBO) algorithm](https://en.wikipedia.org/wiki/Katz's_back-off_model) employing Good-Turing smoothing in order to most accurately capture how humans communicate using the english language. The KBO algorithm is a preferred method for prediction over other weighting [methods](https://web.stanford.edu/~jurafsky/slp3/3.pdf).

In short, a user can feed NextWord some text, as little as a single word, and NextWord uses the prediction algorithm to predict the next most probable word. 


How well does NextWord perform?
========================================================

NextWord's corpus includes 211,644 words. Of these, 7,213 unique words comprise 90% of the entire corpus. The most common words are unsurprisingly, "the", "to", and "and", which comprise 4.7%, 2.8%, and 2.7% of the corpus, respectively.

To assess NextWord's performance, I completed a series of out-of-sample validation tests on text excluded from NextWord's original corpus.

NextWord will continue to be updated to improve prediction accuracy while maintaining high efficiency.


How can I use NextWord?
========================================================

Try out NextWord for yourself! Click [here](https://ajohns34.shinyapps.io/DSS_Capstone/) to access the app!

To learn more about the code used to generate this predictive algorithm and my other ongoing projects, please visit my github: <https://github.com/amandaleejohnson>. 

This project would not be possible without the partnership of Johns Hopkins University, Coursera, and SwiftKey. More information on SwiftKey can be found [here](https://www.microsoft.com/en-us/swiftkey?activetab=pivot_1:primaryr2).
