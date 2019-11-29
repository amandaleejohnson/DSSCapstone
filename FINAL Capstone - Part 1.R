####################################################
#DATA SCIENCE SPECIALIZATION########################
#CAPSTONE###########################################
#PART 1#############################################
####################################################

#HELPFUL SOURCE: https://www.tidytextmining.com/
setwd("C:/Users/ajohns34/Box/Data Science Specialization/Capstone/data/en_US")

# load in libraries we'll need
    library(tidyverse) 
    library(tidytext) #tidy text analysis
    library(glue) #pasting strings
    library(data.table) #for rbindlist, a faster version of rbind
    library(readr)
    library(stringr)
    library(tm) #Used to transform text to lowercase, remove punctuation, numbers, etc.
    library(sentimentr) #for identifying profanity words
    library(textclean) #Replace common non-ascii characters
    library(knitr) #for kable
    library(ggplot2)
    library(wordcloud)
    library(quanteda)

#Read in the english data files with a for loop:
for (i in c("twitter", "blogs", "news")) {
    #Since the for loop doesn't know how to read in the value of i
    #in a name, use the paste0 function:
    filename = paste0("en_US.", i, ".txt")
    print(paste0("File info of ", filename, " below:")) 
    print(file.info(paste0("./", filename)))
    #Read in the text file, suppressing warning messages:
    assign(i, suppressWarnings(readLines(filename)))
    #Transform the data into a tidy data frame using tibble
    #Create two columns: 1 "line" and 2 "text"
    length = length(suppressWarnings(readLines(filename)))
    print(paste0("Total number of lines of ", i, " file = ", length))
    assign(paste0(i, "_df"), tibble(line = 1:length, text = assign(i, suppressWarnings(readLines(filename)))))
}

#Remove words with non-ASCII characters
    length(twitter)
    nonenglish = grep("twitter", iconv(twitter, "latin1", "ASCII", sub="twitter"))
    twitter = twitter[-nonenglish]
    length(twitter)
    tenth_twitter = length(twitter)/10
    
    length(blogs)
    nonenglish = grep("blogs", iconv(blogs, "latin1", "ASCII", sub="blogs"))
    blogs = blogs[-nonenglish]
    length(blogs)
    half_blogs = length(blogs)/2
    
    length(news)  
    nonenglish = grep("news", iconv(news, "latin1", "ASCII", sub="news"))
    news = news[-nonenglish]
    length(news)
    most_news = length(news)*0.8

#Create a random sample of text across all three sources: twitter, blogs, news
#in order to explore the data and train our prediction algorithm:
    twitter_sample = twitter[sample(1:length(twitter), tenth_twitter)]
    blogs_sample = blogs[sample(1:length(blogs), half_blogs)]
    news_sample = news[sample(1:length(news), most_news)] 
    
    subsample = c(twitter_sample, blogs_sample, news_sample)

#Save sample as a txt file to be easily loaded in later:
    writeLines(subsample, "./subsample.txt")
    subsample_df = tibble(line = 1:length(subsample), 
                          text = assign(subsample, suppressWarnings(readLines("./subsample.txt"))))

#An important alternative to Corpus object has emerged in recent years in the form of tidytext. 
#Instead of saving a group of documents and associated meta data, text that is in tidytext format contains one word per row, 
#and each row also includes additional information about the name of the document where the word appears, and the order in which the words appear.
#Remove unwanted characters, restructure the data so that each word is on a separate row, 
#and remove stop words:
#Unwanted characters:
    remove_reg = "[0123456789!@#$%^*+=}{/><]"

#Keep stop words in!
subsample_df_word = subsample_df %>%
    mutate(text = str_remove_all(text, remove_reg)) %>% ##Remove unwanted characters like "&"
    unnest_tokens(word, text) ##Restructure the data so that each word is on a separate row    
    
#Use dplyr's count() to find the most common words 
subsample_words = subsample_df_word %>%
    count(word, sort = TRUE)

#Calculate the total number of words:
#THis might not work if you have multiple packages installed with a "summarize" command.
#Therefore, you need to specify that we are using the "plyr" package:
total_subsample_words = plyr::summarize(subsample_words, total = sum(n))

subsample_words = cbind(subsample_words, total_subsample_words)

#Exclude list of profane words/phrases
#Source of profane word list: https://www.cs.cmu.edu/~biglou/resources/
profane_words = read.delim("./profane_words.txt", header = FALSE, sep="\n") #sep is signaling "new line"
#Rename the V1 column as "word" so that anti_join and inner_join work:
profane_words = profane_words %>% rename(word = V1)

#Since this is based on single word, we need to apply it to the word dataset 
#where each word is its own separate line:
subsample_words_noprofane = anti_join(subsample_words, profane_words)
subsample_words_profane = inner_join(subsample_words, profane_words)

#Replace the subsample_words df with the subsample_words_noprofane:
subsample_words = subsample_words_noprofane

#Examine Zipf's law:
#Zipf's law states that the frequency that a word appears is inversely proportional to its rank.
freq_by_rank_subsample = subsample_words %>%
    mutate(rank = row_number(), 
           `term frequency` = n/total)

#Create a variable taht denotes cumulative count:
freq_by_rank_subsample$`cum count` = cumsum(freq_by_rank_subsample$n)

#Create a variable that denotes cumulative frequency:
freq_by_rank_subsample$`cum freq` = cumsum(freq_by_rank_subsample$`term frequency`)

#Create a variable that denotes the row number (this will be used later to find out how many words are 50% of the corpus)
freq_by_rank_subsample$rownum = 1:dim(freq_by_rank_subsample)[1]

#Create a frequency of frequency table that will be used later for Good-Turing Smoothing
word_freqoffreq = data.frame(word=table(freq_by_rank_subsample$n))

#How many unique words do you need in order to cover 50% of all word instances in the language? 90%?
#Step 1. Plot number of words vs. cumulative relative frequency in order display that coverage increases
#with the addition of words. 
plot(freq_by_rank_subsample$rownum[1:50000], freq_by_rank_subsample$`rel cum freq`[1:50000], type="l", lty=1, 
     ylab = "Percent of Coverage (Cumulative Relative Frequency)", xlab = "# Words (Starting with most common)")

#How many words do we need to achieve 50% of coverage?
head(freq_by_rank_subsample)
paste("NUmber of words comprising 49.99-50.01% of the dictionary:", 
      range(freq_by_rank_subsample$rownum[freq_by_rank_subsample$`cum freq`>0.4999 & freq_by_rank_subsample$`cum freq`<0.5001])[1], 
      "to",
      range(freq_by_rank_subsample$rownum[freq_by_rank_subsample$`cum freq`>0.4999 & freq_by_rank_subsample$`cum freq`<0.5001])[2], 
      "words.")

paste("NUmber of words comprising 90.01-95.01% of the dictionary:", 
      range(freq_by_rank_subsample$rownum[freq_by_rank_subsample$`cum freq`>0.9001 & freq_by_rank_subsample$`cum freq`<0.9501])[1], 
      "to", 
      range(freq_by_rank_subsample$rownum[freq_by_rank_subsample$`cum freq`>0.9001 & freq_by_rank_subsample$`cum freq`<0.9501])[2], 
      "words.")

##Visualize the most common words:
head(freq_by_rank_subsample, 5)
ggplot(freq_by_rank_subsample[1:50,], aes(x = reorder(word, -n), y = n, fill = n)) + #reorder in aes orders the bars by frequency, not alphabetical
    geom_col() +
    geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
    geom_text(aes(label = paste0(round((`term frequency`*100), 2), "%")), vjust = +1.3, size = 3, color = "white") +
    theme(axis.text.x = element_text(angle = 90)) + 
    labs(title = "Most frequent words in sample (including stop words)", x = "Word", y = "N and Relative Frequency %")

#Let's look at the distribution of n/total, 
#the number of times a word appears divided by the total number of terms (words).
#This is exactly what term frequency is.
ggplot(subsample_words, aes(n/total)) + 
    geom_histogram(show.legend = FALSE) + 
    xlim(NA, 0.0002)

head(freq_by_rank_subsample, 5)
tail(freq_by_rank_subsample, 5)
#The rank column here tells us the rank of each word within the frequency table; 
#the table was already ordered by n so we could use row_number() to find the rank. 
#Then, we can calculate the term frequency in the same way we did before. 
#Zipf's law is often visualized by plotting rank on the x-axis and term frequency 
#on the y-axis, on logarithmic scales. Plotting this way, an inversely proportional 
#relationship will have a constant, negative slope.
freq_by_rank_subsample %>% 
    ggplot(aes(rank, `term frequency`)) + 
    geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
    scale_x_log10() +
    scale_y_log10()    
#Ideally, this should be a 45 degree angle

#NOTE - the figure above is in log-log coordinates. 
#Let's see if we can improve this slope. We can view this as a "broken power law" with three sections. 
#Let's see what the exponent of the power law is for the middle section of hte rank range.
rank_subset_subsample = freq_by_rank_subsample %>%
    filter(rank < 1000, rank > 10)

broken_power_model = lm(log10(`term frequency`) ~ log10(rank), data = rank_subset_subsample)    
summary(broken_power_model)
pm_intercept = broken_power_model$coefficients[1]
pm_slope = broken_power_model$coefficients[2]
#Ideally, we want the slope close to -1. This is not the case here (around -0.5)

#Let's plot this fited power law with the data to visualize it:
freq_by_rank_subsample %>%
    ggplot(aes(rank, `term frequency`)) +
    geom_abline(intercept = pm_intercept, slope = pm_slope, color = "gray50", linetype = 2) +
    geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) +
    scale_x_log10() +
    scale_y_log10()
#YIKES!
# The deviations we see here at high rank are not uncommon for many kinds of language; 
# a corpus of language often contains fewer rare words than predicted by a single power law. 
# The deviations at low rank are more unusual. Jane Austen uses a lower percentage of the most common 
# words than many collections of language. This kind of analysis could be extended to compare authors, 
# or to compare any other collections of text; it can be implemented simply using tidy data principles.

#3.3 The bind_tf_idf function
#The idea of tf-idf is to find the important words for the content of each document by decreasing the weight for 
#commonly used words and increasing the weight for words that are not used very much in a collection or corpus 
#of documents, in this case, the group of Jane Austen's novels as a whole. Calculating tf-idf attempts to find 
#the words that are important (i.e., common) in a text, but not too common. Let's do that now.
#The bind_tf_idf function in the tidytext package takes a tidy text dataset as input with one row per token (term), 
#per document. One column (word here) contains the terms/tokens, one column contains the documents (book in this case), 
#and the last necessary column contains the counts, how many times each document contains each term (n in this example). 
#We calculated a total for each book for our explorations in previous sections, but it is not necessary for the 
#bind_tf_idf function; the table only needs to contain all the words in each document.

subsample_words$document = "document"
subsample_words = subsample_words %>%
    bind_tf_idf(word, document, n) #The 2nd variable is necessary, but since all the words have the same 
#total, it's just filler and won't actually matter.
#It would matter if we had used a dataset that had a categorical variable like "source" or "book"

#Notice that idf and thus tf-idf are zero for these extremely common words. 
#In the example, These are all words that appear in all six of Jane Austen's novels, 
#so the idf term (which will then be the natural log of 1) is zero. 
#The inverse document frequency (and thus tf-idf) is very low (near zero) for words that occur in many of the documents in a collection; 
#this is how this approach decreases the weight for common words. The inverse document frequency will be a higher number for words that 
#occur in fewer of the documents in the collection.
summary(subsample_words$tf)
subsample_words %>%
    arrange(desc(idf))
#idf and tf_idf should be 0 for extremely common words
subsample_words %>%
    arrange(idf)

###
# Create the type to token ratio (TTR):  
# This demonstrates how rich/diverse the language is in a corpus. 
# The higher TTR the more unique words are used to achieve to number of tokens
###
    word_tokens = sum(freq_by_rank_subsample$n)
    word_types = dim(freq_by_rank_subsample)[1]
    word_ttr = word_types/word_tokens
    word_ttr = data.frame("Source" = "Corpus", "Tokens" = word_tokens, "Types" = word_types, "TTR" = word_ttr)

#Remove dfs and objects no longer needed:
    rm(blogs, blogs_df, blogs_sample, broken_power_model, 
         filename, half_blogs, length, most_news, news, 
         news_df, news_sample, pm_intercept, pm_slope, 
         rank_subset_subsample, tenth_twitter, total_subsample_words, 
         twitter, twitter_df, twitter_sample)
    
#Save the entire workspace as an RData file:        
    save.image(file="part1.RData")    