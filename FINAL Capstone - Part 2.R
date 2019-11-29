####################################################
#DATA SCIENCE SPECIALIZATION########################
#CAPSTONE###########################################
#PART 2#############################################
####################################################

#Set working directory
    setwd("C:/Users/ajohns34/Box/Data Science Specialization/Capstone/data/en_US")

#Load in Part 1 into the R workspace:
    load(file="part1.RData")

#Evaluate n-grams instead of words
ngram_subsample = function(n_gram, numb) {
    n_gram = data.frame(subsample_df %>%
                            mutate(text = str_remove_all(text, remove_reg)) %>% ##Remove unwanted characters like "&"
                            unnest_tokens(ngram, text, token = "ngrams", n = numb)) ##Restructure the data so that each ngram is on a separate row        
    return(n_gram)
}    

bigram = ngram_subsample("bigram", 2)
trigram = ngram_subsample("trigram", 3)
fourgram = ngram_subsample("fourgram", 4)    

#As one might expect, a lot of the most common bigrams are pairs of common words, 
#such as "of the" and "to be": what are called "stop-words". 
#This is a useful time to use tidyr's separate(), which splits
#a column into multiple based on a delimiter. This lets us separate it into two 
#columns, "word1" and "word2", at which point we can remove cases where either 
#is a stop-word.
#In other analyses, we may want to work with the recombined words.
#tidyr's unite() function is the inverse of separate(), and lets us recombine the columns into one.
#Thus, "separate/filter/count/unite" let us find the most common bigrams not containing stop-words.

##Bi-grams##
    bigram_sep = bigram %>%
        separate(ngram, c("word1", "word2"), sep = " ")
    
    bigram_counts = bigram_sep %>%
        count(word1, word2, sort = TRUE)
    
    #Remove the "NA NA" row
    bigram_counts = na.omit(bigram_counts)
    
    #Create a new column that is the non-separated version of the bigram:
    bigram_counts$bigram = paste(bigram_counts$word1, bigram_counts$word2, sep=" ")
    
    #Calculate the total number of bigrams:
    total_subsample_bigrams = bigram_counts %>%
        plyr::summarize(total = sum(n))
    
    bigram_counts = cbind(bigram_counts, total_subsample_bigrams)
    
    #Zipf's law states that the frequency that a bigram appears is inversely proportional to its rank.
    bigram_freq_by_rank = bigram_counts %>%
        mutate(rank = row_number(), 
               `term frequency` = n/total)
    
    head(bigram_freq_by_rank, 5)
    tail(bigram_freq_by_rank, 5)
    
    #Create a variable that denotes cumulative count:
    bigram_freq_by_rank$`cum count` = cumsum(bigram_freq_by_rank$n)
    
    #Create a variable that denotes cumulative frequency:
    bigram_freq_by_rank$`cum freq` = cumsum(bigram_freq_by_rank$`term frequency`)
    
    #Create a variable that denotes the row number (this will be used later to find out how many words are 50% of the corpus)
    bigram_freq_by_rank$rownum = 1:dim(bigram_freq_by_rank)[1]
    
    #Create a frequency of frequency table that will be used later for Good-Turing Smoothing
    bigram_freqoffreq = data.frame(bi=table(bigram_freq_by_rank$n))
    
   
##Tri-grams##
    trigram_sep = trigram %>%
        separate(ngram, c("word1", "word2", "word3"), sep = " ")
    
    trigram_counts = trigram_sep %>%
        count(word1, word2, word3, sort = TRUE)
    
    #Remove the "NA NA NA" row
    trigram_counts = na.omit(trigram_counts)
    
    #Create a new column that is the non-separated version of the trigram:
    trigram_counts$trigram = paste(trigram_counts$word1, trigram_counts$word2, trigram_counts$word3, sep=" ")
    
    #Create a new column that is the bigram prefix of the trigram (so word1 + word2):
    trigram_counts$bigram_prefix = paste(trigram_counts$word1, trigram_counts$word2, sep=" ")
    
    #Calculate the total number of trigrams:
    total_subsample_trigrams = trigram_counts %>%
        plyr::summarize(total = sum(n))

    trigram_counts = cbind(trigram_counts, total_subsample_trigrams)
    
    #Zipf's law states that the frequency that a trigram appears is inversely proportional to its rank.
    trigram_freq_by_rank = trigram_counts %>%
        mutate(rank = row_number(), 
               `term frequency` = n/total)
    
    head(trigram_freq_by_rank, 5)
    tail(trigram_freq_by_rank, 5)
    
    #Create a variable that denotes cumulative count:
    trigram_freq_by_rank$`cum count` = cumsum(trigram_freq_by_rank$n)
    
    #Create a variable that denotes cumulative frequency:
    trigram_freq_by_rank$`cum freq` = cumsum(trigram_freq_by_rank$`term frequency`)
    
    #Create a variable that denotes the row number (this will be used later to find out how many words are 50% of the corpus)
    trigram_freq_by_rank$rownum = 1:dim(trigram_freq_by_rank)[1]
    
    #Create a frequency of frequency table that will be used later for Good-Turing Smoothing
    trigram_freqoffreq = data.frame(tri=table(trigram_freq_by_rank$n))
    
##Fourgrams##
    fourgram_sep = fourgram %>%
        separate(ngram, c("word1", "word2", "word3", "word4"), sep = " ")
    
    fourgram_counts = fourgram_sep %>%
        count(word1, word2, word3, word4, sort = TRUE)
    
    #Remove the "NA NA NA" row
    fourgram_counts = na.omit(fourgram_counts)
    
    #Create a new column that is the non-separated version of the fourgram:
    fourgram_counts$fourgram = paste(fourgram_counts$word1, fourgram_counts$word2, fourgram_counts$word3, fourgram_counts$word4, sep=" ")
    
    #Create a new column that is the trigram prefix of the trigram (so word1 + word2 + word3):
    fourgram_counts$trigram_prefix = paste(fourgram_counts$word1, fourgram_counts$word2, fourgram_counts$word3, sep=" ")
    
    #Calculate the total number of fourgrams:
    total_subsample_fourgrams = fourgram_counts %>%
        plyr::summarize(total = sum(n))
    
    fourgram_counts = cbind(fourgram_counts, total_subsample_fourgrams)
    
    #Zipf's law states that the frequency that a fourgram appears is inversely proportional to its rank.
    fourgram_freq_by_rank = fourgram_counts %>%
        mutate(rank = row_number(), 
               `term frequency` = n/total)
    
    
    #Create a variable taht denotes cumulative count:
    fourgram_freq_by_rank$`cum count` = cumsum(fourgram_freq_by_rank$n)
    
    #Create a variable that denotes cumulative frequency:
    fourgram_freq_by_rank$`cum freq` = cumsum(fourgram_freq_by_rank$`term frequency`)
    
    #Create a variable that denotes the row number (this will be used later to find out how many words are 50% of the corpus)
    fourgram_freq_by_rank$rownum = 1:dim(fourgram_freq_by_rank)[1]
    
    #Create a frequency of frequency table that will be used later for Good-Turing Smoothing
    fourgram_freqoffreq = data.frame(four=table(fourgram_freq_by_rank$n))
    
#Remove unnecessary objects in the environment:
    rm(bigram_filtered, trigram_filtered, 
       fourgram_counts, trigram_counts, 
       fourgram, trigram, fourgram_sep, trigram_sep, 
       bigram, bigram_counts, bigram_sep, subsample_df_word, 
       subsample_df, subsample, subsample_words, 
       subsample_words_noprofane, profane_words, 
       subsample_words_profane, nonenglish, ngram_subsample, 
       word_ttr, total_subsample_bigrams, total_subsample_trigrams, 
       total_subsample_fourgrams)
    
#Save the entire workspace as an RData file:        
    save.image(file="C:/Users/ajohns34/Desktop/parts1and2.RData") #Save it on desktop for now- not enough space in Box
