####################################################
#DATA SCIENCE SPECIALIZATION########################
#CAPSTONE###########################################
#PART 3#############################################
####################################################

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


gc()
memory.limit(size = 15000)
setwd("C:/Users/ajohns34/Desktop/Final Capstone Submission")


    load(file="C:/Users/ajohns34/Desktop/Final Capstone Submission/parts1thru3sofar.RData")
    
    rm(fourgram_freq_by_rank, fourgramcount.1to5, 
       trigram_freq_by_rank, trigramcount.1to5, 
       fourgram_trigram_prefix, hist.gr, bigram_freq_by_rank, 
       trigram_bigram_prefix, bigramcount.1to5, bigramcount.2pl, 
       trigramcount.2pl, word1_bigram, fourgramcount.2pl, 
       trigramcount.6pl, bigramcount.6pl, freq_by_rank_subsample, 
       wordcount.1to5, fourgramcount.6pl, wordcount.2pl, wordcount.6pl, 
       word_freqoffreq, bigram_freqoffreq, trigram_freqoffreq, fourgram_freqoffreq, 
       gts_matrix, remove_reg)
    
    #Subset the biggest dfs with only necessary columns:
    wordcount=subset(wordcount, select=c(word, n, GTfreq, GTprob, prefix))
    bigramcount = subset(bigramcount, select=c(word1, word2, n, GTfreq, GTprob))
    trigramcount = subset(trigramcount, select=c(bigram_prefix, word1, word2, word3, 
                                                 n, GTfreq, GTprob))
    fourgramcount = subset(fourgramcount, select=c(trigram_prefix, word1, word2, word3, 
                                                   word4, n, GTfreq, GTprob))
    
    ##Due to memory limitations in ShinyApp, we need to dramatically reduce the 
    #size of each df
    wordcount = wordcount[order(-wordcount$n), ]
    bigramcount = bigramcount[order(-bigramcount$n), ]
    trigramcount = trigramcount[order(-trigramcount$n), ]
    fourgramcount = fourgramcount[order(-fourgramcount$n), ]
    
    wordcount = head(wordcount, 100000)
    bigramcount = head(bigramcount, 100000)
    trigramcount = head(trigramcount, 100000)
    fourgramcount = head(fourgramcount, 100000)
    
    
    #Step 5. Run the model!
        #Step 5a. Define a function to clean the inputted phrase
            clean_inputphrase = function(input) {
                
                # 1. Make all text lowercase
                input = tolower(input)
                
                # 2. Remove punctuation from phrase
                input = gsub("[^[:alnum:][:space:]\']", "",input)
                input <- gsub("“", "", input)
                input <- gsub("”", "", input)
                input <- gsub("‘", "", input)
                input <- gsub("’", "", input)

                # 3. Separate words connected with "-" or "/" in the phrase
                input = gsub("-", " ", input)
                input = gsub("/", " ", input)
                
                # 4. Identify the end of the phrase
                input = gsub("\\? |\\?$|\\! |\\!$", " EEOSS ", input)
                input = gsub("[A-Za-z]\\.[A-Za-z]\\.[A-Za-z]\\.[A-Za-z]\\. |[A-Za-z]\\.[A-Za-z]\\.[A-Za-z]\\. |[A-Za-z]\\.[A-Za-z]\\. ", " AABRR ", input)
                input = gsub("\\. |\\.$", " EEOSS ", input)
                input = gsub("[0-9]+"," NNUMM ",input)
                input = gsub("\\S+@\\S+","EEMAILL",input) 
                input = gsub("[Hh}ttp([^ ]+)","HHTMLL",input) 
                input = gsub("RT | via"," RTVIA ",input) # retweets
                input = gsub("@([^ ]+)","ATPPLE",input) # @people
                input = gsub("[@][a - zA - Z0 - 9_]{1,15}","UUSRNMSS",input) # usernames
                
                # 5. Replace any contractions - "n't" = "not" 
                #Remove/replace &, @, 'm, 's, 'are, 'll,
                input = replace_contraction(input)
                #The list of contractions in replace_contractions is not complete - add these in just in case
                    input = gsub("it's", "it is", input)  
                    input = gsub("haven't", "have not", input)
                    input = gsub("hadn't", "had not", input)
                    input = gsub("n't" , "not", input)
                    input = gsub("'ve", "have", input)
                    input = gsub("'d", "would", input)
                    input = gsub("'ll", "will", input)
                    input = gsub("'re", "are", input)
                    
                input = gsub(" & ", " and ", input) #Make & = "and"
                input = gsub(" @ ", " at ", input) #Make @ = "at"
                
                
                # 6. Remove emoji's, emoticons from the phrase - usually a problem in Twitter
                input = gsub("[^\x01-\x7F]", "", input)
                
                # 7. Remove measurements of mass and time (e.g., g, mg, lbs)  
                input = gsub(" [1-9]+g ", " ", input) #grams
                input = gsub(" +g ", " ", input) #grams
                input = gsub(" [1-9]+mg ", " ", input) #miligrams
                input = gsub(" +mg ", " ", input) #miligrams
                input = gsub(" [1-9]+kg ", " ", input) #kilograms
                input = gsub(" +kg ", " ", input) #kilograms
                input = gsub(" +kg ", " ", input) #kilograms
                input = gsub(" [1-9]+lbs ", " ", input) #pounds
                input = gsub(" +lbs ", " ", input) #pounds
                input = gsub(" +lbs ", " ", input) #pounds
                input = gsub(" [1-9]+s ", " ", input) #seconds
                input = gsub(" +s ", " ", input) #seconds
                input = gsub(" [1-9]+m ", " ", input) #minutes
                input = gsub(" +m ", " ", input) #minutes
                input = gsub(" [1-9]+h ", " ", input) #hours
                input = gsub(" +h ", " ", input) #hours
                
                # 8. Remove all single letters except i, b, c, u, and a
                input = gsub(" b ", " be ", input)
                input = gsub(" c ", " see ", input)
                input = gsub(" u ", " you ", input)
                input = gsub(" [b-hj-z] ", " ", input)
                
                #Replace any common internet slang:
                input = gsub(" jk ", " just kidding ", input)
                input = gsub(" lol ", " laughing out loud ", input)
                input = gsub(" rofl ", " rolling on floor laughing ", input)
                input = gsub(" stfu ", " shut the fuck up ", input)
                input = gsub(" lmk ", " let me know ", input)
                input = gsub(" ily ", " i love you ", input)
                input = gsub(" ilysm ", " i love you so much ", input)
                input = gsub(" yolo ", " you only live once ", input)
                input = gsub(" smh ", " shaking my head ", input)
                input = gsub(" lmfao ", " laughing my fucking head off ", input)
                input = gsub(" nvm ", " never mind ", input)
                input = gsub(" ikr ", " i know right ", input)
                input = gsub(" bae ", " before anyone else ", input)
                input = gsub(" lmk", "let me know", input)
                
                # 9. Exclude list of profane words/phrases
                #Source of profane word list: https://www.cs.cmu.edu/~biglou/resources/
                profane_words = read.delim("./profane_words.txt", header = FALSE, sep="\n") #sep is signaling "new line"
                input = removeWords(input, profane_words[,1])
                
                # 10. Remove extra spaces in phrase
                input = stripWhitespace(input)
                # Remove space at beginning of phrase
                input = gsub("^ ", "", input )
                # Remove space at end of phrase
                input = gsub(" $", "", input)
                return(input)
            }
            
        #Step 5b. Define 4 functions to run KBO from words, bigrams, trigrams, and fourgrams
            #Word
            predict.onegram = function(wordcount, gram2.w1) {
                #Get a subset where prefix matches gram2.w1
                prefix.gram1.match = wordcount[which(wordcount["word"]==gram2.w1), ]
                
                #Calculate Katzprob for gram 1:
                prefix.gram1.match$Kprob = prefix.gram1.match$GTprob
                
                #Rename variables
                names(prefix.gram1.match) = c("postfix", "n", "GTfreq", "GTprob", "prefix", "Kprob")
                prediction = prefix.gram1.match
                prediction = prediction[order(-prediction$Kprob), ]
                return(prediction)
            }
            #Bigram
            predict.bigram = function(bigramcount, wordcount, gram2.w1) {
                #Get a subset where prefix matches gram2.w1
                prefix.gram2.match = bigramcount[which(bigramcount$word1==gram2.w1), ]
                
                #Subset gram1 into words predicted by gram2 or not predicted
                gr1.in.gr2 = wordcount[wordcount$word %in% prefix.gram2.match$word2, ]
                gr1.notin.gr2 = wordcount[!(wordcount$word %in% prefix.gram2.match$word2), ]
                
                #Calculate alpha for gram 1 (call gama the denominator of alpha)
                beta.gr1 = 1-sum(prefix.gram2.match$GTprob)
                gama.gr1 = 1-sum(gr1.in.gr2$GTprob)
                alpha.gr1 = beta.gr1/gama.gr1
                
                #Calculate KatzProb for gram 1
                gr1.notin.gr2$Kprob = gr1.notin.gr2$GTprob*alpha.gr1
                    names(gr1.notin.gr2) = c("postfix", "n", "GTfreq", "GTprob", "prefix", "Kprob")
                    
                #Calculate KatzProb for gram 2
                prefix.gram2.match$Kprob = prefix.gram2.match$GTprob
                    names(prefix.gram2.match)
                    #Drop some variables and rename so that they match with gr1.notin.gr2
                    prefix.gram2.match = prefix.gram2.match[, c("word1", "word2", "n", "GTfreq", "GTprob", "Kprob")]
                    prefix.gram2.match = prefix.gram2.match[, c(2, 3, 4, 5, 1, 6)]    
                    names(prefix.gram2.match) = c("postfix", "n", "GTfreq", "GTprob", "prefix", "Kprob")
                
                #Bind the rows of the gr1 and gr matches 
                prediction = rbind(prefix.gram2.match, 
                                   gr1.notin.gr2)
                prediction = prediction[order(-prediction$Kprob), ]
                return(prediction)
                
            }
            #Trigram
            predict.trigram = function(trigramcount, bigramcount, wordcount, gram3.w12, gram2.w1) {
                #Gram 3 match and backoff to gram 2
                    #Subset from gram 3 where prefix matches gram3.w12
                    prefix.gram3.match = trigramcount[which(trigramcount$bigram_prefix==gram3.w12), ]
                    #Subset from gram 2 where prefix matches gram2.w1
                    prefix.gram2.match = bigramcount[which(bigramcount$word1==gram2.w1), ]
                    
                    #Subset from gram 2 into words predicted by gram 3 or not predicted:
                    gr2.in.gr3 = prefix.gram2.match[prefix.gram2.match$word2 %in% 
                                                      prefix.gram3.match$word3, ]
                    gr2.notin.gr3 = prefix.gram2.match[!(prefix.gram2.match$word2 %in% 
                                                           prefix.gram3.match$word3), ]
                    
                    #Calculate alpha for gram 2 (call gama the denominator of alpha)
                    beta.gr2 = 1 - sum(prefix.gram3.match$GTprob)
                    gama.gr2 = 1 - sum(gr2.in.gr3$GTprob)
                    alpha.gr2 = beta.gr2/gama.gr2
                    
                    #Calculate KatzProb (Kprob) for gram 2
                    gr2.notin.gr3$Kprob = gr2.notin.gr3$GTprob *alpha.gr2
                        names(gr2.notin.gr3)
                        gr2.notin.gr3 = gr2.notin.gr3[, c("word1", "word2", "n", "GTfreq", "GTprob", "Kprob")]
                        gr2.notin.gr3 = gr2.notin.gr3[, c(2, 3, 4, 5, 1, 6)]    
                        names(gr2.notin.gr3) = c("postfix", "n", "GTfreq", "GTprob", "prefix", "Kprob")
                    #Calculate KatzProb for gram 3
                    prefix.gram3.match$Kprob = prefix.gram3.match$GTprob
                        names(prefix.gram3.match)
                        prefix.gram3.match = prefix.gram3.match[, c("bigram_prefix", "word3", "n", "GTfreq", "GTprob", "Kprob")]
                        names(prefix.gram3.match) = c("prefix", "postfix", "n", "GTfreq", "GTprob", "Kprob")
                        prefix.gram3.match = prefix.gram3.match[, c(2, 3, 4, 5, 1, 6)]
                        
                #If no matches, backoff to gram 1
                    #Subset gram1 into words predicted by gram2 or not predicted
                    gr1.in.gr2 = wordcount[wordcount$word %in% prefix.gram2.match$word2, ]
                    gr1.notin.gr2 = wordcount[!(wordcount$word %in% prefix.gram2.match$word2), ]
                    
                    #Calculate alpha for gram 1 (call gama the denominator of alpha)
                    beta.gr1 = 1-sum(prefix.gram2.match$GTprob)
                    gama.gr1 = 1-sum(gr1.in.gr2$GTprob)
                    alpha.gr1 = beta.gr1/gama.gr1
                    
                    #Calculate KatzProb for gram 1
                    gr1.notin.gr2$Kprob = gr1.notin.gr2$GTprob * alpha.gr1 * alpha.gr2
                        names(gr1.notin.gr2) = c("postfix", "n", "GTfreq", "GTprob", "prefix", "Kprob")
                        
                    #Bind the rows for gr1 and gr matches and then sort
                    prediction = rbind(prefix.gram3.match, 
                                   gr2.notin.gr3, 
                                   gr1.notin.gr2)
                    prediction = prediction[order(-prediction$Kprob), ]
                return(prediction)
                
            }
            
            #Fourgram
            predict.fourgram = function(fourgramcount, trigramcount, bigramcount, wordcount, 
                                           gram4.w123, gram3.w12, gram2.w1) {
                #Gram 4 match and backoff to gram 1:
                    #Subset from gram 4 where prefix matches gram4.w123
                    prefix.gram4.match = fourgramcount[which(fourgramcount$trigram_prefix==gram4.w123), ]
                    
                    #Subset from gram 3 where prefix matches gram3.w12
                    prefix.gram3.match = trigramcount[which(trigramcount$bigram_prefix==gram3.w12), ]
                    
                    #Subset from gram 3 into words predicted by gram 4 or not predicted
                    gr3.in.gr4 = prefix.gram3.match[prefix.gram3.match$postfix %in% prefix.gram4.match$postfix, ]
                    gr3.notin.gr4 = prefix.gram3.match[!(prefix.gram3.match$postfix %in% prefix.gram4.match$postfix), ]
                    
                    #Calculate alpha for gram 2 (call gama the denominator of alpha)
                    beta.gr3 = 1-sum(prefix.gram4.match$GTprob)
                    gama.gr3 = 1-sum(gr3.in.gr4$GTprob)
                    alpha.gr3 = beta.gr3/gama.gr3
                    
                    #Calculate KatzProb (Kprob) for gram 3
                    gr3.notin.gr4$Kprob = gr3.notin.gr4$GTprob * alpha.gr3
                        gr3.notin.gr4 = gr3.notin.gr4[, c("bigram_prefix", "word3", "n", "GTfreq", "GTprob", "Kprob")]
                        names(gr3.notin.gr4) = c("prefix", "postfix", "n", "GTfreq", "GTprob", "Kprob")
                        gr3.notin.gr4 = gr3.notin.gr4[, c(2, 3, 4, 5, 1, 6)]
                    
                    #Calculate KatzProb (Kprob) for gram 4
                    prefix.gram4.match$Kprob = prefix.gram4.match$GTprob
                        prefix.gram4.match = prefix.gram4.match[, c("trigram_prefix", "word4", "n", "GTfreq", "GTprob", "Kprob")]
                        names(prefix.gram4.match) = c("prefix", "postfix", "n", "GTfreq", "GTprob", "Kprob")
                        prefix.gram4.match = prefix.gram4.match[, c(2, 3, 4, 5, 1, 6)]
                        
                #Gram 3 match and backoff to gram 2
                    #Subset from gram 3 where prefix matches gram3.w12
                    prefix.gram3.match = trigramcount[which(trigramcount$bigram_prefix==gram3.w12), ]
                    #Subset from gram 2 where prefix matches gram2.w1
                    prefix.gram2.match = bigramcount[which(bigramcount$word1==gram2.w1), ]
                    
                    #Subset from gram 2 into words predicted by gram 3 or not predicted:
                    gr2.in.gr3 = prefix.gram2.match[prefix.gram2.match$word2 %in% 
                                                      prefix.gram3.match$word3, ]
                    gr2.notin.gr3 = prefix.gram2.match[!(prefix.gram2.match$word2 %in% 
                                                           prefix.gram3.match$word3), ]
                    
                    #Calculate alpha for gram 2 (call gama the denominator of alpha)
                    beta.gr2 = 1 - sum(prefix.gram3.match$GTprob)
                    gama.gr2 = 1 - sum(gr2.in.gr3$GTprob)
                    alpha.gr2 = beta.gr2/gama.gr2
                    
                    #Calculate KatzProb (Kprob) for gram 2
                    gr2.notin.gr3$Kprob = gr2.notin.gr3$GTprob * alpha.gr2 * alpha.gr3
                        names(gr2.notin.gr3)
                        gr2.notin.gr3 = gr2.notin.gr3[, c("word1", "word2", "n", "GTfreq", "GTprob", "Kprob")]
                        gr2.notin.gr3 = gr2.notin.gr3[, c(2, 3, 4, 5, 1, 6)]    
                        names(gr2.notin.gr3) = c("postfix", "n", "GTfreq", "GTprob", "prefix", "Kprob")
                #If no matches, backoff to gram 1
                    #Subset gram1 into words predicted by gram2 or not predicted
                    gr1.in.gr2 = wordcount[wordcount$word %in% prefix.gram2.match$word2, ]
                    gr1.notin.gr2 = wordcount[!(wordcount$word %in% prefix.gram2.match$word2), ]
                    
                    #Calculate alpha for gram 1 (call gama the denominator of alpha)
                    beta.gr1 = 1-sum(prefix.gram2.match$GTprob)
                    gama.gr1 = 1-sum(gr1.in.gr2$GTprob)
                    alpha.gr1 = beta.gr1/gama.gr1
                    
                    #Calculate KatzProb for gram 1
                    gr1.notin.gr2$Kprob = gr1.notin.gr2$GTprob * alpha.gr1 * alpha.gr2 * alpha.gr3
                        names(gr1.notin.gr2) = c("postfix", "n", "GTfreq", "GTprob", "prefix", "Kprob")
                        
                    #Bind the rows for gr1 and gr matches and then sort
                    prediction = rbind(prefix.gram4.match, 
                                       gr3.notin.gr4, 
                                       gr2.notin.gr3, 
                                       gr1.notin.gr2)
                    prediction = prediction[order(-prediction$Kprob), ]
                    return(prediction)
                    
            }
            
        #Step 5c. Define the main function that takes a phrase, runs Step 5a, runs Step 5b, 
            #and then outputs a table with the predicted next word(s)
            nextword = function(input, wordcount, bigramcount, trigramcount, fourgramcount) {
                #Identify the number of words in the input:
                    n.words.input = length(strsplit(input, "\\s+")[[1]])
                    
                #Output an error message if input wasn't provided:
                    #if(n.words.input < 1) stop("At least one word is necessary") 
                    
                #Clean the input phrase and count the number of words after cleaning:
                    clean.input = clean_inputphrase(input) #This is using the function defined in 5a.
                    clean.input.words = strsplit(clean.input, "\\s+")[[1]] 
                    n.words = length(clean.input.words)
                    
                    #Once the input is cleaned, re-run to identify the number of words in the input:
                    n.words.input = length(strsplit(clean.input, "\\s+")[[1]])
                    
                    #Output an error message if input wasn't provided:
                    #if(n.words.input < 1) stop("At least one word is necessary. The algorithm may have filtered out words it identifies as profane.") 
                    
                    
                    
                #Identify break words:
                    ngram.break = as.list(c("eeoss", "aabrr", "nnumm", "eemaill", "hhtmll", "rtvia", "atpple", "uusrnmss"))
                    
                #If the last word is a break word, or something that isn't english, stop the function
                    #if (any(unlist(lapply(ngram.break, function(x) grepl(x, clean.input.words[n.words])))))
                        #stop("The last sequence of characters is something other than an English word. \n", 
                             #"Please input at least one word.") #"\n" reports a new line.
                    
                #If the phrase is at least three words long:
                    if (n.words >=3) {
                        #Extract the (n-1) words from ngrams from the last words in the phrase
                            gram4.w123 = paste(clean.input.words[n.words - 2], 
                                               clean.input.words[n.words - 1], 
                                               clean.input.words[n.words], sep = " ")
                            gram3.w12 = sub("^[a-z]+ ","",gram4.w123)
                            #Trying this out:
                            gram2.w1 = word(gram3.w12, 2)
                            #Original: gram2.w1 = sub("^[a-z]+ ","",gram3.w12)
                            #gram1.w0 = sub("^[a-z]+ ","",gram3.w12)
                            
                            #If any of the words in the ngram4.w123 is a break word, then move to n-1 ngram
                                if (any(unlist(lapply(ngram.break, function(x) grepl(x,gram4.w123))))) {
                                    # if any of the words in the ngram3.w12 is a break word, then move to n-1 ngram
                                    if (any(unlist(lapply(ngram.break, function(x) grepl(x,gram3.w12))))) {
                                        ###Count the frequency of bigramcount$word1
                                        match.w1.count = sum(bigramcount[which(bigramcount$word1==gram2.w1),"GTfreq"])
                                            #If the user only inputted 1 word and there are no matches, output just the sorted wordcount table
                                            if (match.w1.count == 0) { 
                                                prediction = subset(wordcount, select=c(word, prefix))
                                                #prediction = subset(predict.onegram(gram2.w1), select=c(postfix, prefix))
                                                names(prediction) = c("NextWord", "PrecedingWords")
                                                prediction = prediction[, c(2, 1)]
                                                #prediction = predict.onegram(gram1.w0)
                                                #stop("This word is outside of our prediction capabilities. Please input another word.")
                                            }
                                            #If there IS a match, use the bigram prediction model
                                            else { 
                                                prediction = subset(predict.bigram(bigramcount, wordcount, gram2.w1), select=c(postfix, prefix))
                                                names(prediction) = c("NextWord", "PrecedingWords")
                                                prediction = prediction[, c(2, 1)]
                                            }
                                    
                                    }
                                    #If all of the words ARE NOT break words, execute from the 3-gram:
                                    else { 
                                        match.w12.count = sum(trigramcount[which(trigramcount$bigram_prefix==gram3.w12),"GTfreq"])
                                            #If no matches, use Katz backoff model to report the word from gram2.w1
                                            if (match.w12.count == 0) { 
                                                match.w1.count = sum(bigramcount[which(bigramcount$word1==gram2.w1),"GTfreq"])
                                                #If the user only inputted 1 word and there are no matches, output just the sorted wordcount table
                                                if (match.w1.count == 0) { 
                                                    prediction = subset(wordcount, select=c(word, prefix))
                                                    #prediction = subset(predict.onegram(gram2.w1), select=c(postfix, prefix))
                                                    names(prediction) = c("NextWord", "PrecedingWords")
                                                    prediction = prediction[, c(2, 1)]
                                                    #prediction = predict.onegram(gram1.w0)
                                                    #stop("This word is outside of our prediction capabilities. Please input another word.")
                                                }
                                                #If there IS a match, use the bigram prediction model
                                                else { 
                                                    prediction = subset(predict.bigram(bigramcount, wordcount, gram2.w1), select=c(postfix, prefix))
                                                    names(prediction) = c("NextWord", "PrecedingWords")
                                                    prediction = prediction[, c(2, 1)]
                                                }
                                            }
                                            #If there IS a match, use the trigram prediction model
                                            else { 
                                                prediction = subset(predict.trigram(trigramcount, bigramcount, wordcount, gram3.w12, gram2.w1), select=c(postfix, prefix))
                                                names(prediction) = c("NextWord", "PrecedingWords")
                                                prediction = prediction[, c(2, 1)]            
                                            }              
                                    }
                                
                                #If all of the words ARE NOT break words, execute from the 4-gram:
                                } else { 
                                    #Check for matches in the fourgram list and then work backwards using the Katz back-off model as necessary
                                    
                                    # Count the frequency of fourgramcount$trigramcount_prefix when it equals gram4.w123
                                    match.w123.count = sum(fourgramcount[which(fourgramcount$trigram_prefix==gram4.w123),"GTfreq"])
                                        #If no matches, use Katz backoff model to find the frequency of trigram$bigram_prefix when 
                                        #it equals gram3.w12
                                        if (match.w123.count == 0) { 
                                            match.w12.count = sum(trigramcount[which(trigramcount$bigram_prefix==gram3.w12),"GTfreq"])
                                                #If no matches, use Katz backoff model to find the frequency of bigram$word1 when
                                                #it equals gram2.w1
                                                if (match.w12.count == 0) { 
                                                    match.w1.count = sum(bigramcount[which(bigramcount$word1==gram2.w1),"GTfreq"])
                                                    #If the user only inputted 1 word and there are no matches, output just the sorted wordcount table
                                                    if (match.w1.count == 0) { 
                                                        prediction = subset(wordcount, select=c(word, prefix))
                                                        #prediction = subset(predict.onegram(gram2.w1), select=c(postfix, prefix))
                                                        names(prediction) = c("NextWord", "PrecedingWords")
                                                        prediction = prediction[, c(2, 1)]
                                                        #prediction = predict.onegram(gram1.w0)
                                                        #stop("This word is outside of our prediction capabilities. Please input another word.")
                                                    }
                                                    
                                                    #If there IS a match, use the bigram prediction model
                                                    else { 
                                                        prediction = subset(predict.bigram(bigramcount, wordcount, gram2.w1), select=c(postfix, prefix))
                                                        names(prediction) = c("NextWord", "PrecedingWords")
                                                        prediction = prediction[, c(2, 1)]
                                                    }
                                                }
                                                #If there IS a match, use the trigram prediction model
                                                else { 
                                                    prediction = subset(predict.trigram(trigramcount,bigramcount, wordcount, gram3.w12, gram2.w1), select=c(postfix, prefix))
                                                    names(prediction) = c("NextWord", "PrecedingWords")
                                                    prediction = prediction[, c(2, 1)]            
                                                }        
                                        }
                                        #If there IS a match, use the fourgram prediction model
                                        else { 
                                            prediction = subset(predict.fourgram(fourgramcount, trigramcount, bigramcount, wordcount, gram4.w123, gram3.w12, gram2.w1), select=c(postfix, prefix))
                                            names(prediction) = c("NextWord", "PrecedingWords")
                                            prediction = prediction[, c(2, 1)]
                                        }
                                }    
                    }
                    
                #If the phrase is two words long:
                    else if (n.words ==2) {
                        #Extract the (n-1) words from ngrams from the last words in the phrase
                        gram3.w12 = clean.input
                        #Trying this out:
                        gram2.w1 = word(gram3.w12, 2)
                        #Original: gram2.w1 = sub("^[a-z]+ ","",gram3.w12)
                        #gram1.w0 = sub("^[a-z]+ ","",gram3.w12)
                        
                        #If any of the words in the ngram3.w12 is a break word, then move to n-1 ngram
                        if (any(unlist(lapply(ngram.break, function(x) grepl(x,gram3.w12))))) {
                            #Check for matches in the trigram list and then work backwards using the Katz back-off model as necessary
                            match.w1.count = sum(bigramcount[which(bigramcount$word1==gram2.w1),"GTfreq"])
                            
                            #If the user only inputted 1 word and there are no matches, output just the sorted wordcount table
                            if (match.w1.count == 0) { 
                                prediction = subset(wordcount, select=c(word, prefix))
                                #prediction = subset(predict.onegram(gram2.w1), select=c(postfix, prefix))
                                names(prediction) = c("NextWord", "PrecedingWords")
                                prediction = prediction[, c(2, 1)]
                                #prediction = predict.onegram(gram1.w0)
                                #stop("This word is outside of our prediction capabilities. Please input another word.")
                            }
                            
                            #If there IS a match, run the bigram prediction model
                            else { 
                                prediction = subset(predict.bigram(bigramcount, wordcount, gram2.w1), select=c(postfix, prefix))
                                names(prediction) = c("NextWord", "PrecedingWords")
                                prediction = prediction[, c(2, 1)]
                            }
                            
                        }
                        #If there are NO break words, find the frequency of trigramcount$bigram_prefix when it equals gram3.w12
                        else { 
                            match.w12.count = sum(trigramcount[which(trigramcount$bigram_prefix==gram3.w12),"GTfreq"])
                            #If there are no matches, find the frequency of bigramcount$word1 when it equals gram2.w1
                            if (match.w12.count == 0) { 
                                match.w1.count = sum(bigramcount[which(bigramcount$word1==gram2.w1),"GTfreq"])
                                #If there are no matches, report the gram2.w1
                                if (match.w1.count == 0) { 
                                    #prediction = subset(predict.onegram(gram2.w1), select=c(postfix, prefix))
                                    prediction = subset(wordcount, select=c(word, prefix))
                                    names(prediction) = c("NextWord", "PrecedingWords")
                                    prediction = prediction[, c(2, 1)]
                                    #prediction = predict.onegram(gram1.w0)
                                    #stop("This word is outside of our prediction capabilities. Please input another word.")
                                }
                                #If there IS a match, run the bigram prediction model
                                else { 
                                    prediction = subset(predict.bigram(bigramcount, wordcount, gram2.w1), select=c(postfix, prefix))
                                    names(prediction) = c("NextWord", "PrecedingWords")
                                    prediction = prediction[, c(2, 1)]
                                }
                            }
                            #If there IS a match, run the trigram prediction model
                            else { 
                                prediction = subset(predict.trigram(trigramcount,bigramcount, wordcount, gram3.w12, gram2.w1), select=c(postfix, prefix))
                                    names(prediction) = c("NextWord", "PrecedingWords")
                                    prediction = prediction[, c(2, 1)]           
                            }              
                        }
                        
                        
                    }
             
                #If the phrase is one word long:     
                    else {
                        gram2.w1 = clean.input
                        #Count the frequency of bigramcount$word1 when it equals gram2.w1
                        match.w1.count = sum(bigramcount[which(bigramcount$word1==gram2.w1),"GTfreq"])
                        #If the user only inputted 1 word and there are no matches, output just the sorted wordcount table
                            if (match.w1.count == 0) { 
                                prediction = subset(wordcount, select=c(word, prefix))
                                #prediction = subset(predict.onegram(gram2.w1), select=c(postfix, prefix))
                                names(prediction) = c("NextWord", "PrecedingWords")
                                prediction = prediction[, c(2, 1)]
                            }
                            #If there IS a match, use the bigram prediction model
                            else { 
                                prediction = subset(predict.bigram(bigramcount, wordcount, gram2.w1), select=c(postfix, prefix))
                                names(prediction) = c("NextWord", "PrecedingWords")
                                prediction = prediction[, c(2, 1)]
                            }
                       
                    }       
                return(prediction)    
            }

#Save the model:
#save.image(file = "C:/Users/ajohns34/Desktop/Final Capstone Submission/NextWord_112719.RData") #Save on Desktop for now, not enough space in Box

#load(file="C:/Users/ajohns34/Desktop/Final Capstone Submission/NextWord_112719.RData")

save.image(file = "C:/Users/ajohns34/Desktop/Final Capstone Submission/NextWord_112919.RData") #Save on Desktop for now, not enough space in Box

load(file="C:/Users/ajohns34/Desktop/Final Capstone Submission/NextWord_112919.RData")

