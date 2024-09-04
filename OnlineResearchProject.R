#Online Research Methods (ANU, S2 2023) – Assessment 3 - in R/igraph
#Australian National University SOCY2169
#Luis Carlos Lozano Sanabria Assessment 3 development - 11/2023

library(magrittr)
library(vosonSML)
library(igraph)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(readtext)
library(ggplot2)
library(dplyr)

#Collecting the data from the first reddit URL
myThreadUrls1 <- c("https://www.reddit.com/r/nbadiscussion/comments/14ulg7z/how_to_you_feel_about_the_nbas_new_in_season/")

#saved the raw data to rds file
redditData1 <- Authenticate("reddit") %>%
  Collect(threadUrls = myThreadUrls1, writeToFile = TRUE, verbose = TRUE)

#Create the network object (this is an activity network)
network1 <- redditData1 %>% Create("activity") %>% AddText(redditData1)

#Create the activity graph (igraph graph object)
g1 <- network1 %>% Graph(writeToFile = TRUE)

#visualisation of the network
png("A3-Thread_1_visualisation.png", width=800, height=700)
plot(g1, layout=layout_with_lgl(g1), vertex.label="", vertex.size=2, edge.width=1.5, edge.curved=.5, edge.arrow.size=0.5)
dev.off()

###---2---###
#Collecting the data from the second reddit URL
myThreadUrls2 <- c("https://www.reddit.com/r/nba/comments/17n7wf2/inseason_tournament_tips_off_in_indy/")

#saved the raw data to rds file
redditData2 <- Authenticate("reddit") %>%
  Collect(threadUrls = myThreadUrls2, writeToFile = TRUE, verbose = TRUE)

#Create the network object (this is an activity network)
network2 <- redditData2 %>% Create("activity") %>% AddText(redditData2)

#Create the activity graph (igraph graph object)
g2 <- network2 %>% Graph(writeToFile = TRUE)

#visualisation of the network
png("A3-Thread_2_visualisation.png", width=800, height=700)
plot(g2, layout=layout_with_lgl(g2), vertex.label="", vertex.size=2, edge.width=1.5, edge.curved=.5, edge.arrow.size=0.5)
dev.off()

# Conduct a preliminary analysis to get a sense of the themes in the first thread / corpus
reddit_corpus1 <- corpus(redditData1, text_field = 'comment')
ndoc(reddit_corpus1)
reddit_tokens1  <- reddit_corpus1 %>% tokens(remove_punct = TRUE, remove_numbers=TRUE, 
  remove_separators =TRUE, remove_url = TRUE) %>%  tokens_tolower() %>%
  tokens_remove(stopwords('english')) %>% tokens_ngrams(n=2:3)
reddit_dfm1  <-  dfm(reddit_tokens1)
topfeatures(reddit_dfm1, 50)
png("A3-Themes_wordcloud_visualisation.png", width=800, height=700)
textplot_wordcloud(reddit_dfm1, max_words = 50)
dev.off()

#Conduct a preliminary analysis to get a sense of the themes in the second thread / corpus
reddit_corpus2 <- corpus(redditData2, text_field = 'comment')
ndoc(reddit_corpus2)
reddit_tokens2  <- reddit_corpus2 %>% tokens(remove_punct = TRUE, remove_numbers=TRUE, 
  remove_separators =TRUE, remove_url = TRUE) %>%  tokens_tolower() %>%
  tokens_remove(stopwords('english')) %>% tokens_ngrams(n=2:3)
reddit_dfm2  <-  dfm(reddit_tokens2)
topfeatures(reddit_dfm2, 50)
png("A3-Themes_wordcloud2_visualisation.png", width=800, height=700)
textplot_wordcloud(reddit_dfm2, max_words = 50)
dev.off()

##---SENTIMENT ANALYSIS (1ST CORPUS)---##

#convert corpus to tokens and apply sentiment dictionary to code tokens (1st corpus)
sentiment_tokens1  <- reddit_corpus1 %>% tokens() %>% tokens_lookup(dictionary =
  data_dictionary_LSD2015)

#create document-feature matrix of sentiment (1st corpus)
dfm_sentiment1  <-  dfm(sentiment_tokens1)

#convert to a data.frame to allow further calculations (1st corpus)
sentiment_df1  <- convert(dfm_sentiment1, to = 'data.frame')

head(sentiment_df1)

#calculating sentiment score
#using logit scale sentiment score (1st corpus)
sentiment_df1$sent_score <- log(sentiment_df1$positive + sentiment_df1$neg_negative + 0.5)  -  
  log(sentiment_df1$negative + sentiment_df1$neg_positive + 0.5)
plot(density(sentiment_df1$sent_score))

#bring across all the other variables we have on the reddit data including scores, votes, etc
#remember: cbind means 'bind the columns' of data; like pasting a new column into a spreadsheet
sentiment_df1 <- cbind(sentiment_df1, docvars(dfm_sentiment1))

#calculating the average value of the sentiment score in the 1st corpus / dataframe
mean(sentiment_df1$sent_score)

#Keywords and sentiment
#Calculating range score (1st corpus)
sentiment_df1$range  <- sentiment_df1$positive  +
  sentiment_df1$neg_negative - sentiment_df1$negative - sentiment_df1$neg_positive

boxplot(sentiment_df1$range)

plot(sentiment_df1$range, sentiment_df1$sent_score)

#What keywords tend to be associated with negative/positive sentiment? (1st corpus)
sentiment_df1$sent_overall <- ifelse(sentiment_df1$range>0, "positive", 
  ifelse(sentiment_df1$range==0, "neutral", "negative"))

#add sent_overall to the original reddit dataframe, so we can use it as a document variable
#(1st corpus and dataframe)
df1 <- sentiment_df1 %>% select(thread_id, id, sent_overall)
redditData1<- left_join(redditData1, df1, by=c("thread_id","id"))

#if we are wanting comparison cloud of just comments that are overall positive or negative (ie. 
#exclude the
#neutral sentiment comments), the neutral comments could be removed from the dataframe before
#constructing the corpus. But they can also be removed after constructing the corpus

reddit_corpus1  <-  corpus(redditData1, text_field = 'comment') %>% corpus_subset(sent_overall %in% c("positive", "negative"))

reddit_tokens1  <- reddit_corpus1 %>% tokens( remove_punct = TRUE, remove_numbers=TRUE, 
  remove_separators =TRUE, remove_url = TRUE) %>%  tokens_tolower() %>%
  tokens_remove(stopwords('english')) %>% tokens_ngrams(n=2:3)
reddit_dfm1  <-  dfm(reddit_tokens1)

#Wordcloud (1st corpus)
textplot_wordcloud(reddit_dfm1, max_words = 100)

#Comparison cloud (1st corpus)
reddit_dfm1 <- dfm_group(reddit_dfm1, reddit_dfm1$sent_overall) %>%
  dfm_trim(min_termfreq = 3)

png("A3-Comparison_wordcloud1_visualisation.png", width=800, height=700)
textplot_wordcloud(reddit_dfm1, comparison = TRUE, max_words = 100,
  color = c("grey", "black"))
dev.off()
 
##---SENTIMENT ANALYSIS (2ND CORPUS)---##
#convert corpus to tokens and apply sentiment dictionary to code tokens (2nd corpus)
sentiment_tokens2  <- reddit_corpus2 %>% tokens() %>% tokens_lookup(dictionary =
  data_dictionary_LSD2015)

#create document-feature matrix of sentiment (2nd corpus)
dfm_sentiment2  <-  dfm(sentiment_tokens2)

#convert to a data.frame to allow further calculations (2nd corpus)
sentiment_df2  <- convert(dfm_sentiment2, to = 'data.frame')

head(sentiment_df2)

#calculating sentiment score
#using logit scale sentiment score (2nd corpus)
sentiment_df2$sent_score <- log(sentiment_df2$positive + sentiment_df2$neg_negative + 0.5)  -  
  log(sentiment_df2$negative + sentiment_df2$neg_positive + 0.5)
plot(density(sentiment_df2$sent_score))

#bring across all the other variables we have on the reddit data including scores, votes, etc
#remember: cbind means 'bind the columns' of data; like pasting a new column into a spreadsheet
sentiment_df2 <- cbind(sentiment_df2, docvars(dfm_sentiment2))

#calculating the average value of the sentiment score in the 2nd corpus / dataframe
mean(sentiment_df2$sent_score)

#Keywords and sentiment
#Calculating range score (2nd corpus)
sentiment_df2$range  <- sentiment_df2$positive  +
  sentiment_df2$neg_negative - sentiment_df2$negative - sentiment_df2$neg_positive

boxplot(sentiment_df2$range)

plot(sentiment_df2$range, sentiment_df2$sent_score)

#What keywords tend to be associated with negative/positive sentiment? (2nd corpus)
sentiment_df2$sent_overall <- ifelse(sentiment_df2$range>0, "positive", 
  ifelse(sentiment_df2$range==0, "neutral", "negative"))

#add sent_overall to the original reddit dataframe, so we can use it as a document variable
#(2nd corpus and dataframe)
df2 <- sentiment_df2 %>% select(thread_id, id, sent_overall)
redditData2<- left_join(redditData2, df2, by=c("thread_id","id"))

#if we are wanting comparison cloud of just comments that are overall positive or negative (ie. 
#exclude the
#neutral sentiment comments), the neutral comments could be removed from the dataframe before
#constructing the corpus. But they can also be removed after constructing the corpus

reddit_corpus2  <-  corpus(redditData2, text_field = 'comment') %>% corpus_subset(sent_overall %in% c("positive", "negative"))

reddit_tokens2  <- reddit_corpus2 %>% tokens(remove_punct = TRUE, remove_numbers=TRUE, 
  remove_separators =TRUE, remove_url = TRUE) %>%  tokens_tolower() %>%
  tokens_remove(stopwords('english')) %>% tokens_ngrams(n=2:3)
reddit_dfm2  <-  dfm(reddit_tokens2)

#Wordcloud (2nd corpus)
textplot_wordcloud(reddit_dfm2, max_words = 100)

#Comparison cloud (2nd corpus)
reddit_dfm2 <- dfm_group(reddit_dfm2, reddit_dfm2$sent_overall) %>%
  dfm_trim(min_termfreq = 3)

png("A3-Comparison_wordcloud2_visualisation.png", width=800, height=700)
textplot_wordcloud(reddit_dfm2, comparison = TRUE, max_words = 100,
                   color = c("grey", "black"))
dev.off()

###---NEW THREAD---###
#Collecting the data from the new reddit URL
myThreadUrls <- c("https://www.reddit.com/r/nba/comments/17m92df/the_nba_inseason_tournament_will_turn_into_a_joke/")

#saved the raw data to rds file
redditData <- Authenticate("reddit") %>%
  Collect(threadUrls = myThreadUrls, writeToFile = TRUE, verbose = TRUE)

#Create the network object (this is an activity network)
network <- redditData %>% Create("activity") %>% AddText(redditData)

#Create the activity graph (igraph graph object)
g <- network %>% Graph(writeToFile = TRUE)

#Conduct a preliminary analysis to get a sense of the themes in the new thread / corpus
reddit_corpus <- corpus(redditData, text_field = 'comment')
ndoc(reddit_corpus)
reddit_tokens  <- reddit_corpus %>% tokens(remove_punct = TRUE, remove_numbers=TRUE, 
                                             remove_separators =TRUE, remove_url = TRUE) %>%  tokens_tolower() %>%
  tokens_remove(stopwords('english')) %>% tokens_ngrams(n=2:3)
reddit_dfm  <-  dfm(reddit_tokens)
topfeatures(reddit_dfm, 50)
textplot_wordcloud(reddit_dfm, max_words = 50)

#create tokens
reddit_tokens_aux  <- tokens(reddit_corpus, remove_numbers = TRUE, remove_punct = TRUE, remove_separators = TRUE, remove_url = TRUE )
print(reddit_tokens_aux)

reddit_tokens_lower  <- tokens_tolower(reddit_tokens_aux)
reddit_tokens_clean  <- tokens_remove(reddit_tokens_lower, c(stopwords('en')))
print(reddit_tokens_clean)
reddit_tokens_ngram <- tokens_ngrams(reddit_tokens_clean, n = 1:2)

#explore keywords and plot their distribution within documents
reddit_kwic <- kwic(reddit_tokens_ngram, c('season_game'))
head(reddit_kwic)
textplot_xray(reddit_kwic) + aes(color = keyword) + ggtitle(paste('Keywords in context:', 'season_game'))

##---SENTIMENT ANALYSIS (NEW CORPUS)---##

#convert corpus to tokens and apply sentiment dictionary to code tokens
sentiment_tokens  <- reddit_corpus %>% tokens() %>% tokens_lookup(dictionary =
                                                                      data_dictionary_LSD2015)

#create document-feature matrix of sentiment
dfm_sentiment  <-  dfm(sentiment_tokens)

#convert to a data.frame to allow further calculations
sentiment_df  <- convert(dfm_sentiment, to = 'data.frame')

head(sentiment_df)

#calculating sentiment score
#using logit scale sentiment score
sentiment_df$sent_score <- log(sentiment_df$positive + sentiment_df$neg_negative + 0.5)  -  
  log(sentiment_df$negative + sentiment_df$neg_positive + 0.5)
plot(density(sentiment_df$sent_score))

#bring across all the other variables we have on the reddit data including scores, votes, etc
#remember: cbind means 'bind the columns' of data; like pasting a new column into a spreadsheet
sentiment_df <- cbind(sentiment_df, docvars(dfm_sentiment))

#calculating the average value of the sentiment score in the new corpus / dataframe
mean(sentiment_df$sent_score)

#Keywords and sentiment
#Calculating range score
sentiment_df$range  <- sentiment_df$positive  +
  sentiment_df$neg_negative - sentiment_df$negative - sentiment_df$neg_positive

boxplot(sentiment_df$range)

plot(sentiment_df$range, sentiment_df$sent_score)

#What keywords tend to be associated with negative/positive sentiment?
sentiment_df$sent_overall <- ifelse(sentiment_df$range>0, "positive", 
                                     ifelse(sentiment_df$range==0, "neutral", "negative"))

#add sent_overall to the original reddit dataframe, so we can use it as a document variable
#(new corpus and dataframe)
df <- sentiment_df %>% select(thread_id, id, sent_overall)
redditData<- left_join(redditData, df, by=c("thread_id","id"))

#if we are wanting comparison cloud of just comments that are overall positive or negative (ie. 
#exclude the
#neutral sentiment comments), the neutral comments could be removed from the dataframe before
#constructing the corpus. But they can also be removed after constructing the corpus

reddit_corpus  <-  corpus(redditData, text_field = 'comment') %>% corpus_subset(sent_overall %in% c("positive", "negative"))

reddit_tokens  <- reddit_corpus %>% tokens( remove_punct = TRUE, remove_numbers=TRUE, 
                                              remove_separators =TRUE, remove_url = TRUE) %>%  tokens_tolower() %>%
  tokens_remove(stopwords('english')) %>% tokens_ngrams(n=2:3)
reddit_dfm  <-  dfm(reddit_tokens)

#Wordcloud
textplot_wordcloud(reddit_dfm, max_words = 100)

#Comparison cloud 
reddit_dfm <- dfm_group(reddit_dfm, reddit_dfm$sent_overall) %>%
  dfm_trim(min_termfreq = 3)

png("A3-Comparison_wordcloud3_visualisation.png", width=800, height=700)
textplot_wordcloud(reddit_dfm, comparison = TRUE, max_words = 100,
                   color = c("grey", "black"))
dev.off()


















































