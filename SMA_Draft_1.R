# load the RData xxx.RData
load("df.RData")


# academictwitterR doesn't report as many columns as the rtweet
# we need to find common columns to match them
# cleanup_df function will help you select the matching columns
# run line 9 to line 39 to define the cleanup_df function

require(tibble)
require(anytime)
require(lubridate)  # for with_tz, which sets the timezone used 

cols = c("user_id",
         "status_id",
         "created_at",
         "text",
         "source",
         "reply_to_status_id",
         "reply_to_user_id",
         "is_quote",
         "is_retweet",
         "favorite_count",
         "retweet_count",
         "quote_count",
         "reply_count",
         "hashtags",
         "urls_expanded_url",
         "mentions_user_id",
         "mentions_screen_name",
         "lang")

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

cleanup_df <- function(tb) {
  tb[,cols]
}

###Data Exploration###
library(dplyr)

#check the sources of the original dataset
table(beast_keke2$source)
beast_keke2 %>% 
  group_by(beast_keke2$source) %>%
  summarize(Count=n()) %>%
  mutate(Percent = round((Count/sum(Count)*100))) %>%
  arrange(desc(Count))

#clean by sources and keep those tweets only from Twitter for purpose of this study
keep <- c("Twitter for iPhone", "Twitter for Android", "Twitter Web App", "Twitter for iPad")
tweet_df = subset(beast_keke2, source %in% keep)

#what is the quote with most favorite_count?
temp=tweet_df[order(-tweet_df$favorite_count),]
temp$text[1:3]

#what is the tweet with most interactions?
temp=tweet_df[order(-tweet_df$reply_count),]
temp$text[1:3]

#are these social media messages effective? 
summary(tweet_df$favorite_count)
count_favorite=table(tweet_df$favorite_count)
par(mar = c(3,3,3,3))
barplot(count_favorite)
hist(tweet_df$favorite_count, nclass=20)

#which type of messages received more likes?
re_tweet = tweet_df[tweet_df$is_retweet==TRUE,]
quote_tweet = tweet_df[tweet_df$is_quote==TRUE,]

summary(re_tweet$favorite_count)
summary(quote_tweet$favorite_count)

#the most popular quoted tweets are
temp=quote_tweet[order(-quote_tweet$favorite_count),]
temp$text[1:3]

#study the tweets with external links
tweet_df$if_external_link <- ifelse(grepl("https://t.co/", tweet_df$text), 1, 0)
external_link_0 = tweet_df[grep(0, tweet_df$if_external_link),]
external_link_1 = tweet_df[grep(1, tweet_df$if_external_link),]
summary(external_link_1$favorite_count)

temp=external_link_1[order(-external_link_1$favorite_count),]
temp$text[1:5]


####Sentiment Analysis for the whole data set####
library(sentimentr)
library(textcat)
library(data.table)

language=textcat(tweet_df$text)
sort(table(language),decreasing=T)
tweet_df_eng=tweet_df[tweet_df$lang=="en",]
tweet_df_eng <- na.omit(tweet_df_eng$text)

tweet_df_text=get_sentences(tweet_df_eng)
tweet_df_sentence = sentiment(tweet_df_text)
tweet_df_sentence = as.data.table(tweet_df_sentence)
tweet_df_review=tweet_df_sentence[,
                    list(
                      review.sentiment = mean(sentiment)
                    ),
                    by=list(element_id)]
tweet_df_sentiment = cbind(tweet_df_eng, tweet_df_review)
summary(tweet_df_sentiment$review.sentiment)
hist(tweet_df_sentiment$review.sentiment)

####Sentiment Analysis for the data set containing external links####
#with external link
language=textcat(external_link_1$text)
external_1_eng=external_link_1[external_link_1$lang=="en",]
external_1_eng <- na.omit(external_1_eng$text)

external_1_text=get_sentences(external_1_eng)
external_1_sentence = sentiment(external_1_text)
external_1_sentence = as.data.table(external_1_sentence)
external_1_review=external_1_sentence[,
                                  list(
                                    review.sentiment = mean(sentiment)
                                  ),
                                  by=list(element_id)]
external_link_1=external_link_1[external_link_1$lang=="en",]
external_1_sentiment = cbind(external_link_1, external_1_review)
summary(external_1_sentiment$review.sentiment)
hist(external_1_sentiment$review.sentiment)

#without external link
external_0_eng=external_link_0[external_link_0$lang=="en",]
external_0_eng <- na.omit(external_0_eng$text)

external_0_text=get_sentences(external_0_eng)
external_0_sentence = sentiment(external_0_text)
external_0_sentence = as.data.table(external_0_sentence)
external_0_review=external_0_sentence[,
                                      list(
                                        review.sentiment = mean(sentiment)
                                      ),
                                      by=list(element_id)]
external_link_0=external_link_0[external_link_0$lang=="en",]
external_0_sentiment = cbind(external_link_0, external_0_review)

summary(external_0_sentiment$review.sentiment)
hist(external_0_sentiment$review.sentiment)

###plot the daily average sentiment scores against each other
library(ggplot2)

# define bin (group by date)
external_1_sentiment$datetime = anytime(external_1_sentiment$created_at)
external_1_sentiment$dateGroup = format(external_1_sentiment$datetime,'%m-%d')
external_1_sentiment[is.na(external_1_sentiment$dateGroup),]

external_0_sentiment$datetime = anytime(external_0_sentiment$created_at)
external_0_sentiment$dateGroup = format(external_0_sentiment$datetime,'%m-%d')
external_0_sentiment[is.na(external_0_sentiment$dateGroup),]

external_1_sentiment <- external_1_sentiment %>% group_by(dateGroup) %>% summarize(link="with link",mean=mean(review.sentiment),n=n())
external_0_sentiment <- external_0_sentiment %>% group_by(dateGroup) %>% summarize(link="without link",mean=mean(review.sentiment),n=n())
total_sentiment <- rbind(external_1_sentiment, external_0_sentiment)

#plot 
ggplot(total_sentiment, 
       aes(x = dateGroup, y = mean, fill = link)) +
  geom_col(width = 0.8, position = 'dodge') + 
  labs(title = 'Link vs. No Link Daily Average Sentiment Score',
       x = 'Group Date', 
       y = 'Average Sentiment Score') +
  scale_fill_manual(values = c('#999999','#E69F00')) # customized color theme

ggplot(total_sentiment, aes(x=dateGroup, y=total_sentiment[,3], fill=link)) + geom_bar(stat="identity", position="dodge")


####Topic Model for External Links yes/no#####
library(tm)
library(textstem)

library(RColorBrewer)
library(wordcloud)

library(lda)
library(topicmodels)

###word cloud for texts with external links
#add self-defined stopwords 
myStopwords = c(stopwords("english"),"fantastic","beasts","beast", "fantasticbeasts","secret","dumbledore","secretofdumbledore") #add stop words

#preprocessing data
doc_link_1 = VCorpus(VectorSource(tweet_df$text[tweet_df$if_external_link==1]))
doc_link_1 <-tm_map(doc_link_1,content_transformer(tolower))
doc_link_1 = tm_map(doc_link_1, removePunctuation)
doc_link_1 = tm_map(doc_link_1, removeNumbers)
doc_link_1 = tm_map(doc_link_1, removeWords, myStopwords)
doc_link_1 = tm_map(doc_link_1, removeWords, stopwords("english"))
doc_link_1 = tm_map(doc_link_1, stripWhitespace)
doc_link_1 = tm_map(doc_link_1, content_transformer(lemmatize_strings))

DTM_link_1 = DocumentTermMatrix(doc_link_1)

#wordcloud generation
#frequency
matrix_DTM_link_1 =as.matrix(DTM_link_1)
WD_link_1 = sort(colSums(matrix_DTM_link_1),decreasing=TRUE)

#transform the data to fit with the wordcloud package
wcloud_link_1=data.table(words=names(WD_link_1),freq=WD_link_1)

set.seed(1)
#adjust focus sizes with scale
par(mar=c(1,1,1,1))
wordcloud(words = wcloud_link_1$words, freq = wcloud_link_1$freq, 
          scale=c(5,1),
          min.freq = 2,
          max.words=200, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))


###word cloud for texts without external links
#preprocessing data
doc_link_0 = VCorpus(VectorSource(tweet_df$text[tweet_df$if_external_link==0]))
doc_link_0 <-tm_map(doc_link_0,content_transformer(tolower))
doc_link_0 = tm_map(doc_link_0, removePunctuation)
doc_link_0 = tm_map(doc_link_0, removeNumbers)
doc_link_0 = tm_map(doc_link_0, removeWords, myStopwords)
doc_link_0 = tm_map(doc_link_0, removeWords, stopwords("english"))
doc_link_0 = tm_map(doc_link_0, stripWhitespace)
doc_link_0 = tm_map(doc_link_0, content_transformer(lemmatize_strings))
DTM_link_0 = DocumentTermMatrix(doc_link_0)

#wordcloud generation
#frequency
matrix_DTM_link_0 =as.matrix(DTM_link_0)
WD_link_0 = sort(colSums(matrix_DTM_link_0),decreasing=TRUE)

#transform the data to fit with the wordcloud package
wcloud_link_0=data.table(words=names(WD_link_0),freq=WD_link_0)

set.seed(1)
#adjust focus sizes with scale
par(mar=c(1,1,1,1))
wordcloud(words = wcloud_link_0$words, freq = wcloud_link_0$freq, 
          scale=c(5,1),
          min.freq = 2,
          max.words=200, 
          random.order=FALSE,
          rot.per=0.1, 
          colors=brewer.pal(8, "Dark2"))


