# authorisation
if (!require('pacman')) install.packages('pacman')
pacman::p_load(twitteR, ROAuth, RCurl)

api_key = '86jsUhllx7dSUK18YwYUNHB2U'
api_secret = 'ASjMtwXgYq5IrZBQBubFalI5sUSq7CB7FqHnMFNaR4gzdGLdNt'
access_token = '984604213912244224-qZNR4vDqVVBDI8EnBB33z6IWFkeRqL9'
access_token_secret = 'ZXmhaj3pYwXrYb8U5cqOtTsqm5K0fHCTUYxHwhEroMqso'

# Set SSL certs globally
options(RCurlOptions = list(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')))

# set up the URLs
reqURL = 'https://api.twitter.com/oauth/request_token'
accessURL = 'https://api.twitter.com/oauth/access_token'
authURL = 'https://api.twitter.com/oauth/authorize'

twitCred = OAuthFactory$new(consumerKey = api_key, consumerSecret = api_secret, requestURL = reqURL, accessURL = accessURL, authURL = authURL)

twitCred$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl'))
2659496
install.packages("syuzhet")
install.packages("tm")
install.packages("wordcloud")
installed.packages("SnowballC")
installed.packages("twitteR")
library(foreign)
library(syuzhet)
library(lubridate)
library(plyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(twitteR)

pacman::p_load(twitteR, sentiment, plyr, ggplot2, wordcloud, RColorBrewer, httpuv, RCurl, base64enc)

options(RCurlOptions = list(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')))

api_key = '86jsUhllx7dSUK18YwYUNHB2U'
api_secret = 'ASjMtwXgYq5IrZBQBubFalI5sUSq7CB7FqHnMFNaR4gzdGLdNt'
access_token = '984604213912244224-qZNR4vDqVVBDI8EnBB33z6IWFkeRqL9'
access_token_secret = 'ZXmhaj3pYwXrYb8U5cqOtTsqm5K0fHCTUYxHwhEroMqso'

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
1

#Harvesting Tweets
tweets <- userTimeline("realDonaldTrump", n=200)
n.tweet <- length(tweets)
tweets.df <- twListToDF(tweets) 
head(tweets.df)


#Removing special characters

tweets.df2 <- gsub("http.*","",tweets.df$text)

tweets.df2 <- gsub("https.*","",tweets.df2)

tweets.df2 <- gsub("#.*","",tweets.df2)

tweets.df2 <- gsub("@.*","",tweets.df2)

tweets.df2 <- gsub("http.*","",tweets.df$text)

tweets.df2 <- gsub("https.*","",tweets.df2)

tweets.df2 <- gsub("#.*","",tweets.df2)

tweets.df2 <- gsub("@.*","",tweets.df2)

#Getting sentiment defining words
word.df <- as.vector(tweets.df2)

emotion.df <- get_nrc_sentiment(word.df)

emotion.df2 <- cbind(tweets.df2, emotion.df) 

head(emotion.df2)

word.df <- as.vector(tweets.df2)

emotion.df <- get_nrc_sentiment(word.df)

emotion.df2 <- cbind(tweets.df, emotion.df) 

head(emotion.df2)

#Getting positive sentiments

sent.value <- get_sentiment(word.df)

most.positive <- word.df[sent.value == max(sent.value)]

most.positive

sent.value <- get_sentiment(word.df)

most.positive <- word.df[sent.value == max(sent.value)]

most.positive

#Getting negative sentiments

most.negative <- word.df[sent.value <= min(sent.value)] 

most.negative 

#Positive and negative sentiment scores
sent.value

#Segregating positive tweets

positive.tweets <- word.df[sent.value > 0]
head(positive.tweets)

#Segregating negative tweets

negative.tweets <- word.df[sent.value < 0]
head(negative.tweets)

#Segregating neutral tweets

neutral.tweets <- word.df[sent.value == 0]
head(neutral.tweets)



# Alternate way to classify as Positive, Negative or Neutral tweets

category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

head(category_senti)

#Plot sentiment across time
ggplot(emotion.df2, aes(x=negative, y=retweetCount)) + geom_point() +
  ggtitle("Sentiment") + xlab("Neg") + ylab("Count")
ggplot(emotion.df2, aes(x=positive, y=retweetCount)) + geom_point() +
  ggtitle("Sentiment") + xlab("Pos") + ylab("Count")
ggplot(emotion.df2, aes(x=positive, y=favoriteCount)) + geom_point() +
  ggtitle("Sentiment") + xlab("Pos") + ylab("Count")

write.csv(emotion.df2, "trumptweets.csv")

#getting Tweets from metoo movement
metoo <- searchTwitter("#metoo exclude:retweets", n=3200)
metoo_df <- tbl_df(map_df(metoo, as.data.frame))
write.csv(metoo_df, "metoo.csv")

#Cleaning

metoo_df2 <- gsub("http.*","",metoo_df$text)

metoo_df2 <- gsub("https.*","",metoo_df2)

metoo_df2 <- gsub("#.*","",metoo_df2)

metoo_df2 <- gsub("@.*","",metoo_df2)

metoo_df2 <- gsub("http.*","",metoo_df$text)

metoo_df2 <- gsub("https.*","",metoo_df2)

metoo_df2 <- gsub("#.*","",metoo_df2)

metoo_df2 <- gsub("@.*","",metoo_df2)

#Emotion
emotion.df <- as.vector(metoo_df2)

emotion_metoo.df <- get_nrc_sentiment(emotion.df)

emotion_metoo.df2 <- cbind(metoo_df2, emotion.df) 

#Harvesting walkout tweets

wo_twitter <- searchTwitter("#NationalSchoolWalkout -filter:retweets",n=1000)

wo_twitter_df <- twListToDF(wo_twitter)

write.csv(wo_twitter_df, "wo_twitter.csv")