
## ----load-tweets, echo=F-------------------------------------------------

## load tweets into R
library('twitteR')
library('tidyverse')
library('lubridate')
library('SnowballC')

#authenticate using your login token
consumerKey <- "NTMoENQJ4RQ4rSMTvGX0k0IuU"
consumerSecret <- "7yoIpBCuM9RfxmUt2I3wgyPh5ESl307GI6V2lLbXAe39jxUeSj"
accessToken <- "735776315702288384-QaY452YBvZiZjBAqkjvzEXieVoAPTpp"
accessTokenSecret <- "zUgHvnvJul6bqvLsESPkWFhdv2Ha4joLksj0j2yugGPM1"
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

##Retrieve tweets from Twitter
searchstring <- hashtags <- c("@PEXA_AUSTRALIA", "#PEXA_AUSTRALIA") # @ANZ_AU @Westpac @NAB @CommBank
searchstring <- paste(hashtags, collapse = " OR ")
tweets <- searchTwitter(searchstring, n = 3200) #since="2017-03-01", until="2017-04-07" , resultType = "popular"
rm(hashtags,searchstring)

## ----print-tweets, tidy=F------------------------------------------------
(n.tweet <- tweets %>% length())
# convert tweets to a data frame
tweets.df <- tweets %>% twListToDF()
# tweet #1
tweets.df[1, c("id", "created", "screenName", "replyToSN",
               "favoriteCount", "retweetCount", "longitude", "latitude", "text")]
# print tweet #1 and make text fit for slide width
tweets.df$text[1] %>% strwrap(60) %>% writeLines()

tweets.df
# write_rds(big4_df,'./data/tweets_big4.rds')

## ----text cleaning functions, tidy=F-------------------------------------

remove_reg <- "https?://[^\\s]+|&amp;|&lt;|&gt;|\bRT\\b" #cleaning of twitter texts

tidy_tweets <-
  tweets.df %>%
  filter(!str_detect(text, "^RT")) %>% #remove tweets from this dataset that are retweets so that we only have tweets that we wrote ourselves
  mutate(text = str_remove_all(text, remove_reg), #remove odd characters
         created = ymd_hms(created)) %>% #update date format for use late
  unnest_tokens(word, text, token = "tweets") %>% #token is a variant that retains hashtag # and @ symbols.
  mutate(word_stem = wordStem(word)) %>% #stemming of words (i.e. win, winning, winner)
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))


